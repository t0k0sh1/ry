#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/module_loader.hpp"
#include "ry/codegen.hpp"
#include "ry/source_manager.hpp"
#include "ry/diagnostic.hpp"
#include "ry/jit.hpp"
#include "ry/test_runtime.hpp"
#include "ry/project_config.hpp"
#include "ry/formatter.hpp"
#include "ry/self_update.hpp"
#include "ry/args_runtime.hpp"
#include "ry/paths.hpp"
#include "ry/file_watcher.hpp"
#include "ry/coverage_runtime.hpp"
#include "ry/dotenv.hpp"
#include <algorithm>
#include <cerrno>
#include <chrono>
#ifdef __APPLE__
#include <crt_externs.h>
#define RY_ENVIRON (*_NSGetEnviron())
#else
extern "C" { extern char **environ; }
#define RY_ENVIRON environ
#endif
#include <cstdlib>
#include <iostream>
#include <csignal>
#include <cstdio>
#include <cstring>
#include <deque>
#include <filesystem>
#include <mutex>
#include <spawn.h>
#include <sys/wait.h>
#include <thread>
#include <unistd.h>
#include <unordered_set>
#include <vector>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
using namespace llvm;
using namespace llvm::orc;

namespace fs = std::filesystem;

static void test_timeout_handler(int) {
    const char msg[] = "\nTest timed out (60s)\n";
    (void)write(STDERR_FILENO, msg, sizeof(msg) - 1);
    _exit(124);
}

// RY_ENV: skip $RY_HOME/lib when running in "internal" mode
static bool g_skip_global_lib = false;

// Coverage: accumulated filenames across multiple runRyFile calls.
// Maps global file_id to filename for user files (excluding stdlib).
static std::vector<std::string> g_coverage_filenames;
static int g_coverage_file_count = 0;
// Global offset to avoid file_id collisions across separate runRyFile calls.
static int g_coverage_file_id_offset = 0;

static void resetCoverageState() {
    __ry_coverage_reset();
    g_coverage_file_id_offset = 0;
    g_coverage_file_count = 0;
    g_coverage_filenames.clear();
}

static void emitCoverageReport() {
    if (g_coverage_file_count <= 0) return;
    std::vector<const char*> ptrs(g_coverage_file_count);
    for (int i = 0; i < g_coverage_file_count; ++i)
        ptrs[i] = g_coverage_filenames[i].c_str();
    __ry_coverage_report_summary(ptrs.data(), g_coverage_file_count);
}

// Compile and run Ry source code via JIT. Returns the JIT function's exit code.
static int runRySource(const std::string &src, const std::string &source_name,
                       const std::string &referrer_dir, bool test_mode,
                       const char *argv0, bool coverage_mode = false) {
    // Load .env from project root (once per root per process)
    {
        if (auto root = findProjectRoot(referrer_dir)) {
            static std::string loaded_root;
            if (loaded_root != *root) {
                const char *ry_env = std::getenv("RY_ENV");
                ry::loadDotEnv(*root, ry_env ? ry_env : "");
                loaded_root = *root;
            }
        }
    }

    // Source manager for rich error messages
    SourceManager sm;
    int fileId = sm.addSource(source_name, src);

    // Lex -> Parse
    Lexer  lexer(src);
    Parser parser(lexer, &sm, fileId);
    Program prog = parser.parseProgram();

    // Set up search paths with lib/ for stdlib
    std::vector<std::string> search_paths;
    std::string lib_dir = ry::find_lib_dir(argv0, g_skip_global_lib).string();
    if (!lib_dir.empty()) {
        search_paths.push_back(lib_dir);
        // Enable flattened imports: "from math" resolves to lib/std/math/
        search_paths.push_back((fs::path(lib_dir) / "std").string());
    }

    ModuleLoader loader(search_paths, &sm);

    // Implicit `from std` import (like Java's java.lang)
    // Only insert if std can be found (graceful degradation without stdlib)
    try {
        Program std_prog;
        std_prog.push_back(ImportStmt{"std", {}, {0, 0, fileId}});
        // Resolve against stdlib search paths only (not referrer_dir)
        // to prevent local std/ from shadowing the bundled stdlib
        std_prog = loader.resolveImports(std_prog, "");
        prog.insert(prog.begin(),
            std::make_move_iterator(std_prog.begin()),
            std::make_move_iterator(std_prog.end()));
    } catch (const std::runtime_error &) {
        // stdlib not installed — continue without it
    }

    // Resolve remaining imports
    prog = loader.resolveImports(prog, referrer_dir);

    // CodeGen -> ThreadSafeModule
    CodeGen cg(test_mode, &sm, coverage_mode, g_coverage_file_id_offset);
    ThreadSafeModule tsm = cg.compile(prog);

    // Build LLJIT
    auto jitOrErr = LLJITBuilder().create();
    if (!jitOrErr) {
        errs() << "Failed to create JIT: ";
        logAllUnhandledErrors(jitOrErr.takeError(), errs());
        return 1;
    }
    auto &jit = *jitOrErr;

    jit->getIRTransformLayer().setTransform(ry::optimizeModule);

    auto &mainJD = jit->getMainJITDylib();
    auto dlsg = DynamicLibrarySearchGenerator::GetForCurrentProcess(
        jit->getDataLayout().getGlobalPrefix());
    if (!dlsg) {
        errs() << "Failed to create DynamicLibrarySearchGenerator: ";
        logAllUnhandledErrors(dlsg.takeError(), errs());
        return 1;
    }
    mainJD.addGenerator(std::move(*dlsg));

    // Register test runtime symbols if in test mode
    if (test_mode) {
        auto &es = jit->getExecutionSession();
        SymbolMap testSymbols;
        testSymbols[es.intern("__ry_test_describe_begin")] =
            {ExecutorAddr::fromPtr(&__ry_test_describe_begin), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_describe_end")] =
            {ExecutorAddr::fromPtr(&__ry_test_describe_end), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_begin")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_begin), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_end")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_end), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_expect_fail")] =
            {ExecutorAddr::fromPtr(&__ry_test_expect_fail), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_summary")] =
            {ExecutorAddr::fromPtr(&__ry_test_summary), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_set")] =
            {ExecutorAddr::fromPtr(&__ry_mock_set), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_get")] =
            {ExecutorAddr::fromPtr(&__ry_mock_get), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_get_call_count")] =
            {ExecutorAddr::fromPtr(&__ry_mock_get_call_count), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_increment_call")] =
            {ExecutorAddr::fromPtr(&__ry_mock_increment_call), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_clear_all")] =
            {ExecutorAddr::fromPtr(&__ry_mock_clear_all), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_prop_init_rng")] =
            {ExecutorAddr::fromPtr(&__ry_test_prop_init_rng), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_int")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_int), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_float")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_float), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_bool")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_bool), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_str")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_str), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_is_failed")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_is_failed), JITSymbolFlags::Exported};
        if (auto err = mainJD.define(absoluteSymbols(std::move(testSymbols)))) {
            errs() << "Failed to define test symbols: ";
            logAllUnhandledErrors(std::move(err), errs());
            return 1;
        }
    }

    // Register coverage runtime symbols if in coverage mode
    if (coverage_mode) {
        auto &es = jit->getExecutionSession();
        SymbolMap covSymbols;
        covSymbols[es.intern("__ry_coverage_hit")] =
            {ExecutorAddr::fromPtr(&__ry_coverage_hit), JITSymbolFlags::Exported};
        if (auto err = mainJD.define(absoluteSymbols(std::move(covSymbols)))) {
            errs() << "Failed to define coverage symbols: ";
            logAllUnhandledErrors(std::move(err), errs());
            return 1;
        }
    }

    // Add module
    if (auto err = jit->addIRModule(std::move(tsm))) {
        errs() << "Failed to add IR module: ";
        logAllUnhandledErrors(std::move(err), errs());
        return 1;
    }

    // Lookup and run __ry_main__
    auto symOrErr = jit->lookup("__ry_main__");
    if (!symOrErr) {
        errs() << "Failed to lookup __ry_main__: ";
        logAllUnhandledErrors(symOrErr.takeError(), errs());
        return 1;
    }
    auto *fn = symOrErr->toPtr<int(*)()>();

    if (test_mode) {
        std::signal(SIGALRM, test_timeout_handler);
    }
    int result = fn();

    // Record filenames for coverage reporting (accumulates across calls).
    // Exclude stdlib files (those under lib_dir) from the report.
    if (coverage_mode) {
        int fc = sm.getFileCount();
        int new_total = g_coverage_file_id_offset + fc;
        if (new_total > g_coverage_file_count) {
            g_coverage_filenames.resize(new_total);
            for (int i = 0; i < fc; ++i) {
                int gid = g_coverage_file_id_offset + i;
                const std::string &fname = sm.getFilename(i);
                bool is_stdlib = false;
                if (!lib_dir.empty()) {
                    auto canonical = fs::weakly_canonical(fname).string();
                    auto canonical_lib = fs::weakly_canonical(lib_dir).string();
                    is_stdlib = canonical.find(canonical_lib) == 0;
                }
                g_coverage_filenames[gid] = is_stdlib ? "" : fname;
            }
            g_coverage_file_count = new_total;
        }
        g_coverage_file_id_offset += fc;
    }

    return result > 0 ? 1 : 0;
}

// Compile and run a .ry file via JIT. Returns the JIT function's exit code.
static int runRyFile(const std::string &filepath, bool test_mode,
                     const char *argv0, bool coverage_mode = false) {
    auto bufOrErr = MemoryBuffer::getFile(filepath);
    if (!bufOrErr) {
        errs() << "Error reading file: " << filepath << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();
    std::string referrer_dir = fs::path(filepath).parent_path().string();
    return runRySource(src, filepath, referrer_dir, test_mode, argv0, coverage_mode);
}

// Collect *.test.ry files recursively under root_dir
static std::vector<std::string> findTestFiles(const std::string &root_dir) {
    static const std::unordered_set<std::string> skip_dirs = {
        ".git", "build", "node_modules"
    };
    std::vector<std::string> files;
    std::error_code ec;
    auto it = fs::recursive_directory_iterator(
        root_dir, fs::directory_options::skip_permission_denied, ec);
    if (ec) return files;
    for (auto end = fs::recursive_directory_iterator(); it != end; it.increment(ec)) {
        if (ec) { ec.clear(); continue; }
        auto &entry = *it;
        if (entry.is_directory() && skip_dirs.count(entry.path().filename().string())) {
            it.disable_recursion_pending();
            continue;
        }
        if (!entry.is_regular_file()) continue;
        auto path = entry.path().string();
        if (path.size() >= 8 && path.compare(path.size() - 8, 8, ".test.ry") == 0) {
            files.push_back(path);
        }
    }
    std::sort(files.begin(), files.end());
    return files;
}

// Run multiple test files sequentially and report results
static int runTestFilesSequential(const std::vector<std::string> &test_files,
                                  const char *argv0, bool coverage = false) {
    if (coverage) resetCoverageState();
    int total_failed = 0;
    int total_files = 0;
    for (const auto &tf : test_files) {
        ++total_files;
        try {
            int failed = runRyFile(tf, /*test_mode=*/true, argv0, coverage);
            total_failed += failed;
        } catch (const DiagnosticError &e) {
            errs() << e.what();
            ++total_failed;
        } catch (const std::exception &e) {
            errs() << "Error in " << tf << ": " << e.what() << "\n";
            ++total_failed;
        }
    }
    if (total_files > 1) {
        std::printf("\n%d test files executed, %d total failures\n",
                    total_files, total_failed);
    }
    if (coverage) emitCoverageReport();
    return total_failed > 0 ? 1 : 0;
}

struct TestFileResult {
    std::string filepath;
    int exit_code;
    std::string output;
};

// Run a single test file in a subprocess, capturing stdout+stderr.
// Uses posix_spawn instead of fork to be safe from multi-threaded contexts.
static TestFileResult runTestFileSubprocess(const std::string &filepath,
                                            const std::string &exe_path) {
    TestFileResult result;
    result.filepath = filepath;
    result.exit_code = -1;

    int pipefd[2];
    if (pipe(pipefd) == -1) {
        result.output = "Error: pipe() failed: " + std::string(strerror(errno)) + "\n";
        return result;
    }

    posix_spawn_file_actions_t actions;
    int rc = posix_spawn_file_actions_init(&actions);
    if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, pipefd[0]);
    if (rc == 0) rc = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDOUT_FILENO);
    if (rc == 0) rc = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDERR_FILENO);
    if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, pipefd[1]);
    if (rc != 0) {
        posix_spawn_file_actions_destroy(&actions);
        close(pipefd[0]);
        close(pipefd[1]);
        result.output = "Error: posix_spawn_file_actions failed: " + std::string(strerror(rc)) + "\n";
        return result;
    }

    const char *argv[] = {exe_path.c_str(), "test", filepath.c_str(), nullptr};
    pid_t pid;
    int err = posix_spawn(&pid, exe_path.c_str(), &actions, nullptr,
                          const_cast<char *const *>(argv), RY_ENVIRON);
    posix_spawn_file_actions_destroy(&actions);

    if (err != 0) {
        close(pipefd[0]);
        close(pipefd[1]);
        result.output = "Error: posix_spawn() failed: " + std::string(strerror(err)) + "\n";
        return result;
    }

    close(pipefd[1]);
    char buf[4096];
    ssize_t n;
    while ((n = read(pipefd[0], buf, sizeof(buf))) > 0 || (n == -1 && errno == EINTR)) {
        if (n > 0) result.output.append(buf, n);
    }
    close(pipefd[0]);

    int status = 0;
    pid_t wp;
    while ((wp = waitpid(pid, &status, 0)) == -1) {
        if (errno != EINTR) break;
    }

    if (wp == -1)
        result.exit_code = -1;
    else if (WIFEXITED(status))
        result.exit_code = WEXITSTATUS(status);
    else if (WIFSIGNALED(status))
        result.exit_code = 128 + WTERMSIG(status);
    else
        result.exit_code = -1;

    return result;
}

// Run multiple test files in parallel using worker threads
static int runTestFilesParallel(const std::vector<std::string> &test_files,
                                const std::string &exe_path, int parallelism) {
    int num_files = static_cast<int>(test_files.size());
    std::vector<TestFileResult> results(num_files);

    // Work queue: indices into test_files
    std::deque<int> work_queue;
    for (int i = 0; i < num_files; ++i)
        work_queue.push_back(i);

    std::mutex queue_mutex;
    std::mutex progress_mutex;
    int completed = 0;

    auto worker = [&]() {
        while (true) {
            int idx;
            {
                std::lock_guard<std::mutex> lock(queue_mutex);
                if (work_queue.empty()) return;
                idx = work_queue.front();
                work_queue.pop_front();
            }
            results[idx] = runTestFileSubprocess(test_files[idx], exe_path);
            {
                std::lock_guard<std::mutex> lock(progress_mutex);
                ++completed;
                std::fprintf(stderr, "\r\033[K[%d/%d] Running tests...",
                             completed, num_files);
            }
        }
    };

    int num_workers = std::min(parallelism, num_files);
    std::vector<std::thread> threads;
    threads.reserve(num_workers);

    auto wall_start = std::chrono::steady_clock::now();
    for (int i = 0; i < num_workers; ++i)
        threads.emplace_back(worker);
    for (auto &t : threads)
        t.join();
    auto wall_end = std::chrono::steady_clock::now();
    double wall_elapsed = std::chrono::duration<double>(wall_end - wall_start).count();

    std::fprintf(stderr, "\r\033[K");

    int total_failed = 0;
    for (const auto &r : results) {
        std::fputs(r.output.c_str(), stdout);
        if (r.exit_code != 0)
            ++total_failed;
    }

    std::printf("\n%d test files executed, %d total failures (%.2fs, %d workers)\n",
                num_files, total_failed, wall_elapsed, num_workers);
    return total_failed > 0 ? 1 : 0;
}

// Run multiple test files and report results
static int runTestFiles(const std::vector<std::string> &test_files,
                        const char *argv0, bool parallel, bool coverage = false) {
    if (!parallel || test_files.size() <= 1) {
        return runTestFilesSequential(test_files, argv0, coverage);
    }
    std::string exe_path = ry::self_update::detail::get_executable_path();
    if (exe_path.empty()) {
        errs() << "Warning: cannot resolve executable path, falling back to sequential\n";
        return runTestFilesSequential(test_files, argv0, coverage);
    }
    unsigned hw = std::thread::hardware_concurrency();
    int parallelism = static_cast<int>(std::min({hw, 8u, static_cast<unsigned>(test_files.size())}));
    if (parallelism < 1) parallelism = 1;
    return runTestFilesParallel(test_files, exe_path, parallelism);
}

// Discover *.test.ry files in a directory and run them
static int discoverAndRunTests(const std::string &dir, const char *argv0,
                               bool parallel, bool coverage = false) {
    auto test_files = findTestFiles(dir);
    if (test_files.empty()) {
        errs() << "No *.test.ry files found in " << dir << "\n";
        return 1;
    }
    return runTestFiles(test_files, argv0, parallel, coverage);
}

static void applyRyEnv(const char *value) {
    std::string canonical = ry::resolveRyEnv(value);
    setenv("RY_ENV", canonical.c_str(), 1);
    if (canonical == "internal")
        g_skip_global_lib = true;
}

static void parseRyEnv(int &argc, char **&argv) {
    // --env=<value> CLI flag takes precedence over RY_ENV env var
    if (argc >= 2 && std::strncmp(argv[1], "--env=", 6) == 0) {
        applyRyEnv(argv[1] + 6);
        // Remove --env flag from argv, preserving argv[0] (program name)
        argv[1] = argv[0];
        ++argv;
        --argc;
    } else if (const char *env = std::getenv("RY_ENV")) {
        applyRyEnv(env);
    }
}

int main(int argc, char *argv[]) {
    parseRyEnv(argc, argv);

    if (argc == 2 && (std::strcmp(argv[1], "--version") == 0 || std::strcmp(argv[1], "-v") == 0)) {
        llvm::outs() << "ry " << RY_VERSION << "\n";
        return 0;
    }

    // Handle subcommands that don't need LLVM initialization
    if (argc >= 2 && std::strcmp(argv[1], "self-update") == 0) {
        return cmd_self_update(argc - 2, argv + 2);
    }
    if (argc >= 2 && std::strcmp(argv[1], "init") == 0) {
        return cmd_init();
    }
    if (argc >= 2 && std::strcmp(argv[1], "new") == 0) {
        return cmd_new(argc - 2, argv + 2);
    }
    if (argc >= 2 && std::strcmp(argv[1], "fmt") == 0) {
        return cmd_fmt(argc - 2, argv + 2);
    }

    InitLLVM X(argc, argv);
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    bool test_mode = false;
    bool coverage_mode = false;
    const char *filename = nullptr;

    if (argc >= 2 && std::strcmp(argv[1], "test") != 0) {
        filename = argv[1];
        __ry_args_init(argc - 2, argc > 2 ? argv + 2 : nullptr);
    } else if (argc >= 2 && std::strcmp(argv[1], "test") == 0) {
        // Parse test subcommand arguments
        bool parallel = false;
        bool watch = false;
        bool coverage = false;
        const char *target = nullptr;
        for (int i = 2; i < argc; ++i) {
            if (std::strcmp(argv[i], "-p") == 0 ||
                std::strcmp(argv[i], "--parallel") == 0) {
                parallel = true;
            } else if (std::strcmp(argv[i], "-w") == 0 ||
                       std::strcmp(argv[i], "--watch") == 0) {
                watch = true;
            } else if (std::strcmp(argv[i], "--coverage") == 0 ||
                       std::strcmp(argv[i], "--cov") == 0) {
                coverage = true;
            } else if (!target) {
                target = argv[i];
            }
        }
        if (coverage && parallel) {
            errs() << "Warning: --coverage is not supported with --parallel, falling back to sequential\n";
            parallel = false;
        }

        if (target) {
            std::string target_str = target;
            std::error_code ec;
            if (fs::is_directory(target_str, ec)) {
                if (watch) {
                    const char *a0 = argv[0];
                    ry::watchAndRunTests(target_str, [target_str, a0, parallel, coverage]() {
                        discoverAndRunTests(target_str, a0, parallel, coverage);
                    });
                    return 0;
                }
                return discoverAndRunTests(target_str, argv[0], parallel, coverage);
            }
            if (ec) {
                errs() << "Error: cannot access " << target_str << ": "
                       << ec.message() << "\n";
                return 1;
            }
            if (watch) {
                // Watch project root (or file's parent dir) and re-run single file
                auto target_dir = fs::path(target_str).parent_path().string();
                std::string watch_root = findProjectRoot(target_dir).value_or(target_dir);
                const char *a0 = argv[0];
                ry::watchAndRunTests(watch_root, [target_str, a0]() {
                    try {
                        runRyFile(target_str, true, a0);
                    } catch (const DiagnosticError &e) {
                        errs() << e.what();
                    } catch (const std::exception &e) {
                        errs() << "Error: " << e.what() << "\n";
                    }
                });
                return 0;
            }
            // ry test <file.ry> — single file test (parallel flag ignored)
            test_mode = true;
            coverage_mode = coverage;
            filename = target;
            __ry_args_init(0, nullptr);
        } else {
            // ry test [-p] [-w] — discover from project root
            auto root = findProjectRoot();
            if (!root) {
                errs() << "Error: ry.toml not found. Run 'ry init' first.\n";
                return 1;
            }
            if (watch) {
                std::string root_dir = *root;
                const char *a0 = argv[0];
                ry::watchAndRunTests(root_dir, [root_dir, a0, parallel, coverage]() {
                    discoverAndRunTests(root_dir, a0, parallel, coverage);
                });
                return 0;
            }
            return discoverAndRunTests(*root, argv[0], parallel, coverage);
        }
    } else if (argc == 1 && !isatty(STDIN_FILENO)) {
        std::string src{std::istreambuf_iterator<char>(std::cin),
                        std::istreambuf_iterator<char>{}};
        __ry_args_init(0, nullptr);
        std::string cwd = fs::current_path().string();
        try {
            return runRySource(src, "<stdin>", cwd, false, argv[0]);
        } catch (const DiagnosticError &e) {
            errs() << e.what();
            return 1;
        } catch (const std::exception &e) {
            errs() << "Error: " << e.what() << "\n";
            return 1;
        }
    } else {
        errs() << "Usage: ry [--env=<env>] <file.ry> [args...]\n";
        errs() << "       echo '<code>' | ry\n";
        errs() << "       ry [--env=<env>] test [-p | --parallel] [-w | --watch] [--coverage | --cov] [<file.ry> | <dir>]\n";
        errs() << "       ry init\n";
        errs() << "       ry fmt [--check] [<file|dir>]\n";
        errs() << "       ry new <project-name>\n";
        errs() << "       ry self-update [--nightly | <version>]\n";
        errs() << "       ry --version\n";
        errs() << "\n";
        errs() << "Environment: RY_ENV=production|development|internal (default: production)\n";
        return 1;
    }

    try {
        if (coverage_mode) resetCoverageState();
        int rc = runRyFile(filename, test_mode, argv[0], coverage_mode);
        if (coverage_mode) emitCoverageReport();
        return rc;
    } catch (const DiagnosticError &e) {
        errs() << e.what();
        return 1;
    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
