#include "ry/cli.hpp"
#include "ry/jit_runner.hpp"
#include "ry/test_runner.hpp"
#include "ry/project_config.hpp"
#include "ry/formatter.hpp"
#include "ry/self_update.hpp"
#include "ry/args_runtime.hpp"
#include "ry/file_watcher.hpp"
#include "ry/trace.hpp"
#include "ry/diagnostic.hpp"
#include <cstring>
#include <filesystem>
#include <iostream>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

using namespace ry;
using namespace llvm;

namespace fs = std::filesystem;

// NOLINTNEXTLINE(bugprone-exception-escape): process boundary; uncaught exceptions invoke std::terminate
int main(int argc, char *argv[]) {
    bool skip_global_lib = ry::cli::parseRyEnv(argc, argv);
    bool trace_enabled = false;
    std::string trace_out = "-";
    bool emit_llvm_ir = false;
    ry::cli::parseGlobalFlags(argc, argv, trace_enabled, trace_out, emit_llvm_ir);
    ry::configureTrace(trace_enabled, trace_out);
    bool sessionStarted = false;
    if (ry::traceEnabled()) {
        ry::emitTraceEvent("session.start", "session", nullptr,
                           {ry::TraceField("argv0", argc > 0 && argv[0] ? argv[0] : "ry")});
        sessionStarted = true;
    }
    struct SessionTraceGuard {
        bool &started;
        explicit SessionTraceGuard(bool &s) : started(s) {}
        ~SessionTraceGuard() {
            if (started && ry::traceEnabled()) {
                ry::emitTraceEvent("session.end", "session");
            }
        }
    } sessionTraceGuard(sessionStarted);

    // Help flag handling: -h/--help takes priority over other options
    if (argc >= 2) {
        if (ry::cli::isKnownSubcommand(argv[1])) {
            // Subcommand help: ry <subcmd> ... -h/--help
            if (ry::cli::hasHelpFlag(argc, argv, 2)) {
                if (std::strcmp(argv[1], "test") == 0) { ry::cli::printTestHelp(); return 0; }
                if (std::strcmp(argv[1], "init") == 0) { ry::cli::printInitHelp(); return 0; }
                if (std::strcmp(argv[1], "new") == 0) { ry::cli::printNewHelp(); return 0; }
                if (std::strcmp(argv[1], "fmt") == 0) { ry::cli::printFmtHelp(); return 0; }
                if (std::strcmp(argv[1], "run") == 0) { ry::cli::printRunHelp(); return 0; }
                if (std::strcmp(argv[1], "self-update") == 0) { ry::cli::printSelfUpdateHelp(); return 0; }
            }
        } else {
            // Main help: ry -h, ry --help, ry --env=internal -h, etc.
            if (ry::cli::hasHelpFlag(argc, argv)) {
                ry::cli::printMainHelp();
                return 0;
            }
        }
    }

    if (argc == 2 && (std::strcmp(argv[1], "--version") == 0 || std::strcmp(argv[1], "-v") == 0 || std::strcmp(argv[1], "version") == 0)) {
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
    if (argc >= 2 && std::strcmp(argv[1], "run") == 0) {
        return cmd_run(argc - 2, argv + 2);
    }

    InitLLVM X(argc, argv);
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    bool test_mode = false;
    bool coverage_mode = false;
    bool outline_mode = false;
    const char *filename = nullptr;
    std::string entry_path_storage; // lifetime holder for entry point path

    if (argc >= 2 && std::strcmp(argv[1], "-c") == 0) {
        // ry -c — read and execute code from stdin
        std::string src{std::istreambuf_iterator<char>(std::cin),
                        std::istreambuf_iterator<char>{}};
        __ry_args_init(0, nullptr);
        std::string cwd = fs::current_path().string();
        try {
            return ry::runRySource(src, "<stdin>", cwd, false, argv[0], skip_global_lib,
                                   false, false, nullptr, emit_llvm_ir);
        } catch (const DiagnosticError &e) {
            errs() << e.what();
            return 1;
        } catch (const std::exception &e) {
            errs() << "Error: " << e.what() << "\n";
            return 1;
        }
    } else if (argc >= 2 && std::strcmp(argv[1], "--") == 0) {
        // ry -- [args...] — run entry point with arguments
        entry_path_storage = ry::cli::resolveEntryPoint(true);
        if (entry_path_storage.empty()) return 1;
        filename = entry_path_storage.c_str();
        __ry_args_init(argc - 2, argc > 2 ? argv + 2 : nullptr);
    } else if (argc >= 2 && std::strcmp(argv[1], "test") != 0) {
        // Unknown subcommand detection: if the argument is not an existing file,
        // try resolving a bare filename against package.toml [paths], else error.
        std::string arg1 = argv[1];
        if (fs::exists(arg1)) {
            entry_path_storage.clear();
            filename = argv[1];
        } else {
            // Explicit relative/absolute *.ry path that does not exist — file error, not unknown command.
            const fs::path arg_path(arg1);
            if (arg_path.has_parent_path()) {
                static constexpr const char kRySuffix[] = ".ry";
                constexpr size_t kRySuffixLen = sizeof(kRySuffix) - 1;
                if (arg1.size() >= kRySuffixLen &&
                    arg1.compare(arg1.size() - kRySuffixLen, kRySuffixLen, kRySuffix) == 0) {
                    errs() << "Error: no such file: " << arg1 << "\n";
                    return 1;
                }
            }
            std::string resolved;
            std::string resolve_err;
            if (ry::cli::tryResolveBareRyFile(arg1, resolved, resolve_err)) {
                entry_path_storage = std::move(resolved);
                filename = entry_path_storage.c_str();
            } else if (!resolve_err.empty()) {
                errs() << resolve_err;
                return 1;
            } else {
                errs() << "Error: unknown command '" << arg1 << "'\n\n";
                ry::cli::printMainHelp();
                return 1;
            }
        }
        __ry_args_init(argc - 2, argc > 2 ? argv + 2 : nullptr);
    } else if (argc >= 2 && std::strcmp(argv[1], "test") == 0) {
        // Parse test subcommand arguments
        bool parallel = false;
        bool watch = false;
        bool coverage = false;
        bool outline = false;
        std::string test_trace_out = trace_out;
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
            } else if (std::strcmp(argv[i], "--outline") == 0) {
                outline = true;
            } else if (std::strcmp(argv[i], "--trace") == 0) {
                trace_enabled = true;
            } else if (std::strncmp(argv[i], "--trace-out=", 12) == 0) {
                trace_enabled = true;
                test_trace_out = argv[i] + 12;
            } else if (!target) {
                target = argv[i];
            }
        }
        if (trace_enabled) {
            ry::configureTrace(true, test_trace_out);
            if (ry::traceEnabled() && !sessionStarted) {
                ry::emitTraceEvent("session.start", "session", nullptr,
                                   {ry::TraceField("argv0", argc > 0 && argv[0] ? argv[0] : "ry")});
                sessionStarted = true;
            }
        }
        if (coverage && parallel) {
            errs() << "Warning: --coverage is not supported with --parallel, falling back to sequential\n";
            parallel = false;
        }
        if (trace_enabled && parallel) {
            errs() << "Warning: --trace is not supported with --parallel, falling back to sequential\n";
            parallel = false;
        }

        if (target) {
            std::string target_str = target;
            std::string test_target_storage;
            std::error_code ec;
            const bool path_exists = fs::exists(target_str, ec);
            if (ec) {
                errs() << "Error: cannot access " << target_str << ": "
                       << ec.message() << "\n";
                return 1;
            }
            ec.clear();
            const bool path_is_directory =
                path_exists && fs::is_directory(target_str, ec);
            if (ec) {
                errs() << "Error: cannot access " << target_str << ": "
                       << ec.message() << "\n";
                return 1;
            }
            if (path_is_directory) {
                if (watch) {
                    const char *a0 = argv[0];
                    bool sgl = skip_global_lib;
                    ry::watchAndRunTests(target_str, [target_str, a0, sgl, parallel, coverage, outline]() {
                        ry::discoverAndRunTests(target_str, a0, sgl, parallel, coverage, outline);
                    });
                    return 0;
                }
                return ry::discoverAndRunTests(target_str, argv[0], skip_global_lib, parallel, coverage, outline);
            }
            if (!path_exists) {
                std::string resolved;
                std::string resolve_err;
                if (ry::cli::tryResolveBareRyFile(target_str, resolved, resolve_err)) {
                    test_target_storage = std::move(resolved);
                    target_str = test_target_storage;
                } else if (!resolve_err.empty()) {
                    errs() << resolve_err;
                    return 1;
                } else {
                    errs() << "Error: no such file: " << target_str << "\n";
                    return 1;
                }
            }
            if (watch) {
                // Watch project root (or file's parent dir) and re-run single file
                auto target_dir = fs::path(target_str).parent_path().string();
                std::string watch_root = findProjectRoot(target_dir).value_or(target_dir);
                const char *a0 = argv[0];
                bool sgl = skip_global_lib;
                ry::watchAndRunTests(watch_root, [target_str, a0, sgl, outline]() {
                    try {
                        ry::runRyFile(target_str, true, a0, sgl, false, outline);
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
            outline_mode = outline;
            entry_path_storage = target_str;
            filename = entry_path_storage.c_str();
            __ry_args_init(0, nullptr);
        } else {
            // ry test [-p] [-w] — discover from project root
            auto root = findProjectRoot();
            if (!root) {
                errs() << "Error: package.toml not found. Run 'ry init' first.\n";
                return 1;
            }
            if (watch) {
                const std::string &root_dir = *root;
                const char *a0 = argv[0];
                bool sgl = skip_global_lib;
                // NOLINTNEXTLINE(bugprone-exception-escape): watcher lambda; exceptions terminate the process
                ry::watchAndRunTests(root_dir, [root_dir, a0, sgl, parallel, coverage, outline]() {
                    ry::discoverAndRunTests(root_dir, a0, sgl, parallel, coverage, outline);
                });
                return 0;
            }
            return ry::discoverAndRunTests(*root, argv[0], skip_global_lib, parallel, coverage, outline);
        }
    } else if (argc == 1) {
        entry_path_storage = ry::cli::resolveEntryPoint(false);
        if (entry_path_storage.empty()) {
            ry::cli::printMainHelp();
            return 1;
        }
        filename = entry_path_storage.c_str();
        __ry_args_init(0, nullptr);
    }

    try {
        ry::CoverageState cs;
        if (coverage_mode) ry::resetCoverageState(cs);
        int rc = ry::runRyFile(filename, test_mode, argv[0], skip_global_lib,
                               coverage_mode, outline_mode, coverage_mode ? &cs : nullptr,
                               emit_llvm_ir);
        if (coverage_mode) ry::emitCoverageReport(cs);
        return rc;
    } catch (const DiagnosticError &e) {
        errs() << e.what();
        return 1;
    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }
}
