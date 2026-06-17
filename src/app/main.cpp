#include "ry/cli/cli.hpp"
#include "ry/jit/jit_runner.hpp"
#include "ry/jit/test_runner.hpp"
#include "ry/project/project_config.hpp"
#include "ry/formatter.hpp"
#include "ry/cli/self_update.hpp"
#include "ry/args_runtime.hpp"
#include "ry/cli/file_watcher.hpp"
#include "ry/trace/trace.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <limits>
#include <unistd.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

using namespace ry;
using namespace llvm;

namespace fs = std::filesystem;

// #1749 / #1187 / #1657: after a successful JIT run on Linux/macOS, the C++
// static destructor chain (LLVM ManagedStatic, llvm_shutdown, etc.)
// intermittently aborts inside glibc heap consolidation. The triple-stage
// leak in src/jit_runner.cpp already suppresses ~LLJIT() and ~CodeGen()
// destructors, but residual LLVM static state run from atexit handlers still
// touches the disturbed heap. Bypassing the entire teardown chain via
// _exit(rc) is the canonical workaround pattern when destructor crashes have
// already been narrowed to the LLVM ORC family. Returns rc unchanged on
// platforms or paths that never JIT'd, so non-JIT exits (parse errors,
// help printing, formatter) still run normal teardown.
[[nodiscard]] static int finalizeAfterPossibleJit(int rc) {
#if defined(__linux__) || defined(__APPLE__)
    if (ry::jitWasInitialized()) {
        std::fflush(stdout);
        std::fflush(stderr);
        _exit(rc);
    }
#endif
    return rc;
}

// `.ry` suffix check used to detect deprecated `ry <file>` invocations (#1735).
static bool looksLikeRyFile(const char *arg) {
    if (!arg) return false;
    static constexpr const char kRySuffix[] = ".ry";
    constexpr size_t kRySuffixLen = sizeof(kRySuffix) - 1;
    const size_t n = std::strlen(arg);
    return n >= kRySuffixLen &&
           std::memcmp(arg + n - kRySuffixLen, kRySuffix, kRySuffixLen) == 0;
}

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
        return finalizeAfterPossibleJit(
            cmd_run(argc - 2, argv + 2, argv[0], skip_global_lib, emit_llvm_ir));
    }

    // Reject deprecated bare invocations (#1735). The dual-purpose `ry run`
    // subcommand replaces these. Reject before LLVM init so we don't pay the
    // initialization cost for paths we are about to refuse.
    if (argc == 1) {
        ry::cli::printMainHelp();
        return 1;
    }
    if (std::strcmp(argv[1], "--") == 0) {
        errs() << "Error: 'ry --' is no longer supported. Use 'ry run --' instead.\n";
        return 1;
    }
    if (looksLikeRyFile(argv[1])) {
        errs() << "Error: 'ry <file>' is no longer supported. Use 'ry run <file>' instead.\n";
        return 1;
    }
    if (std::strcmp(argv[1], "-c") != 0 && std::strcmp(argv[1], "test") != 0) {
        errs() << "Error: unknown command '" << argv[1] << "'\n\n";
        ry::cli::printMainHelp();
        return 1;
    }

    InitLLVM X(argc, argv);
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    if (argc >= 2 && std::strcmp(argv[1], "-c") == 0) {
        // ry -c — read and execute code from stdin
        std::string src{std::istreambuf_iterator<char>(std::cin),
                        std::istreambuf_iterator<char>{}};
        __ry_args_init(0, nullptr);
        std::string cwd = fs::current_path().string();
        try {
            return finalizeAfterPossibleJit(
                ry::runRySource(src, "<stdin>", cwd, false, argv[0], skip_global_lib,
                                false, false, nullptr, emit_llvm_ir));
        } catch (const DiagnosticError &e) {
            errs() << e.what();
            return finalizeAfterPossibleJit(1);
        } catch (const std::exception &e) {
            errs() << "Error: " << e.what() << "\n";
            return finalizeAfterPossibleJit(1);
        }
    } else if (argc >= 2 && std::strcmp(argv[1], "test") == 0) {
        // Parse test subcommand arguments
        //
        // -p semantics (#2234): `-p` controls only the worker count.
        //   `-p` absent       → 1 worker (uniform with the no-`-p` default)
        //   `-p` alone        → computeDefaultWorkers(hw_concurrency) = hw-1 (min 1)
        //   `-p N`            → N
        // All three cases go through subprocess fan-out; the in-process
        // sequential loop is gone (each child is "1 source = 1 process").
        bool parallel_flag_seen = false;
        int parallel_workers = 0; // semantics finalised after the parse loop
        bool watch = false;
        bool coverage = false;
        bool outline = false;
        std::string test_trace_out = trace_out;
        const char *target = nullptr;
        // Returns true if `s` looks like a worker-count attempt (leading digit, or
        // leading '-' followed by a digit). When this returns true the caller
        // commits to parsing `s` as a positive integer and reports a diagnostic
        // on any failure — so a malformed value never silently slides into the
        // target slot.
        auto looksLikeCountAttempt = [](const char *s) -> bool {
            if (!s || !*s) return false;
            if (*s >= '0' && *s <= '9') return true;
            if (*s == '-' && s[1] >= '0' && s[1] <= '9') return true;
            return false;
        };
        // Parses a positive worker count. Accepts only fully consumed decimal
        // input in (0, INT_MAX]. Emits its own diagnostic and returns false on
        // any failure (including overflow, zero, negative, trailing junk).
        auto parseWorkerCount = [](const char *s, const char *flag_label,
                                    int &out) -> bool {
            char *end = nullptr;
            errno = 0;
            long val = std::strtol(s, &end, 10);
            if (errno == ERANGE || !end || *end != '\0' || val <= 0 ||
                val > std::numeric_limits<int>::max()) {
                errs() << "Error: " << flag_label
                       << " worker count must be a positive integer, got '"
                       << s << "'\n";
                return false;
            }
            out = static_cast<int>(val);
            return true;
        };
        for (int i = 2; i < argc; ++i) {
            if (std::strcmp(argv[i], "-p") == 0 ||
                std::strcmp(argv[i], "--parallel") == 0) {
                parallel_flag_seen = true;
                // Optional spaced count: `-p N` / `--parallel N`. If the next
                // argv looks like a count attempt (digit, or `-` then digit),
                // commit to parsing it; otherwise leave it for the next loop
                // iteration (it might be another flag or the target).
                if (i + 1 < argc && looksLikeCountAttempt(argv[i + 1])) {
                    int n = 0;
                    if (!parseWorkerCount(argv[i + 1], "--parallel", n))
                        return 1;
                    parallel_workers = n;
                    ++i;
                }
            } else if (std::strncmp(argv[i], "--parallel=", 11) == 0) {
                parallel_flag_seen = true;
                const char *val_str = argv[i] + 11;
                if (*val_str == '\0') {
                    errs() << "Error: --parallel= requires a positive integer "
                              "(e.g. --parallel=8)\n";
                    return 1;
                }
                int n = 0;
                if (!parseWorkerCount(val_str, "--parallel", n)) return 1;
                parallel_workers = n;
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
        // -p semantics finalisation (#2234): `-p` absent → 1 worker (uniform with
        // the no-`-p` default sequential semantics). `-p` alone keeps parallel_workers
        // at 0, which computeParallelism interprets as "computeDefaultWorkers".
        if (!parallel_flag_seen) {
            parallel_workers = 1;
        }

        // Helper to disable per-file flags that cannot cross subprocess
        // boundaries cleanly (#2234). Applied only on multi-file paths
        // (directory target / no target → project root); the single-file
        // direct path keeps coverage / trace / outline fully functional.
        // Worker count is left untouched: throttling to 1 worker buys nothing
        // once the underlying feature is off, and would silently penalise
        // `ry test -p N --coverage` (~Nx slowdown for no functional gain).
        auto warnAndDisableMultiFileFlags = [&]() {
            if (coverage) {
                errs() << "Warning: --coverage is not supported with multi-file "
                          "test execution; coverage is only available for "
                          "single-file runs (e.g. `ry test foo.test.ry "
                          "--coverage`). Disabling.\n";
                coverage = false;
            }
            if (trace_enabled) {
                errs() << "Warning: --trace is not supported with multi-file "
                          "test execution; trace is only available for "
                          "single-file runs (e.g. `ry test foo.test.ry "
                          "--trace`). Disabling.\n";
                trace_enabled = false;
                ry::configureTrace(false, "");
            }
            if (outline) {
                errs() << "Warning: --outline is not supported with multi-file "
                          "test execution; outline is only available for "
                          "single-file runs (e.g. `ry test foo.test.ry "
                          "--outline`). Disabling.\n";
                outline = false;
            }
        };

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
                warnAndDisableMultiFileFlags();
                if (watch) {
                    ry::watchAndRunTests(target_str,
                        [target_str, parallel_workers]() {
                            ry::discoverAndRunTests(target_str, parallel_workers);
                        });
                    return finalizeAfterPossibleJit(0);
                }
                return finalizeAfterPossibleJit(
                    ry::discoverAndRunTests(target_str, parallel_workers));
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
                return finalizeAfterPossibleJit(0);
            }
            // ry test <file.ry> — single file test (parallel flag ignored)
            __ry_args_init(0, nullptr);
            try {
                ry::CoverageState cs;
                if (coverage) ry::resetCoverageState(cs);
                int rc = ry::runRyFile(target_str, /*test_mode=*/true,
                                       argv[0], skip_global_lib,
                                       coverage, outline,
                                       coverage ? &cs : nullptr, emit_llvm_ir);
                if (coverage) ry::emitCoverageReport(cs);
                return finalizeAfterPossibleJit(rc);
            } catch (const DiagnosticError &e) {
                errs() << e.what();
                return finalizeAfterPossibleJit(1);
            } catch (const std::exception &e) {
                errs() << "Error: " << e.what() << "\n";
                return finalizeAfterPossibleJit(1);
            }
        } else {
            // ry test [-p] [-w] — discover from project root
            auto root = findProjectRoot();
            if (!root) {
                errs() << "Error: package.toml not found. Run 'ry init' first.\n";
                return 1;
            }
            warnAndDisableMultiFileFlags();
            if (watch) {
                const std::string &root_dir = *root;
                // NOLINTNEXTLINE(bugprone-exception-escape): watcher lambda; exceptions terminate the process
                ry::watchAndRunTests(root_dir,
                    [root_dir, parallel_workers]() {
                        ry::discoverAndRunTests(root_dir, parallel_workers);
                    });
                return finalizeAfterPossibleJit(0);
            }
            return finalizeAfterPossibleJit(
                ry::discoverAndRunTests(*root, parallel_workers));
        }
    }

    // Unreachable: the deprecation block above already returns for argc == 1,
    // `ry --`, `ry <file.ry>`, and unknown subcommands; only `ry -c` / `ry test`
    // reach this point and both branches above return inside their bodies.
    return 1;
}
