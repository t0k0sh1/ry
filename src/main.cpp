#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/module_loader.hpp"
#include "ry/codegen.hpp"
#include "ry/source_manager.hpp"
#include "ry/diagnostic.hpp"
#include "ry/jit.hpp"
#include "ry/test_runtime.hpp"
#include "ry/project_config.hpp"
#include "ry/self_update.hpp"
#include "ry/args_runtime.hpp"
#include "ry/paths.hpp"
#include <algorithm>
#include <cstdio>
#include <cstring>
#include <filesystem>
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

// Compile and run a .ry file via JIT. Returns the JIT function's exit code.
static int runRyFile(const std::string &filepath, bool test_mode,
                     const char *argv0) {
    auto bufOrErr = MemoryBuffer::getFile(filepath);
    if (!bufOrErr) {
        errs() << "Error reading file: " << filepath << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();

    // Source manager for rich error messages
    SourceManager sm;
    int fileId = sm.addSource(filepath, src);

    // Lex -> Parse
    Lexer  lexer(src);
    Parser parser(lexer, &sm, fileId);
    Program prog = parser.parseProgram();

    // Set up search paths with lib/ for stdlib
    std::vector<std::string> search_paths;
    std::string lib_dir = ry::find_lib_dir(argv0).string();
    if (!lib_dir.empty()) search_paths.push_back(lib_dir);

    std::string referrer_dir = fs::path(filepath).parent_path().string();
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
    CodeGen cg(test_mode, &sm);
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
        if (auto err = mainJD.define(absoluteSymbols(std::move(testSymbols)))) {
            errs() << "Failed to define test symbols: ";
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
    int result = fn();
    return result > 0 ? 1 : 0;
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

int main(int argc, char *argv[]) {
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

    InitLLVM X(argc, argv);

    bool test_mode = false;
    const char *filename = nullptr;

    if (argc >= 2 && std::strcmp(argv[1], "test") != 0) {
        filename = argv[1];
        __ry_args_init(argc - 2, argc > 2 ? argv + 2 : nullptr);
    } else if (argc >= 3 && std::strcmp(argv[1], "test") == 0) {
        // ry test <file.ry> — single file test
        test_mode = true;
        filename = argv[2];
        __ry_args_init(0, nullptr);
    } else if (argc == 2 && std::strcmp(argv[1], "test") == 0) {
        // ry test — auto-discover *.test.ry files
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        InitializeNativeTargetAsmParser();

        auto root = findProjectRoot();
        if (!root) {
            errs() << "Error: ry.toml not found. Run 'ry init' first.\n";
            return 1;
        }

        auto test_files = findTestFiles(*root);
        if (test_files.empty()) {
            errs() << "No *.test.ry files found.\n";
            return 1;
        }

        int total_failed = 0;
        int total_files = 0;
        for (const auto &tf : test_files) {
            ++total_files;
            try {
                int failed = runRyFile(tf, /*test_mode=*/true, argv[0]);
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
        return total_failed > 0 ? 1 : 0;
    } else {
        errs() << "Usage: ry <file.ry> [args...]\n";
        errs() << "       ry test [<file.ry>]\n";
        errs() << "       ry init\n";
        errs() << "       ry self-update [--nightly | <version>]\n";
        errs() << "       ry --version\n";
        return 1;
    }

    // Initialize native target
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    try {
        return runRyFile(filename, test_mode, argv[0]);
    } catch (const DiagnosticError &e) {
        errs() << e.what();
        return 1;
    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
