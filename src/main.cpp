#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/module_loader.hpp"
#include "ry/codegen.hpp"
#include "ry/jit.hpp"
#include "ry/test_runtime.hpp"
#include "ry/project_config.hpp"
#include <cstring>
#include <filesystem>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
using namespace llvm;
using namespace llvm::orc;

int main(int argc, char *argv[]) {
    if (argc == 2 && (std::strcmp(argv[1], "--version") == 0 || std::strcmp(argv[1], "-v") == 0)) {
        llvm::outs() << "ry " << RY_VERSION << "\n";
        return 0;
    }

    // Handle subcommands that don't need LLVM initialization
    if (argc >= 2 && std::strcmp(argv[1], "init") == 0) {
        return cmd_init();
    }

    InitLLVM X(argc, argv);

    bool test_mode = false;
    const char *filename = nullptr;

    if (argc == 2) {
        filename = argv[1];
    } else if (argc == 3 && std::strcmp(argv[1], "test") == 0) {
        test_mode = true;
        filename = argv[2];
    } else {
        errs() << "Usage: ry <file.ry>\n";
        errs() << "       ry test <file.ry>\n";
        errs() << "       ry init\n";
        errs() << "       ry --version\n";
        return 1;
    }

    // Initialize native target
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Read source file
    auto bufOrErr = MemoryBuffer::getFile(filename);
    if (!bufOrErr) {
        errs() << "Error reading file: " << filename << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();

    try {
        // Lex -> Parse
        Lexer  lexer(src);
        Parser parser(lexer);
        Program prog = parser.parseProgram();

        // Resolve imports
        std::string referrer_dir = std::filesystem::path(filename).parent_path().string();
        ModuleLoader loader;
        prog = loader.resolveImports(prog, referrer_dir);

        // CodeGen -> ThreadSafeModule
        CodeGen cg(test_mode);
        ThreadSafeModule tsm = cg.compile(prog);

        // Build LLJIT
        auto jitOrErr = LLJITBuilder().create();
        if (!jitOrErr) {
            errs() << "Failed to create JIT: ";
            logAllUnhandledErrors(jitOrErr.takeError(), errs());
            return 1;
        }
        auto &jit = *jitOrErr;

        // Apply O2 optimization pipeline
        jit->getIRTransformLayer().setTransform(ry::optimizeModule);

        // Expose process symbols (for printf etc.)
        auto &es = jit->getExecutionSession();
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
        return result;

    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
