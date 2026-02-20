#include "ry.hpp"

int main(int argc, char *argv[]) {
    InitLLVM X(argc, argv);

    if (argc != 2) {
        errs() << "Usage: ry <file.ry>\n";
        return 1;
    }

    // Initialize native target
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Read source file
    auto bufOrErr = MemoryBuffer::getFile(argv[1]);
    if (!bufOrErr) {
        errs() << "Error reading file: " << argv[1] << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();

    try {
        // Lex -> Parse
        Lexer  lexer(src);
        Parser parser(lexer);
        Program prog = parser.parseProgram();

        // CodeGen -> ThreadSafeModule
        CodeGen cg;
        ThreadSafeModule tsm = cg.compile(prog);

        // Build LLJIT
        auto jitOrErr = LLJITBuilder().create();
        if (!jitOrErr) {
            errs() << "Failed to create JIT: ";
            logAllUnhandledErrors(jitOrErr.takeError(), errs());
            return 1;
        }
        auto &jit = *jitOrErr;

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
        fn();

    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
