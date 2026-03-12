#pragma once

#include <gtest/gtest.h>
#include "ry/codegen.hpp"
#include "ry/jit.hpp"
#include "ry/parser.hpp"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
using namespace llvm;
using namespace llvm::orc;

#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <string>

class CodeGenTest : public ::testing::Test {
public:
    static void SetUpTestSuite() {
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        InitializeNativeTargetAsmParser();
    }

protected:
    static std::string runModule(ThreadSafeModule tsm) {
        auto jitOrErr = LLJITBuilder().create();
        if (!jitOrErr) {
            llvm::consumeError(jitOrErr.takeError());
            throw std::runtime_error("Failed to create JIT");
        }
        auto &jit = *jitOrErr;

        jit->getIRTransformLayer().setTransform(ry::optimizeModule);

        auto &mainJD = jit->getMainJITDylib();
        auto dlsg = DynamicLibrarySearchGenerator::GetForCurrentProcess(
            jit->getDataLayout().getGlobalPrefix());
        if (!dlsg) {
            llvm::consumeError(dlsg.takeError());
            throw std::runtime_error("Failed to create DynamicLibrarySearchGenerator");
        }
        mainJD.addGenerator(std::move(*dlsg));

        if (auto err = jit->addIRModule(std::move(tsm))) {
            llvm::consumeError(std::move(err));
            throw std::runtime_error("Failed to add IR module");
        }

        auto symOrErr = jit->lookup("__ry_main__");
        if (!symOrErr) {
            llvm::consumeError(symOrErr.takeError());
            throw std::runtime_error("Failed to lookup __ry_main__");
        }

        testing::internal::CaptureStdout();
        symOrErr->toPtr<int(*)()>()();
        fflush(stdout);
        return testing::internal::GetCapturedStdout();
    }

    static std::string runSource(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg;
        auto tsm = cg.compile(prog);
        return runModule(std::move(tsm));
    }
};
