#pragma once

#include <gtest/gtest.h>
#include "ry/codegen.hpp"
#include "ry/jit.hpp"
#include "ry/parser.hpp"
#include "ry/args_runtime.hpp"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ry;
using namespace llvm;
using namespace llvm::orc;

class CodeGenTest : public ::testing::Test {
public:
    static void SetUpTestSuite() {
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        InitializeNativeTargetAsmParser();
        GTEST_FLAG_SET(death_test_style, "threadsafe");
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
        try {
            symOrErr->toPtr<int(*)()>()();
            fflush(stdout);
            return testing::internal::GetCapturedStdout();
        } catch (...) {
            testing::internal::GetCapturedStdout();
            throw;
        }
    }

    static ThreadSafeModule compileSource(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg;
        return cg.compile(prog);
    }

    static std::string runSource(const std::string &src) {
        return runModule(compileSource(src));
    }

    static void expectCompileError(const std::string &src, const std::string &fragment) {
        try {
            compileSource(src);
            FAIL() << "Expected compile error containing: " << fragment;
        } catch (const std::runtime_error &e) {
            std::string msg = e.what();
            EXPECT_NE(msg.find(fragment), std::string::npos)
                << "Expected error to contain: '" << fragment << "', got: " << msg;
        }
    }

    static void expectCompileError(const std::string &src,
                                   std::initializer_list<std::string> fragments) {
        try {
            compileSource(src);
            FAIL() << "Expected compile error";
        } catch (const std::runtime_error &e) {
            std::string msg = e.what();
            for (const auto &frag : fragments)
                EXPECT_NE(msg.find(frag), std::string::npos)
                    << "Expected error to contain: '" << frag << "', got: " << msg;
        }
    }

    static std::string runTestSource(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg(true);  // test_mode = true
        auto tsm = cg.compile(prog);
        return runModule(std::move(tsm));
    }

    static std::string runSourceWithArgs(const std::string &src, const std::vector<std::string> &args) {
        std::vector<const char*> argv_ptrs;
        std::vector<std::string> owned_args(args);
        argv_ptrs.reserve(owned_args.size());
        for (const auto &a : owned_args)
            argv_ptrs.push_back(a.c_str());
        __ry_args_init(static_cast<int>(argv_ptrs.size()),
                       argv_ptrs.empty() ? nullptr : argv_ptrs.data());
        try {
            auto result = runSource(src);
            __ry_args_init(0, nullptr);
            return result;
        } catch (...) {
            __ry_args_init(0, nullptr);
            throw;
        }
    }

    static std::pair<std::string, std::vector<std::string>> runSourceWithWarnings(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg;
        auto tsm = cg.compile(prog);
        auto warnings = cg.getWarnings();
        return {runModule(std::move(tsm)), warnings};
    }

    static std::pair<std::string, std::vector<std::string>> runTestSourceWithWarnings(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg(true);  // test_mode = true
        auto tsm = cg.compile(prog);
        auto warnings = cg.getWarnings();
        return {runModule(std::move(tsm)), warnings};
    }
};
