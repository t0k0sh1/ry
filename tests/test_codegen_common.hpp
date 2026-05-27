#pragma once

#include <gtest/gtest.h>
#include "ry/codegen.hpp"
#include "ry/jit/jit.hpp"
#include "ry/parser/parser.hpp"
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

    // Appended after user source so that line numbers in the user source
    // are not shifted (tests like FailWithMessage assert on specific line
    // numbers in `fail()` output).  Mirrors share/std/testing/testing.ry's
    // declarations of `verify` and the underlying `@native` runtime call
    // (`_mockGetCallCount`).  `fail` is a pure codegen intrinsic — codegen
    // intercepts the callee name in `emitFailCall` and emits a direct call
    // to `__ry_test_fail`, so no Ry declaration is needed here.
    static std::string withTestingFnDecls(const std::string &src) {
        static const char *kDecls =
            "\n"
            "@native(\"testing\")\n"
            "fn _mockGetCallCount(name: str) -> int\n"
            "@public\n"
            "fn verify(name: str) -> int:\n"
            "  return _mockGetCallCount(name)\n"
            "@native(\"testing\")\n"
            "fn _mockClear(name: str) -> Unit\n"
            "@public\n"
            "fn mockClear(name: str) -> Unit:\n"
            "  _mockClear(name)\n"
            "@native(\"testing\")\n"
            "fn _mockReset(name: str) -> Unit\n"
            "@public\n"
            "fn mockReset(name: str) -> Unit:\n"
            "  _mockReset(name)\n"
            "@native(\"testing\")\n"
            "fn _mockResetAll() -> Unit\n"
            "@public\n"
            "fn mockResetAll() -> Unit:\n"
            "  _mockResetAll()\n";
        return src + kDecls;
    }

    // The codegen test harness skips ModuleLoader (see CodeGenTest::compileSource).
    // In production, jit_runner.cpp:165 calls
    // `cg.setTestingIntrinsicsImported(loader.importedTestingIntrinsics())`
    // so codegen knows which testing intrinsics the source imported. For the
    // test harness we mimic the wildcard `from testing` import for the five
    // enforced names (expect/mock/fail from #715, plus it/describe from #716),
    // so existing tests that embed those intrinsics or directives literally
    // in their source strings continue to compile. Negative tests that
    // intentionally exercise the missing-import error must use
    // `runTestSourceNoTestingImports` instead.
    //
    // #722 made `verify` a Ry-level function (declared in
    // share/std/testing/testing.ry, body emitted via emitUserFnCall), so the
    // harness appends `verify` and its `@native` backing declaration
    // (`_mockGetCallCount`) after the user source so codegen can find it in
    // user_fns_.  `fail` reverted to a pure codegen intrinsic in #1690 —
    // intercepted by name in `emitFailCall`, no Ry declaration needed.
    static std::string runTestSource(const std::string &src) {
        std::string augmented = withTestingFnDecls(src);
        Lexer lex(augmented);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg(true);  // test_mode = true
        cg.setTestingIntrinsicsImported({"expect", "mock", "fail", "it", "describe",
                                          "verifyCalledWith", "spy"});
        auto tsm = cg.compile(prog);
        return runModule(std::move(tsm));
    }

    // Same as runTestSource but does NOT inject any testing intrinsic imports.
    // Use this when writing a negative test that asserts the
    // `'<name>' requires 'from testing import <name>'` enforcement fires.
    static std::string runTestSourceNoTestingImports(const std::string &src) {
        std::string augmented = withTestingFnDecls(src);
        Lexer lex(augmented);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg(true);
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

};

// As of #1390, the non-bootstrap built-in directives (@inline / @parallel /
// @const / @deprecated / @each / @property) live in stdlib .ry sources and
// the @it / @describe directives live in share/std/testing/testing.ry, none
// of which are loaded by the codegen test harness (it skips ModuleLoader).
// Tests that exercise these directives must declare them inline at the top
// of the source they hand to the codegen.
inline std::string withStdlibDirectiveDecls(const std::string &src) {
    static const char *kDecls =
        "@directive(target=[\"function\"])\n"
        "fn inline(mode: str = \"always\")\n"
        "@directive(target=[\"for\"])\n"
        "fn parallel()\n"
        "@directive(target=[\"statement\"])\n"
        "fn const()\n"
        "@directive(target=[\"function\", \"record\", \"field\", \"statement\"])\n"
        "fn deprecated(reason: str = \"\")\n"
        "@directive(target=[\"function\"])\n"
        "fn it(description: str)\n"
        "@directive(target=[\"function\"])\n"
        "fn describe(group: str)\n"
        "@directive(target=[\"statement\", \"function\"])\n"
        "fn each(data: any)\n"
        "@directive(target=[\"statement\", \"function\"])\n"
        "fn property(count: int = 100)\n"
        "@directive(target=[\"function\"])\n"
        "fn skip()\n"
        "@directive(target=[\"function\"])\n"
        "fn only()\n"
        "@directive(target=[\"function\"])\n"
        "fn todo()\n"
        "@directive(target=[\"function\"])\n"
        "fn timeout(ms: int)\n"
        "@directive(target=[\"function\"])\n"
        "fn beforeEach()\n"
        "@directive(target=[\"function\"])\n"
        "fn afterEach()\n"
        "@directive(target=[\"function\"])\n"
        "fn beforeAll()\n"
        "@directive(target=[\"function\"])\n"
        "fn afterAll()\n";
    return std::string(kDecls) + src;
}

// `from testing import ...` is satisfied at runtime by ModuleLoader, but the
// codegen test harness above skips ModuleLoader. `withStdlibDirectiveDecls`
// already injects the @it / @describe declarations inline, so the import
// line in migrated `tests/spec/*.test.ry` files (#1599) must be stripped
// before the source reaches the harness — otherwise it fails as
// "unresolved import: testing".
inline std::string stripTestingImport(const std::string &src) {
    static const std::string prefix = "from testing import ";
    if (src.compare(0, prefix.size(), prefix) != 0) {
        return src;
    }
    auto nl = src.find('\n');
    if (nl == std::string::npos) {
        return src;
    }
    return src.substr(nl + 1);
}
