#include <gtest/gtest.h>
#include "ry/codegen.hpp"
#include "ry/parser.hpp"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
using namespace llvm;
using namespace llvm::orc;

#include <cstdio>
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
    static std::string runSource(const std::string &src) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        CodeGen cg;
        auto tsm = cg.compile(prog);

        auto jitOrErr = LLJITBuilder().create();
        if (!jitOrErr) {
            llvm::consumeError(jitOrErr.takeError());
            throw std::runtime_error("Failed to create JIT");
        }
        auto &jit = *jitOrErr;

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
};

TEST_F(CodeGenTest, PrintInteger) {
    EXPECT_EQ(runSource("print(42)"), "42\n");
}

TEST_F(CodeGenTest, PrintNegativeInteger) {
    EXPECT_EQ(runSource("x = -10\nprint(x)"), "-10\n");
}

TEST_F(CodeGenTest, PrintFloat) {
    EXPECT_EQ(runSource("print(3.14)"), "3.14\n");
}

TEST_F(CodeGenTest, PrintFloatWhole) {
    // %g 形式: 1.0 → "1"
    EXPECT_EQ(runSource("print(1.0)"), "1\n");
}

TEST_F(CodeGenTest, PrintTrue) {
    EXPECT_EQ(runSource("print(true)"), "true\n");
}

TEST_F(CodeGenTest, PrintFalse) {
    EXPECT_EQ(runSource("print(false)"), "false\n");
}

TEST_F(CodeGenTest, AddIntegers) {
    EXPECT_EQ(runSource("x = 3 + 4\nprint(x)"), "7\n");
}

TEST_F(CodeGenTest, Modulo) {
    EXPECT_EQ(runSource("x = 10 % 3\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, IntegerDivision) {
    EXPECT_EQ(runSource("x = 7 // 2\nprint(x)"), "3\n");
}

TEST_F(CodeGenTest, FloatDivision) {
    EXPECT_EQ(runSource("x = 7 / 2\nprint(x)"), "3.5\n");
}

TEST_F(CodeGenTest, Exponentiation) {
    // 2 ** 10 = 1024 (f64 → %g → "1024")
    EXPECT_EQ(runSource("x = 2 ** 10\nprint(x)"), "1024\n");
}

TEST_F(CodeGenTest, ExponentiationRightAssoc) {
    // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
    EXPECT_EQ(runSource("x = 2 ** 3 ** 2\nprint(x)"), "512\n");
}

TEST_F(CodeGenTest, EqualTrue) {
    EXPECT_EQ(runSource("x = 1 == 1\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, EqualFalse) {
    EXPECT_EQ(runSource("x = 1 == 2\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LessThan) {
    EXPECT_EQ(runSource("x = 3 < 5\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, LogicalAnd) {
    EXPECT_EQ(runSource("x = true and true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = true and false\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LogicalOr) {
    EXPECT_EQ(runSource("x = false or true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = false or false\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LogicalNot) {
    EXPECT_EQ(runSource("x = not true\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = not false\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, NotNotTrue) {
    EXPECT_EQ(runSource("x = not not true\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, VariableReassignment) {
    EXPECT_EQ(runSource("x = 1\nx = 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, MultipleStatements) {
    EXPECT_EQ(runSource("print(1)\nprint(2)\nprint(3)"), "1\n2\n3\n");
}

TEST_F(CodeGenTest, UndefinedVariableThrows) {
    EXPECT_THROW(runSource("print(z)"), std::runtime_error);
}

TEST_F(CodeGenTest, UnknownFunctionThrows) {
    EXPECT_THROW(runSource("foo(42)"), std::runtime_error);
}

TEST_F(CodeGenTest, SubtractIntegers) {
    EXPECT_EQ(runSource("x = 5 - 3\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, MultiplyIntegers) {
    EXPECT_EQ(runSource("x = 3 * 4\nprint(x)"), "12\n");
}

TEST_F(CodeGenTest, UnaryPlus) {
    EXPECT_EQ(runSource("x = +5\nprint(x)"), "5\n");
}

TEST_F(CodeGenTest, NotEqual) {
    EXPECT_EQ(runSource("x = 1 != 2\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 2 != 2\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LessOrEqual) {
    EXPECT_EQ(runSource("x = 3 <= 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 4 <= 3\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, GreaterThan) {
    EXPECT_EQ(runSource("x = 5 > 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 3 > 5\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, GreaterOrEqual) {
    EXPECT_EQ(runSource("x = 5 >= 5\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 4 >= 5\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, FloatArithmetic) {
    EXPECT_EQ(runSource("x = 1.5 + 2.5\nprint(x)"), "4\n");
}

TEST_F(CodeGenTest, MixedIntFloat) {
    EXPECT_EQ(runSource("x = 1 + 2.5\nprint(x)"), "3.5\n");
}

TEST_F(CodeGenTest, ModuloFloat) {
    EXPECT_EQ(runSource("x = 5.5 % 2.0\nprint(x)"), "1.5\n");
}

// ===== ビット演算子テスト =====

TEST_F(CodeGenTest, BitwiseAnd) {
    // 1100 & 1010 = 1000 = 8
    EXPECT_EQ(runSource("x = 12 & 10\nprint(x)"), "8\n");
}

TEST_F(CodeGenTest, BitwiseOr) {
    // 1100 | 1010 = 1110 = 14
    EXPECT_EQ(runSource("x = 12 | 10\nprint(x)"), "14\n");
}

TEST_F(CodeGenTest, BitwiseXor) {
    // 1100 ^ 1010 = 0110 = 6
    EXPECT_EQ(runSource("x = 12 ^ 10\nprint(x)"), "6\n");
}

TEST_F(CodeGenTest, LeftShift) {
    EXPECT_EQ(runSource("x = 1 << 3\nprint(x)"), "8\n");
}

TEST_F(CodeGenTest, RightShift) {
    EXPECT_EQ(runSource("x = 16 >> 2\nprint(x)"), "4\n");
}

TEST_F(CodeGenTest, RightShiftNeg) {
    // 算術右シフト: -8 >> 1 = -4
    EXPECT_EQ(runSource("x = -8 >> 1\nprint(x)"), "-4\n");
}

TEST_F(CodeGenTest, BitwiseNot) {
    // ~0 = -1
    EXPECT_EQ(runSource("x = ~0\nprint(x)"), "-1\n");
}

TEST_F(CodeGenTest, BitwiseNotOne) {
    // ~1 = -2
    EXPECT_EQ(runSource("x = ~1\nprint(x)"), "-2\n");
}

TEST_F(CodeGenTest, BitwiseBoolZExt) {
    // true(i1=1) & 3 → ZExt → 1 & 3 = 1
    EXPECT_EQ(runSource("x = true & 3\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, BitwiseFloatThrows) {
    EXPECT_THROW(runSource("x = 1.0 & 2"), std::runtime_error);
}

TEST_F(CodeGenTest, BitwisePrecedenceAndOverOr) {
    // 1 | 2 & 3 = 1 | (2&3) = 1 | 2 = 3
    EXPECT_EQ(runSource("x = 1 | 2 & 3\nprint(x)"), "3\n");
}

TEST_F(CodeGenTest, BitwisePrecedenceShiftOverOr) {
    // 1 | 2 << 1 = 1 | (2<<1) = 1 | 4 = 5
    EXPECT_EQ(runSource("x = 1 | 2 << 1\nprint(x)"), "5\n");
}

TEST_F(CodeGenTest, BitwisePrecedenceBitOverCmp) {
    // 3 & 5 == 1 → (3&5)=1, 1==1=true
    EXPECT_EQ(runSource("x = 3 & 5 == 1\nprint(x)"), "true\n");
}

// ===== 型変更再代入テスト =====

TEST_F(CodeGenTest, TypeChangeIntToFloatThrows) {
    EXPECT_THROW(runSource("x = 1\nx = 1.5\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeFloatToIntThrows) {
    EXPECT_THROW(runSource("x = 1.5\nx = 2\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeIntToBoolThrows) {
    EXPECT_THROW(runSource("x = 1\nx = true\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeBoolToIntThrows) {
    EXPECT_THROW(runSource("x = true\nx = 1\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, SameTypeReassignmentFloat) {
    EXPECT_EQ(runSource("x = 1.5\nx = 2.5\nprint(x)"), "2.5\n");
}

TEST_F(CodeGenTest, TypeChangeAfterUseThrows) {
    EXPECT_THROW(runSource("x = 1\nprint(x)\nx = 1.5"), std::runtime_error);
}

// ===== 型アノテーションテスト =====

TEST_F(CodeGenTest, TypeAnnotationIntMatch) {
    EXPECT_EQ(runSource("a: int = 10\nprint(a)"), "10\n");
}

TEST_F(CodeGenTest, TypeAnnotationFloatMatch) {
    EXPECT_EQ(runSource("b: float = 3.14\nprint(b)"), "3.14\n");
}

TEST_F(CodeGenTest, TypeAnnotationBoolMatch) {
    EXPECT_EQ(runSource("c: bool = true\nprint(c)"), "true\n");
}

TEST_F(CodeGenTest, TypeAnnotationIntMismatchThrows) {
    EXPECT_THROW(runSource("a: int = 3.14"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAnnotationFloatMismatchThrows) {
    EXPECT_THROW(runSource("a: float = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAnnotationBoolMismatchThrows) {
    EXPECT_THROW(runSource("a: bool = 10"), std::runtime_error);
}
