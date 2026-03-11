#include <gtest/gtest.h>
#include "ry/codegen.hpp"
#include "ry/jit.hpp"
#include "ry/module_loader.hpp"
#include "ry/parser.hpp"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
using namespace llvm;
using namespace llvm::orc;

#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
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
};

TEST_F(CodeGenTest, PrintInteger) {
    EXPECT_EQ(runSource("print(42)"), "42\n");
}

TEST_F(CodeGenTest, PrintNegativeInteger) {
    EXPECT_EQ(runSource("let x = -10\nprint(x)"), "-10\n");
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
    EXPECT_EQ(runSource("let x = 3 + 4\nprint(x)"), "7\n");
}

TEST_F(CodeGenTest, Modulo) {
    EXPECT_EQ(runSource("let x = 10 % 3\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, IntegerDivision) {
    EXPECT_EQ(runSource("let x = 7 // 2\nprint(x)"), "3\n");
}

TEST_F(CodeGenTest, FloatDivision) {
    EXPECT_EQ(runSource("let x = 7 / 2\nprint(x)"), "3.5\n");
}

TEST_F(CodeGenTest, Exponentiation) {
    // 2 ** 10 = 1024 (f64 → %g → "1024")
    EXPECT_EQ(runSource("let x = 2 ** 10\nprint(x)"), "1024\n");
}

TEST_F(CodeGenTest, ExponentiationRightAssoc) {
    // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
    EXPECT_EQ(runSource("let x = 2 ** 3 ** 2\nprint(x)"), "512\n");
}

TEST_F(CodeGenTest, EqualTrue) {
    EXPECT_EQ(runSource("let x = 1 == 1\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, EqualFalse) {
    EXPECT_EQ(runSource("let x = 1 == 2\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LessThan) {
    EXPECT_EQ(runSource("let x = 3 < 5\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, LogicalAnd) {
    EXPECT_EQ(runSource("let x = true and true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = true and false\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LogicalOr) {
    EXPECT_EQ(runSource("let x = false or true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = false or false\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LogicalNot) {
    EXPECT_EQ(runSource("let x = not true\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("let x = not false\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, NotNotTrue) {
    EXPECT_EQ(runSource("let x = not not true\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, VariableReassignment) {
    EXPECT_EQ(runSource("let x = 1\nx = 2\nprint(x)"), "2\n");
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
    EXPECT_EQ(runSource("let x = 5 - 3\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, MultiplyIntegers) {
    EXPECT_EQ(runSource("let x = 3 * 4\nprint(x)"), "12\n");
}

TEST_F(CodeGenTest, UnaryPlus) {
    EXPECT_EQ(runSource("let x = +5\nprint(x)"), "5\n");
}

TEST_F(CodeGenTest, NotEqual) {
    EXPECT_EQ(runSource("let x = 1 != 2\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = 2 != 2\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, LessOrEqual) {
    EXPECT_EQ(runSource("let x = 3 <= 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = 4 <= 3\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, GreaterThan) {
    EXPECT_EQ(runSource("let x = 5 > 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = 3 > 5\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, GreaterOrEqual) {
    EXPECT_EQ(runSource("let x = 5 >= 5\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("let x = 4 >= 5\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, FloatArithmetic) {
    EXPECT_EQ(runSource("let x = 1.5 + 2.5\nprint(x)"), "4\n");
}

TEST_F(CodeGenTest, MixedIntFloat) {
    EXPECT_EQ(runSource("let x = 1 + 2.5\nprint(x)"), "3.5\n");
}

TEST_F(CodeGenTest, ModuloFloat) {
    EXPECT_EQ(runSource("let x = 5.5 % 2.0\nprint(x)"), "1.5\n");
}

// ===== ビット演算子テスト =====

TEST_F(CodeGenTest, BitwiseAnd) {
    // 1100 & 1010 = 1000 = 8
    EXPECT_EQ(runSource("let x = 12 & 10\nprint(x)"), "8\n");
}

TEST_F(CodeGenTest, BitwiseOr) {
    // 1100 | 1010 = 1110 = 14
    EXPECT_EQ(runSource("let x = 12 | 10\nprint(x)"), "14\n");
}

TEST_F(CodeGenTest, BitwiseXor) {
    // 1100 ^ 1010 = 0110 = 6
    EXPECT_EQ(runSource("let x = 12 ^ 10\nprint(x)"), "6\n");
}

TEST_F(CodeGenTest, LeftShift) {
    EXPECT_EQ(runSource("let x = 1 << 3\nprint(x)"), "8\n");
}

TEST_F(CodeGenTest, RightShift) {
    EXPECT_EQ(runSource("let x = 16 >> 2\nprint(x)"), "4\n");
}

TEST_F(CodeGenTest, RightShiftNeg) {
    // 算術右シフト: -8 >> 1 = -4
    EXPECT_EQ(runSource("let x = -8 >> 1\nprint(x)"), "-4\n");
}

TEST_F(CodeGenTest, BitwiseNot) {
    // ~0 = -1
    EXPECT_EQ(runSource("let x = ~0\nprint(x)"), "-1\n");
}

TEST_F(CodeGenTest, BitwiseNotOne) {
    // ~1 = -2
    EXPECT_EQ(runSource("let x = ~1\nprint(x)"), "-2\n");
}

TEST_F(CodeGenTest, BitwiseBoolZExt) {
    // true(i1=1) & 3 → ZExt → 1 & 3 = 1
    EXPECT_EQ(runSource("let x = true & 3\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, BitwiseFloatThrows) {
    EXPECT_THROW(runSource("let x = 1.0 & 2"), std::runtime_error);
}

TEST_F(CodeGenTest, BitwisePrecedenceAndOverOr) {
    // 1 | 2 & 3 = 1 | (2&3) = 1 | 2 = 3
    EXPECT_EQ(runSource("let x = 1 | 2 & 3\nprint(x)"), "3\n");
}

TEST_F(CodeGenTest, BitwisePrecedenceShiftOverOr) {
    // 1 | 2 << 1 = 1 | (2<<1) = 1 | 4 = 5
    EXPECT_EQ(runSource("let x = 1 | 2 << 1\nprint(x)"), "5\n");
}

TEST_F(CodeGenTest, BitwisePrecedenceBitOverCmp) {
    // 3 & 5 == 1 → (3&5)=1, 1==1=true
    EXPECT_EQ(runSource("let x = 3 & 5 == 1\nprint(x)"), "true\n");
}

// ===== string 型テスト =====

TEST_F(CodeGenTest, PrintStringLiteral) {
    EXPECT_EQ(runSource("print(\"hello\")"), "hello\n");
}

TEST_F(CodeGenTest, PrintEmptyString) {
    EXPECT_EQ(runSource("print(\"\")"), "\n");
}

TEST_F(CodeGenTest, LetStringVariable) {
    EXPECT_EQ(runSource("let s = \"world\"\nprint(s)"), "world\n");
}

TEST_F(CodeGenTest, ConstStringVariable) {
    EXPECT_EQ(runSource("const s = \"hello\"\nprint(s)"), "hello\n");
}

TEST_F(CodeGenTest, StringTypeAnnotation) {
    EXPECT_EQ(runSource("let s: str = \"typed\"\nprint(s)"), "typed\n");
}

TEST_F(CodeGenTest, StringTypeMismatchThrows) {
    EXPECT_THROW(runSource("let s: str = 42"), std::runtime_error);
}

TEST_F(CodeGenTest, IntTypeAnnotationStrThrows) {
    EXPECT_THROW(runSource("let s: int = \"hello\""), std::runtime_error);
}

// ===== 型変更再代入テスト =====

TEST_F(CodeGenTest, TypeChangeIntToFloatThrows) {
    EXPECT_THROW(runSource("let x = 1\nx = 1.5\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeFloatToIntThrows) {
    EXPECT_THROW(runSource("let x = 1.5\nx = 2\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeIntToBoolThrows) {
    EXPECT_THROW(runSource("let x = 1\nx = true\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeBoolToIntThrows) {
    EXPECT_THROW(runSource("let x = true\nx = 1\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, SameTypeReassignmentFloat) {
    EXPECT_EQ(runSource("let x = 1.5\nx = 2.5\nprint(x)"), "2.5\n");
}

TEST_F(CodeGenTest, TypeChangeAfterUseThrows) {
    EXPECT_THROW(runSource("let x = 1\nprint(x)\nx = 1.5"), std::runtime_error);
}

// ===== 型アノテーションテスト =====

TEST_F(CodeGenTest, TypeAnnotationIntMatch) {
    EXPECT_EQ(runSource("let a: int = 10\nprint(a)"), "10\n");
}

TEST_F(CodeGenTest, TypeAnnotationFloatMatch) {
    EXPECT_EQ(runSource("let b: float = 3.14\nprint(b)"), "3.14\n");
}

TEST_F(CodeGenTest, TypeAnnotationBoolMatch) {
    EXPECT_EQ(runSource("let c: bool = true\nprint(c)"), "true\n");
}

TEST_F(CodeGenTest, TypeAnnotationIntMismatchThrows) {
    EXPECT_THROW(runSource("let a: int = 3.14"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAnnotationFloatMismatchThrows) {
    EXPECT_THROW(runSource("let a: float = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAnnotationBoolMismatchThrows) {
    EXPECT_THROW(runSource("let a: bool = 10"), std::runtime_error);
}

// ===== let / const テスト =====

TEST_F(CodeGenTest, LetPrint) {
    EXPECT_EQ(runSource("let x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, ConstPrint) {
    EXPECT_EQ(runSource("const x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, LetReassign) {
    EXPECT_EQ(runSource("let x = 1\nx = 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, ConstReassignThrows) {
    EXPECT_THROW(runSource("const x = 1\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, UndeclaredAssignThrows) {
    EXPECT_THROW(runSource("x = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, RedeclaredLetThrows) {
    EXPECT_THROW(runSource("let x = 1\nlet x = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, RedeclaredConstThrows) {
    EXPECT_THROW(runSource("const x = 1\nconst x = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, ConstWithTypeAnnotation) {
    EXPECT_EQ(runSource("const x: int = 42\nprint(x)"), "42\n");
}

// ===== if/elif/else codegen テスト =====

TEST_F(CodeGenTest, IfTrueExecutes) {
    EXPECT_EQ(runSource("if true:\n    print(1)"), "1\n");
}

TEST_F(CodeGenTest, IfFalseDoesNotExecute) {
    EXPECT_EQ(runSource("if false:\n    print(1)"), "");
}

TEST_F(CodeGenTest, IfElseTrueBranch) {
    EXPECT_EQ(runSource("if true:\n    print(1)\nelse:\n    print(2)"), "1\n");
}

TEST_F(CodeGenTest, IfElseFalseBranch) {
    EXPECT_EQ(runSource("if false:\n    print(1)\nelse:\n    print(2)"), "2\n");
}

TEST_F(CodeGenTest, IfElifElseChain) {
    std::string src =
        "let x = 2\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "else:\n"
        "    print(30)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, IfElifElseChainElse) {
    std::string src =
        "let x = 99\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "else:\n"
        "    print(30)";
    EXPECT_EQ(runSource(src), "30\n");
}

TEST_F(CodeGenTest, IfVariableReassignment) {
    std::string src =
        "let x = 0\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, IfNonBoolConditionInt) {
    EXPECT_EQ(runSource("if 1:\n    print(1)"), "1\n");
    EXPECT_EQ(runSource("if 0:\n    print(1)"), "");
}

TEST_F(CodeGenTest, NestedIf) {
    std::string src =
        "if true:\n"
        "    if true:\n"
        "        print(1)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, IfFollowedByStatement) {
    std::string src =
        "if true:\n"
        "    print(1)\n"
        "print(2)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

TEST_F(CodeGenTest, IfFalseFollowedByStatement) {
    std::string src =
        "if false:\n"
        "    print(1)\n"
        "print(2)";
    EXPECT_EQ(runSource(src), "2\n");
}

// ===== while codegen テスト =====

TEST_F(CodeGenTest, WhileFalseDoesNotExecute) {
    EXPECT_EQ(runSource("while false:\n    print(1)"), "");
}

TEST_F(CodeGenTest, WhileCountdown) {
    std::string src =
        "let i = 3\n"
        "while i > 0:\n"
        "    print(i)\n"
        "    i = i - 1";
    EXPECT_EQ(runSource(src), "3\n2\n1\n");
}

TEST_F(CodeGenTest, WhileFollowedByStatement) {
    std::string src =
        "let i = 0\n"
        "while i < 2:\n"
        "    i = i + 1\n"
        "print(i)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, WhileZeroCondition) {
    EXPECT_EQ(runSource("while 0:\n    print(1)"), "");
}

TEST_F(CodeGenTest, WhileNestedWhile) {
    std::string src =
        "let i = 0\n"
        "while i < 2:\n"
        "    let j = 0\n"
        "    while j < 2:\n"
        "        print(i + j)\n"
        "        j = j + 1\n"
        "    i = i + 1";
    EXPECT_EQ(runSource(src), "0\n1\n1\n2\n");
}

TEST_F(CodeGenTest, WhileWithIf) {
    std::string src =
        "let i = 1\n"
        "while i <= 4:\n"
        "    if i % 2 == 0:\n"
        "        print(i)\n"
        "    i = i + 1";
    EXPECT_EQ(runSource(src), "2\n4\n");
}

// ===== fn 関数定義テスト =====

TEST_F(CodeGenTest, FnBasicAddCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "let result = add(1, 2)\n"
        "print(result)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, FnNoArgs) {
    std::string src =
        "fn zero() -> int:\n"
        "    return 0\n"
        "print(zero())";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, FnFloatReturn) {
    std::string src =
        "fn half(x: float) -> float:\n"
        "    return x / 2.0\n"
        "let r = half(7.0)\n"
        "print(r)";
    EXPECT_EQ(runSource(src), "3.5\n");
}

TEST_F(CodeGenTest, FnBoolReturn) {
    std::string src =
        "fn is_positive(x: int) -> bool:\n"
        "    return x > 0\n"
        "print(is_positive(5))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, FnWithIf) {
    std::string src =
        "fn abs(x: int) -> int:\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-5))\n"
        "print(abs(3))";
    EXPECT_EQ(runSource(src), "5\n3\n");
}

TEST_F(CodeGenTest, FnCallAsStatement) {
    std::string src =
        "fn greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n"
        "greet()";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, FnRecursiveFactorial) {
    std::string src =
        "fn factorial(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * factorial(n - 1)\n"
        "print(factorial(5))";
    EXPECT_EQ(runSource(src), "120\n");
}

TEST_F(CodeGenTest, FnUndefinedCallThrows) {
    EXPECT_THROW(runSource("let x = unknown(1)"), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgCountMismatchThrows) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "let x = add(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgTypeMismatchThrows) {
    std::string src =
        "fn inc(a: int) -> int:\n"
        "    return a + 1\n"
        "let x = inc(1.5)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FnReturnTypeMismatchThrows) {
    std::string src =
        "fn bad() -> int:\n"
        "    return 1.5";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== ブロックスコープテスト =====

TEST_F(CodeGenTest, BlockScopeIfVarNotVisible) {
    std::string src =
        "if true:\n"
        "    let x = 42\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeWhileVarNotVisible) {
    std::string src =
        "let i = 1\n"
        "while i > 0:\n"
        "    let x = 99\n"
        "    i = 0\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeElseVarNotVisible) {
    std::string src =
        "if false:\n"
        "    let a = 1\n"
        "else:\n"
        "    let b = 2\n"
        "print(b)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeInnerAccessesOuter) {
    std::string src =
        "let x = 1\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, BlockScopeShadowing) {
    std::string src =
        "let x = 1\n"
        "if true:\n"
        "    let x = 99\n"
        "    print(x)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "99\n1\n");
}

TEST_F(CodeGenTest, BlockScopeConstNotLeaks) {
    std::string src =
        "if true:\n"
        "    const c = 10\n"
        "let c = 20\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, MultipleElif) {
    std::string src =
        "let x = 3\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "elif x == 3:\n"
        "    print(30)\n"
        "elif x == 4:\n"
        "    print(40)\n"
        "else:\n"
        "    print(50)";
    EXPECT_EQ(runSource(src), "30\n");
}

// ===== 構造体型テスト =====

TEST_F(CodeGenTest, StructBasicFieldAccess) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(10, 20)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, StructFloatFields) {
    std::string src =
        "type Vec2:\n"
        "    x: float\n"
        "    y: float\n"
        "let v = Vec2(1.5, 2.5)\n"
        "print(v.x)\n"
        "print(v.y)";
    EXPECT_EQ(runSource(src), "1.5\n2.5\n");
}

TEST_F(CodeGenTest, StructInFnArg) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "let p = Point(42, 99)\n"
        "print(get_x(p))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, StructAsReturnValue) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn make_point(a: int, b: int) -> Point:\n"
        "    return Point(a, b)\n"
        "let p = make_point(7, 8)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "7\n8\n");
}

TEST_F(CodeGenTest, StructNested) {
    std::string src =
        "type Inner:\n"
        "    val: int\n"
        "type Outer:\n"
        "    inner: Inner\n"
        "    extra: int\n"
        "let i = Inner(42)\n"
        "let o = Outer(i, 99)\n"
        "print(o.inner.val)\n"
        "print(o.extra)";
    EXPECT_EQ(runSource(src), "42\n99\n");
}

TEST_F(CodeGenTest, StructConstructorArgCountMismatchThrows) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructConstructorArgTypeMismatchThrows) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1.5, 2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructUnknownFieldThrows) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
        "print(p.z)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FieldAccessOnNonStructThrows) {
    std::string src =
        "let x = 42\n"
        "print(x.field)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, PrintStructThrows) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
        "print(p)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, UnknownTypeAnnotationThrows) {
    EXPECT_THROW(runSource("let x: foo = 42"), std::runtime_error);
}

TEST_F(CodeGenTest, StructFieldArithmetic) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let a = Point(10, 20)\n"
        "let b = Point(3, 7)\n"
        "let dx = a.x - b.x\n"
        "print(dx)";
    EXPECT_EQ(runSource(src), "7\n");
}

// ===== Unit型テスト =====

TEST_F(CodeGenTest, UnitFnNoReturnType) {
    std::string src =
        "fn greet():\n"
        "    print(42)\n"
        "greet()";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnitFnExplicit) {
    std::string src =
        "fn greet() -> Unit:\n"
        "    print(99)\n"
        "greet()";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, UnitFnReturnVoid) {
    std::string src =
        "fn noop():\n"
        "    return\n"
        "noop()";
    EXPECT_EQ(runSource(src), "");
}

TEST_F(CodeGenTest, UnitFnReturnVoidEarly) {
    std::string src =
        "fn maybe_print(x: int):\n"
        "    if x > 0:\n"
        "        print(x)\n"
        "        return\n"
        "    print(0)\n"
        "maybe_print(5)\n"
        "maybe_print(-1)";
    EXPECT_EQ(runSource(src), "5\n0\n");
}

TEST_F(CodeGenTest, UnitFnReturnValueThrows) {
    std::string src =
        "fn f():\n"
        "    return 42\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Option<T>型テスト =====

TEST_F(CodeGenTest, OptionIntSomePrint) {
    std::string src =
        "let x: Option<int> = Some(42)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(42)\n");
}

TEST_F(CodeGenTest, OptionIntNonePrint) {
    std::string src =
        "let x: Option<int> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionFloatSomePrint) {
    std::string src =
        "let x: Option<float> = Some(3.14)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(3.14)\n");
}

TEST_F(CodeGenTest, OptionFloatNonePrint) {
    std::string src =
        "let x: Option<float> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionBoolSomePrint) {
    std::string src =
        "let x: Option<bool> = Some(true)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(true)\n");
}

TEST_F(CodeGenTest, OptionStrSomePrint) {
    std::string src =
        "let x: Option<str> = Some(\"hello\")\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(hello)\n");
}

TEST_F(CodeGenTest, UnwrapSome) {
    std::string src =
        "let x: Option<int> = Some(42)\n"
        "let v = unwrap(x)\n"
        "print(v)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnwrapNoneExits) {
    std::string src =
        "let x: Option<int> = None\n"
        "let v = unwrap(x)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, OptionFnParamAndReturn) {
    std::string src =
        "fn maybe_double(x: Option<int>) -> Option<int>:\n"
        "    return x\n"
        "let a = maybe_double(Some(21))\n"
        "print(a)";
    EXPECT_EQ(runSource(src), "Some(21)\n");
}

TEST_F(CodeGenTest, OptionFnNoneArg) {
    std::string src =
        "fn f(x: Option<int>) -> int:\n"
        "    return 0\n"
        "let r = f(None)\n"
        "print(r)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, OptionTypeMismatchThrows) {
    std::string src = "let x: Option<int> = Some(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneWithoutAnnotationThrows) {
    EXPECT_THROW(runSource("let x = None"), std::runtime_error);
}

TEST_F(CodeGenTest, OptionReassignSomeToNone) {
    std::string src =
        "let x: Option<int> = Some(42)\n"
        "x = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionReassignNoneToSome) {
    std::string src =
        "let x: Option<int> = None\n"
        "x = Some(99)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(99)\n");
}

// ===== import 統合テスト =====

class ImportTest : public CodeGenTest {
protected:
    std::filesystem::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = std::filesystem::temp_directory_path() / "ry_import_test";
        std::filesystem::create_directories(tmp_dir_);
    }

    void TearDown() override {
        std::filesystem::remove_all(tmp_dir_);
    }

    void writeFile(const std::string &relative_path, const std::string &content) {
        auto path = tmp_dir_ / relative_path;
        std::filesystem::create_directories(path.parent_path());
        std::ofstream f(path);
        f << content;
    }

    std::string runWithImports(const std::string &src,
                               const std::string &referrer_dir = "",
                               const std::vector<std::string> &search_paths = {}) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        std::string dir = referrer_dir.empty() ? tmp_dir_.string() : referrer_dir;
        ModuleLoader loader(search_paths);
        prog = loader.resolveImports(prog, dir);

        CodeGen cg;
        auto tsm = cg.compile(prog);

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
};

TEST_F(ImportTest, ImportAllFunctions) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");

    EXPECT_EQ(runWithImports(
        "from math\n"
        "print(add(1, 2))\n"
        "print(sub(5, 3))"),
        "3\n2\n");
}

TEST_F(ImportTest, ImportSelectedFunction) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");

    EXPECT_EQ(runWithImports(
        "from math import add\n"
        "print(add(10, 20))"),
        "30\n");
}

TEST_F(ImportTest, ImportMultipleSelected) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n");

    EXPECT_EQ(runWithImports(
        "from math import add, mul\n"
        "print(add(2, 3))\n"
        "print(mul(4, 5))"),
        "5\n20\n");
}

TEST_F(ImportTest, ImportSubdirectory) {
    writeFile("utils/math.ry",
        "fn double_it(x: int) -> int:\n"
        "    return x * 2\n");

    EXPECT_EQ(runWithImports(
        "from utils.math import double_it\n"
        "print(double_it(21))"),
        "42\n");
}

TEST_F(ImportTest, DuplicateImportIgnored) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");

    EXPECT_EQ(runWithImports(
        "from math\n"
        "from math\n"
        "print(add(1, 2))"),
        "3\n");
}

TEST_F(ImportTest, CircularImportError) {
    writeFile("a.ry", "from b\n");
    writeFile("b.ry", "from a\n");

    EXPECT_THROW(runWithImports("from a"), std::runtime_error);
}

TEST_F(ImportTest, TransitiveImport) {
    writeFile("base.ry",
        "fn base_fn(x: int) -> int:\n"
        "    return x + 100\n");
    writeFile("mid.ry",
        "from base\n"
        "fn mid_fn(x: int) -> int:\n"
        "    return base_fn(x) + 10\n");

    EXPECT_EQ(runWithImports(
        "from mid\n"
        "print(mid_fn(1))"),
        "111\n");
}

TEST_F(ImportTest, ModuleNotFoundError) {
    EXPECT_THROW(runWithImports("from nonexistent"), std::runtime_error);
}

TEST_F(ImportTest, FunctionNotFoundError) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");

    EXPECT_THROW(runWithImports("from math import nope"), std::runtime_error);
}

// ===== UFCS コード生成テスト =====

TEST_F(CodeGenTest, UFCSBasicCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "let x = 1\n"
        "print(x.add(2))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, UFCSChainedCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n"
        "let x = 2\n"
        "print(x.add(3).mul(4))";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, UFCSWithStruct) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "let p = Point(42, 99)\n"
        "print(p.get_x())";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(ImportTest, SearchPathRYPATH) {
    // Create a module in a separate directory
    auto lib_dir = std::filesystem::temp_directory_path() / "ry_lib_test";
    std::filesystem::create_directories(lib_dir);
    {
        std::ofstream f(lib_dir / "mylib.ry");
        f << "fn greet() -> int:\n"
             "    print(999)\n"
             "    return 0\n";
    }

    // Use search_paths to simulate RY_PATH
    EXPECT_EQ(runWithImports(
        "from mylib import greet\n"
        "greet()",
        tmp_dir_.string(),
        {lib_dir.string()}),
        "999\n");

    std::filesystem::remove_all(lib_dir);
}
