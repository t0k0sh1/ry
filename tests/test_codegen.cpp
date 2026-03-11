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

// ===== タプル型テスト =====

TEST_F(CodeGenTest, TupleCreateAndAccess) {
    std::string src =
        "let t = (10, 20)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, TupleMixedTypes) {
    std::string src =
        "let t = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleWithTypeAnnotation) {
    std::string src =
        "let t: (int, float) = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleThreeElements) {
    std::string src =
        "let t = (1, 2, 3)\n"
        "print(t.0)\n"
        "print(t.1)\n"
        "print(t.2)";
    EXPECT_EQ(runSource(src), "1\n2\n3\n");
}

TEST_F(CodeGenTest, TupleFnReturn) {
    std::string src =
        "fn swap(a: int, b: int) -> (int, int):\n"
        "    return (b, a)\n"
        "let result = swap(1, 2)\n"
        "print(result.0)\n"
        "print(result.1)";
    EXPECT_EQ(runSource(src), "2\n1\n");
}

TEST_F(CodeGenTest, TupleFnReturnAccessDirect) {
    std::string src =
        "fn make_pair(a: int, b: float) -> (int, float):\n"
        "    return (a, b)\n"
        "let p = make_pair(42, 3.14)\n"
        "print(p.0)\n"
        "print(p.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleIndexOutOfRangeThrows) {
    std::string src =
        "let t = (1, 2)\n"
        "print(t.2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleTypeAnnotationMismatchThrows) {
    std::string src = "let t: (int, int) = (1, 3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleLetAssign) {
    std::string src =
        "let a = (1, 2)\n"
        "let x = a.0\n"
        "let y = a.1\n"
        "print(x)\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "1\n2\n");
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

// ===== リスト型テスト =====

TEST_F(CodeGenTest, ListCreateAndAccess) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, ListLen) {
    std::string src =
        "let xs = [10, 20, 30]\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ListMixedAccess) {
    std::string src =
        "let xs = [10, 20, 30]\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])";
    EXPECT_EQ(runSource(src), "10\n20\n30\n");
}

TEST_F(CodeGenTest, ListWithTypeAnnotation) {
    std::string src =
        "let xs: list[int] = [1, 2, 3]\n"
        "print(xs[0])\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "1\n3\n");
}

TEST_F(CodeGenTest, ListIndexOutOfRange) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "print(xs[5])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListTypeMismatch) {
    EXPECT_THROW(runSource("let xs = [1, 3.14]"), std::runtime_error);
}

TEST_F(CodeGenTest, ListEmpty) {
    EXPECT_THROW(runSource("let xs = []"), std::runtime_error);
}

TEST_F(CodeGenTest, ListPrint) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListInFunction) {
    std::string src =
        "fn first(xs: list[int]) -> int:\n"
        "    return xs[0]\n"
        "let xs = [10, 20, 30]\n"
        "print(first(xs))";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, ListLenInFunction) {
    std::string src =
        "fn size(xs: list[int]) -> int:\n"
        "    return len(xs)\n"
        "let xs = [1, 2, 3, 4, 5]\n"
        "print(size(xs))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, ListFloatElements) {
    std::string src =
        "let xs = [1.5, 2.5, 3.5]\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "2.5\n");
}

TEST_F(CodeGenTest, ListPrintFloat) {
    std::string src =
        "let xs = [1.5, 2.5]\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
}

TEST_F(CodeGenTest, ListNegativeIndex) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "print(xs[-1])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListIndexWithExpression) {
    std::string src =
        "let xs = [10, 20, 30]\n"
        "let i = 1\n"
        "print(xs[i])";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, ListStringElements) {
    std::string src =
        "let xs = [\"hello\", \"world\"]\n"
        "print(xs[0])\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
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

// ===== Map型テスト =====

TEST_F(CodeGenTest, MapCreateAndAccess) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "print(m[\"a\"])";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, MapAccessSecondKey) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "print(m[\"b\"])";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapLen) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "print(len(m))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapPrint) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "print(m)";
    EXPECT_EQ(runSource(src), "{a: 1, b: 2}\n");
}

TEST_F(CodeGenTest, MapKeyAssign) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "m[\"c\"] = 3\n"
        "print(m[\"c\"])\n"
        "print(len(m))";
    EXPECT_EQ(runSource(src), "3\n3\n");
}

TEST_F(CodeGenTest, MapKeyUpdate) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "m[\"a\"] = 99\n"
        "print(m[\"a\"])";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, MapKeyNotFound) {
    std::string src =
        "let m = {\"a\": 1}\n"
        "print(m[\"z\"])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, MapWithTypeAnnotation) {
    std::string src =
        "let m: map[str, int] = {\"x\": 42}\n"
        "print(m[\"x\"])";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, MapIntKeys) {
    std::string src =
        "let m = {1: \"hello\", 2: \"world\"}\n"
        "print(m[1])";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, MapInFunction) {
    std::string src =
        "fn get_val(m: map[str, int], k: str) -> int:\n"
        "    return m[k]\n"
        "let m = {\"a\": 10, \"b\": 20}\n"
        "print(get_val(m, \"b\"))";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, MapHasKey) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "print(m.has_key(\"a\"))\n"
        "print(m.has_key(\"z\"))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

// ===== 文字列メソッドテスト =====

TEST_F(CodeGenTest, StringLen) {
    EXPECT_EQ(runSource("print(len(\"hello\"))"), "5\n");
}

TEST_F(CodeGenTest, StringLenEmpty) {
    EXPECT_EQ(runSource("print(len(\"\"))"), "0\n");
}

TEST_F(CodeGenTest, StringLenUFCS) {
    EXPECT_EQ(runSource("let s = \"hello\"\nprint(s.len())"), "5\n");
}

TEST_F(CodeGenTest, StringEqual) {
    std::string src =
        "let a = \"hello\"\n"
        "let b = \"hello\"\n"
        "print(a == b)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringNotEqual) {
    std::string src =
        "let a = \"hello\"\n"
        "let b = \"world\"\n"
        "print(a != b)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringEqualFalse) {
    EXPECT_EQ(runSource("print(\"abc\" == \"def\")"), "false\n");
}

TEST_F(CodeGenTest, StringNotEqualFalse) {
    EXPECT_EQ(runSource("print(\"abc\" != \"abc\")"), "false\n");
}

TEST_F(CodeGenTest, StringConcat) {
    std::string src =
        "let a = \"hello\"\n"
        "let b = \" world\"\n"
        "print(a + b)";
    EXPECT_EQ(runSource(src), "hello world\n");
}

TEST_F(CodeGenTest, StringConcatLiterals) {
    EXPECT_EQ(runSource("print(\"foo\" + \"bar\")"), "foobar\n");
}

TEST_F(CodeGenTest, StringConcatEmpty) {
    EXPECT_EQ(runSource("print(\"hello\" + \"\")"), "hello\n");
}

TEST_F(CodeGenTest, StringContains) {
    std::string src =
        "let s = \"hello world\"\n"
        "print(contains(s, \"world\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringContainsFalse) {
    EXPECT_EQ(runSource("print(contains(\"hello\", \"xyz\"))"), "false\n");
}

TEST_F(CodeGenTest, StringContainsUFCS) {
    std::string src =
        "let s = \"hello world\"\n"
        "print(s.contains(\"hello\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringStartsWith) {
    EXPECT_EQ(runSource("print(starts_with(\"hello world\", \"hello\"))"), "true\n");
}

TEST_F(CodeGenTest, StringStartsWithFalse) {
    EXPECT_EQ(runSource("print(starts_with(\"hello world\", \"world\"))"), "false\n");
}

TEST_F(CodeGenTest, StringStartsWithUFCS) {
    std::string src =
        "let s = \"hello\"\n"
        "print(s.starts_with(\"hel\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringEndsWith) {
    EXPECT_EQ(runSource("print(ends_with(\"hello world\", \"world\"))"), "true\n");
}

TEST_F(CodeGenTest, StringEndsWithFalse) {
    EXPECT_EQ(runSource("print(ends_with(\"hello world\", \"hello\"))"), "false\n");
}

TEST_F(CodeGenTest, StringEndsWithSuffixTooLong) {
    EXPECT_EQ(runSource("print(ends_with(\"hi\", \"hello\"))"), "false\n");
}

TEST_F(CodeGenTest, StringEndsWithUFCS) {
    std::string src =
        "let s = \"hello\"\n"
        "print(s.ends_with(\"llo\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, MapTypeMismatch) {
    EXPECT_THROW(runSource("let m = {\"a\": 1, \"b\": 3.14}"), std::runtime_error);
}

// ===== 関数オーバーロードテスト =====

TEST_F(CodeGenTest, OverloadByArgCount) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "fn f(x: int, y: int) -> int:\n"
        "    return x + y\n"
        "print(f(10))\n"
        "print(f(3, 4))";
    EXPECT_EQ(runSource(src), "10\n7\n");
}

TEST_F(CodeGenTest, OverloadByArgType) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x * 2\n"
        "fn f(x: float) -> float:\n"
        "    return x * 3.0\n"
        "print(f(5))\n"
        "print(f(2.0))";
    EXPECT_EQ(runSource(src), "10\n6\n");
}

TEST_F(CodeGenTest, OverloadDifferentReturn) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "fn f(x: float) -> bool:\n"
        "    return x > 0.0\n"
        "print(f(42))\n"
        "print(f(1.5))";
    EXPECT_EQ(runSource(src), "42\ntrue\n");
}

TEST_F(CodeGenTest, OverloadSameSignatureError) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "fn f(x: int) -> int:\n"
        "    return x + 1\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, OverloadSameParamsDiffReturnError) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "fn f(x: int) -> float:\n"
        "    return 1.0\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, OverloadWithUFCS) {
    std::string src =
        "fn process(x: int) -> int:\n"
        "    return x * 2\n"
        "fn process(x: int, y: int) -> int:\n"
        "    return x + y\n"
        "let a = 5\n"
        "print(a.process())\n"
        "print(a.process(3))";
    EXPECT_EQ(runSource(src), "10\n8\n");
}

TEST_F(CodeGenTest, OverloadNoMatchError) {
    std::string src =
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "f(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, OverloadWithNone) {
    std::string src =
        "fn f(x: Option<int>) -> int:\n"
        "    return 0\n"
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "print(f(None))\n"
        "print(f(42))";
    EXPECT_EQ(runSource(src), "0\n42\n");
}

TEST_F(CodeGenTest, OverloadRecursive) {
    std::string src =
        "fn fact(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * fact(n - 1)\n"
        "fn fact(n: int, acc: int) -> int:\n"
        "    if n <= 1:\n"
        "        return acc\n"
        "    return fact(n - 1, acc * n)\n"
        "print(fact(5))\n"
        "print(fact(5, 1))";
    EXPECT_EQ(runSource(src), "120\n120\n");
}

// ===== Operator overloading =====

TEST_F(CodeGenTest, OperatorOverloadStructPlus) {
    std::string src =
        "type Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "let v1 = Vec2(1, 2)\n"
        "let v2 = Vec2(3, 4)\n"
        "let v3 = v1 + v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "4\n6\n");
}

TEST_F(CodeGenTest, OperatorOverloadStructMinus) {
    std::string src =
        "type Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator-(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x - b.x, a.y - b.y)\n"
        "let v1 = Vec2(5, 8)\n"
        "let v2 = Vec2(2, 3)\n"
        "let v3 = v1 - v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "3\n5\n");
}

TEST_F(CodeGenTest, OperatorOverloadStructEq) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator==(a: Point, b: Point) -> bool:\n"
        "    return a.x == b.x and a.y == b.y\n"
        "let p1 = Point(1, 2)\n"
        "let p2 = Point(1, 2)\n"
        "let p3 = Point(3, 4)\n"
        "print(p1 == p2)\n"
        "print(p1 == p3)";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, OperatorOverloadUnaryMinus) {
    std::string src =
        "type Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator-(a: Vec2) -> Vec2:\n"
        "    return Vec2(0 - a.x, 0 - a.y)\n"
        "let v = Vec2(3, 5)\n"
        "let neg = -v\n"
        "print(neg.x)\n"
        "print(neg.y)";
    EXPECT_EQ(runSource(src), "-3\n-5\n");
}

TEST_F(CodeGenTest, OperatorOverloadBuiltinUnchanged) {
    // Built-in int + still works without operator overload
    EXPECT_EQ(runSource("print(3 + 4)"), "7\n");
    EXPECT_EQ(runSource("print(10 - 3)"), "7\n");
    EXPECT_EQ(runSource("print(2 * 5)"), "10\n");
}

TEST_F(CodeGenTest, OperatorOverloadMultipleOps) {
    std::string src =
        "type Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "fn operator*(a: Vec2, s: int) -> Vec2:\n"
        "    return Vec2(a.x * s, a.y * s)\n"
        "let v = Vec2(1, 2)\n"
        "let v2 = v * 3\n"
        "let v3 = v + v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "4\n8\n");
}

// ===== List element assignment =====

TEST_F(CodeGenTest, ListIndexAssignInt) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "xs[0] = 99\n"
        "xs[1] = 88\n"
        "xs[2] = 77\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])";
    EXPECT_EQ(runSource(src), "99\n88\n77\n");
}

TEST_F(CodeGenTest, ListIndexAssignFloat) {
    std::string src =
        "let xs = [1.0, 2.0, 3.0]\n"
        "xs[1] = 9.5\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "9.5\n");
}

TEST_F(CodeGenTest, ListIndexAssignBool) {
    std::string src =
        "let xs = [true, false, true]\n"
        "xs[0] = false\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, ListIndexAssignStr) {
    std::string src =
        "let xs = [\"hello\", \"world\"]\n"
        "xs[0] = \"goodbye\"\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "goodbye\n");
}

TEST_F(CodeGenTest, ListIndexAssignOutOfRange) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "xs[5] = 99";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListIndexAssignNegative) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "xs[-1] = 99";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== String ordering comparisons =====

TEST_F(CodeGenTest, StringLessThan) {
    std::string src =
        "print(\"apple\" < \"banana\")";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringGreaterThan) {
    std::string src =
        "print(\"banana\" > \"apple\")";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringLessEqual) {
    std::string src =
        "print(\"abc\" <= \"abc\")";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringGreaterEqualFalse) {
    std::string src =
        "print(\"abc\" >= \"abd\")";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, StringLessEqualSame) {
    std::string src =
        "print(\"xyz\" <= \"xyz\")\n"
        "print(\"xyz\" >= \"xyz\")";
    EXPECT_EQ(runSource(src), "true\ntrue\n");
}

TEST_F(CodeGenTest, StringLessThanFalse) {
    std::string src =
        "print(\"banana\" < \"apple\")";
    EXPECT_EQ(runSource(src), "false\n");
}

// ===== For loop =====

TEST_F(CodeGenTest, ForLoopBasic) {
    std::string src =
        "let xs = [10, 20, 30]\n"
        "for x in xs:\n"
        "    print(x)";
    EXPECT_EQ(runSource(src), "10\n20\n30\n");
}

TEST_F(CodeGenTest, ForLoopRange) {
    std::string src =
        "for i in range(5):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "0\n1\n2\n3\n4\n");
}

TEST_F(CodeGenTest, ForLoopRangeStartEnd) {
    std::string src =
        "for i in range(2, 5):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "2\n3\n4\n");
}

TEST_F(CodeGenTest, ForLoopAccumulate) {
    std::string src =
        "let xs = [1, 2, 3, 4, 5]\n"
        "let sum = 0\n"
        "for x in xs:\n"
        "    sum = sum + x\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, ForLoopNested) {
    std::string src =
        "for i in range(3):\n"
        "    for j in range(2):\n"
        "        print(i * 10 + j)";
    EXPECT_EQ(runSource(src), "0\n1\n10\n11\n20\n21\n");
}

TEST_F(CodeGenTest, ForLoopStringList) {
    std::string src =
        "let words = [\"hello\", \"world\"]\n"
        "for w in words:\n"
        "    print(w)";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
}

// ===== Compound assignment =====

TEST_F(CodeGenTest, CompoundAssignPlus) {
    std::string src =
        "let x = 10\n"
        "x += 5\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, CompoundAssignMinus) {
    std::string src =
        "let x = 10\n"
        "x -= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, CompoundAssignStar) {
    std::string src =
        "let x = 4\n"
        "x *= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, CompoundAssignSlash) {
    std::string src =
        "let x = 10.0\n"
        "x /= 4.0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2.5\n");
}

TEST_F(CodeGenTest, CompoundAssignPercent) {
    std::string src =
        "let x = 10\n"
        "x %= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, CompoundAssignInLoop) {
    std::string src =
        "let sum = 0\n"
        "for i in range(1, 6):\n"
        "    sum += i\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "15\n");
}

// ===== Break / Continue =====

TEST_F(CodeGenTest, BreakInWhile) {
    std::string src =
        "let i = 0\n"
        "while true:\n"
        "    if i == 5:\n"
        "        break\n"
        "    i += 1\n"
        "print(i)";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, ContinueInFor) {
    std::string src =
        "for i in range(5):\n"
        "    if i == 2:\n"
        "        continue\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "0\n1\n3\n4\n");
}

TEST_F(CodeGenTest, BreakInFor) {
    std::string src =
        "for i in range(10):\n"
        "    if i == 3:\n"
        "        break\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "0\n1\n2\n");
}

TEST_F(CodeGenTest, BreakInNestedLoop) {
    std::string src =
        "for i in range(3):\n"
        "    for j in range(3):\n"
        "        if j == 1:\n"
        "            break\n"
        "        print(i * 10 + j)\n";
    EXPECT_EQ(runSource(src), "0\n10\n20\n");
}

TEST_F(CodeGenTest, ContinueInWhile) {
    std::string src =
        "let i = 0\n"
        "let sum = 0\n"
        "while i < 10:\n"
        "    i += 1\n"
        "    if i % 2 == 0:\n"
        "        continue\n"
        "    sum += i\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "25\n");
}

// ===== Struct field assignment =====

TEST_F(CodeGenTest, StructFieldAssign) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
        "p.x = 10\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "10\n2\n");
}

TEST_F(CodeGenTest, StructFieldAssignMultiple) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(0, 0)\n"
        "p.x = 3\n"
        "p.y = 4\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "3\n4\n");
}

TEST_F(CodeGenTest, StructFieldAssignConstError) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "const p = Point(1, 2)\n"
        "p.x = 10";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Escape sequences =====

TEST_F(CodeGenTest, EscapeNewline) {
    std::string src = "print(\"hello\\nworld\")";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
}

TEST_F(CodeGenTest, EscapeTab) {
    std::string src = "print(\"a\\tb\")";
    EXPECT_EQ(runSource(src), "a\tb\n");
}

TEST_F(CodeGenTest, EscapeBackslash) {
    std::string src = "print(\"a\\\\b\")";
    EXPECT_EQ(runSource(src), "a\\b\n");
}

TEST_F(CodeGenTest, EscapeQuote) {
    std::string src = "print(\"say \\\"hi\\\"\")";
    EXPECT_EQ(runSource(src), "say \"hi\"\n");
}

// ===== range() as expression =====

TEST_F(CodeGenTest, RangeEmptyList) {
    std::string src =
        "let xs = range(0)\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, RangeUsedAsList) {
    std::string src =
        "let xs = range(3)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "0\n1\n2\n3\n");
}
