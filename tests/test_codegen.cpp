#include "test_codegen_common.hpp"

TEST_F(CodeGenTest, PrintInteger) {
    EXPECT_EQ(runSource("print(42)"), "42\n");
}

TEST_F(CodeGenTest, PrintNegativeInteger) {
    EXPECT_EQ(runSource("var x = -10\nprint(x)"), "-10\n");
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
    EXPECT_EQ(runSource("var x = 1\nx = 2\nprint(x)"), "2\n");
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

TEST_F(CodeGenTest, LetStringImmutable) {
    EXPECT_EQ(runSource("let s = \"hello\"\nprint(s)"), "hello\n");
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
    EXPECT_THROW(runSource("var x = 1\nx = 1.5\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeFloatToIntThrows) {
    EXPECT_THROW(runSource("var x = 1.5\nx = 2\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeIntToBoolThrows) {
    EXPECT_THROW(runSource("var x = 1\nx = true\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeChangeBoolToIntThrows) {
    EXPECT_THROW(runSource("var x = true\nx = 1\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, SameTypeReassignmentFloat) {
    EXPECT_EQ(runSource("var x = 1.5\nx = 2.5\nprint(x)"), "2.5\n");
}

TEST_F(CodeGenTest, TypeChangeAfterUseThrows) {
    EXPECT_THROW(runSource("var x = 1\nprint(x)\nx = 1.5"), std::runtime_error);
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

// ===== let / var テスト =====

TEST_F(CodeGenTest, LetPrint) {
    EXPECT_EQ(runSource("let x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, VarPrint) {
    EXPECT_EQ(runSource("var x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, VarReassign) {
    EXPECT_EQ(runSource("var x = 1\nx = 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, LetReassignThrows) {
    EXPECT_THROW(runSource("let x = 1\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, UndeclaredAssignThrows) {
    EXPECT_THROW(runSource("x = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, RedeclaredLetThrows) {
    EXPECT_THROW(runSource("let x = 1\nlet x = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, RedeclaredVarThrows) {
    EXPECT_THROW(runSource("var x = 1\nvar x = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, LetWithTypeAnnotation) {
    EXPECT_EQ(runSource("let x: int = 42\nprint(x)"), "42\n");
}
