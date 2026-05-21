#include "test_codegen_common.hpp"


using namespace ry;
// ===== Print literals =====

TEST_F(CodeGenTest, PrintLiterals) {
    EXPECT_EQ(runSource("print(42)"), "42\n");
    EXPECT_EQ(runSource("x = -10\nprint(x)"), "-10\n");
    EXPECT_EQ(runSource("print(3.14)"), "3.14\n");
    // %g + ".0" correction: whole-number floats print as "1.0" (#808)
    EXPECT_EQ(runSource("print(1.0)"), "1.0\n");
    EXPECT_EQ(runSource("print(true)"), "true\n");
    EXPECT_EQ(runSource("print(false)"), "false\n");
    EXPECT_EQ(runSource("print(\"hello\")"), "hello\n");
    EXPECT_EQ(runSource("print(\"\")"), "\n");
}

// ===== Print variadic =====

TEST_F(CodeGenTest, PrintVariadic) {
    // Multiple arguments (space-separated)
    EXPECT_EQ(runSource("print(1, 2, 3)"), "1 2 3\n");
    EXPECT_EQ(runSource("print(\"hello\", \"world\")"), "hello world\n");
    EXPECT_EQ(runSource("print(1, \"hello\", true)"), "1 hello true\n");
    // Single argument (backward compatible)
    EXPECT_EQ(runSource("print(42)"), "42\n");
    // No arguments (just newline)
    EXPECT_EQ(runSource("print()"), "\n");
    // Complex type + primitive
    EXPECT_EQ(runSource("print([1, 2], \"x\")"), "[1, 2] x\n");
}

// ===== Arithmetic (int) =====

TEST_F(CodeGenTest, ArithmeticInt) {
    EXPECT_EQ(runSource("x = 3 + 4\nprint(x)"), "7\n");
    EXPECT_EQ(runSource("x = 5 - 3\nprint(x)"), "2\n");
    EXPECT_EQ(runSource("x = 3 * 4\nprint(x)"), "12\n");
    EXPECT_EQ(runSource("x = +5\nprint(x)"), "5\n");
    EXPECT_EQ(runSource("x = 10 % 3\nprint(x)"), "1\n");
    EXPECT_EQ(runSource("x = 7 // 2\nprint(x)"), "3\n");
    // 2 ** 10 = 1024 (f64 with ".0" correction, #808)
    EXPECT_EQ(runSource("x = 2 ** 10\nprint(x)"), "1024.0\n");
    // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
    EXPECT_EQ(runSource("x = 2 ** 3 ** 2\nprint(x)"), "512.0\n");
}

// ===== Arithmetic (float) =====

TEST_F(CodeGenTest, ArithmeticFloat) {
    EXPECT_EQ(runSource("x = 7 / 2\nprint(x)"), "3.5\n");
    EXPECT_EQ(runSource("x = 1.5 + 2.5\nprint(x)"), "4.0\n");
    EXPECT_EQ(runSource("x = 1 + 2.5\nprint(x)"), "3.5\n");
    EXPECT_EQ(runSource("x = 5.5 % 2.0\nprint(x)"), "1.5\n");
}

// ===== Float shortest round-trip (#1031) =====

TEST_F(CodeGenTest, FloatShortestRoundTrip) {
    // Imprecise arithmetic must show the actual stored value (#1031)
    EXPECT_EQ(runSource("print(0.1 + 0.2)"), "0.30000000000000004\n");
    // Literals that happen to be exact IEEE 754 values must be preserved (#808 regression)
    EXPECT_EQ(runSource("print(3.14)"), "3.14\n");
    EXPECT_EQ(runSource("print(2.5)"), "2.5\n");
    EXPECT_EQ(runSource("print(3.0)"), "3.0\n");
    EXPECT_EQ(runSource("print(0.0)"), "0.0\n");
    EXPECT_EQ(runSource("print(-2.0)"), "-2.0\n");
}

// ===== Comparison operators =====

TEST_F(CodeGenTest, ComparisonOperators) {
    EXPECT_EQ(runSource("x = 1 == 1\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 1 == 2\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = 3 < 5\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 1 != 2\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 2 != 2\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = 3 <= 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 4 <= 3\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = 5 > 3\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 3 > 5\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = 5 >= 5\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 4 >= 5\nprint(x)"), "false\n");
}

// ===== Logical operators =====

TEST_F(CodeGenTest, LogicalOperators) {
    EXPECT_EQ(runSource("x = true and true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = true and false\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = false or true\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = false or false\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = not true\nprint(x)"), "false\n");
    EXPECT_EQ(runSource("x = not false\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = not not true\nprint(x)"), "true\n");
}

// ===== Variables =====

TEST_F(CodeGenTest, VariableBasics) {
    EXPECT_EQ(runSource("print(1)\nprint(2)\nprint(3)"), "1\n2\n3\n");
}

TEST_F(CodeGenTest, VariableBindingBasics) {
    EXPECT_EQ(runSource("x = 10\nprint(x)"), "10\n");
    EXPECT_EQ(runSource("x = 1\nx = 2\nprint(x)"), "2\n");
    EXPECT_THROW(runSource(withStdlibDirectiveDecls("@const\nx = 1\nx = 2")), std::runtime_error);
    EXPECT_THROW(runSource(withStdlibDirectiveDecls("@const\nx = 1\n@const\nx = 2")), std::runtime_error);
    EXPECT_EQ(runSource("x: int = 42\nprint(x)"), "42\n");
}

// ===== Error handling (individual) =====

TEST_F(CodeGenTest, UndefinedVariableThrows) {
    EXPECT_THROW(runSource("print(z)"), std::runtime_error);
}

TEST_F(CodeGenTest, UnknownFunctionThrows) {
    EXPECT_THROW(runSource("foo(42)"), std::runtime_error);
}

// ===== Operator return type constraint =====

TEST_F(CodeGenTest, OperatorEqInferredNonBoolThrows) {
    EXPECT_THROW(runSource(
        "record Pt:\n"
        "    x: int\n"
        "fn operator==(a: Pt, b: Pt):\n"
        "    return 42\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorAndExplicitNonBoolThrows) {
    EXPECT_THROW(runSource(
        "record Pt:\n"
        "    x: int\n"
        "fn operator and(a: Pt, b: Pt) -> int:\n"
        "    return 1\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorOrExplicitNonBoolThrows) {
    EXPECT_THROW(runSource(
        "record Pt:\n"
        "    x: int\n"
        "fn operator or(a: Pt, b: Pt) -> int:\n"
        "    return 1\n"), std::runtime_error);
}

// ===== Bitwise operators =====

TEST_F(CodeGenTest, BitwiseOperators) {
    // 1100 & 1010 = 1000 = 8
    EXPECT_EQ(runSource("x = 12 & 10\nprint(x)"), "8\n");
    // 1100 | 1010 = 1110 = 14
    EXPECT_EQ(runSource("x = 12 | 10\nprint(x)"), "14\n");
    // 1100 ^ 1010 = 0110 = 6
    EXPECT_EQ(runSource("x = 12 ^ 10\nprint(x)"), "6\n");
    EXPECT_EQ(runSource("x = 1 << 3\nprint(x)"), "8\n");
    EXPECT_EQ(runSource("x = 16 >> 2\nprint(x)"), "4\n");
    // 算術右シフト: -8 >> 1 = -4
    EXPECT_EQ(runSource("x = -8 >> 1\nprint(x)"), "-4\n");
    // ~0 = -1
    EXPECT_EQ(runSource("x = ~0\nprint(x)"), "-1\n");
    // ~1 = -2
    EXPECT_EQ(runSource("x = ~1\nprint(x)"), "-2\n");
    // explicit cast: (true as int) & 3 = 1 (bool rejected; use 'as int')
    EXPECT_EQ(runSource("x = (true as int) & 3\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, BitwisePrecedence) {
    // 1 | 2 & 3 = 1 | (2&3) = 1 | 2 = 3
    EXPECT_EQ(runSource("x = 1 | 2 & 3\nprint(x)"), "3\n");
    // 1 | 2 << 1 = 1 | (2<<1) = 1 | 4 = 5
    EXPECT_EQ(runSource("x = 1 | 2 << 1\nprint(x)"), "5\n");
    // 3 & 5 == 1 → (3&5)=1, 1==1=true
    EXPECT_EQ(runSource("x = 3 & 5 == 1\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, BitwiseFloatThrows) {
    EXPECT_THROW(runSource("x = 1.0 & 2"), std::runtime_error);
}

// ===== String variables =====

TEST_F(CodeGenTest, StringVariables) {
    EXPECT_EQ(runSource("s = \"world\"\nprint(s)"), "world\n");
    EXPECT_EQ(runSource("s = \"hello\"\nprint(s)"), "hello\n");
    EXPECT_EQ(runSource("s: str = \"typed\"\nprint(s)"), "typed\n");
}

TEST_F(CodeGenTest, StringTypeMismatch) {
    EXPECT_THROW(runSource("s: str = 42"), std::runtime_error);
    EXPECT_THROW(runSource("s: int = \"hello\""), std::runtime_error);
}

TEST_F(CodeGenTest, StringBinaryOpTypeMismatch) {
    // Non-+ arithmetic ops with str still error
    EXPECT_THROW(runSource("print(\"abc\" - 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" / 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" // 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" % 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" ** 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(2 - \"abc\")"), std::runtime_error);
    EXPECT_THROW(runSource("print(2 / \"abc\")"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" - 1.5)"), std::runtime_error);
    // Comparison: str vs int
    EXPECT_THROW(runSource("print(\"abc\" == 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" < 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(2 == \"abc\")"), std::runtime_error);
    // Bitwise: str & int
    EXPECT_THROW(runSource("print(\"abc\" & 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(\"abc\" | 2)"), std::runtime_error);
    // Valid operations still work
    EXPECT_EQ(runSource("print(\"ab\" + \"cd\")"), "abcd\n");
    EXPECT_EQ(runSource("print(\"ab\" * 3)"), "ababab\n");
    EXPECT_EQ(runSource("print(\"ab\" == \"ab\")"), "true\n");
    EXPECT_EQ(runSource("print(\"ab\" < \"cd\")"), "true\n");
}

// ===== String auto-concatenation (#393) =====

TEST_F(CodeGenTest, StringAutoConcat) {
    EXPECT_EQ(runSource("print(\"abc\" + 2)"), "abc2\n");
    EXPECT_EQ(runSource("print(\"value: \" + 42)"), "value: 42\n");
    EXPECT_EQ(runSource("print(1 + \"abc\")"), "1abc\n");
    EXPECT_EQ(runSource("print(\"pi=\" + 3.14)"), "pi=3.14\n");
    EXPECT_EQ(runSource("print(\"ok=\" + true)"), "ok=true\n");
    EXPECT_EQ(runSource("print(false + \" is false\")"), "false is false\n");
}

// ===== Non-string pointer types must not hit string paths (#397) =====

TEST_F(CodeGenTest, NonStrPointerNotTreatedAsStr) {
    // Collections share LLVM ptrTy_ with str but must not activate str-specific operator paths
    EXPECT_THROW(runSource("xs = [1, 2]\nprint(xs == \"abc\")"), std::runtime_error);
    EXPECT_THROW(runSource("xs = [1, 2]\nprint(xs + \"abc\")"), std::runtime_error);
    EXPECT_THROW(runSource("xs = [1, 2]\nprint(xs & 2)"), std::runtime_error);
    EXPECT_THROW(runSource("m = {\"a\": 1}\nprint(m == \"abc\")"), std::runtime_error);
    EXPECT_THROW(runSource("m = {\"a\": 1}\nprint(m + \"abc\")"), std::runtime_error);
}

// ===== Type change =====

TEST_F(CodeGenTest, TypeChangeThrows) {
    EXPECT_THROW(runSource("x = 1\nx = true\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = true\nx = 1\nprint(x)"), std::runtime_error);
    EXPECT_EQ(runSource("x = 1.5\nx = 2.5\nprint(x)"), "2.5\n");
}

TEST_F(CodeGenTest, IntFloatReassignCoerces) {
    EXPECT_EQ(runSource("x = 1\nx = 1.5\nprint(x)"), "1\n");
    EXPECT_EQ(runSource("x = 1\nprint(x)\nx = 1.5\nprint(x)"), "1\n1\n");
    EXPECT_EQ(runSource("x = 1.5\nx = 2\nprint(x)"), "2.0\n");
}

// ===== Type annotation =====

TEST_F(CodeGenTest, TypeAnnotationMatch) {
    EXPECT_EQ(runSource("a: int = 10\nprint(a)"), "10\n");
    EXPECT_EQ(runSource("b: float = 3.14\nprint(b)"), "3.14\n");
    EXPECT_EQ(runSource("c: bool = true\nprint(c)"), "true\n");
}

TEST_F(CodeGenTest, TypeAnnotationBoolMismatchThrows) {
    EXPECT_THROW(runSource("a: bool = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAnnotationIntFromFloatNarrows) {
    EXPECT_EQ(runSource("a: int = 3.14\nprint(a)"), "3\n");
    EXPECT_EQ(runSource("a: int = 3.7\nprint(a)"), "3\n");
    EXPECT_EQ(runSource("a: int = -3.7\nprint(a)"), "-3\n");
}

TEST_F(CodeGenTest, TypeAnnotationFloatFromIntWidens) {
    EXPECT_EQ(runSource("a: float = 10\nprint(a)"), "10.0\n");
}

TEST_F(CodeGenTest, TypeAnnotationLowLevelNoImplicitCoerce) {
    EXPECT_THROW(runSource("a: i64 = 3.14"), std::runtime_error);
    EXPECT_THROW(runSource("a: i8 = 3.14"), std::runtime_error);
    EXPECT_THROW(runSource("a: f32 = 10"), std::runtime_error);
    EXPECT_THROW(runSource("a: i64 = 0i64\na = 3.14"), std::runtime_error);
    EXPECT_THROW(runSource("b: f32 = 0.0f32\nb = 10"), std::runtime_error);
}

TEST_F(CodeGenTest, IntFloatCoercionIssue1192) {
    EXPECT_EQ(runSource("x: int = 2 ** 3\nprint(x)"), "8\n");
    EXPECT_EQ(runSource("x: int = 10 / 2\nprint(x)"), "5\n");
    EXPECT_EQ(runSource("x: int = 5\nx **= 2\nprint(x)"), "25\n");
}

TEST_F(CodeGenTest, IntFloatCoercionFieldCompoundAssign) {
    EXPECT_EQ(runSource("record R:\n  n: int\n\nr = R(5)\nr.n **= 2\nprint(r.n)"), "25\n");
    EXPECT_EQ(runSource("record R:\n  n: int\n\nr = R(10)\nr.n /= 3\nprint(r.n)"), "3\n");
}

TEST_F(CodeGenTest, IntFloatCoercionElementCompoundAssign) {
    EXPECT_EQ(runSource("xs: List<int> = [5, 10]\nxs[0] **= 2\nprint(xs[0])"), "25\n");
    EXPECT_EQ(runSource("m: Map<str, int> = {\"k\": 5}\nm[\"k\"] **= 2\nprint(m[\"k\"])"), "25\n");
}

TEST_F(CodeGenTest, IntFloatCoercionCanaryLowLevelStillRejected) {
    EXPECT_THROW(runSource("x: i64 = 5i64\nx **= 2"), std::runtime_error);
    EXPECT_NO_THROW(runSource("x: i64 = 5i64\nx += 3i64"));
    EXPECT_THROW(runSource("buf: i64[1] = [5i64]\nbuf[0] **= 2"), std::runtime_error);
}

TEST_F(CodeGenTest, IntFloatCoercionBoundaryRejections) {
    EXPECT_THROW(runSource(
        "fn twice(x: int) -> int:\n  return x\ny = twice(3.14)\nprint(y)"),
        std::runtime_error);
}

TEST_F(CodeGenTest, IntFloatCoercionReturnWidens) {
    EXPECT_EQ(runSource(
        "fn f() -> float:\n  return 3\nprint(f())"), "3.0\n");
    EXPECT_EQ(runSource(
        "f = () -> float => 3\nprint(f())"), "3.0\n");
}

TEST_F(CodeGenTest, IntFloatCoercionReturnNarrows) {
    EXPECT_EQ(runSource(
        "fn f() -> int:\n  return 3.14\nprint(f())"), "3\n");
    EXPECT_EQ(runSource(
        "fn g() -> int:\n  return -3.7\nprint(g())"), "-3\n");
    EXPECT_EQ(runSource(
        "fn h() -> int:\n  return 2 ** 3\nprint(h())"), "8\n");
    EXPECT_EQ(runSource(
        "f = () -> int => 3.14\nprint(f())"), "3\n");
}

TEST_F(CodeGenTest, IntFloatCoercionReturnLowLevelStillRejected) {
    EXPECT_THROW(runSource(
        "fn f() -> f32:\n  return 3\nprint(f())"),
        std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f() -> i64:\n  return 3.14\nprint(f())"),
        std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f() -> i32:\n  return 3.14\nprint(f())"),
        std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f() -> u8:\n  return 3.14\nprint(f())"),
        std::runtime_error);
    EXPECT_THROW(runSource(
        "f = () -> f32 => 3\nprint(f())"),
        std::runtime_error);
    EXPECT_THROW(runSource(
        "f = () -> i64 => 3.14\nprint(f())"),
        std::runtime_error);
}

// ===== u8 basics =====

TEST_F(CodeGenTest, U8Basics) {
    EXPECT_EQ(runSource("b: u8 = 42\nprint(b)"), "42\n");
    EXPECT_EQ(runSource("b: u8 = 0\nprint(b)"), "0\n");
    EXPECT_EQ(runSource("b: u8 = 255\nprint(b)"), "255\n");
    EXPECT_EQ(runSource("b: u8 = 10\nb = 20\nprint(b)"), "20\n");
}

TEST_F(CodeGenTest, U8Arithmetic) {
    // u8 + u8 → u8 (native width)
    EXPECT_EQ(runSource("a: u8 = 10\nb: u8 = 20\nc = a + b\nprint(c)"), "30\n");
    EXPECT_EQ(runSource("a: u8 = 50\nb: u8 = 20\nc = a - b\nprint(c)"), "30\n");
    EXPECT_EQ(runSource("a: u8 = 3\nb: u8 = 4\nc = a * b\nprint(c)"), "12\n");
    // u8 / u8 → u8 (integer division)
    EXPECT_EQ(runSource("a: u8 = 7\nb: u8 = 2\nc = a / b\nprint(c)"), "3\n");
}

TEST_F(CodeGenTest, U8ComparisonAndBitwise) {
    EXPECT_EQ(runSource("a: u8 = 10\nb: u8 = 10\nprint(a == b)"), "true\n");
    EXPECT_EQ(runSource("a: u8 = 5\nb: u8 = 10\nprint(a < b)"), "true\n");
    // 0xFF & 0x0F = 0x0F = 15
    EXPECT_EQ(runSource("a: u8 = 255\nb: u8 = 15\nc = a & b\nprint(c)"), "15\n");
    EXPECT_EQ(runSource("a: u8 = 12\nb: u8 = 10\nc = a | b\nprint(c)"), "14\n");
    EXPECT_EQ(runSource("a: u8 = 12\nb: u8 = 10\nc = a ^ b\nprint(c)"), "6\n");
}

TEST_F(CodeGenTest, U8OutOfRange) {
    EXPECT_THROW(runSource("b: u8 = 256"), std::runtime_error);
    EXPECT_THROW(runSource("b: u8 = -1"), std::runtime_error);
    EXPECT_THROW(runSource("b: u8 = 10\nb = 256"), std::runtime_error);
}

// ===== Hex / Binary literal tests =====

TEST_F(CodeGenTest, HexBinaryLiterals) {
    EXPECT_EQ(runSource("print(0xFF)"), "255\n");
    EXPECT_EQ(runSource("print(0x10 + 0x20)"), "48\n");
    EXPECT_EQ(runSource("print(0b1010)"), "10\n");
    EXPECT_EQ(runSource("print(0b1100 & 0b1010)"), "8\n");
    EXPECT_EQ(runSource("b: u8 = 0xFF\nprint(b)"), "255\n");
    EXPECT_EQ(runSource("b: u8 = 0b11111111\nprint(b)"), "255\n");
    EXPECT_EQ(runSource("print(-0xFF)"), "-255\n");
}

// ===== 論理右シフト >>> =====

TEST_F(CodeGenTest, LogicalRightShift) {
    // 8 >>> 2 = 2
    EXPECT_EQ(runSource("x = 8 >>> 2\nprint(x)"), "2\n");
    // -1 >>> 1 = 9223372036854775807 (unsigned shift of all-ones)
    EXPECT_EQ(runSource("x = -1 >>> 1\nprint(x)"), "9223372036854775807\n");
}

// ===== 文字列繰り返し * =====

TEST_F(CodeGenTest, StringRepeatOperator) {
    EXPECT_EQ(runSource("x = \"ab\" * 3\nprint(x)"), "ababab\n");
    EXPECT_EQ(runSource("x = 3 * \"ab\"\nprint(x)"), "ababab\n");
    EXPECT_EQ(runSource("x = \"x\" * 0\nprint(x)"), "\n");
    EXPECT_EQ(runSource("x = \"x\" * -1\nprint(x)"), "\n");
    EXPECT_EQ(runSource("x = \"\" * 5\nprint(x)"), "\n");
    EXPECT_EQ(runSource("x = \"a\" * 1\nprint(x)"), "a\n");
}

// ===== exit() tests (death tests - individual) =====

TEST_F(CodeGenTest, ExitZero) {
    EXPECT_EXIT(runSource("exit(0)"), ::testing::ExitedWithCode(0), "");
}

TEST_F(CodeGenTest, ExitFortyTwo) {
    EXPECT_EXIT(runSource("exit(42)"), ::testing::ExitedWithCode(42), "");
}

TEST_F(CodeGenTest, ExitAfterPrint) {
    EXPECT_EXIT(runSource("print(\"hello\")\nexit(1)"), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ExitInExprContext) {
    EXPECT_EXIT(runSource("x = exit(1)"), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ExitArgumentErrors) {
    EXPECT_THROW(runSource("exit(1.5)"), std::runtime_error);
    EXPECT_THROW(runSource("exit()"), std::runtime_error);
    EXPECT_THROW(runSource("exit(1, 2)"), std::runtime_error);
}

// ===== Top-level `?` operator tests (#745) =====
// `?` at the top level desugars to "print error to stderr + exit(1)"
// when the operand is Err/None; otherwise it unwraps the happy value.

TEST_F(CodeGenTest, TopLevelQuestionOnResultOkContinues) {
    EXPECT_EQ(runSource(
        "fn mk() -> Result<int, Error>:\n"
        "  return Ok(42)\n"
        "v = mk()?\n"
        "print(v)\n"), "42\n");
}

TEST_F(CodeGenTest, TopLevelQuestionOnResultErrExits) {
    EXPECT_EXIT(runSource(
        "fn mk() -> Result<int, Error>:\n"
        "  return Err(Error(\"top level boom\"))\n"
        "v = mk()?\n"
        "print(v)\n"),
        ::testing::ExitedWithCode(1),
        "top level boom");
}

TEST_F(CodeGenTest, BangBangIsRejectedAtParse) {
    EXPECT_THROW(runSource(
        "fn mk() -> Result<int, Error>:\n"
        "  return Ok(1)\n"
        "v = mk()!!\n"
        "print(v)\n"), std::runtime_error);
}

TEST_F(CodeGenTest, TopLevelQuestionOnOptionSomeContinues) {
    EXPECT_EQ(runSource(
        "x: int? = Some(7)\n"
        "v = x?\n"
        "print(v)\n"), "7\n");
}

TEST_F(CodeGenTest, TopLevelQuestionOnOptionNoneExits) {
    EXPECT_EXIT(runSource(
        "x: int? = none\n"
        "v = x?\n"
        "print(v)\n"),
        ::testing::ExitedWithCode(1),
        "unexpected None");
}

// ===== args() tests =====

TEST_F(CodeGenTest, ArgumentsTests) {
    EXPECT_EQ(runSourceWithArgs("a = args()\nprint(len(a))", {}), "0\n");
    EXPECT_EQ(runSourceWithArgs(
        "a = args()\nprint(len(a))\nprint(a[0])\nprint(a[1])",
        {"hello", "world"}), "2\nhello\nworld\n");
    EXPECT_EQ(runSourceWithArgs(
        "for x in args():\n    print(x)",
        {"foo", "bar"}), "foo\nbar\n");
}

// ===== @native fn missing dispatcher detection =====

TEST_F(CodeGenTest, NativeFunctionMissingDispatcher) {
    try {
        runSource(
            "@native\n"
            "fn unhandledNative(x: str) -> str\n"
            "\n"
            "print(unhandledNative(\"hello\"))\n"
        );
        FAIL() << "Expected exception for unhandled @native fn";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("no matching overload for @native fn"), std::string::npos)
            << "Error was: " << msg;
    }
}

// ===== Zero division guard tests (death tests - individual) =====

TEST_F(CodeGenTest, DivByZeroReturnsInf) {
    EXPECT_EQ(runSource("print(1 / 0)"),   "inf\n");
    EXPECT_EQ(runSource("print(-1 / 0)"),  "-inf\n");
    EXPECT_EQ(runSource("print(10 / 0)"),  "inf\n");
    EXPECT_EQ(runSource("print(-10 / 0)"), "-inf\n");
}

TEST_F(CodeGenTest, DivByZeroVariableReturnsInf) {
    EXPECT_EQ(runSource("x = 0\nprint(1 / x)"),   "inf\n");
    EXPECT_EQ(runSource("x = 0\nprint(-1 / x)"),  "-inf\n");
}

TEST_F(CodeGenTest, DivZeroByZeroReturnsNaN) {
    EXPECT_EQ(runSource("print(0 / 0)"),   "nan\n");
    EXPECT_EQ(runSource("print(0.0 / 0)"), "nan\n");
    EXPECT_EQ(runSource("print(0 / 0.0)"), "nan\n");
}

TEST_F(CodeGenTest, DivFloatByZeroReturnsInf) {
    EXPECT_EQ(runSource("print(1.0 / 0.0)"), "inf\n");
    EXPECT_EQ(runSource("print(-1.0 / 0.0)"), "-inf\n");
}

TEST_F(CodeGenTest, FloorDivByZeroExits) {
    EXPECT_EXIT(runSource("print(7 // 0)"),
                ::testing::ExitedWithCode(1),
                "runtime error: division by zero");
}

TEST_F(CodeGenTest, ModByZeroExits) {
    EXPECT_EXIT(runSource("print(7 % 0)"),
                ::testing::ExitedWithCode(1),
                "runtime error: modulo by zero");
}

// ===== Low-level integer zero division guard tests =====

TEST_F(CodeGenTest, LowLevelDivByZeroExits) {
    EXPECT_EXIT(runSource("a: i32 = 10\nb: i32 = 0\nc = a / b\nprint(c as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: division by zero");
}

TEST_F(CodeGenTest, LowLevelFloorDivByZeroExits) {
    EXPECT_EXIT(runSource("a: i32 = 10\nb: i32 = 0\nc = a // b\nprint(c as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: division by zero");
}

TEST_F(CodeGenTest, LowLevelModByZeroExits) {
    EXPECT_EXIT(runSource("a: i32 = 10\nb: i32 = 0\nc = a % b\nprint(c as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: modulo by zero");
}

TEST_F(CodeGenTest, LowLevelUnsignedDivByZeroExits) {
    EXPECT_EXIT(runSource("a: u32 = 10\nb: u32 = 0\nc = a / b\nprint(c as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: division by zero");
}

TEST_F(CodeGenTest, LowLevelUnsignedModByZeroExits) {
    EXPECT_EXIT(runSource("a: u32 = 10\nb: u32 = 0\nc = a % b\nprint(c as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: modulo by zero");
}

TEST_F(CodeGenTest, FloorDivModWorks) {
    EXPECT_EQ(runSource("print(7 // 2)"), "3\n");
    EXPECT_EQ(runSource("print(-7 // 2)"), "-4\n");
    EXPECT_EQ(runSource("print(7 % 3)"), "1\n");
    // Floor modulo: sign follows divisor (Python semantics)
    EXPECT_EQ(runSource("print(-7 % 3)"), "2\n");
    EXPECT_EQ(runSource("print(7 % -3)"), "-2\n");
    EXPECT_EQ(runSource("print(-7 % -3)"), "-1\n");
    EXPECT_EQ(runSource("print(0 % 5)"), "0\n");
    // Identity: a == (a // b) * b + (a % b)
    EXPECT_EQ(runSource("print((-7 // 3) * 3 + (-7 % 3))"), "-7\n");
    EXPECT_EQ(runSource("print((7 // -3) * -3 + (7 % -3))"), "7\n");
}

// ===== Integer overflow guard tests (death tests) =====

TEST_F(CodeGenTest, IntAddOverflowExits) {
    EXPECT_EXIT(runSource("x = 9223372036854775807\nprint(x + 1)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntSubOverflowExits) {
    EXPECT_EXIT(runSource("x = -9223372036854775807 - 1\nprint(x - 1)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntMulOverflowExits) {
    EXPECT_EXIT(runSource("x = 9223372036854775807\nprint(x * 2)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntNegOverflowExits) {
    EXPECT_EXIT(runSource("x = -9223372036854775807 - 1\nprint(-x)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntFloorDivIntMinByMinusOneExits) {
    EXPECT_EXIT(runSource("x = -9223372036854775807 - 1\ny = -1\nprint(x // y)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntModIntMinByMinusOneExits) {
    EXPECT_EXIT(runSource("x = -9223372036854775807 - 1\ny = -1\nprint(x % y)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, IntOverflowConstantFolding) {
    // Compile-time overflow detection
    EXPECT_THROW(runSource("print(9223372036854775807 + 1)"), std::runtime_error);
    EXPECT_THROW(runSource("print((-9223372036854775807 - 1) - 1)"), std::runtime_error);
    EXPECT_THROW(runSource("print(9223372036854775807 * 2)"), std::runtime_error);
    EXPECT_THROW(runSource("print(-(-9223372036854775807 - 1))"), std::runtime_error);
}

TEST_F(CodeGenTest, IntArithmeticNoOverflow) {
    EXPECT_EQ(runSource("print(1000000 * 1000000)"), "1000000000000\n");
    EXPECT_EQ(runSource("print(9223372036854775807 + 0)"), "9223372036854775807\n");
    // INT64_MIN exactly: -9223372036854775807 - 1 = -9223372036854775808, no overflow
    EXPECT_EQ(runSource("print(-9223372036854775807 - 1)"), "-9223372036854775808\n");
    EXPECT_EQ(runSource("print(100 + 200)"), "300\n");
    EXPECT_EQ(runSource("print(50 - 100)"), "-50\n");
    EXPECT_EQ(runSource("print(12345 * 67890)"), "838102050\n");
}

TEST_F(CodeGenTest, IntCompoundAssignOverflowExits) {
    EXPECT_EXIT(runSource("x = 9223372036854775807\nx += 1\nprint(x)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, LowLevelIntStillWraps) {
    EXPECT_EQ(runSource("x = 2147483647i32\nprint(wrappingAdd(x, 1i32) as int)"), "-2147483648\n");
}

TEST_F(CodeGenTest, LowLevelI64StillWraps) {
    // i64 constants: overflow should wrap, not panic
    EXPECT_EQ(runSource("x = 9223372036854775807i64\ny = 1i64\nprint((x + y) as int)"),
              "-9223372036854775808\n");
    EXPECT_EQ(runSource("print((9223372036854775807i64 + 1i64) as int)"),
              "-9223372036854775808\n");
    // i64 unary negation should wrap
    EXPECT_EQ(runSource("x = -9223372036854775807i64 - 1i64\nprint((-x) as int)"),
              "-9223372036854775808\n");
}

// ===== float → int conversion runtime check (#1232) =====

TEST_F(CodeGenTest, CastFloatInfToIntExits) {
    EXPECT_EXIT(runSource("print((1.0 / 0.0) as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastFloatNanToIntExits) {
    EXPECT_EXIT(runSource("print((0.0 / 0.0) as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastFloatOverflowToIntExits) {
    EXPECT_EXIT(runSource("print(1e100 as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, ImplicitFloatToIntInfCoerceExits) {
    // #1192 coerceToLowLevelType path
    EXPECT_EXIT(runSource("x: int = 1.0 / 0.0\nprint(x)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CompoundAssignFloatToIntInfExits) {
    // #1192 applyCompoundOp path (int /= 0 → +inf intermediate)
    EXPECT_EXIT(runSource("x: int = 5\nx /= 0\nprint(x)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastFloatOverflowToI32Exits) {
    EXPECT_EXIT(runSource("print(1.5e10 as i32)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastNegativeFloatToU8Exits) {
    EXPECT_EXIT(runSource("print(-1.0 as u8)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastLargeFloatToU64Exits) {
    EXPECT_EXIT(runSource("print(1e100 as u64)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

// Math-path (floor/ceil/round/trunc) death tests and the INT64_MIN boundary
// regression live in tests/spec/math.test.ry because `runSource` here does
// not drive the ModuleLoader. The helper is shared with the cast sites above,
// so the runtime-trap contract is exercised end-to-end here.

TEST_F(CodeGenTest, CastF32InfToIntExits) {
    // f32 → int path: f32 1.0 / 0.0 produces +inf, then cast to int
    EXPECT_EXIT(runSource("x: f32 = 1.0\ny: f32 = 0.0\nprint((x / y) as int)"),
                ::testing::ExitedWithCode(1),
                "runtime error: cannot convert");
}

TEST_F(CodeGenTest, CastInt64MinBoundaryAccepted) {
    // Regression: -9.223372036854776e+18 = -2^63 is exactly representable in
    // f64 and fits in i64. Old math check used `fabs >= 2^63` which rejected
    // this boundary. The new helper uses asymmetric bounds (low inclusive,
    // high exclusive) and accepts INT64_MIN.
    EXPECT_EQ(runSource("print((-9.223372036854776e+18) as int)"),
              "-9223372036854775808\n");
}

TEST_F(CodeGenTest, CastFiniteFloatToIntStillWorks) {
    // Ensure the new guard does not regress happy path
    EXPECT_EQ(runSource("print(3.14 as int)"), "3\n");
    EXPECT_EQ(runSource("print(-3.7 as int)"), "-3\n");
    EXPECT_EQ(runSource("print(0.0 as int)"), "0\n");
    EXPECT_EQ(runSource("print(1.0 as int)"), "1\n");
    EXPECT_EQ(runSource("print(1e10 as int)"), "10000000000\n");
}

// ===== any type acceptance / rejection =====

TEST_F(CodeGenTest, AnyTypeAcceptsCollections) {
    // #1697: `any` now holds List / Map / Set collections. Verify wrap and
    // function-boundary roundtrip compile cleanly. Runtime behaviour for
    // collection-holding `any` is exercised in tests/spec/any.test.ry.
    EXPECT_NO_THROW(compileSource("x: any = [1, 2, 3]"));
    EXPECT_NO_THROW(compileSource("x: any = {\"a\": 1}"));
    EXPECT_NO_THROW(compileSource("x: any = {1, 2, 3}"));
    EXPECT_NO_THROW(compileSource(
        "fn f(x):\n"
        "    return x\n"
        "f([1, 2, 3])"));
    EXPECT_NO_THROW(compileSource(
        "fn f() -> any:\n"
        "    return [1, 2, 3]"));
}

TEST_F(CodeGenTest, AnyTypeRejectionForFunctionPointer) {
    // Function pointers are still out of scope for `any` (#1697 covers
    // collections only; record/enum/fn-ptr are tracked in follow-up issues).
    EXPECT_THROW(runSource(
        "fn f() -> int:\n"
        "    return 42\n"
        "x: any = f"), std::runtime_error);
}

TEST_F(CodeGenTest, AnyEnumUnwrapSimpleEnumTypeMismatchTraps) {
    // #1798: simple enum wrapped in `any` must trap when unwrapped as a
    // different simple-enum type. Descriptor-pointer equality at unwrap
    // distinguishes Color from Mode even though both lower to `i64`.
    EXPECT_EXIT(runSource(
        "enum AnyMismatchColor:\n"
        "  Red\n"
        "  Green\n"
        "enum AnyMismatchMode:\n"
        "  On\n"
        "  Off\n"
        "a: any = AnyMismatchColor::Red\n"
        "m: AnyMismatchMode = a\n"
        "print(m == AnyMismatchMode::On)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected AnyMismatchMode");
}

TEST_F(CodeGenTest, AnyEnumUnwrapOptionGenericMismatchTraps) {
    // #1798: Option<T> wrapped in `any` must trap when unwrapped as a
    // different generic parameterization. The descriptor cache key
    // includes the full inner type, so `Option<int>` and `Option<str>`
    // resolve to distinct descriptors.
    EXPECT_EXIT(runSource(
        "a: any = Some(1)\n"
        "o: Option<str> = a\n"
        "case o:\n"
        "  Some(v): print(v)\n"
        "  None: print(\"none\")\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected Option<str>");
}

TEST_F(CodeGenTest, AnyEnumUnwrapResultGenericMismatchTraps) {
    // #1798: Result<T, E> with an ARC-managed Ok slot must trap when
    // unwrapped as a different parameterization. Exercises the
    // source_type_name propagation rule across the alloca for `r`.
    EXPECT_EXIT(runSource(
        "r: Result<int, str> = Ok(7)\n"
        "a: any = r\n"
        "out: Result<List<int>, str> = a\n"
        "case out:\n"
        "  Ok(v): print(v[0])\n"
        "  Err(e): print(e)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected Result<List<int>, str>");
}

TEST_F(CodeGenTest, AnyEnumUnwrapNonEnumPayloadTraps) {
    // #1798: a non-enum tag in `any` (here a List, tag 5) must trap when
    // the static target is an enum type. Hits the tag-check branch
    // before the descriptor check.
    EXPECT_EXIT(runSource(
        "a: any = [1, 2, 3]\n"
        "o: Option<int> = a\n"
        "case o:\n"
        "  Some(v): print(v)\n"
        "  None: print(\"none\")\n"),
        ::testing::ExitedWithCode(1),
        "any type mismatch \\(expected Option<int>, got non-enum\\)");
}

TEST_F(CodeGenTest, CoerceResultOkSimpleEnumMismatchTraps) {
    // #1808: coerceResultType Ok slot dispatches to unwrapFromAny via the
    // enum path; a wrong target simple-enum type must trap with the
    // descriptor-pointer-equality check inside unwrapEnumFromAny.
    EXPECT_EXIT(runSource(
        "enum CoerceResEnumA:\n"
        "  X\n"
        "  Y\n"
        "enum CoerceResEnumB:\n"
        "  P\n"
        "  Q\n"
        "a: any = CoerceResEnumA::X\n"
        "r: Result<CoerceResEnumB, str> = Ok(a)\n"
        "case r:\n"
        "  Ok(v): print(v == CoerceResEnumB::P)\n"
        "  Err(e): print(e)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected CoerceResEnumB");
}

TEST_F(CodeGenTest, CoerceResultOkOptionGenericMismatchTraps) {
    // #1808: coerceResultType Ok slot mismatch on Option<T> generic
    // parameterization. Descriptor cache keys include the inner type so
    // Option<int> and Option<str> have distinct descriptors.
    EXPECT_EXIT(runSource(
        "o: Option<str> = Some(\"x\")\n"
        "a: any = o\n"
        "r: Result<Option<int>, str> = Ok(a)\n"
        "case r:\n"
        "  Ok(v):\n"
        "    case v:\n"
        "      Some(n): print(n)\n"
        "      None: print(\"none\")\n"
        "  Err(e): print(e)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected Option<int>");
}

TEST_F(CodeGenTest, CoerceResultOkNestedResultMismatchTraps) {
    // #1808: coerceResultType Ok slot mismatch on nested Result. Inner
    // Result<int, bool> vs target Result<int, str> have distinct
    // descriptors at the Err parameter.
    EXPECT_EXIT(runSource(
        "inner: Result<int, bool> = Ok(1)\n"
        "a: any = inner\n"
        "r: Result<Result<int, str>, bool> = Ok(a)\n"
        "case r:\n"
        "  Ok(v):\n"
        "    case v:\n"
        "      Ok(n): print(n)\n"
        "      Err(e): print(e)\n"
        "  Err(e): print(e)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected Result<int, str>");
}

TEST_F(CodeGenTest, CoerceResultOkNestedResultMismatchTrapsArcPayload) {
    // #1838: sibling of CoerceResultOkNestedResultMismatchTraps with an
    // ARC-bearing inner Ok payload (str instead of int). Exercises the
    // unwrapEnumFromAny trap path while the inner enum carries an ARC
    // field — used to detect double-free during the abort sequence on
    // Linux glibc (issue was masked under macOS and ASan).
    EXPECT_EXIT(runSource(
        "inner: Result<str, bool> = Ok(\"payload\")\n"
        "a: any = inner\n"
        "r: Result<Result<str, int>, bool> = Ok(a)\n"
        "case r:\n"
        "  Ok(v):\n"
        "    case v:\n"
        "      Ok(s): print(s)\n"
        "      Err(e): print(e)\n"
        "  Err(e): print(e)\n"),
        ::testing::ExitedWithCode(1),
        "any enum type mismatch \\(expected Result<str, int>");
}

// ===== Low-level numeric types (i16, i32, f32) =====

TEST_F(CodeGenTest, LowLevelI32Basics) {
    EXPECT_EQ(runSource("x: i32 = 42\nprint(x)"), "42\n");
    EXPECT_EQ(runSource("x: i32 = -10\nprint(x)"), "-10\n");
    EXPECT_EQ(runSource("x: i32 = 0\nprint(x)"), "0\n");
}

TEST_F(CodeGenTest, LowLevelI16Basics) {
    EXPECT_EQ(runSource("x: i16 = 100\nprint(x)"), "100\n");
    EXPECT_EQ(runSource("x: i16 = -100\nprint(x)"), "-100\n");
}

TEST_F(CodeGenTest, LowLevelF32Basics) {
    EXPECT_EQ(runSource("x: f32 = 2.5\nprint(x)"), "2.5\n");
    EXPECT_EQ(runSource("x: f32 = 0.0\nprint(x)"), "0.0\n");
}

TEST_F(CodeGenTest, LowLevelI32Arithmetic) {
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 20\nc = a + b\nprint(c)"), "30\n");
    EXPECT_EQ(runSource("a: i32 = 50\nb: i32 = 20\nc = a - b\nprint(c)"), "30\n");
    EXPECT_EQ(runSource("a: i32 = 3\nb: i32 = 4\nc = a * b\nprint(c)"), "12\n");
    // i32 / i32 → i32 (integer division, Rust-style)
    EXPECT_EQ(runSource("a: i32 = 7\nb: i32 = 2\nc = a / b\nprint(c)"), "3\n");
    EXPECT_EQ(runSource("a: i32 = 7\nb: i32 = 2\nc = a // b\nprint(c)"), "3\n");
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 3\nc = a % b\nprint(c)"), "1\n");
}

TEST_F(CodeGenTest, LowLevelF32Arithmetic) {
    EXPECT_EQ(runSource("a: f32 = 1.5\nb: f32 = 2.5\nc = a + b\nprint(c)"), "4.0\n");
    EXPECT_EQ(runSource("a: f32 = 5.0\nb: f32 = 2.0\nc = a - b\nprint(c)"), "3.0\n");
    EXPECT_EQ(runSource("a: f32 = 3.0\nb: f32 = 4.0\nc = a * b\nprint(c)"), "12.0\n");
    EXPECT_EQ(runSource("a: f32 = 7.0\nb: f32 = 2.0\nc = a / b\nprint(c)"), "3.5\n");
}

TEST_F(CodeGenTest, LowLevelComparison) {
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 10\nprint(a == b)"), "true\n");
    EXPECT_EQ(runSource("a: i32 = 5\nb: i32 = 10\nprint(a < b)"), "true\n");
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 5\nprint(a > b)"), "true\n");
    EXPECT_EQ(runSource("a: f32 = 1.0\nb: f32 = 2.0\nprint(a < b)"), "true\n");
}

TEST_F(CodeGenTest, LowLevelCast) {
    // i32 <-> int
    EXPECT_EQ(runSource("x: i32 = 42\ny = x as int\nprint(y)"), "42\n");
    EXPECT_EQ(runSource("x = 42\ny = x as i32\nprint(y)"), "42\n");
    // i32 <-> i16
    EXPECT_EQ(runSource("x: i32 = 100\ny = x as i16\nprint(y)"), "100\n");
    EXPECT_EQ(runSource("x: i16 = 100\ny = x as i32\nprint(y)"), "100\n");
    // i32 <-> float
    EXPECT_EQ(runSource("x: i32 = 42\ny = x as float\nprint(y)"), "42.0\n");
    EXPECT_EQ(runSource("x = 3.14\ny = x as i32\nprint(y)"), "3\n");
    // f32 <-> float
    EXPECT_EQ(runSource("x: f32 = 2.5\ny = x as float\nprint(y)"), "2.5\n");
    EXPECT_EQ(runSource("x = 2.5\ny = x as f32\ny2 = y as float\nprint(y2)"), "2.5\n");
    // i32 <-> f32
    EXPECT_EQ(runSource("x: i32 = 42\ny = x as f32\ny2 = y as float\nprint(y2)"), "42.0\n");
    EXPECT_EQ(runSource("x: f32 = 3.14\ny = x as i32\nprint(y)"), "3\n");
}

TEST_F(CodeGenTest, LowLevelTypeInference) {
    // i32 arithmetic result infers as i32 (propagation)
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 20\nc = a + b\nprint(c)"), "30\n");
    // Inferred i32 variable can be reassigned with i32 value
    EXPECT_EQ(runSource("a: i32 = 10\nb: i32 = 20\nc = a + b\nc = a * b\nprint(c)"), "200\n");
    // Inferred i32 variable cannot be mixed with int
    EXPECT_THROW(runSource("a: i32 = 10\nb: i32 = 20\nc = a + b\nd = 5\ne = c + d"), std::runtime_error);
    // i16 inference propagation
    EXPECT_EQ(runSource("a: i16 = 3\nb: i16 = 7\nc = a + b\nprint(c)"), "10\n");
    // f32 inference propagation
    EXPECT_EQ(runSource("a: f32 = 1.5\nb: f32 = 2.5\nc = a + b\nprint(c)"), "4.0\n");
    // Inferred f32 variable cannot be mixed with float
    EXPECT_THROW(runSource("a: f32 = 1.0\nb: f32 = 2.0\nc = a + b\nd = 3.0\ne = c + d"), std::runtime_error);
}

TEST_F(CodeGenTest, LowLevelMixedTypeError) {
    // i32 + int → error
    EXPECT_THROW(runSource("a: i32 = 10\nb = 20\nc = a + b"), std::runtime_error);
    // i16 + i32 → error
    EXPECT_THROW(runSource("a: i16 = 10\nb: i32 = 20\nc = a + b"), std::runtime_error);
    // i32 + float → error
    EXPECT_THROW(runSource("a: i32 = 10\nb = 3.14\nc = a + b"), std::runtime_error);
    // f32 + float → error
    EXPECT_THROW(runSource("a: f32 = 1.0\nb = 2.0\nc = a + b"), std::runtime_error);
    // i32 + f32 → error
    EXPECT_THROW(runSource("a: i32 = 10\nb: f32 = 2.0\nc = a + b"), std::runtime_error);
    // ** on low-level types → error
    EXPECT_THROW(runSource("a: i32 = 2\nb: i32 = 3\nc = a ** b"), std::runtime_error);
    // i64 + int → error (#595)
    EXPECT_THROW(runSource("a: i64 = 1\nb = 2\nc = a + b"), std::runtime_error);
    // int + i64 → error (#595)
    EXPECT_THROW(runSource("a = 1\nb: i64 = 2\nc = a + b"), std::runtime_error);
    // u64 + int → error (#595)
    EXPECT_THROW(runSource("a: u64 = 1\nb = 2\nc = a + b"), std::runtime_error);
    // i64 += int → error (#595)
    EXPECT_THROW(runSource("a: i64 = 1\na += 1"), std::runtime_error);
    // int += i64 → error (#595)
    EXPECT_THROW(runSource("a = 1\na += 1i64"), std::runtime_error);
    // u64 += int → error (#595)
    EXPECT_THROW(runSource("a: u64 = 1\na += 1"), std::runtime_error);
    // i64 literal + int literal → error (#595)
    EXPECT_THROW(runSource("c = 1i64 + 1"), std::runtime_error);
    // int literal + i64 literal → error (#595)
    EXPECT_THROW(runSource("c = 1 + 1i64"), std::runtime_error);
    // i64 + u64 → error (#595)
    EXPECT_THROW(runSource("a: i64 = 1\nb: u64 = 1\nc = a + b"), std::runtime_error);
    // i64 comparison with int → error (#595)
    EXPECT_THROW(runSource("a: i64 = 1\nb = a == 1"), std::runtime_error);
    // i64 bitwise with int → error (#595)
    EXPECT_THROW(runSource("a: i64 = 1\nb = a & 1"), std::runtime_error);
    // i64 + i64 → ok, verify value and metadata propagation (#595)
    EXPECT_EQ(runSource("a: i64 = 1\nb: i64 = 2\nc = a + b\nprint(c as int)"), "3\n");
    EXPECT_THROW(runSource("a: i64 = 1\nb: i64 = 2\nc = a + b\nd = c + 1"), std::runtime_error);
    // int + int → ok (regression)
    EXPECT_EQ(runSource("a = 1\nb = 2\nc = a + b\nprint(c)"), "3\n");
    // unary minus with i64 suffix (#595)
    EXPECT_EQ(runSource("c = 1i64 + -1i64\nprint(c as int)"), "0\n");
    EXPECT_EQ(runSource("c = -1i64 + -1i64\nprint(c as int)"), "-2\n");
    EXPECT_THROW(runSource("c = -1i64 + 1"), std::runtime_error);
    EXPECT_THROW(runSource("c = 1 + -1i64"), std::runtime_error);
}

// ===== Numeric literal suffix =====

TEST_F(CodeGenTest, NumericLiteralSuffix_i32) {
    EXPECT_EQ(runSource("x = 42i32\nprint(x)"), "42\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_u8) {
    EXPECT_EQ(runSource("x = 255u8\nprint(x)"), "255\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_f32) {
    EXPECT_EQ(runSource("x = 3.14f32\nprint(x)"), "3.14\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_IntWithF32) {
    // Integer literal with f32 suffix becomes float
    EXPECT_EQ(runSource("x = 42f32\nprint(x)"), "42.0\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_Hex) {
    EXPECT_EQ(runSource("x = 0xFFu8\nprint(x)"), "255\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_Binary) {
    EXPECT_EQ(runSource("x = 0b1010u8\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_Arithmetic) {
    EXPECT_EQ(runSource("c = 10i32 + 20i32\nprint(c)"), "30\n");
}

TEST_F(CodeGenTest, NumericLiteralSuffix_OutOfRange) {
    EXPECT_THROW(runSource("x = 256u8"), std::runtime_error);
    EXPECT_THROW(runSource("x = -1u8"), std::runtime_error);
    EXPECT_THROW(runSource("x = 129i8"), std::runtime_error);
    EXPECT_THROW(runSource("x = -129i8"), std::runtime_error);
    EXPECT_THROW(runSource("x = -1u32"), std::runtime_error);
}

// ===== Unsigned variable negation error (#312) =====

TEST_F(CodeGenTest, UnsignedVariableNegation_u8) {
    EXPECT_THROW(runSource("x: u8 = 5\ny = -x"), std::runtime_error);
}

TEST_F(CodeGenTest, UnsignedVariableNegation_u16) {
    EXPECT_THROW(runSource("x: u16 = 100\ny = -x"), std::runtime_error);
}

TEST_F(CodeGenTest, UnsignedVariableNegation_u32) {
    EXPECT_THROW(runSource("x: u32 = 42\ny = -x"), std::runtime_error);
}

TEST_F(CodeGenTest, UnsignedVariableNegation_u64) {
    EXPECT_THROW(runSource("x: u64 = 999\ny = -x"), std::runtime_error);
}

TEST_F(CodeGenTest, UnsignedFunctionReturnNegation) {
    EXPECT_THROW(runSource(
        "fn getU32() -> u32:\n"
        "    return 42u32\n"
        "y = -getU32()"), std::runtime_error);
}

// ===== Return type inference for named functions =====

TEST_F(CodeGenTest, ReturnTypeInference_Int) {
    // Omitted return type with return int → inferred as int
    EXPECT_EQ(runSource(
        "fn getVal():\n"
        "    return 42\n"
        "x = getVal()\n"
        "print(x + 1)"), "43\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Unit) {
    // Omitted return type with no return → inferred as Unit
    EXPECT_EQ(runSource(
        "fn sideEffect():\n"
        "    print(\"hi\")\n"
        "sideEffect()"), "hi\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Float) {
    EXPECT_EQ(runSource(
        "fn getPi():\n"
        "    return 3.14\n"
        "print(getPi())"), "3.14\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Str) {
    EXPECT_EQ(runSource(
        "fn getName():\n"
        "    return \"hello\"\n"
        "print(getName())"), "hello\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Bool) {
    EXPECT_EQ(runSource(
        "fn isOk():\n"
        "    return true\n"
        "print(isOk())"), "true\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_UnionIntFloat) {
    // Multiple return types → union type
    EXPECT_EQ(runSource(
        "fn mixed(flag: bool):\n"
        "    if flag:\n"
        "        return 42\n"
        "    return 3.14\n"
        "print(mixed(true))\n"
        "print(mixed(false))"), "42\n3.14\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_UnionStrInt) {
    EXPECT_EQ(runSource(
        "fn mixed(flag: bool):\n"
        "    if flag:\n"
        "        return \"hello\"\n"
        "    return 42\n"
        "print(mixed(true))\n"
        "print(mixed(false))"), "hello\n42\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_ExplicitAnyUnchanged) {
    // Explicit -> any still works as before
    EXPECT_EQ(runSource(
        "fn getVal() -> any:\n"
        "    return 42\n"
        "x: any = getVal()\n"
        "print(x)"), "42\n");
}

// ============================================================
// Fixed-length array T[N]
// ============================================================

TEST_F(CodeGenTest, ArrayBasicDecl) {
    EXPECT_EQ(runSource(
        "buf: i32[4] = [1, 2, 3, 4]\n"
        "print(buf[0])\n"
        "print(buf[3])"), "1\n4\n");
}

TEST_F(CodeGenTest, ArrayIndexRead) {
    EXPECT_EQ(runSource(
        "buf: i32[4] = [10, 20, 30, 40]\n"
        "print(buf[2])"), "30\n");
}

TEST_F(CodeGenTest, ArrayIndexWrite) {
    EXPECT_EQ(runSource(
        "buf: i32[3] = [0, 0, 0]\n"
        "buf[1] = 42\n"
        "print(buf[1])"), "42\n");
}

TEST_F(CodeGenTest, ArrayLength) {
    EXPECT_EQ(runSource(
        "buf: i32[8] = [1, 2, 3, 4, 5, 6, 7, 8]\n"
        "print(len(buf))"), "8\n");
}

TEST_F(CodeGenTest, ArrayPrint) {
    EXPECT_EQ(runSource(
        "buf: i32[3] = [10, 20, 30]\n"
        "print(buf)"), "[10, 20, 30]\n");
}

TEST_F(CodeGenTest, ArrayU8) {
    EXPECT_EQ(runSource(
        "buf: u8[3] = [65, 66, 67]\n"
        "print(buf[0])\n"
        "print(buf[2])"), "65\n67\n");
}

TEST_F(CodeGenTest, ArrayF32) {
    EXPECT_EQ(runSource(
        "buf: f32[2] = [1.5, 2.5]\n"
        "print(buf[0])\n"
        "print(buf[1])"), "1.5\n2.5\n");
}

TEST_F(CodeGenTest, ArrayI16) {
    EXPECT_EQ(runSource(
        "buf: i16[2] = [100, 200]\n"
        "print(buf[0])\n"
        "print(buf[1])"), "100\n200\n");
}

TEST_F(CodeGenTest, ArraySizeMismatch) {
    EXPECT_THROW(runSource("buf: i32[4] = [1, 2]"), std::runtime_error);
}

TEST_F(CodeGenTest, ArrayNonLowLevelType) {
    EXPECT_THROW(runSource("buf: str[2] = [\"a\", \"b\"]"), std::runtime_error);
}

TEST_F(CodeGenTest, ArrayBoundsError) {
    EXPECT_THROW(runSource(
        "buf: i32[2] = [1, 2]\n"
        "print(buf[5])"), std::runtime_error);
}

TEST_F(CodeGenTest, ArrayInitRangeCheck) {
    // i8 out of range
    EXPECT_THROW(runSource("buf: i8[1] = [999]"), std::runtime_error);
    // u8 out of range
    EXPECT_THROW(runSource("buf: u8[1] = [256]"), std::runtime_error);
    // u8 negative
    EXPECT_THROW(runSource("buf: u8[1] = [-1]"), std::runtime_error);
}

TEST_F(CodeGenTest, ArrayAssignRangeCheck) {
    EXPECT_THROW(runSource(
        "buf: i8[1] = [0]\n"
        "buf[0] = 999"), std::runtime_error);
    EXPECT_THROW(runSource(
        "buf: u8[1] = [0]\n"
        "buf[0] = 256"), std::runtime_error);
}

// ===== Checked/Saturating/Wrapping Arithmetic =====

TEST_F(CodeGenTest, CheckedAddOk) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(1i32, 2i32)\ncase r:\n  Ok(v):\n    print(v as int)\n  Err(e):\n    print(\"err\")"), "3\n");
}

TEST_F(CodeGenTest, CheckedAddOverflow) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(2147483647i32, 1i32)\ncase r:\n  Ok(v):\n    print(\"ok\")\n  Err(e):\n    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedSubOverflow) {
    EXPECT_EQ(runSource(
        "r = checkedSub(-2147483648i32, 1i32)\ncase r:\n  Ok(v):\n    print(\"ok\")\n  Err(e):\n    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedMulOverflow) {
    EXPECT_EQ(runSource(
        "r = checkedMul(100000i32, 100000i32)\ncase r:\n  Ok(v):\n    print(\"ok\")\n  Err(e):\n    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedUnsignedOverflow) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(255u8, 1u8)\ncase r:\n  Ok(v):\n    print(\"ok\")\n  Err(e):\n    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedI64Overflow) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(9223372036854775807i64, 1i64)\ncase r:\n  Ok(v):\n    print(\"ok\")\n  Err(e):\n    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, SaturatingAddMax) {
    EXPECT_EQ(runSource(
        "v = saturatingAdd(2147483647i32, 100i32)\n"
        "print(v as int)"), "2147483647\n");
}

TEST_F(CodeGenTest, SaturatingSubMin) {
    EXPECT_EQ(runSource(
        "v = saturatingSub(-2147483648i32, 1i32)\n"
        "print(v as int)"), "-2147483648\n");
}

TEST_F(CodeGenTest, SaturatingMulMax) {
    EXPECT_EQ(runSource(
        "v = saturatingMul(100000i32, 100000i32)\n"
        "print(v as int)"), "2147483647\n");
}

TEST_F(CodeGenTest, SaturatingUnsigned) {
    EXPECT_EQ(runSource(
        "v = saturatingAdd(250u8, 10u8)\n"
        "print(v as int)"), "255\n");
}

TEST_F(CodeGenTest, WrappingAdd) {
    EXPECT_EQ(runSource(
        "v = wrappingAdd(2147483647i32, 1i32)\n"
        "print(v as int)"), "-2147483648\n");
}

TEST_F(CodeGenTest, WrappingSubUnsigned) {
    EXPECT_EQ(runSource(
        "v = wrappingSub(0u8, 1u8)\n"
        "print(v as int)"), "255\n");
}

TEST_F(CodeGenTest, CheckedTypeMismatch) {
    EXPECT_THROW(runSource("checkedAdd(1i32, 1i16)"), std::runtime_error);
}

TEST_F(CodeGenTest, CheckedIntMixedLowLevel) {
    // int (no metadata) must not silently mix with a named low-level type.
    // Use typed variables to force int vs i64 — a bare literal 1 alongside
    // 1i64 can be coerced to i64 by type inference.
    EXPECT_THROW(runSource("a = 1\nb: i64 = 2i64\ncheckedAdd(a, b)"), std::runtime_error);
}

TEST_F(CodeGenTest, CheckedIntOk) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(1, 2)\n"
        "case r:\n"
        "  Ok(v): print(v)\n"
        "  Err(e): print(\"err\")"),
        "3\n");
}

TEST_F(CodeGenTest, CheckedIntOverflow) {
    EXPECT_EQ(runSource(
        "r = checkedAdd(9223372036854775807, 1)\n"
        "case r:\n"
        "  Ok(v): print(v)\n"
        "  Err(e): print(\"overflow\")"),
        "overflow\n");
}

TEST_F(CodeGenTest, SaturatingIntMax) {
    EXPECT_EQ(runSource("print(saturatingAdd(9223372036854775807, 100))"),
        "9223372036854775807\n");
}

TEST_F(CodeGenTest, WrappingIntOverflow) {
    EXPECT_EQ(runSource("print(wrappingAdd(9223372036854775807, 1))"),
        "-9223372036854775808\n");
}

TEST_F(CodeGenTest, CheckedFloat) {
    EXPECT_THROW(runSource("checkedAdd(1.0f32, 2.0f32)"), std::runtime_error);
}

// ===== Top-level bindings accessible from top-level functions (#817) =====

TEST_F(CodeGenTest, TopLevelConstFloatAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "PI: float = 3.14\n"
        "fn showPi() -> float:\n"
        "    return PI\n"
        "print(showPi())")), "3.14\n");
}

TEST_F(CodeGenTest, TopLevelConstIntAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "MAX: int = 10\n"
        "fn getMax() -> int:\n"
        "    return MAX\n"
        "print(getMax())")), "10\n");
}

TEST_F(CodeGenTest, TopLevelConstBoolAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "FLAG: bool = true\n"
        "fn getFlag() -> bool:\n"
        "    return FLAG\n"
        "print(getFlag())")), "true\n");
}

TEST_F(CodeGenTest, TopLevelConstStrLiteralAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "G: str = \"hello\"\n"
        "fn greet() -> str:\n"
        "    return G\n"
        "print(greet())")), "hello\n");
}

TEST_F(CodeGenTest, TopLevelConstStrComputedAccessibleFromFunction) {
    // Runtime-computed (not a bare string literal) initializer must also work.
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "G: str = \"foo\" + \"bar\"\n"
        "fn greet() -> str:\n"
        "    return G\n"
        "print(greet())")), "foobar\n");
}

TEST_F(CodeGenTest, TopLevelConstListAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "XS: List<int> = [10, 20, 30]\n"
        "fn headXs() -> int:\n"
        "    return XS[0]\n"
        "fn sum3() -> int:\n"
        "    return XS[0] + XS[1] + XS[2]\n"
        "print(headXs())\n"
        "print(sum3())")), "10\n60\n");
}

TEST_F(CodeGenTest, TopLevelConstMapAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "M: Map<str, int> = {\"a\": 1, \"b\": 2}\n"
        "fn getA() -> int:\n"
        "    return M[\"a\"]\n"
        "print(getA())")), "1\n");
}

TEST_F(CodeGenTest, TopLevelConstRecordFieldAccessibleFromFunction) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "@const\n"
        "P: Point = Point(11, 22)\n"
        "fn px() -> int:\n"
        "    return P.x\n"
        "fn py() -> int:\n"
        "    return P.y\n"
        "print(px())\n"
        "print(py())")), "11\n22\n");
}

TEST_F(CodeGenTest, TopLevelConstTransitiveCallChain) {
    // b -> a -> read top-level @const K
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "K: int = 5\n"
        "fn a() -> int:\n"
        "    return K\n"
        "fn b() -> int:\n"
        "    return a()\n"
        "print(b())")), "5\n");
}

TEST_F(CodeGenTest, TopLevelConstReadFromInnerLambda) {
    // An inner lambda inside a top-level function should resolve the top-level
    // @const via the module-global fallback (not via closure capture of a
    // non-existent local).
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "K: int = 42\n"
        "fn outer() -> int:\n"
        "    f = () -> int => K\n"
        "    return f()\n"
        "print(outer())")), "42\n");
}

TEST_F(CodeGenTest, TopLevelLetAccessibleFromFunction) {
    EXPECT_EQ(runSource(
        "x: int = 7\n"
        "fn getX() -> int:\n"
        "    return x\n"
        "print(getX())"), "7\n");
}

TEST_F(CodeGenTest, TopLevelLetMutableWriteThroughFromFunction) {
    // Reassigning a top-level mutable `let` from inside a function must
    // actually mutate the top-level binding (write-through), not silently
    // shadow it with a new local.
    EXPECT_EQ(runSource(
        "counter: int = 0\n"
        "fn bump():\n"
        "    counter = counter + 1\n"
        "bump()\n"
        "bump()\n"
        "bump()\n"
        "print(counter)"), "3\n");
}

TEST_F(CodeGenTest, TopLevelConstReassignFromFunctionThrows) {
    // Reassigning a top-level @const from inside a function must be rejected.
    EXPECT_THROW(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "N: int = 5\n"
        "fn bad():\n"
        "    N = 6\n"
        "bad()")), std::runtime_error);
}

TEST_F(CodeGenTest, TopLevelForwardReferenceSourceOrderStrict) {
    // Source-order strict: a function cannot reference a top-level binding
    // declared textually AFTER the function definition.
    EXPECT_THROW(runSource(withStdlibDirectiveDecls(
        "fn foo() -> int:\n"
        "    return X\n"
        "@const\n"
        "X: int = 1\n"
        "print(foo())")), std::runtime_error);
}

TEST_F(CodeGenTest, TopLevelBindingInsideNestedBlockStaysLocal) {
    // `let y = 1` inside a top-level `if` branch is NOT a module-level
    // binding (scope depth > 1). Functions should not see it.
    EXPECT_THROW(runSource(
        "if true:\n"
        "    y: int = 1\n"
        "fn readY() -> int:\n"
        "    return y\n"
        "print(readY())"), std::runtime_error);
}

TEST_F(CodeGenTest, TopLevelMutableFieldAssignFromFunction) {
    // Assigning to a field of a top-level MUTABLE record from inside a
    // function must actually mutate the top-level record via the pointer
    // trampoline. Before the fix this hit "undefined variable" at codegen.
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p: Point = Point(1, 2)\n"
        "fn bumpX():\n"
        "    p.x = 99\n"
        "bumpX()\n"
        "print(p.x)\n"
        "print(p.y)"), "99\n2\n");
}

TEST_F(CodeGenTest, TopLevelConstFieldAssignFromFunctionThrows) {
    // Mutating a field of a top-level @const record from inside a function
    // must be rejected by the @const immutability check. Verify the error
    // message explicitly so this test can distinguish the @const rejection
    // from an accidental "undefined variable" or other unrelated error.
    try {
        runSource(withStdlibDirectiveDecls(
            "record Point:\n"
            "    x: int\n"
            "    y: int\n"
            "@const\n"
            "P: Point = Point(1, 2)\n"
            "fn bad():\n"
            "    P.x = 99\n"
            "bad()"));
        FAIL() << "expected runtime_error for @const field assignment";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("@const"), std::string::npos)
            << "error message did not mention @const: " << msg;
    }
}

// A parameter named the same as a top-level @const must still shadow the
// module global: reading the parameter returns the argument value, not the
// module-level constant.
TEST_F(CodeGenTest, TopLevelConstShadowedByParameter) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "x: int = 100\n"
        "fn f(x: int) -> int:\n"
        "    return x\n"
        "print(f(3))")), "3\n");
}

// A parameter that shadows a top-level @const must be freely reassignable
// inside the function body. This is the shadowing regression for the
// isImmutable() bug where module-level @const leaked through by name.
TEST_F(CodeGenTest, ParameterCanReassignShadowingTopLevelConst) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const\n"
        "n: int = 100\n"
        "fn bump(n: int) -> int:\n"
        "    n = n + 1\n"
        "    return n\n"
        "print(bump(5))")), "6\n");
}

// An explicitly typed local declaration inside a function that happens to
// share a name with a top-level mutable binding must create a NEW local
// shadow, not be misrouted into a module-global write-through.
TEST_F(CodeGenTest, TypedLocalShadowsTopLevelLet) {
    EXPECT_EQ(runSource(
        "x: int = 7\n"
        "fn g() -> int:\n"
        "    x: int = 2\n"
        "    return x\n"
        "print(g())\n"
        "print(x)"), "2\n7\n");
}

// A local @const declaration inside a function that shadows a top-level
// mutable `let` must not be rejected as a reassignment of the module global.
TEST_F(CodeGenTest, LocalConstShadowsTopLevelLet) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "k: int = 1\n"
        "fn h() -> int:\n"
        "    @const\n"
        "    k: int = 99\n"
        "    return k\n"
        "print(h())\n"
        "print(k)")), "99\n1\n");
}

// A local mutable variable that shadows a top-level @const record must allow
// field mutation on the local without the module-level immutability leaking
// through by name.
TEST_F(CodeGenTest, LocalRecordShadowsTopLevelConstRecord) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "@const\n"
        "myPoint: Point = Point(1, 2)\n"
        "fn f() -> int:\n"
        "    myPoint: Point = Point(10, 20)\n"
        "    myPoint.x = 99\n"
        "    return myPoint.x\n"
        "print(f())\n"
        "print(myPoint.x)")), "99\n1\n");
}

// A weak top-level binding must be rejected from function-body reads with
// an explicit #817 follow-up error. The weak classification is captured at
// module-global registration time (in __ry_main__ context), because
// FnScope clears `weak_managed_vars_` on entry to a function, which would
// otherwise make the live `isWeakManaged()` check silently return false.
TEST_F(CodeGenTest, WeakTopLevelRejectedFromFunction) {
    EXPECT_THROW(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "w: weak List<int> = weak xs\n"
        "fn useW():\n"
        "    y = w\n"
        "useW()"), std::runtime_error);
}

// A top-level List reassigned many times from inside a function must not
// leak the old list values. The ARC classification (`is_arc_managed`) is
// captured at registration time; before the fix, FnScope cleared
// `arc_managed_vars_` and the write-through skipped ARC release. With the
// fix, emitModuleGlobalWriteThrough consults the cached flag and drives
// retain/release correctly. This test exercises the path end-to-end under
// the regular build; the ASan run in CI is the memory-safety gate.
TEST_F(CodeGenTest, TopLevelListReassignedManyTimesFromFunction) {
    EXPECT_EQ(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "fn replaceXs(i: int):\n"
        "    xs = [i, i+1, i+2]\n"
        "replaceXs(10)\n"
        "replaceXs(20)\n"
        "replaceXs(30)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])"), "30\n31\n32\n");
}

// A nested `for` loop inside a parallel-for body must correctly treat its
// own loop variable as local, even when a same-named top-level binding
// exists. Before the fix, the nested-for branch in the parallel-for
// scanner never seeded `var_names` into `localScopes`, so assignments to
// the inner loop variable were misclassified as module-global mutations.
TEST_F(CodeGenTest, ParallelForNestedLoopVarShadowsModuleGlobal) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "g: int = 999\n"
        "fn f() -> int:\n"
        "    @parallel\n"
        "    for i in 0..3:\n"
        "        for g in [1, 2, 3]:\n"
        "            g = g + 1\n"
        "    return g\n"
        "print(f())")), "999\n");
}

// A parallel-for inside a function body must accept an explicitly typed
// local declaration that happens to share a name with a top-level binding
// — it is a new local shadow, not a data-race-inducing mutation of the
// outer variable.
TEST_F(CodeGenTest, ParallelForInFunctionBodyAllowsShadowOfModuleGlobal) {
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "x: int = 100\n"
        "fn f():\n"
        "    @parallel\n"
        "    for i in 0..3:\n"
        "        x: int = i\n"
        "f()\n"
        "print(x)")), "100\n");
}

// The other side of the parallel-for gating: a PLAIN assignment (without
// a type annotation or @const) to a name that exists as a top-level
// module global IS a mutation of the outer binding, so the parallel-for
// validator must reject it to prevent a data race. Without this test the
// rejection branch in src/codegen_stmt_loop.cpp could regress silently.
TEST_F(CodeGenTest, ParallelForRejectsModuleGlobalMutation) {
    EXPECT_THROW(runSource(withStdlibDirectiveDecls(
        "x: int = 100\n"
        "fn f():\n"
        "    @parallel\n"
        "    for i in 0..3:\n"
        "        x = i\n"
        "f()")), std::runtime_error);
}

// `using` body in @parallel for must be scanned for outer mutations — without
// the UsingStmt arm in validateParallelFor, the assignment below would be
// silently accepted and codegen would later trip on the io.File kind check
// (different error). Asserting the specific message proves the new scan arm
// fires.
TEST_F(CodeGenTest, ParallelForRejectsOuterMutationInsideUsingBody) {
    expectCompileError(withStdlibDirectiveDecls(
        "x: int = 100\n"
        "fn f():\n"
        "    @parallel\n"
        "    for i in 0..3:\n"
        "        using h = 0:\n"
        "            x = i\n"
        "f()"), "parallel for cannot assign to outer variable");
}

// Top-level fixed-length array (`i32[N]`) must be indexable from a function
// body. Before the fix, the array GEP path in IndexExpr only fired for
// `llvm::AllocaInst` object pointers, so loading through the module-global
// trampoline (which returns a LoadInst result) fell through to the list/map
// path and reported "cannot determine list element type".
TEST_F(CodeGenTest, TopLevelFixedArrayIndexedFromFunction) {
    EXPECT_EQ(runSource(
        "buf: i32[4] = [10i32, 20i32, 30i32, 40i32]\n"
        "fn head() -> i32:\n"
        "    return buf[0]\n"
        "fn atTwo() -> i32:\n"
        "    return buf[2]\n"
        "print(head())\n"
        "print(atTwo())"), "10\n30\n");
}

// ===== #807 u64 max literals =====

TEST_F(CodeGenTest, U64MaxViaAnnotation) {
    EXPECT_EQ(runSource("h: u64 = 18446744073709551615\nprint(h)"),
              "18446744073709551615\n");
}

TEST_F(CodeGenTest, U64MaxViaSuffix) {
    EXPECT_EQ(runSource("x = 18446744073709551615u64\nprint(x)"),
              "18446744073709551615\n");
}

TEST_F(CodeGenTest, U64MaxViaHexAnnotation) {
    EXPECT_EQ(runSource("x: u64 = 0xFFFFFFFFFFFFFFFF\nprint(x)"),
              "18446744073709551615\n");
}

TEST_F(CodeGenTest, U64MaxReassignment) {
    // AssignStmt path must accept bare u64 max when variable is u64.
    EXPECT_EQ(runSource(
        "x: u64 = 0u64\n"
        "x = 18446744073709551615\n"
        "print(x)"),
        "18446744073709551615\n");
}

TEST_F(CodeGenTest, I64MaxPlus1BareIntRejected) {
    // After #807, INT64_MAX+1 passes the parser (bit-pattern stored), but
    // codegen must reject it when the target is bare `int` (= i64).
    EXPECT_THROW(runSource("x = 9223372036854775808\nprint(x)"),
                 std::runtime_error);
}

// #1025 – bare int INT64_MIN literal support
TEST_F(CodeGenTest, NegInt64MinBareLiteralAccepted) {
    EXPECT_EQ(runSource("x = -9223372036854775808\nprint(x)"),
              "-9223372036854775808\n");
}

TEST_F(CodeGenTest, NegBareIntMagnitudeOverI64MinRejected) {
    EXPECT_THROW(runSource("x = -9223372036854775809\nprint(x)"),
                 std::runtime_error);
}

TEST_F(CodeGenTest, NegInt64MinBareNegatedRuntimeOverflow) {
    EXPECT_EXIT(runSource("x = -9223372036854775808\nprint(-x)"),
                ::testing::ExitedWithCode(1),
                "runtime error: integer overflow");
}

TEST_F(CodeGenTest, U64OverflowInCodegenRejected) {
    EXPECT_THROW(runSource("x: u64 = 18446744073709551616\nprint(x)"),
                 std::runtime_error);
}

TEST_F(CodeGenTest, U32MaxLiteralAccepted) {
    EXPECT_EQ(runSource("x: u32 = 4294967295\nprint(x as int)"),
              "4294967295\n");
}

TEST_F(CodeGenTest, U32OverflowRejected) {
    EXPECT_THROW(runSource("x: u32 = 4294967296\nprint(x)"),
                 std::runtime_error);
}

// ===== #819 scientific notation =====

TEST_F(CodeGenTest, ScientificBasic) {
    EXPECT_EQ(runSource("print(1e10)"), "1e+10\n");
}

TEST_F(CodeGenTest, ScientificNegativeExponent) {
    EXPECT_EQ(runSource("print(1.5e-3)"), "0.0015\n");
}

TEST_F(CodeGenTest, ScientificPositiveSign) {
    EXPECT_EQ(runSource("print(2.5E+2)"), "250.0\n");
}

TEST_F(CodeGenTest, ScientificIntegerSuffixRejected) {
    EXPECT_THROW(runSource("print(1e10i32)"), std::runtime_error);
}

TEST_F(CodeGenTest, ScientificLeadingDot) {
    EXPECT_EQ(runSource("print(.5e2)"), "50.0\n");
}

// `128i8` (positive overflow) must be rejected; `-128i8` (INT8_MIN via
// unary-minus fast-path) must be accepted.
TEST_F(CodeGenTest, SignedSuffixPositiveMaxPlus1Rejected) {
    EXPECT_THROW(runSource("x = 128i8\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = 32768i16\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = 2147483648i32\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, SignedSuffixMinAcceptedViaUnaryMinus) {
    EXPECT_EQ(runSource("x = -128i8\nprint(x as int)"), "-128\n");
    EXPECT_EQ(runSource("x = -32768i16\nprint(x as int)"), "-32768\n");
    EXPECT_EQ(runSource("x = -2147483648i32\nprint(x as int)"), "-2147483648\n");
    EXPECT_EQ(runSource("x = -9223372036854775808i64\nprint(x)"),
              "-9223372036854775808\n");
}

TEST_F(CodeGenTest, U64ArrayInitializerMax) {
    // Fixed-length array initializer must inject the element suffix so
    // u64 max literals pass the codegen range check.
    EXPECT_EQ(runSource(
        "buf: u64[1] = [18446744073709551615]\n"
        "print(buf[0])"),
        "18446744073709551615\n");
}

TEST_F(CodeGenTest, U64AnnotationFormatterRoundtrip) {
    // Annotation-driven u64 max must format correctly (#807 formatter fix).
    // The formatter injects the suffix and renders the unsigned form.
    EXPECT_EQ(runSource(
        "h: u64 = 18446744073709551615\n"
        "print(h)"),
        "18446744073709551615\n");
}

// ===== Option-returning index `m[k]?` / `xs[i]?` rejections (#1699) =====

TEST_F(CodeGenTest, OptionIndexRejectsMapAssignmentTarget) {
    // Write-form `m[k]? = v` must be a compile error — `?` returns Option, not a slot.
    expectCompileError(
        "m: Map<str, int> = {}\nm[\"k\"]? = 1\n",
        "'?' index cannot be used as assignment target");
}

TEST_F(CodeGenTest, OptionIndexRejectsListAssignmentTarget) {
    // List write-form is rejected the same way.
    expectCompileError(
        "xs = [1, 2, 3]\nxs[0]? = 99\n",
        "'?' index cannot be used as assignment target");
}

TEST_F(CodeGenTest, OptionIndexRejectsCompoundAssignmentTarget) {
    // Compound assignment routes through the same lvalue path.
    expectCompileError(
        "m: Map<str, int> = {\"a\": 1}\nm[\"a\"]? += 1\n",
        "'?' index cannot be used as assignment target");
}

TEST_F(CodeGenTest, OptionIndexRejectsFieldAssignmentChain) {
    // `m[k]?.x = v` must be rejected at the CoW-chain layer.
    expectCompileError(
        "record P:\n  x: int\n"
        "m: Map<str, P> = {}\nm[\"k\"]?.x = 1\n",
        "'?' index cannot be used as assignment target");
}

TEST_F(CodeGenTest, OptionIndexRejectsNestedIndexAssignment) {
    // Nested `mm[k]?[k2] = v` likewise rejected via path-CoW guard.
    expectCompileError(
        "mm: Map<str, Map<str, int>> = {}\nmm[\"a\"]?[\"b\"] = 1\n",
        "'?' index cannot be used as assignment target");
}

TEST_F(CodeGenTest, OptionIndexRejectsFixedLengthArray) {
    // Fixed-length arrays already abort on OOB statically; `?` is meaningless.
    expectCompileError(
        "a: i32[3] = [1, 2, 3]\nx = a[0]?\n",
        "'?' index not supported for fixed-length arrays");
}

TEST_F(CodeGenTest, OptionIndexRejectsString) {
    // str does not support `[]` at all; the diagnostic is redirected to charAt(),
    // but `?` short-circuits to a dedicated message first.
    expectCompileError(
        "s = \"hi\"\nx = s[0]?\n",
        "'?' index not supported for str");
}

TEST_F(CodeGenTest, OptionIndexRejectsRangeSlice) {
    // Range slice `xs[a..b]` returns a new list, not a single element — Option<List<T>>
    // is intentionally out of scope for v0.0.25.
    expectCompileError(
        "xs = [1, 2, 3]\nys = xs[0..1]?\n",
        "'?' index not supported for range slice");
}

TEST_F(CodeGenTest, OptionIndexRejectsAnyTypedNested) {
    // `any`-typed nested `v[k]?` falls through to the existing list/map gate (#1701
    // will lift this restriction). The compile error is "index operator requires list
    // or map", not the dedicated try_mode message, but it's a compile error either way.
    expectCompileError(
        "a: any = 1\nv = a[\"x\"]?\n",
        "index operator requires list or map");
}

