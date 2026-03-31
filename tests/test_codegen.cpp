#include "test_codegen_common.hpp"

// ===== Print literals =====

TEST_F(CodeGenTest, PrintLiterals) {
    EXPECT_EQ(runSource("print(42)"), "42\n");
    EXPECT_EQ(runSource("x = -10\nprint(x)"), "-10\n");
    EXPECT_EQ(runSource("print(3.14)"), "3.14\n");
    // %g 形式: 1.0 → "1"
    EXPECT_EQ(runSource("print(1.0)"), "1\n");
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
    // 2 ** 10 = 1024 (f64 → %g → "1024")
    EXPECT_EQ(runSource("x = 2 ** 10\nprint(x)"), "1024\n");
    // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
    EXPECT_EQ(runSource("x = 2 ** 3 ** 2\nprint(x)"), "512\n");
}

// ===== Arithmetic (float) =====

TEST_F(CodeGenTest, ArithmeticFloat) {
    EXPECT_EQ(runSource("x = 7 / 2\nprint(x)"), "3.5\n");
    EXPECT_EQ(runSource("x = 1.5 + 2.5\nprint(x)"), "4\n");
    EXPECT_EQ(runSource("x = 1 + 2.5\nprint(x)"), "3.5\n");
    EXPECT_EQ(runSource("x = 5.5 % 2.0\nprint(x)"), "1.5\n");
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
    EXPECT_THROW(runSource("@const\nx = 1\nx = 2"), std::runtime_error);
    EXPECT_THROW(runSource("@const\nx = 1\n@const\nx = 2"), std::runtime_error);
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
        "function operator==(a: Pt, b: Pt):\n"
        "    return 42\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorAndExplicitNonBoolThrows) {
    EXPECT_THROW(runSource(
        "record Pt:\n"
        "    x: int\n"
        "function operator and(a: Pt, b: Pt) -> int:\n"
        "    return 1\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorOrExplicitNonBoolThrows) {
    EXPECT_THROW(runSource(
        "record Pt:\n"
        "    x: int\n"
        "function operator or(a: Pt, b: Pt) -> int:\n"
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
    // true(i1=1) & 3 → ZExt → 1 & 3 = 1
    EXPECT_EQ(runSource("x = true & 3\nprint(x)"), "1\n");
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

// ===== Type change throws =====

TEST_F(CodeGenTest, TypeChangeThrows) {
    EXPECT_THROW(runSource("x = 1\nx = 1.5\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = 1.5\nx = 2\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = 1\nx = true\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = true\nx = 1\nprint(x)"), std::runtime_error);
    EXPECT_EQ(runSource("x = 1.5\nx = 2.5\nprint(x)"), "2.5\n");
    EXPECT_THROW(runSource("x = 1\nprint(x)\nx = 1.5"), std::runtime_error);
}

// ===== Type annotation =====

TEST_F(CodeGenTest, TypeAnnotationMatch) {
    EXPECT_EQ(runSource("a: int = 10\nprint(a)"), "10\n");
    EXPECT_EQ(runSource("b: float = 3.14\nprint(b)"), "3.14\n");
    EXPECT_EQ(runSource("c: bool = true\nprint(c)"), "true\n");
}

TEST_F(CodeGenTest, TypeAnnotationMismatchThrows) {
    EXPECT_THROW(runSource("a: int = 3.14"), std::runtime_error);
    EXPECT_THROW(runSource("a: float = 10"), std::runtime_error);
    EXPECT_THROW(runSource("a: bool = 10"), std::runtime_error);
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

// ===== arguments() tests =====

TEST_F(CodeGenTest, ArgumentsTests) {
    EXPECT_EQ(runSourceWithArgs("a = arguments()\nprint(length(a))", {}), "0\n");
    EXPECT_EQ(runSourceWithArgs(
        "a = arguments()\nprint(length(a))\nprint(a[0])\nprint(a[1])",
        {"hello", "world"}), "2\nhello\nworld\n");
    EXPECT_EQ(runSourceWithArgs(
        "for x in arguments():\n    print(x)",
        {"foo", "bar"}), "foo\nbar\n");
}

// ===== @native function missing dispatcher detection =====

TEST_F(CodeGenTest, NativeFunctionMissingDispatcher) {
    try {
        runSource(
            "@native\n"
            "function unhandled_native(x: str) -> str\n"
            "\n"
            "print(unhandled_native(\"hello\"))\n"
        );
        FAIL() << "Expected exception for unhandled @native function";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("not handled by any builtin dispatcher"), std::string::npos)
            << "Error was: " << msg;
    }
}

// ===== Zero division guard tests (death tests - individual) =====

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

// ===== any type rejection =====

TEST_F(CodeGenTest, AnyTypeRejection) {
    EXPECT_THROW(runSource("x: any = [1, 2, 3]"), std::runtime_error);
    EXPECT_THROW(runSource("x: any = {\"a\": 1}"), std::runtime_error);
    EXPECT_THROW(runSource("x: any = {1, 2, 3}"), std::runtime_error);
    EXPECT_THROW(runSource(
        "function f(x):\n"
        "    return x\n"
        "f([1, 2, 3])"), std::runtime_error);
    EXPECT_THROW(runSource(
        "function f() -> any:\n"
        "    return [1, 2, 3]"), std::runtime_error);
    EXPECT_THROW(runSource(
        "function f() -> int:\n"
        "    return 42\n"
        "x: any = f"), std::runtime_error);
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
    EXPECT_EQ(runSource("x: f32 = 0.0\nprint(x)"), "0\n");
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
    EXPECT_EQ(runSource("a: f32 = 1.5\nb: f32 = 2.5\nc = a + b\nprint(c)"), "4\n");
    EXPECT_EQ(runSource("a: f32 = 5.0\nb: f32 = 2.0\nc = a - b\nprint(c)"), "3\n");
    EXPECT_EQ(runSource("a: f32 = 3.0\nb: f32 = 4.0\nc = a * b\nprint(c)"), "12\n");
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
    EXPECT_EQ(runSource("x: i32 = 42\ny = x as float\nprint(y)"), "42\n");
    EXPECT_EQ(runSource("x = 3.14\ny = x as i32\nprint(y)"), "3\n");
    // f32 <-> float
    EXPECT_EQ(runSource("x: f32 = 2.5\ny = x as float\nprint(y)"), "2.5\n");
    EXPECT_EQ(runSource("x = 2.5\ny = x as f32\ny2 = y as float\nprint(y2)"), "2.5\n");
    // i32 <-> f32
    EXPECT_EQ(runSource("x: i32 = 42\ny = x as f32\ny2 = y as float\nprint(y2)"), "42\n");
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
    EXPECT_EQ(runSource("a: f32 = 1.5\nb: f32 = 2.5\nc = a + b\nprint(c)"), "4\n");
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
    EXPECT_EQ(runSource("x = 42f32\nprint(x)"), "42\n");
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
        "function get_u32() -> u32:\n"
        "    return 42u32\n"
        "y = -get_u32()"), std::runtime_error);
}

// ===== Return type inference for named functions =====

TEST_F(CodeGenTest, ReturnTypeInference_Int) {
    // Omitted return type with return int → inferred as int
    EXPECT_EQ(runSource(
        "function get_val():\n"
        "    return 42\n"
        "x = get_val()\n"
        "print(x + 1)"), "43\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Unit) {
    // Omitted return type with no return → inferred as Unit
    EXPECT_EQ(runSource(
        "function side_effect():\n"
        "    print(\"hi\")\n"
        "side_effect()"), "hi\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Float) {
    EXPECT_EQ(runSource(
        "function get_pi():\n"
        "    return 3.14\n"
        "print(get_pi())"), "3.14\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Str) {
    EXPECT_EQ(runSource(
        "function get_name():\n"
        "    return \"hello\"\n"
        "print(get_name())"), "hello\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_Bool) {
    EXPECT_EQ(runSource(
        "function is_ok():\n"
        "    return true\n"
        "print(is_ok())"), "true\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_UnionIntFloat) {
    // Multiple return types → union type
    EXPECT_EQ(runSource(
        "function mixed(flag: bool):\n"
        "    if flag:\n"
        "        return 42\n"
        "    return 3.14\n"
        "print(mixed(true))\n"
        "print(mixed(false))"), "42\n3.14\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_UnionStrInt) {
    EXPECT_EQ(runSource(
        "function mixed(flag: bool):\n"
        "    if flag:\n"
        "        return \"hello\"\n"
        "    return 42\n"
        "print(mixed(true))\n"
        "print(mixed(false))"), "hello\n42\n");
}

TEST_F(CodeGenTest, ReturnTypeInference_ExplicitAnyUnchanged) {
    // Explicit -> any still works as before
    EXPECT_EQ(runSource(
        "function get_val() -> any:\n"
        "    return 42\n"
        "x: any = get_val()\n"
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
        "print(length(buf))"), "8\n");
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
        "r = checked_add(1i32, 2i32)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(v as int)\n"
        "  case Err(e):\n"
        "    print(\"err\")"), "3\n");
}

TEST_F(CodeGenTest, CheckedAddOverflow) {
    EXPECT_EQ(runSource(
        "r = checked_add(2147483647i32, 1i32)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(\"ok\")\n"
        "  case Err(e):\n"
        "    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedSubOverflow) {
    EXPECT_EQ(runSource(
        "r = checked_sub(-2147483648i32, 1i32)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(\"ok\")\n"
        "  case Err(e):\n"
        "    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedMulOverflow) {
    EXPECT_EQ(runSource(
        "r = checked_mul(100000i32, 100000i32)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(\"ok\")\n"
        "  case Err(e):\n"
        "    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedUnsignedOverflow) {
    EXPECT_EQ(runSource(
        "r = checked_add(255u8, 1u8)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(\"ok\")\n"
        "  case Err(e):\n"
        "    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, CheckedI64Overflow) {
    EXPECT_EQ(runSource(
        "r = checked_add(9223372036854775807i64, 1i64)\n"
        "when r:\n"
        "  case Ok(v):\n"
        "    print(\"ok\")\n"
        "  case Err(e):\n"
        "    print(\"overflow\")"), "overflow\n");
}

TEST_F(CodeGenTest, SaturatingAddMax) {
    EXPECT_EQ(runSource(
        "v = saturating_add(2147483647i32, 100i32)\n"
        "print(v as int)"), "2147483647\n");
}

TEST_F(CodeGenTest, SaturatingSubMin) {
    EXPECT_EQ(runSource(
        "v = saturating_sub(-2147483648i32, 1i32)\n"
        "print(v as int)"), "-2147483648\n");
}

TEST_F(CodeGenTest, SaturatingMulMax) {
    EXPECT_EQ(runSource(
        "v = saturating_mul(100000i32, 100000i32)\n"
        "print(v as int)"), "2147483647\n");
}

TEST_F(CodeGenTest, SaturatingUnsigned) {
    EXPECT_EQ(runSource(
        "v = saturating_add(250u8, 10u8)\n"
        "print(v as int)"), "255\n");
}

TEST_F(CodeGenTest, WrappingAdd) {
    EXPECT_EQ(runSource(
        "v = wrapping_add(2147483647i32, 1i32)\n"
        "print(v as int)"), "-2147483648\n");
}

TEST_F(CodeGenTest, WrappingSubUnsigned) {
    EXPECT_EQ(runSource(
        "v = wrapping_sub(0u8, 1u8)\n"
        "print(v as int)"), "255\n");
}

TEST_F(CodeGenTest, CheckedTypeMismatch) {
    EXPECT_THROW(runSource("checked_add(1i32, 1i16)"), std::runtime_error);
}

TEST_F(CodeGenTest, CheckedNonLowLevel) {
    EXPECT_THROW(runSource("checked_add(1, 2)"), std::runtime_error);
}

TEST_F(CodeGenTest, CheckedFloat) {
    EXPECT_THROW(runSource("checked_add(1.0f32, 2.0f32)"), std::runtime_error);
}
