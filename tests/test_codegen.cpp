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

// ===== Byte basics =====

TEST_F(CodeGenTest, ByteBasics) {
    EXPECT_EQ(runSource("b: byte = 42\nprint(b)"), "42\n");
    EXPECT_EQ(runSource("b: byte = 0\nprint(b)"), "0\n");
    EXPECT_EQ(runSource("b: byte = 255\nprint(b)"), "255\n");
    EXPECT_EQ(runSource("b: byte = 10\nb = 20\nprint(b)"), "20\n");
}

TEST_F(CodeGenTest, ByteArithmetic) {
    // byte + byte → int (i64)
    EXPECT_EQ(runSource("a: byte = 10\nb: byte = 20\nc = a + b\nprint(c)"), "30\n");
    // byte + int → int
    EXPECT_EQ(runSource("a: byte = 10\nb = 100\nc = a + b\nprint(c)"), "110\n");
    EXPECT_EQ(runSource("a: byte = 50\nb: byte = 20\nc = a - b\nprint(c)"), "30\n");
    EXPECT_EQ(runSource("a: byte = 3\nb: byte = 4\nc = a * b\nprint(c)"), "12\n");
    // byte / byte → float
    EXPECT_EQ(runSource("a: byte = 7\nb: byte = 2\nc = a / b\nprint(c)"), "3.5\n");
}

TEST_F(CodeGenTest, ByteComparisonAndBitwise) {
    EXPECT_EQ(runSource("a: byte = 10\nb: byte = 10\nprint(a == b)"), "true\n");
    EXPECT_EQ(runSource("a: byte = 5\nb: byte = 10\nprint(a < b)"), "true\n");
    // 0xFF & 0x0F = 0x0F = 15
    EXPECT_EQ(runSource("a: byte = 255\nb: byte = 15\nc = a & b\nprint(c)"), "15\n");
    EXPECT_EQ(runSource("a: byte = 12\nb: byte = 10\nc = a | b\nprint(c)"), "14\n");
    EXPECT_EQ(runSource("a: byte = 12\nb: byte = 10\nc = a ^ b\nprint(c)"), "6\n");
}

TEST_F(CodeGenTest, ByteOutOfRange) {
    EXPECT_THROW(runSource("b: byte = 256"), std::runtime_error);
    EXPECT_THROW(runSource("b: byte = -1"), std::runtime_error);
    EXPECT_THROW(runSource("b: byte = 10\nb = 256"), std::runtime_error);
}

// ===== Hex / Binary literal tests =====

TEST_F(CodeGenTest, HexBinaryLiterals) {
    EXPECT_EQ(runSource("print(0xFF)"), "255\n");
    EXPECT_EQ(runSource("print(0x10 + 0x20)"), "48\n");
    EXPECT_EQ(runSource("print(0b1010)"), "10\n");
    EXPECT_EQ(runSource("print(0b1100 & 0b1010)"), "8\n");
    EXPECT_EQ(runSource("b: byte = 0xFF\nprint(b)"), "255\n");
    EXPECT_EQ(runSource("b: byte = 0b11111111\nprint(b)"), "255\n");
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

// ===== args() tests =====

TEST_F(CodeGenTest, ArgsTests) {
    EXPECT_EQ(runSourceWithArgs("a = args()\nprint(length(a))", {}), "0\n");
    EXPECT_EQ(runSourceWithArgs(
        "a = args()\nprint(length(a))\nprint(a[0])\nprint(a[1])",
        {"hello", "world"}), "2\nhello\nworld\n");
    EXPECT_EQ(runSourceWithArgs(
        "for x in args():\n    print(x)",
        {"foo", "bar"}), "foo\nbar\n");
}

// ===== @native function missing dispatcher detection =====

TEST_F(CodeGenTest, NativeFunctionMissingDispatcher) {
    try {
        runSource(
            "@native\n"
            "fn unhandled_native(x: str) -> str\n"
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
}

// ===== any type rejection =====

TEST_F(CodeGenTest, AnyTypeRejection) {
    EXPECT_THROW(runSource("x: any = [1, 2, 3]"), std::runtime_error);
    EXPECT_THROW(runSource("x: any = {\"a\": 1}"), std::runtime_error);
    EXPECT_THROW(runSource("x: any = {1, 2, 3}"), std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f(x):\n"
        "    return x\n"
        "f([1, 2, 3])"), std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f() -> any:\n"
        "    return [1, 2, 3]"), std::runtime_error);
    EXPECT_THROW(runSource(
        "fn f() -> int:\n"
        "    return 42\n"
        "x: any = f"), std::runtime_error);
}
