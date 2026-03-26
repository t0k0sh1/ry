#include "test_codegen_common.hpp"

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

// ===== string 型テスト =====

TEST_F(CodeGenTest, PrintStringLiteral) {
    EXPECT_EQ(runSource("print(\"hello\")"), "hello\n");
}

TEST_F(CodeGenTest, PrintEmptyString) {
    EXPECT_EQ(runSource("print(\"\")"), "\n");
}

TEST_F(CodeGenTest, LetStringVariable) {
    EXPECT_EQ(runSource("s = \"world\"\nprint(s)"), "world\n");
}

TEST_F(CodeGenTest, LetStringImmutable) {
    EXPECT_EQ(runSource("s = \"hello\"\nprint(s)"), "hello\n");
}

TEST_F(CodeGenTest, StringTypeAnnotation) {
    EXPECT_EQ(runSource("s: str = \"typed\"\nprint(s)"), "typed\n");
}

TEST_F(CodeGenTest, StringTypeMismatchThrows) {
    EXPECT_THROW(runSource("s: str = 42"), std::runtime_error);
}

TEST_F(CodeGenTest, IntTypeAnnotationStrThrows) {
    EXPECT_THROW(runSource("s: int = \"hello\""), std::runtime_error);
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

// ===== let / var テスト =====

TEST_F(CodeGenTest, LetPrint) {
    EXPECT_EQ(runSource("x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, VarPrint) {
    EXPECT_EQ(runSource("x = 10\nprint(x)"), "10\n");
}

TEST_F(CodeGenTest, VarReassign) {
    EXPECT_EQ(runSource("x = 1\nx = 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, ConstReassignThrows) {
    EXPECT_THROW(runSource("@const\nx = 1\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, MutableReassignOk) {
    EXPECT_EQ(runSource("x = 1\nx = 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, RedeclaredConstThrows) {
    EXPECT_THROW(runSource("@const\nx = 1\n@const\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, RedeclaredVarThrows) {
    // Same scope redeclaration via @const is an error
    EXPECT_THROW(runSource("@const\nx = 1\n@const\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, LetWithTypeAnnotation) {
    EXPECT_EQ(runSource("x: int = 42\nprint(x)"), "42\n");
}

// ===== byte 型テスト =====

TEST_F(CodeGenTest, ByteDeclareAndPrint) {
    EXPECT_EQ(runSource("b: byte = 42\nprint(b)"), "42\n");
}

TEST_F(CodeGenTest, ByteZeroPrint) {
    EXPECT_EQ(runSource("b: byte = 0\nprint(b)"), "0\n");
}

TEST_F(CodeGenTest, ByteMaxPrint) {
    EXPECT_EQ(runSource("b: byte = 255\nprint(b)"), "255\n");
}

TEST_F(CodeGenTest, ByteAddPromotesToInt) {
    // byte + byte → int (i64)
    EXPECT_EQ(runSource("a: byte = 10\nb: byte = 20\nc = a + b\nprint(c)"), "30\n");
}

TEST_F(CodeGenTest, ByteAddIntPromotesToInt) {
    // byte + int → int
    EXPECT_EQ(runSource("a: byte = 10\nb = 100\nc = a + b\nprint(c)"), "110\n");
}

TEST_F(CodeGenTest, ByteSubtract) {
    EXPECT_EQ(runSource("a: byte = 50\nb: byte = 20\nc = a - b\nprint(c)"), "30\n");
}

TEST_F(CodeGenTest, ByteMultiply) {
    EXPECT_EQ(runSource("a: byte = 3\nb: byte = 4\nc = a * b\nprint(c)"), "12\n");
}

TEST_F(CodeGenTest, ByteComparisonEq) {
    EXPECT_EQ(runSource("a: byte = 10\nb: byte = 10\nprint(a == b)"), "true\n");
}

TEST_F(CodeGenTest, ByteComparisonLt) {
    EXPECT_EQ(runSource("a: byte = 5\nb: byte = 10\nprint(a < b)"), "true\n");
}

TEST_F(CodeGenTest, ByteBitwiseAnd) {
    // 0xFF & 0x0F = 0x0F = 15
    EXPECT_EQ(runSource("a: byte = 255\nb: byte = 15\nc = a & b\nprint(c)"), "15\n");
}

TEST_F(CodeGenTest, ByteBitwiseOr) {
    EXPECT_EQ(runSource("a: byte = 12\nb: byte = 10\nc = a | b\nprint(c)"), "14\n");
}

TEST_F(CodeGenTest, ByteBitwiseXor) {
    EXPECT_EQ(runSource("a: byte = 12\nb: byte = 10\nc = a ^ b\nprint(c)"), "6\n");
}

TEST_F(CodeGenTest, ByteOutOfRangeHighThrows) {
    EXPECT_THROW(runSource("b: byte = 256"), std::runtime_error);
}

TEST_F(CodeGenTest, ByteOutOfRangeNegativeThrows) {
    EXPECT_THROW(runSource("b: byte = -1"), std::runtime_error);
}

TEST_F(CodeGenTest, ByteVarReassign) {
    EXPECT_EQ(runSource("b: byte = 10\nb = 20\nprint(b)"), "20\n");
}

TEST_F(CodeGenTest, ByteVarReassignOutOfRangeThrows) {
    EXPECT_THROW(runSource("b: byte = 10\nb = 256"), std::runtime_error);
}

TEST_F(CodeGenTest, ByteDivisionProducesFloat) {
    // byte / byte → float
    EXPECT_EQ(runSource("a: byte = 7\nb: byte = 2\nc = a / b\nprint(c)"), "3.5\n");
}

// ===== Hex / Binary literal tests =====

TEST_F(CodeGenTest, HexLiteral) {
    EXPECT_EQ(runSource("print(0xFF)"), "255\n");
}

TEST_F(CodeGenTest, HexLiteralArithmetic) {
    EXPECT_EQ(runSource("print(0x10 + 0x20)"), "48\n");
}

TEST_F(CodeGenTest, BinaryLiteral) {
    EXPECT_EQ(runSource("print(0b1010)"), "10\n");
}

TEST_F(CodeGenTest, BinaryLiteralBitwiseAnd) {
    EXPECT_EQ(runSource("print(0b1100 & 0b1010)"), "8\n");
}

TEST_F(CodeGenTest, HexByteAssignment) {
    EXPECT_EQ(runSource("b: byte = 0xFF\nprint(b)"), "255\n");
}

TEST_F(CodeGenTest, BinaryByteAssignment) {
    EXPECT_EQ(runSource("b: byte = 0b11111111\nprint(b)"), "255\n");
}

TEST_F(CodeGenTest, NegativeHexLiteral) {
    EXPECT_EQ(runSource("print(-0xFF)"), "-255\n");
}

// ===== 論理右シフト >>> テスト =====

TEST_F(CodeGenTest, LogicalRightShiftPositive) {
    // 8 >>> 2 = 2
    EXPECT_EQ(runSource("x = 8 >>> 2\nprint(x)"), "2\n");
}

TEST_F(CodeGenTest, LogicalRightShiftNegative) {
    // -1 >>> 1 = 9223372036854775807 (unsigned shift of all-ones)
    EXPECT_EQ(runSource("x = -1 >>> 1\nprint(x)"), "9223372036854775807\n");
}

// ===== 文字列繰り返し * テスト =====

TEST_F(CodeGenTest, StringRepeat) {
    EXPECT_EQ(runSource("x = \"ab\" * 3\nprint(x)"), "ababab\n");
}

TEST_F(CodeGenTest, StringRepeatCommutative) {
    EXPECT_EQ(runSource("x = 3 * \"ab\"\nprint(x)"), "ababab\n");
}

TEST_F(CodeGenTest, StringRepeatZero) {
    EXPECT_EQ(runSource("x = \"x\" * 0\nprint(x)"), "\n");
}

TEST_F(CodeGenTest, StringRepeatNegative) {
    EXPECT_EQ(runSource("x = \"x\" * -1\nprint(x)"), "\n");
}

TEST_F(CodeGenTest, StringRepeatEmpty) {
    EXPECT_EQ(runSource("x = \"\" * 5\nprint(x)"), "\n");
}

TEST_F(CodeGenTest, StringRepeatOne) {
    EXPECT_EQ(runSource("x = \"a\" * 1\nprint(x)"), "a\n");
}

// ===== exit() テスト =====

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

TEST_F(CodeGenTest, ExitFloatArgThrows) {
    EXPECT_THROW(runSource("exit(1.5)"), std::runtime_error);
}

TEST_F(CodeGenTest, ExitNoArgThrows) {
    EXPECT_THROW(runSource("exit()"), std::runtime_error);
}

TEST_F(CodeGenTest, ExitTooManyArgsThrows) {
    EXPECT_THROW(runSource("exit(1, 2)"), std::runtime_error);
}

// ===== args() テスト =====

TEST_F(CodeGenTest, ArgsEmpty) {
    EXPECT_EQ(runSourceWithArgs("a = args()\nprint(length(a))", {}), "0\n");
}

TEST_F(CodeGenTest, ArgsWithValues) {
    EXPECT_EQ(runSourceWithArgs(
        "a = args()\nprint(length(a))\nprint(a[0])\nprint(a[1])",
        {"hello", "world"}), "2\nhello\nworld\n");
}

TEST_F(CodeGenTest, ArgsIteration) {
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

// ===== zero-division guard tests =====

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

TEST_F(CodeGenTest, FloorDivNonZeroWorks) {
    EXPECT_EQ(runSource("print(7 // 2)"), "3\n");
    EXPECT_EQ(runSource("print(-7 // 2)"), "-4\n");
}

TEST_F(CodeGenTest, ModNonZeroWorks) {
    EXPECT_EQ(runSource("print(7 % 3)"), "1\n");
}

// --- any type: reject non-str pointer types (#233) ---

TEST_F(CodeGenTest, AnyRejectsListType) {
    EXPECT_THROW(runSource("x: any = [1, 2, 3]"), std::runtime_error);
}

TEST_F(CodeGenTest, AnyRejectsMapType) {
    EXPECT_THROW(runSource("x: any = {\"a\": 1}"), std::runtime_error);
}

TEST_F(CodeGenTest, AnyRejectsSetType) {
    EXPECT_THROW(runSource("x: any = {1, 2, 3}"), std::runtime_error);
}

TEST_F(CodeGenTest, AnyRejectsCollectionInFnParam) {
    EXPECT_THROW(runSource(
        "fn f(x):\n"
        "    return x\n"
        "f([1, 2, 3])"), std::runtime_error);
}

TEST_F(CodeGenTest, AnyRejectsCollectionInFnReturn) {
    EXPECT_THROW(runSource(
        "fn f() -> any:\n"
        "    return [1, 2, 3]"), std::runtime_error);
}
