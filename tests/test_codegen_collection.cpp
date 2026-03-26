#include "test_codegen_common.hpp"

// ===== リスト型テスト =====

TEST_F(CodeGenTest, ListCreateAndAccess) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, ListLen) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ListMixedAccess) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])";
    EXPECT_EQ(runSource(src), "10\n20\n30\n");
}

TEST_F(CodeGenTest, ListWithTypeAnnotation) {
    std::string src =
        "xs: List<int> = [1, 2, 3]\n"
        "print(xs[0])\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "1\n3\n");
}

TEST_F(CodeGenTest, ListIndexOutOfRange) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[5])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListTypeMismatch) {
    EXPECT_THROW(runSource("xs = [1, 3.14]"), std::runtime_error);
}

TEST_F(CodeGenTest, ListEmpty) {
    EXPECT_THROW(runSource("xs = []"), std::runtime_error);
}

TEST_F(CodeGenTest, ListPrint) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListInFunction) {
    std::string src =
        "fn head(xs: List<int>) -> int:\n"
        "    return xs[0]\n"
        "xs = [10, 20, 30]\n"
        "print(head(xs))";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, ListLenInFunction) {
    std::string src =
        "fn size(xs: List<int>) -> int:\n"
        "    return length(xs)\n"
        "xs = [1, 2, 3, 4, 5]\n"
        "print(size(xs))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, ListFloatElements) {
    std::string src =
        "xs = [1.5, 2.5, 3.5]\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "2.5\n");
}

TEST_F(CodeGenTest, ListPrintFloat) {
    std::string src =
        "xs = [1.5, 2.5]\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
}

TEST_F(CodeGenTest, ListNegativeIndex) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[-1])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListIndexWithExpression) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "i = 1\n"
        "print(xs[i])";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, ListStringElements) {
    std::string src =
        "xs = [\"hello\", \"world\"]\n"
        "print(xs[0])\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
}

// ===== UFCS コード生成テスト =====

TEST_F(CodeGenTest, UFCSIntLiteral) {
    std::string src =
        "fn double(x: int) -> int:\n"
        "    return x * 2\n"
        "print(5.double())";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, UFCSBasicCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = 1\n"
        "print(x.add(2))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, UFCSChainedCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n"
        "x = 2\n"
        "print(x.add(3).mul(4))";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, UFCSWithStruct) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "p = Point(42, 99)\n"
        "print(p.get_x())";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== Map型テスト =====

TEST_F(CodeGenTest, MapCreateAndAccess) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "print(m[\"a\"])";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, MapAccessSecondKey) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "print(m[\"b\"])";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapLen) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "print(length(m))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapPrint) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "print(m)";
    EXPECT_EQ(runSource(src), "{a: 1, b: 2}\n");
}

TEST_F(CodeGenTest, MapKeyAssign) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "m[\"c\"] = 3\n"
        "print(m[\"c\"])\n"
        "print(length(m))";
    EXPECT_EQ(runSource(src), "3\n3\n");
}

TEST_F(CodeGenTest, MapKeyUpdate) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "m[\"a\"] = 99\n"
        "print(m[\"a\"])";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, MapKeyNotFound) {
    std::string src =
        "m = {\"a\": 1}\n"
        "print(m[\"z\"])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, MapWithTypeAnnotation) {
    std::string src =
        "m: Map<str, int> = {\"x\": 42}\n"
        "print(m[\"x\"])";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, MapIntKeys) {
    std::string src =
        "m = {1: \"hello\", 2: \"world\"}\n"
        "print(m[1])";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, MapInFunction) {
    std::string src =
        "fn get_val(m: Map<str, int>, k: str) -> int:\n"
        "    return m[k]\n"
        "m = {\"a\": 10, \"b\": 20}\n"
        "print(get_val(m, \"b\"))";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, MapHasKey) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "print(m.has_key(\"a\"))\n"
        "print(m.has_key(\"z\"))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

// ===== 文字列メソッドテスト =====

TEST_F(CodeGenTest, StringLen) {
    EXPECT_EQ(runSource("print(length(\"hello\"))"), "5\n");
}

TEST_F(CodeGenTest, StringLenEmpty) {
    EXPECT_EQ(runSource("print(length(\"\"))"), "0\n");
}

TEST_F(CodeGenTest, StringLenUFCS) {
    EXPECT_EQ(runSource("s = \"hello\"\nprint(s.length())"), "5\n");
}

TEST_F(CodeGenTest, StringEqual) {
    std::string src =
        "a = \"hello\"\n"
        "b = \"hello\"\n"
        "print(a == b)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringNotEqual) {
    std::string src =
        "a = \"hello\"\n"
        "b = \"world\"\n"
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
        "a = \"hello\"\n"
        "b = \" world\"\n"
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
        "s = \"hello world\"\n"
        "print(contains(s, \"world\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, StringContainsFalse) {
    EXPECT_EQ(runSource("print(contains(\"hello\", \"xyz\"))"), "false\n");
}

TEST_F(CodeGenTest, StringContainsUFCS) {
    std::string src =
        "s = \"hello world\"\n"
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
        "s = \"hello\"\n"
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
        "s = \"hello\"\n"
        "print(s.ends_with(\"llo\"))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, MapTypeMismatch) {
    EXPECT_THROW(runSource("m = {\"a\": 1, \"b\": 3.14}"), std::runtime_error);
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
        "a = 5\n"
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
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "v1 = Vec2(1, 2)\n"
        "v2 = Vec2(3, 4)\n"
        "v3 = v1 + v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "4\n6\n");
}

TEST_F(CodeGenTest, OperatorOverloadStructMinus) {
    std::string src =
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator-(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x - b.x, a.y - b.y)\n"
        "v1 = Vec2(5, 8)\n"
        "v2 = Vec2(2, 3)\n"
        "v3 = v1 - v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "3\n5\n");
}

TEST_F(CodeGenTest, OperatorOverloadStructEq) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator==(a: Point, b: Point) -> bool:\n"
        "    return a.x == b.x and a.y == b.y\n"
        "p1 = Point(1, 2)\n"
        "p2 = Point(1, 2)\n"
        "p3 = Point(3, 4)\n"
        "print(p1 == p2)\n"
        "print(p1 == p3)";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, OperatorOverloadUnaryMinus) {
    std::string src =
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator-(a: Vec2) -> Vec2:\n"
        "    return Vec2(0 - a.x, 0 - a.y)\n"
        "v = Vec2(3, 5)\n"
        "neg = -v\n"
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
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "fn operator*(a: Vec2, s: int) -> Vec2:\n"
        "    return Vec2(a.x * s, a.y * s)\n"
        "v = Vec2(1, 2)\n"
        "v2 = v * 3\n"
        "v3 = v + v2\n"
        "print(v3.x)\n"
        "print(v3.y)";
    EXPECT_EQ(runSource(src), "4\n8\n");
}

// ===== List element assignment =====

TEST_F(CodeGenTest, ListIndexAssignInt) {
    std::string src =
        "xs = [1, 2, 3]\n"
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
        "xs = [1.0, 2.0, 3.0]\n"
        "xs[1] = 9.5\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "9.5\n");
}

TEST_F(CodeGenTest, ListIndexAssignBool) {
    std::string src =
        "xs = [true, false, true]\n"
        "xs[0] = false\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, ListIndexAssignStr) {
    std::string src =
        "xs = [\"hello\", \"world\"]\n"
        "xs[0] = \"goodbye\"\n"
        "print(xs[0])";
    EXPECT_EQ(runSource(src), "goodbye\n");
}

TEST_F(CodeGenTest, ListIndexAssignOutOfRange) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "xs[5] = 99";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListIndexAssignNegative) {
    std::string src =
        "xs = [1, 2, 3]\n"
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
        "xs = [10, 20, 30]\n"
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
        "xs = [1, 2, 3, 4, 5]\n"
        "sum = 0\n"
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
        "words = [\"hello\", \"world\"]\n"
        "for w in words:\n"
        "    print(w)";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
}

// ===== Compound assignment =====

TEST_F(CodeGenTest, CompoundAssignPlus) {
    std::string src =
        "x = 10\n"
        "x += 5\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, CompoundAssignMinus) {
    std::string src =
        "x = 10\n"
        "x -= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, CompoundAssignStar) {
    std::string src =
        "x = 4\n"
        "x *= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, CompoundAssignSlash) {
    std::string src =
        "x = 10.0\n"
        "x /= 4.0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2.5\n");
}

TEST_F(CodeGenTest, CompoundAssignPercent) {
    std::string src =
        "x = 10\n"
        "x %= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, CompoundAssignInLoop) {
    std::string src =
        "sum = 0\n"
        "for i in range(1, 6):\n"
        "    sum += i\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "15\n");
}

// ===== Phase 1: 基本文字列操作テスト =====

TEST_F(CodeGenTest, ToIntBasic) {
    EXPECT_EQ(runSource("print(to_int(\"42\"))"), "42\n");
}

TEST_F(CodeGenTest, ToIntNegative) {
    EXPECT_EQ(runSource("print(to_int(\"-7\"))"), "-7\n");
}

TEST_F(CodeGenTest, ToIntZero) {
    EXPECT_EQ(runSource("print(to_int(\"0\"))"), "0\n");
}

TEST_F(CodeGenTest, ToIntUFCS) {
    EXPECT_EQ(runSource("s = \"123\"\nprint(s.to_int())"), "123\n");
}

TEST_F(CodeGenTest, ToFloatBasic) {
    EXPECT_EQ(runSource("print(to_float(\"3.14\"))"), "3.14\n");
}

TEST_F(CodeGenTest, ToFloatInteger) {
    EXPECT_EQ(runSource("print(to_float(\"42\"))"), "42\n");
}

TEST_F(CodeGenTest, ToFloatUFCS) {
    EXPECT_EQ(runSource("s = \"2.5\"\nprint(s.to_float())"), "2.5\n");
}

TEST_F(CodeGenTest, ToStrInt) {
    EXPECT_EQ(runSource("print(to_str(42))"), "42\n");
}

TEST_F(CodeGenTest, ToStrNegativeInt) {
    EXPECT_EQ(runSource("print(to_str(-7))"), "-7\n");
}

TEST_F(CodeGenTest, ToStrFloat) {
    EXPECT_EQ(runSource("print(to_str(3.14))"), "3.14\n");
}

TEST_F(CodeGenTest, ToStrBoolTrue) {
    EXPECT_EQ(runSource("print(to_str(true))"), "true\n");
}

TEST_F(CodeGenTest, ToStrBoolFalse) {
    EXPECT_EQ(runSource("print(to_str(false))"), "false\n");
}

TEST_F(CodeGenTest, ToStrStr) {
    EXPECT_EQ(runSource("print(to_str(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, ToStrUFCS) {
    EXPECT_EQ(runSource("x = 99\nprint(x.to_str())"), "99\n");
}

TEST_F(CodeGenTest, FindBasic) {
    EXPECT_EQ(runSource("print(find(\"hello world\", \"world\"))"), "Some(6)\n");
}

TEST_F(CodeGenTest, FindNotFound) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"xyz\"))"), "None\n");
}

TEST_F(CodeGenTest, FindAtStart) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"hel\"))"), "Some(0)\n");
}

TEST_F(CodeGenTest, FindEmpty) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"\"))"), "Some(0)\n");
}

TEST_F(CodeGenTest, FindUFCS) {
    EXPECT_EQ(runSource("s = \"abcdef\"\nprint(s.find(\"cd\"))"), "Some(2)\n");
}

TEST_F(CodeGenTest, SubstringBasic) {
    EXPECT_EQ(runSource("print(substring(\"hello world\", 0, 5))"), "hello\n");
}

TEST_F(CodeGenTest, SubstringMiddle) {
    EXPECT_EQ(runSource("print(substring(\"hello world\", 6, 11))"), "world\n");
}

TEST_F(CodeGenTest, SubstringEmpty) {
    EXPECT_EQ(runSource("print(substring(\"hello\", 2, 2))"), "\n");
}

TEST_F(CodeGenTest, SubstringUFCS) {
    EXPECT_EQ(runSource("s = \"abcdef\"\nprint(s.substring(1, 4))"), "bcd\n");
}

TEST_F(CodeGenTest, CharAtBasic) {
    EXPECT_EQ(runSource("print(char_at(\"hello\", 0))"), "h\n");
}

TEST_F(CodeGenTest, CharAtMiddle) {
    EXPECT_EQ(runSource("print(char_at(\"hello\", 4))"), "o\n");
}

TEST_F(CodeGenTest, CharAtUFCS) {
    EXPECT_EQ(runSource("s = \"abc\"\nprint(s.char_at(1))"), "b\n");
}

TEST_F(CodeGenTest, ReplaceBasic) {
    EXPECT_EQ(runSource("print(replace(\"hello world\", \"world\", \"ry\"))"), "hello ry\n");
}

TEST_F(CodeGenTest, ReplaceMultiple) {
    EXPECT_EQ(runSource("print(replace(\"aaa\", \"a\", \"bb\"))"), "bbbbbb\n");
}

TEST_F(CodeGenTest, ReplaceNotFound) {
    EXPECT_EQ(runSource("print(replace(\"hello\", \"xyz\", \"abc\"))"), "hello\n");
}

TEST_F(CodeGenTest, ReplaceEmpty) {
    EXPECT_EQ(runSource("print(replace(\"hello\", \"l\", \"\"))"), "heo\n");
}

TEST_F(CodeGenTest, ReplaceUFCS) {
    EXPECT_EQ(runSource("s = \"foo bar foo\"\nprint(s.replace(\"foo\", \"baz\"))"), "baz bar baz\n");
}

// ===== Phase 2: テキスト変換テスト =====

TEST_F(CodeGenTest, ToUpperBasic) {
    EXPECT_EQ(runSource("print(to_upper(\"hello\"))"), "HELLO\n");
}

TEST_F(CodeGenTest, ToUpperMixed) {
    EXPECT_EQ(runSource("print(to_upper(\"Hello World\"))"), "HELLO WORLD\n");
}

TEST_F(CodeGenTest, ToUpperAlreadyUpper) {
    EXPECT_EQ(runSource("print(to_upper(\"ABC\"))"), "ABC\n");
}

TEST_F(CodeGenTest, ToUpperEmpty) {
    EXPECT_EQ(runSource("print(to_upper(\"\"))"), "\n");
}

TEST_F(CodeGenTest, ToUpperUFCS) {
    EXPECT_EQ(runSource("s = \"hi\"\nprint(s.to_upper())"), "HI\n");
}

TEST_F(CodeGenTest, ToLowerBasic) {
    EXPECT_EQ(runSource("print(to_lower(\"HELLO\"))"), "hello\n");
}

TEST_F(CodeGenTest, ToLowerMixed) {
    EXPECT_EQ(runSource("print(to_lower(\"Hello World\"))"), "hello world\n");
}

TEST_F(CodeGenTest, ToLowerAlreadyLower) {
    EXPECT_EQ(runSource("print(to_lower(\"abc\"))"), "abc\n");
}

TEST_F(CodeGenTest, ToLowerEmpty) {
    EXPECT_EQ(runSource("print(to_lower(\"\"))"), "\n");
}

TEST_F(CodeGenTest, ToLowerUFCS) {
    EXPECT_EQ(runSource("s = \"HI\"\nprint(s.to_lower())"), "hi\n");
}

TEST_F(CodeGenTest, TrimBasic) {
    EXPECT_EQ(runSource("print(trim(\"  hello  \"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimTabs) {
    EXPECT_EQ(runSource("print(trim(\"\\thello\\t\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimNoWhitespace) {
    EXPECT_EQ(runSource("print(trim(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimAllWhitespace) {
    EXPECT_EQ(runSource("print(trim(\"   \"))"), "\n");
}

TEST_F(CodeGenTest, TrimEmpty) {
    EXPECT_EQ(runSource("print(trim(\"\"))"), "\n");
}

TEST_F(CodeGenTest, TrimUFCS) {
    EXPECT_EQ(runSource("s = \"  hi  \"\nprint(s.trim())"), "hi\n");
}

TEST_F(CodeGenTest, TrimStartBasic) {
    EXPECT_EQ(runSource("print(trim_start(\"  hello  \"))"), "hello  \n");
}

TEST_F(CodeGenTest, TrimStartNoWhitespace) {
    EXPECT_EQ(runSource("print(trim_start(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimStartUFCS) {
    EXPECT_EQ(runSource("s = \"  hi\"\nprint(s.trim_start())"), "hi\n");
}

TEST_F(CodeGenTest, TrimEndBasic) {
    EXPECT_EQ(runSource("print(trim_end(\"  hello  \"))"), "  hello\n");
}

TEST_F(CodeGenTest, TrimEndNoWhitespace) {
    EXPECT_EQ(runSource("print(trim_end(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimEndUFCS) {
    EXPECT_EQ(runSource("s = \"hi  \"\nprint(s.trim_end())"), "hi\n");
}

TEST_F(CodeGenTest, RepeatBasic) {
    EXPECT_EQ(runSource("print(repeat(\"ab\", 3))"), "ababab\n");
}

TEST_F(CodeGenTest, RepeatOne) {
    EXPECT_EQ(runSource("print(repeat(\"hello\", 1))"), "hello\n");
}

TEST_F(CodeGenTest, RepeatZero) {
    EXPECT_EQ(runSource("print(repeat(\"hello\", 0))"), "\n");
}

TEST_F(CodeGenTest, RepeatUFCS) {
    EXPECT_EQ(runSource("s = \"ha\"\nprint(s.repeat(3))"), "hahaha\n");
}

TEST_F(CodeGenTest, ReverseBasic) {
    EXPECT_EQ(runSource("print(reverse(\"hello\"))"), "olleh\n");
}

TEST_F(CodeGenTest, ReverseEmpty) {
    EXPECT_EQ(runSource("print(reverse(\"\"))"), "\n");
}

TEST_F(CodeGenTest, ReverseSingleChar) {
    EXPECT_EQ(runSource("print(reverse(\"a\"))"), "a\n");
}

TEST_F(CodeGenTest, ReversePalindrome) {
    EXPECT_EQ(runSource("print(reverse(\"abcba\"))"), "abcba\n");
}

TEST_F(CodeGenTest, ReverseUFCS) {
    EXPECT_EQ(runSource("s = \"abc\"\nprint(s.reverse())"), "cba\n");
}

// ===== Phase 3: List<str> 連携テスト =====

TEST_F(CodeGenTest, SplitBasic) {
    std::string src =
        "parts = split(\"a,b,c\", \",\")\n"
        "print(parts[0])\n"
        "print(parts[1])\n"
        "print(parts[2])";
    EXPECT_EQ(runSource(src), "a\nb\nc\n");
}

TEST_F(CodeGenTest, SplitNoDelimiter) {
    std::string src =
        "parts = split(\"hello\", \",\")\n"
        "print(length(parts))\n"
        "print(parts[0])";
    EXPECT_EQ(runSource(src), "1\nhello\n");
}

TEST_F(CodeGenTest, SplitMultiCharDelim) {
    std::string src =
        "parts = split(\"a::b::c\", \"::\")\n"
        "print(parts[0])\n"
        "print(parts[1])\n"
        "print(parts[2])";
    EXPECT_EQ(runSource(src), "a\nb\nc\n");
}

TEST_F(CodeGenTest, SplitLen) {
    EXPECT_EQ(runSource("print(length(split(\"a,b,c,d\", \",\")))"), "4\n");
}

TEST_F(CodeGenTest, SplitUFCS) {
    std::string src =
        "s = \"hello world\"\n"
        "parts = s.split(\" \")\n"
        "print(parts[0])\n"
        "print(parts[1])";
    EXPECT_EQ(runSource(src), "hello\nworld\n");
}

TEST_F(CodeGenTest, SplitForLoop) {
    std::string src =
        "for part in split(\"x,y,z\", \",\"):\n"
        "    print(part)";
    EXPECT_EQ(runSource(src), "x\ny\nz\n");
}

TEST_F(CodeGenTest, JoinBasic) {
    std::string src =
        "parts = [\"a\", \"b\", \"c\"]\n"
        "print(join(parts, \",\"))";
    EXPECT_EQ(runSource(src), "a,b,c\n");
}

TEST_F(CodeGenTest, JoinSingleElement) {
    std::string src =
        "parts = [\"hello\"]\n"
        "print(join(parts, \",\"))";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, JoinWithSpace) {
    std::string src =
        "words = [\"hello\", \"world\"]\n"
        "print(join(words, \" \"))";
    EXPECT_EQ(runSource(src), "hello world\n");
}

TEST_F(CodeGenTest, JoinUFCS) {
    std::string src =
        "parts = [\"x\", \"y\", \"z\"]\n"
        "print(parts.join(\"-\"))";
    EXPECT_EQ(runSource(src), "x-y-z\n");
}

TEST_F(CodeGenTest, SplitJoinRoundTrip) {
    std::string src =
        "s = \"hello,world,ry\"\n"
        "print(join(split(s, \",\"), \",\"))";
    EXPECT_EQ(runSource(src), "hello,world,ry\n");
}

// ===== filter テスト =====

TEST_F(CodeGenTest, FilterIntBasic) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = filter(xs, fn(x: int): x > 3)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[4, 5]\n");
}

TEST_F(CodeGenTest, FilterIntEmpty) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = filter(xs, fn(x: int): x > 10)\n"
        "print(length(ys))";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, FilterAllMatch) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = filter(xs, fn(x: int): x > 0)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, FilterFloat) {
    std::string src =
        "xs = [1.5, 2.5, 0.5]\n"
        "ys = filter(xs, fn(x: float): x > 1.0)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
}

TEST_F(CodeGenTest, FilterString) {
    std::string src =
        "xs = [\"hello\", \"hi\", \"hey\"]\n"
        "ys = filter(xs, fn(s: str): s.starts_with(\"he\"))\n"
        "print(length(ys))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, FilterWithClosure) {
    std::string src =
        "threshold = 3\n"
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = filter(xs, fn(x: int): x > threshold)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[4, 5]\n");
}

TEST_F(CodeGenTest, FilterUFCS) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = xs.filter(fn(x: int): x > 3)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[4, 5]\n");
}

TEST_F(CodeGenTest, FilterUntypedLambdaParam) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = filter(xs, fn(x): x > 3)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[4, 5]\n");
}

TEST_F(CodeGenTest, FilterStillRejectsTypedMismatch) {
    EXPECT_THROW(runSource(
        "xs = [1, 2, 3]\n"
        "ys = filter(xs, fn(x: str): true)\n"
        "print(ys)"
    ), std::runtime_error);
}

// ===== map テスト =====

TEST_F(CodeGenTest, MapIntToInt) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = map(xs, fn(x: int): x * 2)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[2, 4, 6]\n");
}

TEST_F(CodeGenTest, MapIntToFloat) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = map(xs, fn(x: int): x * 1.5)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1.5, 3, 4.5]\n");
}

TEST_F(CodeGenTest, MapStrToInt) {
    std::string src =
        "xs = [\"hello\", \"hi\"]\n"
        "ys = map(xs, fn(s: str): length(s))\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[5, 2]\n");
}

TEST_F(CodeGenTest, MapFloat) {
    std::string src =
        "xs = [1.0, 2.0]\n"
        "ys = map(xs, fn(x: float): x + 0.5)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
}

TEST_F(CodeGenTest, MapWithClosure) {
    std::string src =
        "factor = 10\n"
        "xs = [1, 2, 3]\n"
        "ys = map(xs, fn(x: int): x * factor)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[10, 20, 30]\n");
}

TEST_F(CodeGenTest, MapPreservesOriginal) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = map(xs, fn(x: int): x * 2)\n"
        "print(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n[2, 4, 6]\n");
}

TEST_F(CodeGenTest, MapUntypedLambdaParam) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = map(xs, fn(x) -> int: 7)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[7, 7, 7]\n");
}

// ===== filter + map チェーンテスト =====

TEST_F(CodeGenTest, FilterThenMap) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = xs.filter(fn(x: int): x > 2).map(fn(x: int): x * 10)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[30, 40, 50]\n");
}

TEST_F(CodeGenTest, MapThenFilter) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = xs.map(fn(x: int): x * 2).filter(fn(x: int): x > 4)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[6]\n");
}

TEST_F(CodeGenTest, FilterThenFilter) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "ys = xs.filter(fn(x: int): x > 1).filter(fn(x: int): x < 5)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[2, 3, 4]\n");
}

// ===== sort テスト =====

TEST_F(CodeGenTest, SortIntAsc) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = sort(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, SortFloatAsc) {
    std::string src =
        "xs = [3.0, 1.0, 2.0]\n"
        "ys = sort(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, SortStrAsc) {
    std::string src =
        "xs = [\"banana\", \"apple\", \"cherry\"]\n"
        "ys = sort(xs)\n"
        "print(ys[0])\n"
        "print(ys[1])\n"
        "print(ys[2])";
    EXPECT_EQ(runSource(src), "apple\nbanana\ncherry\n");
}

TEST_F(CodeGenTest, SortSingleElement) {
    std::string src =
        "xs = [42]\n"
        "ys = sort(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[42]\n");
}

TEST_F(CodeGenTest, SortWithComparator) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = sort(xs, fn(a: int, b: int): a > b)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[3, 2, 1]\n");
}

TEST_F(CodeGenTest, SortPreservesOriginal) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = sort(xs)\n"
        "print(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[3, 1, 2]\n[1, 2, 3]\n");
}

TEST_F(CodeGenTest, SortUFCS) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = xs.sort()\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, SortWithComparatorUFCS) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = xs.sort(fn(a: int, b: int): a > b)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[3, 2, 1]\n");
}

// ===== filter + map + sort 全チェーンテスト =====

TEST_F(CodeGenTest, FilterMapSort) {
    std::string src =
        "xs = [5, 3, 1, 4, 2]\n"
        "ys = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[20, 30, 40, 50]\n");
}

TEST_F(CodeGenTest, SortThenFilter) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = xs.sort().filter(fn(x: int): x > 1)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[2, 3]\n");
}

TEST_F(CodeGenTest, SortThenMap) {
    std::string src =
        "xs = [3, 1, 2]\n"
        "ys = xs.sort().map(fn(x: int): x * 2)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[2, 4, 6]\n");
}

TEST_F(CodeGenTest, SortAlreadySorted) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5]\n");
}

TEST_F(CodeGenTest, SortReverseSorted) {
    std::string src =
        "xs = [5, 4, 3, 2, 1]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5]\n");
}

TEST_F(CodeGenTest, SortDuplicates) {
    std::string src =
        "xs = [3, 1, 2, 1, 3]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 1, 2, 3, 3]\n");
}

TEST_F(CodeGenTest, SortEmpty) {
    std::string src =
        "xs: List<int> = [1]\n"
        "ys = xs.filter(fn(x: int): x > 10)\n"
        "print(sort(ys))";
    EXPECT_EQ(runSource(src), "[]\n");
}

TEST_F(CodeGenTest, SortLargerList) {
    std::string src =
        "xs = [15, 3, 9, 1, 14, 7, 20, 5, 18, 2, 12, 8, 16, 4, 11, 6, 19, 10, 17, 13]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]\n");
}

// ===== 複合代入演算子 (追加分) =====

TEST_F(CodeGenTest, CompoundAssignFloorDiv) {
    std::string src =
        "x = 7\n"
        "x //= 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, CompoundAssignPower) {
    std::string src =
        "x = 2.0\n"
        "x **= 3.0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "8\n");
}

TEST_F(CodeGenTest, CompoundAssignBitAnd) {
    std::string src =
        "x = 12\n"
        "x &= 10\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "8\n");
}

TEST_F(CodeGenTest, CompoundAssignBitOr) {
    std::string src =
        "x = 12\n"
        "x |= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, CompoundAssignBitXor) {
    std::string src =
        "x = 12\n"
        "x ^= 10\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, CompoundAssignLeftShift) {
    std::string src =
        "x = 1\n"
        "x <<= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "8\n");
}

TEST_F(CodeGenTest, CompoundAssignRightShift) {
    std::string src =
        "x = 16\n"
        "x >>= 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "4\n");
}

// ===== in / not in: List・Map対応 =====

TEST_F(CodeGenTest, InListInt) {
    EXPECT_EQ(runSource("print(2 in [1, 2, 3])"), "true\n");
}

TEST_F(CodeGenTest, InListIntFalse) {
    EXPECT_EQ(runSource("print(4 in [1, 2, 3])"), "false\n");
}

TEST_F(CodeGenTest, InListStr) {
    EXPECT_EQ(runSource("print(\"a\" in [\"a\", \"b\"])"), "true\n");
}

TEST_F(CodeGenTest, NotInList) {
    EXPECT_EQ(runSource("print(4 not in [1, 2, 3])"), "true\n");
}

TEST_F(CodeGenTest, InMapKey) {
    EXPECT_EQ(runSource("print(\"a\" in {\"a\": 1})"), "true\n");
}

TEST_F(CodeGenTest, InMapKeyFalse) {
    EXPECT_EQ(runSource("print(\"c\" in {\"a\": 1})"), "false\n");
}

TEST_F(CodeGenTest, NotInMapKey) {
    EXPECT_EQ(runSource("print(\"b\" not in {\"a\": 1})"), "true\n");
}

// ===== List append / pop / reverse / slice =====

TEST_F(CodeGenTest, ListAppend) {
    std::string src =
        "xs = [1, 2]\n"
        "xs.append(3)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListAppendString) {
    std::string src =
        "xs = [\"a\"]\n"
        "xs.append(\"b\")\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[a, b]\n");
}

TEST_F(CodeGenTest, ListPop) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "v = xs.pop()\n"
        "print(v)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "Some(3)\n[1, 2]\n");
}

TEST_F(CodeGenTest, ListPopEmpty) {
    std::string src =
        "xs: List<int> = [1]\n"
        "ys = xs.filter(fn(x: int): x > 10)\n"
        "print(ys.pop())";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, ListReverse) {
    std::string src =
        "print(reverse([1, 2, 3]))";
    EXPECT_EQ(runSource(src), "[3, 2, 1]\n");
}

TEST_F(CodeGenTest, ListReverseOriginal) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = reverse(xs)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListReverseStr) {
    // str reverse still works
    EXPECT_EQ(runSource("print(reverse(\"hello\"))"), "olleh\n");
}

TEST_F(CodeGenTest, ListSlice) {
    std::string src =
        "print(slice([1, 2, 3, 4, 5], 1, 3))";
    EXPECT_EQ(runSource(src), "[2, 3]\n");
}

TEST_F(CodeGenTest, ListSliceClamp) {
    std::string src =
        "print(slice([1, 2, 3], 0, 100))";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListSliceEmpty) {
    std::string src =
        "print(slice([1, 2, 3], 2, 2))";
    EXPECT_EQ(runSource(src), "[]\n");
}

// ===== range() ステップ対応 =====

TEST_F(CodeGenTest, RangeStep) {
    std::string src =
        "for i in range(0, 10, 2):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "0\n2\n4\n6\n8\n");
}

TEST_F(CodeGenTest, RangeStepNegative) {
    std::string src =
        "for i in range(10, 0, -1):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n");
}

TEST_F(CodeGenTest, RangeStepNegative3) {
    std::string src =
        "for i in range(10, 0, -3):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "10\n7\n4\n1\n");
}

TEST_F(CodeGenTest, RangeStepEmpty) {
    std::string src =
        "for i in range(0, 10, -1):\n"
        "    print(i)";
    EXPECT_EQ(runSource(src), "");
}

TEST_F(CodeGenTest, RangeStepList) {
    std::string src =
        "xs = range(0, 10, 3)\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "4\n");
}

// ===== reduce / fold =====

TEST_F(CodeGenTest, ReduceBasic) {
    std::string src =
        "xs = [1, 2, 3, 4]\n"
        "sum = reduce(xs, fn(a: int, b: int): a + b)\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, ReduceUFCS) {
    std::string src =
        "xs = [1, 2, 3, 4]\n"
        "print(xs.reduce(fn(a: int, b: int): a + b))";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, ReduceUntypedLambdaParams) {
    std::string src =
        "xs = [1, 2, 3, 4]\n"
        "sum = reduce(xs, fn(a, b) -> int: 99)\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, FoldBasic) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "sum = fold(xs, 10, fn(a: int, b: int): a + b)\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "16\n");
}

TEST_F(CodeGenTest, FoldUFCS) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs.fold(0, fn(a: int, b: int): a + b))";
    EXPECT_EQ(runSource(src), "6\n");
}

// ===== keys / values / items =====

TEST_F(CodeGenTest, MapKeys) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "ks = keys(m)\n"
        "print(length(ks))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapValues) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "vs = values(m)\n"
        "print(length(vs))";
    EXPECT_EQ(runSource(src), "2\n");
}

// ===== any / all =====

TEST_F(CodeGenTest, AnyTrue) {
    std::string src =
        "xs = [1, 2, 3, 4]\n"
        "print(any(xs, fn(x: int): x > 3))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, AnyFalse) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(any(xs, fn(x: int): x > 10))";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, AllTrue) {
    std::string src =
        "xs = [2, 4, 6]\n"
        "print(all(xs, fn(x: int): x > 0))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, AllFalse) {
    std::string src =
        "xs = [2, 4, 6]\n"
        "print(all(xs, fn(x: int): x > 3))";
    EXPECT_EQ(runSource(src), "false\n");
}

// ===== sum / min / max =====

TEST_F(CodeGenTest, SumInt) {
    std::string src =
        "xs = [1, 2, 3, 4, 5]\n"
        "print(sum(xs))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, MinInt) {
    std::string src =
        "xs = [3, 1, 4, 1, 5]\n"
        "print(min(xs))";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, MaxInt) {
    std::string src =
        "xs = [3, 1, 4, 1, 5]\n"
        "print(max(xs))";
    EXPECT_EQ(runSource(src), "5\n");
}

// ===== enumerate / zip =====

TEST_F(CodeGenTest, EnumerateBasic) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "es = enumerate(xs)\n"
        "print(length(es))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ZipBasic) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = [\"a\", \"b\", \"c\"]\n"
        "zs = zip(xs, ys)\n"
        "print(length(zs))";
    EXPECT_EQ(runSource(src), "3\n");
}

// ===== first / last / is_empty =====

TEST_F(CodeGenTest, FirstBasic) {
    EXPECT_EQ(runSource("print(first([10, 20, 30]))"), "Some(10)\n");
}

TEST_F(CodeGenTest, LastBasic) {
    EXPECT_EQ(runSource("print(last([10, 20, 30]))"), "Some(30)\n");
}

TEST_F(CodeGenTest, IsEmptyFalse) {
    EXPECT_EQ(runSource("print(is_empty([1, 2]))"), "false\n");
}

TEST_F(CodeGenTest, IsEmptyTrue) {
    std::string src =
        "xs: List<int> = [1]\n"
        "ys = xs.filter(fn(x: int): x > 10)\n"
        "print(is_empty(ys))";
    EXPECT_EQ(runSource(src), "true\n");
}

// ===== insert(list, i, val) =====

TEST_F(CodeGenTest, ListInsert) {
    std::string src =
        "xs = [1, 3, 4]\n"
        "insert(xs, 1, 2)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "1\n2\n3\n4\n");
}

// ===== remove_at(list, i) =====

TEST_F(CodeGenTest, ListRemoveAt) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "removed = remove_at(xs, 1)\n"
        "print(removed)\n"
        "print(length(xs))\n"
        "print(xs[0])\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "20\n2\n10\n30\n");
}

// ===== items(map) =====

TEST_F(CodeGenTest, MapItems) {
    std::string src =
        "m = {\"a\": 1}\n"
        "pairs = items(m)\n"
        "print(length(pairs))";
    EXPECT_EQ(runSource(src), "1\n");
}

// ===== get(map, key, default) =====

TEST_F(CodeGenTest, MapGetWithDefault) {
    std::string src =
        "m = {\"a\": 10}\n"
        "print(get(m, \"a\", 0))\n"
        "print(get(m, \"b\", 99))";
    EXPECT_EQ(runSource(src), "10\n99\n");
}

// ===== remove(map, key) =====

TEST_F(CodeGenTest, MapRemove) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "remove(m, \"a\")\n"
        "print(length(m))";
    EXPECT_EQ(runSource(src), "1\n");
}

// ===== Set operations =====

TEST_F(CodeGenTest, SetUnion) {
    std::string src =
        "a: Set<int> = {1, 2, 3}\n"
        "b: Set<int> = {3, 4, 5}\n"
        "c = union(a, b)\n"
        "print(length(c))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, SetIntersection) {
    std::string src =
        "a: Set<int> = {1, 2, 3}\n"
        "b: Set<int> = {2, 3, 4}\n"
        "c = intersection(a, b)\n"
        "print(length(c))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, SetDifference) {
    std::string src =
        "a: Set<int> = {1, 2, 3}\n"
        "b: Set<int> = {2, 3, 4}\n"
        "c = difference(a, b)\n"
        "print(length(c))";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, SetSymmetricDifference) {
    std::string src =
        "a: Set<int> = {1, 2, 3}\n"
        "b: Set<int> = {2, 3, 4}\n"
        "c = symmetric_difference(a, b)\n"
        "print(length(c))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, SetIsSubset) {
    std::string src =
        "a: Set<int> = {1, 2}\n"
        "b: Set<int> = {1, 2, 3}\n"
        "print(is_subset(a, b))\n"
        "print(is_subset(b, a))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, SetIsSuperset) {
    std::string src =
        "a: Set<int> = {1, 2, 3}\n"
        "b: Set<int> = {1, 2}\n"
        "print(is_superset(a, b))\n"
        "print(is_superset(b, a))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

// ===== remove(list, val) テスト =====

TEST_F(CodeGenTest, ListRemoveInt) {
    std::string src =
        "xs = [1, 2, 3, 4]\n"
        "remove(xs, 3)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 4]\n");
}

TEST_F(CodeGenTest, ListRemoveStr) {
    std::string src =
        "xs = [\"a\", \"b\", \"c\"]\n"
        "remove(xs, \"b\")\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[a, c]\n");
}

TEST_F(CodeGenTest, ListRemoveFloat) {
    std::string src =
        "xs = [1.0, 2.5, 3.0]\n"
        "remove(xs, 2.5)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 3]\n");
}

TEST_F(CodeGenTest, ListRemoveNotFound) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "remove(xs, 99)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, ListRemoveFirst) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "remove(xs, 10)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[20, 30]\n");
}

TEST_F(CodeGenTest, ListRemoveLast) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "remove(xs, 30)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[10, 20]\n");
}

TEST_F(CodeGenTest, ListRemoveDuplicate) {
    std::string src =
        "xs = [1, 2, 3, 2, 4]\n"
        "remove(xs, 2)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[1, 3, 2, 4]\n");
}

TEST_F(CodeGenTest, ListRemoveSingleElement) {
    std::string src =
        "xs = [42]\n"
        "remove(xs, 42)\n"
        "print(xs)";
    EXPECT_EQ(runSource(src), "[]\n");
}

// ===== distinct(list) テスト =====

TEST_F(CodeGenTest, DistinctInt) {
    std::string src =
        "xs = [1, 2, 3, 2, 1, 4]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4]\n");
}

TEST_F(CodeGenTest, DistinctStr) {
    std::string src =
        "xs = [\"a\", \"b\", \"a\", \"c\", \"b\"]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[a, b, c]\n");
}

TEST_F(CodeGenTest, DistinctFloat) {
    std::string src =
        "xs = [1.0, 2.0, 1.0, 3.0]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, DistinctAlreadyUnique) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
}

TEST_F(CodeGenTest, DistinctAllSame) {
    std::string src =
        "xs = [5, 5, 5, 5]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[5]\n");
}

TEST_F(CodeGenTest, DistinctPreservesOrder) {
    std::string src =
        "xs = [3, 1, 2, 1, 3]\n"
        "print(distinct(xs))";
    EXPECT_EQ(runSource(src), "[3, 1, 2]\n");
}

TEST_F(CodeGenTest, DistinctNonDestructive) {
    std::string src =
        "xs = [1, 2, 1, 3]\n"
        "ys = distinct(xs)\n"
        "print(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 1, 3]\n[1, 2, 3]\n");
}

// ===== merge(map1, map2) テスト =====

TEST_F(CodeGenTest, MergeNoOverlap) {
    std::string src =
        "m1 = {\"a\": 1, \"b\": 2}\n"
        "m2 = {\"c\": 3, \"d\": 4}\n"
        "m3 = merge(m1, m2)\n"
        "print(length(m3))\n"
        "print(m3[\"a\"])\n"
        "print(m3[\"b\"])\n"
        "print(m3[\"c\"])\n"
        "print(m3[\"d\"])";
    EXPECT_EQ(runSource(src), "4\n1\n2\n3\n4\n");
}

TEST_F(CodeGenTest, MergeOverlap) {
    std::string src =
        "m1 = {\"a\": 1, \"b\": 2}\n"
        "m2 = {\"b\": 99, \"c\": 3}\n"
        "m3 = merge(m1, m2)\n"
        "print(length(m3))\n"
        "print(m3[\"a\"])\n"
        "print(m3[\"b\"])\n"
        "print(m3[\"c\"])";
    EXPECT_EQ(runSource(src), "3\n1\n99\n3\n");
}

TEST_F(CodeGenTest, MergeIntKeys) {
    std::string src =
        "m1 = {1: \"a\", 2: \"b\"}\n"
        "m2 = {2: \"x\", 3: \"c\"}\n"
        "m3 = merge(m1, m2)\n"
        "print(length(m3))\n"
        "print(m3[1])\n"
        "print(m3[2])\n"
        "print(m3[3])";
    EXPECT_EQ(runSource(src), "3\na\nx\nc\n");
}

TEST_F(CodeGenTest, MergeNonDestructive) {
    std::string src =
        "m1 = {\"a\": 1}\n"
        "m2 = {\"b\": 2}\n"
        "m3 = merge(m1, m2)\n"
        "print(length(m1))\n"
        "print(length(m2))\n"
        "print(length(m3))";
    EXPECT_EQ(runSource(src), "1\n1\n2\n");
}

// ===== flatten(list) テスト =====

TEST_F(CodeGenTest, FlattenIntLists) {
    std::string src =
        "xs = [[1, 2], [3, 4]]\n"
        "print(flatten(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4]\n");
}

TEST_F(CodeGenTest, FlattenStrLists) {
    std::string src =
        "xs = [[\"a\", \"b\"], [\"c\", \"d\"]]\n"
        "print(flatten(xs))";
    EXPECT_EQ(runSource(src), "[a, b, c, d]\n");
}

TEST_F(CodeGenTest, FlattenUnevenInner) {
    std::string src =
        "xs = [[1], [2, 3, 4], [5, 6]]\n"
        "ys = flatten(xs)\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6]\n");
}

TEST_F(CodeGenTest, FlattenSingleInner) {
    std::string src =
        "xs = [[10, 20, 30]]\n"
        "print(flatten(xs))";
    EXPECT_EQ(runSource(src), "[10, 20, 30]\n");
}

TEST_F(CodeGenTest, FlattenNonDestructive) {
    std::string src =
        "xs = [[1, 2], [3, 4]]\n"
        "ys = flatten(xs)\n"
        "print(length(xs))\n"
        "print(ys)";
    EXPECT_EQ(runSource(src), "2\n[1, 2, 3, 4]\n");
}

// ===== for tuple destructuring =====

TEST_F(CodeGenTest, ForEnumerateDestructure) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "total = 0\n"
        "for i, x in enumerate(xs):\n"
        "    total = total + i + x\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "63\n");
}

TEST_F(CodeGenTest, ForZipDestructure) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = [10, 20, 30]\n"
        "total = 0\n"
        "for a, b in zip(xs, ys):\n"
        "    total = total + a + b\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "66\n");
}

TEST_F(CodeGenTest, ForEnumerateWildcardFirst) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "total = 0\n"
        "for _, x in enumerate(xs):\n"
        "    total = total + x\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "60\n");
}

TEST_F(CodeGenTest, ForEnumerateWildcardSecond) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "total = 0\n"
        "for i, _ in enumerate(xs):\n"
        "    total = total + i\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ForEnumerateWithPrint) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "for i, x in enumerate(xs):\n"
        "    print(f\"{i}: {x}\")";
    EXPECT_EQ(runSource(src), "0: 10\n1: 20\n2: 30\n");
}

TEST_F(CodeGenTest, ForTwoVarNonTupleError) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "for a, b in xs:\n"
        "    print(a)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== TimSort / Hybrid Sort テスト =====

TEST_F(CodeGenTest, SortExactly2Elements) {
    std::string src =
        "xs = [2, 1]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2]\n");
}

TEST_F(CodeGenTest, SortExactly32Elements) {
    // MIN_MERGE threshold: 32 elements
    std::string src =
        "xs = [32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, "
        "16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, "
              "17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]\n");
}

TEST_F(CodeGenTest, SortExactly33Elements) {
    // minRun+1: enters TimSort merge path
    std::string src =
        "xs = [33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, "
        "16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]\n"
        "print(sort(xs))";
    EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, "
              "17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33]\n");
}

TEST_F(CodeGenTest, SortLargeListWithDuplicates) {
    // 50 elements with many duplicates
    std::string src =
        "xs = [5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
        "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
        "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
        "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
        "5, 3, 8, 1, 9, 2, 7, 4, 6, 10]\n"
        "ys = sort(xs)\n"
        "print(length(ys))\n"
        "print(ys[0])\n"
        "print(ys[49])";
    EXPECT_EQ(runSource(src), "50\n1\n10\n");
}

TEST_F(CodeGenTest, SortStability) {
    // Stability test: sort by first digit only, equal elements should preserve order
    // Use pairs represented as values: encode (key, order) as key*100+order
    // Sort by key (value / 100), check order preservation for equal keys
    std::string src =
        "xs = [201, 102, 303, 104, 205]\n"
        "ys = sort(xs, fn(a: int, b: int): a / 100 < b / 100)\n"
        "print(ys)";
    // key=1: 102, 104 (original order preserved)
    // key=2: 201, 205 (original order preserved)
    // key=3: 303
    EXPECT_EQ(runSource(src), "[102, 104, 201, 205, 303]\n");
}

TEST_F(CodeGenTest, SortStabilityLarger) {
    // Larger stability test crossing TimSort merge boundary
    std::string src =
        "xs = range(40).map(fn(i: int): (i % 5) * 1000 + i)\n"
        "ys = sort(xs, fn(a: int, b: int): a / 1000 < b / 1000)\n"
        "stable = true\n"
        "for i in range(1, 40):\n"
        "    ka = ys[i - 1] / 1000\n"
        "    kb = ys[i] / 1000\n"
        "    if ka == kb:\n"
        "        if ys[i - 1] % 1000 > ys[i] % 1000:\n"
        "            stable = false\n"
        "print(stable)";
    EXPECT_EQ(runSource(src), "true\n");
}
