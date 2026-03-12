#include "test_codegen_common.hpp"

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
        "var sum = 0\n"
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
        "var x = 10\n"
        "x += 5\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, CompoundAssignMinus) {
    std::string src =
        "var x = 10\n"
        "x -= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, CompoundAssignStar) {
    std::string src =
        "var x = 4\n"
        "x *= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, CompoundAssignSlash) {
    std::string src =
        "var x = 10.0\n"
        "x /= 4.0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2.5\n");
}

TEST_F(CodeGenTest, CompoundAssignPercent) {
    std::string src =
        "var x = 10\n"
        "x %= 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, CompoundAssignInLoop) {
    std::string src =
        "var sum = 0\n"
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
    EXPECT_EQ(runSource("let s = \"123\"\nprint(s.to_int())"), "123\n");
}

TEST_F(CodeGenTest, ToFloatBasic) {
    EXPECT_EQ(runSource("print(to_float(\"3.14\"))"), "3.14\n");
}

TEST_F(CodeGenTest, ToFloatInteger) {
    EXPECT_EQ(runSource("print(to_float(\"42\"))"), "42\n");
}

TEST_F(CodeGenTest, ToFloatUFCS) {
    EXPECT_EQ(runSource("let s = \"2.5\"\nprint(s.to_float())"), "2.5\n");
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
    EXPECT_EQ(runSource("let x = 99\nprint(x.to_str())"), "99\n");
}

TEST_F(CodeGenTest, FindBasic) {
    EXPECT_EQ(runSource("print(find(\"hello world\", \"world\"))"), "6\n");
}

TEST_F(CodeGenTest, FindNotFound) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"xyz\"))"), "-1\n");
}

TEST_F(CodeGenTest, FindAtStart) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"hel\"))"), "0\n");
}

TEST_F(CodeGenTest, FindEmpty) {
    EXPECT_EQ(runSource("print(find(\"hello\", \"\"))"), "0\n");
}

TEST_F(CodeGenTest, FindUFCS) {
    EXPECT_EQ(runSource("let s = \"abcdef\"\nprint(s.find(\"cd\"))"), "2\n");
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
    EXPECT_EQ(runSource("let s = \"abcdef\"\nprint(s.substring(1, 4))"), "bcd\n");
}

TEST_F(CodeGenTest, CharAtBasic) {
    EXPECT_EQ(runSource("print(char_at(\"hello\", 0))"), "h\n");
}

TEST_F(CodeGenTest, CharAtMiddle) {
    EXPECT_EQ(runSource("print(char_at(\"hello\", 4))"), "o\n");
}

TEST_F(CodeGenTest, CharAtUFCS) {
    EXPECT_EQ(runSource("let s = \"abc\"\nprint(s.char_at(1))"), "b\n");
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
    EXPECT_EQ(runSource("let s = \"foo bar foo\"\nprint(s.replace(\"foo\", \"baz\"))"), "baz bar baz\n");
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
    EXPECT_EQ(runSource("let s = \"hi\"\nprint(s.to_upper())"), "HI\n");
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
    EXPECT_EQ(runSource("let s = \"HI\"\nprint(s.to_lower())"), "hi\n");
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
    EXPECT_EQ(runSource("let s = \"  hi  \"\nprint(s.trim())"), "hi\n");
}

TEST_F(CodeGenTest, TrimStartBasic) {
    EXPECT_EQ(runSource("print(trim_start(\"  hello  \"))"), "hello  \n");
}

TEST_F(CodeGenTest, TrimStartNoWhitespace) {
    EXPECT_EQ(runSource("print(trim_start(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimStartUFCS) {
    EXPECT_EQ(runSource("let s = \"  hi\"\nprint(s.trim_start())"), "hi\n");
}

TEST_F(CodeGenTest, TrimEndBasic) {
    EXPECT_EQ(runSource("print(trim_end(\"  hello  \"))"), "  hello\n");
}

TEST_F(CodeGenTest, TrimEndNoWhitespace) {
    EXPECT_EQ(runSource("print(trim_end(\"hello\"))"), "hello\n");
}

TEST_F(CodeGenTest, TrimEndUFCS) {
    EXPECT_EQ(runSource("let s = \"hi  \"\nprint(s.trim_end())"), "hi\n");
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
    EXPECT_EQ(runSource("let s = \"ha\"\nprint(s.repeat(3))"), "hahaha\n");
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
    EXPECT_EQ(runSource("let s = \"abc\"\nprint(s.reverse())"), "cba\n");
}

// ===== Phase 3: list[str] 連携テスト =====

TEST_F(CodeGenTest, SplitBasic) {
    std::string src =
        "let parts = split(\"a,b,c\", \",\")\n"
        "print(parts[0])\n"
        "print(parts[1])\n"
        "print(parts[2])";
    EXPECT_EQ(runSource(src), "a\nb\nc\n");
}

TEST_F(CodeGenTest, SplitNoDelimiter) {
    std::string src =
        "let parts = split(\"hello\", \",\")\n"
        "print(len(parts))\n"
        "print(parts[0])";
    EXPECT_EQ(runSource(src), "1\nhello\n");
}

TEST_F(CodeGenTest, SplitMultiCharDelim) {
    std::string src =
        "let parts = split(\"a::b::c\", \"::\")\n"
        "print(parts[0])\n"
        "print(parts[1])\n"
        "print(parts[2])";
    EXPECT_EQ(runSource(src), "a\nb\nc\n");
}

TEST_F(CodeGenTest, SplitLen) {
    EXPECT_EQ(runSource("print(len(split(\"a,b,c,d\", \",\")))"), "4\n");
}

TEST_F(CodeGenTest, SplitUFCS) {
    std::string src =
        "let s = \"hello world\"\n"
        "let parts = s.split(\" \")\n"
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
        "let parts = [\"a\", \"b\", \"c\"]\n"
        "print(join(parts, \",\"))";
    EXPECT_EQ(runSource(src), "a,b,c\n");
}

TEST_F(CodeGenTest, JoinSingleElement) {
    std::string src =
        "let parts = [\"hello\"]\n"
        "print(join(parts, \",\"))";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, JoinWithSpace) {
    std::string src =
        "let words = [\"hello\", \"world\"]\n"
        "print(join(words, \" \"))";
    EXPECT_EQ(runSource(src), "hello world\n");
}

TEST_F(CodeGenTest, JoinUFCS) {
    std::string src =
        "let parts = [\"x\", \"y\", \"z\"]\n"
        "print(parts.join(\"-\"))";
    EXPECT_EQ(runSource(src), "x-y-z\n");
}

TEST_F(CodeGenTest, SplitJoinRoundTrip) {
    std::string src =
        "let s = \"hello,world,ry\"\n"
        "print(join(split(s, \",\"), \",\"))";
    EXPECT_EQ(runSource(src), "hello,world,ry\n");
}
