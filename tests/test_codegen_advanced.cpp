#include "test_codegen_common.hpp"

// ===== Break / Continue =====

TEST_F(CodeGenTest, BreakInWhile) {
    std::string src =
        "var i = 0\n"
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
        "var i = 0\n"
        "var sum = 0\n"
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
        "var p = Point(1, 2)\n"
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
        "var p = Point(0, 0)\n"
        "p.x = 3\n"
        "p.y = 4\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "3\n4\n");
}

TEST_F(CodeGenTest, StructFieldAssignLetError) {
    std::string src =
        "type Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
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

// ===== Lambda (arrow function) tests =====

TEST_F(CodeGenTest, LambdaBasicExpr) {
    std::string src =
        "let double = (x: int): int => x * 2\n"
        "print(double(5))";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, LambdaNoParams) {
    std::string src =
        "let answer = (): int => 42\n"
        "print(answer())";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, LambdaMultipleParams) {
    std::string src =
        "let add = (a: int, b: int): int => a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, LambdaFloat) {
    std::string src =
        "let half = (x: float): float => x / 2.0\n"
        "print(half(7.0))";
    EXPECT_EQ(runSource(src), "3.5\n");
}

TEST_F(CodeGenTest, LambdaAsArgument) {
    std::string src =
        "fn apply(f: fn(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "let doubled = apply((n: int): int => n * 2, 10)\n"
        "print(doubled)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, LambdaStoredAndPassed) {
    std::string src =
        "fn apply(f: fn(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "let triple = (n: int): int => n * 3\n"
        "print(apply(triple, 5))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, FunctionReference) {
    std::string src =
        "fn square(x: int) -> int:\n"
        "    return x * x\n"
        "fn apply(f: fn(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "print(apply(square, 6))";
    EXPECT_EQ(runSource(src), "36\n");
}

TEST_F(CodeGenTest, LambdaBoolReturn) {
    std::string src =
        "let is_positive = (x: int): bool => x > 0\n"
        "print(is_positive(5))\n"
        "print(is_positive(-3))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, LambdaMultiLine) {
    std::string src =
        "let abs = (x: int): int =>\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-7))\n"
        "print(abs(3))";
    EXPECT_EQ(runSource(src), "7\n3\n");
}

TEST_F(CodeGenTest, LambdaClosure) {
    std::string src =
        "let offset = 10\n"
        "let add_offset = (x: int): int => x + offset\n"
        "print(add_offset(5))\n"
        "print(add_offset(20))";
    EXPECT_EQ(runSource(src), "15\n30\n");
}

TEST_F(CodeGenTest, LambdaArgCountError) {
    std::string src =
        "let f = (x: int): int => x\n"
        "f(1, 2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, LambdaArgTypeError) {
    std::string src =
        "let f = (x: int): int => x\n"
        "f(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Set テスト =====

TEST_F(CodeGenTest, SetCreateAndIn) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "print(2 in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, SetNotIn) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "print(5 in s)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, SetLen) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "print(len(s))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, SetAdd) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "s.add(4)\n"
        "print(4 in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, SetAddDuplicate) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "s.add(1)\n"
        "print(len(s))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, SetRemove) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "s.remove(2)\n"
        "print(2 in s)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, SetPrint) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "{1, 2, 3}\n");
}

TEST_F(CodeGenTest, SetStringElements) {
    std::string src =
        "let s = {\"a\", \"b\"}\n"
        "print(\"a\" in s)\n"
        "print(\"c\" in s)";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, SetEmptyWithAnnotation) {
    std::string src =
        "let s: Set<int> = {}\n"
        "print(len(s))\n"
        "s.add(42)\n"
        "print(42 in s)";
    EXPECT_EQ(runSource(src), "0\ntrue\n");
}

TEST_F(CodeGenTest, SetInFunction) {
    std::string src =
        "fn has_element(s: Set<int>, x: int) -> bool:\n"
        "    return x in s\n"
        "let s = {10, 20, 30}\n"
        "print(has_element(s, 20))\n"
        "print(has_element(s, 40))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, SetForIn) {
    std::string src =
        "let s = {10, 20, 30}\n"
        "var total = 0\n"
        "for x in s:\n"
        "    total += x\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "60\n");
}

// ===== Enum テスト =====

TEST_F(CodeGenTest, EnumBasic) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Red\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "Red\n");
}

TEST_F(CodeGenTest, EnumComparison) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Red\n"
        "print(c == Color::Red)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, EnumNotEqual) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Red\n"
        "print(c != Color::Green)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, EnumInIf) {
    std::string src =
        "enum Direction:\n"
        "    Up\n"
        "    Down\n"
        "    Left\n"
        "    Right\n"
        "let d = Direction::Up\n"
        "if d == Direction::Up:\n"
        "    print(1)\n"
        "else:\n"
        "    print(0)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, EnumAsParam) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "fn is_red(c: Color) -> bool:\n"
        "    return c == Color::Red\n"
        "print(is_red(Color::Red))\n"
        "print(is_red(Color::Blue))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, EnumMultiple) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "enum Size:\n"
        "    Small\n"
        "    Medium\n"
        "    Large\n"
        "let c = Color::Green\n"
        "let s = Size::Large\n"
        "print(c)\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "Green\nLarge\n");
}

// ===== Match statement tests =====

TEST_F(CodeGenTest, MatchEnumAllVariants) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Green\n"
        "match c:\n"
        "    case Color::Red:\n"
        "        print(\"red\")\n"
        "    case Color::Green:\n"
        "        print(\"green\")\n"
        "    case Color::Blue:\n"
        "        print(\"blue\")\n";
    EXPECT_EQ(runSource(src), "green\n");
}

TEST_F(CodeGenTest, MatchEnumWithWildcard) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Blue\n"
        "match c:\n"
        "    case Color::Red:\n"
        "        print(\"red\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "other\n");
}

TEST_F(CodeGenTest, MatchOptionSomeNone) {
    std::string src =
        "let x: Option<int> = Some(42)\n"
        "match x:\n"
        "    case Some(v):\n"
        "        print(v)\n"
        "    case None:\n"
        "        print(\"nothing\")\n"
        "let y: Option<int> = None\n"
        "match y:\n"
        "    case Some(v):\n"
        "        print(v)\n"
        "    case None:\n"
        "        print(\"nothing\")\n";
    EXPECT_EQ(runSource(src), "42\nnothing\n");
}

TEST_F(CodeGenTest, MatchIntLiteral) {
    std::string src =
        "let x = 2\n"
        "match x:\n"
        "    case 0:\n"
        "        print(\"zero\")\n"
        "    case 1:\n"
        "        print(\"one\")\n"
        "    case 2:\n"
        "        print(\"two\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "two\n");
}

TEST_F(CodeGenTest, MatchStringLiteral) {
    std::string src =
        "let s = \"hello\"\n"
        "match s:\n"
        "    case \"world\":\n"
        "        print(\"world\")\n"
        "    case \"hello\":\n"
        "        print(\"hello!\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "hello!\n");
}

TEST_F(CodeGenTest, MatchBoolLiteral) {
    std::string src =
        "let b = true\n"
        "match b:\n"
        "    case true:\n"
        "        print(\"yes\")\n"
        "    case false:\n"
        "        print(\"no\")\n";
    EXPECT_EQ(runSource(src), "yes\n");
}

TEST_F(CodeGenTest, MatchVariableBinding) {
    std::string src =
        "let x = 42\n"
        "match x:\n"
        "    case n:\n"
        "        print(n)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, MatchGuard) {
    std::string src =
        "let x = 5\n"
        "match x:\n"
        "    case n if n > 0:\n"
        "        print(\"positive\")\n"
        "    case n if n < 0:\n"
        "        print(\"negative\")\n"
        "    case _:\n"
        "        print(\"zero\")\n";
    EXPECT_EQ(runSource(src), "positive\n");
}

TEST_F(CodeGenTest, MatchGuardZero) {
    std::string src =
        "let x = 0\n"
        "match x:\n"
        "    case n if n > 0:\n"
        "        print(\"positive\")\n"
        "    case n if n < 0:\n"
        "        print(\"negative\")\n"
        "    case _:\n"
        "        print(\"zero\")\n";
    EXPECT_EQ(runSource(src), "zero\n");
}

TEST_F(CodeGenTest, MatchNonExhaustiveEnum) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "let c = Color::Red\n"
        "match c:\n"
        "    case Color::Red:\n"
        "        print(\"red\")\n"
        "    case Color::Green:\n"
        "        print(\"green\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveOption) {
    std::string src =
        "let x: Option<int> = Some(1)\n"
        "match x:\n"
        "    case Some(v):\n"
        "        print(v)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveBool) {
    std::string src =
        "let b = true\n"
        "match b:\n"
        "    case true:\n"
        "        print(\"yes\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveLiteral) {
    std::string src =
        "let x = 1\n"
        "match x:\n"
        "    case 0:\n"
        "        print(\"zero\")\n"
        "    case 1:\n"
        "        print(\"one\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNested) {
    std::string src =
        "enum Dir:\n"
        "    Up\n"
        "    Down\n"
        "let d = Dir::Up\n"
        "if true:\n"
        "    match d:\n"
        "        case Dir::Up:\n"
        "            print(\"up\")\n"
        "        case Dir::Down:\n"
        "            print(\"down\")\n";
    EXPECT_EQ(runSource(src), "up\n");
}

TEST_F(CodeGenTest, MatchNegativeLiteral) {
    std::string src =
        "let x = -1\n"
        "match x:\n"
        "    case -1:\n"
        "        print(\"neg one\")\n"
        "    case 0:\n"
        "        print(\"zero\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "neg one\n");
}

// ===== Union 型テスト =====

TEST_F(CodeGenTest, UnionLetInt) {
    std::string src =
        "let x: int | str = 42\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnionLetStr) {
    std::string src =
        "let x: int | str = \"hello\"\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, UnionReassign) {
    std::string src =
        "var x: int | str = 42\n"
        "print(x)\n"
        "x = \"hello\"\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "42\nhello\n");
}

TEST_F(CodeGenTest, UnionFnParam) {
    std::string src =
        "fn show(x: int | str) -> int:\n"
        "    print(x)\n"
        "    return 0\n"
        "show(42)\n"
        "show(\"hi\")\n";
    EXPECT_EQ(runSource(src), "42\nhi\n");
}

TEST_F(CodeGenTest, UnionFnReturn) {
    std::string src =
        "fn get_val(flag: bool) -> int | str:\n"
        "    if flag:\n"
        "        return 42\n"
        "    return \"hello\"\n"
        "let a = get_val(true)\n"
        "print(a)\n"
        "let b = get_val(false)\n"
        "print(b)\n";
    EXPECT_EQ(runSource(src), "42\nhello\n");
}

TEST_F(CodeGenTest, UnionPrint) {
    std::string src =
        "let x: int | str = 99\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, UnionTypeMismatch) {
    std::string src =
        "let x: int | str = 3.14\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BitwiseOrStillWorks) {
    EXPECT_EQ(runSource("print(1 | 2)"), "3\n");
}

TEST_F(CodeGenTest, UnionThreeTypes) {
    std::string src =
        "let x: int | float | str = 3.14\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "3.14\n");
}
