#include "test_codegen_common.hpp"


using namespace ry;
// ===== Break / Continue =====

TEST_F(CodeGenTest, BreakInWhile) {
    std::string src =
        "i = 0\n"
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
        "i = 0\n"
        "sum = 0\n"
        "while i < 10:\n"
        "    i += 1\n"
        "    if i % 2 == 0:\n"
        "        continue\n"
        "    sum += i\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "25\n");
}

TEST_F(CodeGenTest, ParallelForBreakRejected) {
    std::string src =
        "@parallel\n"
        "for i in range(5):\n"
        "    if i == 2:\n"
        "        break\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ParallelForContinueRejected) {
    std::string src =
        "@parallel\n"
        "for i in range(5):\n"
        "    if i == 2:\n"
        "        continue\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Struct field assignment =====

TEST_F(CodeGenTest, StructFieldAssign) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p.x = 10\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "10\n2\n");
}

TEST_F(CodeGenTest, StructFieldAssignMultiple) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(0, 0)\n"
        "p.x = 3\n"
        "p.y = 4\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "3\n4\n");
}

TEST_F(CodeGenTest, StructFieldAssignConstError) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "@const\n"
        "p = Point(1, 2)\n"
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
        "xs = range(0)\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, RangeUsedAsList) {
    std::string src =
        "xs = range(3)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "0\n1\n2\n3\n");
}

// ===== Lambda (arrow function) tests =====

TEST_F(CodeGenTest, LambdaBasicExpr) {
    std::string src =
        "double = (x: int) => x * 2\n"
        "print(double(5))";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, LambdaNoParams) {
    std::string src =
        "answer = () => 42\n"
        "print(answer())";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, LambdaMultipleParams) {
    std::string src =
        "add = (a: int, b: int) => a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, LambdaFloat) {
    std::string src =
        "half = (x: float) => x / 2.0\n"
        "print(half(7.0))";
    EXPECT_EQ(runSource(src), "3.5\n");
}

TEST_F(CodeGenTest, LambdaAsArgument) {
    std::string src =
        "function apply(f: function(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "doubled = apply((n: int) => n * 2, 10)\n"
        "print(doubled)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, LambdaStoredAndPassed) {
    std::string src =
        "function apply(f: function(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "triple = (n: int) => n * 3\n"
        "print(apply(triple, 5))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, FunctionReference) {
    std::string src =
        "function square(x: int) -> int:\n"
        "    return x * x\n"
        "function apply(f: function(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "print(apply(square, 6))";
    EXPECT_EQ(runSource(src), "36\n");
}

TEST_F(CodeGenTest, LambdaBoolReturn) {
    std::string src =
        "is_positive = (x: int) => x > 0\n"
        "print(is_positive(5))\n"
        "print(is_positive(-3))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, LambdaMultiLine) {
    std::string src =
        "abs = (x: int):\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-7))\n"
        "print(abs(3))";
    EXPECT_EQ(runSource(src), "7\n3\n");
}

TEST_F(CodeGenTest, LambdaClosure) {
    std::string src =
        "offset = 10\n"
        "add_offset = (x: int) => x + offset\n"
        "print(add_offset(5))\n"
        "print(add_offset(20))";
    EXPECT_EQ(runSource(src), "15\n30\n");
}

TEST_F(CodeGenTest, LambdaArgCountError) {
    std::string src =
        "f = (x: int) => x\n"
        "f(1, 2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, LambdaArgTypeError) {
    std::string src =
        "f = (x: int) => x\n"
        "f(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Effectively final capture tests =====

TEST_F(CodeGenTest, CapturedVarReassignError) {
    std::string src =
        "x = 10\n"
        "f = ():\n"
        "    x = 20\n"
        "\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, CapturedVarCompoundAssignError) {
    std::string src =
        "x = 10\n"
        "f = ():\n"
        "    x += 1\n"
        "\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, CapturedVarReadOK) {
    std::string src =
        "x = 10\n"
        "f = () => x + 5\n"
        "print(f())";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, OuterReassignAfterCaptureOK) {
    std::string src =
        "base = 10\n"
        "add_base = (x: int) => x + base\n"
        "base = 99\n"
        "print(add_base(5))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, CapturedRecordFieldAssignOK) {
    // Field assignment on captured record should be allowed (not blocked by effectively-final)
    // The outer value is 0 because records are captured by value (copy)
    std::string src =
        "record Counter:\n"
        "    value: int\n"
        "\n"
        "c = Counter(0)\n"
        "inc = ():\n"
        "    c.value = c.value + 1\n"
        "    print(c.value)\n"
        "\n"
        "inc()\n"
        "print(c.value)";
    EXPECT_EQ(runSource(src), "1\n0\n");
}

TEST_F(CodeGenTest, CapturedVarShadowedByForLoopOK) {
    // A for-loop variable that shadows a captured variable should be assignable,
    // and the captured outer binding must remain intact after the loop
    std::string src =
        "x = 10\n"
        "f = ():\n"
        "    for x in [1, 2, 3]:\n"
        "        x += 10\n"
        "        print(x)\n"
        "    print(x)\n"
        "\n"
        "f()";
    EXPECT_EQ(runSource(src), "11\n12\n13\n10\n");
}

TEST_F(CodeGenTest, CapturedConstFieldAssignError) {
    // @const variable captured by closure must still block field assignment
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "\n"
        "@const p = Point(1)\n"
        "f = ():\n"
        "    p.x = 99\n"
        "\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Set テスト =====

TEST_F(CodeGenTest, SetCreateAndIn) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(2 in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, SetNotIn) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(5 in s)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, SetLen) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(length(s))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, SetAdd) {
    std::string src =
        "s = {1, 2, 3}\n"
        "s.add(4)\n"
        "print(4 in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, SetAddDuplicate) {
    std::string src =
        "s = {1, 2, 3}\n"
        "s.add(1)\n"
        "print(length(s))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, SetRemove) {
    std::string src =
        "s = {1, 2, 3}\n"
        "s.remove(2)\n"
        "print(2 in s)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, SetPrint) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "{1, 2, 3}\n");
}

TEST_F(CodeGenTest, SetStringElements) {
    std::string src =
        "s = {\"a\", \"b\"}\n"
        "print(\"a\" in s)\n"
        "print(\"c\" in s)";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, SetEmptyWithAnnotation) {
    std::string src =
        "s: Set<int> = {}\n"
        "print(length(s))\n"
        "s.add(42)\n"
        "print(42 in s)";
    EXPECT_EQ(runSource(src), "0\ntrue\n");
}

TEST_F(CodeGenTest, SetInFunction) {
    std::string src =
        "function has_element(s: Set<int>, x: int) -> bool:\n"
        "    return x in s\n"
        "s = {10, 20, 30}\n"
        "print(has_element(s, 20))\n"
        "print(has_element(s, 40))";
    EXPECT_EQ(runSource(src), "true\nfalse\n");
}

TEST_F(CodeGenTest, SetForIn) {
    std::string src =
        "s = {10, 20, 30}\n"
        "total = 0\n"
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
        "c = Color::Red\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "Red\n");
}

TEST_F(CodeGenTest, EnumComparison) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "c = Color::Red\n"
        "print(c == Color::Red)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, EnumNotEqual) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "c = Color::Red\n"
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
        "d = Direction::Up\n"
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
        "function is_red(c: Color) -> bool:\n"
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
        "c = Color::Green\n"
        "s = Size::Large\n"
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
        "c = Color::Green\n"
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
        "c = Color::Blue\n"
        "match c:\n"
        "    case Color::Red:\n"
        "        print(\"red\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "other\n");
}

TEST_F(CodeGenTest, MatchOptionSomeNone) {
    std::string src =
        "x: Option<int> = Some(42)\n"
        "match x:\n"
        "    case Some(v):\n"
        "        print(v)\n"
        "    case None:\n"
        "        print(\"nothing\")\n"
        "y: Option<int> = None\n"
        "match y:\n"
        "    case Some(v):\n"
        "        print(v)\n"
        "    case None:\n"
        "        print(\"nothing\")\n";
    EXPECT_EQ(runSource(src), "42\nnothing\n");
}

TEST_F(CodeGenTest, MatchIntLiteral) {
    std::string src =
        "x = 2\n"
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
        "s = \"hello\"\n"
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
        "b = true\n"
        "match b:\n"
        "    case true:\n"
        "        print(\"yes\")\n"
        "    case false:\n"
        "        print(\"no\")\n";
    EXPECT_EQ(runSource(src), "yes\n");
}

TEST_F(CodeGenTest, MatchVariableBinding) {
    std::string src =
        "x = 42\n"
        "match x:\n"
        "    case n:\n"
        "        print(n)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, MatchGuard) {
    std::string src =
        "x = 5\n"
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
        "x = 0\n"
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
        "c = Color::Red\n"
        "match c:\n"
        "    case Color::Red:\n"
        "        print(\"red\")\n"
        "    case Color::Green:\n"
        "        print(\"green\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveOption) {
    std::string src =
        "x: Option<int> = Some(1)\n"
        "match x:\n"
        "    case Some(v):\n"
        "        print(v)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveBool) {
    std::string src =
        "b = true\n"
        "match b:\n"
        "    case true:\n"
        "        print(\"yes\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveLiteral) {
    std::string src =
        "x = 1\n"
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
        "d = Dir::Up\n"
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
        "x = -1\n"
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
        "x: int | str = 42\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnionLetStr) {
    std::string src =
        "x: int | str = \"hello\"\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "hello\n");
}

TEST_F(CodeGenTest, UnionReassign) {
    std::string src =
        "x: int | str = 42\n"
        "print(x)\n"
        "x = \"hello\"\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "42\nhello\n");
}

TEST_F(CodeGenTest, UnionFnParam) {
    std::string src =
        "function show(x: int | str) -> int:\n"
        "    print(x)\n"
        "    return 0\n"
        "show(42)\n"
        "show(\"hi\")\n";
    EXPECT_EQ(runSource(src), "42\nhi\n");
}

TEST_F(CodeGenTest, UnionFnReturn) {
    std::string src =
        "function get_val(flag: bool) -> int | str:\n"
        "    if flag:\n"
        "        return 42\n"
        "    return \"hello\"\n"
        "a = get_val(true)\n"
        "print(a)\n"
        "b = get_val(false)\n"
        "print(b)\n";
    EXPECT_EQ(runSource(src), "42\nhello\n");
}

TEST_F(CodeGenTest, UnionPrint) {
    std::string src =
        "x: int | str = 99\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, UnionTypeMismatch) {
    std::string src =
        "x: int | str = 3.14\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BitwiseOrStillWorks) {
    EXPECT_EQ(runSource("print(1 | 2)"), "3\n");
}

TEST_F(CodeGenTest, UnionThreeTypes) {
    std::string src =
        "x: int | float | str = 3.14\n"
        "print(x)\n";
    EXPECT_EQ(runSource(src), "3.14\n");
}

// ===== not in テスト =====

TEST_F(CodeGenTest, NotInSetTrue) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(4 not in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, NotInSetFalse) {
    std::string src =
        "s = {1, 2, 3}\n"
        "print(2 not in s)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, NotInSetString) {
    std::string src =
        "s = {\"a\", \"b\"}\n"
        "print(\"c\" not in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, NotInWithInRegression) {
    // in still works after not in was added
    std::string src =
        "s = {1, 2, 3}\n"
        "print(2 in s)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, NotExprRegression) {
    // not x still works
    EXPECT_EQ(runSource("x = not true\nprint(x)"), "false\n");
}

// ===== f-string tests =====

TEST_F(CodeGenTest, FStringBasic) {
    EXPECT_EQ(runSource("name = \"world\"\nprint(f\"Hello {name}\")"), "Hello world\n");
}

TEST_F(CodeGenTest, FStringMultipleExprs) {
    EXPECT_EQ(runSource("a = 1\nb = 2\nprint(f\"{a} + {b} = {a + b}\")"), "1 + 2 = 3\n");
}

TEST_F(CodeGenTest, FStringNoInterpolation) {
    EXPECT_EQ(runSource("print(f\"plain text\")"), "plain text\n");
}

TEST_F(CodeGenTest, FStringEscapedBraces) {
    EXPECT_EQ(runSource("print(f\"{{braces}}\")"), "{braces}\n");
}

TEST_F(CodeGenTest, FStringIntValue) {
    EXPECT_EQ(runSource("print(f\"value = {42}\")"), "value = 42\n");
}

TEST_F(CodeGenTest, FStringFloatValue) {
    EXPECT_EQ(runSource("print(f\"pi = {3.14}\")"), "pi = 3.14\n");
}

TEST_F(CodeGenTest, FStringBoolValue) {
    EXPECT_EQ(runSource("print(f\"flag = {true}\")"), "flag = true\n");
}

// ===== as cast tests =====

TEST_F(CodeGenTest, AsCastIntToFloat) {
    EXPECT_EQ(runSource("x = 42 as float\nprint(x)"), "42\n");
}

TEST_F(CodeGenTest, AsCastFloatToInt) {
    EXPECT_EQ(runSource("x = 3.14 as int\nprint(x)"), "3\n");
}

TEST_F(CodeGenTest, AsCastIntToBool) {
    EXPECT_EQ(runSource("x = 1 as bool\nprint(x)"), "true\n");
    EXPECT_EQ(runSource("x = 0 as bool\nprint(x)"), "false\n");
}

TEST_F(CodeGenTest, AsCastBoolToInt) {
    EXPECT_EQ(runSource("x = true as int\nprint(x)"), "1\n");
}

TEST_F(CodeGenTest, AsCastIntToStr) {
    EXPECT_EQ(runSource("x = 42 as str\nprint(x)"), "42\n");
}

TEST_F(CodeGenTest, AsCastFloatToStr) {
    EXPECT_EQ(runSource("x = 3.14 as str\nprint(x)"), "3.14\n");
}

TEST_F(CodeGenTest, AsCastBoolToStr) {
    EXPECT_EQ(runSource("x = true as str\nprint(x)"), "true\n");
}

TEST_F(CodeGenTest, AsCastIntToU8) {
    EXPECT_EQ(runSource("x = 255 as u8\nprint(x)"), "255\n");
}

TEST_F(CodeGenTest, AsCastU8ToInt) {
    EXPECT_EQ(runSource("b: u8 = 200\nx = b as int\nprint(x)"), "200\n");
}

// ===== Error type tests =====

TEST_F(CodeGenTest, ErrorConstructor1Arg) {
    std::string src = R"(
e = Error("something went wrong")
print(e.message)
print(e.code)
)";
    EXPECT_EQ(runSource(src), "something went wrong\n0\n");
}

TEST_F(CodeGenTest, ErrorConstructor2Args) {
    std::string src = R"(
e = Error("not found", 404)
print(e.message)
print(e.code)
)";
    EXPECT_EQ(runSource(src), "not found\n404\n");
}

TEST_F(CodeGenTest, ErrorOptionNone) {
    std::string src = R"(
e: Error? = none
print(e == none)
)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, ErrorPrint) {
    std::string src = R"(
e = Error("test error", 42)
print(e)
)";
    EXPECT_EQ(runSource(src), "Error: test error (code: 42)\n");
}

// ===== Result type tests =====

TEST_F(CodeGenTest, ResultOkMatch) {
    std::string src = R"(
function read_file(path: str) -> Result<str, Error>:
    if path == "":
        return Err(Error("empty path"))
    return Ok("content")

res = read_file("test.txt")
match res:
    case Ok(data):
        print(data)
    case Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "content\n");
}

TEST_F(CodeGenTest, ResultErrMatch) {
    std::string src = R"(
function read_file(path: str) -> Result<str, Error>:
    if path == "":
        return Err(Error("empty path"))
    return Ok("content")

res = read_file("")
match res:
    case Ok(data):
        print(data)
    case Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "empty path\n");
}

TEST_F(CodeGenTest, ResultDivideOk) {
    std::string src = R"(
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

match divide(10, 2):
    case Ok(v):
        print(v)
    case Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, ResultDivideErr) {
    std::string src = R"(
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

match divide(10, 0):
    case Ok(v):
        print(v)
    case Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "division by zero\n");
}

// ===== when OR pattern =====

TEST_F(CodeGenTest, MatchOrPatternInt) {
    std::string src =
        "x = 2\n"
        "match x:\n"
        "    case 1 | 2 | 3:\n"
        "        print(\"small\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "small\n");
}

TEST_F(CodeGenTest, MatchOrPatternIntNoMatch) {
    std::string src =
        "x = 5\n"
        "match x:\n"
        "    case 1 | 2 | 3:\n"
        "        print(\"small\")\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "other\n");
}

TEST_F(CodeGenTest, MatchOrPatternEnum) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "c = Color::Red\n"
        "match c:\n"
        "    case Color::Red | Color::Blue:\n"
        "        print(\"rb\")\n"
        "    case Color::Green:\n"
        "        print(\"g\")\n";
    EXPECT_EQ(runSource(src), "rb\n");
}

TEST_F(CodeGenTest, MatchOrPatternExhaustive) {
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "c = Color::Green\n"
        "match c:\n"
        "    case Color::Red | Color::Blue:\n"
        "        print(\"rb\")\n"
        "    case Color::Green:\n"
        "        print(\"g\")\n";
    EXPECT_EQ(runSource(src), "g\n");
}

TEST_F(CodeGenTest, MatchOrPatternVariableError) {
    std::string src =
        "x = 1\n"
        "match x:\n"
        "    case a | b:\n"
        "        print(a)\n"
        "    case _:\n"
        "        print(\"other\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== enum ADT (associated data) =====

TEST_F(CodeGenTest, EnumADTCreate) {
    std::string src =
        "enum Shape:\n"
        "    Circle(float)\n"
        "    Rectangle(float, float)\n"
        "    Point\n"
        "s = Shape::Circle(3.14)\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "Circle(3.14)\n");
}

TEST_F(CodeGenTest, EnumADTMatchBinding) {
    std::string src =
        "enum Shape:\n"
        "    Circle(float)\n"
        "    Rectangle(float, float)\n"
        "    Point\n"
        "s = Shape::Circle(3.14)\n"
        "match s:\n"
        "    case Shape::Circle(r):\n"
        "        print(r)\n"
        "    case Shape::Rectangle(w, h):\n"
        "        print(w)\n"
        "    case Shape::Point:\n"
        "        print(\"point\")";
    EXPECT_EQ(runSource(src), "3.14\n");
}

TEST_F(CodeGenTest, EnumADTMatchRect) {
    std::string src =
        "enum Shape:\n"
        "    Circle(float)\n"
        "    Rectangle(float, float)\n"
        "    Point\n"
        "s = Shape::Rectangle(3.0, 4.0)\n"
        "match s:\n"
        "    case Shape::Circle(r):\n"
        "        print(r)\n"
        "    case Shape::Rectangle(w, h):\n"
        "        print(w + h)\n"
        "    case Shape::Point:\n"
        "        print(\"point\")";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, EnumADTMixed) {
    std::string src =
        "enum Shape:\n"
        "    Circle(float)\n"
        "    Rectangle(float, float)\n"
        "    Point\n"
        "s = Shape::Point\n"
        "match s:\n"
        "    case Shape::Circle(r):\n"
        "        print(r)\n"
        "    case Shape::Rectangle(w, h):\n"
        "        print(w)\n"
        "    case Shape::Point:\n"
        "        print(\"point\")";
    EXPECT_EQ(runSource(src), "point\n");
}

TEST_F(CodeGenTest, EnumADTSingleField) {
    std::string src =
        "enum Wrapper:\n"
        "    IntVal(int)\n"
        "    StrVal(str)\n"
        "w = Wrapper::IntVal(42)\n"
        "match w:\n"
        "    case Wrapper::IntVal(v):\n"
        "        print(v)\n"
        "    case Wrapper::StrVal(s):\n"
        "        print(s)";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== Generic enum =====

TEST_F(CodeGenTest, GenericEnumBasic) {
    std::string src =
        "enum MyOption<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "x = MyOption<int>::MySome(42)\n"
        "match x:\n"
        "    case MyOption::MySome(v):\n"
        "        print(v)\n"
        "    case MyOption::MyNone:\n"
        "        print(\"none\")";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, GenericEnumNone) {
    std::string src =
        "enum MyOption<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "x = MyOption<int>::MyNone\n"
        "match x:\n"
        "    case MyOption::MySome(v):\n"
        "        print(v)\n"
        "    case MyOption::MyNone:\n"
        "        print(\"none\")";
    EXPECT_EQ(runSource(src), "none\n");
}

TEST_F(CodeGenTest, GenericEnumFloat) {
    std::string src =
        "enum MyOption<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "x = MyOption<float>::MySome(3.14)\n"
        "match x:\n"
        "    case MyOption::MySome(v):\n"
        "        print(v)\n"
        "    case MyOption::MyNone:\n"
        "        print(\"none\")";
    EXPECT_EQ(runSource(src), "3.14\n");
}

// ===== Explicit enum values =====

TEST_F(CodeGenTest, EnumExplicitValuePrint) {
    std::string src =
        "enum HttpStatus:\n"
        "    Ok = 200\n"
        "    NotFound = 404\n"
        "    InternalError = 500\n"
        "s = HttpStatus::Ok\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "Ok\n");
}

TEST_F(CodeGenTest, EnumExplicitValueComparison) {
    std::string src =
        "enum HttpStatus:\n"
        "    Ok = 200\n"
        "    NotFound = 404\n"
        "    InternalError = 500\n"
        "s = HttpStatus::NotFound\n"
        "print(s == HttpStatus::NotFound)\n"
        "print(s != HttpStatus::Ok)";
    EXPECT_EQ(runSource(src), "true\ntrue\n");
}

TEST_F(CodeGenTest, EnumExplicitValueMatch) {
    std::string src =
        "enum HttpStatus:\n"
        "    Ok = 200\n"
        "    NotFound = 404\n"
        "    InternalError = 500\n"
        "s = HttpStatus::InternalError\n"
        "match s:\n"
        "    case HttpStatus::Ok:\n"
        "        print(\"ok\")\n"
        "    case HttpStatus::NotFound:\n"
        "        print(\"not found\")\n"
        "    case HttpStatus::InternalError:\n"
        "        print(\"error\")";
    EXPECT_EQ(runSource(src), "error\n");
}

TEST_F(CodeGenTest, EnumExplicitValueDuplicateError) {
    std::string src =
        "enum Bad:\n"
        "    A = 1\n"
        "    B = 1\n"
        "x = Bad::A";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, EnumExplicitValueNegative) {
    std::string src =
        "enum Temp:\n"
        "    Cold = -10\n"
        "    Hot = 40\n"
        "t = Temp::Cold\n"
        "print(t)";
    EXPECT_EQ(runSource(src), "Cold\n");
}

// ===== r-string =====

TEST_F(CodeGenTest, RawStringPrint) {
    EXPECT_EQ(runSource(R"(print(r"\n"))"), "\\n\n");
}

TEST_F(CodeGenTest, RawStringConcat) {
    EXPECT_EQ(runSource(R"(print(r"\t" + "hello"))"), "\\thello\n");
}

// ===== test matcher: to_not_eq =====

TEST_F(CodeGenTest, ExpectToNotEq) {
    std::string src =
        "describe(\"matchers\", ():\n"
        "    it(\"not equal\", ():\n"
        "        expect(1).to_not_eq(2)\n"
        "    )\n"
        ")";
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== test matcher: to_be_some =====

TEST_F(CodeGenTest, ExpectToBeSome) {
    std::string src =
        "describe(\"matchers\", ():\n"
        "    it(\"some\", ():\n"
        "        x = Some(5)\n"
        "        expect(x).to_be_some()\n"
        "    )\n"
        ")";
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== test matcher: to_contain =====

TEST_F(CodeGenTest, ExpectToContainList) {
    std::string src =
        "describe(\"matchers\", ():\n"
        "    it(\"contains\", ():\n"
        "        xs = [1, 2, 3]\n"
        "        expect(xs).to_contain(2)\n"
        "    )\n"
        ")";
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToContainString) {
    std::string src =
        "describe(\"matchers\", ():\n"
        "    it(\"contains str\", ():\n"
        "        s = \"hello world\"\n"
        "        expect(s).to_contain(\"world\")\n"
        "    )\n"
        ")";
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== lambda argument syntax =====

TEST_F(CodeGenTest, LambdaArgDescribeIt) {
    std::string src =
        "describe(\"Calculator\", ():\n"
        "    it(\"adds\", ():\n"
        "        expect(1 + 2).to_eq(3)\n"
        "    )\n"
        "    it(\"subtracts\", ():\n"
        "        expect(5 - 3).to_eq(2)\n"
        "    )\n"
        ")";
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== Field assignment subtype coercion (#359) =====

TEST_F(CodeGenTest, FieldAssignSubtypeCoercion) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "record Owner:\n"
        "    pet: Animal\n"
        "o = Owner(Animal(\"cat\", 4))\n"
        "o.pet = Dog(\"Rex\", 4, \"Lab\")\n"
        "print(o.pet.name)\n"
        "print(o.pet.legs)";
    EXPECT_EQ(runSource(src), "Rex\n4\n");
}

// ===== ? operator subtype coercion (#360) =====

TEST_F(CodeGenTest, ErrorPropagateSubtypeCoercion) {
    std::string src =
        "record ApiError < Error:\n"
        "    endpoint: str\n"
        "function fetch(url: str) -> Result<int, ApiError>:\n"
        "    return Err(ApiError(\"fail\", 500, url))\n"
        "function process(url: str) -> Result<int, Error>:\n"
        "    val = fetch(url)?\n"
        "    return Ok(val)\n"
        "result = process(\"/api\")\n"
        "match result:\n"
        "    case Err(e):\n"
        "        print(e.message)\n"
        "    case Ok(v):\n"
        "        print(v)";
    EXPECT_EQ(runSource(src), "fail\n");
}

// ===== Generic record bounds (#297) =====

TEST_F(CodeGenTest, GenericRecordBound) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "function describe<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(describe(Dog(\"Rex\", 4, \"Lab\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundExactType) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "function describe<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(describe(Animal(\"Cat\", 4)))";
    EXPECT_EQ(runSource(src), "Cat\n");
}

TEST_F(CodeGenTest, GenericRecordBoundViolation) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "function describe<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "describe(42)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, GenericRecordBoundDeepInheritance) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "record GuideDog < Dog:\n"
        "    handler: str\n"
        "function get_name<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(get_name(GuideDog(\"Rex\", \"Lab\", \"John\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundExplicitTypeArg) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "function get_name<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(get_name[Dog](Dog(\"Rex\", \"Lab\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundMixedParams) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "function pair_name<T: Animal, U>(a: T, x: U) -> str:\n"
        "    return a.name\n"
        "print(pair_name(Dog(\"Rex\", \"Lab\"), 42))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

// ===== Tail Call Optimization (#214) =====

TEST_F(CodeGenTest, TailCallOptimization) {
    // Deep self-recursive tail call should not stack overflow
    EXPECT_EQ(runSource(
        "function sum_to(n: int, acc: int) -> int:\n"
        "  if n <= 0:\n"
        "    return acc\n"
        "  return sum_to(n - 1, acc + n)\n"
        "print(sum_to(1000000, 0))"), "500000500000\n");
}

TEST_F(CodeGenTest, TailCallFactorial) {
    // Tail-recursive factorial with accumulator
    EXPECT_EQ(runSource(
        "function factorial(n: int, acc: int) -> int:\n"
        "  if n <= 1:\n"
        "    return acc\n"
        "  return factorial(n - 1, n * acc)\n"
        "print(factorial(20, 1))"), "2432902008176640000\n");
}

TEST_F(CodeGenTest, NonTailRecursionStillWorks) {
    // n * factorial(n-1) is NOT a tail call — should still work for small N
    EXPECT_EQ(runSource(
        "function factorial(n: int) -> int:\n"
        "  if n <= 1:\n"
        "    return 1\n"
        "  return n * factorial(n - 1)\n"
        "print(factorial(10))"), "3628800\n");
}

// ===== Mutual Recursion (Forward Function References) =====

TEST_F(CodeGenTest, MutualRecursion) {
    EXPECT_EQ(runSource(
        "function is_even(n: int) -> bool:\n"
        "  if n == 0:\n"
        "    return true\n"
        "  return is_odd(n - 1)\n"
        "\n"
        "function is_odd(n: int) -> bool:\n"
        "  if n == 0:\n"
        "    return false\n"
        "  return is_even(n - 1)\n"
        "\n"
        "print(is_even(4))\n"
        "print(is_odd(3))"), "true\ntrue\n");
}

TEST_F(CodeGenTest, ThreeWayMutualRecursion) {
    EXPECT_EQ(runSource(
        "function fa(n: int) -> str:\n"
        "  if n <= 0:\n"
        "    return \"a\"\n"
        "  return fb(n - 1)\n"
        "\n"
        "function fb(n: int) -> str:\n"
        "  if n <= 0:\n"
        "    return \"b\"\n"
        "  return fc(n - 1)\n"
        "\n"
        "function fc(n: int) -> str:\n"
        "  if n <= 0:\n"
        "    return \"c\"\n"
        "  return fa(n - 1)\n"
        "\n"
        "print(fa(0))\n"
        "print(fa(1))\n"
        "print(fa(2))\n"
        "print(fa(3))"), "a\nb\nc\na\n");
}

TEST_F(CodeGenTest, ForwardFunctionReference) {
    EXPECT_EQ(runSource(
        "function caller(n: int) -> int:\n"
        "  return callee(n) + 1\n"
        "\n"
        "function callee(n: int) -> int:\n"
        "  return n * 2\n"
        "\n"
        "print(caller(5))"), "11\n");
}
