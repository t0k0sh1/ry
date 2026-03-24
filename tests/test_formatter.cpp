#include "ry/formatter.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include <gtest/gtest.h>

// Helper: parse source, format, return result
static std::string fmt(const std::string &src) {
    return Formatter::formatSource(src);
}

// ===== Expression Tests =====

TEST(Formatter, NumberLiteral) {
    EXPECT_EQ(fmt("x = 42\n"), "x = 42\n");
}

TEST(Formatter, FloatLiteral) {
    EXPECT_EQ(fmt("x = 3.14\n"), "x = 3.14\n");
}

TEST(Formatter, BoolLiteral) {
    EXPECT_EQ(fmt("x = true\n"), "x = true\n");
    EXPECT_EQ(fmt("y = false\n"), "y = false\n");
}

TEST(Formatter, StringLiteral) {
    EXPECT_EQ(fmt("x = \"hello\"\n"), "x = \"hello\"\n");
}

TEST(Formatter, StringEscape) {
    EXPECT_EQ(fmt("x = \"a\\nb\"\n"), "x = \"a\\nb\"\n");
}

TEST(Formatter, NoneLiteral) {
    EXPECT_EQ(fmt("x = none\n"), "x = none\n");
}

TEST(Formatter, BinaryExprSpacing) {
    EXPECT_EQ(fmt("x = 1 + 2\n"), "x = 1 + 2\n");
    EXPECT_EQ(fmt("x = a * b + c\n"), "x = a * b + c\n");
}

TEST(Formatter, UnaryExpr) {
    EXPECT_EQ(fmt("x = -5\n"), "x = -5\n");
    EXPECT_EQ(fmt("x = not true\n"), "x = not true\n");
}

TEST(Formatter, CallExpr) {
    EXPECT_EQ(fmt("x = f(1, 2)\n"), "x = f(1, 2)\n");
}

TEST(Formatter, ListExpr) {
    EXPECT_EQ(fmt("x = [1, 2, 3]\n"), "x = [1, 2, 3]\n");
}

TEST(Formatter, MapExpr) {
    EXPECT_EQ(fmt("x = {\"a\": 1, \"b\": 2}\n"), "x = {\"a\": 1, \"b\": 2}\n");
}

TEST(Formatter, TernaryExpr) {
    EXPECT_EQ(fmt("x = a > 0 ? 1 : 0\n"), "x = a > 0 ? 1 : 0\n");
}

TEST(Formatter, RangeExpr) {
    EXPECT_EQ(fmt("x = 1..10\n"), "x = 1..10\n");
}

TEST(Formatter, EnumAccess) {
    auto src = "enum Color:\n    Red\n    Green\nx = Color::Red\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("x = Color::Red"), std::string::npos);
}

TEST(Formatter, CastExpr) {
    EXPECT_EQ(fmt("x = y as int\n"), "x = y as int\n");
}

// ===== Statement Tests =====

TEST(Formatter, AssignWithType) {
    EXPECT_EQ(fmt("x: int = 42\n"), "x: int = 42\n");
}

TEST(Formatter, ImportStatement) {
    EXPECT_EQ(fmt("from std.io import print, println\n"), "from std.io import print, println\n");
}

TEST(Formatter, ReturnStatement) {
    auto src = "fn f() -> int:\n    return 42\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("  return 42"), std::string::npos);
}

TEST(Formatter, BreakContinue) {
    auto src = "while true:\n    break\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("  break"), std::string::npos);
}

// ===== Indentation Tests =====

TEST(Formatter, FunctionIndent2Space) {
    auto src = "fn add(a: int, b: int) -> int:\n    return a + b\n";
    auto expected = "fn add(a: int, b: int) -> int:\n  return a + b\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, NestedIndent) {
    auto src =
        "fn f(x: int) -> int:\n"
        "    if x > 0:\n"
        "        return x\n"
        "    return 0\n";
    auto expected =
        "fn f(x: int) -> int:\n"
        "  if x > 0:\n"
        "    return x\n"
        "  return 0\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, DeeplyNested) {
    auto src =
        "fn f() -> int:\n"
        "    if true:\n"
        "        while true:\n"
        "            return 1\n"
        "    return 0\n";
    auto expected =
        "fn f() -> int:\n"
        "  if true:\n"
        "    while true:\n"
        "      return 1\n"
        "  return 0\n";
    EXPECT_EQ(fmt(src), expected);
}

// ===== Record/Enum Tests =====

TEST(Formatter, RecordDef) {
    auto src = "record Point:\n    x: int\n    y: int\n";
    auto expected = "record Point:\n  x: int\n  y: int\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, EnumDef) {
    auto src = "enum Color:\n    Red\n    Green\n    Blue\n";
    auto expected = "enum Color:\n  Red\n  Green\n  Blue\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, EnumWithAssociatedData) {
    auto src = "enum Shape:\n    Circle(float)\n    Rect(float, float)\n    Point\n";
    auto expected = "enum Shape:\n  Circle(float)\n  Rect(float, float)\n  Point\n";
    EXPECT_EQ(fmt(src), expected);
}

// ===== Control Flow Tests =====

TEST(Formatter, IfElifElse) {
    auto src =
        "if x > 10:\n"
        "    res = 1\n"
        "elif x == 5:\n"
        "    res = 2\n"
        "else:\n"
        "    res = 3\n";
    auto expected =
        "if x > 10:\n"
        "  res = 1\n"
        "elif x == 5:\n"
        "  res = 2\n"
        "else:\n"
        "  res = 3\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, ForLoop) {
    // += is desugared by parser to x = x + expr
    auto src = "for i in range(5):\n    sum += i\n";
    auto expected = "for i in range(5):\n  sum = sum + i\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, ForLoopTwoVars) {
    auto src = "for k, v in m:\n    total += v\n";
    auto expected = "for k, v in m:\n  total = total + v\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, WhileLoop) {
    auto src = "while i < 5:\n    i += 1\n";
    auto expected = "while i < 5:\n  i = i + 1\n";
    EXPECT_EQ(fmt(src), expected);
}

// ===== Match Tests =====

TEST(Formatter, MatchStatement) {
    auto src =
        "match x:\n"
        "    case 1:\n"
        "        res = 10\n"
        "    case _:\n"
        "        res = 0\n";
    auto expected =
        "match x:\n"
        "  case 1:\n"
        "    res = 10\n"
        "  case _:\n"
        "    res = 0\n";
    EXPECT_EQ(fmt(src), expected);
}

// ===== Function Tests =====

TEST(Formatter, AsyncFunction) {
    auto src = "async fn fetch() -> int:\n    return 42\n";
    auto expected = "async fn fetch() -> int:\n  return 42\n";
    EXPECT_EQ(fmt(src), expected);
}

TEST(Formatter, OperatorOverload) {
    auto src =
        "record Vec2:\n    x: float\n    y: float\n\n"
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n";
    auto out = fmt(src);
    // Parser stores name as "operator+", so fn name is "operator+"
    EXPECT_NE(out.find("fn operator+(a: Vec2, b: Vec2) -> Vec2:"), std::string::npos);
    EXPECT_NE(out.find("  return Vec2(a.x + b.x, a.y + b.y)"), std::string::npos);
}

// ===== Directive Tests =====

TEST(Formatter, NativeDirective) {
    auto src = "@native\nfn append(values: List<int>, value: int) -> Unit\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("@native"), std::string::npos);
    EXPECT_NE(out.find("fn append("), std::string::npos);
}

TEST(Formatter, NativeBangFunction) {
    auto src = "@native\nfn sort!(values: List<int>) -> Unit\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("@native"), std::string::npos);
    EXPECT_NE(out.find("fn sort!(values: List<int>) -> Unit"), std::string::npos);
}

TEST(Formatter, NativeFunctionNoColon) {
    auto src = "@native\nfn append(values: List<int>, value: int) -> Unit\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("fn append(values: List<int>, value: int) -> Unit\n"), std::string::npos);
    // Must NOT have a colon after native function declaration
    EXPECT_EQ(out.find("-> Unit:"), std::string::npos);
}

TEST(Formatter, NativeFunctionNoBody) {
    auto src = "@native\nfn pop(values: List<int>) -> Option<int>\n";
    auto out = fmt(src);
    EXPECT_EQ(out.find("-> Option<int>:"), std::string::npos);
    EXPECT_NE(out.find("-> Option<int>\n"), std::string::npos);
}

// ===== Comment Tests =====

TEST(Formatter, StandaloneComment) {
    auto src = "# this is a comment\nx = 42\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("# this is a comment"), std::string::npos);
    EXPECT_NE(out.find("x = 42"), std::string::npos);
}

TEST(Formatter, InlineComment) {
    auto src = "x = 42  # inline\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("x = 42  # inline"), std::string::npos);
}

TEST(Formatter, CommentBetweenFunctions) {
    auto src =
        "fn a() -> int:\n"
        "    return 1\n"
        "\n"
        "# separator\n"
        "\n"
        "fn b() -> int:\n"
        "    return 2\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("# separator"), std::string::npos);
}

// ===== Blank Line Tests =====

TEST(Formatter, BlankLineBetweenDefs) {
    auto src =
        "fn a() -> int:\n"
        "    return 1\n"
        "fn b() -> int:\n"
        "    return 2\n";
    auto out = fmt(src);
    // Should have blank line between functions
    EXPECT_NE(out.find("  return 1\n\nfn b"), std::string::npos);
}

// ===== Lambda / Trailing Block Tests =====

TEST(Formatter, SingleLineLambda) {
    auto src = "double = fn(x: int): x * 2\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("double = fn(x: int): x * 2"), std::string::npos);
}

TEST(Formatter, TrailingBlockCall) {
    auto src =
        "describe(\"test\", fn():\n"
        "    it(\"case\", fn():\n"
        "        expect(1 + 1).to_eq(2)\n"
        "    )\n"
        ")\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("describe(\"test\", fn():"), std::string::npos);
    EXPECT_NE(out.find("  it(\"case\", fn():"), std::string::npos);
}

// ===== Expect Statement =====

TEST(Formatter, ExpectStatement) {
    auto src =
        "fn id(x: int) -> int:\n"
        "    return x\n"
        "\n"
        "describe(\"t\", fn():\n"
        "    it(\"c\", fn():\n"
        "        expect(id(5)).to_eq(5)\n"
        "    )\n"
        ")\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("expect(id(5)).to_eq(5)"), std::string::npos);
}

// ===== Idempotency Test =====

TEST(Formatter, Idempotent) {
    auto src =
        "fn add(a: int, b: int) -> int:\n"
        "  return a + b\n"
        "\n"
        "fn sub(a: int, b: int) -> int:\n"
        "  return a - b\n";
    auto first = fmt(src);
    auto second = fmt(first);
    EXPECT_EQ(first, second);
}

// ===== Type Alias =====

TEST(Formatter, TypeAlias) {
    auto src = "type Num = int\n";
    EXPECT_EQ(fmt(src), "type Num = int\n");
}

// ===== Tuple Destructuring =====

// TupleDestruct is tested indirectly since it requires specific parser context

// ===== Index Assignment =====

TEST(Formatter, IndexAssign) {
    auto src = "xs = [1, 2, 3]\nxs[0] = 99\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("xs[0] = 99"), std::string::npos);
}

// ===== Field Assignment =====

TEST(Formatter, FieldAssign) {
    auto src = "record Point:\n    x: int\n    y: int\n\np = Point(1, 2)\np.x = 100\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("p.x = 100"), std::string::npos);
}

// ===== Comment Extraction =====

TEST(FormatterComments, ExtractsStandalone) {
    Formatter f("# comment\nx = 1\n");
    Lexer lex("# comment\nx = 1\n");
    Parser parser(lex);
    auto prog = parser.parseProgram();
    auto result = f.format(prog);
    EXPECT_NE(result.find("# comment"), std::string::npos);
}
