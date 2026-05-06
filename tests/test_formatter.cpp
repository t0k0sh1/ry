#include "ry/formatter.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include <gtest/gtest.h>


using namespace ry;
// Helper: parse source, format, return result
static std::string fmt(const std::string &src) {
    return Formatter::formatSource(src);
}

// ===== Literal Formatting =====

TEST(Formatter, LiteralFormatting) {
    // Number
    EXPECT_EQ(fmt("x = 42\n"), "x = 42\n");
    // Float
    EXPECT_EQ(fmt("x = 3.14\n"), "x = 3.14\n");
    // Bool
    EXPECT_EQ(fmt("x = true\n"), "x = true\n");
    EXPECT_EQ(fmt("y = false\n"), "y = false\n");
    // String
    EXPECT_EQ(fmt("x = \"hello\"\n"), "x = \"hello\"\n");
    // String escape
    EXPECT_EQ(fmt("x = \"a\\nb\"\n"), "x = \"a\\nb\"\n");
    // None
    EXPECT_EQ(fmt("x = none\n"), "x = none\n");
}

// ===== Expression Formatting =====

TEST(Formatter, ExpressionFormatting) {
    // Binary expression spacing
    EXPECT_EQ(fmt("x = 1 + 2\n"), "x = 1 + 2\n");
    EXPECT_EQ(fmt("x = a * b + c\n"), "x = a * b + c\n");
    // Unary expression
    EXPECT_EQ(fmt("x = -5\n"), "x = -5\n");
    EXPECT_EQ(fmt("x = not true\n"), "x = not true\n");
    // Call expression
    EXPECT_EQ(fmt("x = f(1, 2)\n"), "x = f(1, 2)\n");
}

// ===== Complex Expression Formatting =====

TEST(Formatter, ComplexExprFormatting) {
    // List
    EXPECT_EQ(fmt("x = [1, 2, 3]\n"), "x = [1, 2, 3]\n");
    // Map
    EXPECT_EQ(fmt("x = {\"a\": 1, \"b\": 2}\n"), "x = {\"a\": 1, \"b\": 2}\n");
    // when expression
    EXPECT_EQ(fmt("x = case:\n    a > 0 : 1\n    _ : 0\n"),
              "x = case:\n  a > 0 : 1\n  _ : 0\n");
    // Range
    EXPECT_EQ(fmt("x = 1..10\n"), "x = 1..10\n");
    // Enum access
    {
        auto src = "enum Color:\n    Red\n    Green\nx = Color::Red\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("x = Color::Red"), std::string::npos);
    }
    // Cast
    EXPECT_EQ(fmt("x = y as int\n"), "x = y as int\n");
}

// ===== Statement Formatting =====

TEST(Formatter, StatementFormatting) {
    // Assign with type
    EXPECT_EQ(fmt("x: int = 42\n"), "x: int = 42\n");
    // Import
    EXPECT_EQ(fmt("from io import print, println\n"), "from io import print, println\n");
    // Relative imports
    EXPECT_EQ(fmt("from . import add\n"), "from . import add\n");
    EXPECT_EQ(fmt("from .utils import helper\n"), "from .utils import helper\n");
    EXPECT_EQ(fmt("from .utils.calc import add\n"), "from .utils.calc import add\n");
    // Wildcard import (no 'import' keyword)
    EXPECT_EQ(fmt("from math\n"), "from math\n");
    EXPECT_EQ(fmt("from .\n"), "from .\n");
    // Return
    {
        auto src = "fn f() -> int:\n    return 42\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("  return 42"), std::string::npos);
    }
    // Break / Continue
    {
        auto src = "while true:\n    break\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("  break"), std::string::npos);
    }
    // Type alias
    EXPECT_EQ(fmt("type Num = int\n"), "type Num = int\n");
    // Index assignment
    {
        auto src = "xs = [1, 2, 3]\nxs[0] = 99\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("xs[0] = 99"), std::string::npos);
    }
    // Field assignment
    {
        auto src = "record Point:\n    x: int\n    y: int\n\np = Point(1, 2)\np.x = 100\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("p.x = 100"), std::string::npos);
    }
}

// ===== Indentation Levels =====

TEST(Formatter, IndentationLevels) {
    // Function indent (2-space)
    {
        auto src = "fn add(a: int, b: int) -> int:\n    return a + b\n";
        auto expected = "fn add(a: int, b: int) -> int:\n  return a + b\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Nested indent
    {
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
    // Deeply nested
    {
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
}

// ===== Record/Enum Tests =====

TEST(Formatter, RecordEnumDef) {
    // Record
    {
        auto src = "record Point:\n    x: int\n    y: int\n";
        auto expected = "record Point:\n  x: int\n  y: int\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Enum
    {
        auto src = "enum Color:\n    Red\n    Green\n    Blue\n";
        auto expected = "enum Color:\n  Red\n  Green\n  Blue\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Enum with associated data
    {
        auto src = "enum Shape:\n    Circle(float)\n    Rect(float, float)\n    Point\n";
        auto expected = "enum Shape:\n  Circle(float)\n  Rect(float, float)\n  Point\n";
        EXPECT_EQ(fmt(src), expected);
    }
}

// ===== Control Flow Tests =====

TEST(Formatter, ControlFlow) {
    // If / when
    {
        auto src =
            "case:\n    x > 10:\n        res = 1\n    x == 5:\n        res = 2\n    _:\n        res = 3\n";
        auto expected =
            "case:\n  x > 10:\n    res = 1\n  x == 5:\n    res = 2\n  _:\n    res = 3\n";
        EXPECT_EQ(fmt(src), expected);
    }
    {
        auto src = "x = if n > 0: \"pos\" else: \"neg\"\n";
        auto expected = "x = if n > 0: \"pos\" else: \"neg\"\n";
        EXPECT_EQ(fmt(src), expected);
    }
    {
        auto src = "x = if n > 0: \"pos\" else:\n    \"neg\"\n";
        auto expected = "x = if n > 0: \"pos\" else: \"neg\"\n";
        EXPECT_EQ(fmt(src), expected);
    }
    {
        auto src =
            "x = if n > 0:\n"
            "    a = 1\n"
            "    1\n"
            "else:\n"
            "    b = 2\n"
            "    2\n";
        auto expected =
            "x = if n > 0:\n"
            "  a = 1\n"
            "  1\n"
            "else:\n"
            "  b = 2\n"
            "  2\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // For loop
    {
        auto src = "for i in range(5):\n    sum += i\n";
        auto expected = "for i in range(5):\n  sum += i\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // For loop with two vars
    {
        auto src = "for k, v in m:\n    total += v\n";
        auto expected = "for k, v in m:\n  total += v\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // For loop with nested tuple destructuring
    {
        auto src = "for i,(k,v) in enumerate(items):\n    print(k)\n";
        auto expected = "for i, (k, v) in enumerate(items):\n  print(k)\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // For loop with top-level 1-tuple binding must keep parentheses
    {
        auto src = "for (x,) in xs:\n    print(x)\n";
        auto expected = "for (x,) in xs:\n  print(x)\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // While loop
    {
        auto src = "while i < 5:\n    i += 1\n";
        auto expected = "while i < 5:\n  i += 1\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Match statement
    {
        auto src =
            "case x:\n    1:\n        res = 10\n    _:\n        res = 0\n";
        auto expected =
            "case x:\n  1:\n    res = 10\n  _:\n    res = 0\n";
        EXPECT_EQ(fmt(src), expected);
    }
}

// ===== Function Tests =====

TEST(Formatter, FunctionVariants) {
    // Async function
    {
        auto src = "async fn fetch() -> int:\n    return 42\n";
        auto expected = "async fn fetch() -> int:\n  return 42\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Operator overload
    {
        auto src =
            "record Vec2:\n    x: float\n    y: float\n\n"
            "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
            "    return Vec2(a.x + b.x, a.y + b.y)\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("fn operator+(a: Vec2, b: Vec2) -> Vec2:"), std::string::npos);
        EXPECT_NE(out.find("  return Vec2(a.x + b.x, a.y + b.y)"), std::string::npos);
    }
}

// ===== Comment Formatting =====

TEST(Formatter, CommentFormatting) {
    // Standalone comment
    {
        auto src = "# this is a comment\nx = 42\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("# this is a comment"), std::string::npos);
        EXPECT_NE(out.find("x = 42"), std::string::npos);
    }
    // Inline comment
    {
        auto src = "x = 42  # inline\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("x = 42  # inline"), std::string::npos);
    }
    // Comment between functions
    {
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
    // Comment extraction via Formatter object
    {
        Formatter f("# comment\nx = 1\n");
        Lexer lex("# comment\nx = 1\n");
        Parser parser(lex);
        auto prog = parser.parseProgram();
        auto result = f.format(prog);
        EXPECT_NE(result.find("# comment"), std::string::npos);
    }
}

// ===== Native Directive Formatting =====

TEST(Formatter, NativeDirectiveFormatting) {
    // Basic native directive
    {
        auto src = "@native\nfn append(values: List<int>, value: int) -> Unit\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@native"), std::string::npos);
        EXPECT_NE(out.find("fn append("), std::string::npos);
    }
    // Native bang function
    {
        auto src = "@native\nfn sort!(values: List<int>) -> Unit\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@native"), std::string::npos);
        EXPECT_NE(out.find("fn sort!(values: List<int>) -> Unit"), std::string::npos);
    }
    // Native function has no colon
    {
        auto src = "@native\nfn append(values: List<int>, value: int) -> Unit\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("fn append(values: List<int>, value: int) -> Unit\n"), std::string::npos);
        EXPECT_EQ(out.find("-> Unit:"), std::string::npos);
    }
    // Native function has no body
    {
        auto src = "@native\nfn pop(values: List<int>) -> Option<int>\n";
        auto out = fmt(src);
        EXPECT_EQ(out.find("-> Option<int>:"), std::string::npos);
        EXPECT_NE(out.find("-> Option<int>\n"), std::string::npos);
    }
    // Native const declaration
    EXPECT_EQ(fmt("@native\n@const\nPI: float\n"), "@native\n@const\nPI: float\n");
    // Native const multiple declarations
    {
        std::string src = "@native\n@const\nPI: float\n\n@native\n@const\nE: float\n";
        EXPECT_EQ(fmt(src), src);
    }
    // Native def followed by section comment
    {
        auto src =
            "@native\n"
            "fn readAll() -> str\n"
            "\n"
            "# File I/O\n"
            "@native\n"
            "fn readText(path: str) -> str\n";
        std::string out = fmt(src);
        EXPECT_EQ(out.find("\n\n\n"), std::string::npos)
            << "Found triple newline (double blank line) in:\n" << out;
        EXPECT_NE(out.find("-> str\n\n# File I/O"), std::string::npos)
            << "Expected single blank line before comment in:\n" << out;
        // Idempotent
        std::string second = fmt(out);
        EXPECT_EQ(out, second)
            << "Formatting is not idempotent:\nFirst:\n" << out << "\nSecond:\n" << second;
    }
}

// ===== @native("libname") directive formatting =====

TEST(Formatter, NativeLibraryDirectiveRoundTrip) {
    // @native("base64") round-trips correctly
    {
        std::string src = "@native(\"base64\")\nfn encode(data: str) -> str\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@native(\"base64\")"), std::string::npos)
            << "Expected @native(\"base64\") in:\n" << out;
        // Idempotent
        EXPECT_EQ(out, fmt(out));
    }
    // @inline(mode="always") — value is a string literal, quotes preserved
    {
        std::string src = "@inline(mode=\"always\")\nfn add(a: int, b: int) -> int:\n    return a + b\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@inline(mode=\"always\")"), std::string::npos)
            << "Expected @inline(mode=\"always\") in:\n" << out;
        EXPECT_EQ(out, fmt(out));
    }
    // @deprecated(reason="use new_func") — string value keeps quotes
    {
        std::string src = "@deprecated(reason=\"use new_func\")\nfn old() -> int:\n    return 1\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@deprecated(reason=\"use new_func\")"), std::string::npos)
            << "Expected quoted reason in:\n" << out;
        EXPECT_EQ(out, fmt(out));
    }
    // String value with escape sequences round-trips correctly
    {
        std::string src = "@deprecated(reason=\"has \\\"quotes\\\" inside\")\nfn old() -> int:\n    return 1\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@deprecated(reason=\"has \\\"quotes\\\" inside\")"), std::string::npos)
            << "Expected escaped quotes in:\n" << out;
        EXPECT_EQ(out, fmt(out));
    }
}

// ===== Blank Line Tests =====

TEST(Formatter, BlankLineBetweenDefs) {
    auto src =
        "fn a() -> int:\n"
        "    return 1\n"
        "fn b() -> int:\n"
        "    return 2\n";
    auto out = fmt(src);
    EXPECT_NE(out.find("  return 1\n\nfn b"), std::string::npos);
}

// ===== Lambda / Trailing Block Tests =====

TEST(Formatter, LambdaAndTrailingBlock) {
    // Single-line lambda
    {
        auto src = "double = (x: int) => x * 2\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("double = (x: int) => x * 2"), std::string::npos);
    }
    // Directive-based test group: formatter preserves @describe/@it
    {
        auto src =
            "@describe(\"test\")\n"
            "fn testGroup():\n"
            "    @it(\"case\")\n"
            "    fn caseTest():\n"
            "        expect(1 + 1).toEq(2)\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("@describe(\"test\")"), std::string::npos);
        EXPECT_NE(out.find("@it(\"case\")"), std::string::npos);
    }
    // Expect statement
    {
        auto src =
            "fn id(x: int) -> int:\n"
            "    return x\n"
            "\n"
            "@describe(\"t\")\n"
            "fn tGroup():\n"
            "    @it(\"c\")\n"
            "    fn cCase():\n"
            "        expect(id(5)).toEq(5)\n";
        auto out = fmt(src);
        EXPECT_NE(out.find("expect(id(5)).toEq(5)"), std::string::npos);
    }
}

// ===== Bare-paren-omitted single-param lambda (#1572) =====

TEST(Formatter, BareLambdaCanonicalizesToParen) {
    // `s => s + 1` should be re-emitted in canonical paren form. The existing
    // formatter already emits `: any` for inferred-type lambda params, so the
    // canonical output for both `s => ...` and `(s) => ...` is `(s: any) => ...`.
    auto out = fmt("f = s => s + 1\n");
    EXPECT_NE(out.find("f = (s: any) => s + 1"), std::string::npos);
    // And the result must be identical to the existing paren-only form.
    EXPECT_EQ(fmt("f = s => s + 1\n"), fmt("f = (s) => s + 1\n"));
}

TEST(Formatter, BareLambdaRoundTripIdempotent) {
    // After one formatting pass, a second pass must be a no-op.
    auto first = fmt("f = s => s + 1\n");
    auto second = fmt(first);
    EXPECT_EQ(first, second);
}

// ===== Round-Trip / Idempotency Tests =====

TEST(Formatter, RoundTripAndIdempotency) {
    // Basic idempotency
    {
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
    // Verify formatting stable
    {
        auto src = "x: int = 1\n";
        auto formatted = Formatter::formatSource(src);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(formatted, reason));
        EXPECT_TRUE(reason.empty());
    }
    // Verify formatting re-parse failure
    {
        std::string garbage = "{{{{totally broken syntax}}}}";
        std::string reason;
        EXPECT_FALSE(Formatter::verifyFormatting(garbage, reason));
        EXPECT_NE(reason.find("failed to re-parse"), std::string::npos);
    }
    // Grouped expression round-trip
    {
        auto src = "x: int = (1 + 2) * 3\n";
        auto first = Formatter::formatSource(src);
        EXPECT_EQ(first, "x: int = (1 + 2) * 3\n");
        auto second = Formatter::formatSource(first);
        EXPECT_EQ(first, second);
    }
}

TEST(FormatterTest, TupleSingleElementRoundTrip) {
    auto src = "t = (42,)\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, "t = (42,)\n");
    auto second = Formatter::formatSource(first);
    EXPECT_EQ(first, second);
}

TEST(FormatterTest, TupleMultiElementRoundTrip) {
    auto src = "t = (1, 2)\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, "t = (1, 2)\n");
    auto second = Formatter::formatSource(first);
    EXPECT_EQ(first, second);
}

TEST(FormatterTest, ParenTupleDestructRoundTrip) {
    // #1189 — parenthesized tuple destructuring must round-trip through the
    // formatter. Guards against the historical `: ` emission between `)` and
    // `=` that made @const variants unparseable on re-format.
    {
        auto src = "(a, b) = (1, 2)\n";
        auto first = Formatter::formatSource(src);
        EXPECT_EQ(first, src);
        EXPECT_EQ(Formatter::formatSource(first), first);
    }
    {
        auto src = "@const\n(c, d) = (3, 4)\n";
        auto first = Formatter::formatSource(src);
        EXPECT_EQ(first, src);
        EXPECT_EQ(Formatter::formatSource(first), first);
    }
    {
        // `_` wildcard round-trip
        auto src = "(_, e) = (5, 6)\n";
        auto first = Formatter::formatSource(src);
        EXPECT_EQ(first, src);
        EXPECT_EQ(Formatter::formatSource(first), first);
    }
}

TEST(FormatterTest, DefaultArgRoundTrip) {
    auto src = "fn greet(name: str, greeting: str = \"Hello\") -> str:\n  return greeting\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    auto second = Formatter::formatSource(first);
    EXPECT_EQ(first, second);
}

// ===== @directive definition syntax (#708) — formatter round-trip =====

TEST(FormatterTest, DirectiveDefRoundTripSingleTarget) {
    auto src = "@directive(target=[\"function\"])\nfn it(description: str)\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
}

TEST(FormatterTest, DirectiveDefRoundTripMultipleTargets) {
    auto src =
        "@directive(target=[\"function\", \"record\"])\n"
        "fn cacheable()\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
}

// Bare-string sugar `target="function"` canonicalises to List form
// `target=["function"]` after one format pass, then round-trips idempotently.
TEST(FormatterTest, DirectiveDefBareStringSugarCanonicalises) {
    auto src = "@directive(target=\"function\")\nfn it(d: str)\n";
    auto expected = "@directive(target=[\"function\"])\nfn it(d: str)\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, expected);
    EXPECT_EQ(Formatter::formatSource(first), first);

    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(first, reason));
    EXPECT_TRUE(reason.empty());
}

TEST(FormatterTest, DirectiveDefRoundTripDefaultArg) {
    auto src =
        "@directive(target=[\"function\"])\n"
        "fn inline(mode: str = \"always\")\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
}

TEST(FormatterTest, DirectiveDefRoundTripWithPublic) {
    auto src =
        "@public\n"
        "@directive(target=[\"function\"])\n"
        "fn it(description: str)\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
}
