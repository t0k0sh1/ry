#include "ry/formatter.hpp"
#include "ry/lexer/lexer.hpp"
#include "ry/parser/parser.hpp"
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

// ===== Block String Literal Formatting (#1843) =====

TEST(Formatter, BlockStringLiteralFormatting) {
    // Same-line round-trip preserves """...""" form
    EXPECT_EQ(fmt("s = \"\"\"hello\"\"\"\n"), "s = \"\"\"hello\"\"\"\n");

    // Empty block string
    EXPECT_EQ(fmt("s = \"\"\"\"\"\"\n"), "s = \"\"\"\"\"\"\n");

    // Multiline block string – issue exact example. Top-level indent_level=0
    // produces 2-space baseline for content and closing """.
    {
        const std::string input =
            "s = \"\"\"\n"
            "  a\n"
            "    b\n"
            "  c\n"
            "  \"\"\"\n";
        const std::string expected =
            "s = \"\"\"\n"
            "  a\n"
            "    b\n"
            "  c\n"
            "  \"\"\"\n";
        EXPECT_EQ(fmt(input), expected);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(fmt(input), reason)) << reason;
    }

    // Blank line preservation
    {
        const std::string input =
            "s = \"\"\"\n"
            "  a\n"
            "\n"
            "  b\n"
            "  \"\"\"\n";
        // After lexing: value = "a\n\nb". Formatter emits blank line raw.
        const std::string expected =
            "s = \"\"\"\n"
            "  a\n"
            "\n"
            "  b\n"
            "  \"\"\"\n";
        EXPECT_EQ(fmt(input), expected);
    }

    // Regular string stays "..." (regression guard for the is_block branch)
    EXPECT_EQ(fmt("s = \"hello\"\n"), "s = \"hello\"\n");
    EXPECT_EQ(fmt("s = \"a\\nb\"\n"), "s = \"a\\nb\"\n");

    // Embedded triple-quote in value: escape `"` characters in the body so
    // re-parsing reconstructs the value exactly. The first `"` of the run is
    // always escaped (rule a); the third `"` is also escaped here because it
    // lies within the last two positions (rule b for the trailing-quote
    // safeguard). Both orderings of the inner escapes round-trip correctly.
    {
        const std::string input = "s = \"\"\"a\\\"\"\"b\"\"\"\n";
        const std::string out = fmt(input);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(out, reason)) << reason;
    }

    // Indent-aware multiline output: a block string inside a function body
    // (indent_level_ = 1) must use a baseline of 4 spaces for content and the
    // closing """, not 2. This exercises the `(indent_level_ + 1) *
    // indent_width_` math at depth ≥ 1 — the spec tests cover the *value* at
    // depth but never the *formatted output*, which has a different shape.
    {
        const std::string input =
            "fn doc() -> str:\n"
            "  return \"\"\"\n"
            "    a\n"
            "      b\n"
            "    c\n"
            "    \"\"\"\n";
        const std::string expected =
            "fn doc() -> str:\n"
            "  return \"\"\"\n"
            "    a\n"
            "      b\n"
            "    c\n"
            "    \"\"\"\n";
        EXPECT_EQ(fmt(input), expected);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(fmt(input), reason)) << reason;
    }

    // Trailing-quote round-trip: a block string whose content ends in `"` or
    // `""` would otherwise merge with the closing `"""` and close early on
    // round-trip. The formatter must escape the trailing `"`s.
    {
        // value ends in single `"`: r"""a"""" must escape → """a\""""
        const std::string input = "s = \"\"\"a\\\"\"\"\"\n";
        const std::string out = fmt(input);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(out, reason)) << reason;
    }
    {
        // value ends in double `""`: """a\"\"""""  must round-trip
        const std::string input = "s = \"\"\"a\\\"\\\"\"\"\"\n";
        const std::string out = fmt(input);
        std::string reason;
        EXPECT_TRUE(Formatter::verifyFormatting(out, reason)) << reason;
    }
}

// ===== Regex Literal Formatting (#2113) =====

TEST(Formatter, RegexLiteralFormatting) {
    EXPECT_EQ(fmt("x = /abc/\n"), "x = /abc/\n");
    // Discriminating: lexer keeps regex backslashes verbatim, so routing
    // pattern through escapeString would emit /\\d+/ and break regex meaning.
    EXPECT_EQ(fmt("x = /\\d+/\n"), "x = /\\d+/\n");
    EXPECT_EQ(fmt("x = /a\\/b/\n"), "x = /a\\/b/\n");
    // Lexer translates `\0` -> literal NUL inside pattern; formatter must
    // reverse it so the output file stays text and remains executable.
    EXPECT_EQ(fmt("x = /a\\0b/\n"), "x = /a\\0b/\n");
    // End-to-end repro from the issue body.
    EXPECT_EQ(fmt("from regex import replace\nprint(replace(\"order-12\", /\\d+/, \"N\"))\n"),
              "from regex import replace\nprint(replace(\"order-12\", /\\d+/, \"N\"))\n");
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
    // Import alias (#1721)
    EXPECT_EQ(fmt("from math import add as plus\n"), "from math import add as plus\n");
    EXPECT_EQ(fmt("from math import a, b as B, c\n"), "from math import a, b as B, c\n");
    // Self-alias is normalized away by the parser (alias == name → no alias)
    EXPECT_EQ(fmt("from math import add as add\n"), "from math import add\n");
    // Qualified import (#1723): `import math` round-trips verbatim.
    EXPECT_EQ(fmt("import math\n"), "import math\n");
    // Qualified call site preserves the `module.callee(...)` prefix.
    EXPECT_EQ(fmt("import math\nx = math.sqrt(2.0)\n"),
              "import math\nx = math.sqrt(2.0)\n");
    // Qualified call as a bare statement (#1723): formatter must not rewrite
    // it into UFCS form (`sqrt(math, 3.0)`).
    EXPECT_EQ(fmt("import math\nmath.sqrt(3.0)\n"),
              "import math\nmath.sqrt(3.0)\n");
    // Qualified const access via `import xxx`.
    EXPECT_EQ(fmt("import math\nx = math.PI\n"),
              "import math\nx = math.PI\n");
    // Qualified import alias (#1724): `import math as m` round-trips verbatim.
    EXPECT_EQ(fmt("import math as m\n"), "import math as m\n");
    // Alias propagates through qualified call sites.
    EXPECT_EQ(fmt("import math as m\nx = m.sqrt(2.0)\n"),
              "import math as m\nx = m.sqrt(2.0)\n");
    // Alias propagates through qualified const access.
    EXPECT_EQ(fmt("import math as m\nx = m.PI\n"),
              "import math as m\nx = m.PI\n");
    // #2423: `import ry.<mod>` canonical form must be preserved by fmt.
    EXPECT_EQ(fmt("import ry.math\n"), "import ry.math\n");
    EXPECT_EQ(fmt("import ry.math as m\n"), "import ry.math as m\n");
    EXPECT_EQ(fmt("import ry.json5\n"), "import ry.json5\n");
    EXPECT_EQ(fmt("from ry.net import open as net_open\n"),
              "from ry.net import open as net_open\n");
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
    // Enum with named associated-data fields (#2424 side fix)
    {
        auto src = "enum Shape:\n    Circle(radius: float)\n    Rect(width: float, height: float)\n";
        auto expected = "enum Shape:\n  Circle(radius: float)\n  Rect(width: float, height: float)\n";
        EXPECT_EQ(fmt(src), expected);
    }
}

// ===== Enum explicit discriminant value preservation (#2424) =====
//
// `ry fmt` previously stripped explicit integer discriminants from enum
// variants (e.g. `Ok = 200` became `Ok`), corrupting any code that relied on
// the value for ABI, wire format, or debug output purposes.

TEST(Formatter, EnumExplicitDiscriminantValues) {
    // Positive integer discriminants
    {
        auto src = "enum Status:\n    Ok = 200\n    NotFound = 404\n    ServerError = 500\n";
        auto expected = "enum Status:\n  Ok = 200\n  NotFound = 404\n  ServerError = 500\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Negative and zero discriminants
    {
        auto src = "enum Signed:\n    A = -10\n    B = 0\n    C = 40\n";
        auto expected = "enum Signed:\n  A = -10\n  B = 0\n  C = 40\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // i64 boundary values round-trip without overflow truncation
    {
        auto src = "enum Bounds:\n    Lo = -9223372036854775808\n    Hi = 9223372036854775807\n";
        auto expected = "enum Bounds:\n  Lo = -9223372036854775808\n  Hi = 9223372036854775807\n";
        EXPECT_EQ(fmt(src), expected);
    }
    // Format output is idempotent: formatting twice yields the same source.
    {
        auto src = "enum Status:\n    Ok = 200\n    NotFound = 404\n";
        auto once = fmt(src);
        auto twice = fmt(once);
        EXPECT_EQ(once, twice);
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

// #1844: @doc("...") with a single-line string round-trips and is idempotent.
TEST(FormatterTest, DocDirectiveSingleLineRoundTrip) {
    auto src = "@doc(\"Returns the absolute value of x.\")\n"
               "fn abs(x: int) -> int:\n"
               "  return x\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(first, reason)) << reason;
}

// #1844: @doc("""...""") with a triple-quoted block string round-trips and
// is idempotent. The formatter routes block strings through formatBlockString
// so the multi-line form is preserved.
TEST(FormatterTest, DocDirectiveBlockStringRoundTrip) {
    auto src = "@doc(\"\"\"\n"
               "Returns the absolute value of x.\n"
               "\n"
               "## Examples\n"
               "\"\"\")\n"
               "fn abs(x: int) -> int:\n"
               "  return x\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(Formatter::formatSource(first), first);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(first, reason)) << reason;
}

// @doc on a record and on record fields preserves placement through fmt.
TEST(FormatterTest, DocDirectiveOnRecordRoundTrip) {
    auto src = "@doc(\"A 2D point.\")\n"
               "record Point:\n"
               "  @doc(\"X coordinate.\")\n"
               "  x: int\n"
               "  @doc(\"Y coordinate.\")\n"
               "  y: int\n";
    auto first = Formatter::formatSource(src);
    EXPECT_EQ(first, src);
    EXPECT_EQ(Formatter::formatSource(first), first);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(first, reason)) << reason;
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

TEST(FormatterTest, ExpectToBeBetweenRoundTrip) {
    auto src = "expect(x).toBeBetween(1, 10)\n";
    EXPECT_EQ(fmt(src), src);
    EXPECT_EQ(fmt(fmt(src)), fmt(src));
}

TEST(FormatterTest, ExpectToBeOneOfRoundTrip) {
    auto src = "expect(x).toBeOneOf([200, 201, 204])\n";
    EXPECT_EQ(fmt(src), src);
    EXPECT_EQ(fmt(fmt(src)), fmt(src));
}

TEST(FormatterTest, ExpectToBeCloseToWithDecimalsRoundTrip) {
    auto src = "expect(x).toBeCloseTo(1.0, 4)\n";
    EXPECT_EQ(fmt(src), src);
    EXPECT_EQ(fmt(fmt(src)), fmt(src));
}

TEST(FormatterTest, CaseExprBlockArmRoundTrip) {
    // Block-form subject case arm (#1891): intermediate statement + tail
    // expression, with the inline `_` arm preserved.
    auto src = "r = case x:\n    1:\n        tmp = x + 10\n        tmp * 2\n    _ : 0\n";
    auto expected = "r = case x:\n  1:\n    tmp = x + 10\n    tmp * 2\n  _ : 0\n";
    EXPECT_EQ(fmt(src), expected);
    EXPECT_EQ(fmt(fmt(src)), fmt(src)); // idempotent
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(fmt(src), reason)); // re-parses
}

TEST(FormatterTest, CaseExprTailOnlyBlockCanonicalizesToInline) {
    // A block arm with no intermediate statements canonicalizes to inline form.
    auto src = "b = case x:\n    1:\n        100\n    _ : 0\n";
    EXPECT_EQ(fmt(src), "b = case x:\n  1 : 100\n  _ : 0\n");
    EXPECT_EQ(fmt(fmt(src)), fmt(src));
}

TEST(FormatterTest, CaseCondExprBlockArmRoundTrip) {
    // Block-form condition arm + inline else (#1891).
    auto src = "s = case:\n    x > 10:\n        big = x + 1\n        big * 2\n    _ : 0\n";
    auto expected = "s = case:\n  x > 10:\n    big = x + 1\n    big * 2\n  _ : 0\n";
    EXPECT_EQ(fmt(src), expected);
    EXPECT_EQ(fmt(fmt(src)), fmt(src));
}

TEST(FormatterTest, OptionPropagateAfterSafeIndexAdjacency) {
    // #2114: ErrorPropagateExpr wrapping IndexExpr{try_mode} must keep a space
    // between the two `?` tokens; otherwise they fuse into the `??` null-coalesce
    // token and re-parse fails.
    auto src = "fn first(values: List<int>) -> Option<int>:\n"
               "    value = values[0]? ?\n"
               "    return Some(value)\n";
    auto formatted = fmt(src);
    EXPECT_NE(formatted.find("values[0]? ?"), std::string::npos)
        << "expected space-separated `? ?`, got: " << formatted;
    EXPECT_EQ(formatted.find("values[0]??"), std::string::npos)
        << "fused `??` would be the null-coalesce token, not two postfix `?`: " << formatted;
    EXPECT_EQ(fmt(formatted), formatted);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(formatted, reason)) << reason;
}

TEST(FormatterTest, OptionPropagateAfterPropagateAdjacency) {
    // #2114: nested ErrorPropagateExpr (no IndexExpr involved) — same fusion risk.
    // This case forces the general fix over an IndexExpr-only check.
    auto src = "fn first(values: List<int>) -> Option<int>:\n"
               "    value = safeGet(values, 0)? ?\n"
               "    return Some(value)\n";
    auto formatted = fmt(src);
    EXPECT_NE(formatted.find("safeGet(values, 0)? ?"), std::string::npos)
        << "expected space-separated `? ?`, got: " << formatted;
    EXPECT_EQ(formatted.find("safeGet(values, 0)??"), std::string::npos)
        << "fused `??` would be the null-coalesce token, not two postfix `?`: " << formatted;
    EXPECT_EQ(fmt(formatted), formatted);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(formatted, reason)) << reason;
}

// ===== Multi-line inline lambda in call-expression contexts (#2425) =====
//
// Regression guard: fmt must round-trip and stay idempotent when a multi-line
// block-form lambda appears as an argument inside a CallExpr value
// (AssignStmt / ReturnStmt / IndexAssignStmt / FieldAssignStmt) -- and when
// the call is nested inside another call. Previously, formatExpr for the
// LambdaExpr branch emitted the body directly to out_ and returned an empty
// string, so the surrounding assignment was reassembled with the body spliced
// before the callee. The formatted file failed to re-parse, so `ry fmt`
// silently skipped any file using this pattern (tests/spec/thread.test.ry).
TEST(Formatter, MultilineInlineLambdaInAssignRoundTrip) {
    std::string src =
        "fn main():\n"
        "  t = threadSpawn(():\n"
        "    atomicIntAdd(counter, 1)\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(out, reason)) << reason;
}

TEST(Formatter, MultilineInlineLambdaInReturnRoundTrip) {
    std::string src =
        "fn main() -> Thread:\n"
        "  return threadSpawn(():\n"
        "    atomicIntAdd(counter, 1)\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaInIndexAssignRoundTrip) {
    std::string src =
        "fn main():\n"
        "  arr[0] = threadSpawn(():\n"
        "    work()\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaInFieldAssignRoundTrip) {
    std::string src =
        "fn main():\n"
        "  obj.f = threadSpawn(():\n"
        "    work()\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaNestedCallRoundTrip) {
    std::string src =
        "fn main():\n"
        "  x = outer(inner(():\n"
        "    work()\n"
        "  ))\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaWithLeadingArgs) {
    std::string src =
        "fn main():\n"
        "  t = call(1, 2, ():\n"
        "    work()\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaWithParamsAndReturnType) {
    std::string src =
        "fn main():\n"
        "  t = run((x: int) -> int:\n"
        "    return x * 2\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaInNestedBlock) {
    std::string src =
        "fn main():\n"
        "  if cond:\n"
        "    t = threadSpawn(():\n"
        "      work()\n"
        "    )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

// Non-trailing multi-line lambda: lambda at args[0] with positional arg after.
// The output must put the continuation `, 3)` on a line indented to the call's
// indent so the parser still accepts it. Pre-fix this was emitted at column 0
// and `ry fmt` skipped the file. (#2425)
TEST(Formatter, MultilineInlineLambdaNonTrailingPositionAsStmt) {
    std::string src =
        "fn main():\n"
        "  callTwo(():\n"
        "    work(1)\n"
        "  , 3)\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
    std::string reason;
    EXPECT_TRUE(Formatter::verifyFormatting(out, reason)) << reason;
}

TEST(Formatter, MultilineInlineLambdaNonTrailingPositionInAssign) {
    std::string src =
        "fn main():\n"
        "  t = callTwo(():\n"
        "    work(1)\n"
        "  , 3)\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

TEST(Formatter, MultilineInlineLambdaInTupleDestructRoundTrip) {
    std::string src =
        "fn main():\n"
        "  (a, b) = callTwo(():\n"
        "    work()\n"
        "  , 3)\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}

// Bare CallStmt (no enclosing assignment / return / destructure) with a
// trailing multi-line lambda. This path used to be served by the old
// `formatCall` trailing branch; the refactor reroutes it through the shared
// `emitCallTrailingLambda` helper, so guard the equivalence explicitly.
TEST(Formatter, MultilineInlineLambdaInBareCallStmtRoundTrip) {
    std::string src =
        "fn main():\n"
        "  threadSpawn(():\n"
        "    work()\n"
        "  )\n";
    auto out = fmt(src);
    EXPECT_EQ(out, src);
    EXPECT_EQ(fmt(out), out);
}
