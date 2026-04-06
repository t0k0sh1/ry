#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// Missing return — should produce compile error
// ============================================================

TEST_F(CodeGenTest, MissingReturnInt) {
    EXPECT_THROW(runSource(
        "function foo() -> int:\n"
        "    x = 1\n"
        "foo()\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnNoElse) {
    EXPECT_THROW(runSource(
        "function foo(x: int) -> int:\n"
        "    if x > 0:\n"
        "        return 1\n"
        "foo(1)\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnInElseBranch) {
    EXPECT_THROW(runSource(
        "function foo(x: int) -> int:\n"
        "    if x > 0:\n"
        "        return 1\n"
        "    else:\n"
        "        x = 0\n"
        "foo(1)\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnStr) {
    EXPECT_THROW(runSource(
        "function greet() -> str:\n"
        "    x = \"hello\"\n"
        "greet()\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnFloat) {
    EXPECT_THROW(runSource(
        "function pi() -> float:\n"
        "    x = 3.14\n"
        "pi()\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnBool) {
    EXPECT_THROW(runSource(
        "function check() -> bool:\n"
        "    x = true\n"
        "check()\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MissingReturnLambdaExplicitType) {
    EXPECT_THROW(runSource(
        "f = () -> int:\n"
        "    x = 1\n"
        "f()\n"
    ), std::runtime_error);
}

// ============================================================
// Valid returns — should compile and run fine
// ============================================================

TEST_F(CodeGenTest, AllPathsReturnIfElse) {
    EXPECT_EQ(runSource(
        "function foo(x: int) -> int:\n"
        "    if x > 0:\n"
        "        return 1\n"
        "    else:\n"
        "        return 0\n"
        "print(foo(5))\n"
    ), "1\n");
}

TEST_F(CodeGenTest, AllPathsReturnWhenMultiBranch) {
    EXPECT_EQ(runSource(
        "function grade(x: int) -> str:\n"
        "    when:\n"
        "        x >= 90:\n"
        "            return \"A\"\n"
        "        x >= 80:\n"
        "            return \"B\"\n"
        "        else:\n"
        "            return \"C\"\n"
        "print(grade(95))\n"
    ), "A\n");
}

TEST_F(CodeGenTest, AllPathsReturnMatchWildcard) {
    EXPECT_EQ(runSource(
        "function describe(x: int) -> str:\n"
        "    match x:\n"
        "        case 1:\n"
        "            return \"one\"\n"
        "        case _:\n"
        "            return \"other\"\n"
        "print(describe(1))\n"
    ), "one\n");
}

TEST_F(CodeGenTest, AllPathsReturnMatchOkErr) {
    EXPECT_EQ(runSource(
        "function check(x: int) -> str:\n"
        "    r: Result<int, Error> = Ok(x)\n"
        "    match r:\n"
        "        case Ok(v):\n"
        "            return \"ok\"\n"
        "        case Err(e):\n"
        "            return \"err\"\n"
        "print(check(42))\n"
    ), "ok\n");
}

TEST_F(CodeGenTest, AllPathsReturnMatchSomeNone) {
    EXPECT_EQ(runSource(
        "function check(x: Option<int>) -> str:\n"
        "    match x:\n"
        "        case Some(v):\n"
        "            return \"some\"\n"
        "        case None:\n"
        "            return \"none\"\n"
        "print(check(Some(1)))\n"
    ), "some\n");
}

TEST_F(CodeGenTest, UnitFunctionNoReturnOk) {
    EXPECT_EQ(runSource(
        "function greet() -> Unit:\n"
        "    print(\"hello\")\n"
        "greet()\n"
    ), "hello\n");
}

TEST_F(CodeGenTest, AnyFunctionNoReturnOk) {
    EXPECT_NO_THROW(runSource(
        "function foo() -> any:\n"
        "    x = 1\n"
        "foo()\n"
    ));
}

TEST_F(CodeGenTest, OmittedReturnTypeNoReturnOk) {
    EXPECT_NO_THROW(runSource(
        "function foo():\n"
        "    x = 1\n"
        "foo()\n"
    ));
}

TEST_F(CodeGenTest, AllPathsReturnMatchVariable) {
    EXPECT_EQ(runSource(
        "function describe(x: int) -> str:\n"
        "    match x:\n"
        "        case 1:\n"
        "            return \"one\"\n"
        "        case n:\n"
        "            return \"other\"\n"
        "print(describe(1))\n"
    ), "one\n");
}

TEST_F(CodeGenTest, NestedIfElseAllReturn) {
    EXPECT_EQ(runSource(
        "function classify(x: int) -> str:\n"
        "    if x > 0:\n"
        "        if x > 100:\n"
        "            return \"big\"\n"
        "        else:\n"
        "            return \"small\"\n"
        "    else:\n"
        "        return \"negative\"\n"
        "print(classify(50))\n"
    ), "small\n");
}

// ============================================================
// Exhaustive enum match — should compile and run (#513)
// ============================================================

TEST_F(CodeGenTest, AllPathsReturnMatchEnumExhaustive) {
    EXPECT_EQ(runSource(
        "enum Shape:\n"
        "    Circle(float)\n"
        "    Rect(float, float)\n"
        "\n"
        "function area(s: Shape) -> float:\n"
        "    match s:\n"
        "        case Shape::Circle(r):\n"
        "            return 3.14 * r * r\n"
        "        case Shape::Rect(w, h):\n"
        "            return w * h\n"
        "print(area(Shape::Circle(5.0)))\n"
    ), "78.5\n");
}

TEST_F(CodeGenTest, AllPathsReturnMatchSimpleEnumExhaustive) {
    EXPECT_EQ(runSource(
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "\n"
        "function name(c: Color) -> str:\n"
        "    match c:\n"
        "        case Color::Red:\n"
        "            return \"red\"\n"
        "        case Color::Green:\n"
        "            return \"green\"\n"
        "        case Color::Blue:\n"
        "            return \"blue\"\n"
        "print(name(Color::Red))\n"
    ), "red\n");
}

TEST_F(CodeGenTest, MissingReturnMatchEnumNotExhaustive) {
    EXPECT_THROW(runSource(
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n"
        "\n"
        "function name(c: Color) -> str:\n"
        "    match c:\n"
        "        case Color::Red:\n"
        "            return \"red\"\n"
        "        case Color::Green:\n"
        "            return \"green\"\n"
        "name(Color::Red)\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, AllPathsReturnMatchBoolExhaustive) {
    EXPECT_EQ(runSource(
        "function describe(b: bool) -> str:\n"
        "    match b:\n"
        "        case true:\n"
        "            return \"yes\"\n"
        "        case false:\n"
        "            return \"no\"\n"
        "print(describe(true))\n"
    ), "yes\n");
}
