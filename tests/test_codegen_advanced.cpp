// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"


using namespace ry;
// ===== [contract] Break / Continue =====

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
    std::string src = withStdlibDirectiveDecls(
        "@parallel\n"
        "for i in range(5):\n"
        "    if i == 2:\n"
        "        break\n");
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ParallelForContinueRejected) {
    std::string src = withStdlibDirectiveDecls(
        "@parallel\n"
        "for i in range(5):\n"
        "    if i == 2:\n"
        "        continue\n");
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Struct field assignment =====

TEST_F(CodeGenTest, RecordFieldAssign) {
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

TEST_F(CodeGenTest, RecordFieldAssignMultiple) {
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

TEST_F(CodeGenTest, RecordFieldAssignConstError) {
    std::string src = withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "@const\n"
        "p = Point(1, 2)\n"
        "p.x = 10");
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Escape sequences =====

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

// ===== [contract] range() as expression =====

TEST_F(CodeGenTest, RangeEmptyList) {
    std::string src =
        "xs = range(0)\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, RangeUsedAsList) {
    std::string src =
        "xs = range(3)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "0\n1\n2\n3\n");
}

// ===== [contract] Lambda (arrow function) tests =====

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
        "fn apply(f: fn(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "doubled = apply((n: int) => n * 2, 10)\n"
        "print(doubled)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, LambdaStoredAndPassed) {
    std::string src =
        "fn apply(f: fn(int) -> int, x: int) -> int:\n"
        "    return f(x)\n"
        "triple = (n: int) => n * 3\n"
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
        "isPositive = (x: int) => x > 0\n"
        "print(isPositive(5))\n"
        "print(isPositive(-3))";
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

TEST_F(CodeGenTest, LambdaUnitReturnSingleExpr) {
    // #2421: a single-expression lambda with an explicit `-> Unit` return type
    // (`=> expr` where expr is Unit-typed) must compile and run. The body is
    // evaluated for its side effects and Unit is returned; a void-returning
    // body yields a null Value that previously crashed codegen (the single-expr
    // path dereferenced it / fed it to CreateRet, unlike the block-body path).
    std::string src =
        "fn emit(x: int) -> Unit:\n"
        "    print(x)\n"
        "run = (n: int) -> Unit => emit(n)\n"
        "run(7)";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, LambdaUnitReturnSingleExprAsArgument) {
    // #2421: the same single-expression `-> Unit` lambda passed as an argument
    // and invoked through the closure-call path (the http.listen portCallback
    // shape: `(p: int) -> Unit => store(p)`).
    std::string src =
        "fn emit(x: int) -> Unit:\n"
        "    print(x)\n"
        "fn apply(f: fn(int) -> Unit, v: int) -> Unit:\n"
        "    f(v)\n"
        "apply((n: int) -> Unit => emit(n), 9)";
    EXPECT_EQ(runSource(src), "9\n");
}

TEST_F(CodeGenTest, LambdaUnitReturnSingleExprNonUnitBodyRejected) {
    // #2421: a single-expression `-> Unit` lambda whose body produces a real
    // (non-Unit) value is a type error — the value cannot be returned from a
    // Unit lambda, mirroring `return <expr>` in a Unit function. Only Unit-
    // typed bodies (side-effecting calls) are accepted on this path.
    std::string src =
        "f = (x: int) -> Unit => x + 1\n"
        "f(5)";
    EXPECT_THROW(runSource(src), std::runtime_error);
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

// ===== [contract] Effectively final capture tests =====

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
    std::string src = withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "\n"
        "@const p = Point(1)\n"
        "f = ():\n"
        "    p.x = 99\n"
        "\n"
        "f()");
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Set テスト =====

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
        "print(len(s))";
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
        "print(len(s))";
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
        "print(len(s))\n"
        "s.add(42)\n"
        "print(42 in s)";
    EXPECT_EQ(runSource(src), "0\ntrue\n");
}

TEST_F(CodeGenTest, SetInFunction) {
    std::string src =
        "fn hasElement(s: Set<int>, x: int) -> bool:\n"
        "    return x in s\n"
        "s = {10, 20, 30}\n"
        "print(hasElement(s, 20))\n"
        "print(hasElement(s, 40))";
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

// ===== [contract] Enum テスト =====

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
        "fn isRed(c: Color) -> bool:\n"
        "    return c == Color::Red\n"
        "print(isRed(Color::Red))\n"
        "print(isRed(Color::Blue))";
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

// ===== [contract] Match statement tests =====

TEST_F(CodeGenTest, MatchEnumAllVariants) {
    std::string src =
        "enum Color:\n    Red\n    Green\n    Blue\nc = Color::Green\ncase c:\n    Color::Red:\n        print(\"red\")\n    Color::Green:\n        print(\"green\")\n    Color::Blue:\n        print(\"blue\")\n";
    EXPECT_EQ(runSource(src), "green\n");
}

TEST_F(CodeGenTest, MatchEnumWithWildcard) {
    std::string src =
        "enum Color:\n    Red\n    Green\n    Blue\nc = Color::Blue\ncase c:\n    Color::Red:\n        print(\"red\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "other\n");
}

TEST_F(CodeGenTest, MatchOptionSomeNone) {
    std::string src =
        "x: Option<int> = Some(42)\ncase x:\n    Some(v):\n        print(v)\n    None:\n        print(\"nothing\")\ny: Option<int> = None\ncase y:\n    Some(v):\n        print(v)\n    None:\n        print(\"nothing\")\n";
    EXPECT_EQ(runSource(src), "42\nnothing\n");
}

TEST_F(CodeGenTest, MatchIntLiteral) {
    std::string src =
        "x = 2\ncase x:\n    0:\n        print(\"zero\")\n    1:\n        print(\"one\")\n    2:\n        print(\"two\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "two\n");
}

TEST_F(CodeGenTest, MatchStringLiteral) {
    std::string src =
        "s = \"hello\"\ncase s:\n    \"world\":\n        print(\"world\")\n    \"hello\":\n        print(\"hello!\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "hello!\n");
}

TEST_F(CodeGenTest, MatchBoolLiteral) {
    std::string src =
        "b = true\ncase b:\n    true:\n        print(\"yes\")\n    false:\n        print(\"no\")\n";
    EXPECT_EQ(runSource(src), "yes\n");
}

TEST_F(CodeGenTest, MatchVariableBinding) {
    std::string src =
        "x = 42\ncase x:\n    n:\n        print(n)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, MatchGuard) {
    std::string src =
        "x = 5\ncase x:\n    n if n > 0:\n        print(\"positive\")\n    n if n < 0:\n        print(\"negative\")\n    _:\n        print(\"zero\")\n";
    EXPECT_EQ(runSource(src), "positive\n");
}

TEST_F(CodeGenTest, MatchGuardZero) {
    std::string src =
        "x = 0\ncase x:\n    n if n > 0:\n        print(\"positive\")\n    n if n < 0:\n        print(\"negative\")\n    _:\n        print(\"zero\")\n";
    EXPECT_EQ(runSource(src), "zero\n");
}

TEST_F(CodeGenTest, MatchNonExhaustiveEnum) {
    std::string src =
        "enum Color:\n    Red\n    Green\n    Blue\nc = Color::Red\ncase c:\n    Color::Red:\n        print(\"red\")\n    Color::Green:\n        print(\"green\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveOption) {
    std::string src =
        "x: Option<int> = Some(1)\ncase x:\n    Some(v):\n        print(v)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveBool) {
    std::string src =
        "b = true\ncase b:\n    true:\n        print(\"yes\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNonExhaustiveLiteral) {
    std::string src =
        "x = 1\ncase x:\n    0:\n        print(\"zero\")\n    1:\n        print(\"one\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, MatchNested) {
    std::string src =
        "enum Dir:\n    Up\n    Down\nd = Dir::Up\nif true:\n    case d:\n        Dir::Up:\n            print(\"up\")\n        Dir::Down:\n            print(\"down\")\n";
    EXPECT_EQ(runSource(src), "up\n");
}

TEST_F(CodeGenTest, MatchNegativeLiteral) {
    std::string src =
        "x = -1\ncase x:\n    -1:\n        print(\"neg one\")\n    0:\n        print(\"zero\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "neg one\n");
}

// ===== [contract] Union 型テスト =====

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
        "fn show(x: int | str) -> int:\n"
        "    print(x)\n"
        "    return 0\n"
        "show(42)\n"
        "show(\"hi\")\n";
    EXPECT_EQ(runSource(src), "42\nhi\n");
}

TEST_F(CodeGenTest, UnionFnReturn) {
    std::string src =
        "fn getVal(flag: bool) -> int | str:\n"
        "    if flag:\n"
        "        return 42\n"
        "    return \"hello\"\n"
        "a = getVal(true)\n"
        "print(a)\n"
        "b = getVal(false)\n"
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

// ===== [contract] not in テスト =====

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

// ===== [contract] f-string tests =====

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

// ===== [contract] as cast tests =====

TEST_F(CodeGenTest, AsCastIntToFloat) {
    EXPECT_EQ(runSource("x = 42 as float\nprint(x)"), "42.0\n");
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

// ===== [contract] Error type tests =====

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

// ===== [contract] Result type tests =====

TEST_F(CodeGenTest, ResultOkMatch) {
    std::string src = R"(
fn readFile(path: str) -> Result<str, Error>:
    if path == "":
        return Err(Error("empty path"))
    return Ok("content")

res = readFile("test.txt")
case res:
    Ok(data):
        print(data)
    Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "content\n");
}

TEST_F(CodeGenTest, ResultErrMatch) {
    std::string src = R"(
fn readFile(path: str) -> Result<str, Error>:
    if path == "":
        return Err(Error("empty path"))
    return Ok("content")

res = readFile("")
case res:
    Ok(data):
        print(data)
    Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "empty path\n");
}

TEST_F(CodeGenTest, ResultDivideOk) {
    std::string src = R"(
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)
    Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, ResultDivideErr) {
    std::string src = R"(
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 0):
    Ok(v):
        print(v)
    Err(e):
        print(e.message)
)";
    EXPECT_EQ(runSource(src), "division by zero\n");
}

// ===== [contract] when OR pattern =====

TEST_F(CodeGenTest, MatchOrPatternInt) {
    std::string src =
        "x = 2\ncase x:\n    1 | 2 | 3:\n        print(\"small\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "small\n");
}

TEST_F(CodeGenTest, MatchOrPatternIntNoMatch) {
    std::string src =
        "x = 5\ncase x:\n    1 | 2 | 3:\n        print(\"small\")\n    _:\n        print(\"other\")\n";
    EXPECT_EQ(runSource(src), "other\n");
}

TEST_F(CodeGenTest, MatchOrPatternEnum) {
    std::string src =
        "enum Color:\n    Red\n    Green\n    Blue\nc = Color::Red\ncase c:\n    Color::Red | Color::Blue:\n        print(\"rb\")\n    Color::Green:\n        print(\"g\")\n";
    EXPECT_EQ(runSource(src), "rb\n");
}

TEST_F(CodeGenTest, MatchOrPatternExhaustive) {
    std::string src =
        "enum Color:\n    Red\n    Green\n    Blue\nc = Color::Green\ncase c:\n    Color::Red | Color::Blue:\n        print(\"rb\")\n    Color::Green:\n        print(\"g\")\n";
    EXPECT_EQ(runSource(src), "g\n");
}

TEST_F(CodeGenTest, MatchOrPatternVariableError) {
    std::string src =
        "x = 1\ncase x:\n    a | b:\n        print(a)\n    _:\n        print(\"other\")\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Tuple patterns in case (#834) =====

TEST_F(CodeGenTest, TuplePatternArityMismatch) {
    // (int, int) subject matched with a 3-element pattern -> codegen error
    std::string src =
        "t: (int, int) = (1, 2)\n"
        "case t:\n"
        "    (a, b, c):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TuplePatternNonTupleSubject) {
    // scalar subject with a tuple pattern -> codegen error
    std::string src =
        "n: int = 1\n"
        "case n:\n"
        "    (a, b):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Record Pattern =====

TEST_F(CodeGenTest, RecordPatternArityMismatch) {
    // Point has 2 fields; pattern uses 3 -> codegen error
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "case p:\n"
        "    Point(a, b, c):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RecordPatternUnknownTypeName) {
    // NotAType is not a known record -> codegen error
    std::string src =
        "x = 42\n"
        "case x:\n"
        "    NotAType(a, b):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RecordPatternAppliedToEnum) {
    // Color is an enum, not a record -> codegen error
    std::string src =
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "c = Color::Red\n"
        "case c:\n"
        "    Color(a, b):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RecordPatternWrongSubjectType) {
    // Point(a, b) arm against an int subject -> codegen error
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "n: int = 42\n"
        "case n:\n"
        "    Point(a, b):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RecordPatternTypeAlias) {
    // type Pt = Point; case p: Pt(a, b): ... should resolve through the alias and bind fields
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "type Pt = Point\n"
        "p = Point(5, 6)\n"
        "case p:\n"
        "    Pt(a, b):\n"
        "        print(a)\n"
        "        print(b)\n"
        "    _:\n"
        "        print(-1)\n";
    EXPECT_EQ(runSource(src), "5\n6\n");
}

TEST_F(CodeGenTest, RecordPatternSubjectTypeMismatch) {
    // Pt(a, b) against a subject of aliased-but-different record type -> codegen error
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "record Other:\n"
        "    v: int\n"
        "    w: int\n"
        "type Pt = Point\n"
        "type Ot = Other\n"
        "o: Ot = Other(1, 2)\n"
        "case o:\n"
        "    Pt(a, b):\n"
        "        print(a)\n"
        "    _:\n"
        "        print(0)\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] enum ADT (associated data) =====

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
        "enum Shape:\n    Circle(float)\n    Rectangle(float, float)\n    Point\ns = Shape::Circle(3.14)\ncase s:\n    Shape::Circle(r):\n        print(r)\n    Shape::Rectangle(w, h):\n        print(w)\n    Shape::Point:\n        print(\"point\")";
    EXPECT_EQ(runSource(src), "3.14\n");
}

TEST_F(CodeGenTest, EnumADTMatchRect) {
    std::string src =
        "enum Shape:\n    Circle(float)\n    Rectangle(float, float)\n    Point\ns = Shape::Rectangle(3.0, 4.0)\ncase s:\n    Shape::Circle(r):\n        print(r)\n    Shape::Rectangle(w, h):\n        print(w + h)\n    Shape::Point:\n        print(\"point\")";
    EXPECT_EQ(runSource(src), "7.0\n");
}

TEST_F(CodeGenTest, EnumADTMixed) {
    std::string src =
        "enum Shape:\n    Circle(float)\n    Rectangle(float, float)\n    Point\ns = Shape::Point\ncase s:\n    Shape::Circle(r):\n        print(r)\n    Shape::Rectangle(w, h):\n        print(w)\n    Shape::Point:\n        print(\"point\")";
    EXPECT_EQ(runSource(src), "point\n");
}

TEST_F(CodeGenTest, EnumADTSingleField) {
    std::string src =
        "enum Wrapper:\n    IntVal(int)\n    StrVal(str)\nw = Wrapper::IntVal(42)\ncase w:\n    Wrapper::IntVal(v):\n        print(v)\n    Wrapper::StrVal(s):\n        print(s)";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== [contract] ADT enum equality (payload-aware) =====

static const char *kShapeEnum =
    "enum Shape:\n"
    "    Circle(float)\n"
    "    Rectangle(float, float)\n"
    "    Point\n";

// Same tag + same primitive payload → equal
TEST_F(CodeGenTest, EnumADTEqSamePayload) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Circle(1.0) == Shape::Circle(1.0))"), "true\n");
}

// Same tag + different primitive payload → not equal (#959 regression)
TEST_F(CodeGenTest, EnumADTEqDifferentPayload) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Circle(1.0) == Shape::Circle(2.0))"), "false\n");
}

// Different tag → not equal (tag-only regression guard)
TEST_F(CodeGenTest, EnumADTEqDifferentTag) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Circle(1.0) == Shape::Point)"), "false\n");
}

// No-payload variant: tag equality is sufficient
TEST_F(CodeGenTest, EnumADTEqNoPayload) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Point == Shape::Point)\n"
        "print(Shape::Point != Shape::Point)"),
        "true\nfalse\n");
}

// Multi-field variant: field-by-field
TEST_F(CodeGenTest, EnumADTEqMultiField) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Rectangle(1.0, 2.0) == Shape::Rectangle(1.0, 2.0))\n"
        "print(Shape::Rectangle(1.0, 2.0) == Shape::Rectangle(1.0, 3.0))"),
        "true\nfalse\n");
}

// != operator
TEST_F(CodeGenTest, EnumADTNeOperator) {
    EXPECT_EQ(runSource(std::string(kShapeEnum) +
        "print(Shape::Circle(1.0) != Shape::Circle(2.0))\n"
        "print(Shape::Circle(1.0) != Shape::Circle(1.0))"),
        "true\nfalse\n");
}

// Pointer-typed payload (str): exercises propagateTypeMeta path
TEST_F(CodeGenTest, EnumADTEqStrPayload) {
    EXPECT_EQ(runSource(
        "enum Wrapper:\n"
        "    IntVal(int)\n"
        "    StrVal(str)\n"
        "print(Wrapper::StrVal(\"hello\") == Wrapper::StrVal(\"hello\"))\n"
        "print(Wrapper::StrVal(\"hello\") == Wrapper::StrVal(\"world\"))"),
        "true\nfalse\n");
}

// Generic ADT: same payload → equal, different payload → not equal
TEST_F(CodeGenTest, GenericEnumADTEqSame) {
    EXPECT_EQ(runSource(
        "enum MyOpt<T>:\n"
        "    Some(T)\n"
        "    None\n"
        "print(MyOpt<int>::Some(42) == MyOpt<int>::Some(42))\n"
        "print(MyOpt<int>::Some(1)  == MyOpt<int>::Some(2))"),
        "true\nfalse\n");
}

// List<int>-typed payload: exercises propagateTypeMeta for collection fields
TEST_F(CodeGenTest, EnumADTEqListPayload) {
    EXPECT_EQ(runSource(
        "enum Bag:\n"
        "    Items(List<int>)\n"
        "    Empty\n"
        "a = Bag::Items([1, 2, 3])\n"
        "b = Bag::Items([1, 2, 3])\n"
        "c = Bag::Items([1, 2, 4])\n"
        "print(a == b)\n"
        "print(a == c)"),
        "true\nfalse\n");
}

// Nested ADT payload: outer variant carries an inner ADT enum as its payload,
// so emitComparisonOp is called recursively for the inner enum.
TEST_F(CodeGenTest, EnumADTEqNestedPayload) {
    EXPECT_EQ(runSource(
        "enum Inner:\n"
        "    Val(int)\n"
        "    Empty\n"
        "enum Outer:\n"
        "    Wrap(Inner)\n"
        "    None\n"
        "print(Outer::Wrap(Inner::Val(1)) == Outer::Wrap(Inner::Val(1)))\n"
        "print(Outer::Wrap(Inner::Val(1)) == Outer::Wrap(Inner::Val(2)))\n"
        "print(Outer::Wrap(Inner::Empty)  == Outer::Wrap(Inner::Val(1)))\n"
        "print(Outer::None == Outer::None)"),
        "true\nfalse\nfalse\ntrue\n");
}

// ===== [contract] Generic enum =====

TEST_F(CodeGenTest, GenericEnumBasic) {
    std::string src =
        "enum MyOption<T>:\n    MySome(T)\n    MyNone\nx = MyOption<int>::MySome(42)\ncase x:\n    MyOption::MySome(v):\n        print(v)\n    MyOption::MyNone:\n        print(\"none\")";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, GenericEnumNone) {
    std::string src =
        "enum MyOption<T>:\n    MySome(T)\n    MyNone\nx = MyOption<int>::MyNone\ncase x:\n    MyOption::MySome(v):\n        print(v)\n    MyOption::MyNone:\n        print(\"none\")";
    EXPECT_EQ(runSource(src), "none\n");
}

TEST_F(CodeGenTest, GenericEnumFloat) {
    std::string src =
        "enum MyOption<T>:\n    MySome(T)\n    MyNone\nx = MyOption<float>::MySome(3.14)\ncase x:\n    MyOption::MySome(v):\n        print(v)\n    MyOption::MyNone:\n        print(\"none\")";
    EXPECT_EQ(runSource(src), "3.14\n");
}

// ===== [contract] Explicit enum values =====

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
        "enum HttpStatus:\n    Ok = 200\n    NotFound = 404\n    InternalError = 500\ns = HttpStatus::InternalError\ncase s:\n    HttpStatus::Ok:\n        print(\"ok\")\n    HttpStatus::NotFound:\n        print(\"not found\")\n    HttpStatus::InternalError:\n        print(\"error\")";
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

// ===== [contract] r-string =====

TEST_F(CodeGenTest, RawStringPrint) {
    EXPECT_EQ(runSource(R"(print(r"\n"))"), "\\n\n");
}

TEST_F(CodeGenTest, RawStringConcat) {
    EXPECT_EQ(runSource(R"(print(r"\t" + "hello"))"), "\\thello\n");
}

// ===== [contract] test matcher: toNotEq =====

TEST_F(CodeGenTest, ExpectToNotEq) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"not equal\")\n"
        "    fn notEqual():\n"
        "        expect(1).toNotEq(2)\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== [contract] test matcher: toBeSome =====

TEST_F(CodeGenTest, ExpectToBeSome) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"some\")\n"
        "    fn some():\n"
        "        x = Some(5)\n"
        "        expect(x).toBeSome()\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== [contract] test matcher: toContain =====

TEST_F(CodeGenTest, ExpectToContainList) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"contains\")\n"
        "    fn contains():\n"
        "        xs = [1, 2, 3]\n"
        "        expect(xs).toContain(2)\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToContainString) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"contains str\")\n"
        "    fn containsStr():\n"
        "        s = \"hello world\"\n"
        "        expect(s).toContain(\"world\")\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== [contract] test matcher: toMatch =====

TEST_F(CodeGenTest, ExpectToMatchLiteral) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"matches literal\")\n"
        "    fn matchesLiteral():\n"
        "        expect(\"hello world\").toMatch(\"world\")\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToMatchAnchoredPattern) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"anchored\")\n"
        "    fn anchored():\n"
        "        expect(\"v1.2.3\").toMatch(\"^v\\\\d+\\\\.\\\\d+\\\\.\\\\d+$\")\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToBeCloseToFloatSum) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"float sum\")\n"
        "    fn floatSum():\n"
        "        expect(0.1 + 0.2).toBeCloseTo(0.3)\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToBeCloseToCustomDecimals) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"decimals 4\")\n"
        "    fn decimals4():\n"
        "        expect(1.00001).toBeCloseTo(1.00002, 4)\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

// ===== [contract] toBeBetween / toBeOneOf (#1689) =====

TEST_F(CodeGenTest, ExpectToBeBetweenIntInside) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"in range\")\n"
        "    fn inRange():\n"
        "        expect(5).toBeBetween(1, 10)\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToBeBetweenRejectsBoolActual) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"bool actual\")\n"
        "    fn boolActual():\n"
        "        expect(true).toBeBetween(1, 10)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeBetweenRejectsStrBound) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"str bound\")\n"
        "    fn strBound():\n"
        "        expect(5).toBeBetween(\"a\", 10)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeBetweenRejectsSingleArg) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"single arg\")\n"
        "    fn singleArg():\n"
        "        expect(5).toBeBetween(1)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeOneOfIntMatch) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"int match\")\n"
        "    fn intMatch():\n"
        "        expect(200).toBeOneOf([200, 201, 204])\n"
    );
    EXPECT_NO_THROW(runTestSource(src));
}

TEST_F(CodeGenTest, ExpectToBeOneOfRejectsNonList) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"non-list\")\n"
        "    fn nonList():\n"
        "        expect(200).toBeOneOf(200)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeOneOfRejectsStrArg) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"str not list\")\n"
        "    fn strNotList():\n"
        "        expect(\"foo\").toBeOneOf(\"bar\")\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeOneOfRejectsTypeMismatch) {
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"type mismatch\")\n"
        "    fn typeMismatch():\n"
        "        expect(\"foo\").toBeOneOf([1, 2, 3])\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToBeOneOfRejectsListOfLists) {
    // Pointer-element allowlist guard: non-str ptr elements (List<List<int>>)
    // must be rejected before reaching strcmp on List headers.
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"list of lists\")\n"
        "    fn listOfLists():\n"
        "        a: List<int> = [1, 2]\n"
        "        b: List<int> = [3, 4]\n"
        "        expect(a).toBeOneOf([a, b])\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToContainRejectsListOfLists) {
    // toContain on List<List<T>> must be rejected before reaching strcmp on
    // List headers (#1763).
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"list of lists\")\n"
        "    fn listOfLists():\n"
        "        a: List<int> = [1, 2]\n"
        "        b: List<int> = [3, 4]\n"
        "        outer: List<List<int>> = [a, b]\n"
        "        expect(outer).toContain(a)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToContainRejectsListOfMaps) {
    // toContain on List<Map<K, V>> must be rejected: strcmp on Map headers
    // is UB (#1763).
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"list of maps\")\n"
        "    fn listOfMaps():\n"
        "        m: Map<str, int> = {\"a\": 1}\n"
        "        outer: List<Map<str, int>> = [m]\n"
        "        expect(outer).toContain(m)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToContainRejectsSetOfLists) {
    // toContain on Set<List<T>> must be rejected: strcmp on List headers
    // (Set branch shares the same UB) (#1763).
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"set of lists\")\n"
        "    fn setOfLists():\n"
        "        a: List<int> = [1, 2]\n"
        "        s: Set<List<int>> = {a}\n"
        "        expect(s).toContain(a)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToContainRejectsListOfFns) {
    // Directly trigger the list_elem_fn_type_info disjunct: List<fn() -> int>
    // would otherwise read closure header bytes via strcmp (#1763).
    std::string src = withStdlibDirectiveDecls(
        "fn helper() -> int:\n"
        "    return 42\n"
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"list of fns\")\n"
        "    fn listOfFns():\n"
        "        flist: List<fn() -> int> = [helper]\n"
        "        expect(flist).toContain(helper)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ExpectToNotContainRejectsListOfLists) {
    // toNotContain shares the same code path as toContain and must reject
    // List<List<T>> via the same guard (#1763).
    std::string src = withStdlibDirectiveDecls(
        "@describe(\"matchers\")\n"
        "fn matchers():\n"
        "    @it(\"toNotContain list of lists\")\n"
        "    fn notContainListOfLists():\n"
        "        a: List<int> = [1, 2]\n"
        "        b: List<int> = [3, 4]\n"
        "        c: List<int> = [99, 99]\n"
        "        outer: List<List<int>> = [a, b]\n"
        "        expect(outer).toNotContain(c)\n"
    );
    EXPECT_THROW(runTestSource(src), std::runtime_error);
}

// ===== [contract] Field assignment subtype coercion (#359) =====

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

// ===== [contract] ? operator subtype coercion (#360) =====

TEST_F(CodeGenTest, ErrorPropagateSubtypeCoercion) {
    std::string src =
        "record ApiError < Error:\n    endpoint: str\nfn fetch(url: str) -> Result<int, ApiError>:\n    return Err(ApiError(\"fail\", 500, url))\nfn process(url: str) -> Result<int, Error>:\n    val = fetch(url)?\n    return Ok(val)\nresult = process(\"/api\")\ncase result:\n    Err(e):\n        print(e.message)\n    Ok(v):\n        print(v)";
    EXPECT_EQ(runSource(src), "fail\n");
}

// ===== [contract] Generic record bounds (#297) =====

TEST_F(CodeGenTest, GenericRecordBound) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "fn describe<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(describe(Dog(\"Rex\", 4, \"Lab\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundExactType) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "fn describe<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(describe(Animal(\"Cat\", 4)))";
    EXPECT_EQ(runSource(src), "Cat\n");
}

TEST_F(CodeGenTest, GenericRecordBoundViolation) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "    legs: int\n"
        "fn describe<T: Animal>(a: T) -> str:\n"
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
        "fn getName<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(getName(GuideDog(\"Rex\", \"Lab\", \"John\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundExplicitTypeArg) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "fn getName<T: Animal>(a: T) -> str:\n"
        "    return a.name\n"
        "print(getName[Dog](Dog(\"Rex\", \"Lab\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundMixedParams) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "fn pairName<T: Animal, U>(a: T, x: U) -> str:\n"
        "    return a.name\n"
        "print(pairName(Dog(\"Rex\", \"Lab\"), 42))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundAliasAcceptsSubtype) {
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Dog < Animal:\n"
        "    breed: str\n"
        "type AnimalAlias = Animal\n"
        "fn describe<T: AnimalAlias>(a: T) -> str:\n"
        "    return a.name\n"
        "print(describe(Dog(\"Rex\", \"Lab\")))";
    EXPECT_EQ(runSource(src), "Rex\n");
}

TEST_F(CodeGenTest, GenericRecordBoundAliasRejectsNonSubtype) {
    // Cat has 'name' so that the body's `a.name` typechecks — the only
    // reason this source must throw is the bound check itself rejecting
    // a non-subtype. This guards against a regression that silently
    // loosens validateTypeBounds when aliases are involved.
    std::string src =
        "record Animal:\n"
        "    name: str\n"
        "record Cat:\n"
        "    name: str\n"
        "type AnimalAlias = Animal\n"
        "fn describe<T: AnimalAlias>(a: T) -> str:\n"
        "    return a.name\n"
        "describe(Cat(\"Tom\"))";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== [contract] Tail Call Optimization (#214) =====

TEST_F(CodeGenTest, TailCallOptimization) {
    // Deep self-recursive tail call should not stack overflow
    EXPECT_EQ(runSource(
        "fn sumTo(n: int, acc: int) -> int:\n"
        "  if n <= 0:\n"
        "    return acc\n"
        "  return sumTo(n - 1, acc + n)\n"
        "print(sumTo(1000000, 0))"), "500000500000\n");
}

TEST_F(CodeGenTest, TailCallFactorial) {
    // Tail-recursive factorial with accumulator
    EXPECT_EQ(runSource(
        "fn factorial(n: int, acc: int) -> int:\n"
        "  if n <= 1:\n"
        "    return acc\n"
        "  return factorial(n - 1, n * acc)\n"
        "print(factorial(20, 1))"), "2432902008176640000\n");
}

TEST_F(CodeGenTest, NonTailRecursionStillWorks) {
    // n * factorial(n-1) is NOT a tail call — should still work for small N
    EXPECT_EQ(runSource(
        "fn factorial(n: int) -> int:\n"
        "  if n <= 1:\n"
        "    return 1\n"
        "  return n * factorial(n - 1)\n"
        "print(factorial(10))"), "3628800\n");
}

// ===== [contract] Mutual Recursion (Forward Function References) =====

TEST_F(CodeGenTest, MutualRecursion) {
    EXPECT_EQ(runSource(
        "fn isEven(n: int) -> bool:\n"
        "  if n == 0:\n"
        "    return true\n"
        "  return isOdd(n - 1)\n"
        "\n"
        "fn isOdd(n: int) -> bool:\n"
        "  if n == 0:\n"
        "    return false\n"
        "  return isEven(n - 1)\n"
        "\n"
        "print(isEven(4))\n"
        "print(isOdd(3))"), "true\ntrue\n");
}

TEST_F(CodeGenTest, ThreeWayMutualRecursion) {
    EXPECT_EQ(runSource(
        "fn fa(n: int) -> str:\n"
        "  if n <= 0:\n"
        "    return \"a\"\n"
        "  return fb(n - 1)\n"
        "\n"
        "fn fb(n: int) -> str:\n"
        "  if n <= 0:\n"
        "    return \"b\"\n"
        "  return fc(n - 1)\n"
        "\n"
        "fn fc(n: int) -> str:\n"
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
        "fn caller(n: int) -> int:\n"
        "  return callee(n) + 1\n"
        "\n"
        "fn callee(n: int) -> int:\n"
        "  return n * 2\n"
        "\n"
        "print(caller(5))"), "11\n");
}

// ===== [regression #1157] Result coerce — PHI-static disc via if-expr both-branches Ok =====

TEST_F(CodeGenTest, ResultCoerceIfBothBranchesOk) {
    // Both branches of the if-expr emit Ok (disc=1 statically through PHI).
    // tryGetStaticResultDisc recurses through the PHI and allows the
    // compile-time slot copy despite the Err type mismatch (errorTy_ to MyErr).
    EXPECT_NO_THROW(compileSource(
        "record MyErr:\n"
        "  msg: str\n"
        "fn testFn(x: int) -> Result<int, MyErr>:\n"
        "  r: Result<int, MyErr> = if x > 0:\n"
        "    (Ok(x))\n"
        "  else:\n"
        "    (Ok(0))\n"
        "  return r\n"
    ));
}

