#include "test_codegen_common.hpp"
#include "ry/module_loader.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <set>
#include <sstream>


using namespace ry;
// ===== when expression =====

TEST_F(CodeGenTest, WhenExprBasics) {
    EXPECT_EQ(runSource("x = case:\n    true : 1\n    _ : 2\nprint(x)"), "1\n");
    EXPECT_EQ(runSource("x = case:\n    false : 1\n    _ : 2\nprint(x)"), "2\n");
    EXPECT_EQ(runSource("x = case:\n    3 > 2 : 10\n    _ : 20\nprint(x)"), "10\n");
    EXPECT_EQ(runSource("s = case:\n    true : \"yes\"\n    _ : \"no\"\nprint(s)"), "yes\n");
    EXPECT_EQ(runSource("x = case:\n    true : case:\n        false : 1\n        _ : 2\n    _ : 3\nprint(x)"), "2\n");
    EXPECT_EQ(runSource("x = case:\n    true : \"a\"\n    _ : \"b\"\nprint(x)"), "a\n");
}

TEST_F(CodeGenTest, WhenExprTypeMismatchErrors) {
    EXPECT_THROW(runSource("x = case:\n    true : \"hello\"\n    _ : [1, 2, 3]\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = case:\n    true : [1, 2]\n    _ : {\"a\": 1}\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, WhenExprListSameType) {
    std::string src =
        "x = case:\n    true : [1, 2]\n    _ : [3, 4]\nprint(x)";
    EXPECT_EQ(runSource(src), "[1, 2]\n");
}

// ===== if / when codegen =====

TEST_F(CodeGenTest, IfAndWhenBasics) {
    EXPECT_EQ(runSource("if true:\n    print(1)"), "1\n");
    EXPECT_EQ(runSource("if false:\n    print(1)"), "");
    EXPECT_EQ(runSource("if true:\n    print(1)\nelse:\n    print(2)"), "1\n");
    EXPECT_EQ(runSource("if false:\n    print(1)\nelse:\n    print(2)"), "2\n");
    EXPECT_EQ(runSource(
        "x = 2\ncase:\n    x == 1:\n        print(10)\n    x == 2:\n        print(20)\n    _:\n        print(30)"), "20\n");
    EXPECT_EQ(runSource(
        "x = 99\ncase:\n    x == 1:\n        print(10)\n    x == 2:\n        print(20)\n    _:\n        print(30)"), "30\n");
    EXPECT_EQ(runSource(
        "if true:\n"
        "    print(1)\n"
        "print(2)"), "1\n2\n");
    EXPECT_EQ(runSource(
        "if false:\n"
        "    print(1)\n"
        "print(2)"), "2\n");
    EXPECT_EQ(runSource(
        "x = 3\ncase:\n    x == 1:\n        print(10)\n    x == 2:\n        print(20)\n    x == 3:\n        print(30)\n    x == 4:\n        print(40)\n    _:\n        print(50)"), "30\n");
}

TEST_F(CodeGenTest, IfEdgeCases) {
    // IfVariableReassignment
    EXPECT_EQ(runSource(
        "x = 0\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)"), "42\n");
    // IfNonBoolConditionInt
    EXPECT_EQ(runSource("if 1:\n    print(1)"), "1\n");
    EXPECT_EQ(runSource("if 0:\n    print(1)"), "");
    // NestedIf
    EXPECT_EQ(runSource(
        "if true:\n"
        "    if true:\n"
        "        print(1)"), "1\n");
}

// ===== while codegen =====

TEST_F(CodeGenTest, WhileLoopVariants) {
    // WhileFalseDoesNotExecute
    EXPECT_EQ(runSource("while false:\n    print(1)"), "");
    // WhileCountdown
    EXPECT_EQ(runSource(
        "i = 3\n"
        "while i > 0:\n"
        "    print(i)\n"
        "    i = i - 1"), "3\n2\n1\n");
    // WhileFollowedByStatement
    EXPECT_EQ(runSource(
        "i = 0\n"
        "while i < 2:\n"
        "    i = i + 1\n"
        "print(i)"), "2\n");
    // WhileZeroCondition
    EXPECT_EQ(runSource("while 0:\n    print(1)"), "");
    // WhileNestedWhile
    EXPECT_EQ(runSource(
        "i = 0\n"
        "while i < 2:\n"
        "    j = 0\n"
        "    while j < 2:\n"
        "        print(i + j)\n"
        "        j = j + 1\n"
        "    i = i + 1"), "0\n1\n1\n2\n");
    // WhileWithIf
    EXPECT_EQ(runSource(
        "i = 1\n"
        "while i <= 4:\n"
        "    if i % 2 == 0:\n"
        "        print(i)\n"
        "    i = i + 1"), "2\n4\n");
}

// ===== function =====

TEST_F(CodeGenTest, FnBasicDefinitions) {
    // FnBasicAddCall
    EXPECT_EQ(runSource(
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "res = add(1, 2)\n"
        "print(res)"), "3\n");
    // FnNoArgs
    EXPECT_EQ(runSource(
        "fn zero() -> int:\n"
        "    return 0\n"
        "print(zero())"), "0\n");
    // FnFloatReturn
    EXPECT_EQ(runSource(
        "fn half(x: float) -> float:\n"
        "    return x / 2.0\n"
        "r = half(7.0)\n"
        "print(r)"), "3.5\n");
    // FnBoolReturn
    EXPECT_EQ(runSource(
        "fn isPositive(x: int) -> bool:\n"
        "    return x > 0\n"
        "print(isPositive(5))"), "true\n");
    // FnWithIf
    EXPECT_EQ(runSource(
        "fn abs(x: int) -> int:\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-5))\n"
        "print(abs(3))"), "5\n3\n");
    // FnCallAsStatement
    EXPECT_EQ(runSource(
        "fn greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n"
        "greet()"), "42\n");
    // FnRecursiveFactorial
    EXPECT_EQ(runSource(
        "fn factorial(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * factorial(n - 1)\n"
        "print(factorial(5))"), "120\n");
}

TEST_F(CodeGenTest, FnErrors) {
    // FnUndefinedCallThrows
    EXPECT_THROW(runSource("x = unknown(1)"), std::runtime_error);
    // FnArgCountMismatchThrows
    EXPECT_THROW(runSource(
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = add(1)"), std::runtime_error);
    // FnArgTypeMismatchThrows
    EXPECT_THROW(runSource(
        "fn inc(a: int) -> int:\n"
        "    return a + 1\n"
        "x = inc(1.5)"), std::runtime_error);
    // FnReturnTypeMismatchThrows
    EXPECT_THROW(runSource(
        "fn bad() -> int:\n"
        "    return \"hello\""), std::runtime_error);
}

// ===== Block scope =====

TEST_F(CodeGenTest, BlockScopeNotVisible) {
    // BlockScopeIfVarNotVisible
    EXPECT_THROW(runSource(
        "if true:\n"
        "    x = 42\n"
        "print(x)"), std::runtime_error);
    // BlockScopeWhileVarNotVisible
    EXPECT_THROW(runSource(
        "i = 1\n"
        "while i > 0:\n"
        "    x = 99\n"
        "    i = 0\n"
        "print(x)"), std::runtime_error);
    // BlockScopeElseVarNotVisible
    EXPECT_THROW(runSource(
        "if false:\n"
        "    a = 1\n"
        "else:\n"
        "    b = 2\n"
        "print(b)"), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeAccessible) {
    // BlockScopeInnerAccessesOuter
    EXPECT_EQ(runSource(
        "x = 1\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)"), "42\n");
    // BlockScopeReassignment
    EXPECT_EQ(runSource(
        "x = 1\n"
        "if true:\n"
        "    x = 99\n"
        "    print(x)\n"
        "print(x)"), "99\n99\n");
    // BlockScopeNewVariable
    EXPECT_EQ(runSource(
        "if true:\n"
        "    c = 10\n"
        "c = 20\n"
        "print(c)"), "20\n");
}

// ===== Record =====

TEST_F(CodeGenTest, RecordBasics) {
    // RecordBasicFieldAccess
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(10, 20)\n"
        "print(p.x)\n"
        "print(p.y)"), "10\n20\n");
    // RecordFloatFields
    EXPECT_EQ(runSource(
        "record Vec2:\n"
        "    x: float\n"
        "    y: float\n"
        "v = Vec2(1.5, 2.5)\n"
        "print(v.x)\n"
        "print(v.y)"), "1.5\n2.5\n");
    // RecordInFnArg
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn getX(p: Point) -> int:\n"
        "    return p.x\n"
        "p = Point(42, 99)\n"
        "print(getX(p))"), "42\n");
    // RecordAsReturnValue
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn makePoint(a: int, b: int) -> Point:\n"
        "    return Point(a, b)\n"
        "p = makePoint(7, 8)\n"
        "print(p.x)\n"
        "print(p.y)"), "7\n8\n");
    // RecordNested
    EXPECT_EQ(runSource(
        "record Inner:\n"
        "    val: int\n"
        "record Outer:\n"
        "    inner: Inner\n"
        "    extra: int\n"
        "i = Inner(42)\n"
        "o = Outer(i, 99)\n"
        "print(o.inner.val)\n"
        "print(o.extra)"), "42\n99\n");
    // RecordFieldArithmetic
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "a = Point(10, 20)\n"
        "b = Point(3, 7)\n"
        "dx = a.x - b.x\n"
        "print(dx)"), "7\n");
    // RecordKeyword
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p.x)\n"
        "print(p.y)"), "1\n2\n");
}

TEST_F(CodeGenTest, RecordErrors) {
    // RecordConstructorArgCountMismatchThrows
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1)"), std::runtime_error);
    // RecordConstructorArgTypeMismatchThrows
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1.5, 2)"), std::runtime_error);
    // RecordUnknownFieldThrows
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p.z)"), std::runtime_error);
    // FieldAccessOnNonRecordThrows
    EXPECT_THROW(runSource(
        "x = 42\n"
        "print(x.field)"), std::runtime_error);
    // PrintStruct — now works via auto-generated toStr
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p)"), "Point(x: 1, y: 2)\n");
}

// ===== Record toStr =====

TEST_F(CodeGenTest, RecordToStr) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(toStr(p))"), "Point(x: 1, y: 2)\n");
}

TEST_F(CodeGenTest, RecordFString) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(3, 4)\n"
        "print(f\"pos={p}\")"), "pos=Point(x: 3, y: 4)\n");
}

TEST_F(CodeGenTest, RecordNestedToStr) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "record Circle:\n"
        "    center: Point\n"
        "    radius: float\n"
        "c = Circle(Point(0, 0), 1.5)\n"
        "print(c)"), "Circle(center: Point(x: 0, y: 0), radius: 1.5)\n");
}

TEST_F(CodeGenTest, RecordUserDefinedToStr) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn toStr(p: Point) -> str:\n"
        "    return f\"({p.x}, {p.y})\"\n"
        "p = Point(5, 6)\n"
        "print(toStr(p))"), "(5, 6)\n");
}

TEST_F(CodeGenTest, RecordUserDefinedToStrWithPrint) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn toStr(p: Point) -> str:\n"
        "    return f\"({p.x}, {p.y})\"\n"
        "p = Point(5, 6)\n"
        "print(p)"), "(5, 6)\n");
}

TEST_F(CodeGenTest, RecordStrField) {
    EXPECT_EQ(runSource(
        "record Person:\n"
        "    name: str\n"
        "    age: int\n"
        "p = Person(\"Alice\", 30)\n"
        "print(p)"), "Person(name: \"Alice\", age: 30)\n");
}

TEST_F(CodeGenTest, UnknownTypeAnnotationThrows) {
    EXPECT_THROW(runSource("x: foo = 42"), std::runtime_error);
}

// ===== Unit type =====

TEST_F(CodeGenTest, UnitTypeBasics) {
    // UnitFnNoReturnType
    EXPECT_EQ(runSource(
        "fn greet() -> Unit:\n"
        "    print(42)\n"
        "greet()"), "42\n");
    // UnitFnExplicit
    EXPECT_EQ(runSource(
        "fn greet() -> Unit:\n"
        "    print(99)\n"
        "greet()"), "99\n");
    // UnitFnReturnVoid
    EXPECT_EQ(runSource(
        "fn noop() -> Unit:\n"
        "    return\n"
        "noop()"), "");
    // UnitFnReturnVoidEarly
    EXPECT_EQ(runSource(
        "fn maybePrint(x: int) -> Unit:\n"
        "    if x > 0:\n"
        "        print(x)\n"
        "        return\n"
        "    print(0)\n"
        "maybePrint(5)\n"
        "maybePrint(-1)"), "5\n0\n");
}

TEST_F(CodeGenTest, UnitFnReturnValueThrows) {
    std::string src =
        "fn f() -> Unit:\n"
        "    return 42\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Option<T> =====

TEST_F(CodeGenTest, OptionPrintVariants) {
    // OptionIntSomePrint
    EXPECT_EQ(runSource("x: Option<int> = Some(42)\nprint(x)"), "Some(42)\n");
    // OptionIntNonePrint
    EXPECT_EQ(runSource("x: Option<int> = None\nprint(x)"), "None\n");
    // OptionFloatSomePrint
    EXPECT_EQ(runSource("x: Option<float> = Some(3.14)\nprint(x)"), "Some(3.14)\n");
    // OptionFloatNonePrint
    EXPECT_EQ(runSource("x: Option<float> = None\nprint(x)"), "None\n");
    // OptionBoolSomePrint
    EXPECT_EQ(runSource("x: Option<bool> = Some(true)\nprint(x)"), "Some(true)\n");
    // OptionStrSomePrint
    EXPECT_EQ(runSource("x: Option<str> = Some(\"hello\")\nprint(x)"), "Some(hello)\n");
}

TEST_F(CodeGenTest, OptionInFunctions) {
    // OptionFnParamAndReturn
    EXPECT_EQ(runSource(
        "fn maybeDouble(x: Option<int>) -> Option<int>:\n"
        "    return x\n"
        "a = maybeDouble(Some(21))\n"
        "print(a)"), "Some(21)\n");
    // OptionFnNoneArg
    EXPECT_EQ(runSource(
        "fn f(x: Option<int>) -> int:\n"
        "    return 0\n"
        "r = f(None)\n"
        "print(r)"), "0\n");
}

TEST_F(CodeGenTest, OptionErrors) {
    // NoneKeywordWithoutAnnotationThrows
    EXPECT_THROW(runSource("x = none"), std::runtime_error);
    // NoneKeywordWithNonOptionAnnotationThrows
    EXPECT_THROW(runSource("x: int = none"), std::runtime_error);
    // NoneWithoutAnnotationThrows
    EXPECT_THROW(runSource("x = None"), std::runtime_error);
    // OptionTypeMismatchThrows
    EXPECT_THROW(runSource("x: Option<int> = Some(3.14)"), std::runtime_error);
    // UnwrapRemoved
    EXPECT_THROW(runSource(
        "x: Option<int> = Some(42)\n"
        "v = unwrap(x)\n"
        "print(v)"), std::runtime_error);
    // UnwrapRemovedUFCS
    EXPECT_THROW(runSource(
        "x: Option<int> = Some(42)\n"
        "v = x.unwrap()\n"
        "print(v)"), std::runtime_error);
}

TEST_F(CodeGenTest, OptionReassignment) {
    // NoneKeywordWithOptionAnnotation
    EXPECT_EQ(runSource("x: Option<int> = none\nprint(x)"), "None\n");
    // OptionReassignSomeToNone
    EXPECT_EQ(runSource(
        "x: Option<int> = Some(42)\n"
        "x = None\n"
        "print(x)"), "None\n");
    // OptionReassignNoneToSome
    EXPECT_EQ(runSource(
        "x: Option<int> = None\n"
        "x = Some(99)\n"
        "print(x)"), "Some(99)\n");
    // OptionAutoWrapInt
    EXPECT_EQ(runSource("x: int? = 42\nprint(x)"), "Some(42)\n");
    // OptionAutoWrapStr
    EXPECT_EQ(runSource("x: str? = \"hello\"\nprint(x)"), "Some(hello)\n");
    // OptionAutoWrapBool
    EXPECT_EQ(runSource("x: bool? = true\nprint(x)"), "Some(true)\n");
    // OptionAutoWrapFloat
    EXPECT_EQ(runSource("x: float? = 3.14\nprint(x)"), "Some(3.14)\n");
}

// ===== Null coalesce =====

TEST_F(CodeGenTest, NullCoalesceVariants) {
    // NullCoalesceSome
    EXPECT_EQ(runSource(
        "x: int? = Some(5)\n"
        "y = x ?? 0\n"
        "print(y)"), "5\n");
    // NullCoalesceNone
    EXPECT_EQ(runSource(
        "x: int? = none\n"
        "y = x ?? 42\n"
        "print(y)"), "42\n");
    // NoneKeyword
    EXPECT_EQ(runSource(
        "x: int? = none\n"
        "print(x)"), "None\n");
}

// ===== Tuple =====

TEST_F(CodeGenTest, TupleBasics) {
    // TupleCreateAndAccess
    EXPECT_EQ(runSource(
        "t = (10, 20)\n"
        "print(t.0)\n"
        "print(t.1)"), "10\n20\n");
    // TupleMixedTypes
    EXPECT_EQ(runSource(
        "t = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)"), "42\n3.14\n");
    // TupleWithTypeAnnotation
    EXPECT_EQ(runSource(
        "t: (int, float) = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)"), "42\n3.14\n");
    // TupleThreeElements
    EXPECT_EQ(runSource(
        "t = (1, 2, 3)\n"
        "print(t.0)\n"
        "print(t.1)\n"
        "print(t.2)"), "1\n2\n3\n");
    // TupleLetAssign
    EXPECT_EQ(runSource(
        "a = (1, 2)\n"
        "x = a.0\n"
        "y = a.1\n"
        "print(x)\n"
        "print(y)"), "1\n2\n");
}

TEST_F(CodeGenTest, TupleFnReturn) {
    // TupleFnReturn
    EXPECT_EQ(runSource(
        "fn swap(a: int, b: int) -> (int, int):\n"
        "    return (b, a)\n"
        "res = swap(1, 2)\n"
        "print(res.0)\n"
        "print(res.1)"), "2\n1\n");
    // TupleFnReturnAccessDirect
    EXPECT_EQ(runSource(
        "fn makePair(a: int, b: float) -> (int, float):\n"
        "    return (a, b)\n"
        "p = makePair(42, 3.14)\n"
        "print(p.0)\n"
        "print(p.1)"), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleDestructuring) {
    // TupleDestructBasic
    EXPECT_EQ(runSource(
        "a, b = (10, 20)\n"
        "print(a)\n"
        "print(b)"), "10\n20\n");
    // TupleDestructWildcard
    EXPECT_EQ(runSource(
        "a, _ = (10, 20)\n"
        "print(a)"), "10\n");
    // TupleDestructTriple
    EXPECT_EQ(runSource(
        "a, b, c = (1, 2, 3)\n"
        "print(a)\n"
        "print(b)\n"
        "print(c)"), "1\n2\n3\n");
    // TupleDestructFromFn
    EXPECT_EQ(runSource(
        "fn f() -> (int, int):\n"
        "    return (3, 4)\n"
        "a, b = f()\n"
        "print(a)\n"
        "print(b)"), "3\n4\n");
    // VarTupleDestruct
    EXPECT_EQ(runSource(
        "a, b = (10, 20)\n"
        "a = 100\n"
        "print(a)\n"
        "print(b)"), "100\n20\n");
    // TupleDestructMixedTypes
    EXPECT_EQ(runSource(
        "x, y = (42, 3.14)\n"
        "print(x)\n"
        "print(y)"), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleErrors) {
    // TupleIndexOutOfRangeThrows
    EXPECT_THROW(runSource("t = (1, 2)\nprint(t.2)"), std::runtime_error);
    // TupleTypeAnnotationMismatchThrows
    EXPECT_THROW(runSource("t: (int, int) = (1, 3.14)"), std::runtime_error);
}

TEST_F(CodeGenTest, ListDestructuring) {
    // ListDestructBasic
    EXPECT_EQ(runSource(
        "a, b = [10, 20]\n"
        "print(a)\n"
        "print(b)"), "10\n20\n");
    // ListDestructTriple
    EXPECT_EQ(runSource(
        "a, b, c = [1, 2, 3]\n"
        "print(a)\n"
        "print(b)\n"
        "print(c)"), "1\n2\n3\n");
    // ListDestructWildcard
    EXPECT_EQ(runSource(
        "_, b = [10, 20]\n"
        "print(b)"), "20\n");
    // ListDestructParenForm
    EXPECT_EQ(runSource(
        "(a, b) = [3, 4]\n"
        "print(a)\n"
        "print(b)"), "3\n4\n");
    // ListDestructConst
    EXPECT_EQ(runSource(withStdlibDirectiveDecls(
        "@const a, b = [5, 6]\n"
        "print(a)\n"
        "print(b)")), "5\n6\n");
    // ListDestructFromFn
    EXPECT_EQ(runSource(
        "fn pair() -> List<int>:\n"
        "    return [7, 8]\n"
        "a, b = pair()\n"
        "print(a)\n"
        "print(b)"), "7\n8\n");
    // ListDestructFromVariable
    EXPECT_EQ(runSource(
        "xs: List<int> = [100, 200]\n"
        "a, b = xs\n"
        "print(a)\n"
        "print(b)"), "100\n200\n");
}

TEST_F(CodeGenTest, ListDestructuringErrors) {
    // ListDestructLengthMismatchTooManyPanics
    EXPECT_EXIT(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "a, b = xs"), ::testing::ExitedWithCode(1), "list destructuring expected");
    // ListDestructLengthMismatchTooFewPanics
    EXPECT_EXIT(runSource(
        "xs: List<int> = [1]\n"
        "a, b = xs"), ::testing::ExitedWithCode(1), "list destructuring expected");
    // DestructFromScalarRejected
    EXPECT_THROW(runSource("a, b = 42"), std::runtime_error);
    // DestructFromMapRejected
    EXPECT_THROW(runSource("m: Map<str, int> = {}\na, b = m"), std::runtime_error);
    // SamePatternDuplicateRejected
    EXPECT_THROW(runSource("a, a = [1, 2]"), std::runtime_error);
}

// ===== import =====

class ImportTest : public CodeGenTest {
protected:
    std::filesystem::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = std::filesystem::temp_directory_path() / "ry_import_test";
        std::filesystem::create_directories(tmp_dir_);
    }

    void TearDown() override {
        std::filesystem::remove_all(tmp_dir_);
    }

    void writeFile(const std::string &relative_path, const std::string &content) {
        auto path = tmp_dir_ / relative_path;
        std::filesystem::create_directories(path.parent_path());
        std::ofstream f(path);
        f << content;
    }

    // Drop a minimal `package.toml` in @p relative_dir (relative to tmp_dir_),
    // marking that directory as a package boundary for visibility tests.
    void writePackageToml(const std::string &relative_dir,
                          const std::string &name = "testpkg") {
        auto dir = relative_dir.empty() ? tmp_dir_ : (tmp_dir_ / relative_dir);
        std::filesystem::create_directories(dir);
        std::ofstream f(dir / "package.toml");
        f << "[project]\nname = \"" << name << "\"\nversion = \"0.0.0\"\n";
    }

    std::string runWithImports(const std::string &src,
                               const std::string &referrer_dir = "",
                               const std::vector<std::string> &search_paths = {}) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        std::string dir = referrer_dir.empty() ? tmp_dir_.string() : referrer_dir;
        ModuleLoader loader(search_paths);
        prog = loader.resolveImports(prog, dir);

        CodeGen cg;
        cg.setTestingIntrinsicsImported(loader.importedTestingIntrinsics());
        auto tsm = cg.compile(prog);
        return runModule(std::move(tsm));
    }

    Program resolveImportsOnly(const std::string &src,
                               const std::string &referrer_dir = "",
                               const std::vector<std::string> &search_paths = {}) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        std::string dir = referrer_dir.empty() ? tmp_dir_.string() : referrer_dir;
        ModuleLoader loader(search_paths);
        return loader.resolveImports(prog, dir);
    }

    std::unordered_set<std::string> resolveAndGetTestingIntrinsics(
        const std::string &src,
        const std::string &referrer_dir = "",
        const std::vector<std::string> &search_paths = {}) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        std::string dir = referrer_dir.empty() ? tmp_dir_.string() : referrer_dir;
        ModuleLoader loader(search_paths);
        loader.resolveImports(prog, dir);
        return loader.importedTestingIntrinsics();
    }

    std::unordered_set<std::string> resolveAndGetCodeGenTestingIntrinsics(
        const std::string &src,
        const std::string &referrer_dir = "",
        const std::vector<std::string> &search_paths = {}) {
        Lexer lex(src);
        Parser parser(lex);
        Program prog = parser.parseProgram();

        std::string dir = referrer_dir.empty() ? tmp_dir_.string() : referrer_dir;
        ModuleLoader loader(search_paths);
        loader.resolveImports(prog, dir);

        CodeGen cg;
        cg.setTestingIntrinsicsImported(loader.importedTestingIntrinsics());
        return cg.getTestingIntrinsicsImported();
    }
};

TEST_F(ImportTest, ImportBasics) {
    // ImportAllFunctions
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");
    EXPECT_EQ(runWithImports(
        "from math\n"
        "print(add(1, 2))\n"
        "print(sub(5, 3))"),
        "3\n2\n");

    // ImportSelectedFunction
    EXPECT_EQ(runWithImports(
        "from math import add\n"
        "print(add(10, 20))"),
        "30\n");

    // ImportMultipleSelected
    writeFile("math2.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n");
    EXPECT_EQ(runWithImports(
        "from math2 import add, mul\n"
        "print(add(2, 3))\n"
        "print(mul(4, 5))"),
        "5\n20\n");

    // DuplicateImportIgnored
    EXPECT_EQ(runWithImports(
        "from math\n"
        "from math\n"
        "print(add(1, 2))"),
        "3\n");
}

TEST_F(ImportTest, ImportSubdirectory) {
    writeFile("utils/math.ry",
        "fn doubleIt(x: int) -> int:\n"
        "    return x * 2\n");

    EXPECT_EQ(runWithImports(
        "from utils.math import doubleIt\n"
        "print(doubleIt(21))"),
        "42\n");
}

TEST_F(ImportTest, ImportErrors) {
    // CircularImportError
    writeFile("a.ry", "from b\n");
    writeFile("b.ry", "from a\n");
    EXPECT_THROW(runWithImports("from a"), std::runtime_error);

    // ModuleNotFoundError
    EXPECT_THROW(runWithImports("from nonexistent"), std::runtime_error);

    // FunctionNotFoundError
    writeFile("mathmod.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");
    EXPECT_THROW(runWithImports("from mathmod import nope"), std::runtime_error);
}

// Symbol alias (`from m import x as y`) — #1721.
//
// Only `@const` aliases work end-to-end in v0.0.23: emitting an
// `AssignStmt { name=alias, value=VariableExpr(orig) }` for a `@const`
// passes through codegen as a plain top-level binding. fn / record /
// enum / type-alias aliases require codegen-side name-resolution
// fallback that is not yet implemented, so `module_loader.cpp`
// explicitly rejects them with a message pointing at follow-up #1725.
TEST_F(ImportTest, ImportAliasConstWorks) {
    // Inline-declare the `@const` directive so the imported file can use it
    // without relying on stdlib resolution (the harness loader is constructed
    // with empty search_paths). With CodeRabbit's #1726 fix, only AssignStmts
    // carrying `@const` qualify as `ExportableKind::Const` and are aliasable.
    writeFile("constmod.ry",
        "@directive(target=[\"statement\"])\n"
        "fn const()\n"
        "@const\n"
        "ORIGIN_X: int = 42\n");
    EXPECT_EQ(runWithImports(
        "from constmod import ORIGIN_X as ZERO_X\n"
        "print(ZERO_X)"),
        "42\n");
}

// Non-`@const` module-level assignments are classified as `ExportableKind::Value`
// and the alias path rejects them. Regression for CodeRabbit's #1726 concern that
// every AssignStmt was previously classified as Const, bypassing rejection.
TEST_F(ImportTest, ImportAliasValueRejected) {
    writeFile("valmod.ry",
        "counter: int = 42\n");
    try {
        runWithImports("from valmod import counter as c\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("alias not supported for non-@const assignment 'counter'"),
                  std::string::npos)
            << "got: " << msg;
        EXPECT_NE(msg.find("mutable globals and directives cannot be re-bound"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(ImportTest, ImportAliasFunctionWorks) {
    writeFile("fnmod.ry",
        "@public\n"
        "fn rectArea(w: int, h: int) -> int:\n"
        "    return w * h\n");
    EXPECT_EQ(runWithImports(
        "from fnmod import rectArea as area\n"
        "print(area(3, 4))"),
        "12\n");
}

TEST_F(ImportTest, ImportAliasRecordWorks) {
    writeFile("recmod.ry",
        "@public\n"
        "record Point:\n"
        "    x: int\n"
        "    y: int\n");
    EXPECT_EQ(runWithImports(
        "from recmod import Point as P\n"
        "p: P = P(7, 9)\n"
        "print(p.x)\n"
        "print(p.y)"),
        "7\n9\n");
}

TEST_F(ImportTest, ImportAliasEnumWorks) {
    writeFile("enummod.ry",
        "@public\n"
        "enum Color:\n"
        "    Red\n"
        "    Green\n");
    EXPECT_EQ(runWithImports(
        "from enummod import Color as C\n"
        "c: C = C::Red\n"
        "print(toStr(c))"),
        "Red\n");
}

TEST_F(ImportTest, ImportAliasTypeAliasWorks) {
    writeFile("tymod.ry",
        "@public\n"
        "type Distance = int\n");
    EXPECT_EQ(runWithImports(
        "from tymod import Distance as D\n"
        "d: D = 42\n"
        "print(d)"),
        "42\n");
}

// Cache-hit path (`loaded_.count(abs_path) != 0`) also handles the alias.
// Two `from <mod>` statements force the second one through the cached branch.
TEST_F(ImportTest, ImportAliasFunctionWorksCacheHit) {
    writeFile("fnmod2.ry",
        "@public\n"
        "fn helper() -> int:\n"
        "    return 100\n");
    EXPECT_EQ(runWithImports(
        "from fnmod2\n"
        "from fnmod2 import helper as h\n"
        "print(h())"),
        "100\n");
}

// Cache-hit path for record alias: second `from <mod>` triggers the cached
// branch in module_loader. The alias still registers into record_types_.
TEST_F(ImportTest, ImportAliasRecordWorksCacheHit) {
    writeFile("recmod2.ry",
        "@public\n"
        "record Pt:\n"
        "    x: int\n"
        "    y: int\n");
    EXPECT_EQ(runWithImports(
        "from recmod2\n"
        "from recmod2 import Pt as Q\n"
        "q: Q = Q(5, 8)\n"
        "print(q.x)"),
        "5\n");
}

TEST_F(ImportTest, ImportAliasEnumWorksCacheHit) {
    writeFile("enummod2.ry",
        "@public\n"
        "enum Mode:\n"
        "    On\n"
        "    Off\n");
    EXPECT_EQ(runWithImports(
        "from enummod2\n"
        "from enummod2 import Mode as M\n"
        "m: M = M::On\n"
        "print(toStr(m))"),
        "On\n");
}

TEST_F(ImportTest, ImportAliasTypeAliasWorksCacheHit) {
    writeFile("tymod2.ry",
        "@public\n"
        "type Count = int\n");
    EXPECT_EQ(runWithImports(
        "from tymod2\n"
        "from tymod2 import Count as N\n"
        "n: N = 7\n"
        "print(n)"),
        "7\n");
}

// Overloaded function: alias must point at the entire OverloadEntry vector
// so each arity is callable through the alias.
TEST_F(ImportTest, ImportAliasFunctionOverload) {
    writeFile("ovlmod.ry",
        "@public\n"
        "fn area(w: int) -> int:\n"
        "    return w * w\n"
        "@public\n"
        "fn area(w: int, h: int) -> int:\n"
        "    return w * h\n");
    EXPECT_EQ(runWithImports(
        "from ovlmod import area as a\n"
        "print(a(5))\n"
        "print(a(3, 4))"),
        "25\n12\n");
}

// Record with parent: the parent_name field must travel with the alias.
TEST_F(ImportTest, ImportAliasRecordWithParent) {
    writeFile("parmod.ry",
        "@public\n"
        "record Animal:\n"
        "    name: str\n"
        "@public\n"
        "record Dog < Animal:\n"
        "    breed: str\n");
    EXPECT_EQ(runWithImports(
        "from parmod import Dog as D\n"
        "d: D = D(\"Rex\", \"Lab\")\n"
        "print(d.name)\n"
        "print(d.breed)"),
        "Rex\nLab\n");
}

// ADT enum: pattern match on a variant through the alias.
TEST_F(ImportTest, ImportAliasEnumAdtPatternMatch) {
    writeFile("adtmod.ry",
        "@public\n"
        "enum Shape:\n"
        "    Circle(radius: int)\n"
        "    Square(side: int)\n");
    EXPECT_EQ(runWithImports(
        "from adtmod import Shape as S\n"
        "s: S = S::Circle(5)\n"
        "case s:\n"
        "    S::Circle(r): print(r)\n"
        "    S::Square(side): print(side)"),
        "5\n");
}

// Type alias chain: `type B = int` in m, then `from m import B as X`. The
// new alias X resolves through the chain to int.
TEST_F(ImportTest, ImportAliasTypeAliasChain) {
    writeFile("chainmod.ry",
        "@public\n"
        "type B = int\n");
    EXPECT_EQ(runWithImports(
        "from chainmod import B as X\n"
        "y: X = 100\n"
        "print(y)"),
        "100\n");
}

// An import alias whose chosen name shadows a local definition in the
// importing module must be rejected by the alias's name-collision guard.
TEST_F(ImportTest, ImportAliasCollidesWithLocalDefinition) {
    writeFile("colmod.ry",
        "@public\n"
        "fn helper() -> int:\n"
        "    return 1\n");
    try {
        runWithImports(
            "fn helper() -> int:\n"
            "    return 2\n"
            "from colmod import helper as helper\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("helper"), std::string::npos) << "got: " << msg;
    }
}

// #1889: `from <mod> import <fn> as <builtin>` is just as dangerous as a
// direct `fn <builtin>` declaration — the alias goes into `fn_aliases_`
// where the hardcoded dispatch chain never consults it.
TEST_F(ImportTest, ImportAliasRejectsReservedBuiltinName) {
    writeFile("aliasbimod.ry",
        "@public\n"
        "fn helper() -> int:\n"
        "    return 1\n");
    try {
        runWithImports(
            "from aliasbimod import helper as sum\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("is reserved for a built-in"), std::string::npos)
            << "got: " << msg;
        EXPECT_NE(msg.find("'sum'"), std::string::npos) << "got: " << msg;
    }
}

TEST_F(ImportTest, ImportAliasRejectsReservedBuiltinPrintName) {
    writeFile("aliasprintmod.ry",
        "@public\n"
        "fn writer(x: int) -> int:\n"
        "    return x\n");
    try {
        runWithImports(
            "from aliasprintmod import writer as print\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("is reserved for a built-in"), std::string::npos)
            << "got: " << msg;
    }
}

// #1889 positive case: a user-defined module declaring `fn sum`
// inside a `QualifiedImportStmt`-emitted bucket compiles cleanly.
// Reaches the `fn sum` declaration with `current_namespace_target_
// != nullptr`, which skips Guard 1 (caller dispatch is always via
// `<mod>.sum(...)` through `fn_overloads`, never the hardcoded chain).
TEST_F(ImportTest, ImportAllowsReservedNameInQualifiedNamespace) {
    writeFile("nsmod.ry",
        "fn sum(x: int, y: int) -> int:\n"
        "    return x + y\n");
    EXPECT_EQ(runWithImports(
        "import nsmod\n"
        "print(nsmod.sum(2, 3))\n"), "5\n");
}

// Generic fn alias is scope-out: the instantiation cache + mangle keys are
// driven by the original name, so aliasing breaks them. Reject explicitly.
TEST_F(ImportTest, ImportAliasGenericFnRejected) {
    writeFile("genfnmod.ry",
        "@public\n"
        "fn identity<T>(x: T) -> T:\n"
        "    return x\n");
    try {
        runWithImports(
            "from genfnmod import identity as id\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("generic"), std::string::npos)
            << "got: " << msg;
        EXPECT_NE(msg.find("identity"), std::string::npos)
            << "got: " << msg;
    }
}

// Generic enum alias is scope-out for the same reasons as generic fn.
TEST_F(ImportTest, ImportAliasGenericEnumRejected) {
    writeFile("genenummod.ry",
        "@public\n"
        "enum Maybe<T>:\n"
        "    Just(value: T)\n"
        "    Nothing\n");
    try {
        runWithImports(
            "from genenummod import Maybe as M\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("generic"), std::string::npos)
            << "got: " << msg;
        EXPECT_NE(msg.find("Maybe"), std::string::npos)
            << "got: " << msg;
    }
}

// Lock in the v0.0.17 error-wording switch from "package" -> "module" so that
// any future regression that re-introduces the old wording is caught directly.
TEST_F(ImportTest, ModuleNotFoundErrorMentionsModule) {
    try {
        runWithImports("from nonexistentmodule");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("module not found"), std::string::npos)
            << "Error message should say 'module not found' (not 'package not found'): "
            << msg;
        EXPECT_EQ(msg.find("package not found"), std::string::npos)
            << "Error message should NOT contain old 'package not found' wording: " << msg;
    }
}

TEST_F(ImportTest, NameNotFoundErrorMentionsModule) {
    writeFile("mymod.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");
    try {
        runWithImports("from mymod import nope");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("not found in module"), std::string::npos)
            << "Error message should say 'not found in module' (not '... in package'): "
            << msg;
        EXPECT_EQ(msg.find("not found in package"), std::string::npos)
            << "Error message should NOT contain old 'not found in package' wording: " << msg;
    }
}

// Cross-package import of a non-@public symbol surfaces an error that
// mentions the module name (and uses 'module' rather than legacy 'package'
// wording, matching the v0.0.17 glossary in #1480).
TEST_F(ImportTest, CrossPackageImportErrorMentionsModuleNotPackage) {
    writePackageToml("");                    // caller package = tmp_dir_
    writePackageToml("ext", "extpkg");       // separate package for the importee
    writeFile("ext/privmod.ry",
        "fn hidden() -> int:\n"
        "    return 0\n");

    auto search_paths = std::vector<std::string>{(tmp_dir_ / "ext").string()};
    try {
        runWithImports("from privmod import hidden", tmp_dir_.string(), search_paths);
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("from module"), std::string::npos)
            << "Error message should say 'from module' (not 'from package'): " << msg;
        EXPECT_EQ(msg.find("from package"), std::string::npos)
            << "Error message should NOT contain old 'from package' wording: " << msg;
    }
}

TEST_F(ImportTest, TransitiveImport) {
    writeFile("base.ry",
        "fn baseFn(x: int) -> int:\n"
        "    return x + 100\n");
    writeFile("mid.ry",
        "from base\n"
        "fn midFn(x: int) -> int:\n"
        "    return baseFn(x) + 10\n");

    EXPECT_EQ(runWithImports(
        "from mid\n"
        "print(midFn(1))"),
        "111\n");
}

TEST_F(ImportTest, SearchPathRYPATH) {
    auto lib_dir = std::filesystem::temp_directory_path() / "ry_lib_test";
    std::filesystem::create_directories(lib_dir);
    {
        std::ofstream f(lib_dir / "mylib.ry");
        f << "fn greet() -> int:\n"
             "    print(999)\n"
             "    return 0\n";
    }

    EXPECT_EQ(runWithImports(
        "from mylib import greet\n"
        "greet()",
        tmp_dir_.string(),
        {lib_dir.string()}),
        "999\n");

    std::filesystem::remove_all(lib_dir);
}

// ===== directory module tests =====

TEST_F(ImportTest, DirectoryModuleImportAll) {
    writeFile("mypack/math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");
    writeFile("mypack/string.ry",
        "fn greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n");

    EXPECT_EQ(runWithImports(
        "from mypack\n"
        "print(add(1, 2))\n"
        "greet()"),
        "3\n42\n");
}

TEST_F(ImportTest, DirectoryModuleSelectiveImport) {
    writeFile("mypack2/math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");
    writeFile("mypack2/string.ry",
        "fn greet() -> int:\n"
        "    print(99)\n"
        "    return 0\n");

    EXPECT_EQ(runWithImports(
        "from mypack2 import add\n"
        "print(add(10, 20))"),
        "30\n");
}

TEST_F(ImportTest, DirectoryModuleFallbackToFile) {
    writeFile("single.ry",
        "fn solo() -> int:\n"
        "    return 7\n");

    EXPECT_EQ(runWithImports(
        "from single import solo\n"
        "print(solo())"),
        "7\n");
}

// ===== Cross-package visibility tests (#1544) =====

// Same-package callers see exportable definitions even without `@public`.
TEST_F(ImportTest, SamePackageImportSeesNonPublicSymbols) {
    writePackageToml("");
    writeFile("intra.ry",
        "fn helper() -> int:\n"
        "    return 7\n");
    EXPECT_EQ(runWithImports(
        "from intra import helper\n"
        "print(helper())"),
        "7\n");
}

// Cross-package named import of a non-@public symbol fails with an error
// that mentions @public.
TEST_F(ImportTest, CrossPackageNamedImportRequiresPublic) {
    writePackageToml("", "callerpkg");
    writePackageToml("lib", "libpkg");
    writeFile("lib/secret.ry",
        "fn helper() -> int:\n"
        "    return 7\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "lib").string()};
    try {
        runWithImports("from secret import helper", tmp_dir_.string(), search_paths);
        FAIL() << "Expected cross-package private import to fail";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("@public"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'helper'"), std::string::npos) << msg;
    }
}

// Cross-package named import of a `@public` symbol succeeds.
TEST_F(ImportTest, CrossPackagePublicSymbolImportable) {
    writePackageToml("", "callerpkg");
    writePackageToml("lib", "libpkg");
    writeFile("lib/api.ry",
        "@public\n"
        "fn helper() -> int:\n"
        "    return 7\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "lib").string()};
    EXPECT_EQ(runWithImports(
        "from api import helper\nprint(helper())",
        tmp_dir_.string(), search_paths),
        "7\n");
}

// Cross-package wildcard import includes both `@public` and non-`@public`
// exportable definitions in the importer's program (#1560 REQ-B3): the
// non-public ones must be present so a `@public` facade can call them. Name
// visibility — i.e. what the importer can write in `from foo import <name>`
// — is enforced separately by the named-import validation path.
TEST_F(ImportTest, CrossPackageWildcardImportIncludesAllExportablesForCodegen) {
    writePackageToml("", "callerpkg");
    writePackageToml("lib", "libpkg");
    writeFile("lib/api.ry",
        "@public\n"
        "fn pub() -> int:\n"
        "    return 7\n"
        "fn priv() -> int:\n"
        "    return 9\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "lib").string()};

    Program prog = resolveImportsOnly("from api\n", tmp_dir_.string(), search_paths);
    std::set<std::string> fn_names;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt))
            fn_names.insert(std::get<std::unique_ptr<FnStmt>>(stmt)->name);
    }
    EXPECT_EQ(fn_names.count("pub"), 1u);
    EXPECT_EQ(fn_names.count("priv"), 1u);
}

// Caller without a `package.toml` ancestor (sentinel / REPL-like context)
// against a rooted target package only sees `@public` symbols. Use a temp
// directory that is *not* under tmp_dir_ for the caller, since tmp_dir_
// itself becomes a package once `writePackageToml("")` is called elsewhere
// in the suite — here we want to verify the unrooted ↔ rooted boundary.
TEST_F(ImportTest, UnrootedCallerCannotSeeNonPublicFromRootedPackage) {
    writePackageToml("lib", "libpkg");
    writeFile("lib/api.ry",
        "fn helper() -> int:\n"
        "    return 7\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "lib").string()};

    auto unrooted_caller = std::filesystem::temp_directory_path() / "ry_import_unrooted";
    std::filesystem::create_directories(unrooted_caller);
    EXPECT_THROW(runWithImports(
        "from api import helper",
        unrooted_caller.string(), search_paths),
        std::runtime_error);
    std::filesystem::remove_all(unrooted_caller);
}

// ===== Directive definition (@directive) export tests (#709) =====

TEST_F(ImportTest, DirectiveDefSelectiveImport) {
    writeFile("dirmod1/mod.ry",
        "@directive(target=[\"function\"])\n"
        "fn myDir(x: int)\n"
        "fn marker() -> int:\n"
        "    return 42\n");

    EXPECT_EQ(runWithImports(
        "from dirmod1 import myDir, marker\n"
        "print(marker())"),
        "42\n");
}

TEST_F(ImportTest, DirectiveDefWildcardImport) {
    writeFile("dirmod2/mod.ry",
        "@directive(target=[\"function\"])\n"
        "fn myDir(x: int)\n"
        "fn marker() -> int:\n"
        "    return 42\n");

    EXPECT_EQ(runWithImports(
        "from dirmod2\n"
        "print(marker())"),
        "42\n");
}

// Cross-package wildcard import includes both `@public` and non-`@public`
// `@directive` defs — non-public ones are co-located in the importer's
// program for codegen (#1560 REQ-B3). Named-import validation still rejects
// `from mod import nonPubDir` cross-package (covered by the function-shaped
// CrossPackageNamedImportRequiresPublic test).
TEST_F(ImportTest, DirectiveDefCrossPackageWildcardIncludesAllForCodegen) {
    writePackageToml("", "callerpkg");
    writePackageToml("dirmod3", "dirpkg");
    writeFile("dirmod3/mod.ry",
        "@public\n"
        "@directive(target=[\"function\"])\n"
        "fn pubDir(x: int)\n"
        "@directive(target=[\"function\"])\n"
        "fn nonPubDir(x: int)\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "dirmod3").string()};

    Program prog = resolveImportsOnly("from mod\n", tmp_dir_.string(), search_paths);

    std::set<std::string> directive_names;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<DirectiveDefStmt>(stmt)) {
            directive_names.insert(std::get<DirectiveDefStmt>(stmt).name);
        }
    }
    EXPECT_EQ(directive_names.count("pubDir"), 1u);
    EXPECT_EQ(directive_names.count("nonPubDir"), 1u);
}

// ===== enum / type-alias cross-package visibility (#1544) =====

// Cross-package wildcard import includes both `@public` and non-`@public`
// enums in the importer's program for codegen (#1560 REQ-B3). Named import
// of a non-public enum still errors (EnumCrossPackageNamedImportRequiresPublic).
TEST_F(ImportTest, EnumCrossPackageWildcardIncludesAllForCodegen) {
    writePackageToml("", "callerpkg");
    writePackageToml("enumlib", "enumpkg");
    writeFile("enumlib/mod.ry",
        "@public\n"
        "enum PubColor:\n"
        "    Red\n"
        "    Green\n"
        "enum NonPubColor:\n"
        "    Yes\n"
        "    No\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "enumlib").string()};

    Program prog = resolveImportsOnly("from mod\n", tmp_dir_.string(), search_paths);

    std::set<std::string> enum_names;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<EnumStmt>(stmt))
            enum_names.insert(std::get<EnumStmt>(stmt).name);
    }
    EXPECT_EQ(enum_names.count("PubColor"), 1u);
    EXPECT_EQ(enum_names.count("NonPubColor"), 1u);
}

// Cross-package named import of a non-@public enum fails with a diagnostic
// that mentions @public.
TEST_F(ImportTest, EnumCrossPackageNamedImportRequiresPublic) {
    writePackageToml("", "callerpkg");
    writePackageToml("enumlib2", "enumpkg2");
    writeFile("enumlib2/mod.ry",
        "enum HiddenColor:\n"
        "    A\n"
        "    B\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "enumlib2").string()};
    try {
        runWithImports("from mod import HiddenColor", tmp_dir_.string(), search_paths);
        FAIL() << "Expected cross-package non-public enum import to fail";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("@public"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'HiddenColor'"), std::string::npos) << msg;
    }
}

// Cross-package wildcard import includes both `@public` and non-`@public`
// type aliases in the importer's program for codegen (#1560 REQ-B3). Named
// import of a non-public alias still errors
// (TypeAliasCrossPackageNamedImportRequiresPublic).
TEST_F(ImportTest, TypeAliasCrossPackageWildcardIncludesAllForCodegen) {
    writePackageToml("", "callerpkg");
    writePackageToml("alib", "alibpkg");
    writeFile("alib/mod.ry",
        "@public\n"
        "type PubAlias = int\n"
        "type NonPubAlias = str\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "alib").string()};

    Program prog = resolveImportsOnly("from mod\n", tmp_dir_.string(), search_paths);

    std::set<std::string> alias_names;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<TypeAliasStmt>(stmt))
            alias_names.insert(std::get<TypeAliasStmt>(stmt).name);
    }
    EXPECT_EQ(alias_names.count("PubAlias"), 1u);
    EXPECT_EQ(alias_names.count("NonPubAlias"), 1u);
}

// Cross-package named import of a non-@public type alias fails.
TEST_F(ImportTest, TypeAliasCrossPackageNamedImportRequiresPublic) {
    writePackageToml("", "callerpkg");
    writePackageToml("alib2", "alib2pkg");
    writeFile("alib2/mod.ry",
        "type HiddenAlias = int\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "alib2").string()};
    try {
        runWithImports("from mod import HiddenAlias", tmp_dir_.string(), search_paths);
        FAIL() << "Expected cross-package non-public type-alias import to fail";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("@public"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'HiddenAlias'"), std::string::npos) << msg;
    }
}

// Same-package callers see exportable enums and type aliases without `@public`.
TEST_F(ImportTest, SamePackageEnumAndTypeAliasVisibleWithoutPublic) {
    writePackageToml("");
    writeFile("intra_types.ry",
        "enum LocalColor:\n"
        "    Red\n"
        "    Green\n"
        "type LocalInt = int\n");

    Program prog = resolveImportsOnly("from intra_types\n", tmp_dir_.string(), {});

    bool saw_enum = false;
    bool saw_alias = false;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<EnumStmt>(stmt) &&
            std::get<EnumStmt>(stmt).name == "LocalColor")
            saw_enum = true;
        if (std::holds_alternative<TypeAliasStmt>(stmt) &&
            std::get<TypeAliasStmt>(stmt).name == "LocalInt")
            saw_alias = true;
    }
    EXPECT_TRUE(saw_enum);
    EXPECT_TRUE(saw_alias);
}

// ===== REQ-B3 wrapper pattern (#1560) =====
//
// A `@public` facade in a public-facing module is allowed to call a
// non-`@public` helper from the same package. The importer can `from foo
// import bas` and run `bas()`; the helper `xxx` is co-located in the same
// JIT linkage unit so codegen can resolve it, but it remains unimportable
// by name across the package boundary (REQ-A1 still enforced).

TEST_F(ImportTest, CrossPackageSelectiveWrapperFacadeCallsPrivateHelperInOtherFile) {
    writePackageToml("", "callerpkg");
    writePackageToml("foo", "foopkg");
    writeFile("foo/foo.ry",
        "from .bar import xxx\n"
        "@public\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    writeFile("foo/bar.ry",
        "fn xxx() -> int:\n"
        "    return 42\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "foo").string()};
    EXPECT_EQ(runWithImports(
        "from foo import bas\n"
        "print(bas())",
        tmp_dir_.string(), search_paths),
        "42\n");
}

TEST_F(ImportTest, CrossPackageSelectiveWrapperFacadeCallsPrivateHelperInSameFile) {
    writePackageToml("", "callerpkg");
    writePackageToml("foo2", "foo2pkg");
    writeFile("foo2/api.ry",
        "fn xxx() -> int:\n"
        "    return 7\n"
        "@public\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "foo2").string()};
    EXPECT_EQ(runWithImports(
        "from api import bas\n"
        "print(bas())",
        tmp_dir_.string(), search_paths),
        "7\n");
}

TEST_F(ImportTest, CrossPackageWildcardWrapperFacadeCallsPrivateHelper) {
    writePackageToml("", "callerpkg");
    writePackageToml("foo3", "foo3pkg");
    writeFile("foo3/api.ry",
        "fn xxx() -> int:\n"
        "    return 11\n"
        "@public\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "foo3").string()};
    EXPECT_EQ(runWithImports(
        "from api\n"
        "print(bas())",
        tmp_dir_.string(), search_paths),
        "11\n");
}

TEST_F(ImportTest, SamePackageSelectiveWrapperFacadeCallsPrivateHelper) {
    writePackageToml("");
    writeFile("api_intra.ry",
        "fn xxx() -> int:\n"
        "    return 13\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    EXPECT_EQ(runWithImports(
        "from api_intra import bas\n"
        "print(bas())"),
        "13\n");
}

TEST_F(ImportTest, SamePackageWildcardWrapperFacadeCallsPrivateHelper) {
    writePackageToml("");
    writeFile("api_intra2.ry",
        "fn xxx() -> int:\n"
        "    return 17\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    EXPECT_EQ(runWithImports(
        "from api_intra2\n"
        "print(bas())"),
        "17\n");
}

TEST_F(ImportTest, CrossPackageNamedImportOfPrivateHelperStillRejected) {
    writePackageToml("", "callerpkg");
    writePackageToml("foo4", "foo4pkg");
    writeFile("foo4/api.ry",
        "fn xxx() -> int:\n"
        "    return 19\n"
        "@public\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "foo4").string()};
    try {
        runWithImports("from api import xxx", tmp_dir_.string(), search_paths);
        FAIL() << "Expected named import of non-@public helper to fail";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("@public"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'xxx'"), std::string::npos) << msg;
    }
}

// Two importers of the same module both compile in the same program (the
// second sees the cached exports table; the first put all defs into the
// shared dest).
TEST_F(ImportTest, WrapperPatternWorksAcrossTwoImporters) {
    writePackageToml("", "callerpkg");
    writePackageToml("foo5", "foo5pkg");
    writeFile("foo5/api.ry",
        "fn xxx() -> int:\n"
        "    return 23\n"
        "@public\n"
        "fn bas() -> int:\n"
        "    return xxx()\n");
    writeFile("relay.ry",
        "from api import bas\n"
        "fn use1() -> int:\n"
        "    return bas() + 1\n");
    auto search_paths = std::vector<std::string>{(tmp_dir_ / "foo5").string()};
    EXPECT_EQ(runWithImports(
        "from relay import use1\n"
        "from api import bas\n"
        "print(use1())\n"
        "print(bas())",
        tmp_dir_.string(), search_paths),
        "24\n23\n");
}

// An imported `@directive` definition is registered in the per-program user
// directive registry, so applying it later in the importing source passes
// validation and the program compiles + runs.
TEST_F(ImportTest, ImportedDirectiveValidatesAtUseSite) {
    writeFile("dirmod4/mod.ry",
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str)\n");

    EXPECT_EQ(runWithImports(
        "from dirmod4 import logged\n"
        "@logged(\"hello\")\n"
        "fn targetFn() -> int:\n"
        "    return 7\n"
        "print(targetFn())\n"),
        "7\n");
}

// ===== testing module intrinsic import allow-list (#712) =====
//
// `from testing import expect / mock / fail` references compiler intrinsics
// that have no AST declaration in `share/std/testing/testing.ry`. The loader
// must whitelist the intrinsic names for the testing module so the named
// import does not fail at the "not found in module" check. As of #722,
// `verify` is no longer an intrinsic — it is a regular `@public fn verify`
// declared in `share/std/testing/testing.ry` and resolves via normal import.

namespace {
// Derive the repo's share/std path from the test source location so the
// loader can find `testing/testing.ry` regardless of the build cwd.
inline std::string testingStdlibSearchPath() {
    return (std::filesystem::path(__FILE__).parent_path().parent_path()
            / "share" / "std").string();
}
} // namespace

// AC #1: `from testing import expect` resolves and `expect` does not appear
// in the AST (it is a compiler intrinsic, not a declaration).
TEST_F(ImportTest, FromTestingImportExpectResolves) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    Program prog = resolveImportsOnly(
        "from testing import expect\n",
        tmp_dir_.string(),
        search_paths);

    std::set<std::string> directive_names;
    std::set<std::string> fn_names;
    for (const auto &stmt : prog) {
        if (std::holds_alternative<DirectiveDefStmt>(stmt))
            directive_names.insert(std::get<DirectiveDefStmt>(stmt).name);
        if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt))
            fn_names.insert(std::get<std::unique_ptr<FnStmt>>(stmt)->name);
    }
    // Intrinsics live in codegen, not AST.
    EXPECT_EQ(directive_names.count("expect"), 0u);
    EXPECT_EQ(fn_names.count("expect"), 0u);
    // testing.ry's @directive declarations still flow through (REQ-B3 #1560).
    EXPECT_EQ(directive_names.count("it"), 1u);
    EXPECT_EQ(directive_names.count("describe"), 1u);
}

// AC #2: all testing-module symbols (`it`, `describe`, `expect`, `mock`,
// `verify`, `fail`) are accepted as named imports. Resolution differs by
// kind: `expect` / `mock` / `fail` resolve via the intrinsic allow-list,
// `it` / `describe` via their `@directive` declarations, and `verify`
// (post-#722) via its `@public fn` declaration in testing.ry.
TEST_F(ImportTest, FromTestingImportAllTestingSymbolsResolve) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        resolveImportsOnly(
            "from testing import it, describe, expect, mock, verify, fail\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// AC #2 (boundary): the cache-hit path in `resolveImports` (line ~427)
// must apply the same intrinsic allow-list as the first-load path. Without
// allow-list parity, a wildcard `from testing` followed by a named
// `from testing import expect` throws on the second statement.
TEST_F(ImportTest, FromTestingImportExpectViaCacheHit) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        resolveImportsOnly(
            "from testing\n"
            "from testing import expect\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// #1726 (CodeRabbit): testing intrinsics are looked up via the allow-list, not
// via the AST. The alias path must reject `from testing import expect as e`
// instead of silently dropping the alias. Both load and cache-hit branches
// share this check.
TEST_F(ImportTest, FromTestingImportRejectsAliasOnLoadPath) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    try {
        resolveImportsOnly(
            "from testing import expect as e\n",
            tmp_dir_.string(),
            search_paths);
        FAIL() << "Expected alias on testing intrinsic to be rejected";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("alias not supported for testing intrinsic 'expect'"),
                  std::string::npos) << msg;
        EXPECT_NE(msg.find("#1725"), std::string::npos) << msg;
    }
}

TEST_F(ImportTest, FromTestingImportRejectsAliasOnCacheHitPath) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    try {
        // First `from testing` warms the cache; the aliased lookup must then
        // travel the cache-hit branch in resolveImports.
        resolveImportsOnly(
            "from testing\n"
            "from testing import mock as m\n",
            tmp_dir_.string(),
            search_paths);
        FAIL() << "Expected alias on testing intrinsic to be rejected (cache-hit)";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("alias not supported for testing intrinsic 'mock'"),
                  std::string::npos) << msg;
        EXPECT_NE(msg.find("#1725"), std::string::npos) << msg;
    }
}

// AC #4: a name that is neither a testing intrinsic nor a declared symbol
// is still rejected with the "not found in module" diagnostic.
TEST_F(ImportTest, FromTestingImportNonIntrinsicErrors) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    try {
        resolveImportsOnly(
            "from testing import xyz\n",
            tmp_dir_.string(),
            search_paths);
        FAIL() << "Expected non-intrinsic name to be rejected";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("'xyz'"), std::string::npos) << msg;
        EXPECT_NE(msg.find("not found in module"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'testing'"), std::string::npos) << msg;
    }
}

// Regression: a project-local `testing.ry` that shadows the stdlib testing
// module must NOT silently absorb the intrinsic allow-list. `resolve()`
// finds the local file via `referrer_dir` first, so `rp.from_stdlib` is
// false and the bypass is gated off — the missing `expect` declaration
// surfaces as the regular `'expect' not found in module 'testing'` error,
// and `imported_testing_intrinsics_` is not polluted.
TEST_F(ImportTest, FromLocalTestingShadowDoesNotBypassIntrinsicCheck) {
    // Local `testing.ry` declares no `expect` / etc.
    writeFile("testing.ry", "fn unrelated() -> int:\n    return 1\n");
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    try {
        resolveImportsOnly(
            "from testing import expect\n",
            tmp_dir_.string(),
            search_paths);
        FAIL() << "Expected local testing shadow to reject 'expect' as "
                  "not found";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("'expect'"), std::string::npos) << msg;
        EXPECT_NE(msg.find("not found in module"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'testing'"), std::string::npos) << msg;
    }
}

// Regression mirror: the wildcard-import recording path must also be gated
// on `from_stdlib`. A local shadow of `testing` must leave
// `imported_testing_intrinsics_` empty even on wildcard `from testing`.
TEST_F(ImportTest, FromLocalTestingShadowDoesNotPolluteIntrinsicSet) {
    writeFile("testing.ry", "fn unrelated() -> int:\n    return 1\n");
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from testing\n",
        tmp_dir_.string(),
        search_paths);
    EXPECT_TRUE(intrinsics.empty())
        << "local testing.ry shadow should not record stdlib intrinsics";
}

TEST_F(ImportTest, FromTestingWildcardRecordsAllIntrinsics) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from testing\n",
        tmp_dir_.string(),
        search_paths);
    // Post-#722 `verify` is a regular `@public fn verify` in
    // share/std/testing/testing.ry — it flows through the normal import
    // path and is no longer recorded in the testing-intrinsic set.
    // #1677 adds `verifyCalledWith` as an intrinsic.
    // #1683 adds `spy` as an intrinsic.
    // #1681 adds `mockReturnValueOnce` as an intrinsic.
    std::unordered_set<std::string> expected = {
        "expect", "mock", "mockReturnValueOnce", "spy", "fail", "verifyCalledWith"};
    EXPECT_EQ(intrinsics.size(), 6u);
    EXPECT_EQ(intrinsics, expected);
}

TEST_F(ImportTest, FromTestingWildcardDoesNotLeakDirectiveDecls) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from testing\n",
        tmp_dir_.string(),
        search_paths);
    EXPECT_EQ(intrinsics.count("each"), 0u);
    EXPECT_EQ(intrinsics.count("property"), 0u);
    // Post-#721: `it` / `describe` are also `@directive` decls, not
    // intrinsics. Lock that they never re-enter the intrinsic set.
    EXPECT_EQ(intrinsics.count("it"), 0u);
    EXPECT_EQ(intrinsics.count("describe"), 0u);
}

TEST_F(ImportTest, FromTestingImportSingleIntrinsicRecordsOne) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from testing import expect\n",
        tmp_dir_.string(),
        search_paths);
    std::unordered_set<std::string> expected = {"expect"};
    EXPECT_EQ(intrinsics, expected);
}

TEST_F(ImportTest, FromTestingImportMultipleIntrinsicsRecordsAll) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from testing import expect, mock, fail\n",
        tmp_dir_.string(),
        search_paths);
    std::unordered_set<std::string> expected = {"expect", "mock", "fail"};
    EXPECT_EQ(intrinsics, expected);
}

TEST_F(ImportTest, FromMathDoesNotPolluteTestingIntrinsics) {
    writeFile("mathmod.ry", "fn add(a: int, b: int) -> int:\n    return a + b\n");
    auto intrinsics = resolveAndGetTestingIntrinsics(
        "from mathmod import add\n");
    EXPECT_TRUE(intrinsics.empty());
}

TEST_F(ImportTest, CodeGenReceivesPartialTestingIntrinsics) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetCodeGenTestingIntrinsics(
        "from testing import expect, it\n",
        tmp_dir_.string(),
        search_paths);
    // `it` is a `@directive` declaration in testing.ry, not a runtime
    // intrinsic. Only `expect` (intrinsic) is recorded; `it` flows
    // through the general directive-import mechanism (#721).
    std::unordered_set<std::string> expected = {"expect"};
    EXPECT_EQ(intrinsics, expected);
}

TEST_F(ImportTest, CodeGenReceivesAllTestingIntrinsicsForWildcard) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetCodeGenTestingIntrinsics(
        "from testing\n",
        tmp_dir_.string(),
        search_paths);
    // Post-#722 `verify` is no longer a testing intrinsic (it is a regular
    // `@public fn` in share/std/testing/testing.ry).
    // #1677 adds `verifyCalledWith` as an intrinsic.
    // #1683 adds `spy` as an intrinsic.
    // #1681 adds `mockReturnValueOnce` as an intrinsic.
    std::unordered_set<std::string> expected = {
        "expect", "mock", "mockReturnValueOnce", "spy", "fail", "verifyCalledWith"};
    EXPECT_EQ(intrinsics.size(), 6u);
    EXPECT_EQ(intrinsics, expected);
}

TEST_F(ImportTest, CodeGenReceivesEmptySetWithoutTestingImport) {
    writeFile("mathmod.ry", "fn add(a: int, b: int) -> int:\n    return a + b\n");
    auto intrinsics = resolveAndGetCodeGenTestingIntrinsics(
        "from mathmod import add\n");
    EXPECT_TRUE(intrinsics.empty());
}

TEST_F(ImportTest, CodeGenReceivesNamedSubsetTestingIntrinsics) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    auto intrinsics = resolveAndGetCodeGenTestingIntrinsics(
        "from testing import expect, fail, it\n",
        tmp_dir_.string(),
        search_paths);
    // `it` is a `@directive` declaration in testing.ry; only `expect`
    // and `fail` are runtime intrinsics (#721).
    std::unordered_set<std::string> expected = {"expect", "fail"};
    EXPECT_EQ(intrinsics, expected);
}

// Qualified import — user-defined module call works (#1730).
// Previously rejected as a v0.0.23 limitation; with the namespace-isolation
// rewrite (#1730), `import usermod` + `usermod.greet()` resolves through the
// per-module namespace bucket on CodeGen.
TEST_F(ImportTest, QualifiedImportUserDefinedModuleCallWorks) {
    writeFile("mymod.ry",
        "fn greet() -> int:\n"
        "    return 7\n");
    EXPECT_EQ(runWithImports(
        "import mymod\n"
        "print(mymod.greet())\n"), "7\n");
}

// Qualified import — user-defined module field access works (#1730).
// `@const` declarations now route through `ModuleNamespaceInfo::consts`
// when emitted under `NamespaceEmitScope`; qualified field-access reads
// from that bucket via the namespace `consts` map.
TEST_F(ImportTest, QualifiedImportUserDefinedModuleFieldWorks) {
    writeFile("constmod2.ry",
        "@directive(target=[\"statement\"])\n"
        "fn const()\n"
        "@const\n"
        "ANSWER: int = 42\n");
    EXPECT_EQ(runWithImports(
        "import constmod2\n"
        "print(constmod2.ANSWER)\n"), "42\n");
}

// Qualified import alias — user-defined module call works with alias (#1730).
// `import mymod_alias_call as m` registers the module under effective alias `m`
// while preserving `original_name = "mymod_alias_call"`; the qualified call
// `m.greet()` dispatches through the namespace bucket keyed by `m`.
TEST_F(ImportTest, QualifiedImportAliasUserDefinedModuleCallWorks) {
    writeFile("mymod_alias_call.ry",
        "fn greet() -> int:\n"
        "    return 7\n");
    EXPECT_EQ(runWithImports(
        "import mymod_alias_call as m\n"
        "print(m.greet())\n"), "7\n");
}

// Qualified import alias — user-defined module field access works with alias (#1730).
TEST_F(ImportTest, QualifiedImportAliasUserDefinedFieldWorks) {
    writeFile("mymod_alias_field.ry",
        "@directive(target=[\"statement\"])\n"
        "fn const()\n"
        "@const\n"
        "ANSWER: int = 42\n");
    EXPECT_EQ(runWithImports(
        "import mymod_alias_field as m\n"
        "print(m.ANSWER)\n"), "42\n");
}

// Qualified import — user-defined module must NOT leak bare names into caller (#1723, CR review).
// Triggers the module_loader.cpp branch that routes user-defined defs to a throwaway
// Program (instead of inlining them into the caller) so `import usermod\ngreet()`
// fails with the regular `undefined function: greet` resolver error rather than
// silently succeeding.
TEST_F(ImportTest, QualifiedImportUserDefinedDoesNotLeakBareNames) {
    writeFile("leakmod.ry",
        "fn greet() -> int:\n"
        "    return 7\n");
    try {
        runWithImports(
            "import leakmod\n"
            "print(greet())\n");
        FAIL() << "Expected exception (bare 'greet' must not resolve through qualified import)";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("undefined function: greet"), std::string::npos)
            << "got: " << msg;
    }
}

// Qualified import — user-defined module `@const` must NOT leak bare names
// into caller (#1730 AC4). Mirrors `QualifiedImportUserDefinedDoesNotLeakBareNames`
// but for `@const` declarations. `AssignStmt` is the only namespace-routed kind
// that flows through `emitVarDecl` (which adds to `scope_stack_[0]` unconditionally);
// without the scope_stack_[0]/immutable_scope_stack_[0] cleanup after
// `registerModuleGlobal`, a bare reference to the namespaced const would resolve
// via the top-level scope alloca and break AC4 isolation.
TEST_F(ImportTest, QualifiedImportUserDefinedDoesNotLeakBareConst) {
    writeFile("leakconst.ry",
        "@directive(target=[\"statement\"])\n"
        "fn const()\n"
        "@const\n"
        "ANSWER: int = 42\n");
    try {
        runWithImports(
            "import leakconst\n"
            "print(ANSWER)\n");
        FAIL() << "Expected exception (bare 'ANSWER' must not resolve through qualified import)";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("ANSWER"), std::string::npos) << "got: " << msg;
    }
}

// Qualified import — generic functions in user-defined modules are explicitly
// rejected with an actionable error (#1730 scope-out).
// Generic fn declarations route through `generic_fn_templates_` flat-table,
// which the per-module namespace bucket cannot intercept; reject under
// `NamespaceEmitScope` with a redirect to the `from <mod> import <fn>` form.
TEST_F(ImportTest, QualifiedImportUserDefinedGenericFnRejected) {
    writeFile("genmod.ry",
        "fn id<T>(x: T) -> T:\n"
        "    return x\n");
    try {
        runWithImports(
            "import genmod\n"
            "print(genmod.id(7))\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("generic functions in qualified-imported user-defined modules"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// Qualified import — `enum` declarations in user-defined modules are
// explicitly rejected (#1730 scope-out). Enums register via the flat
// `enum_types_` table; the per-module namespace bucket has no slot for
// them. Reject under `NamespaceEmitScope` with an actionable error.
TEST_F(ImportTest, QualifiedImportUserDefinedEnumRejected) {
    writeFile("enummod.ry",
        "enum Color:\n"
        "    Red\n"
        "    Green\n"
        "    Blue\n");
    try {
        runWithImports(
            "import enummod\n"
            "print(0)\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("enum declarations in qualified-imported user-defined modules"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// Qualified import — `type` aliases in user-defined modules are
// explicitly rejected (#1730 scope-out). Type aliases register via the
// flat `type_aliases_` table; the per-module namespace bucket has no slot
// for them. Reject under `NamespaceEmitScope` with an actionable error.
TEST_F(ImportTest, QualifiedImportUserDefinedTypeAliasRejected) {
    writeFile("aliasmod.ry",
        "type Age = int\n");
    try {
        runWithImports(
            "import aliasmod\n"
            "print(0)\n");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("type alias declarations in qualified-imported user-defined modules"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// Qualified import — `import usermod` followed by `from usermod import foo`
// must reuse the cached AST so the selective form sees `foo` (AC3 from #1730).
TEST_F(ImportTest, QualifiedImportThenSelectiveImportReusesCache) {
    writeFile("cachemod.ry",
        "fn greet() -> int:\n"
        "    return 42\n");
    EXPECT_EQ(runWithImports(
        "import cachemod\n"
        "from cachemod import greet\n"
        "print(greet())\n"
        "print(cachemod.greet())\n"), "42\n42\n");
}

// Qualified import — record constructor in user-defined module works (#1730).
// `usermod.MyRecord(...)` resolves through `ModuleNamespaceInfo::records`
// when `NamespaceEmitScope` reroutes record registration during import emission.
// Note: qualified type annotations (`p: recmod.Point = ...`) are not yet
// supported by the parser; rely on inference for the let binding.
TEST_F(ImportTest, QualifiedImportUserDefinedRecordConstructor) {
    writeFile("recmod.ry",
        "record Point:\n"
        "    x: int\n"
        "    y: int\n");
    EXPECT_EQ(runWithImports(
        "import recmod\n"
        "p = recmod.Point(3, 4)\n"
        "print(p.x)\n"
        "print(p.y)\n"), "3\n4\n");
}

// ===== #1888: import of C++-registered resource kinds and builtin records =====
//
// Resource kinds (File / TcpListener / Thread / ...) and the builtin record
// Match (regex) are registered programmatically in C++ rather than declared
// in their stdlib `.ry` files. `from io import File` etc. must resolve
// because the names are semantically part of those modules even though
// `extractDefinitions` cannot see them via AST scan.
TEST_F(ImportTest, FromCppResourceKindImportsResolve) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    struct Case { const char *mod; const char *type; };
    const Case cases[] = {
        {"io", "File"},
        {"net", "TcpListener"},
        {"net", "TcpStream"},
        {"http", "TlsStream"},
        {"http", "HttpRequest"},
        {"http", "HttpResponse"},
        {"http", "HttpClientResponse"},
        {"thread", "Thread"},
        {"thread", "Lock"},
        {"thread", "RWLock"},
        {"thread", "Semaphore"},
        {"thread", "Barrier"},
        {"thread", "AtomicInt"},
        {"thread", "AtomicBool"},
    };
    for (const auto &c : cases) {
        std::string src = std::string("from ") + c.mod + " import " + c.type + "\n";
        EXPECT_NO_THROW({
            resolveImportsOnly(src, tmp_dir_.string(), search_paths);
        }) << "case: " << src;
    }
}

TEST_F(ImportTest, FromRegexImportMatchResolves) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        resolveImportsOnly(
            "from regex import Match\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// Cache-hit boundary: a second import in the same loader must use the
// `resolveImports` cache-hit branch (L778-803), which has its own throw site.
TEST_F(ImportTest, FromCppResourceKindResolvesViaCacheHit) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        resolveImportsOnly(
            "from io\n"
            "from io import File\n",
            tmp_dir_.string(),
            search_paths);
    });
}

TEST_F(ImportTest, FromRegexImportMatchResolvesViaCacheHit) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        resolveImportsOnly(
            "from regex\n"
            "from regex import Match\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// Negative: wrong module rejects (TcpListener belongs to "net", not "io").
TEST_F(ImportTest, FromIoImportTcpListenerStillRejected) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_THROW({
        resolveImportsOnly(
            "from io import TcpListener\n",
            tmp_dir_.string(),
            search_paths);
    }, std::runtime_error);
}

// Negative: unknown type still rejected.
TEST_F(ImportTest, FromIoImportUnknownTypeStillRejected) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_THROW({
        resolveImportsOnly(
            "from io import NotARealType\n",
            tmp_dir_.string(),
            search_paths);
    }, std::runtime_error);
}

// Negative: Error type is global, not associated with any module.
// Importing it from io (or anywhere) must remain rejected.
TEST_F(ImportTest, FromIoImportErrorStillRejected) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_THROW({
        resolveImportsOnly(
            "from io import Error\n",
            tmp_dir_.string(),
            search_paths);
    }, std::runtime_error);
}

// Symmetry with isTestingIntrinsic's from_stdlib guard: a local `io.ry`
// shadow must NOT bypass the C++ type check. Importing File from a local
// io.ry that does not declare File should fail with the standard
// "not found in module" diagnostic.
TEST_F(ImportTest, FromLocalIoShadowDoesNotBypassCppTypeCheck) {
    writeFile("io.ry", "fn dummy() -> int:\n    return 0\n");
    EXPECT_THROW({
        resolveImportsOnly(
            "from io import File\n",
            tmp_dir_.string(),
            {});
    }, std::runtime_error);
}

// Alias of C++-registered resource kind: `from io import File as MyFile`
// must (a) parse, (b) reach codegen without throwing, and (c) allow the
// aliased name to be used in a type annotation that propagates resource
// metadata for ARC cleanup.
TEST_F(ImportTest, ImportAliasResourceTypeWorks) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        runWithImports(
            "from io import File as MyFile\n"
            "fn use(f: MyFile) -> bool:\n"
            "    return true\n",
            tmp_dir_.string(),
            search_paths);
    });
}

TEST_F(ImportTest, ImportAliasResourceTypeWorksCacheHit) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        runWithImports(
            "from io\n"
            "from io import File as MyFile\n"
            "fn use(f: MyFile) -> bool:\n"
            "    return true\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// Alias of C++-registered builtin record (Match).
TEST_F(ImportTest, ImportAliasBuiltinRecordWorks) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        runWithImports(
            "from regex import Match as M\n"
            "fn use(m: M) -> str:\n"
            "    return m.full\n",
            tmp_dir_.string(),
            search_paths);
    });
}

TEST_F(ImportTest, ImportAliasBuiltinRecordWorksCacheHit) {
    auto search_paths = std::vector<std::string>{testingStdlibSearchPath()};
    EXPECT_NO_THROW({
        runWithImports(
            "from regex\n"
            "from regex import Match as M\n"
            "fn use(m: M) -> str:\n"
            "    return m.full\n",
            tmp_dir_.string(),
            search_paths);
    });
}

// ===== type alias =====

TEST_F(CodeGenTest, TypeAlias) {
    std::string src =
        "type MyInt = int\n"
        "fn add(a: MyInt, b: MyInt) -> MyInt:\n"
        "    return a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, TypeAliasLeaflessCycleRejected) {
    // `type A = B | C; type B = A; type C = A` — every expansion path
    // cycles back into A with no concrete leaf ever reached. Must reject
    // with a "Circular type alias" error instead of returning an empty
    // flattened name (#835 follow-up).
    std::string src =
        "type A = B | C\n"
        "type B = A\n"
        "type C = A\n"
        "x: A = 42\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAliasLiteralUnionMetaDoesNotCrashOnReassignment) {
    // Reassigning through an alias whose flattened canonical form is a
    // literal union (`"N" | "S"`) must not crash wrapInUnion. Literal
    // unions do not produce union_type_info_ entries, so the metadata
    // write is skipped and reassignment to a mismatched type should
    // surface the normal "cannot be reassigned" diagnostic (#835
    // follow-up).
    std::string src =
        "type LitDir = \"N\" | \"S\"\n"
        "type Alias = LitDir | \"N\"\n"
        "x: Alias = \"N\"\n"
        "x = 42\n";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== for k, v in map / range =====

TEST_F(CodeGenTest, ForKVInMap) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "total = 0\n"
        "for k, v in m:\n"
        "    total = total + v\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, RangeExprVariants) {
    // RangeExpr
    EXPECT_EQ(runSource(
        "xs = 1 .. 5\n"
        "print(len(xs))"), "5\n");
    // RangeExprIterate
    EXPECT_EQ(runSource(
        "total = 0\n"
        "for x in 1 .. 3:\n"
        "    total = total + x\n"
        "print(total)"), "6\n");
}

// ===== Concurrency: async/await =====

TEST_F(CodeGenTest, AsyncAwaitBasics) {
    // blockOn awaits direct async call
    EXPECT_EQ(runSource(
        "async fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(blockOn(add(20, 22)))"), "42\n");
    // blockOn awaits task variable
    EXPECT_EQ(runSource(
        "async fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "t: Task<int> = add(7, 8)\n"
        "print(blockOn(t))"), "15\n");
    // await chains inside async fn, blockOn at top level
    EXPECT_EQ(runSource(
        "async fn inner() -> int:\n"
        "    return 21\n"
        "async fn outer() -> int:\n"
        "    return (await inner()) * 2\n"
        "print(blockOn(outer()))"), "42\n");
}

TEST_F(CodeGenTest, AsyncStatementForms) {
    // blockOn Unit-returning async
    EXPECT_EQ(runSource(
        "async fn bump() -> Unit:\n"
        "    print(\"done\")\n"
        "blockOn(bump())\n"
        "print(\"after\")"), "done\nafter\n");
    // blockOn discards value
    EXPECT_EQ(runSource(
        "async fn add(a: int, b: int) -> int:\n"
        "    print(a + b)\n"
        "    return a + b\n"
        "blockOn(add(20, 22))\n"
        "print(\"after\")"), "42\nafter\n");
}

TEST_F(CodeGenTest, AwaitErrors) {
    // await outside async fn is a parse error
    EXPECT_THROW(runSource("async fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = await add(1, 2)\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("async fn bump() -> Unit:\n"
        "    print(\"done\")\n"
        "await bump()"), std::runtime_error);
    // blockOn requires Task
    EXPECT_THROW(runSource("x = blockOn(123)\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, AvailableParallelismBuiltin) {
    EXPECT_EQ(runSource("print(availableParallelism() > 0)"), "true\n");
}

// ===== Parallel for =====

// TODO: print output interleaves across threads (#473)
// TEST_F(CodeGenTest, ParallelForBasics) {
//     EXPECT_EQ(runSource(
//         "fn one(x: int) -> int:\n"
//         "    return 1\n"
//         "@parallel\n"
//         "for i in range(5):\n"
//         "    print(one(i))"), "1\n1\n1\n1\n1\n");
// }
TEST_F(CodeGenTest, ParallelForPrintComposite) {
    std::string result = runSource(withStdlibDirectiveDecls(
        "@parallel\n"
        "for i in range(3):\n"
        "    print([i, i])"));
    // Output lines are unordered across threads, but each line must be well-formed
    std::istringstream ss(result);
    std::string line;
    std::vector<std::string> lines;
    while (std::getline(ss, line)) lines.push_back(line);
    EXPECT_EQ(lines.size(), 3u);
    std::sort(lines.begin(), lines.end());
    EXPECT_EQ(lines[0], "[0, 0]");
    EXPECT_EQ(lines[1], "[1, 1]");
    EXPECT_EQ(lines[2], "[2, 2]");
}

TEST_F(CodeGenTest, ParallelForErrors) {
    // ParallelForRejectsOuterMutation
    EXPECT_THROW(runSource(withStdlibDirectiveDecls(
        "total = 0\n"
        "@parallel\n"
        "for i in range(4):\n"
        "    total = total + i\n"
        "print(total)")), std::runtime_error);
    // ParallelForRejectsNestedFunctionDef (#1118)
    EXPECT_THROW(runSource(withStdlibDirectiveDecls(
        "@parallel\n"
        "for i in range(4):\n"
        "    fn helper() -> int:\n"
        "        return i\n"
        "    print(helper())\n")), std::runtime_error);
}

// ===== Int literal type =====

TEST_F(CodeGenTest, IntLiteralType) {
    // IntLiteralTypeSingle
    EXPECT_EQ(runSource("x: 42 = 42\nprint(x)"), "42\n");
    // IntLiteralTypeUnionSuccess
    EXPECT_EQ(runSource("x: 0 | 1 = 0\nprint(x)"), "0\n");
    // IntLiteralTypeUnionFail
    EXPECT_THROW(runSource("x: 0 | 1 = 2"), std::runtime_error);
    // IntLiteralTypeVarReassignSuccess
    EXPECT_EQ(runSource("x: 0 | 1 = 0\nx = 1\nprint(x)"), "1\n");
    // IntLiteralTypeVarReassignConstFail
    EXPECT_THROW(runSource("x: 0 | 1 = 0\nx = 2"), std::runtime_error);
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn getTwo() -> int:\n"
        "    return 2\n"
        "x: 0 | 1 = 0\n"
        "x = getTwo()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== Range type =====

TEST_F(CodeGenTest, RangeType) {
    // RangeTypeSuccess
    EXPECT_EQ(runSource("month: 1..12 = 6\nprint(month)"), "6\n");
    // RangeTypeTooLow
    EXPECT_THROW(runSource("month: 1..12 = 0"), std::runtime_error);
    // RangeTypeTooHigh
    EXPECT_THROW(runSource("month: 1..12 = 13"), std::runtime_error);
    // RangeTypeBoundaryLow
    EXPECT_EQ(runSource("x: 1..12 = 1\nprint(x)"), "1\n");
    // RangeTypeBoundaryHigh
    EXPECT_EQ(runSource("x: 1..12 = 12\nprint(x)"), "12\n");
    // RangeTypeNegative
    EXPECT_EQ(runSource("t: -10..10 = -5\nprint(t)"), "-5\n");
    // RangeTypeVarReassignRuntime
    EXPECT_EQ(runSource("x: 1..12 = 6\nx = 12\nprint(x)"), "12\n");
    // RangeTypeVarReassignConstFail
    EXPECT_THROW(runSource("x: 1..12 = 6\nx = 13"), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeVarReassignDynamicFail) {
    std::string src =
        "fn getThirteen() -> int:\n"
        "    return 13\n"
        "x: 1..12 = 6\n"
        "x = getThirteen()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== Str literal type =====

TEST_F(CodeGenTest, StrLiteralType) {
    // StrLiteralTypeSuccess
    EXPECT_EQ(runSource("dir: \"N\" | \"S\" | \"E\" | \"W\" = \"N\"\nprint(dir)"), "N\n");
    // StrLiteralTypeFail
    EXPECT_THROW(runSource("dir: \"N\" | \"S\" | \"E\" | \"W\" = \"X\""), std::runtime_error);
    // StrLiteralTypeVarReassignSuccess
    EXPECT_EQ(runSource("dir: \"N\" | \"S\" = \"N\"\ndir = \"S\"\nprint(dir)"), "S\n");
}

TEST_F(CodeGenTest, StrLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn getX() -> str:\n"
        "    return \"X\"\n"
        "dir: \"N\" | \"S\" = \"N\"\n"
        "dir = getX()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== Type alias with refined types =====

TEST_F(CodeGenTest, TypeAliasRefinedTypes) {
    // TypeAliasRangeType
    EXPECT_EQ(runSource(
        "type Month = 1..12\n"
        "m: Month = 6\n"
        "print(m)"), "6\n");
    // TypeAliasRangeTypeFail
    EXPECT_THROW(runSource(
        "type Month = 1..12\n"
        "m: Month = 13"), std::runtime_error);
    // TypeAliasStrLiteralType
    EXPECT_EQ(runSource(
        "type Direction = \"N\" | \"S\" | \"E\" | \"W\"\n"
        "d: Direction = \"N\"\n"
        "print(d)"), "N\n");
    // TypeAliasIntLiteralType
    EXPECT_EQ(runSource(
        "type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n"
        "d: Digit = 5\n"
        "print(d)"), "5\n");
}

// ===== Function type alias =====

TEST_F(CodeGenTest, TypeAliasFnType) {
    // TypeAliasFnType
    EXPECT_EQ(runSource(
        "type Callback = fn(int, int) -> int\n"
        "add: Callback = (a: int, b: int) => a + b\n"
        "print(add(3, 4))"), "7\n");
    // TypeAliasFnTypeParam
    EXPECT_EQ(runSource(
        "type BinOp = fn(int, int) -> int\n"
        "fn apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "res = apply((x: int, y: int) => x + y, 10, 20)\n"
        "print(res)"), "30\n");
    // TypeAliasFnTypeLambdaParam
    EXPECT_EQ(runSource(
        "type Mapper = fn(int) -> int\n"
        "apply = (f: Mapper, x: int) => f(x)\n"
        "print(apply((n: int) => n * 3, 7))"), "21\n");
    // TypeAliasFnTypeNamedFn
    EXPECT_EQ(runSource(
        "type BinOp = fn(int, int) -> int\n"
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "print(apply(add, 5, 6))"), "11\n");
}

// ===== Function parameter constraints =====

TEST_F(CodeGenTest, FnParamConstraints) {
    // FnParamRangeTypeSuccess
    EXPECT_EQ(runSource(
        "fn setMonth(m: 1..12) -> int:\n"
        "    return m\n"
        "print(setMonth(6))"), "6\n");
    // FnParamRangeTypeConstFail
    EXPECT_THROW(runSource(
        "fn setMonth(m: 1..12) -> int:\n"
        "    return m\n"
        "setMonth(13)"), std::runtime_error);
    // FnParamIntLiteralSuccess
    EXPECT_EQ(runSource(
        "fn f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "print(f(1))"), "1\n");
    // FnParamIntLiteralFail
    EXPECT_THROW(runSource(
        "fn f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "f(2)"), std::runtime_error);
}

// ===== Ellipsis =====

TEST_F(CodeGenTest, EllipsisVariants) {
    // EllipsisNoOp
    EXPECT_EQ(runSource("..."), "");
    // EllipsisFnBody
    EXPECT_EQ(runSource(
        "fn stub() -> Unit:\n"
        "    ...\n"
        "stub()\n"
        "print(1)"), "1\n");
    // EllipsisIfElse
    EXPECT_EQ(runSource(
        "if true:\n"
        "    ...\n"
        "else:\n"
        "    ...\n"
        "print(42)"), "42\n");
    // EllipsisWhile
    EXPECT_EQ(runSource(
        "i = 0\n"
        "while i < 3:\n"
        "    ...\n"
        "    i += 1\n"
        "print(i)"), "3\n");
    // EllipsisFor
    EXPECT_EQ(runSource(
        "for i in range(3):\n"
        "    ...\n"
        "print(\"done\")"), "done\n");
}

// ===== Short-circuit evaluation =====

TEST_F(CodeGenTest, ShortCircuitEvaluation) {
    // AndShortCircuitFalse
    EXPECT_EQ(runSource(
        "fn sideEffect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false and sideEffect()\n"
        "print(res)"), "false\n");
    // AndShortCircuitTrue
    EXPECT_EQ(runSource(
        "fn sideEffect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = true and sideEffect()\n"
        "print(res)"), "side\ntrue\n");
    // OrShortCircuitTrue
    EXPECT_EQ(runSource(
        "fn sideEffect() -> bool:\n"
        "    print(\"side\")\n"
        "    return false\n"
        "res = true or sideEffect()\n"
        "print(res)"), "true\n");
    // OrShortCircuitFalse
    EXPECT_EQ(runSource(
        "fn sideEffect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false or sideEffect()\n"
        "print(res)"), "side\ntrue\n");
}

// ===== Default arguments =====

TEST_F(CodeGenTest, DefaultArgBasic) {
    EXPECT_EQ(runSource(
        "fn greet(name: str, greeting: str = \"Hello\") -> str:\n"
        "    return greeting + \", \" + name\n"
        "print(greet(\"Alice\"))\n"
        "print(greet(\"Alice\", \"Hi\"))"), "Hello, Alice\nHi, Alice\n");
}

TEST_F(CodeGenTest, DefaultArgMultiple) {
    EXPECT_EQ(runSource(
        "fn f(a: int, b: int = 10, c: int = 20) -> int:\n"
        "    return a + b + c\n"
        "print(f(1))\n"
        "print(f(1, 2))\n"
        "print(f(1, 2, 3))"), "31\n23\n6\n");
}

TEST_F(CodeGenTest, DefaultArgOverloadConflict) {
    EXPECT_THROW(runSource(
        "fn calc(x: int, y: int = 0) -> int:\n"
        "    return x + y\n"
        "fn calc(x: int) -> int:\n"
        "    return x * 2\n"
        "print(calc(1))"), std::runtime_error);
}

TEST_F(CodeGenTest, DefaultArgWithWidening) {
    EXPECT_EQ(runSource(
        "fn calc(x: float, precision: int = 6) -> float:\n"
        "    return x\n"
        "print(calc(3))"), "3.0\n");
}

TEST_F(CodeGenTest, DefaultArgGenericError) {
    EXPECT_THROW(runSource(
        "fn identity<T>(x: T, label: str = \"default\") -> T:\n"
        "    return x\n"
        "print(identity(42))"), std::runtime_error);
}

// ===== unimported stdlib module diagnostic (#1746) =====
//
// `<mod>.fn(...)` / `<mod>.field` where `<mod>` names a registered
// stdlib package but the user forgot to `import <mod>` must produce an
// actionable error pointing at the missing import. The check fires at
// codegen-dispatch time (parser cannot discriminate between an
// unimported stdlib module and a local that happens to share its name,
// because parser has no scope info; codegen does, via `findVar`).
//
// `import <mod>` / `import <mod> as <alias>` requires ModuleLoader to
// run, which `runSource` does not invoke (see
// .claude/rules/tests-cpp-conventions.md "CodeGenTest::runSource cannot
// compile code that imports stdlib modules"). The alias-then-bare-call
// regression case (`import math as m; math.sqrt(2.0)`) is therefore
// covered by smoke tests in the PR description rather than here.
// #1747's alias-suggestion variant (`'math' is not defined. Did you
// mean 'm' (aliased from 'math')?`) inherits the same harness
// limitation and is likewise smoke-tested only.

TEST_F(CodeGenTest, UnimportedStdlibCallExprEmitsError) {
    expectCompileError("x = math.sqrt(4.0)\n",
                       "module 'math' is not imported");
}

TEST_F(CodeGenTest, UnimportedStdlibCallStmtEmitsError) {
    expectCompileError("math.sqrt(4.0)\n",
                       "module 'math' is not imported");
}

TEST_F(CodeGenTest, UnimportedStdlibFieldAccessEmitsError) {
    expectCompileError("x = math.PI\n",
                       "module 'math' is not imported");
}

TEST_F(CodeGenTest, UnimportedStdlibCallExprIncludesImportHint) {
    expectCompileError("x = json.parse(\"{}\")\n",
                       "add 'import json'");
}

// Regression guard: a local variable whose name happens to match a
// stdlib package shadows the package, and the new diagnostic must NOT
// fire. The existing "undefined function: basename" / "undefined
// variable: ..." paths remain authoritative for these cases.
TEST_F(CodeGenTest, LocalShadowingStdlibPackageNameDoesNotTriggerImportError) {
    try {
        runSource("path: str = \"/tmp/foo.txt\"\nx = path.basename()\n");
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_EQ(msg.find("module 'path' is not imported"), std::string::npos)
            << "local-shadow guard failed; got: " << msg;
    }
}

// Regression guard: non-stdlib `<ident>.<call>` continues to flow
// through the existing UFCS-lowering / "undefined function" path.
TEST_F(CodeGenTest, NonStdlibUfcsStillRoutesToUndefinedFunction) {
    try {
        runSource("x = foo.bar(1)\n");
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_EQ(msg.find("module 'foo' is not imported"), std::string::npos)
            << "non-stdlib name must not trip the import diagnostic; got: " << msg;
    }
}

// #1883: typed collection `any` assignment gate. `case Ok(v):` for
// `Result<any, _>` binds `v` with no source-level collection type info, so
// allowing `xs: List<str> = v` silently re-strides the any payload as native
// (SIGSEGV / silent corruption). Require the source type name to match the
// annotation; otherwise reject with a `load[T]` hint.
// NB: `Ok(42)` does not auto-wrap into `any` at the Result-construction
// boundary, so the helper wraps explicitly via `a: any = 42; return Ok(a)`
// before returning.
TEST_F(CodeGenTest, AnyToTypedListStrFromResultRejected) {
    expectCompileError(
        "fn produceAny() -> Result<any, str>:\n"
        "    a: any = 42\n"
        "    return Ok(a)\n"
        "case produceAny():\n"
        "    Ok(v):\n"
        "        xs: List<str> = v\n"
        "    Err(_):\n"
        "        x = 0\n",
        "load");
}

TEST_F(CodeGenTest, AnyToTypedMapStrIntFromResultRejected) {
    expectCompileError(
        "fn produceAny() -> Result<any, str>:\n"
        "    a: any = 42\n"
        "    return Ok(a)\n"
        "case produceAny():\n"
        "    Ok(v):\n"
        "        m: Map<str, int> = v\n"
        "    Err(_):\n"
        "        x = 0\n",
        "load");
}

TEST_F(CodeGenTest, AnyToTypedSetIntFromResultRejected) {
    expectCompileError(
        "fn produceAny() -> Result<any, str>:\n"
        "    a: any = 42\n"
        "    return Ok(a)\n"
        "case produceAny():\n"
        "    Ok(v):\n"
        "        s: Set<int> = v\n"
        "    Err(_):\n"
        "        x = 0\n",
        "load");
}

// Whitelist: `List<any>` / `Set<any>` / `Map<str, any>` are always safe — the
// any payload's stride matches the destination directly.
TEST_F(CodeGenTest, AnyToListAnyAllowedFromResult) {
    EXPECT_NO_THROW(compileSource(
        "fn produceAny() -> Result<any, str>:\n"
        "    xs: List<any> = [1, 2, 3]\n"
        "    a: any = xs\n"
        "    return Ok(a)\n"
        "case produceAny():\n"
        "    Ok(v):\n"
        "        xs: List<any> = v\n"
        "    Err(_):\n"
        "        x = 0\n"));
}

TEST_F(CodeGenTest, AnyToMapStrAnyAllowedFromResult) {
    EXPECT_NO_THROW(compileSource(
        "fn produceAny() -> Result<any, str>:\n"
        "    a: any = 42\n"
        "    return Ok(a)\n"
        "case produceAny():\n"
        "    Ok(v):\n"
        "        m: Map<str, any> = v\n"
        "    Err(_):\n"
        "        x = 0\n"));
}

// Regression guard: the legitimate roundtrip `a: any = xs; ys: List<int> = a`
// must keep working — `a`'s alloca stamps `source_type_name = "List<int>"`
// via `registerAnyManagedVar`, so the gate permits the unwrap.
TEST_F(CodeGenTest, AnyToTypedListIntRoundtripStillWorks) {
    EXPECT_EQ(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "a: any = xs\n"
        "ys: List<int> = a\n"
        "print(ys)\n"), "[1, 2, 3]\n");
}

// Regression guard mirroring tests/spec/any.test.ry's
// `shouldPreserveListThroughAnyRoundtripInFn`: when a value flows through a
// function whose return type is `any`, the call-site sourceName is "any"
// (ambiguous). The gate must NOT treat this as a concrete mismatch — defer to
// unwrapFromAny's runtime tag check. Catching the empty/concrete-mismatch
// hazard while permitting the ambiguous-any path keeps shipped semantics.
TEST_F(CodeGenTest, AnyToTypedListIntThroughAnyReturningFnStillWorks) {
    EXPECT_EQ(runSource(
        "fn identityAny(v: any) -> any:\n"
        "    return v\n"
        "xs: List<int> = [10, 20]\n"
        "a: any = identityAny(xs)\n"
        "ys: List<int> = a\n"
        "print(ys)\n"), "[10, 20]\n");
}

// Direct triggers for the gate's second branch
// (`resolvedSource != resolvedColl`). The empty-source branch above is covered
// by the FromResult cases; without these tests, a refactor that drops the
// concrete-mismatch check (but keeps the empty check) would pass the existing
// suite. See .claude/rules/tests-rejection-tdd.md.
TEST_F(CodeGenTest, AnyToTypedListConcreteSourceMismatchRejected) {
    expectCompileError(
        "xs: List<int> = [1, 2, 3]\n"
        "a: any = xs\n"
        "ys: List<str> = a\n",
        "load");
}

TEST_F(CodeGenTest, AnyToTypedMapConcreteSourceMismatchRejected) {
    expectCompileError(
        "m: Map<str, int> = {\"k\": 1}\n"
        "a: any = m\n"
        "n: Map<str, str> = a\n",
        "load");
}

TEST_F(CodeGenTest, AnyToTypedSetConcreteSourceMismatchRejected) {
    expectCompileError(
        "s: Set<int> = {1, 2, 3}\n"
        "a: any = s\n"
        "t: Set<str> = a\n",
        "load");
}

// #1884: when LHS does NOT pin a `*_any` axis, the strict per-element type
// check must still fire. Guards against accidental hint leakage that would
// silently legalise mixed-type literals under typed annotations.
TEST_F(CodeGenTest, MapValueTypeMismatchUnderTypedAnnotationRejected) {
    expectCompileError(
        "m: Map<str, int> = {\"a\": 1, \"b\": \"two\"}\n",
        "map values must all have the same type");
}

TEST_F(CodeGenTest, ListElementTypeMismatchUnderTypedAnnotationRejected) {
    expectCompileError(
        "xs: List<int> = [1, \"x\"]\n",
        "list elements must all have the same type");
}

TEST_F(CodeGenTest, SetElementTypeMismatchUnderTypedAnnotationRejected) {
    expectCompileError(
        "s: Set<int> = {1, \"x\"}\n",
        "set elements must all have the same type");
}

// Map<any, V> is intentionally out of scope for #1884: the rehash dispatch has
// no anyTy_ (16B struct) variant. Mixed key types under Map<any, V> must
// continue to fail at the strict same-type check.
TEST_F(CodeGenTest, MapKeyTypeMismatchUnderAnyAnnotationRejected) {
    expectCompileError(
        "m: Map<any, str> = {1: \"a\", \"two\": \"b\"}\n",
        "map keys must all have the same type");
}
