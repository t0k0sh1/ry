#include "test_codegen_common.hpp"
#include "ry/module_loader.hpp"
#include <filesystem>
#include <fstream>

// ===== Ternary operator =====

TEST_F(CodeGenTest, TernaryBasicTrue) {
    std::string src =
        "x = true ? 1 : 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, TernaryBasicFalse) {
    std::string src =
        "x = false ? 1 : 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, TernaryWithComparison) {
    std::string src =
        "x = 3 > 2 ? 10 : 20\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, TernaryString) {
    std::string src =
        "s = true ? \"yes\" : \"no\"\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "yes\n");
}

TEST_F(CodeGenTest, TernaryNested) {
    std::string src =
        "x = true ? (false ? 1 : 2) : 3\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, TernaryInCall) {
    std::string src =
        "print(true ? \"a\" : \"b\")";
    EXPECT_EQ(runSource(src), "a\n");
}

TEST_F(CodeGenTest, TernaryTypeMismatchStrList) {
    std::string src =
        "x = true ? \"hello\" : [1, 2, 3]\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TernaryTypeMismatchListMap) {
    std::string src =
        "x = true ? [1, 2] : {\"a\": 1}\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TernaryListSameType) {
    std::string src =
        "x = true ? [1, 2] : [3, 4]\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "[1, 2]\n");
}

// ===== if/elif/else codegen テスト =====

TEST_F(CodeGenTest, IfTrueExecutes) {
    EXPECT_EQ(runSource("if true:\n    print(1)"), "1\n");
}

TEST_F(CodeGenTest, IfFalseDoesNotExecute) {
    EXPECT_EQ(runSource("if false:\n    print(1)"), "");
}

TEST_F(CodeGenTest, IfElseTrueBranch) {
    EXPECT_EQ(runSource("if true:\n    print(1)\nelse:\n    print(2)"), "1\n");
}

TEST_F(CodeGenTest, IfElseFalseBranch) {
    EXPECT_EQ(runSource("if false:\n    print(1)\nelse:\n    print(2)"), "2\n");
}

TEST_F(CodeGenTest, IfElifElseChain) {
    std::string src =
        "x = 2\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "else:\n"
        "    print(30)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, IfElifElseChainElse) {
    std::string src =
        "x = 99\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "else:\n"
        "    print(30)";
    EXPECT_EQ(runSource(src), "30\n");
}

TEST_F(CodeGenTest, IfVariableReassignment) {
    std::string src =
        "x = 0\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, IfNonBoolConditionInt) {
    EXPECT_EQ(runSource("if 1:\n    print(1)"), "1\n");
    EXPECT_EQ(runSource("if 0:\n    print(1)"), "");
}

TEST_F(CodeGenTest, NestedIf) {
    std::string src =
        "if true:\n"
        "    if true:\n"
        "        print(1)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, IfFollowedByStatement) {
    std::string src =
        "if true:\n"
        "    print(1)\n"
        "print(2)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

TEST_F(CodeGenTest, IfFalseFollowedByStatement) {
    std::string src =
        "if false:\n"
        "    print(1)\n"
        "print(2)";
    EXPECT_EQ(runSource(src), "2\n");
}

// ===== while codegen テスト =====

TEST_F(CodeGenTest, WhileFalseDoesNotExecute) {
    EXPECT_EQ(runSource("while false:\n    print(1)"), "");
}

TEST_F(CodeGenTest, WhileCountdown) {
    std::string src =
        "i = 3\n"
        "while i > 0:\n"
        "    print(i)\n"
        "    i = i - 1";
    EXPECT_EQ(runSource(src), "3\n2\n1\n");
}

TEST_F(CodeGenTest, WhileFollowedByStatement) {
    std::string src =
        "i = 0\n"
        "while i < 2:\n"
        "    i = i + 1\n"
        "print(i)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, WhileZeroCondition) {
    EXPECT_EQ(runSource("while 0:\n    print(1)"), "");
}

TEST_F(CodeGenTest, WhileNestedWhile) {
    std::string src =
        "i = 0\n"
        "while i < 2:\n"
        "    j = 0\n"
        "    while j < 2:\n"
        "        print(i + j)\n"
        "        j = j + 1\n"
        "    i = i + 1";
    EXPECT_EQ(runSource(src), "0\n1\n1\n2\n");
}

TEST_F(CodeGenTest, WhileWithIf) {
    std::string src =
        "i = 1\n"
        "while i <= 4:\n"
        "    if i % 2 == 0:\n"
        "        print(i)\n"
        "    i = i + 1";
    EXPECT_EQ(runSource(src), "2\n4\n");
}

// ===== fn 関数定義テスト =====

TEST_F(CodeGenTest, FnBasicAddCall) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "res = add(1, 2)\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, FnNoArgs) {
    std::string src =
        "fn zero() -> int:\n"
        "    return 0\n"
        "print(zero())";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, FnFloatReturn) {
    std::string src =
        "fn half(x: float) -> float:\n"
        "    return x / 2.0\n"
        "r = half(7.0)\n"
        "print(r)";
    EXPECT_EQ(runSource(src), "3.5\n");
}

TEST_F(CodeGenTest, FnBoolReturn) {
    std::string src =
        "fn is_positive(x: int) -> bool:\n"
        "    return x > 0\n"
        "print(is_positive(5))";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, FnWithIf) {
    std::string src =
        "fn abs(x: int) -> int:\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-5))\n"
        "print(abs(3))";
    EXPECT_EQ(runSource(src), "5\n3\n");
}

TEST_F(CodeGenTest, FnCallAsStatement) {
    std::string src =
        "fn greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n"
        "greet()";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, FnRecursiveFactorial) {
    std::string src =
        "fn factorial(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * factorial(n - 1)\n"
        "print(factorial(5))";
    EXPECT_EQ(runSource(src), "120\n");
}

TEST_F(CodeGenTest, FnUndefinedCallThrows) {
    EXPECT_THROW(runSource("x = unknown(1)"), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgCountMismatchThrows) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = add(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgTypeMismatchThrows) {
    std::string src =
        "fn inc(a: int) -> int:\n"
        "    return a + 1\n"
        "x = inc(1.5)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FnReturnTypeMismatchThrows) {
    std::string src =
        "fn bad() -> int:\n"
        "    return 1.5";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== ブロックスコープテスト =====

TEST_F(CodeGenTest, BlockScopeIfVarNotVisible) {
    std::string src =
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    // With Python-style, x created in inner scope; outer print(x) sees it
    // because findVar doesn't find x → new variable at outer scope
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeWhileVarNotVisible) {
    std::string src =
        "i = 1\n"
        "while i > 0:\n"
        "    x = 99\n"
        "    i = 0\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeElseVarNotVisible) {
    std::string src =
        "if false:\n"
        "    a = 1\n"
        "else:\n"
        "    b = 2\n"
        "print(b)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeInnerAccessesOuter) {
    std::string src =
        "x = 1\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, BlockScopeReassignment) {
    // With Python-style variables, inner assignment modifies outer variable
    std::string src =
        "x = 1\n"
        "if true:\n"
        "    x = 99\n"
        "    print(x)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "99\n99\n");
}

TEST_F(CodeGenTest, BlockScopeNewVariable) {
    // Variable created in inner scope, then created again in outer scope
    std::string src =
        "if true:\n"
        "    c = 10\n"
        "c = 20\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, MultipleElif) {
    std::string src =
        "x = 3\n"
        "if x == 1:\n"
        "    print(10)\n"
        "elif x == 2:\n"
        "    print(20)\n"
        "elif x == 3:\n"
        "    print(30)\n"
        "elif x == 4:\n"
        "    print(40)\n"
        "else:\n"
        "    print(50)";
    EXPECT_EQ(runSource(src), "30\n");
}

// ===== 構造体型テスト =====

TEST_F(CodeGenTest, StructBasicFieldAccess) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(10, 20)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, StructFloatFields) {
    std::string src =
        "record Vec2:\n"
        "    x: float\n"
        "    y: float\n"
        "v = Vec2(1.5, 2.5)\n"
        "print(v.x)\n"
        "print(v.y)";
    EXPECT_EQ(runSource(src), "1.5\n2.5\n");
}

TEST_F(CodeGenTest, StructInFnArg) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "p = Point(42, 99)\n"
        "print(get_x(p))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, StructAsReturnValue) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn make_point(a: int, b: int) -> Point:\n"
        "    return Point(a, b)\n"
        "p = make_point(7, 8)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "7\n8\n");
}

TEST_F(CodeGenTest, StructNested) {
    std::string src =
        "record Inner:\n"
        "    val: int\n"
        "record Outer:\n"
        "    inner: Inner\n"
        "    extra: int\n"
        "i = Inner(42)\n"
        "o = Outer(i, 99)\n"
        "print(o.inner.val)\n"
        "print(o.extra)";
    EXPECT_EQ(runSource(src), "42\n99\n");
}

TEST_F(CodeGenTest, StructConstructorArgCountMismatchThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructConstructorArgTypeMismatchThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1.5, 2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructUnknownFieldThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p.z)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FieldAccessOnNonStructThrows) {
    std::string src =
        "x = 42\n"
        "print(x.field)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, PrintStructThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, UnknownTypeAnnotationThrows) {
    EXPECT_THROW(runSource("x: foo = 42"), std::runtime_error);
}

TEST_F(CodeGenTest, StructFieldArithmetic) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "a = Point(10, 20)\n"
        "b = Point(3, 7)\n"
        "dx = a.x - b.x\n"
        "print(dx)";
    EXPECT_EQ(runSource(src), "7\n");
}

// ===== Unit型テスト =====

TEST_F(CodeGenTest, UnitFnNoReturnType) {
    std::string src =
        "fn greet():\n"
        "    print(42)\n"
        "greet()";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnitFnExplicit) {
    std::string src =
        "fn greet() -> Unit:\n"
        "    print(99)\n"
        "greet()";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, UnitFnReturnVoid) {
    std::string src =
        "fn noop():\n"
        "    return\n"
        "noop()";
    EXPECT_EQ(runSource(src), "");
}

TEST_F(CodeGenTest, UnitFnReturnVoidEarly) {
    std::string src =
        "fn maybe_print(x: int):\n"
        "    if x > 0:\n"
        "        print(x)\n"
        "        return\n"
        "    print(0)\n"
        "maybe_print(5)\n"
        "maybe_print(-1)";
    EXPECT_EQ(runSource(src), "5\n0\n");
}

TEST_F(CodeGenTest, UnitFnReturnValueThrows) {
    std::string src =
        "fn f():\n"
        "    return 42\n"
        "f()";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Option<T>型テスト =====

TEST_F(CodeGenTest, OptionIntSomePrint) {
    std::string src =
        "x: Option<int> = Some(42)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(42)\n");
}

TEST_F(CodeGenTest, OptionIntNonePrint) {
    std::string src =
        "x: Option<int> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionFloatSomePrint) {
    std::string src =
        "x: Option<float> = Some(3.14)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(3.14)\n");
}

TEST_F(CodeGenTest, OptionFloatNonePrint) {
    std::string src =
        "x: Option<float> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionBoolSomePrint) {
    std::string src =
        "x: Option<bool> = Some(true)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(true)\n");
}

TEST_F(CodeGenTest, OptionStrSomePrint) {
    std::string src =
        "x: Option<str> = Some(\"hello\")\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(hello)\n");
}

TEST_F(CodeGenTest, UnwrapRemoved) {
    std::string src =
        "x: Option<int> = Some(42)\n"
        "v = unwrap(x)\n"
        "print(v)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, UnwrapRemovedUFCS) {
    std::string src =
        "x: Option<int> = Some(42)\n"
        "v = x.unwrap()\n"
        "print(v)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneKeywordWithoutAnnotationThrows) {
    std::string src = "x = none";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneKeywordWithNonOptionAnnotationThrows) {
    std::string src = "x: int = none";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneKeywordWithOptionAnnotation) {
    std::string src =
        "x: Option<int> = none\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionFnParamAndReturn) {
    std::string src =
        "fn maybe_double(x: Option<int>) -> Option<int>:\n"
        "    return x\n"
        "a = maybe_double(Some(21))\n"
        "print(a)";
    EXPECT_EQ(runSource(src), "Some(21)\n");
}

TEST_F(CodeGenTest, OptionFnNoneArg) {
    std::string src =
        "fn f(x: Option<int>) -> int:\n"
        "    return 0\n"
        "r = f(None)\n"
        "print(r)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, OptionTypeMismatchThrows) {
    std::string src = "x: Option<int> = Some(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneWithoutAnnotationThrows) {
    EXPECT_THROW(runSource("x = None"), std::runtime_error);
}

TEST_F(CodeGenTest, OptionReassignSomeToNone) {
    std::string src =
        "x: Option<int> = Some(42)\n"
        "x = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionReassignNoneToSome) {
    std::string src =
        "x: Option<int> = None\n"
        "x = Some(99)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(99)\n");
}

// ===== タプル型テスト =====

TEST_F(CodeGenTest, TupleCreateAndAccess) {
    std::string src =
        "t = (10, 20)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, TupleMixedTypes) {
    std::string src =
        "t = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleWithTypeAnnotation) {
    std::string src =
        "t: (int, float) = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleThreeElements) {
    std::string src =
        "t = (1, 2, 3)\n"
        "print(t.0)\n"
        "print(t.1)\n"
        "print(t.2)";
    EXPECT_EQ(runSource(src), "1\n2\n3\n");
}

TEST_F(CodeGenTest, TupleFnReturn) {
    std::string src =
        "fn swap(a: int, b: int) -> (int, int):\n"
        "    return (b, a)\n"
        "res = swap(1, 2)\n"
        "print(res.0)\n"
        "print(res.1)";
    EXPECT_EQ(runSource(src), "2\n1\n");
}

TEST_F(CodeGenTest, TupleFnReturnAccessDirect) {
    std::string src =
        "fn make_pair(a: int, b: float) -> (int, float):\n"
        "    return (a, b)\n"
        "p = make_pair(42, 3.14)\n"
        "print(p.0)\n"
        "print(p.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleIndexOutOfRangeThrows) {
    std::string src =
        "t = (1, 2)\n"
        "print(t.2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleTypeAnnotationMismatchThrows) {
    std::string src = "t: (int, int) = (1, 3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleLetAssign) {
    std::string src =
        "a = (1, 2)\n"
        "x = a.0\n"
        "y = a.1\n"
        "print(x)\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

// ===== Tuple 分解代入テスト =====

TEST_F(CodeGenTest, TupleDestructBasic) {
    std::string src =
        "a, b = (10, 20)\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, TupleDestructWildcard) {
    std::string src =
        "a, _ = (10, 20)\n"
        "print(a)";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, TupleDestructTriple) {
    std::string src =
        "a, b, c = (1, 2, 3)\n"
        "print(a)\n"
        "print(b)\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "1\n2\n3\n");
}

TEST_F(CodeGenTest, TupleDestructFromFn) {
    std::string src =
        "fn f() -> (int, int):\n"
        "    return (3, 4)\n"
        "a, b = f()\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "3\n4\n");
}

TEST_F(CodeGenTest, VarTupleDestruct) {
    std::string src =
        "a, b = (10, 20)\n"
        "a = 100\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "100\n20\n");
}

TEST_F(CodeGenTest, TupleDestructMixedTypes) {
    std::string src =
        "x, y = (42, 3.14)\n"
        "print(x)\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

// ===== import 統合テスト =====

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
        auto tsm = cg.compile(prog);
        return runModule(std::move(tsm));
    }
};

TEST_F(ImportTest, ImportAllFunctions) {
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
}

TEST_F(ImportTest, ImportSelectedFunction) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");

    EXPECT_EQ(runWithImports(
        "from math import add\n"
        "print(add(10, 20))"),
        "30\n");
}

TEST_F(ImportTest, ImportMultipleSelected) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n");

    EXPECT_EQ(runWithImports(
        "from math import add, mul\n"
        "print(add(2, 3))\n"
        "print(mul(4, 5))"),
        "5\n20\n");
}

TEST_F(ImportTest, ImportSubdirectory) {
    writeFile("utils/math.ry",
        "fn double_it(x: int) -> int:\n"
        "    return x * 2\n");

    EXPECT_EQ(runWithImports(
        "from utils.math import double_it\n"
        "print(double_it(21))"),
        "42\n");
}

TEST_F(ImportTest, DuplicateImportIgnored) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");

    EXPECT_EQ(runWithImports(
        "from math\n"
        "from math\n"
        "print(add(1, 2))"),
        "3\n");
}

TEST_F(ImportTest, CircularImportError) {
    writeFile("a.ry", "from b\n");
    writeFile("b.ry", "from a\n");

    EXPECT_THROW(runWithImports("from a"), std::runtime_error);
}

TEST_F(ImportTest, TransitiveImport) {
    writeFile("base.ry",
        "fn base_fn(x: int) -> int:\n"
        "    return x + 100\n");
    writeFile("mid.ry",
        "from base\n"
        "fn mid_fn(x: int) -> int:\n"
        "    return base_fn(x) + 10\n");

    EXPECT_EQ(runWithImports(
        "from mid\n"
        "print(mid_fn(1))"),
        "111\n");
}

TEST_F(ImportTest, ModuleNotFoundError) {
    EXPECT_THROW(runWithImports("from nonexistent"), std::runtime_error);
}

TEST_F(ImportTest, FunctionNotFoundError) {
    writeFile("math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n");

    EXPECT_THROW(runWithImports("from math import nope"), std::runtime_error);
}

TEST_F(ImportTest, SearchPathRYPATH) {
    // Create a module in a separate directory
    auto lib_dir = std::filesystem::temp_directory_path() / "ry_lib_test";
    std::filesystem::create_directories(lib_dir);
    {
        std::ofstream f(lib_dir / "mylib.ry");
        f << "fn greet() -> int:\n"
             "    print(999)\n"
             "    return 0\n";
    }

    // Use search_paths to simulate RY_PATH
    EXPECT_EQ(runWithImports(
        "from mylib import greet\n"
        "greet()",
        tmp_dir_.string(),
        {lib_dir.string()}),
        "999\n");

    std::filesystem::remove_all(lib_dir);
}

// ===== directory package tests =====

TEST_F(ImportTest, DirectoryPackageImportAll) {
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

TEST_F(ImportTest, DirectoryPackageSelectiveImport) {
    writeFile("mypack/math.ry",
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n");
    writeFile("mypack/string.ry",
        "fn greet() -> int:\n"
        "    print(99)\n"
        "    return 0\n");

    EXPECT_EQ(runWithImports(
        "from mypack import add\n"
        "print(add(10, 20))"),
        "30\n");
}

TEST_F(ImportTest, DirectoryPackageSkipsUnderscoreFiles) {
    writeFile("mypack/public.ry",
        "fn visible() -> int:\n"
        "    return 42\n");
    writeFile("mypack/_hidden.ry",
        "fn secret() -> int:\n"
        "    return 99\n");

    // 'secret' should not be available
    EXPECT_THROW(runWithImports(
        "from mypack import secret"), std::runtime_error);
}

TEST_F(ImportTest, DirectoryPackageFallbackToFile) {
    // When both directory and file don't exist, but file exists
    writeFile("single.ry",
        "fn solo() -> int:\n"
        "    return 7\n");

    EXPECT_EQ(runWithImports(
        "from single import solo\n"
        "print(solo())"),
        "7\n");
}

// ===== record キーワード =====

TEST_F(CodeGenTest, RecordKeyword) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

// ===== type エイリアス =====

TEST_F(CodeGenTest, TypeAlias) {
    std::string src =
        "type MyInt = int\n"
        "fn add(a: MyInt, b: MyInt) -> MyInt:\n"
        "    return a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

// ===== for k, v in map =====

TEST_F(CodeGenTest, ForKVInMap) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "total = 0\n"
        "for k, v in m:\n"
        "    total = total + v\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "3\n");
}

// ===== .. 演算子 =====

TEST_F(CodeGenTest, RangeExpr) {
    std::string src =
        "xs = 1 .. 5\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, RangeExprIterate) {
    std::string src =
        "total = 0\n"
        "for x in 1 .. 3:\n"
        "    total = total + x\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, SpawnJoinNamedFunction) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "t: Task<int> = spawn add(20, 22)\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, SpawnJoinLambdaVariable) {
    std::string src =
        "add = fn(a: int, b: int) -> int: a + b\n"
        "t: Task<int> = spawn add(7, 8)\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, SpawnJoinNestedTask) {
    std::string src =
        "fn inner(x: int) -> int:\n"
        "    return x + 1\n"
        "fn outer(x: int) -> int:\n"
        "    t: Task<int> = spawn inner(x)\n"
        "    return join(t) * 2\n"
        "t: Task<int> = spawn outer(20)\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, AsyncAwaitDirectCall) {
    std::string src =
        "async fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(await add(20, 22))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, AsyncAwaitTaskVariable) {
    std::string src =
        "async fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "t: Task<int> = add(7, 8)\n"
        "print(await t)";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, AsyncAwaitChain) {
    std::string src =
        "async fn inner() -> int:\n"
        "    return 21\n"
        "async fn outer() -> int:\n"
        "    return (await inner()) * 2\n"
        "print(await outer())";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, AwaitRequiresTask) {
    std::string src =
        "x = await 123\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, AsyncUnitJoinStatement) {
    std::string src =
        "async fn bump() -> Unit:\n"
        "    print(\"done\")\n"
        "join(bump())\n"
        "print(\"after\")";
    EXPECT_EQ(runSource(src), "done\nafter\n");
}

TEST_F(CodeGenTest, AsyncUnitAwaitStatement) {
    std::string src =
        "async fn bump() -> Unit:\n"
        "    print(\"done\")\n"
        "await bump()\n"
        "print(\"after\")";
    EXPECT_EQ(runSource(src), "done\nafter\n");
}

TEST_F(CodeGenTest, AsyncAwaitStatementDiscardsValue) {
    std::string src =
        "async fn add(a: int, b: int) -> int:\n"
        "    print(a + b)\n"
        "    return a + b\n"
        "await add(20, 22)\n"
        "print(\"after\")";
    EXPECT_EQ(runSource(src), "42\nafter\n");
}

TEST_F(CodeGenTest, AwaitStatementRequiresTask) {
    EXPECT_THROW(runSource("await 123"), std::runtime_error);
}

TEST_F(CodeGenTest, AvailableParallelismBuiltin) {
    EXPECT_EQ(runSource("print(available_parallelism() > 0)"), "true\n");
}

TEST_F(CodeGenTest, ChannelSendRecvUnbuffered) {
    std::string src =
        "fn produce(ch: Channel<int>) -> int:\n"
        "    send(ch, 42)\n"
        "    return 0\n"
        "ch: Channel<int> = channel[int]()\n"
        "t: Task<int> = spawn produce(ch)\n"
        "print(recv(ch))\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "42\n0\n");
}

TEST_F(CodeGenTest, ChannelSendRecvBufferedFifo) {
    std::string src =
        "ch: Channel<int> = channel[int](2)\n"
        "send(ch, 1)\n"
        "send(ch, 2)\n"
        "print(recv(ch))\n"
        "print(recv(ch))";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

TEST_F(CodeGenTest, ChannelWorksAcrossAsyncTasks) {
    std::string src =
        "async fn produce(ch: Channel<int>) -> Unit:\n"
        "    send(ch, 7)\n"
        "ch: Channel<int> = channel[int]()\n"
        "t: Task<Unit> = produce(ch)\n"
        "print(recv(ch))\n"
        "await t";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, ChannelSendRejectsWrongType) {
    std::string src =
        "ch: Channel<int> = channel[int]()\n"
        "send(ch, \"x\")";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ChannelRecvRequiresChannel) {
    EXPECT_THROW(runSource("print(recv(123))"), std::runtime_error);
}

TEST_F(CodeGenTest, ChannelCloseThenSendFails) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "send(ch, 1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ChannelCloseThenEmptyRecvFails) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "recv(ch)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, SelectRecvReadyCase) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "send(ch, 42)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    else:\n"
        "        print(0)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, SelectSendReadyCase) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case send(ch, 42):\n"
        "        print(recv(ch))\n"
        "    else:\n"
        "        print(0)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, SelectElseDefaultCase) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    else:\n"
        "        print(0)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, SelectClosedRecvFails) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    else:\n"
        "        print(0)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RecvOptReturnsSomeThenNoneForClosedBufferedChannel) {
    std::string src =
        "ch: Channel<int> = channel[int](2)\n"
        "send(ch, 7)\n"
        "close(ch)\n"
        "print(recv_opt(ch))\n"
        "print(recv_opt(ch))";
    EXPECT_EQ(runSource(src), "Some(7)\nNone\n");
}

TEST_F(CodeGenTest, RecvOptReturnsBoolForUnitChannel) {
    std::string src =
        "ch: Channel<Unit> = channel[Unit]()\n"
        "close(ch)\n"
        "print(recv_opt(ch))";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, RecvOptRejectsNonChannel) {
    EXPECT_THROW(runSource("print(recv_opt(123))"), std::runtime_error);
}

TEST_F(CodeGenTest, TryRecvReturnsSomeThenNoneForBufferedChannel) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "send(ch, 9)\n"
        "print(try_recv(ch))\n"
        "print(try_recv(ch))";
    EXPECT_EQ(runSource(src), "Some(9)\nNone\n");
}

TEST_F(CodeGenTest, TryRecvReturnsFalseForDrainedUnitChannel) {
    std::string src =
        "ch: Channel<Unit> = channel[Unit]()\n"
        "print(try_recv(ch))";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, TrySendReturnsFalseForClosedChannel) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "print(try_send(ch, 1))";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, TrySendEventuallySucceedsForUnbufferedReceiver) {
    std::string src =
        "fn consume(ch: Channel<int>) -> int:\n"
        "    return recv(ch)\n"
        "ch: Channel<int> = channel[int]()\n"
        "t: Task<int> = spawn consume(ch)\n"
        "sent = false\n"
        "while not sent:\n"
        "    sent = try_send(ch, 13)\n"
        "print(sent)\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "true\n13\n");
}

TEST_F(CodeGenTest, ForLoopIteratesClosedBufferedChannel) {
    std::string src =
        "ch: Channel<int> = channel[int](2)\n"
        "send(ch, 1)\n"
        "send(ch, 2)\n"
        "close(ch)\n"
        "for x in ch:\n"
        "    print(x)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

TEST_F(CodeGenTest, ForLoopIteratesUnbufferedChannelFromSpawnedProducer) {
    std::string src =
        "fn produce(ch: Channel<int>) -> int:\n"
        "    send(ch, 3)\n"
        "    send(ch, 4)\n"
        "    close(ch)\n"
        "    return 0\n"
        "ch: Channel<int> = channel[int]()\n"
        "t: Task<int> = spawn produce(ch)\n"
        "for x in ch:\n"
        "    print(x)\n"
        "print(join(t))";
    EXPECT_EQ(runSource(src), "3\n4\n0\n");
}

TEST_F(CodeGenTest, ForLoopSupportsBreakAndContinueOnChannels) {
    std::string src =
        "ch: Channel<int> = channel[int](3)\n"
        "send(ch, 1)\n"
        "send(ch, 2)\n"
        "send(ch, 3)\n"
        "close(ch)\n"
        "for x in ch:\n"
        "    if x == 2:\n"
        "        continue\n"
        "    print(x)\n"
        "    if x == 3:\n"
        "        break";
    EXPECT_EQ(runSource(src), "1\n3\n");
}

TEST_F(CodeGenTest, ForLoopOverClosedDrainedChannelDoesNothing) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "for x in ch:\n"
        "    print(x)\n"
        "print(99)";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, ForLoopOverUnitChannelRequiresDiscardBinding) {
    std::string src =
        "ch: Channel<Unit> = channel[Unit]()\n"
        "close(ch)\n"
        "for x in ch:\n"
        "    print(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TryRecvRejectsNonChannel) {
    EXPECT_THROW(runSource("print(try_recv(123))"), std::runtime_error);
}

TEST_F(CodeGenTest, TrySendRejectsNonChannel) {
    EXPECT_THROW(runSource("print(try_send(123, 1))"), std::runtime_error);
}

TEST_F(CodeGenTest, SelectRecvOptClosedBufferedChannelReturnsNone) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "close(ch)\n"
        "select:\n"
        "    case let x = recv_opt(ch):\n"
        "        print(x)\n"
        "    else:\n"
        "        print(Some(0))";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, SelectRecvOptUnitChannelBindsBool) {
    std::string src =
        "ch: Channel<Unit> = channel[Unit]()\n"
        "close(ch)\n"
        "select:\n"
        "    case let ok = recv_opt(ch):\n"
        "        print(ok)\n"
        "    else:\n"
        "        print(true)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, SelectSendRejectsWrongType) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case send(ch, \"x\"):\n"
        "        print(1)\n"
        "    else:\n"
        "        print(0)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, SelectTimeoutZeroActsAsDefault) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    timeout 0:\n"
        "        print(99)";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, SelectReadyCaseWinsOverTimeout) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "send(ch, 7)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    timeout 0:\n"
        "        print(99)";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, SelectRejectsNonIntTimeout) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    timeout \"x\":\n"
        "        print(0)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, SelectRejectsNegativeTimeoutAtRuntime) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "select:\n"
        "    case let x = recv(ch):\n"
        "        print(x)\n"
        "    timeout -1:\n"
        "        print(0)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ParallelForRangeComputesSum) {
    std::string src =
        "fn one(x: int) -> int:\n"
        "    return 1\n"
        "@parallel\n"
        "for i in range(5):\n"
        "    print(one(i))";
    EXPECT_EQ(runSource(src), "1\n1\n1\n1\n1\n");
}

TEST_F(CodeGenTest, ParallelForRejectsOuterMutation) {
    std::string src =
        "total = 0\n"
        "@parallel\n"
        "for i in range(4):\n"
        "    total = total + i\n"
        "print(total)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ParallelForPropagatesWorkerException) {
    std::string src =
        "fn fail(x: int) -> int:\n"
        "    ch: Channel<int> = channel[int](1)\n"
        "    close(ch)\n"
        "    send(ch, x)\n"
        "    return 0\n"
        "@parallel\n"
        "for i in range(4):\n"
        "    fail(i)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ParallelForRejectsChannelIteration) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "@parallel\n"
        "for x in ch:\n"
        "    print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, SpawnUnitReturningCallRejected) {
    std::string src =
        "fn log_value(x: int):\n"
        "    print(x)\n"
        "t = spawn log_value(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, DoubleJoinThrowsRuntimeError) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "t: Task<int> = spawn add(1, 2)\n"
        "r1 = join(t)\n"
        "r2 = join(t)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, SpawnWithoutJoinDoesNotCrash) {
    std::string src =
        "fn work(x: int) -> int:\n"
        "    return x * 2\n"
        "t: Task<int> = spawn work(5)\n"
        "print(\"ok\")";
    EXPECT_EQ(runSource(src), "ok\n");
}

TEST_F(CodeGenTest, ChannelWithoutCloseDoesNotCrash) {
    std::string src =
        "ch: Channel<int> = channel[int](1)\n"
        "send(ch, 42)\n"
        "print(recv(ch))";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, ParallelForCapturesChannel) {
    std::string src =
        "fn producer(ch: Channel<int>, val: int) -> int:\n"
        "    send(ch, val)\n"
        "    return 0\n"
        "ch: Channel<int> = channel[int](10)\n"
        "@parallel\n"
        "for i in range(3):\n"
        "    producer(ch, i)\n"
        "total = 0\n"
        "for j in range(3):\n"
        "    total = total + recv(ch)\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "3\n");
}

// ===== Option auto-wrap =====

TEST_F(CodeGenTest, OptionAutoWrapInt) {
    std::string src =
        "x: int? = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(42)\n");
}

TEST_F(CodeGenTest, OptionAutoWrapStr) {
    std::string src =
        "x: str? = \"hello\"\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(hello)\n");
}

TEST_F(CodeGenTest, OptionAutoWrapBool) {
    std::string src =
        "x: bool? = true\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(true)\n");
}

TEST_F(CodeGenTest, OptionAutoWrapFloat) {
    std::string src =
        "x: float? = 3.14\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(3.14)\n");
}

// ===== ?? 演算子 =====

TEST_F(CodeGenTest, NullCoalesceSome) {
    std::string src =
        "x: int? = Some(5)\n"
        "y = x ?? 0\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, NullCoalesceNone) {
    std::string src =
        "x: int? = none\n"
        "y = x ?? 42\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== none キーワード =====

TEST_F(CodeGenTest, NoneKeyword) {
    std::string src =
        "x: int? = none\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

// ===== int リテラル型 =====

TEST_F(CodeGenTest, IntLiteralTypeSingle) {
    std::string src =
        "x: 42 = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, IntLiteralTypeUnionSuccess) {
    std::string src =
        "x: 0 | 1 = 0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, IntLiteralTypeUnionFail) {
    std::string src = "x: 0 | 1 = 2";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignSuccess) {
    std::string src =
        "x: 0 | 1 = 0\n"
        "x = 1\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignConstFail) {
    std::string src =
        "x: 0 | 1 = 0\n"
        "x = 2";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_two() -> int:\n"
        "    return 2\n"
        "x: 0 | 1 = 0\n"
        "x = get_two()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== 範囲型 =====

TEST_F(CodeGenTest, RangeTypeSuccess) {
    std::string src =
        "month: 1..12 = 6\n"
        "print(month)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, RangeTypeTooLow) {
    std::string src = "month: 1..12 = 0";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeTooHigh) {
    std::string src = "month: 1..12 = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeBoundaryLow) {
    std::string src =
        "x: 1..12 = 1\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, RangeTypeBoundaryHigh) {
    std::string src =
        "x: 1..12 = 12\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, RangeTypeNegative) {
    std::string src =
        "t: -10..10 = -5\n"
        "print(t)";
    EXPECT_EQ(runSource(src), "-5\n");
}

TEST_F(CodeGenTest, RangeTypeVarReassignRuntime) {
    std::string src =
        "x: 1..12 = 6\n"
        "x = 12\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, RangeTypeVarReassignConstFail) {
    std::string src =
        "x: 1..12 = 6\n"
        "x = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_thirteen() -> int:\n"
        "    return 13\n"
        "x: 1..12 = 6\n"
        "x = get_thirteen()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== str リテラル型 =====

TEST_F(CodeGenTest, StrLiteralTypeSuccess) {
    std::string src =
        "dir: \"N\" | \"S\" | \"E\" | \"W\" = \"N\"\n"
        "print(dir)";
    EXPECT_EQ(runSource(src), "N\n");
}

TEST_F(CodeGenTest, StrLiteralTypeFail) {
    std::string src = "dir: \"N\" | \"S\" | \"E\" | \"W\" = \"X\"";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StrLiteralTypeVarReassignSuccess) {
    std::string src =
        "dir: \"N\" | \"S\" = \"N\"\n"
        "dir = \"S\"\n"
        "print(dir)";
    EXPECT_EQ(runSource(src), "S\n");
}

TEST_F(CodeGenTest, StrLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_x() -> str:\n"
        "    return \"X\"\n"
        "dir: \"N\" | \"S\" = \"N\"\n"
        "dir = get_x()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== 型エイリアスとリテラル型/範囲型 =====

TEST_F(CodeGenTest, TypeAliasRangeType) {
    std::string src =
        "type Month = 1..12\n"
        "m: Month = 6\n"
        "print(m)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, TypeAliasRangeTypeFail) {
    std::string src =
        "type Month = 1..12\n"
        "m: Month = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAliasStrLiteralType) {
    std::string src =
        "type Direction = \"N\" | \"S\" | \"E\" | \"W\"\n"
        "d: Direction = \"N\"\n"
        "print(d)";
    EXPECT_EQ(runSource(src), "N\n");
}

TEST_F(CodeGenTest, TypeAliasIntLiteralType) {
    std::string src =
        "type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n"
        "d: Digit = 5\n"
        "print(d)";
    EXPECT_EQ(runSource(src), "5\n");
}

// ===== 関数型エイリアス =====

TEST_F(CodeGenTest, TypeAliasFnType) {
    std::string src =
        "type Callback = fn(int, int) -> int\n"
        "add: Callback = fn(a: int, b: int): a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, TypeAliasFnTypeParam) {
    std::string src =
        "type BinOp = fn(int, int) -> int\n"
        "fn apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "res = apply(fn(x: int, y: int): x + y, 10, 20)\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "30\n");
}

TEST_F(CodeGenTest, TypeAliasFnTypeLambdaParam) {
    std::string src =
        "type Mapper = fn(int) -> int\n"
        "apply = fn(f: Mapper, x: int): f(x)\n"
        "print(apply(fn(n: int): n * 3, 7))";
    EXPECT_EQ(runSource(src), "21\n");
}

TEST_F(CodeGenTest, TypeAliasFnTypeNamedFn) {
    std::string src =
        "type BinOp = fn(int, int) -> int\n"
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "fn apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "print(apply(add, 5, 6))";
    EXPECT_EQ(runSource(src), "11\n");
}

// ===== 関数パラメータの制約 =====

TEST_F(CodeGenTest, FnParamRangeTypeSuccess) {
    std::string src =
        "fn set_month(m: 1..12) -> int:\n"
        "    return m\n"
        "print(set_month(6))";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, FnParamRangeTypeConstFail) {
    std::string src =
        "fn set_month(m: 1..12) -> int:\n"
        "    return m\n"
        "set_month(13)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, EllipsisNoOp) {
    std::string src = "...";
    EXPECT_EQ(runSource(src), "");
}

TEST_F(CodeGenTest, EllipsisFnBody) {
    std::string src =
        "fn stub():\n"
        "    ...\n"
        "stub()\n"
        "print(1)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, EllipsisIfElse) {
    std::string src =
        "if true:\n"
        "    ...\n"
        "else:\n"
        "    ...\n"
        "print(42)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, EllipsisWhile) {
    std::string src =
        "i = 0\n"
        "while i < 3:\n"
        "    ...\n"
        "    i += 1\n"
        "print(i)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, EllipsisFor) {
    std::string src =
        "for i in range(3):\n"
        "    ...\n"
        "print(\"done\")";
    EXPECT_EQ(runSource(src), "done\n");
}

TEST_F(CodeGenTest, FnParamIntLiteralSuccess) {
    std::string src =
        "fn f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "print(f(1))";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, FnParamIntLiteralFail) {
    std::string src =
        "fn f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "f(2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== Short-circuit evaluation for and/or =====

TEST_F(CodeGenTest, AndShortCircuitFalse) {
    // When left side is false, right side should NOT be evaluated (no "side" printed)
    std::string src =
        "fn side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false and side_effect()\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "false\n");
}

TEST_F(CodeGenTest, AndShortCircuitTrue) {
    // When left side is true, right side should be evaluated ("side" printed)
    std::string src =
        "fn side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = true and side_effect()\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "side\ntrue\n");
}

TEST_F(CodeGenTest, OrShortCircuitTrue) {
    // When left side is true, right side should NOT be evaluated (no "side" printed)
    std::string src =
        "fn side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return false\n"
        "res = true or side_effect()\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "true\n");
}

TEST_F(CodeGenTest, OrShortCircuitFalse) {
    // When left side is false, right side should be evaluated ("side" printed)
    std::string src =
        "fn side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false or side_effect()\n"
        "print(res)";
    EXPECT_EQ(runSource(src), "side\ntrue\n");
}

// ===== Private symbol (_prefix) tests =====

TEST_F(ImportTest, PrivateSymbolWildcardImport) {
    // _prefixed symbols should be excluded from wildcard imports
    writeFile("mypkg/pub.ry",
        "fn public_fn() -> int:\n"
        "    return 42\n"
        "fn _private_fn() -> int:\n"
        "    return 99\n");

    EXPECT_EQ(runWithImports(
        "from mypkg\n"
        "print(public_fn())"),
        "42\n");

    // Attempting to use a _prefixed symbol after a wildcard import should error
    EXPECT_THROW(runWithImports(
        "from mypkg\n"
        "_private_fn()"),
        std::runtime_error);
}

TEST_F(ImportTest, PrivateSymbolNamedImportError) {
    // Attempting to import a _prefixed symbol by name should error
    writeFile("mypkg2/mod.ry",
        "fn _hidden() -> int:\n"
        "    return 1\n"
        "fn visible() -> int:\n"
        "    return 2\n");

    EXPECT_THROW(runWithImports(
        "from mypkg2 import _hidden"),
        std::runtime_error);
}

TEST_F(ImportTest, PrivateLetSymbolExcluded) {
    // _prefixed let constants should be excluded from wildcard imports
    writeFile("mypkg3/mod.ry",
        "_secret = 42\n"
        "public_val = 10\n");

    EXPECT_EQ(runWithImports(
        "from mypkg3\n"
        "print(public_val)"),
        "10\n");

    // Referencing _secret after a wildcard import should fail
    EXPECT_THROW(runWithImports(
        "from mypkg3\n"
        "print(_secret)"),
        std::runtime_error);

    // Attempting a named import of _secret should also error
    EXPECT_THROW(runWithImports(
        "from mypkg3 import _secret"),
        std::runtime_error);
}
