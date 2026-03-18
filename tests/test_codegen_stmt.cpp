#include "test_codegen_common.hpp"
#include "ry/module_loader.hpp"
#include <filesystem>
#include <fstream>

// ===== Ternary operator =====

TEST_F(CodeGenTest, TernaryBasicTrue) {
    std::string src =
        "let x = true ? 1 : 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, TernaryBasicFalse) {
    std::string src =
        "let x = false ? 1 : 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, TernaryWithComparison) {
    std::string src =
        "let x = 3 > 2 ? 10 : 20\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, TernaryString) {
    std::string src =
        "let s = true ? \"yes\" : \"no\"\n"
        "print(s)";
    EXPECT_EQ(runSource(src), "yes\n");
}

TEST_F(CodeGenTest, TernaryNested) {
    std::string src =
        "let x = true ? (false ? 1 : 2) : 3\n"
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
        "let x = true ? \"hello\" : [1, 2, 3]\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TernaryTypeMismatchListMap) {
    std::string src =
        "let x = true ? [1, 2] : {\"a\": 1}\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TernaryListSameType) {
    std::string src =
        "let x = true ? [1, 2] : [3, 4]\n"
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
        "let x = 2\n"
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
        "let x = 99\n"
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
        "var x = 0\n"
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
        "var i = 3\n"
        "while i > 0:\n"
        "    print(i)\n"
        "    i = i - 1";
    EXPECT_EQ(runSource(src), "3\n2\n1\n");
}

TEST_F(CodeGenTest, WhileFollowedByStatement) {
    std::string src =
        "var i = 0\n"
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
        "var i = 0\n"
        "while i < 2:\n"
        "    var j = 0\n"
        "    while j < 2:\n"
        "        print(i + j)\n"
        "        j = j + 1\n"
        "    i = i + 1";
    EXPECT_EQ(runSource(src), "0\n1\n1\n2\n");
}

TEST_F(CodeGenTest, WhileWithIf) {
    std::string src =
        "var i = 1\n"
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
        "let res = add(1, 2)\n"
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
        "let r = half(7.0)\n"
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
    EXPECT_THROW(runSource("let x = unknown(1)"), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgCountMismatchThrows) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "let x = add(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FnArgTypeMismatchThrows) {
    std::string src =
        "fn inc(a: int) -> int:\n"
        "    return a + 1\n"
        "let x = inc(1.5)";
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
        "    let x = 42\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeWhileVarNotVisible) {
    std::string src =
        "var i = 1\n"
        "while i > 0:\n"
        "    let x = 99\n"
        "    i = 0\n"
        "print(x)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeElseVarNotVisible) {
    std::string src =
        "if false:\n"
        "    let a = 1\n"
        "else:\n"
        "    let b = 2\n"
        "print(b)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, BlockScopeInnerAccessesOuter) {
    std::string src =
        "var x = 1\n"
        "if true:\n"
        "    x = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, BlockScopeShadowing) {
    std::string src =
        "let x = 1\n"
        "if true:\n"
        "    let x = 99\n"
        "    print(x)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "99\n1\n");
}

TEST_F(CodeGenTest, BlockScopeLetNotLeaks) {
    std::string src =
        "if true:\n"
        "    let c = 10\n"
        "let c = 20\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, MultipleElif) {
    std::string src =
        "let x = 3\n"
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
        "let p = Point(10, 20)\n"
        "print(p.x)\n"
        "print(p.y)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, StructFloatFields) {
    std::string src =
        "record Vec2:\n"
        "    x: float\n"
        "    y: float\n"
        "let v = Vec2(1.5, 2.5)\n"
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
        "let p = Point(42, 99)\n"
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
        "let p = make_point(7, 8)\n"
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
        "let i = Inner(42)\n"
        "let o = Outer(i, 99)\n"
        "print(o.inner.val)\n"
        "print(o.extra)";
    EXPECT_EQ(runSource(src), "42\n99\n");
}

TEST_F(CodeGenTest, StructConstructorArgCountMismatchThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructConstructorArgTypeMismatchThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1.5, 2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StructUnknownFieldThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
        "print(p.z)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, FieldAccessOnNonStructThrows) {
    std::string src =
        "let x = 42\n"
        "print(x.field)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, PrintStructThrows) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
        "print(p)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, UnknownTypeAnnotationThrows) {
    EXPECT_THROW(runSource("let x: foo = 42"), std::runtime_error);
}

TEST_F(CodeGenTest, StructFieldArithmetic) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let a = Point(10, 20)\n"
        "let b = Point(3, 7)\n"
        "let dx = a.x - b.x\n"
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
        "let x: Option<int> = Some(42)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(42)\n");
}

TEST_F(CodeGenTest, OptionIntNonePrint) {
    std::string src =
        "let x: Option<int> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionFloatSomePrint) {
    std::string src =
        "let x: Option<float> = Some(3.14)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(3.14)\n");
}

TEST_F(CodeGenTest, OptionFloatNonePrint) {
    std::string src =
        "let x: Option<float> = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionBoolSomePrint) {
    std::string src =
        "let x: Option<bool> = Some(true)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(true)\n");
}

TEST_F(CodeGenTest, OptionStrSomePrint) {
    std::string src =
        "let x: Option<str> = Some(\"hello\")\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(hello)\n");
}

TEST_F(CodeGenTest, UnwrapSome) {
    std::string src =
        "let x: Option<int> = Some(42)\n"
        "let v = unwrap(x)\n"
        "print(v)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, UnwrapNoneExits) {
    std::string src =
        "let x: Option<int> = None\n"
        "let v = unwrap(x)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, OptionFnParamAndReturn) {
    std::string src =
        "fn maybe_double(x: Option<int>) -> Option<int>:\n"
        "    return x\n"
        "let a = maybe_double(Some(21))\n"
        "print(a)";
    EXPECT_EQ(runSource(src), "Some(21)\n");
}

TEST_F(CodeGenTest, OptionFnNoneArg) {
    std::string src =
        "fn f(x: Option<int>) -> int:\n"
        "    return 0\n"
        "let r = f(None)\n"
        "print(r)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, OptionTypeMismatchThrows) {
    std::string src = "let x: Option<int> = Some(3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, NoneWithoutAnnotationThrows) {
    EXPECT_THROW(runSource("let x = None"), std::runtime_error);
}

TEST_F(CodeGenTest, OptionReassignSomeToNone) {
    std::string src =
        "var x: Option<int> = Some(42)\n"
        "x = None\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

TEST_F(CodeGenTest, OptionReassignNoneToSome) {
    std::string src =
        "var x: Option<int> = None\n"
        "x = Some(99)\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "Some(99)\n");
}

// ===== タプル型テスト =====

TEST_F(CodeGenTest, TupleCreateAndAccess) {
    std::string src =
        "let t = (10, 20)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, TupleMixedTypes) {
    std::string src =
        "let t = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleWithTypeAnnotation) {
    std::string src =
        "let t: (int, float) = (42, 3.14)\n"
        "print(t.0)\n"
        "print(t.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleThreeElements) {
    std::string src =
        "let t = (1, 2, 3)\n"
        "print(t.0)\n"
        "print(t.1)\n"
        "print(t.2)";
    EXPECT_EQ(runSource(src), "1\n2\n3\n");
}

TEST_F(CodeGenTest, TupleFnReturn) {
    std::string src =
        "fn swap(a: int, b: int) -> (int, int):\n"
        "    return (b, a)\n"
        "let res = swap(1, 2)\n"
        "print(res.0)\n"
        "print(res.1)";
    EXPECT_EQ(runSource(src), "2\n1\n");
}

TEST_F(CodeGenTest, TupleFnReturnAccessDirect) {
    std::string src =
        "fn make_pair(a: int, b: float) -> (int, float):\n"
        "    return (a, b)\n"
        "let p = make_pair(42, 3.14)\n"
        "print(p.0)\n"
        "print(p.1)";
    EXPECT_EQ(runSource(src), "42\n3.14\n");
}

TEST_F(CodeGenTest, TupleIndexOutOfRangeThrows) {
    std::string src =
        "let t = (1, 2)\n"
        "print(t.2)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleTypeAnnotationMismatchThrows) {
    std::string src = "let t: (int, int) = (1, 3.14)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TupleLetAssign) {
    std::string src =
        "let a = (1, 2)\n"
        "let x = a.0\n"
        "let y = a.1\n"
        "print(x)\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "1\n2\n");
}

// ===== Tuple 分解代入テスト =====

TEST_F(CodeGenTest, TupleDestructBasic) {
    std::string src =
        "let a, b = (10, 20)\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "10\n20\n");
}

TEST_F(CodeGenTest, TupleDestructWildcard) {
    std::string src =
        "let a, _ = (10, 20)\n"
        "print(a)";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, TupleDestructTriple) {
    std::string src =
        "let a, b, c = (1, 2, 3)\n"
        "print(a)\n"
        "print(b)\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "1\n2\n3\n");
}

TEST_F(CodeGenTest, TupleDestructFromFn) {
    std::string src =
        "fn f() -> (int, int):\n"
        "    return (3, 4)\n"
        "let a, b = f()\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "3\n4\n");
}

TEST_F(CodeGenTest, VarTupleDestruct) {
    std::string src =
        "var a, b = (10, 20)\n"
        "a = 100\n"
        "print(a)\n"
        "print(b)";
    EXPECT_EQ(runSource(src), "100\n20\n");
}

TEST_F(CodeGenTest, TupleDestructMixedTypes) {
    std::string src =
        "let x, y = (42, 3.14)\n"
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

// ===== record キーワード =====

TEST_F(CodeGenTest, RecordKeyword) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = Point(1, 2)\n"
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
        "var m = {\"a\": 1, \"b\": 2}\n"
        "var total = 0\n"
        "for k, v in m:\n"
        "    total = total + v\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "3\n");
}

// ===== .. 演算子 =====

TEST_F(CodeGenTest, RangeExpr) {
    std::string src =
        "let xs = 1 .. 5\n"
        "print(len(xs))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, RangeExprIterate) {
    std::string src =
        "var total = 0\n"
        "for x in 1 .. 3:\n"
        "    total = total + x\n"
        "print(total)";
    EXPECT_EQ(runSource(src), "6\n");
}

// ===== ?? 演算子 =====

TEST_F(CodeGenTest, NullCoalesceSome) {
    std::string src =
        "let x: int? = Some(5)\n"
        "let y = x ?? 0\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, NullCoalesceNone) {
    std::string src =
        "let x: int? = none\n"
        "let y = x ?? 42\n"
        "print(y)";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== none キーワード =====

TEST_F(CodeGenTest, NoneKeyword) {
    std::string src =
        "let x: int? = none\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "None\n");
}

// ===== int リテラル型 =====

TEST_F(CodeGenTest, IntLiteralTypeSingle) {
    std::string src =
        "let x: 42 = 42\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "42\n");
}

TEST_F(CodeGenTest, IntLiteralTypeUnionSuccess) {
    std::string src =
        "let x: 0 | 1 = 0\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, IntLiteralTypeUnionFail) {
    std::string src = "let x: 0 | 1 = 2";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignSuccess) {
    std::string src =
        "var x: 0 | 1 = 0\n"
        "x = 1\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignConstFail) {
    std::string src =
        "var x: 0 | 1 = 0\n"
        "x = 2";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, IntLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_two() -> int:\n"
        "    return 2\n"
        "var x: 0 | 1 = 0\n"
        "x = get_two()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== 範囲型 =====

TEST_F(CodeGenTest, RangeTypeSuccess) {
    std::string src =
        "let month: 1..12 = 6\n"
        "print(month)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, RangeTypeTooLow) {
    std::string src = "let month: 1..12 = 0";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeTooHigh) {
    std::string src = "let month: 1..12 = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeBoundaryLow) {
    std::string src =
        "let x: 1..12 = 1\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "1\n");
}

TEST_F(CodeGenTest, RangeTypeBoundaryHigh) {
    std::string src =
        "let x: 1..12 = 12\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, RangeTypeNegative) {
    std::string src =
        "let t: -10..10 = -5\n"
        "print(t)";
    EXPECT_EQ(runSource(src), "-5\n");
}

TEST_F(CodeGenTest, RangeTypeVarReassignRuntime) {
    std::string src =
        "var x: 1..12 = 6\n"
        "x = 12\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "12\n");
}

TEST_F(CodeGenTest, RangeTypeVarReassignConstFail) {
    std::string src =
        "var x: 1..12 = 6\n"
        "x = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, RangeTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_thirteen() -> int:\n"
        "    return 13\n"
        "var x: 1..12 = 6\n"
        "x = get_thirteen()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== str リテラル型 =====

TEST_F(CodeGenTest, StrLiteralTypeSuccess) {
    std::string src =
        "let dir: \"N\" | \"S\" | \"E\" | \"W\" = \"N\"\n"
        "print(dir)";
    EXPECT_EQ(runSource(src), "N\n");
}

TEST_F(CodeGenTest, StrLiteralTypeFail) {
    std::string src = "let dir: \"N\" | \"S\" | \"E\" | \"W\" = \"X\"";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, StrLiteralTypeVarReassignSuccess) {
    std::string src =
        "var dir: \"N\" | \"S\" = \"N\"\n"
        "dir = \"S\"\n"
        "print(dir)";
    EXPECT_EQ(runSource(src), "S\n");
}

TEST_F(CodeGenTest, StrLiteralTypeVarReassignDynamicFail) {
    std::string src =
        "fn get_x() -> str:\n"
        "    return \"X\"\n"
        "var dir: \"N\" | \"S\" = \"N\"\n"
        "dir = get_x()";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== 型エイリアスとリテラル型/範囲型 =====

TEST_F(CodeGenTest, TypeAliasRangeType) {
    std::string src =
        "type Month = 1..12\n"
        "let m: Month = 6\n"
        "print(m)";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, TypeAliasRangeTypeFail) {
    std::string src =
        "type Month = 1..12\n"
        "let m: Month = 13";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, TypeAliasStrLiteralType) {
    std::string src =
        "type Direction = \"N\" | \"S\" | \"E\" | \"W\"\n"
        "let d: Direction = \"N\"\n"
        "print(d)";
    EXPECT_EQ(runSource(src), "N\n");
}

TEST_F(CodeGenTest, TypeAliasIntLiteralType) {
    std::string src =
        "type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n"
        "let d: Digit = 5\n"
        "print(d)";
    EXPECT_EQ(runSource(src), "5\n");
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
