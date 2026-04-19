#include "test_codegen_common.hpp"
#include "ry/module_loader.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <sstream>


using namespace ry;
// ===== when expression =====

TEST_F(CodeGenTest, WhenExprBasics) {
    EXPECT_EQ(runSource("x = case:\n    true => 1\n    _ => 2\nprint(x)"), "1\n");
    EXPECT_EQ(runSource("x = case:\n    false => 1\n    _ => 2\nprint(x)"), "2\n");
    EXPECT_EQ(runSource("x = case:\n    3 > 2 => 10\n    _ => 20\nprint(x)"), "10\n");
    EXPECT_EQ(runSource("s = case:\n    true => \"yes\"\n    _ => \"no\"\nprint(s)"), "yes\n");
    EXPECT_EQ(runSource("x = case:\n    true => case:\n        false => 1\n        _ => 2\n    _ => 3\nprint(x)"), "2\n");
    EXPECT_EQ(runSource("x = case:\n    true => \"a\"\n    _ => \"b\"\nprint(x)"), "a\n");
}

TEST_F(CodeGenTest, WhenExprTypeMismatchErrors) {
    EXPECT_THROW(runSource("x = case:\n    true => \"hello\"\n    _ => [1, 2, 3]\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("x = case:\n    true => [1, 2]\n    _ => {\"a\": 1}\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, WhenExprListSameType) {
    std::string src =
        "x = case:\n    true => [1, 2]\n    _ => [3, 4]\nprint(x)";
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
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "res = add(1, 2)\n"
        "print(res)"), "3\n");
    // FnNoArgs
    EXPECT_EQ(runSource(
        "function zero() -> int:\n"
        "    return 0\n"
        "print(zero())"), "0\n");
    // FnFloatReturn
    EXPECT_EQ(runSource(
        "function half(x: float) -> float:\n"
        "    return x / 2.0\n"
        "r = half(7.0)\n"
        "print(r)"), "3.5\n");
    // FnBoolReturn
    EXPECT_EQ(runSource(
        "function is_positive(x: int) -> bool:\n"
        "    return x > 0\n"
        "print(is_positive(5))"), "true\n");
    // FnWithIf
    EXPECT_EQ(runSource(
        "function abs(x: int) -> int:\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-5))\n"
        "print(abs(3))"), "5\n3\n");
    // FnCallAsStatement
    EXPECT_EQ(runSource(
        "function greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n"
        "greet()"), "42\n");
    // FnRecursiveFactorial
    EXPECT_EQ(runSource(
        "function factorial(n: int) -> int:\n"
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
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = add(1)"), std::runtime_error);
    // FnArgTypeMismatchThrows
    EXPECT_THROW(runSource(
        "function inc(a: int) -> int:\n"
        "    return a + 1\n"
        "x = inc(1.5)"), std::runtime_error);
    // FnReturnTypeMismatchThrows
    EXPECT_THROW(runSource(
        "function bad() -> int:\n"
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
        "function get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "p = Point(42, 99)\n"
        "print(get_x(p))"), "42\n");
    // RecordAsReturnValue
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "function make_point(a: int, b: int) -> Point:\n"
        "    return Point(a, b)\n"
        "p = make_point(7, 8)\n"
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
    // PrintStruct — now works via auto-generated to_str
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(p)"), "Point(x: 1, y: 2)\n");
}

// ===== Record to_str =====

TEST_F(CodeGenTest, RecordToStr) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "print(to_str(p))"), "Point(x: 1, y: 2)\n");
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
        "function to_str(p: Point) -> str:\n"
        "    return f\"({p.x}, {p.y})\"\n"
        "p = Point(5, 6)\n"
        "print(to_str(p))"), "(5, 6)\n");
}

TEST_F(CodeGenTest, RecordUserDefinedToStrWithPrint) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "function to_str(p: Point) -> str:\n"
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
        "function greet() -> Unit:\n"
        "    print(42)\n"
        "greet()"), "42\n");
    // UnitFnExplicit
    EXPECT_EQ(runSource(
        "function greet() -> Unit:\n"
        "    print(99)\n"
        "greet()"), "99\n");
    // UnitFnReturnVoid
    EXPECT_EQ(runSource(
        "function noop() -> Unit:\n"
        "    return\n"
        "noop()"), "");
    // UnitFnReturnVoidEarly
    EXPECT_EQ(runSource(
        "function maybe_print(x: int) -> Unit:\n"
        "    if x > 0:\n"
        "        print(x)\n"
        "        return\n"
        "    print(0)\n"
        "maybe_print(5)\n"
        "maybe_print(-1)"), "5\n0\n");
}

TEST_F(CodeGenTest, UnitFnReturnValueThrows) {
    std::string src =
        "function f() -> Unit:\n"
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
        "function maybe_double(x: Option<int>) -> Option<int>:\n"
        "    return x\n"
        "a = maybe_double(Some(21))\n"
        "print(a)"), "Some(21)\n");
    // OptionFnNoneArg
    EXPECT_EQ(runSource(
        "function f(x: Option<int>) -> int:\n"
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
        "function swap(a: int, b: int) -> (int, int):\n"
        "    return (b, a)\n"
        "res = swap(1, 2)\n"
        "print(res.0)\n"
        "print(res.1)"), "2\n1\n");
    // TupleFnReturnAccessDirect
    EXPECT_EQ(runSource(
        "function make_pair(a: int, b: float) -> (int, float):\n"
        "    return (a, b)\n"
        "p = make_pair(42, 3.14)\n"
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
        "function f() -> (int, int):\n"
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

TEST_F(ImportTest, ImportBasics) {
    // ImportAllFunctions
    writeFile("math.ry",
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "function sub(a: int, b: int) -> int:\n"
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
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "function sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "function mul(a: int, b: int) -> int:\n"
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
        "function double_it(x: int) -> int:\n"
        "    return x * 2\n");

    EXPECT_EQ(runWithImports(
        "from utils.math import double_it\n"
        "print(double_it(21))"),
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
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n");
    EXPECT_THROW(runWithImports("from mathmod import nope"), std::runtime_error);
}

TEST_F(ImportTest, TransitiveImport) {
    writeFile("base.ry",
        "function base_fn(x: int) -> int:\n"
        "    return x + 100\n");
    writeFile("mid.ry",
        "from base\n"
        "function mid_fn(x: int) -> int:\n"
        "    return base_fn(x) + 10\n");

    EXPECT_EQ(runWithImports(
        "from mid\n"
        "print(mid_fn(1))"),
        "111\n");
}

TEST_F(ImportTest, SearchPathRYPATH) {
    auto lib_dir = std::filesystem::temp_directory_path() / "ry_lib_test";
    std::filesystem::create_directories(lib_dir);
    {
        std::ofstream f(lib_dir / "mylib.ry");
        f << "function greet() -> int:\n"
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

// ===== directory package tests =====

TEST_F(ImportTest, DirectoryPackageImportAll) {
    writeFile("mypack/math.ry",
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n");
    writeFile("mypack/string.ry",
        "function greet() -> int:\n"
        "    print(42)\n"
        "    return 0\n");

    EXPECT_EQ(runWithImports(
        "from mypack\n"
        "print(add(1, 2))\n"
        "greet()"),
        "3\n42\n");
}

TEST_F(ImportTest, DirectoryPackageSelectiveImport) {
    writeFile("mypack2/math.ry",
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "function sub(a: int, b: int) -> int:\n"
        "    return a - b\n");
    writeFile("mypack2/string.ry",
        "function greet() -> int:\n"
        "    print(99)\n"
        "    return 0\n");

    EXPECT_EQ(runWithImports(
        "from mypack2 import add\n"
        "print(add(10, 20))"),
        "30\n");
}

TEST_F(ImportTest, DirectoryPackageSkipsUnderscoreFiles) {
    writeFile("mypack3/public.ry",
        "function visible() -> int:\n"
        "    return 42\n");
    writeFile("mypack3/_hidden.ry",
        "function secret() -> int:\n"
        "    return 99\n");

    EXPECT_THROW(runWithImports(
        "from mypack3 import secret"), std::runtime_error);
}

TEST_F(ImportTest, DirectoryPackageFallbackToFile) {
    writeFile("single.ry",
        "function solo() -> int:\n"
        "    return 7\n");

    EXPECT_EQ(runWithImports(
        "from single import solo\n"
        "print(solo())"),
        "7\n");
}

// ===== Private symbol (_prefix) tests =====

TEST_F(ImportTest, PrivateSymbolWildcardImport) {
    writeFile("mypkg/pub.ry",
        "function public_fn() -> int:\n"
        "    return 42\n"
        "function _private_fn() -> int:\n"
        "    return 99\n");

    EXPECT_EQ(runWithImports(
        "from mypkg\n"
        "print(public_fn())"),
        "42\n");

    EXPECT_THROW(runWithImports(
        "from mypkg\n"
        "_private_fn()"),
        std::runtime_error);
}

TEST_F(ImportTest, PrivateSymbolNamedImportError) {
    writeFile("mypkg2/mod.ry",
        "function _hidden() -> int:\n"
        "    return 1\n"
        "function visible() -> int:\n"
        "    return 2\n");

    EXPECT_THROW(runWithImports(
        "from mypkg2 import _hidden"),
        std::runtime_error);
}

TEST_F(ImportTest, PrivateLetSymbolExcluded) {
    writeFile("mypkg3/mod.ry",
        "_secret = 42\n"
        "public_val = 10\n");

    EXPECT_EQ(runWithImports(
        "from mypkg3\n"
        "print(public_val)"),
        "10\n");

    EXPECT_THROW(runWithImports(
        "from mypkg3\n"
        "print(_secret)"),
        std::runtime_error);

    EXPECT_THROW(runWithImports(
        "from mypkg3 import _secret"),
        std::runtime_error);
}

// ===== type alias =====

TEST_F(CodeGenTest, TypeAlias) {
    std::string src =
        "type MyInt = int\n"
        "function add(a: MyInt, b: MyInt) -> MyInt:\n"
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
        "print(length(xs))"), "5\n");
    // RangeExprIterate
    EXPECT_EQ(runSource(
        "total = 0\n"
        "for x in 1 .. 3:\n"
        "    total = total + x\n"
        "print(total)"), "6\n");
}

// ===== Concurrency: async/await =====

TEST_F(CodeGenTest, AsyncAwaitBasics) {
    // block_on awaits direct async call
    EXPECT_EQ(runSource(
        "async function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(block_on(add(20, 22)))"), "42\n");
    // block_on awaits task variable
    EXPECT_EQ(runSource(
        "async function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "t: Task<int> = add(7, 8)\n"
        "print(block_on(t))"), "15\n");
    // await chains inside async function, block_on at top level
    EXPECT_EQ(runSource(
        "async function inner() -> int:\n"
        "    return 21\n"
        "async function outer() -> int:\n"
        "    return (await inner()) * 2\n"
        "print(block_on(outer()))"), "42\n");
}

TEST_F(CodeGenTest, AsyncStatementForms) {
    // block_on Unit-returning async
    EXPECT_EQ(runSource(
        "async function bump() -> Unit:\n"
        "    print(\"done\")\n"
        "block_on(bump())\n"
        "print(\"after\")"), "done\nafter\n");
    // block_on discards value
    EXPECT_EQ(runSource(
        "async function add(a: int, b: int) -> int:\n"
        "    print(a + b)\n"
        "    return a + b\n"
        "block_on(add(20, 22))\n"
        "print(\"after\")"), "42\nafter\n");
}

TEST_F(CodeGenTest, AwaitErrors) {
    // await outside async function is a parse error
    EXPECT_THROW(runSource("async function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = await add(1, 2)\nprint(x)"), std::runtime_error);
    EXPECT_THROW(runSource("async function bump() -> Unit:\n"
        "    print(\"done\")\n"
        "await bump()"), std::runtime_error);
    // block_on requires Task
    EXPECT_THROW(runSource("x = block_on(123)\nprint(x)"), std::runtime_error);
}

TEST_F(CodeGenTest, AvailableParallelismBuiltin) {
    EXPECT_EQ(runSource("print(available_parallelism() > 0)"), "true\n");
}

// ===== Parallel for =====

// TODO: print output interleaves across threads (#473)
// TEST_F(CodeGenTest, ParallelForBasics) {
//     EXPECT_EQ(runSource(
//         "function one(x: int) -> int:\n"
//         "    return 1\n"
//         "@parallel\n"
//         "for i in range(5):\n"
//         "    print(one(i))"), "1\n1\n1\n1\n1\n");
// }
TEST_F(CodeGenTest, ParallelForPrintComposite) {
    std::string result = runSource(
        "@parallel\n"
        "for i in range(3):\n"
        "    print([i, i])");
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
    EXPECT_THROW(runSource(
        "total = 0\n"
        "@parallel\n"
        "for i in range(4):\n"
        "    total = total + i\n"
        "print(total)"), std::runtime_error);
    // ParallelForRejectsNestedFunctionDef (#1118)
    EXPECT_THROW(runSource(
        "@parallel\n"
        "for i in range(4):\n"
        "    function helper() -> int:\n"
        "        return i\n"
        "    print(helper())\n"), std::runtime_error);
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
        "function get_two() -> int:\n"
        "    return 2\n"
        "x: 0 | 1 = 0\n"
        "x = get_two()";
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
        "function get_thirteen() -> int:\n"
        "    return 13\n"
        "x: 1..12 = 6\n"
        "x = get_thirteen()";
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
        "function get_x() -> str:\n"
        "    return \"X\"\n"
        "dir: \"N\" | \"S\" = \"N\"\n"
        "dir = get_x()";
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
        "type Callback = function(int, int) -> int\n"
        "add: Callback = (a: int, b: int) => a + b\n"
        "print(add(3, 4))"), "7\n");
    // TypeAliasFnTypeParam
    EXPECT_EQ(runSource(
        "type BinOp = function(int, int) -> int\n"
        "function apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "res = apply((x: int, y: int) => x + y, 10, 20)\n"
        "print(res)"), "30\n");
    // TypeAliasFnTypeLambdaParam
    EXPECT_EQ(runSource(
        "type Mapper = function(int) -> int\n"
        "apply = (f: Mapper, x: int) => f(x)\n"
        "print(apply((n: int) => n * 3, 7))"), "21\n");
    // TypeAliasFnTypeNamedFn
    EXPECT_EQ(runSource(
        "type BinOp = function(int, int) -> int\n"
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "function apply(f: BinOp, a: int, b: int) -> int:\n"
        "    return f(a, b)\n"
        "print(apply(add, 5, 6))"), "11\n");
}

// ===== Function parameter constraints =====

TEST_F(CodeGenTest, FnParamConstraints) {
    // FnParamRangeTypeSuccess
    EXPECT_EQ(runSource(
        "function set_month(m: 1..12) -> int:\n"
        "    return m\n"
        "print(set_month(6))"), "6\n");
    // FnParamRangeTypeConstFail
    EXPECT_THROW(runSource(
        "function set_month(m: 1..12) -> int:\n"
        "    return m\n"
        "set_month(13)"), std::runtime_error);
    // FnParamIntLiteralSuccess
    EXPECT_EQ(runSource(
        "function f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "print(f(1))"), "1\n");
    // FnParamIntLiteralFail
    EXPECT_THROW(runSource(
        "function f(x: 0 | 1) -> int:\n"
        "    return x\n"
        "f(2)"), std::runtime_error);
}

// ===== Ellipsis =====

TEST_F(CodeGenTest, EllipsisVariants) {
    // EllipsisNoOp
    EXPECT_EQ(runSource("..."), "");
    // EllipsisFnBody
    EXPECT_EQ(runSource(
        "function stub() -> Unit:\n"
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
        "function side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false and side_effect()\n"
        "print(res)"), "false\n");
    // AndShortCircuitTrue
    EXPECT_EQ(runSource(
        "function side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = true and side_effect()\n"
        "print(res)"), "side\ntrue\n");
    // OrShortCircuitTrue
    EXPECT_EQ(runSource(
        "function side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return false\n"
        "res = true or side_effect()\n"
        "print(res)"), "true\n");
    // OrShortCircuitFalse
    EXPECT_EQ(runSource(
        "function side_effect() -> bool:\n"
        "    print(\"side\")\n"
        "    return true\n"
        "res = false or side_effect()\n"
        "print(res)"), "side\ntrue\n");
}

// ===== Default arguments =====

TEST_F(CodeGenTest, DefaultArgBasic) {
    EXPECT_EQ(runSource(
        "function greet(name: str, greeting: str = \"Hello\") -> str:\n"
        "    return greeting + \", \" + name\n"
        "print(greet(\"Alice\"))\n"
        "print(greet(\"Alice\", \"Hi\"))"), "Hello, Alice\nHi, Alice\n");
}

TEST_F(CodeGenTest, DefaultArgMultiple) {
    EXPECT_EQ(runSource(
        "function f(a: int, b: int = 10, c: int = 20) -> int:\n"
        "    return a + b + c\n"
        "print(f(1))\n"
        "print(f(1, 2))\n"
        "print(f(1, 2, 3))"), "31\n23\n6\n");
}

TEST_F(CodeGenTest, DefaultArgOverloadConflict) {
    EXPECT_THROW(runSource(
        "function calc(x: int, y: int = 0) -> int:\n"
        "    return x + y\n"
        "function calc(x: int) -> int:\n"
        "    return x * 2\n"
        "print(calc(1))"), std::runtime_error);
}

TEST_F(CodeGenTest, DefaultArgWithWidening) {
    EXPECT_EQ(runSource(
        "function calc(x: float, precision: int = 6) -> float:\n"
        "    return x\n"
        "print(calc(3))"), "3.0\n");
}

TEST_F(CodeGenTest, DefaultArgGenericError) {
    EXPECT_THROW(runSource(
        "function identity<T>(x: T, label: str = \"default\") -> T:\n"
        "    return x\n"
        "print(identity(42))"), std::runtime_error);
}
