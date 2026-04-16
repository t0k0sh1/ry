#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// Struct type rejected by arithmetic operators
// ============================================================

TEST_F(CodeGenTest, RecordAddRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p + 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordSubRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p - 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordMulRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p * 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordDivRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p / 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordFloorDivRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p // 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordModRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p % 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordPowerRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p ** 2\n"
    ), std::runtime_error);
}

// ============================================================
// Struct type rejected by unary operators
// ============================================================

TEST_F(CodeGenTest, RecordNegationRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "-p\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordBitwiseNotRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "~p\n"
    ), std::runtime_error);
}

// ============================================================
// Pointer/str type rejected by unary operators
// ============================================================

TEST_F(CodeGenTest, StringNegationRejected) {
    EXPECT_THROW(runSource(
        "-\"abc\"\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StringBitwiseNotRejected) {
    EXPECT_THROW(runSource(
        "~\"abc\"\n"
    ), std::runtime_error);
}

// ============================================================
// Struct type rejected by bitwise operators
// ============================================================

TEST_F(CodeGenTest, RecordBitwiseAndRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p & 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordBitwiseOrRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p | 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordBitwiseXorRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p ^ 1\n"
    ), std::runtime_error);
}

// ============================================================
// Struct type rejected by comparison operators (< <= > >=)
// ============================================================

TEST_F(CodeGenTest, RecordLessThanRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(3, 4)\n"
        "p < q\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordGreaterThanRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(3, 4)\n"
        "p > q\n"
    ), std::runtime_error);
}

// ============================================================
// Regression: Struct == and != must still work
// ============================================================

TEST_F(CodeGenTest, RecordEqualityStillWorks) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(1, 2)\n"
        "print(p == q)\n"
    ), "true\n");
}

TEST_F(CodeGenTest, RecordInequalityStillWorks) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(3, 4)\n"
        "print(p != q)\n"
    ), "true\n");
}

// ============================================================
// IR verify error sweep — #805, #818, #821
//
// These used to reach LLVM IR verification and crash with cryptic
// "icmp ne ptr, i0 0" or "Terminator found in the middle of a basic
// block" messages. After the sweep, each case is either handled
// frontend-side with a clear diagnostic or silently accepted with a
// trailing dead block that LLVM DCE removes.
// ============================================================

// ---- #818: pointer-backed collection/str types cannot be conditions ----

TEST_F(CodeGenTest, ListInIfConditionRejected) {
    EXPECT_THROW(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "if xs:\n"
        "    print(\"non-empty\")\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, ListInWhileConditionRejected) {
    EXPECT_THROW(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "while xs:\n"
        "    break\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StrInIfConditionRejected) {
    EXPECT_THROW(runSource(
        "s: str = \"hello\"\n"
        "if s:\n"
        "    print(\"non-empty\")\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, MapInIfConditionRejected) {
    EXPECT_THROW(runSource(
        "m: Map<str, int> = {\"a\": 1}\n"
        "if m:\n"
        "    print(\"non-empty\")\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, SetInIfConditionRejected) {
    EXPECT_THROW(runSource(
        "s: Set<int> = {1, 2, 3}\n"
        "if s:\n"
        "    print(\"non-empty\")\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, NotOnListRejected) {
    EXPECT_THROW(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "print(not xs)\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, RecordInIfConditionRejected) {
    // Records land in the "not i1, not double, not ptr, not integer" branch
    // of toBool() and must be rejected with the generic condition error
    // rather than falling through to the integer compare path.
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "if p:\n"
        "    print(\"has\")\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, WhenCondCollectionRejected) {
    EXPECT_THROW(runSource(
        "xs: List<int> = [1]\ncase:\n    xs:\n        print(\"has\")\n    _:\n        print(\"none\")\n"
    ), std::runtime_error);
}

// Regression: integer and bool conditions must still work.
TEST_F(CodeGenTest, IntInIfConditionStillWorks) {
    EXPECT_EQ(runSource(
        "x = 1\n"
        "if x:\n"
        "    print(\"yes\")\n"
    ), "yes\n");
}

TEST_F(CodeGenTest, LengthGreaterZeroIdiomWorks) {
    EXPECT_EQ(runSource(
        "xs: List<int> = [1, 2, 3]\n"
        "if length(xs) > 0:\n"
        "    print(\"non-empty\")\n"
    ), "non-empty\n");
}

// ---- #821: code after exit() must not trigger IR verify error ----
//
// Using compileSource() here instead of runSource(): calling exit(0) from a
// JIT-loaded module would terminate the test process itself. We only need to
// verify that codegen produces valid IR (no "Terminator found in the middle
// of a basic block" from LLVM verification), which compileSource() checks.

TEST_F(CodeGenTest, ExitFollowedByPrintCompiles) {
    // exit(0) is a diverging call. emitExit() switches to a fresh dead block
    // so the trailing print() lands on a valid (unreachable) block and LLVM
    // verification sees exactly one terminator per basic block.
    EXPECT_NO_THROW(compileSource(
        "print(\"before\")\n"
        "exit(0)\n"
        "print(\"after - should not print\")\n"
    ));
}

TEST_F(CodeGenTest, ExitInsideIfFollowedByStmtCompiles) {
    EXPECT_NO_THROW(compileSource(
        "x = 1\n"
        "if x == 1:\n"
        "    print(\"first\")\n"
        "    exit(0)\n"
        "    print(\"dead\")\n"
        "print(\"unreachable-after-if\")\n"
    ));
}

TEST_F(CodeGenTest, ExitInFunctionBodyFollowedByStmtCompiles) {
    // Function body emission loop should also tolerate a dead block after
    // exit(). Covers the CallStmt path (emitStmt(CallStmt) routes exit
    // through builtins_["exit"]).
    EXPECT_NO_THROW(compileSource(
        "function f():\n"
        "    exit(1)\n"
        "    print(\"dead\")\n"
        "f()\n"
    ));
}

// ---- #805: Result wrapper must not be mistaken for inner JsonValue ----
//
// These tests inline the @native("json") declarations so they do not rely
// on the module loader (runSource goes through CodeGen::compile directly).
// The declarations mirror share/std/json/json.ry.

static const std::string JSON_DECLS = R"(
@native("json")
function parse(text: str) -> Result<JsonValue, Error>
@native("json")
function stringify(value: JsonValue) -> str
@native("json")
function kind(value: JsonValue) -> str
@native("json")
function get(value: JsonValue, key: str) -> Result<JsonValue, Error>
@native("json")
function to_str(value: JsonValue) -> Result<str, Error>
@native("json")
function json_free(value: JsonValue) -> Unit
)";

TEST_F(CodeGenTest, JsonKindOnResultRejected) {
    // json.get() returns Result<JsonValue, Error>. Passing it to kind()
    // without unwrapping used to sneak past isJsonValue() (metadata-only
    // check) and reach __ry_json_type() with a Result struct, producing
    // an IR verify error. After the fix, isJsonValue gates on ptrTy_, so
    // the Result is rejected at the frontend with kind()'s existing
    // "requires a JsonValue argument" diagnostic — not an LLVM verify
    // failure.
    std::string err;
    try {
        compileSource(JSON_DECLS + R"(
case parse("{\"name\": \"ry\"}"):
    Ok(j):
        v = get(j, "name")
        print(kind(v))
    Err(e):
        print(e)
)");
    } catch (const std::runtime_error &e) {
        err = e.what();
    }
    EXPECT_NE(err.find("kind() requires a JsonValue argument"),
              std::string::npos) << "error was: " << err;
    // Guard: must NOT be an LLVM verify error — that would mean the
    // isJsonValue fix regressed.
    EXPECT_EQ(err.find("IR verify error"), std::string::npos)
        << "error was: " << err;
}

TEST_F(CodeGenTest, JsonStringifyOnResultRejected) {
    std::string err;
    try {
        compileSource(JSON_DECLS + R"(
case parse("{\"name\": \"ry\"}"):
    Ok(j):
        v = get(j, "name")
        print(stringify(v))
    Err(e):
        print(e)
)");
    } catch (const std::runtime_error &e) {
        err = e.what();
    }
    EXPECT_NE(err.find("stringify() requires a JsonValue argument"),
              std::string::npos) << "error was: " << err;
    EXPECT_EQ(err.find("IR verify error"), std::string::npos)
        << "error was: " << err;
}

TEST_F(CodeGenTest, ToStrOnJsonResultFallsThroughToGenericResult) {
    // After the fix, to_str() on a Result<JsonValue, Error> does NOT route
    // through the JSON-specific dispatcher (isJsonValue now returns false
    // for non-ptr values). It falls through to the generic valueToString()
    // Result branch, which formats as "Ok(...)" just like any other Result.
    // This matches the baseline behavior of print() on Result values.
    std::string out = runSource(JSON_DECLS + R"(
case parse("{\"name\": \"ry\"}"):
    Ok(j):
        v = get(j, "name")
        print(to_str(v))
    Err(e):
        print("err")
)");
    // The inner JsonValue formatting is a separate concern; we assert only
    // that the Result wrapper is preserved and no IR verify error occurs.
    EXPECT_NE(out.find("Ok("), std::string::npos) << "output was: " << out;
}

// Regression: generic Result stringification still works for non-JSON cases.
TEST_F(CodeGenTest, GenericResultToStrStillWorks) {
    EXPECT_EQ(runSource(
        "r: Result<int, Error> = Ok(42)\n"
        "print(to_str(r))\n"
    ), "Ok(42)\n");
}

// ============================================================
// findMatchingCloseParen — depth-tracking paren matcher (#768)
// ============================================================

TEST(FindMatchingCloseParen, Simple) {
    // function(int) -> int
    //         ^   ^
    //         8   12
    std::string s = "function(int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 8), 12u);
}

TEST(FindMatchingCloseParen, Nested) {
    // function(function() -> int) -> int
    //         ^                 ^
    //         8                 26
    std::string s = "function(function() -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 8), 26u);
}

TEST(FindMatchingCloseParen, DeeplyNested) {
    // function(function(function() -> int) -> int) -> int
    //         ^                                  ^
    std::string s = "function(function(function() -> int) -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 8), 43u);
}

TEST(FindMatchingCloseParen, MultipleParamsWithNested) {
    // function(int, function() -> int) -> int
    //         ^                      ^
    std::string s = "function(int, function() -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 8), 31u);
}

TEST(FindMatchingCloseParen, NoMatch) {
    std::string s = "function(int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 8), std::string::npos);
}

// ============================================================
// Cross-category type duplicate rejection (#815)
// ============================================================

TEST_F(CodeGenTest, RecordThenEnumWithSameNameRejected) {
    expectCompileError(
        "record Foo:\n"
        "    x: int\n"
        "enum Foo:\n"
        "    A\n"
        "    B\n",
        "already defined as a record");
}

TEST_F(CodeGenTest, EnumThenRecordWithSameNameRejected) {
    expectCompileError(
        "enum Foo:\n"
        "    A\n"
        "    B\n"
        "record Foo:\n"
        "    x: int\n",
        "already defined as an enum");
}

TEST_F(CodeGenTest, RecordThenGenericEnumWithSameNameRejected) {
    expectCompileError(
        "record Foo:\n"
        "    x: int\n"
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n",
        "already defined as a record");
}

TEST_F(CodeGenTest, GenericEnumThenRecordWithSameNameRejected) {
    expectCompileError(
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "record Foo:\n"
        "    x: int\n",
        "already defined as a generic enum");
}

TEST_F(CodeGenTest, EnumThenGenericEnumWithSameNameRejected) {
    expectCompileError(
        "enum Foo:\n"
        "    A\n"
        "enum Foo<T>:\n"
        "    MySome(T)\n",
        "already defined as an enum");
}

TEST_F(CodeGenTest, GenericEnumThenEnumWithSameNameRejected) {
    expectCompileError(
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "enum Foo:\n"
        "    A\n",
        "already defined as a generic enum");
}

TEST_F(CodeGenTest, DuplicateGenericEnumRejected) {
    expectCompileError(
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "enum Foo<U>:\n"
        "    MyOther(U)\n",
        "generic enum 'Foo' is already defined");
}

// ============================================================
// Type alias cross-category duplicate rejection (#850)
// ============================================================

TEST_F(CodeGenTest, TypeAliasThenRecordWithSameNameRejected) {
    expectCompileError(
        "type Foo = int\n"
        "record Foo:\n"
        "    x: int\n",
        "already defined as a type alias");
}

TEST_F(CodeGenTest, RecordThenTypeAliasWithSameNameRejected) {
    expectCompileError(
        "record Foo:\n"
        "    x: int\n"
        "type Foo = int\n",
        "already defined as a record");
}

TEST_F(CodeGenTest, TypeAliasThenEnumWithSameNameRejected) {
    expectCompileError(
        "type Foo = int\n"
        "enum Foo:\n"
        "    A\n"
        "    B\n",
        "already defined as a type alias");
}

TEST_F(CodeGenTest, EnumThenTypeAliasWithSameNameRejected) {
    expectCompileError(
        "enum Foo:\n"
        "    A\n"
        "    B\n"
        "type Foo = int\n",
        "already defined as an enum");
}

TEST_F(CodeGenTest, TypeAliasThenGenericEnumWithSameNameRejected) {
    expectCompileError(
        "type Foo = int\n"
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n",
        "already defined as a type alias");
}

TEST_F(CodeGenTest, GenericEnumThenTypeAliasWithSameNameRejected) {
    expectCompileError(
        "enum Foo<T>:\n"
        "    MySome(T)\n"
        "    MyNone\n"
        "type Foo = int\n",
        "already defined as a generic enum");
}

TEST_F(CodeGenTest, DuplicateTypeAliasRejected) {
    expectCompileError(
        "type Foo = int\n"
        "type Foo = str\n",
        "type alias 'Foo' is already defined");
}

TEST_F(CodeGenTest, UnionTypeAliasThenRecordWithSameNameRejected) {
    expectCompileError(
        "type Foo = int | str\n"
        "record Foo:\n"
        "    x: int\n",
        "already defined as a type alias");
}

TEST_F(CodeGenTest, RecordThenUnionTypeAliasWithSameNameRejected) {
    expectCompileError(
        "record Foo:\n"
        "    x: int\n"
        "type Foo = int | str\n",
        "already defined as a record");
}

TEST_F(CodeGenTest, DuplicateUnionTypeAliasRejected) {
    expectCompileError(
        "type Foo = int | str\n"
        "type Foo = bool | str\n",
        "type alias 'Foo' is already defined");
}

// ============================================================
// bool rejected in arithmetic operators (#1030)
// ============================================================

TEST_F(CodeGenTest, BoolLhsAddRejected) {
    expectCompileError("x = true + 1\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolRhsAddRejected) {
    expectCompileError("x = 1 + true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolBothAddRejected) {
    expectCompileError("x = true + true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolSubRejected) {
    expectCompileError("x = true - 1\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolMulRejected) {
    expectCompileError("x = true * 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolDivIntRejected) {
    expectCompileError("x = true / 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolDivFloatRejected) {
    expectCompileError("x = true / 2.0\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolFloorDivRejected) {
    expectCompileError("x = true // 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolModRejected) {
    expectCompileError("x = true % 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolPowRejected) {
    expectCompileError("x = true ** 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolUnaryNegRejected) {
    expectCompileError("x = -true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolCompoundAddAssignRejected) {
    expectCompileError(
        "x = 0\n"
        "x += true\n",
        "bool cannot be used with arithmetic operator");
}

// ============================================================
// bool rejected in bitwise operators (#1030)
// ============================================================

TEST_F(CodeGenTest, BoolBitwiseAndRejected) {
    expectCompileError("x = true & 1\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolBitwiseOrRejected) {
    expectCompileError("x = 1 | true\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolBitwiseXorRejected) {
    expectCompileError("x = true ^ true\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolShlRejected) {
    expectCompileError("x = true << 1\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolShrRejected) {
    expectCompileError("x = 1 >> true\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolUnaryBitwiseNotRejected) {
    expectCompileError("x = ~true\n",
        "bool cannot be used with bitwise operator");
}

TEST_F(CodeGenTest, BoolCompoundAndAssignRejected) {
    expectCompileError(
        "x = 0\n"
        "x &= true\n",
        "bool cannot be used with bitwise operator");
}
