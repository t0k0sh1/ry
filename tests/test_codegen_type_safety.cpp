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
        "if len(xs) > 0:\n"
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
        "fn f():\n"
        "    exit(1)\n"
        "    print(\"dead\")\n"
        "f()\n"
    ));
}

// Regression: generic Result stringification still works.
TEST_F(CodeGenTest, GenericResultToStrStillWorks) {
    EXPECT_EQ(runSource(
        "r: Result<int, Error> = Ok(42)\n"
        "print(str(r))\n"
    ), "Ok(42)\n");
}

// ============================================================
// findMatchingCloseParen — depth-tracking paren matcher (#768)
// ============================================================

TEST(FindMatchingCloseParen, Simple) {
    // fn(int) -> int
    //   ^   ^
    //   2   6
    std::string s = "fn(int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 2), 6u);
}

TEST(FindMatchingCloseParen, Nested) {
    // fn(fn() -> int) -> int
    //   ^              ^
    //   2              14
    std::string s = "fn(fn() -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 2), 14u);
}

TEST(FindMatchingCloseParen, DeeplyNested) {
    // fn(fn(fn() -> int) -> int) -> int
    //   ^                        ^
    std::string s = "fn(fn(fn() -> int) -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 2), 25u);
}

TEST(FindMatchingCloseParen, MultipleParamsWithNested) {
    // fn(int, fn() -> int) -> int
    //   ^                    ^
    std::string s = "fn(int, fn() -> int) -> int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 2), 19u);
}

TEST(FindMatchingCloseParen, NoMatch) {
    std::string s = "fn(int";
    ASSERT_EQ(CodeGen::findMatchingCloseParen(s, 2), std::string::npos);
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

TEST_F(CodeGenTest, BoolDivRhsRejected) {
    expectCompileError("x = 1 / true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolFloorDivRejected) {
    expectCompileError("x = true // 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolFloorDivRhsRejected) {
    expectCompileError("x = 1 // true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolModRejected) {
    expectCompileError("x = true % 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolModRhsRejected) {
    expectCompileError("x = 1 % true\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolPowRejected) {
    expectCompileError("x = true ** 2\n",
        "bool cannot be used with arithmetic operator");
}

TEST_F(CodeGenTest, BoolPowRhsRejected) {
    expectCompileError("x = 2 ** true\n",
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

TEST_F(CodeGenTest, BoolShlRhsRejected) {
    expectCompileError("x = 1 << true\n",
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

// ============================================================
// str does not support index access (#1026)
// ============================================================

TEST_F(CodeGenTest, StrIndexAccessRejected) {
    expectCompileError(
        "s = \"hello\"\n"
        "_ = s[0]\n",
        {"str", "charAt"});
}

TEST_F(CodeGenTest, StrLiteralIndexAccessRejected) {
    expectCompileError(
        "_ = \"hello\"[0]\n",
        {"str", "charAt"});
}

TEST_F(CodeGenTest, StrFnReturnIndexAccessRejected) {
    expectCompileError(
        "fn f() -> str:\n"
        "    return \"x\"\n"
        "_ = f()[0]\n",
        {"str", "charAt"});
}

TEST_F(CodeGenTest, StrIndexAssignmentRejected) {
    expectCompileError(
        "s = \"hello\"\n"
        "s[0] = \"x\"\n",
        {"str", "immutable"});
}

TEST_F(CodeGenTest, StrFnReturnVarIndexAssignmentRejected) {
    expectCompileError(
        "fn f() -> str:\n"
        "    return \"x\"\n"
        "s = f()\n"
        "s[0] = \"y\"\n",
        {"str", "immutable"});
}

// ============================================================
// bytesToStr / writeBytes require List<u8> (#1055)
// ============================================================

static const std::string IO_DECLS_1055 = R"(
@native("io")
fn bytesToStr(bs: List<u8>) -> Result<str, Error>
@native("io")
fn writeBytes(path: str, data: List<u8>) -> Result<Unit, Error>
)";

TEST_F(CodeGenTest, BytesToStrIntListRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
case bytesToStr([97, 0, 98]):
    Ok(s): print(s)
    Err(e): print(e.message)
)", {"bytesToStr", "List<u8>", "97u8", "toBytes"});
}

TEST_F(CodeGenTest, WriteBytesIntListRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
case writeBytes("/tmp/x", [97, 0, 98]):
    Ok(_): print("ok")
    Err(e): print(e.message)
)", {"writeBytes", "List<u8>", "97u8", "toBytes"});
}

TEST_F(CodeGenTest, BytesToStrU16ListRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
case bytesToStr([97u16, 0u16, 98u16]):
    Ok(s): print(s)
    Err(e): print(e.message)
)", {"bytesToStr", "List<u8>"});
}

// ============================================================
// List<T> annotation propagates element suffix (#1079)
// ============================================================

TEST_F(CodeGenTest, ListU8AnnotationAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97, 0, 98]
case bytesToStr(bs):
    Ok(s): print(s)
    Err(e): print(e.message)
)"));
}

TEST_F(CodeGenTest, ListU8AnnotationRangeCheckRejected) {
    expectCompileError(R"(
bs: List<u8> = [256]
print(len(bs))
)", {"u8 literal out of range"});
}

TEST_F(CodeGenTest, ListI8AnnotationBoundaryCompiles) {
    EXPECT_NO_THROW(compileSource(R"(
bs: List<i8> = [-128]
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListU8AnnotationNegativeRejected) {
    expectCompileError(R"(
bs: List<u8> = [-1]
print(len(bs))
)", {"cannot negate unsigned type"});
}

TEST_F(CodeGenTest, ListU16AnnotationAccepted) {
    EXPECT_NO_THROW(compileSource(R"(
xs: List<u16> = [300, 1000, 65535]
print(len(xs))
)"));
}

// ============================================================
// List<T> reassignment propagates element suffix (#1085)
// ============================================================

TEST_F(CodeGenTest, ListU8ReassignmentAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97, 0, 98]
bs = [99, 100, 101]
case bytesToStr(bs):
    Ok(s): print(s)
    Err(e): print(e.message)
)"));
}

TEST_F(CodeGenTest, ListU8ReassignmentRangeCheckRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
bs: List<u8> = [97]
bs = [256]
print(len(bs))
)", {"u8 literal out of range"});
}

TEST_F(CodeGenTest, ListU8ReassignmentNegativeRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
bs: List<u8> = [97]
bs = [-1]
print(len(bs))
)", {"cannot negate unsigned type"});
}

TEST_F(CodeGenTest, ListI8ReassignmentBoundaryCompiles) {
    EXPECT_NO_THROW(compileSource(R"(
bs: List<i8> = [0]
bs = [-128]
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListU8ModuleGlobalWriteThroughAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97, 0, 98]
fn update() -> int:
    bs = [99, 100, 101]
    return 0
update()
case bytesToStr(bs):
    Ok(s): print(s)
    Err(e): print(e.message)
)"));
}

// ============================================================
// List<T> compound assignment propagates element suffix (#1102)
// ============================================================

TEST_F(CodeGenTest, ListU8CompoundAssignmentAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97]
bs += [99]
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListU8CompoundAssignmentFromVariableAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97]
rhs: List<u8> = [99]
bs += rhs
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListU8ModuleGlobalCompoundAssignmentAccepted) {
    EXPECT_NO_THROW(compileSource(IO_DECLS_1055 + R"(
bs: List<u8> = [97]
fn update() -> int:
    bs += [99]
    return 0
update()
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListI8CompoundAssignmentBoundaryCompiles) {
    EXPECT_NO_THROW(compileSource(R"(
bs: List<i8> = [0]
bs += [-128]
bs += [127]
print(len(bs))
)"));
}

TEST_F(CodeGenTest, ListU8CompoundAssignmentRangeCheckRejected) {
    expectCompileError(IO_DECLS_1055 + R"(
bs: List<u8> = [0]
bs += [256]
print(len(bs))
)", {"u8 literal out of range"});
}

// ============================================================
// reverse!() on a string is rejected with a clear diagnostic (#1124)
// ============================================================

TEST_F(CodeGenTest, ReverseMutOnStringRejected) {
    expectCompileError(
        "s = \"hello\"\n"
        "s.reverse!()\n",
        {"reverse!", "list", "immutable"});
}

// ============================================================
// range-index (..) on non-list receivers is rejected (#1184)
// ============================================================

TEST_F(CodeGenTest, StrRangeIndexRejected) {
    expectCompileError(
        "s = \"hello\"\n"
        "_ = s[0..2]\n",
        {"str", "range index"});
}

TEST_F(CodeGenTest, StrLiteralRangeIndexRejected) {
    expectCompileError(
        "_ = \"hello\"[0..2]\n",
        {"str", "range index"});
}

TEST_F(CodeGenTest, MapRangeIndexRejected) {
    expectCompileError(
        "m = {\"a\": 1}\n"
        "_ = m[0..1]\n",
        {"range index", "list"});
}
