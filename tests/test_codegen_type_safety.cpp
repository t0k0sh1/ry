#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// Struct type rejected by arithmetic operators
// ============================================================

TEST_F(CodeGenTest, StructAddRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p + 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructSubRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p - 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructMulRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p * 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructDivRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p / 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructFloorDivRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p // 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructModRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p % 2\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructPowerRejected) {
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

TEST_F(CodeGenTest, StructNegationRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "-p\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructBitwiseNotRejected) {
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

TEST_F(CodeGenTest, StructBitwiseAndRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p & 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructBitwiseOrRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "p | 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructBitwiseXorRejected) {
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

TEST_F(CodeGenTest, StructLessThanRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(3, 4)\n"
        "p < q\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StructGreaterThanRejected) {
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

TEST_F(CodeGenTest, StructEqualityStillWorks) {
    EXPECT_EQ(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "q = Point(1, 2)\n"
        "print(p == q)\n"
    ), "true\n");
}

TEST_F(CodeGenTest, StructInequalityStillWorks) {
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
