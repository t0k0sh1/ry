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
