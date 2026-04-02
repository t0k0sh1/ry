#include "test_codegen_common.hpp"

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
// List (pointer-backed collection) rejected by arithmetic operators
// ============================================================

TEST_F(CodeGenTest, ListAddRejected) {
    EXPECT_THROW(runSource(
        "xs = [1, 2, 3]\n"
        "xs + 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, ListSubRejected) {
    EXPECT_THROW(runSource(
        "xs = [1, 2, 3]\n"
        "xs - 1\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, ListMulRejected) {
    EXPECT_THROW(runSource(
        "xs = [1, 2, 3]\n"
        "xs * 2\n"
    ), std::runtime_error);
}

// ============================================================
// Struct on right-hand side of binary operator also rejected
// ============================================================

TEST_F(CodeGenTest, RhsStructAddRejected) {
    EXPECT_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "p = Point(1, 2)\n"
        "1 + p\n"
    ), std::runtime_error);
}

// ============================================================
// str rejected by numeric comparison operators (< <= > >=)
// ============================================================

TEST_F(CodeGenTest, StringLessThanRejected) {
    EXPECT_THROW(runSource(
        "\"abc\" < \"def\"\n"
    ), std::runtime_error);
}

TEST_F(CodeGenTest, StringGreaterThanRejected) {
    EXPECT_THROW(runSource(
        "\"abc\" > \"def\"\n"
    ), std::runtime_error);
}

// ============================================================
// Regression: struct equality / inequality with same-type operands
// ============================================================

TEST_F(CodeGenTest, StructSameValueEquality) {
    EXPECT_EQ(runSource(
        "record Pair:\n"
        "    a: int\n"
        "    b: int\n"
        "p = Pair(7, 8)\n"
        "q = Pair(7, 8)\n"
        "print(p == q)\n"
    ), "true\n");
}

// ============================================================
// Struct with operator overload: overloaded + must still work
// ============================================================

TEST_F(CodeGenTest, StructOverloadedAddWorks) {
    EXPECT_EQ(runSource(
        "record Vec:\n"
        "    x: int\n"
        "    y: int\n"
        "function operator+(a: Vec, b: Vec) -> Vec:\n"
        "    return Vec(a.x + b.x, a.y + b.y)\n"
        "u = Vec(1, 2)\n"
        "v = Vec(3, 4)\n"
        "w = u + v\n"
        "print(w.x)\n"
        "print(w.y)\n"
    ), "4\n6\n");
}