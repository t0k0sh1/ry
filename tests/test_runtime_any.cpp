#include "ry/runtime_any.hpp"
#include <gtest/gtest.h>
#include <cstring>
#include <cstdlib>

// Death tests use "threadsafe" style to avoid fork issues with LLVM state
class RuntimeAnyDeathTest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        GTEST_FLAG_SET(death_test_style, "threadsafe");
    }
};

// ===== Helpers =====

static RyAny mkInt(int64_t v) {
    RyAny a;
    a.tag = TAG_INT;
    memcpy(a.data, &v, sizeof(v));
    return a;
}

static RyAny mkFloat(double v) {
    RyAny a;
    a.tag = TAG_FLOAT;
    memcpy(a.data, &v, sizeof(v));
    return a;
}

static RyAny mkStr(const char *v) {
    RyAny a;
    a.tag = TAG_STR;
    memcpy(a.data, &v, sizeof(v));
    return a;
}

static RyAny mkBool(bool v) {
    RyAny a;
    a.tag = TAG_BOOL;
    int64_t iv = v ? 1 : 0;
    memcpy(a.data, &iv, sizeof(iv));
    return a;
}

static int64_t getInt(const RyAny *a) {
    int64_t v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

static double getFloat(const RyAny *a) {
    double v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

static const char *getStr(const RyAny *a) {
    const char *v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

// ===== Type error (#224) =====

TEST_F(RuntimeAnyDeathTest, TypeErrorPrintsMessageAndExits) {
    EXPECT_EXIT(
        __ry_any_type_error("+", TAG_INT, TAG_STR),
        ::testing::ExitedWithCode(1),
        "runtime error: operator \\+ not supported for int and str"
    );
}

TEST_F(RuntimeAnyDeathTest, TypeErrorDifferentOps) {
    EXPECT_EXIT(
        __ry_any_type_error("-", TAG_BOOL, TAG_FLOAT),
        ::testing::ExitedWithCode(1),
        "operator - not supported for bool and float"
    );
}

// ===== Arithmetic: add (#221) =====

TEST(RuntimeAnyAdd, IntPlusInt) {
    RyAny a = mkInt(3), b = mkInt(4), r;
    __ry_any_add(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 7);
}

TEST(RuntimeAnyAdd, FloatPlusFloat) {
    RyAny a = mkFloat(1.5), b = mkFloat(2.5), r;
    __ry_any_add(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 4.0);
}

TEST(RuntimeAnyAdd, IntPlusFloat) {
    RyAny a = mkInt(3), b = mkFloat(0.5), r;
    __ry_any_add(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
}

TEST(RuntimeAnyAdd, FloatPlusInt) {
    RyAny a = mkFloat(1.5), b = mkInt(2), r;
    __ry_any_add(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
}

TEST(RuntimeAnyAdd, StrPlusStr) {
    RyAny a = mkStr("hello"), b = mkStr(" world"), r;
    __ry_any_add(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_STR);
    EXPECT_STREQ(getStr(&r), "hello world");
    free(const_cast<char *>(getStr(&r)));
}

TEST_F(RuntimeAnyDeathTest, AddIntPlusStrError) {
    RyAny a = mkInt(1), b = mkStr("x");
    EXPECT_EXIT(
        { RyAny r; __ry_any_add(&r, &a, &b); },
        ::testing::ExitedWithCode(1),
        "operator \\+ not supported for int and str"
    );
}

// ===== Arithmetic: sub =====

TEST(RuntimeAnySub, IntMinusInt) {
    RyAny a = mkInt(10), b = mkInt(3), r;
    __ry_any_sub(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 7);
}

TEST(RuntimeAnySub, IntMinusFloat) {
    RyAny a = mkInt(5), b = mkFloat(1.5), r;
    __ry_any_sub(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
}

// ===== Arithmetic: mul =====

TEST(RuntimeAnyMul, IntTimesInt) {
    RyAny a = mkInt(6), b = mkInt(7), r;
    __ry_any_mul(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 42);
}

TEST(RuntimeAnyMul, StrTimesInt) {
    RyAny a = mkStr("ab"), b = mkInt(3), r;
    __ry_any_mul(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_STR);
    EXPECT_STREQ(getStr(&r), "ababab");
    free(const_cast<char *>(getStr(&r)));
}

TEST(RuntimeAnyMul, IntTimesStr) {
    RyAny a = mkInt(2), b = mkStr("xy"), r;
    __ry_any_mul(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_STR);
    EXPECT_STREQ(getStr(&r), "xyxy");
    free(const_cast<char *>(getStr(&r)));
}

TEST(RuntimeAnyMul, StrTimesZero) {
    RyAny a = mkStr("abc"), b = mkInt(0), r;
    __ry_any_mul(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_STR);
    EXPECT_STREQ(getStr(&r), "");
    free(const_cast<char *>(getStr(&r)));
}

// ===== Arithmetic: div =====

TEST(RuntimeAnyDiv, IntDivInt) {
    RyAny a = mkInt(10), b = mkInt(3), r;
    __ry_any_div(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 3);
}

TEST(RuntimeAnyDiv, FloatDivFloat) {
    RyAny a = mkFloat(7.0), b = mkFloat(2.0), r;
    __ry_any_div(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
}

TEST_F(RuntimeAnyDeathTest, DivByZero) {
    RyAny a = mkInt(1), b = mkInt(0);
    EXPECT_EXIT(
        { RyAny r; __ry_any_div(&r, &a, &b); },
        ::testing::ExitedWithCode(1),
        "division by zero"
    );
}

// ===== Arithmetic: mod =====

TEST(RuntimeAnyMod, IntModInt) {
    RyAny a = mkInt(10), b = mkInt(3), r;
    __ry_any_mod(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 1);
}

TEST_F(RuntimeAnyDeathTest, ModByZero) {
    RyAny a = mkInt(5), b = mkInt(0);
    EXPECT_EXIT(
        { RyAny r; __ry_any_mod(&r, &a, &b); },
        ::testing::ExitedWithCode(1),
        "modulo by zero"
    );
}

// ===== Arithmetic: floordiv =====

TEST(RuntimeAnyFloordiv, IntFloorDivInt) {
    RyAny a = mkInt(7), b = mkInt(2), r;
    __ry_any_floordiv(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 3);
}

TEST(RuntimeAnyFloordiv, NegativeFloorDiv) {
    RyAny a = mkInt(-7), b = mkInt(2), r;
    __ry_any_floordiv(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), -4);
}

TEST(RuntimeAnyFloordiv, FloatFloorDiv) {
    RyAny a = mkFloat(7.5), b = mkFloat(2.0), r;
    __ry_any_floordiv(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.0);
}

// ===== Arithmetic: pow =====

TEST(RuntimeAnyPow, IntPowInt) {
    RyAny a = mkInt(2), b = mkInt(10), r;
    __ry_any_pow(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), 1024);
}

TEST(RuntimeAnyPow, IntPowNegative) {
    RyAny a = mkInt(2), b = mkInt(-1), r;
    __ry_any_pow(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 0.5);
}

TEST(RuntimeAnyPow, FloatPowFloat) {
    RyAny a = mkFloat(4.0), b = mkFloat(0.5), r;
    __ry_any_pow(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 2.0);
}

// ===== Arithmetic: neg =====

TEST(RuntimeAnyNeg, NegInt) {
    RyAny a = mkInt(42), r;
    __ry_any_neg(&r, &a);
    EXPECT_EQ(r.tag, TAG_INT);
    EXPECT_EQ(getInt(&r), -42);
}

TEST(RuntimeAnyNeg, NegFloat) {
    RyAny a = mkFloat(3.14), r;
    __ry_any_neg(&r, &a);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), -3.14);
}

TEST_F(RuntimeAnyDeathTest, NegStrError) {
    RyAny a = mkStr("x");
    EXPECT_EXIT(
        { RyAny r; __ry_any_neg(&r, &a); },
        ::testing::ExitedWithCode(1),
        "unary - not supported for str"
    );
}

// ===== Comparison: eq/ne (#222) =====

TEST(RuntimeAnyEq, IntEqInt) {
    RyAny a = mkInt(42), b = mkInt(42);
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    EXPECT_EQ(__ry_any_ne(&a, &b), 0);
}

TEST(RuntimeAnyEq, IntNeInt) {
    RyAny a = mkInt(1), b = mkInt(2);
    EXPECT_EQ(__ry_any_eq(&a, &b), 0);
    EXPECT_EQ(__ry_any_ne(&a, &b), 1);
}

TEST(RuntimeAnyEq, FloatEqFloat) {
    RyAny a = mkFloat(3.14), b = mkFloat(3.14);
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
}

TEST(RuntimeAnyEq, IntEqFloat) {
    RyAny a = mkInt(5), b = mkFloat(5.0);
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
}

TEST(RuntimeAnyEq, StrEqStr) {
    RyAny a = mkStr("hello"), b = mkStr("hello");
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
}

TEST(RuntimeAnyEq, StrNeStr) {
    RyAny a = mkStr("hello"), b = mkStr("world");
    EXPECT_EQ(__ry_any_eq(&a, &b), 0);
}

TEST(RuntimeAnyEq, BoolEqBool) {
    RyAny a = mkBool(true), b = mkBool(true);
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
}

TEST(RuntimeAnyEq, DifferentTypesNotEqual) {
    RyAny a = mkInt(1), b = mkStr("1");
    EXPECT_EQ(__ry_any_eq(&a, &b), 0);
}

TEST(RuntimeAnyEq, UnitEqUnit) {
    RyAny a, b;
    a.tag = TAG_UNIT;
    b.tag = TAG_UNIT;
    EXPECT_EQ(__ry_any_eq(&a, &b), 1);
}

// ===== Comparison: ordering (#222) =====

TEST(RuntimeAnyOrd, IntLtInt) {
    RyAny a = mkInt(1), b = mkInt(2);
    EXPECT_EQ(__ry_any_lt(&a, &b), 1);
    EXPECT_EQ(__ry_any_le(&a, &b), 1);
    EXPECT_EQ(__ry_any_gt(&a, &b), 0);
    EXPECT_EQ(__ry_any_ge(&a, &b), 0);
}

TEST(RuntimeAnyOrd, IntGeInt) {
    RyAny a = mkInt(5), b = mkInt(5);
    EXPECT_EQ(__ry_any_le(&a, &b), 1);
    EXPECT_EQ(__ry_any_ge(&a, &b), 1);
    EXPECT_EQ(__ry_any_lt(&a, &b), 0);
    EXPECT_EQ(__ry_any_gt(&a, &b), 0);
}

TEST(RuntimeAnyOrd, FloatLtFloat) {
    RyAny a = mkFloat(1.5), b = mkFloat(2.5);
    EXPECT_EQ(__ry_any_lt(&a, &b), 1);
    EXPECT_EQ(__ry_any_gt(&a, &b), 0);
}

TEST(RuntimeAnyOrd, IntLtFloat) {
    RyAny a = mkInt(3), b = mkFloat(3.5);
    EXPECT_EQ(__ry_any_lt(&a, &b), 1);
}

TEST(RuntimeAnyOrd, StrLtStr) {
    RyAny a = mkStr("apple"), b = mkStr("banana");
    EXPECT_EQ(__ry_any_lt(&a, &b), 1);
    EXPECT_EQ(__ry_any_gt(&a, &b), 0);
}

TEST_F(RuntimeAnyDeathTest, OrdDifferentTypesError) {
    RyAny a = mkInt(1), b = mkStr("x");
    EXPECT_EXIT(
        __ry_any_lt(&a, &b),
        ::testing::ExitedWithCode(1),
        "operator < not supported for int and str"
    );
}

TEST_F(RuntimeAnyDeathTest, OrdGtReportsCorrectOp) {
    RyAny a = mkInt(1), b = mkBool(true);
    EXPECT_EXIT(
        __ry_any_gt(&a, &b),
        ::testing::ExitedWithCode(1),
        "operator > not supported for int and bool"
    );
}

// ===== Additional coverage =====

TEST(RuntimeAnyMod, FloatModFloat) {
    RyAny a = mkFloat(7.5), b = mkFloat(2.0), r;
    __ry_any_mod(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 1.5);
}

TEST(RuntimeAnyFloordiv, IntFloordivFloat) {
    RyAny a = mkInt(7), b = mkFloat(2.0), r;
    __ry_any_floordiv(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 3.0);
}

TEST(RuntimeAnyPow, FloatPowInt) {
    RyAny a = mkFloat(2.0), b = mkInt(3), r;
    __ry_any_pow(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_FLOAT);
    EXPECT_DOUBLE_EQ(getFloat(&r), 8.0);
}

TEST(RuntimeAnyMul, StrTimesNegative) {
    RyAny a = mkStr("abc"), b = mkInt(-5), r;
    __ry_any_mul(&r, &a, &b);
    EXPECT_EQ(r.tag, TAG_STR);
    EXPECT_STREQ(getStr(&r), "");
    free(const_cast<char *>(getStr(&r)));
}

TEST_F(RuntimeAnyDeathTest, FloordivByZero) {
    RyAny a = mkInt(5), b = mkInt(0);
    EXPECT_EXIT(
        { RyAny r; __ry_any_floordiv(&r, &a, &b); },
        ::testing::ExitedWithCode(1),
        "division by zero"
    );
}
