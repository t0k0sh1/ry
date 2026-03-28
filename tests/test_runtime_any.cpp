#include "ry/runtime_any.hpp"
#include <gtest/gtest.h>
#include <cstring>
#include <cstdlib>
#include <cmath>

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

static RyAny mkUnit() {
    RyAny a;
    a.tag = TAG_UNIT;
    memset(a.data, 0, sizeof(a.data));
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

TEST(RuntimeAnyArith, AddVariants) {
    // IntPlusInt
    {
        RyAny a = mkInt(3), b = mkInt(4), r;
        __ry_any_add(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), 7);
    }
    // FloatPlusFloat
    {
        RyAny a = mkFloat(1.5), b = mkFloat(2.5), r;
        __ry_any_add(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 4.0);
    }
    // IntPlusFloat
    {
        RyAny a = mkInt(3), b = mkFloat(0.5), r;
        __ry_any_add(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
    }
    // FloatPlusInt
    {
        RyAny a = mkFloat(1.5), b = mkInt(2), r;
        __ry_any_add(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
    }
    // StrPlusStr
    {
        RyAny a = mkStr("hello"), b = mkStr(" world"), r;
        __ry_any_add(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_STR);
        EXPECT_STREQ(getStr(&r), "hello world");
        free(const_cast<char *>(getStr(&r)));
    }
}

// ===== Arithmetic: sub =====

TEST(RuntimeAnyArith, SubVariants) {
    // IntMinusInt
    {
        RyAny a = mkInt(10), b = mkInt(3), r;
        __ry_any_sub(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), 7);
    }
    // IntMinusFloat
    {
        RyAny a = mkInt(5), b = mkFloat(1.5), r;
        __ry_any_sub(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
    }
}

// ===== Arithmetic: mul =====

TEST(RuntimeAnyArith, MulVariants) {
    // IntTimesInt
    {
        RyAny a = mkInt(6), b = mkInt(7), r;
        __ry_any_mul(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), 42);
    }
    // StrTimesInt
    {
        RyAny a = mkStr("ab"), b = mkInt(3), r;
        __ry_any_mul(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_STR);
        EXPECT_STREQ(getStr(&r), "ababab");
        free(const_cast<char *>(getStr(&r)));
    }
    // IntTimesStr
    {
        RyAny a = mkInt(2), b = mkStr("xy"), r;
        __ry_any_mul(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_STR);
        EXPECT_STREQ(getStr(&r), "xyxy");
        free(const_cast<char *>(getStr(&r)));
    }
    // StrTimesZero
    {
        RyAny a = mkStr("abc"), b = mkInt(0), r;
        __ry_any_mul(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_STR);
        EXPECT_STREQ(getStr(&r), "");
        free(const_cast<char *>(getStr(&r)));
    }
    // StrTimesNegative
    {
        RyAny a = mkStr("abc"), b = mkInt(-5), r;
        __ry_any_mul(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_STR);
        EXPECT_STREQ(getStr(&r), "");
        free(const_cast<char *>(getStr(&r)));
    }
}

// ===== Arithmetic: div =====

TEST(RuntimeAnyArith, DivVariants) {
    // IntDivInt
    {
        RyAny a = mkInt(10), b = mkInt(3), r;
        __ry_any_div(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 10.0 / 3.0);
    }
    // FloatDivFloat
    {
        RyAny a = mkFloat(7.0), b = mkFloat(2.0), r;
        __ry_any_div(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.5);
    }
}

// ===== Arithmetic: mod =====

TEST(RuntimeAnyArith, ModVariants) {
    // IntModInt
    {
        RyAny a = mkInt(10), b = mkInt(3), r;
        __ry_any_mod(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), 1);
    }
    // FloatModFloat
    {
        RyAny a = mkFloat(7.5), b = mkFloat(2.0), r;
        __ry_any_mod(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 1.5);
    }
}

// ===== Arithmetic: floordiv =====

TEST(RuntimeAnyArith, FloordivVariants) {
    // IntFloorDivInt
    {
        RyAny a = mkInt(7), b = mkInt(2), r;
        __ry_any_floordiv(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), 3);
    }
    // NegativeFloorDiv
    {
        RyAny a = mkInt(-7), b = mkInt(2), r;
        __ry_any_floordiv(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), -4);
    }
    // FloatFloorDiv
    {
        RyAny a = mkFloat(7.5), b = mkFloat(2.0), r;
        __ry_any_floordiv(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.0);
    }
    // IntFloordivFloat
    {
        RyAny a = mkInt(7), b = mkFloat(2.0), r;
        __ry_any_floordiv(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 3.0);
    }
}

// ===== Arithmetic: pow =====

TEST(RuntimeAnyArith, PowVariants) {
    // IntPowInt
    {
        RyAny a = mkInt(2), b = mkInt(10), r;
        __ry_any_pow(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 1024.0);
    }
    // IntPowNegative
    {
        RyAny a = mkInt(2), b = mkInt(-1), r;
        __ry_any_pow(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 0.5);
    }
    // FloatPowFloat
    {
        RyAny a = mkFloat(4.0), b = mkFloat(0.5), r;
        __ry_any_pow(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 2.0);
    }
    // FloatPowInt
    {
        RyAny a = mkFloat(2.0), b = mkInt(3), r;
        __ry_any_pow(&r, &a, &b);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), 8.0);
    }
}

// ===== Arithmetic: neg =====

TEST(RuntimeAnyArith, NegVariants) {
    // NegInt
    {
        RyAny a = mkInt(42), r;
        __ry_any_neg(&r, &a);
        EXPECT_EQ(r.tag, TAG_INT);
        EXPECT_EQ(getInt(&r), -42);
    }
    // NegFloat
    {
        RyAny a = mkFloat(3.14), r;
        __ry_any_neg(&r, &a);
        EXPECT_EQ(r.tag, TAG_FLOAT);
        EXPECT_DOUBLE_EQ(getFloat(&r), -3.14);
    }
}

// ===== Comparison: eq/ne (#222) =====

TEST(RuntimeAnyCmp, EqNeVariants) {
    // IntEqInt
    {
        RyAny a = mkInt(42), b = mkInt(42);
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
        EXPECT_EQ(__ry_any_ne(&a, &b), 0);
    }
    // IntNeInt
    {
        RyAny a = mkInt(1), b = mkInt(2);
        EXPECT_EQ(__ry_any_eq(&a, &b), 0);
        EXPECT_EQ(__ry_any_ne(&a, &b), 1);
    }
    // FloatEqFloat
    {
        RyAny a = mkFloat(3.14), b = mkFloat(3.14);
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    }
    // IntEqFloat
    {
        RyAny a = mkInt(5), b = mkFloat(5.0);
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    }
    // StrEqStr
    {
        RyAny a = mkStr("hello"), b = mkStr("hello");
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    }
    // StrNeStr
    {
        RyAny a = mkStr("hello"), b = mkStr("world");
        EXPECT_EQ(__ry_any_eq(&a, &b), 0);
    }
    // BoolEqBool
    {
        RyAny a = mkBool(true), b = mkBool(true);
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    }
    // DifferentTypesNotEqual
    {
        RyAny a = mkInt(1), b = mkStr("1");
        EXPECT_EQ(__ry_any_eq(&a, &b), 0);
    }
    // UnitEqUnit
    {
        RyAny a = mkUnit(), b = mkUnit();
        EXPECT_EQ(__ry_any_eq(&a, &b), 1);
    }
}

// ===== Comparison: ordering (#222) =====

TEST(RuntimeAnyCmp, OrdVariants) {
    // IntLtInt
    {
        RyAny a = mkInt(1), b = mkInt(2);
        EXPECT_EQ(__ry_any_lt(&a, &b), 1);
        EXPECT_EQ(__ry_any_le(&a, &b), 1);
        EXPECT_EQ(__ry_any_gt(&a, &b), 0);
        EXPECT_EQ(__ry_any_ge(&a, &b), 0);
    }
    // IntGeInt
    {
        RyAny a = mkInt(5), b = mkInt(5);
        EXPECT_EQ(__ry_any_le(&a, &b), 1);
        EXPECT_EQ(__ry_any_ge(&a, &b), 1);
        EXPECT_EQ(__ry_any_lt(&a, &b), 0);
        EXPECT_EQ(__ry_any_gt(&a, &b), 0);
    }
    // FloatLtFloat
    {
        RyAny a = mkFloat(1.5), b = mkFloat(2.5);
        EXPECT_EQ(__ry_any_lt(&a, &b), 1);
        EXPECT_EQ(__ry_any_gt(&a, &b), 0);
    }
    // IntLtFloat
    {
        RyAny a = mkInt(3), b = mkFloat(3.5);
        EXPECT_EQ(__ry_any_lt(&a, &b), 1);
    }
    // StrLtStr
    {
        RyAny a = mkStr("apple"), b = mkStr("banana");
        EXPECT_EQ(__ry_any_lt(&a, &b), 1);
        EXPECT_EQ(__ry_any_gt(&a, &b), 0);
    }
}

// ===== NaN comparisons =====

TEST(RuntimeAnyCmp, NaNComparisons) {
    RyAny nan = mkFloat(NAN), one = mkFloat(1.0), iVal = mkInt(42);

    // EqNaN
    EXPECT_EQ(__ry_any_eq(&nan, &one), 0);
    EXPECT_EQ(__ry_any_eq(&one, &nan), 0);
    EXPECT_EQ(__ry_any_eq(&nan, &nan), 0);

    // NeNaN
    EXPECT_EQ(__ry_any_ne(&nan, &one), 0);
    EXPECT_EQ(__ry_any_ne(&one, &nan), 0);
    EXPECT_EQ(__ry_any_ne(&nan, &nan), 0);

    // LtNaN
    EXPECT_EQ(__ry_any_lt(&nan, &one), 0);
    EXPECT_EQ(__ry_any_lt(&one, &nan), 0);
    EXPECT_EQ(__ry_any_lt(&nan, &nan), 0);

    // LeNaN
    EXPECT_EQ(__ry_any_le(&nan, &one), 0);
    EXPECT_EQ(__ry_any_le(&one, &nan), 0);
    EXPECT_EQ(__ry_any_le(&nan, &nan), 0);

    // GtNaN
    EXPECT_EQ(__ry_any_gt(&nan, &one), 0);
    EXPECT_EQ(__ry_any_gt(&one, &nan), 0);
    EXPECT_EQ(__ry_any_gt(&nan, &nan), 0);

    // GeNaN
    EXPECT_EQ(__ry_any_ge(&nan, &one), 0);
    EXPECT_EQ(__ry_any_ge(&one, &nan), 0);
    EXPECT_EQ(__ry_any_ge(&nan, &nan), 0);

    // MixedIntNaN
    EXPECT_EQ(__ry_any_eq(&nan, &iVal), 0);
    EXPECT_EQ(__ry_any_ne(&nan, &iVal), 0);
    EXPECT_EQ(__ry_any_lt(&nan, &iVal), 0);
    EXPECT_EQ(__ry_any_le(&nan, &iVal), 0);
    EXPECT_EQ(__ry_any_gt(&nan, &iVal), 0);
    EXPECT_EQ(__ry_any_ge(&nan, &iVal), 0);
}

// ===== Death tests: arithmetic errors =====

TEST_F(RuntimeAnyDeathTest, AddIntPlusStrError) {
    RyAny a = mkInt(1), b = mkStr("x");
    EXPECT_EXIT(
        { RyAny r; __ry_any_add(&r, &a, &b); },
        ::testing::ExitedWithCode(1),
        "operator \\+ not supported for int and str"
    );
}

TEST_F(RuntimeAnyDeathTest, ArithDivisionAndModByZero) {
    // DivByZero
    {
        RyAny a = mkInt(1), b = mkInt(0);
        EXPECT_EXIT(
            { RyAny r; __ry_any_div(&r, &a, &b); },
            ::testing::ExitedWithCode(1),
            "division by zero"
        );
    }
    // ModByZero
    {
        RyAny a = mkInt(5), b = mkInt(0);
        EXPECT_EXIT(
            { RyAny r; __ry_any_mod(&r, &a, &b); },
            ::testing::ExitedWithCode(1),
            "modulo by zero"
        );
    }
    // FloordivByZero
    {
        RyAny a = mkInt(5), b = mkInt(0);
        EXPECT_EXIT(
            { RyAny r; __ry_any_floordiv(&r, &a, &b); },
            ::testing::ExitedWithCode(1),
            "division by zero"
        );
    }
}

TEST_F(RuntimeAnyDeathTest, NegStrError) {
    RyAny a = mkStr("x");
    EXPECT_EXIT(
        { RyAny r; __ry_any_neg(&r, &a); },
        ::testing::ExitedWithCode(1),
        "unary - not supported for str"
    );
}

// ===== Death tests: ordering type errors =====

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

// ===== Death tests: NaN ordering with incompatible types =====

TEST_F(RuntimeAnyDeathTest, NaNOrderingWithBool) {
    RyAny nan = mkFloat(NAN);
    RyAny bTrue = mkBool(true);

    EXPECT_EXIT(__ry_any_lt(&nan, &bTrue), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_le(&nan, &bTrue), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_gt(&nan, &bTrue), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_ge(&nan, &bTrue), ::testing::ExitedWithCode(1), ".*");

    EXPECT_EXIT(__ry_any_lt(&bTrue, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_le(&bTrue, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_gt(&bTrue, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_ge(&bTrue, &nan), ::testing::ExitedWithCode(1), ".*");
}

TEST_F(RuntimeAnyDeathTest, NaNOrderingWithString) {
    RyAny nan = mkFloat(NAN);
    RyAny str = mkStr("x");

    EXPECT_EXIT(__ry_any_lt(&nan, &str), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_le(&nan, &str), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_gt(&nan, &str), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_ge(&nan, &str), ::testing::ExitedWithCode(1), ".*");

    EXPECT_EXIT(__ry_any_lt(&str, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_le(&str, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_gt(&str, &nan), ::testing::ExitedWithCode(1), ".*");
    EXPECT_EXIT(__ry_any_ge(&str, &nan), ::testing::ExitedWithCode(1), ".*");
}

// ===== __ry_any_to_string =====

TEST(RuntimeAnyToString, IntToString) {
    RyAny a = mkInt(42);
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "42");
    free(const_cast<char*>(s));
}

TEST(RuntimeAnyToString, NegativeIntToString) {
    RyAny a = mkInt(-123);
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "-123");
    free(const_cast<char*>(s));
}

TEST(RuntimeAnyToString, FloatToString) {
    RyAny a = mkFloat(3.14);
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "3.14");
    free(const_cast<char*>(s));
}

TEST(RuntimeAnyToString, BoolTrueToString) {
    RyAny a = mkBool(true);
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "true");
}

TEST(RuntimeAnyToString, BoolFalseToString) {
    RyAny a = mkBool(false);
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "false");
}

TEST(RuntimeAnyToString, StrToString) {
    RyAny a = mkStr("hello");
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "hello");
}

TEST(RuntimeAnyToString, UnitToString) {
    RyAny a = mkUnit();
    const char *s = __ry_any_to_string(&a);
    EXPECT_STREQ(s, "Unit");
    // s points to a string literal — no free needed
}
