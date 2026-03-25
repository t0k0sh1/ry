#include "ry/runtime_json.hpp"
#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>

// ===== Parse tests =====

TEST(RuntimeJson, ParseObject) {
    void *v = __ry_json_parse("{\"name\": \"Alice\", \"age\": 30}");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "object");
    EXPECT_EQ(__ry_json_len(v), 2);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseArray) {
    void *v = __ry_json_parse("[1, 2, 3]");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "array");
    EXPECT_EQ(__ry_json_len(v), 3);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseString) {
    void *v = __ry_json_parse("\"hello world\"");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "string");
    const char *s = __ry_json_str(v);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "hello world");
    free((void*)s);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseNumber) {
    void *v = __ry_json_parse("42");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "number");
    int64_t out;
    EXPECT_EQ(__ry_json_int(v, &out), 0);
    EXPECT_EQ(out, 42);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseFloat) {
    void *v = __ry_json_parse("3.14");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "number");
    double out;
    EXPECT_EQ(__ry_json_float(v, &out), 0);
    EXPECT_NEAR(out, 3.14, 0.001);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseBoolTrue) {
    void *v = __ry_json_parse("true");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "boolean");
    int64_t out;
    EXPECT_EQ(__ry_json_bool(v, &out), 0);
    EXPECT_EQ(out, 1);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseBoolFalse) {
    void *v = __ry_json_parse("false");
    ASSERT_NE(v, nullptr);
    int64_t out;
    EXPECT_EQ(__ry_json_bool(v, &out), 0);
    EXPECT_EQ(out, 0);
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseNull) {
    void *v = __ry_json_parse("null");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "null");
    __ry_json_free(v);
}

TEST(RuntimeJson, ParseInvalid) {
    void *v = __ry_json_parse("{invalid}");
    EXPECT_EQ(v, nullptr);
}

TEST(RuntimeJson, ParseTrailingContent) {
    void *v = __ry_json_parse("123 456");
    EXPECT_EQ(v, nullptr);
}

TEST(RuntimeJson, ParseEmpty) {
    void *v = __ry_json_parse("");
    EXPECT_EQ(v, nullptr);
}

// ===== Access tests =====

TEST(RuntimeJson, GetObjectField) {
    void *v = __ry_json_parse("{\"name\": \"Alice\"}");
    ASSERT_NE(v, nullptr);
    void *field = __ry_json_get(v, "name");
    ASSERT_NE(field, nullptr);
    const char *s = __ry_json_str(field);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "Alice");
    free((void*)s);
    __ry_json_free(v);
}

TEST(RuntimeJson, GetMissingKey) {
    void *v = __ry_json_parse("{\"a\": 1}");
    ASSERT_NE(v, nullptr);
    void *field = __ry_json_get(v, "missing");
    EXPECT_EQ(field, nullptr);
    __ry_json_free(v);
}

TEST(RuntimeJson, AtArrayIndex) {
    void *v = __ry_json_parse("[10, 20, 30]");
    ASSERT_NE(v, nullptr);
    void *elem = __ry_json_at(v, 1);
    ASSERT_NE(elem, nullptr);
    int64_t out;
    EXPECT_EQ(__ry_json_int(elem, &out), 0);
    EXPECT_EQ(out, 20);
    __ry_json_free(v);
}

TEST(RuntimeJson, AtOutOfBounds) {
    void *v = __ry_json_parse("[1, 2]");
    ASSERT_NE(v, nullptr);
    void *elem = __ry_json_at(v, 5);
    EXPECT_EQ(elem, nullptr);
    __ry_json_free(v);
}

TEST(RuntimeJson, TypeMismatchInt) {
    void *v = __ry_json_parse("\"hello\"");
    ASSERT_NE(v, nullptr);
    int64_t out;
    EXPECT_NE(__ry_json_int(v, &out), 0);
    __ry_json_free(v);
}

TEST(RuntimeJson, TypeMismatchBool) {
    void *v = __ry_json_parse("42");
    ASSERT_NE(v, nullptr);
    int64_t out;
    EXPECT_NE(__ry_json_bool(v, &out), 0);
    __ry_json_free(v);
}

// ===== Nested access =====

TEST(RuntimeJson, NestedObject) {
    void *v = __ry_json_parse("{\"user\": {\"name\": \"Bob\"}}");
    ASSERT_NE(v, nullptr);
    void *user = __ry_json_get(v, "user");
    ASSERT_NE(user, nullptr);
    void *name = __ry_json_get(user, "name");
    ASSERT_NE(name, nullptr);
    const char *s = __ry_json_str(name);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "Bob");
    free((void*)s);
    __ry_json_free(v);
}

// ===== Keys =====

TEST(RuntimeJson, ObjectKeys) {
    void *v = __ry_json_parse("{\"a\": 1, \"b\": 2}");
    ASSERT_NE(v, nullptr);
    void *keys = __ry_json_keys(v);
    ASSERT_NE(keys, nullptr);
    // ListHeader: {len, cap, data}
    struct LH { int64_t len; int64_t cap; void *data; };
    auto *lh = (LH*)keys;
    EXPECT_EQ(lh->len, 2);
    const char **data = (const char**)lh->data;
    EXPECT_STREQ(data[0], "a");
    EXPECT_STREQ(data[1], "b");
    // Cleanup
    for (int64_t i = 0; i < lh->len; i++) free((void*)data[i]);
    free(data);
    free(keys);
    __ry_json_free(v);
}

// ===== Stringify =====

TEST(RuntimeJson, StringifyCompact) {
    void *v = __ry_json_parse("{\"a\":1,\"b\":2}");
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_stringify(v);
    ASSERT_NE(s, nullptr);
    // Round-trip: parse the stringified result
    void *v2 = __ry_json_parse(s);
    ASSERT_NE(v2, nullptr);
    EXPECT_EQ(__ry_json_len(v2), 2);
    free((void*)s);
    __ry_json_free(v);
    __ry_json_free(v2);
}

TEST(RuntimeJson, StringifyPretty) {
    void *v = __ry_json_parse("{\"a\":1}");
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_stringify_pretty(v, 2);
    ASSERT_NE(s, nullptr);
    EXPECT_NE(strstr(s, "\n"), nullptr);
    free((void*)s);
    __ry_json_free(v);
}

// ===== String escapes =====

TEST(RuntimeJson, StringEscapes) {
    void *v = __ry_json_parse("\"hello\\nworld\\t!\"");
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_str(v);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "hello\nworld\t!");
    free((void*)s);
    __ry_json_free(v);
}

TEST(RuntimeJson, UnicodeEscape) {
    void *v = __ry_json_parse("\"\\u0041\"");
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_str(v);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "A");
    free((void*)s);
    __ry_json_free(v);
}

// ===== Float to int conversion =====

TEST(RuntimeJson, FloatToInt) {
    void *v = __ry_json_parse("42.0");
    ASSERT_NE(v, nullptr);
    int64_t out;
    EXPECT_EQ(__ry_json_int(v, &out), 0);
    EXPECT_EQ(out, 42);
    __ry_json_free(v);
}

TEST(RuntimeJson, IntToFloat) {
    void *v = __ry_json_parse("42");
    ASSERT_NE(v, nullptr);
    double out;
    EXPECT_EQ(__ry_json_float(v, &out), 0);
    EXPECT_NEAR(out, 42.0, 0.001);
    __ry_json_free(v);
}

// ===== Empty containers =====

TEST(RuntimeJson, EmptyObject) {
    void *v = __ry_json_parse("{}");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "object");
    EXPECT_EQ(__ry_json_len(v), 0);
    __ry_json_free(v);
}

TEST(RuntimeJson, EmptyArray) {
    void *v = __ry_json_parse("[]");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "array");
    EXPECT_EQ(__ry_json_len(v), 0);
    __ry_json_free(v);
}

// ===== Negative numbers =====

TEST(RuntimeJson, NegativeInt) {
    void *v = __ry_json_parse("-42");
    ASSERT_NE(v, nullptr);
    int64_t out;
    EXPECT_EQ(__ry_json_int(v, &out), 0);
    EXPECT_EQ(out, -42);
    __ry_json_free(v);
}

TEST(RuntimeJson, NegativeFloat) {
    void *v = __ry_json_parse("-3.14");
    ASSERT_NE(v, nullptr);
    double out;
    EXPECT_EQ(__ry_json_float(v, &out), 0);
    EXPECT_NEAR(out, -3.14, 0.001);
    __ry_json_free(v);
}

// ===== Scientific notation =====

TEST(RuntimeJson, ScientificNotation) {
    void *v = __ry_json_parse("1.5e10");
    ASSERT_NE(v, nullptr);
    EXPECT_STREQ(__ry_json_type(v), "number");
    double out;
    EXPECT_EQ(__ry_json_float(v, &out), 0);
    EXPECT_NEAR(out, 1.5e10, 1e5);
    __ry_json_free(v);
}
