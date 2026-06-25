// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file dominant tag: [internal] — drives __ry_json5_parse_to_any /
// __ry_json5_release_any with hand-built RyAny.

#include "ry/runtime/native/json5.hpp"
#include "ry/runtime/core/any.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/core/error.hpp"
#include "ry/runtime/core/list.hpp"
#include "ry/runtime/core/string.hpp"
#include "ry/runtime/native/http/http_types.hpp"
#include "ry/ry_layout.hpp"
#include <gtest/gtest.h>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>


using namespace ry;

// Tracks Ry string handles allocated by ms() so they can be released at
// suite teardown (LSan-clean on Linux).
static std::vector<const char *> g_ms_handles; // NOLINT
static const char *ms(const char *literal) {
    const char *h = makeString(literal, std::strlen(literal));
    g_ms_handles.push_back(h);
    return h;
}
struct MsCleanupEnvJson5 : public ::testing::Environment {
    void TearDown() override {
        for (auto *h : g_ms_handles) freeStringSlot(const_cast<char *>(h));
        g_ms_handles.clear();
    }
};
// NOLINTNEXTLINE
static const ::testing::Environment *const kMsCleanupJson5 =
    ::testing::AddGlobalTestEnvironment(new MsCleanupEnvJson5);

static void releaseAny(RyAny &v) { __ry_json5_release_any(&v); }

// ===== [internal] Strict-JSON parity: every json input still parses =====

TEST(Json5ParseToAny, ParsesNullToUnitTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("null"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Unit));
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesIntegerToIntTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("42"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Int));
    int64_t value = 0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_EQ(value, 42);
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesFloatToFloatTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("3.5"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Float));
    double value = 0.0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_DOUBLE_EQ(value, 3.5);
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesBoolTrueToBoolTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("true"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Bool));
    int64_t value = 0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_EQ(value, 1);
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesDoubleQuotedString) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("\"hello\""), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Str));
    const char *handle = nullptr;
    std::memcpy(&handle, out.data, sizeof(handle));
    ASSERT_NE(handle, nullptr);
    EXPECT_EQ(stringByteLen(handle), 5);
    EXPECT_EQ(std::memcmp(handle, "hello", 5), 0);
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesArrayToListTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("[1, 2, 3]"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::List));
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    ASSERT_NE(header, nullptr);
    auto *list = static_cast<ListHeader *>(header);
    EXPECT_EQ(list->len, 3);
    releaseAny(out);
}

TEST(Json5ParseToAny, ParsesObjectToMapTag) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("{\"a\": 1, \"b\": 2}"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Map));
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    ASSERT_NE(header, nullptr);
    auto *map = static_cast<MapHeader *>(header);
    EXPECT_EQ(map->len, 2);
    releaseAny(out);
}

// ===== [internal] Errors =====

TEST(Json5ParseToAny, RejectsEmptyInput) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms(""), &out);
    EXPECT_NE(status, 0);
}

TEST(Json5ParseToAny, RejectsTrailingContent) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("123 456"), &out);
    EXPECT_NE(status, 0);
}

// ===== [internal] JSON5-specific extensions =====

TEST(Json5ParseToAny, AcceptsLineComment) {
    RyAny out{};
    int64_t status =
        __ry_json5_parse_to_any(ms("// leading\n42 // trailing"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Int));
    int64_t v = 0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_EQ(v, 42);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsBlockComment) {
    RyAny out{};
    int64_t status =
        __ry_json5_parse_to_any(ms("/* prefix */ [1, /*mid*/ 2]"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::List));
    releaseAny(out);
}

TEST(Json5ParseToAny, RejectsUnterminatedBlockComment) {
    RyAny out{};
    int64_t status =
        __ry_json5_parse_to_any(ms("/* unterminated 42"), &out);
    EXPECT_NE(status, 0);
}

TEST(Json5ParseToAny, AcceptsTrailingCommaInArray) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("[1, 2, 3,]"), &out);
    ASSERT_EQ(status, 0);
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    auto *list = static_cast<ListHeader *>(header);
    EXPECT_EQ(list->len, 3);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsTrailingCommaInObject) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("{\"a\": 1,}"), &out);
    ASSERT_EQ(status, 0);
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    auto *map = static_cast<MapHeader *>(header);
    EXPECT_EQ(map->len, 1);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsSingleQuotedString) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("'hi'"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Str));
    const char *handle = nullptr;
    std::memcpy(&handle, out.data, sizeof(handle));
    EXPECT_EQ(stringByteLen(handle), 2);
    EXPECT_EQ(std::memcmp(handle, "hi", 2), 0);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsUnquotedObjectKey) {
    RyAny out{};
    int64_t status =
        __ry_json5_parse_to_any(ms("{x: 1, y: 2}"), &out);
    ASSERT_EQ(status, 0);
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    auto *map = static_cast<MapHeader *>(header);
    EXPECT_EQ(map->len, 2);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsHexLiteral) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("0xFF"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Int));
    int64_t v = 0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_EQ(v, 255);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsLeadingDecimalPoint) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms(".5"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Float));
    double v = 0.0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_DOUBLE_EQ(v, 0.5);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsTrailingDecimalPoint) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("5."), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Float));
    double v = 0.0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_DOUBLE_EQ(v, 5.0);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsInfinity) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("Infinity"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Float));
    double v = 0.0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_TRUE(std::isinf(v));
    EXPECT_GT(v, 0.0);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsNegativeInfinity) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("-Infinity"), &out);
    ASSERT_EQ(status, 0);
    double v = 0.0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_TRUE(std::isinf(v));
    EXPECT_LT(v, 0.0);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsNaN) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("NaN"), &out);
    ASSERT_EQ(status, 0);
    double v = 0.0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_TRUE(std::isnan(v));
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsPositiveSign) {
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(ms("+5"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Int));
    int64_t v = 0;
    std::memcpy(&v, out.data, sizeof(v));
    EXPECT_EQ(v, 5);
    releaseAny(out);
}

TEST(Json5ParseToAny, AcceptsLineContinuationInString) {
    RyAny out{};
    // 'foo\\\nbar' source bytes — backslash followed by LF inside single-quoted str
    int64_t status = __ry_json5_parse_to_any(ms("'foo\\\nbar'"), &out);
    ASSERT_EQ(status, 0);
    const char *handle = nullptr;
    std::memcpy(&handle, out.data, sizeof(handle));
    EXPECT_EQ(stringByteLen(handle), 6);
    EXPECT_EQ(std::memcmp(handle, "foobar", 6), 0);
    releaseAny(out);
}

// ===== [internal] Stringify primitives (JSON5 difference for non-finite floats) =====

TEST(Json5StringifyAnyEx, EncodesIntegerAsDigits) {
    RyAny v = anyFromInt(7);
    const char *s = __ry_json5_stringify_any_ex(&v, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "7");
    freeStringSlot(const_cast<char *>(s));
}

TEST(Json5StringifyAnyEx, EncodesNaNAsBareToken) {
    RyAny v = anyFromFloat(std::nan(""));
    const char *s = __ry_json5_stringify_any_ex(&v, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "NaN");
    freeStringSlot(const_cast<char *>(s));
}

TEST(Json5StringifyAnyEx, EncodesPositiveInfinityAsBareToken) {
    RyAny v = anyFromFloat(std::numeric_limits<double>::infinity());
    const char *s = __ry_json5_stringify_any_ex(&v, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "Infinity");
    freeStringSlot(const_cast<char *>(s));
}

TEST(Json5StringifyAnyEx, EncodesNegativeInfinityAsBareToken) {
    RyAny v = anyFromFloat(-std::numeric_limits<double>::infinity());
    const char *s = __ry_json5_stringify_any_ex(&v, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "-Infinity");
    freeStringSlot(const_cast<char *>(s));
}

TEST(Json5StringifyAnySafeEx, NonFiniteFloatIsOk) {
    RyAny nan = anyFromFloat(std::nan(""));
    const char *s = __ry_json5_stringify_any_safe_ex(&nan, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "NaN");
    freeStringSlot(const_cast<char *>(s));

    RyAny inf = anyFromFloat(std::numeric_limits<double>::infinity());
    const char *s2 = __ry_json5_stringify_any_safe_ex(&inf, -1, 0);
    ASSERT_NE(s2, nullptr);
    EXPECT_STREQ(s2, "Infinity");
    freeStringSlot(const_cast<char *>(s2));
}

TEST(Json5StringifyAnyEx, SortKeysSortsMapKeys) {
    RyAny m{};
    int64_t status = __ry_json5_parse_to_any(
        ms("{\"c\": 3, \"a\": 1, \"b\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *s = __ry_json5_stringify_any_ex(&m, -1, 1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "{\"a\":1,\"b\":2,\"c\":3}");
    freeStringSlot(const_cast<char *>(s));
    releaseAny(m);
}

// ===== [internal] Round-trip =====

TEST(Json5RoundTrip, NaNRoundTrips) {
    // load("NaN") → stringify → "NaN" → load → NaN
    RyAny parsed{};
    ASSERT_EQ(__ry_json5_parse_to_any(ms("NaN"), &parsed), 0);
    const char *s = __ry_json5_stringify_any_ex(&parsed, -1, 0);
    EXPECT_STREQ(s, "NaN");
    RyAny reparsed{};
    ASSERT_EQ(__ry_json5_parse_to_any(s, &reparsed), 0);
    double v = 0.0;
    std::memcpy(&v, reparsed.data, sizeof(v));
    EXPECT_TRUE(std::isnan(v));
    freeStringSlot(const_cast<char *>(s));
    releaseAny(parsed);
    releaseAny(reparsed);
}
