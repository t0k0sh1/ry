// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file dominant tag: [internal] — drives __ry_json_parse_to_any /
// __ry_json_release_any with hand-built RyAny.

#include "ry/runtime/native/json.hpp"
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
struct MsCleanupEnv : public ::testing::Environment {
    void TearDown() override {
        for (auto *h : g_ms_handles) freeStringSlot(const_cast<char *>(h));
        g_ms_handles.clear();
    }
};
// NOLINTNEXTLINE
static const ::testing::Environment *const kMsCleanup =
    ::testing::AddGlobalTestEnvironment(new MsCleanupEnv);

// Releases the ARC payload backing a RyAny via the exported runtime helper,
// matching what emitAnyReleaseVar dispatches on the codegen side. This walks
// nested list/map elements so list/map smoke tests stay ASan/LSan clean.
static void releaseAny(RyAny &v) { __ry_json_release_any(&v); }

// ===== [internal] Primitives =====

TEST(JsonParseToAny, ParsesNullToUnitTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("null"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Unit));
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesIntegerToIntTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("42"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Int));
    int64_t value = 0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_EQ(value, 42);
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesFloatToFloatTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("3.5"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Float));
    double value = 0.0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_DOUBLE_EQ(value, 3.5);
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesBoolTrueToBoolTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("true"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Bool));
    int64_t value = 0;
    std::memcpy(&value, out.data, sizeof(value));
    EXPECT_EQ(value, 1);
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesStringToStrTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("\"hello\""), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Str));
    const char *handle = nullptr;
    std::memcpy(&handle, out.data, sizeof(handle));
    ASSERT_NE(handle, nullptr);
    EXPECT_EQ(stringByteLen(handle), 5);
    EXPECT_EQ(std::memcmp(handle, "hello", 5), 0);
    releaseAny(out);
}

// ===== [internal] Arrays =====

TEST(JsonParseToAny, ParsesArrayToListTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("[1, 2, 3]"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::List));
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    ASSERT_NE(header, nullptr);
    auto *list = static_cast<ListHeader *>(header);
    EXPECT_EQ(list->len, 3);
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesEmptyArrayToListTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("[]"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::List));
    releaseAny(out);
}

// ===== [internal] Objects =====

TEST(JsonParseToAny, ParsesObjectToMapTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("{\"a\": 1, \"b\": 2}"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Map));
    void *header = nullptr;
    std::memcpy(&header, out.data, sizeof(header));
    ASSERT_NE(header, nullptr);
    auto *map = static_cast<MapHeader *>(header);
    EXPECT_EQ(map->len, 2);
    releaseAny(out);
}

TEST(JsonParseToAny, ParsesEmptyObjectToMapTag) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("{}"), &out);
    ASSERT_EQ(status, 0);
    EXPECT_EQ(out.tag, static_cast<int64_t>(RyAnyTag::Map));
    releaseAny(out);
}

// ===== [internal] Errors =====

TEST(JsonParseToAny, RejectsInvalidInput) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("{invalid}"), &out);
    EXPECT_NE(status, 0);
}

TEST(JsonParseToAny, RejectsTrailingContent) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms("123 456"), &out);
    EXPECT_NE(status, 0);
}

TEST(JsonParseToAny, RejectsEmptyInput) {
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms(""), &out);
    EXPECT_NE(status, 0);
}

// ===== [internal] Depth limit =====

TEST(JsonParseToAny, AcceptsMaxNestingDepth) {
    std::string deep;
    for (int i = 0; i < 256; i++) deep += "[";
    for (int i = 0; i < 256; i++) deep += "]";
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms(deep.c_str()), &out);
    EXPECT_EQ(status, 0);
    if (status == 0) releaseAny(out);
}

TEST(JsonParseToAny, RejectsTooDeepNesting) {
    std::string deep;
    for (int i = 0; i < 257; i++) deep += "[";
    for (int i = 0; i < 257; i++) deep += "]";
    RyAny out{};
    int64_t status = __ry_json_parse_to_any(ms(deep.c_str()), &out);
    EXPECT_NE(status, 0);
}

// ===== [internal] Stringify primitives =====

TEST(JsonStringifyAny, EncodesUnitAsNull) {
    RyAny v = anyFromUnit();
    const char *s = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "null");
    freeStringSlot(const_cast<char *>(s));
}

TEST(JsonStringifyAny, EncodesIntegerAsDigits) {
    RyAny v = anyFromInt(7);
    const char *s = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "7");
    freeStringSlot(const_cast<char *>(s));
}

TEST(JsonStringifyAny, EncodesBoolAsKeyword) {
    RyAny v = anyFromBool(1);
    const char *s = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "true");
    freeStringSlot(const_cast<char *>(s));
}

TEST(JsonStringifyAny, EncodesStringWithQuotes) {
    const char *handle = ms("hi");
    RyAny v = anyFromStr(handle);
    const char *s = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "\"hi\"");
    freeStringSlot(const_cast<char *>(s));
}

TEST(JsonStringifyAny, EncodesNulInsideString) {
    const char raw[] = {'a', '\0', 'b'};
    const char *handle = makeString(raw, sizeof(raw));
    g_ms_handles.push_back(handle);
    RyAny v = anyFromStr(handle);
    const char *s = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "\"a\\u0000b\"");
    freeStringSlot(const_cast<char *>(s));
}

// ===== [internal] Unified stringify ABI (#1890) =====
//
// __ry_json_stringify_any_ex / _safe_ex are the codegen-facing entry
// points after #1890. The four name-encoded variants (`_sorted`, `_safe`,
// `_sorted_safe`) remain for direct C++ callers but are no longer reached
// from generated IR. The cross-validation tests below confirm the unified
// symbols produce identical output to their name-encoded predecessors.

extern "C" {
const char *__ry_json_stringify_any_sorted(const RyAny *v, int64_t indent);
const char *__ry_json_stringify_any_safe(const RyAny *v, int64_t indent);
const char *__ry_json_stringify_any_sorted_safe(const RyAny *v, int64_t indent);
}

TEST(JsonStringifyAnyEx, SortKeys0MatchesBaseSymbol) {
    RyAny v = anyFromInt(7);
    const char *a = __ry_json_stringify_any_ex(&v, -1, 0);
    const char *b = __ry_json_stringify_any(&v, -1);
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);
    EXPECT_STREQ(a, b);
    EXPECT_STREQ(a, "7");
    freeStringSlot(const_cast<char *>(a));
    freeStringSlot(const_cast<char *>(b));
}

TEST(JsonStringifyAnyEx, SortKeys0PreservesInsertionOrder) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"c\": 3, \"a\": 1, \"b\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *s = __ry_json_stringify_any_ex(&m, -1, 0);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "{\"c\":3,\"a\":1,\"b\":2}");
    freeStringSlot(const_cast<char *>(s));
    releaseAny(m);
}

TEST(JsonStringifyAnyEx, SortKeys1SortsMapKeys) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"c\": 3, \"a\": 1, \"b\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *s = __ry_json_stringify_any_ex(&m, -1, 1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "{\"a\":1,\"b\":2,\"c\":3}");
    freeStringSlot(const_cast<char *>(s));
    releaseAny(m);
}

TEST(JsonStringifyAnyEx, SortKeys1MatchesSortedSymbol) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"c\": 3, \"a\": 1, \"b\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *a = __ry_json_stringify_any_ex(&m, -1, 1);
    const char *b = __ry_json_stringify_any_sorted(&m, -1);
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);
    EXPECT_STREQ(a, b);
    freeStringSlot(const_cast<char *>(a));
    freeStringSlot(const_cast<char *>(b));
    releaseAny(m);
}

TEST(JsonStringifyAnyEx, SortKeys1WithIndentMatchesSortedSymbol) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"c\": 3, \"a\": 1}"), &m);
    ASSERT_EQ(status, 0);
    const char *a = __ry_json_stringify_any_ex(&m, 2, 1);
    const char *b = __ry_json_stringify_any_sorted(&m, 2);
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);
    EXPECT_STREQ(a, b);
    freeStringSlot(const_cast<char *>(a));
    freeStringSlot(const_cast<char *>(b));
    releaseAny(m);
}

TEST(JsonStringifyAnySafeEx, SortKeys0OkMatchesSafeSymbol) {
    RyAny v = anyFromInt(42);
    const char *a = __ry_json_stringify_any_safe_ex(&v, -1, 0);
    const char *b = __ry_json_stringify_any_safe(&v, -1);
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);
    EXPECT_STREQ(a, b);
    EXPECT_STREQ(a, "42");
    freeStringSlot(const_cast<char *>(a));
    freeStringSlot(const_cast<char *>(b));
}

TEST(JsonStringifyAnySafeEx, SortKeys1OkSortsKeys) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"z\": 1, \"a\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *s = __ry_json_stringify_any_safe_ex(&m, -1, 1);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "{\"a\":2,\"z\":1}");
    freeStringSlot(const_cast<char *>(s));
    releaseAny(m);
}

TEST(JsonStringifyAnySafeEx, ErrOnNaNRegardlessOfSortKeys) {
    RyAny v = anyFromFloat(std::nan(""));
    EXPECT_EQ(__ry_json_stringify_any_safe_ex(&v, -1, 0), nullptr);
    EXPECT_EQ(__ry_json_stringify_any_safe_ex(&v, -1, 1), nullptr);
}

TEST(JsonStringifyAnySafeEx, SortKeys1MatchesSortedSafeSymbol) {
    RyAny m{};
    int64_t status = __ry_json_parse_to_any(
        ms("{\"c\": 3, \"a\": 1, \"b\": 2}"), &m);
    ASSERT_EQ(status, 0);
    const char *a = __ry_json_stringify_any_safe_ex(&m, -1, 1);
    const char *b = __ry_json_stringify_any_sorted_safe(&m, -1);
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);
    EXPECT_STREQ(a, b);
    freeStringSlot(const_cast<char *>(a));
    freeStringSlot(const_cast<char *>(b));
    releaseAny(m);
}
