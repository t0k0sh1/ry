#include "ry/runtime_json.hpp"
#include "ry/runtime_any.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_error.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"
#include "ry/runtime_http_types.hpp"
#include "ry/ry_layout.hpp"
#include <gtest/gtest.h>
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

// ---- Primitives ----

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

// ---- Arrays ----

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

// ---- Objects ----

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

// ---- Errors ----

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

// ---- Depth limit ----

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

// ---- Stringify primitives ----

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
