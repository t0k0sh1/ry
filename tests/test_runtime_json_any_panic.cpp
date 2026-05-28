#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/any.hpp"
#include "ry/runtime/core/any_typed_coll.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/native/http/http_types.hpp"
#include "ry/runtime/native/json.hpp"
#include "ry/runtime/core/list.hpp"
#include "ry/runtime/core/string.hpp"

#include <gtest/gtest.h>

#include <cstdint>
#include <cstdlib>
#include <cstring>


using namespace ry;

// Regression tests for #1811: `json.stringify(any)` on a typed (non-`any`)
// collection no longer reads OOB. The side-table records the source-level
// type name on `wrapInAny` and stringify checks it before walking the inner
// buffer. Element-stride mismatch (e.g. `List<int>` walked as `RyAny[]`)
// would have been UB; it is now a deterministic `exit(1)` with the
// recorded type name embedded in the message.
class JsonAnyTypedCollPanicTest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        GTEST_FLAG_SET(death_test_style, "threadsafe");
    }
};

// =========================================================================
// Rejection branch: typed-non-any collection wrapped in any → panic
// =========================================================================

TEST_F(JsonAnyTypedCollPanicTest, StringifyListIntPanics) {
    EXPECT_EXIT({
        // Build a List<int> header by hand (data stride = 8B, not 16B).
        auto *hdr = static_cast<ListHeader *>(arc_alloc(sizeof(ListHeader)));
        hdr->len = 3;
        hdr->cap = 3;
        auto *data = static_cast<int64_t *>(
            checked_array_malloc(3, sizeof(int64_t)));
        data[0] = 1; data[1] = 2; data[2] = 3;
        hdr->data = reinterpret_cast<char **>(data);

        // Codegen-side `wrapInAny` would have done this for a typed-non-any
        // List input — we replicate it directly here.
        __ry_any_register_typed_coll(hdr, "List<int>");

        RyAny v{};
        v.tag = static_cast<int64_t>(RyAnyTag::List);
        std::memcpy(v.data, &hdr, sizeof(hdr));

        // Should fprintf + exit(1) before touching the inner buffer.
        (void)__ry_json_stringify_any(&v, -1);
        std::exit(0); // unreachable
    }, ::testing::ExitedWithCode(1),
       "any holds typed collection 'List<int>'");
}

TEST_F(JsonAnyTypedCollPanicTest, StringifyMapStrIntPanics) {
    EXPECT_EXIT({
        // Build a Map<str, int> header by hand (value stride = 8B, not 16B).
        auto *hdr = static_cast<MapHeader *>(arc_alloc(sizeof(MapHeader)));
        hdr->len = 1;
        hdr->cap = 1;
        auto **keys = static_cast<char **>(
            checked_array_malloc(1, sizeof(char *)));
        keys[0] = makeString("k", 1);
        hdr->keys = keys;
        auto *vals = static_cast<int64_t *>(
            checked_array_malloc(1, sizeof(int64_t)));
        vals[0] = 42;
        hdr->vals = reinterpret_cast<char **>(vals);
        hdr->bucket_count = 0;
        hdr->buckets = nullptr;

        __ry_any_register_typed_coll(hdr, "Map<str, int>");

        RyAny v{};
        v.tag = static_cast<int64_t>(RyAnyTag::Map);
        std::memcpy(v.data, &hdr, sizeof(hdr));

        (void)__ry_json_stringify_any(&v, -1);
        std::exit(0); // unreachable
    }, ::testing::ExitedWithCode(1),
       "any holds typed collection 'Map<str, int>'");
}

// =========================================================================
// Positive siblings: List<any> / Map<str, any> still stringify normally.
// (No side-table entry — the wrap arm's gate `meta->*_elem != anyTy_`
//  excludes these.) Required by `.claude/rules/tests-rejection-tdd.md`:
// "New rejections that narrow a form need a positive test for the
//  preserved sibling".
// =========================================================================

TEST_F(JsonAnyTypedCollPanicTest, StringifyListAnyIsOk) {
    // List<any> = [1, "x", true]
    auto *hdr = static_cast<ListHeader *>(arc_alloc(sizeof(ListHeader)));
    hdr->len = 3;
    hdr->cap = 3;
    auto *items = static_cast<RyAny *>(
        checked_array_malloc(3, sizeof(RyAny)));
    {
        int64_t n = 1;
        items[0].tag = static_cast<int64_t>(RyAnyTag::Int);
        std::memcpy(items[0].data, &n, sizeof(n));
    }
    {
        const char *h = makeString("x", 1);
        items[1].tag = static_cast<int64_t>(RyAnyTag::Str);
        std::memcpy(items[1].data, &h, sizeof(h));
    }
    {
        int64_t b = 1;
        items[2].tag = static_cast<int64_t>(RyAnyTag::Bool);
        std::memcpy(items[2].data, &b, sizeof(b));
    }
    hdr->data = reinterpret_cast<char **>(items);

    RyAny v{};
    v.tag = static_cast<int64_t>(RyAnyTag::List);
    std::memcpy(v.data, &hdr, sizeof(hdr));

    const char *json = __ry_json_stringify_any(&v, -1);
    EXPECT_STREQ(json, "[1,\"x\",true]");

    freeStringSlot(const_cast<char *>(json));
    {
        char *h;
        std::memcpy(&h, items[1].data, sizeof(h));
        freeStringSlot(h);
    }
    std::free(items);
    arc_free(hdr);
}

TEST_F(JsonAnyTypedCollPanicTest, StringifyMapStrAnyIsOk) {
    // Map<str, any> = {"k": 42}
    auto *hdr = static_cast<MapHeader *>(arc_alloc(sizeof(MapHeader)));
    hdr->len = 1;
    hdr->cap = 1;
    auto **keys = static_cast<char **>(
        checked_array_malloc(1, sizeof(char *)));
    keys[0] = makeString("k", 1);
    hdr->keys = keys;
    auto *vals = static_cast<RyAny *>(
        checked_array_malloc(1, sizeof(RyAny)));
    {
        int64_t n = 42;
        vals[0].tag = static_cast<int64_t>(RyAnyTag::Int);
        std::memcpy(vals[0].data, &n, sizeof(n));
    }
    hdr->vals = reinterpret_cast<char **>(vals);
    hdr->bucket_count = 0;
    hdr->buckets = nullptr;

    RyAny v{};
    v.tag = static_cast<int64_t>(RyAnyTag::Map);
    std::memcpy(v.data, &hdr, sizeof(hdr));

    const char *json = __ry_json_stringify_any(&v, -1);
    EXPECT_STREQ(json, "{\"k\":42}");

    freeStringSlot(const_cast<char *>(json));
    freeStringSlot(keys[0]);
    std::free(keys);
    std::free(vals);
    arc_free(hdr);
}
