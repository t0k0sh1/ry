// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file tag: [regression #1811] mirror (json5) — guards typed-non-any
// collection OOB on json5.stringify(any) by driving runtime entry points
// directly.

#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/any.hpp"
#include "ry/runtime/core/any_typed_coll.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/native/http/http_types.hpp"
#include "ry/runtime/native/json5.hpp"
#include "ry/runtime/core/list.hpp"
#include "ry/runtime/core/string.hpp"

#include <gtest/gtest.h>

#include <cstdint>
#include <cstdlib>
#include <cstring>


using namespace ry;

// Regression tests mirroring tests/test_runtime_json_any_panic.cpp for json5.
// `json5.stringify(any)` on a typed (non-`any`) collection panics via the
// same side-table guard as `json.stringify`. JSON5 does not change this
// rejection branch — typed collections still have no JSON5 representation.
class Json5AnyTypedCollPanicTest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        GTEST_FLAG_SET(death_test_style, "threadsafe");
    }
};

TEST_F(Json5AnyTypedCollPanicTest, StringifyListIntPanics) {
    EXPECT_EXIT({
        auto *hdr = static_cast<ListHeader *>(arc_alloc(sizeof(ListHeader)));
        hdr->len = 3;
        hdr->cap = 3;
        auto *data = static_cast<int64_t *>(
            checked_array_malloc(3, sizeof(int64_t)));
        data[0] = 1; data[1] = 2; data[2] = 3;
        hdr->data = reinterpret_cast<char **>(data);

        __ry_any_register_typed_coll(hdr, "List<int>");

        RyAny v{};
        v.tag = static_cast<int64_t>(RyAnyTag::List);
        std::memcpy(v.data, &hdr, sizeof(hdr));

        (void)__ry_json5_stringify_any_ex(&v, -1, 0);
        std::exit(0);
    }, ::testing::ExitedWithCode(1),
       "any holds typed collection 'List<int>'");
}

TEST_F(Json5AnyTypedCollPanicTest, StringifyMapStrIntPanics) {
    EXPECT_EXIT({
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

        (void)__ry_json5_stringify_any_ex(&v, -1, 0);
        std::exit(0);
    }, ::testing::ExitedWithCode(1),
       "any holds typed collection 'Map<str, int>'");
}

// Positive sibling — List<any> / Map<str, any> still stringify normally.

TEST_F(Json5AnyTypedCollPanicTest, StringifyListAnyIsOk) {
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

    const char *json = __ry_json5_stringify_any_ex(&v, -1, 0);
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
