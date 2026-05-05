#include "ry/runtime_json.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_string.hpp"
#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <vector>


using namespace ry;
// RAII helper to prevent JSON handle leaks when ASSERT_* aborts a test
struct JsonDeleter {
    void operator()(void *p) const { if (p) __ry_json_free(p); }
};
using JsonPtr = std::unique_ptr<void, JsonDeleter>;

// Helper: retrieve type string, assert it equals expected, then free it.
static void expectJsonType(void *v, const char *expected) {
    const char *t = __ry_json_type(v);
    EXPECT_STREQ(t, expected);
    freeStringSlot(const_cast<char *>(t));
}

// Helper: wrap a C string literal in a StringHeader handle.
// __ry_json_parse / __ry_json_get expect StringHeader handles (same as Ry
// codegen passes them); plain C string literals must be wrapped.
// Handles are tracked in g_ms_handles and freed at test-suite teardown so
// LSan (integrated into ASan on Linux) does not report them as leaks.
static std::vector<const char *> g_ms_handles; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
static const char *ms(const char *literal) {
    const char *h = makeString(literal, strlen(literal));
    g_ms_handles.push_back(h);
    return h;
}
struct MsCleanupEnv : public ::testing::Environment {
    void TearDown() override {
        for (auto *h : g_ms_handles) freeStringSlot(const_cast<char *>(h));
        g_ms_handles.clear();
    }
};
// Registers MsCleanupEnv before RUN_ALL_TESTS() via static initialisation.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
static const ::testing::Environment *const kMsCleanup =
    ::testing::AddGlobalTestEnvironment(new MsCleanupEnv);

// ===== Merged parse type tests =====

TEST(RuntimeJson, ParseTypes) {
    // Object
    {
        void *v = __ry_json_parse(ms("{\"name\": \"Alice\", \"age\": 30}"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "object");
        EXPECT_EQ(__ry_json_len(v), 2);
        __ry_json_free(v);
    }

    // Array
    {
        void *v = __ry_json_parse(ms("[1, 2, 3]"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "array");
        EXPECT_EQ(__ry_json_len(v), 3);
        __ry_json_free(v);
    }

    // String
    {
        void *v = __ry_json_parse(ms("\"hello world\""));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "string");
        const char *s = __ry_json_str(v);
        ASSERT_NE(s, nullptr);
        EXPECT_STREQ(s, "hello world");
        freeStringSlot(const_cast<char *>(s));
        __ry_json_free(v);
    }

    // Number (int)
    {
        void *v = __ry_json_parse(ms("42"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "number");
        int64_t out;
        EXPECT_EQ(__ry_json_int(v, &out), 0);
        EXPECT_EQ(out, 42);
        __ry_json_free(v);
    }

    // Number (float)
    {
        void *v = __ry_json_parse(ms("3.14"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "number");
        double out;
        EXPECT_EQ(__ry_json_float(v, &out), 0);
        EXPECT_NEAR(out, 3.14, 0.001);
        __ry_json_free(v);
    }

    // Bool true
    {
        void *v = __ry_json_parse(ms("true"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "boolean");
        int64_t out;
        EXPECT_EQ(__ry_json_bool(v, &out), 0);
        EXPECT_EQ(out, 1);
        __ry_json_free(v);
    }

    // Bool false
    {
        void *v = __ry_json_parse(ms("false"));
        ASSERT_NE(v, nullptr);
        int64_t out;
        EXPECT_EQ(__ry_json_bool(v, &out), 0);
        EXPECT_EQ(out, 0);
        __ry_json_free(v);
    }

    // Null
    {
        void *v = __ry_json_parse(ms("null"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "null");
        __ry_json_free(v);
    }
}

// ===== Merged parse error tests =====

TEST(RuntimeJson, ParseErrors) {
    // Invalid
    {
        void *v = __ry_json_parse(ms("{invalid}"));
        EXPECT_EQ(v, nullptr);
    }

    // Trailing content
    {
        void *v = __ry_json_parse(ms("123 456"));
        EXPECT_EQ(v, nullptr);
    }

    // Empty
    {
        void *v = __ry_json_parse(ms(""));
        EXPECT_EQ(v, nullptr);
    }
}

// Regression for #1593: deeply nested JSON must produce an Err message,
// not a SIGABRT from C-stack exhaustion.
TEST(RuntimeJson, ParseDepthLimit) {
    constexpr int kMaxDepth = 256;

    auto buildArrayNest = [](int depth) {
        return std::string(static_cast<size_t>(depth), '[') +
               std::string(static_cast<size_t>(depth), ']');
    };
    auto buildObjectNest = [](int depth) {
        std::string s;
        for (int i = 0; i < depth; ++i) s += "{\"a\":";
        s += "1";
        s += std::string(static_cast<size_t>(depth), '}');
        return s;
    };

    // Boundary: depth 256 (== MAX_NESTING_DEPTH) must succeed.
    {
        std::string ok = buildArrayNest(kMaxDepth);
        void *v = __ry_json_parse(ms(ok.c_str()));
        ASSERT_NE(v, nullptr);
        __ry_json_free(v);
    }

    // depth 257 array → nullptr + error message.
    {
        std::string deep = buildArrayNest(kMaxDepth + 1);
        void *v = __ry_json_parse(ms(deep.c_str()));
        EXPECT_EQ(v, nullptr);
        const char *err = __ry_get_last_error();
        ASSERT_NE(err, nullptr);
        EXPECT_NE(std::string(err).find("maximum nesting depth exceeded"), std::string::npos)
            << "expected depth-limit error but got: " << err;
        freeStringSlot(const_cast<char *>(err));
    }

    // depth 257 object → nullptr + error message.
    {
        std::string deep = buildObjectNest(kMaxDepth + 1);
        void *v = __ry_json_parse(ms(deep.c_str()));
        EXPECT_EQ(v, nullptr);
        const char *err = __ry_get_last_error();
        ASSERT_NE(err, nullptr);
        EXPECT_NE(std::string(err).find("maximum nesting depth exceeded"), std::string::npos)
            << "expected depth-limit error but got: " << err;
        freeStringSlot(const_cast<char *>(err));
    }

    // Mixed array+object nesting where the combined depth exceeds the limit.
    // 129 alternating "[{\"a\":" pairs contribute depth 258 > 256.
    {
        constexpr int kPairs = 129;
        std::string deep;
        for (int i = 0; i < kPairs; ++i) deep += "[{\"a\":";
        deep += "1";
        for (int i = 0; i < kPairs; ++i) deep += "}]";
        void *v = __ry_json_parse(ms(deep.c_str()));
        EXPECT_EQ(v, nullptr);
        const char *err = __ry_get_last_error();
        ASSERT_NE(err, nullptr);
        EXPECT_NE(std::string(err).find("maximum nesting depth exceeded"), std::string::npos)
            << "expected depth-limit error but got: " << err;
        freeStringSlot(const_cast<char *>(err));
    }

    // Issue reproducer: depth 90000 — must not crash, must return nullptr.
    {
        std::string deep = buildArrayNest(90000);
        void *v = __ry_json_parse(ms(deep.c_str()));
        EXPECT_EQ(v, nullptr);
    }
}

// ===== Merged access tests =====

TEST(RuntimeJson, AccessTests) {
    // GetObjectField
    {
        JsonPtr v(__ry_json_parse(ms("{\"name\": \"Alice\"}")));
        ASSERT_NE(v.get(), nullptr);
        void *field = __ry_json_get(v.get(), ms("name"));
        ASSERT_NE(field, nullptr);
        const char *s = __ry_json_str(field);
        ASSERT_NE(s, nullptr);
        EXPECT_STREQ(s, "Alice");
        freeStringSlot(const_cast<char *>(s));
    }

    // GetMissingKey
    {
        JsonPtr v(__ry_json_parse(ms("{\"a\": 1}")));
        ASSERT_NE(v.get(), nullptr);
        void *field = __ry_json_get(v.get(), ms("missing"));
        EXPECT_EQ(field, nullptr);
    }

    // AtArrayIndex
    {
        JsonPtr v(__ry_json_parse(ms("[10, 20, 30]")));
        ASSERT_NE(v.get(), nullptr);
        void *elem = __ry_json_at(v.get(), 1);
        ASSERT_NE(elem, nullptr);
        int64_t out;
        EXPECT_EQ(__ry_json_int(elem, &out), 0);
        EXPECT_EQ(out, 20);
    }

    // AtOutOfBounds
    {
        JsonPtr v(__ry_json_parse(ms("[1, 2]")));
        ASSERT_NE(v.get(), nullptr);
        void *elem = __ry_json_at(v.get(), 5);
        EXPECT_EQ(elem, nullptr);
    }

    // TypeMismatchInt
    {
        JsonPtr v(__ry_json_parse(ms("\"hello\"")));
        ASSERT_NE(v.get(), nullptr);
        int64_t out;
        EXPECT_NE(__ry_json_int(v.get(), &out), 0);
    }

    // TypeMismatchBool
    {
        JsonPtr v(__ry_json_parse(ms("42")));
        ASSERT_NE(v.get(), nullptr);
        int64_t out;
        EXPECT_NE(__ry_json_bool(v.get(), &out), 0);
    }
}

// ===== Nested access =====

TEST(RuntimeJson, NestedObject) {
    void *v = __ry_json_parse(ms("{\"user\": {\"name\": \"Bob\"}}"));
    ASSERT_NE(v, nullptr);
    void *user = __ry_json_get(v, ms("user"));
    ASSERT_NE(user, nullptr);
    void *name = __ry_json_get(user, ms("name"));
    ASSERT_NE(name, nullptr);
    const char *s = __ry_json_str(name);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "Bob");
    freeStringSlot(const_cast<char *>(s));
    __ry_json_free(v);
}

// ===== Keys =====

TEST(RuntimeJson, ObjectKeys) {
    JsonPtr v(__ry_json_parse(ms("{\"a\": 1, \"b\": 2}")));
    ASSERT_NE(v.get(), nullptr);
    void *keys = __ry_json_keys(v.get());
    ASSERT_NE(keys, nullptr);
    auto *lh = (ListHeader*)keys;
    EXPECT_EQ(lh->len, 2);
    const char **data = (const char**)lh->data;
    EXPECT_STREQ(data[0], "a");
    EXPECT_STREQ(data[1], "b");
    for (int64_t i = 0; i < lh->len; i++) freeStringSlot(const_cast<char *>(data[i]));
    free(data);
    arc_free(keys); // ListHeader is arc_alloc'd by makeStringList
}

TEST(RuntimeJson, ObjectKeysNullInput) {
    void *keys = __ry_json_keys(nullptr);
    EXPECT_EQ(keys, nullptr);
    const char *err = __ry_get_last_error();
    EXPECT_STREQ(err, "json_keys: value is null");
    freeStringSlot(const_cast<char *>(err));
}

TEST(RuntimeJson, ObjectKeysNonObject) {
    JsonPtr v(__ry_json_parse(ms("[1, 2, 3]")));
    ASSERT_NE(v.get(), nullptr);
    void *keys = __ry_json_keys(v.get());
    EXPECT_EQ(keys, nullptr);
    const char *err = __ry_get_last_error();
    EXPECT_STREQ(err, "json_keys: value is not an object");
    freeStringSlot(const_cast<char *>(err));
}

TEST(RuntimeJson, ObjectKeysEmptyObject) {
    JsonPtr v(__ry_json_parse(ms("{}")));
    ASSERT_NE(v.get(), nullptr);
    void *keys = __ry_json_keys(v.get());
    ASSERT_NE(keys, nullptr);
    auto *lh = (ListHeader*)keys;
    EXPECT_EQ(lh->len, 0);
    free(lh->data);
    arc_free(keys); // ListHeader is arc_alloc'd by makeStringList
}

// ===== Merged stringify tests =====

TEST(RuntimeJson, StringifyTests) {
    // Compact
    {
        void *v = __ry_json_parse(ms("{\"a\":1,\"b\":2}"));
        ASSERT_NE(v, nullptr);
        const char *s = __ry_json_stringify(v);
        ASSERT_NE(s, nullptr);
        // Round-trip: stringify output is a StringHeader handle, so parse works.
        void *v2 = __ry_json_parse(s);
        ASSERT_NE(v2, nullptr);
        EXPECT_EQ(__ry_json_len(v2), 2);
        freeStringSlot(const_cast<char *>(s));
        __ry_json_free(v);
        __ry_json_free(v2);
    }

    // Pretty
    {
        void *v = __ry_json_parse(ms("{\"a\":1}"));
        ASSERT_NE(v, nullptr);
        const char *s = __ry_json_stringify_pretty(v, 2);
        ASSERT_NE(s, nullptr);
        EXPECT_NE(strstr(s, "\n"), nullptr);
        freeStringSlot(const_cast<char *>(s));
        __ry_json_free(v);
    }
}

// ===== String escapes =====

TEST(RuntimeJson, StringEscapes) {
    void *v = __ry_json_parse(ms("\"hello\\nworld\\t!\""));
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_str(v);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "hello\nworld\t!");
    freeStringSlot(const_cast<char *>(s));
    __ry_json_free(v);
}

TEST(RuntimeJson, UnicodeEscape) {
    void *v = __ry_json_parse(ms("\"\\u0041\""));
    ASSERT_NE(v, nullptr);
    const char *s = __ry_json_str(v);
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "A");
    freeStringSlot(const_cast<char *>(s));
    __ry_json_free(v);
}

// ===== NUL round-trip (C++ level) =====

TEST(RuntimeJson, NulRoundTrip) {
    // parse \u0000 in a string value
    {
        void *v = __ry_json_parse(ms("\"a\\u0000b\""));
        ASSERT_NE(v, nullptr);
        const char *s = __ry_json_str(v);
        ASSERT_NE(s, nullptr);
        int64_t len = stringByteLen(s);
        EXPECT_EQ(len, 3);
        EXPECT_EQ(s[0], 'a');
        EXPECT_EQ(s[1], '\0');
        EXPECT_EQ(s[2], 'b');
        freeStringSlot(const_cast<char *>(s));
        const char *js = __ry_json_stringify(v);
        ASSERT_NE(js, nullptr);
        EXPECT_STREQ(js, "\"a\\u0000b\"");
        freeStringSlot(const_cast<char *>(js));
        __ry_json_free(v);
    }

    // object key containing NUL
    {
        void *v = __ry_json_parse(ms("{\"k\\u0000x\":42}"));
        ASSERT_NE(v, nullptr);
        void *keys = __ry_json_keys(v);
        ASSERT_NE(keys, nullptr);
        auto *lh = (ListHeader*)keys;
        EXPECT_EQ(lh->len, 1);
        const char *k0 = ((const char **)lh->data)[0];
        EXPECT_EQ(stringByteLen(k0), 3);
        EXPECT_EQ(k0[0], 'k');
        EXPECT_EQ(k0[1], '\0');
        EXPECT_EQ(k0[2], 'x');
        // get() via NUL-containing key
        void *val = __ry_json_get(v, k0);
        ASSERT_NE(val, nullptr);
        int64_t out;
        EXPECT_EQ(__ry_json_int(val, &out), 0);
        EXPECT_EQ(out, 42);
        for (int64_t i = 0; i < lh->len; i++)
            freeStringSlot(const_cast<char *>(((const char **)lh->data)[i]));
        free(lh->data);
        arc_free(keys);
        __ry_json_free(v);
    }

    // stringify round-trip for NUL key
    {
        void *v = __ry_json_parse(ms("{\"k\\u0000x\":1}"));
        ASSERT_NE(v, nullptr);
        const char *js = __ry_json_stringify(v);
        ASSERT_NE(js, nullptr);
        EXPECT_STREQ(js, "{\"k\\u0000x\":1}");
        freeStringSlot(const_cast<char *>(js));
        __ry_json_free(v);
    }
}

// ===== Merged number conversion tests =====

TEST(RuntimeJson, NumberConversions) {
    // Float to int
    {
        void *v = __ry_json_parse(ms("42.0"));
        ASSERT_NE(v, nullptr);
        int64_t out;
        EXPECT_EQ(__ry_json_int(v, &out), 0);
        EXPECT_EQ(out, 42);
        __ry_json_free(v);
    }

    // Int to float
    {
        void *v = __ry_json_parse(ms("42"));
        ASSERT_NE(v, nullptr);
        double out;
        EXPECT_EQ(__ry_json_float(v, &out), 0);
        EXPECT_NEAR(out, 42.0, 0.001);
        __ry_json_free(v);
    }

    // Negative int
    {
        void *v = __ry_json_parse(ms("-42"));
        ASSERT_NE(v, nullptr);
        int64_t out;
        EXPECT_EQ(__ry_json_int(v, &out), 0);
        EXPECT_EQ(out, -42);
        __ry_json_free(v);
    }

    // Negative float
    {
        void *v = __ry_json_parse(ms("-3.14"));
        ASSERT_NE(v, nullptr);
        double out;
        EXPECT_EQ(__ry_json_float(v, &out), 0);
        EXPECT_NEAR(out, -3.14, 0.001);
        __ry_json_free(v);
    }

    // Scientific notation
    {
        void *v = __ry_json_parse(ms("1.5e10"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "number");
        double out;
        EXPECT_EQ(__ry_json_float(v, &out), 0);
        EXPECT_NEAR(out, 1.5e10, 1e5);
        __ry_json_free(v);
    }
}

// ===== Merged empty container tests =====

TEST(RuntimeJson, EmptyContainers) {
    // Empty object
    {
        void *v = __ry_json_parse(ms("{}"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "object");
        EXPECT_EQ(__ry_json_len(v), 0);
        __ry_json_free(v);
    }

    // Empty array
    {
        void *v = __ry_json_parse(ms("[]"));
        ASSERT_NE(v, nullptr);
        expectJsonType(v, "array");
        EXPECT_EQ(__ry_json_len(v), 0);
        __ry_json_free(v);
    }
}

// ===== NUL key disambiguation =====
// Verifies that memcmp-based key lookup distinguishes keys that share a
// common prefix up to NUL but differ in the byte that follows.

TEST(RuntimeJson, NulKeyDisambiguation) {
    // Object: {"k\u0000a": 1, "k\u0000b": 2}
    void *v = __ry_json_parse(ms("{\"k\\u0000a\":1,\"k\\u0000b\":2}"));
    ASSERT_NE(v, nullptr);
    EXPECT_EQ(__ry_json_len(v), 2);

    // Build StringHeader keys with explicit lengths (ms() uses strlen, which would
    // truncate at the embedded NUL byte, so we call makeString directly here).
    // Use octal escapes to avoid \xNNX ambiguity: \x00a = 0x0a (newline), not NUL+'a'.
    const char raw_a[] = {'k', '\0', 'a'};
    const char *key_a = makeString(raw_a, 3);
    g_ms_handles.push_back(key_a);
    const char raw_b[] = {'k', '\0', 'b'};
    const char *key_b = makeString(raw_b, 3);
    g_ms_handles.push_back(key_b);

    void *val_a = __ry_json_get(v, key_a);
    ASSERT_NE(val_a, nullptr);
    int64_t n_a = 0;
    EXPECT_EQ(__ry_json_int(val_a, &n_a), 0);
    EXPECT_EQ(n_a, 1);

    void *val_b = __ry_json_get(v, key_b);
    ASSERT_NE(val_b, nullptr);
    int64_t n_b = 0;
    EXPECT_EQ(__ry_json_int(val_b, &n_b), 0);
    EXPECT_EQ(n_b, 2);

    __ry_json_free(v);
}
