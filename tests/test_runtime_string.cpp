#include "ry/runtime_string.hpp"
#include "ry/runtime_list.hpp"
#include "ry/ry_layout.hpp"
#include <gtest/gtest.h>
#include <cstring>

extern "C" void *__ry_str_split(const char *s, int64_t sLen,
                                 const char *delim, int64_t delimLen);

using namespace ry;

// ===== Layout constants =====

TEST(StringHeader, LayoutConstants) {
    EXPECT_EQ(STRING_HEADER_EXTRA, 8u);
    EXPECT_EQ(STRING_HEADER_SIZE,  24u);
    EXPECT_EQ(STRING_BYTELEN_OFFSET, 8);
    // STRING_HEADER_SIZE = ARC_HEADER_SIZE (16) + byteLen field (8)
    EXPECT_EQ(STRING_HEADER_SIZE, ARC_HEADER_SIZE + 8u);
}

// ===== makeString =====

TEST(StringHeader, MakeStringEmpty) {
    char *s = makeString("", 0);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 0);
    EXPECT_EQ(s[0], '\0');
    freeStringSlot(s);
}

TEST(StringHeader, MakeStringBasic) {
    char *s = makeString("hello", 5);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 5);
    EXPECT_STREQ(s, "hello");
    EXPECT_EQ(s[5], '\0');
    freeStringSlot(s);
}

TEST(StringHeader, MakeStringWithEmbeddedNul) {
    const char src[] = {'a', '\0', 'b'};
    char *s = makeString(src, 3);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 3);
    EXPECT_EQ(s[0], 'a');
    EXPECT_EQ(s[1], '\0');
    EXPECT_EQ(s[2], 'b');
    EXPECT_EQ(s[3], '\0');  // null terminator
    freeStringSlot(s);
}

TEST(StringHeader, MakeStringNullSrcBecomesEmpty) {
    char *s = makeString(nullptr, 42);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 0);
    EXPECT_EQ(s[0], '\0');
    freeStringSlot(s);
}

TEST(StringHeader, MakeStringSingleNulByte) {
    const char src[] = {'\0'};
    char *s = makeString(src, 1);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 1);
    EXPECT_EQ(s[0], '\0');
    EXPECT_EQ(s[1], '\0');  // terminator
    freeStringSlot(s);
}

// ===== makeStringUninit =====

TEST(StringHeader, MakeStringUninitSetsLenAndTerminator) {
    char *s = makeStringUninit(7);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 7);
    EXPECT_EQ(s[7], '\0');
    freeStringSlot(s);
}

TEST(StringHeader, MakeStringUninitZeroLength) {
    char *s = makeStringUninit(0);
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(stringByteLen(s), 0);
    EXPECT_EQ(s[0], '\0');
    freeStringSlot(s);
}

// ===== stringByteLen =====

TEST(StringHeader, ByteLenReflectsActualLength) {
    char *s = makeString("xyz", 3);
    EXPECT_EQ(stringByteLen(s), 3);
    freeStringSlot(s);
}

TEST(StringHeader, ByteLenCountsEmbeddedNulBytes) {
    const char src[] = {'\0', '\0', '\0'};
    char *s = makeString(src, 3);
    EXPECT_EQ(stringByteLen(s), 3);  // not 0
    freeStringSlot(s);
}

TEST(StringHeader, ByteLenIsIndependentOfStrcmp) {
    // Two strings that differ only beyond the first NUL
    const char a_src[] = {'a', '\0', 'x'};
    const char b_src[] = {'a', '\0', 'y'};
    char *a = makeString(a_src, 3);
    char *b = makeString(b_src, 3);
    EXPECT_EQ(stringByteLen(a), 3);
    EXPECT_EQ(stringByteLen(b), 3);
    // strcmp sees them as equal (stops at '\0'), but they differ at byte 2
    EXPECT_EQ(strcmp(a, b), 0);  // both look like "a" to strcmp
    EXPECT_EQ(a[2], 'x');
    EXPECT_EQ(b[2], 'y');
    freeStringSlot(a);
    freeStringSlot(b);
}

// ===== stringHeaderPtr =====

TEST(StringHeader, HeaderPtrIsOffsetBeforeData) {
    char *s = makeString("test", 4);
    char *hdr = stringHeaderPtr(s);
    EXPECT_EQ(hdr, s - static_cast<ptrdiff_t>(STRING_HEADER_SIZE));
    // strong_count at offset 0, weak_count at offset 8, byteLen at offset 16
    auto *block = reinterpret_cast<int64_t *>(hdr);
    EXPECT_EQ(block[0], 1);   // strong_count = 1 (not immortal for dynamic alloc)
    EXPECT_EQ(block[1], 0);   // weak_count = 0
    EXPECT_EQ(block[2], 4);   // byteLen = 4
    freeStringSlot(s);
}

// ===== freeStringSlot =====

TEST(StringHeader, FreeStringSlotNullIsNoop) {
    // Should not crash
    freeStringSlot(nullptr);
}

// ===== NUL-safe operations =====

TEST(StringHeader, MemcmpCanDistinguishNulEmbeddedStrings) {
    const char a_src[] = {'a', '\0', 'b'};
    const char b_src[] = {'a', '\0', 'c'};
    char *a = makeString(a_src, 3);
    char *b = makeString(b_src, 3);
    EXPECT_EQ(stringByteLen(a), 3);
    EXPECT_EQ(stringByteLen(b), 3);
    int cmp = memcmp(a, b, static_cast<size_t>(stringByteLen(a)));
    EXPECT_LT(cmp, 0);  // 'b' < 'c'
    freeStringSlot(a);
    freeStringSlot(b);
}

TEST(StringHeader, ConcatViaMemcpy) {
    const char a_src[] = {'a', '\0'};
    const char b_src[] = {'b', '\0'};
    int64_t la = 2, lb = 2;
    char *buf = makeStringUninit(static_cast<size_t>(la + lb));
    memcpy(buf, a_src, static_cast<size_t>(la));
    memcpy(buf + la, b_src, static_cast<size_t>(lb));
    EXPECT_EQ(stringByteLen(buf), 4);
    EXPECT_EQ(buf[0], 'a');
    EXPECT_EQ(buf[1], '\0');
    EXPECT_EQ(buf[2], 'b');
    EXPECT_EQ(buf[3], '\0');
    EXPECT_EQ(buf[4], '\0');  // null terminator
    freeStringSlot(buf);
}

// ===== __ry_str_split =====

// Helper: cast __ry_str_split result to ListHeader*.
static ry::ListHeader *doSplit(const char *s, int64_t sLen,
                               const char *delim, int64_t delimLen) {
    return static_cast<ry::ListHeader *>(__ry_str_split(s, sLen, delim, delimLen));
}

// Helper: free a ListHeader produced by __ry_str_split.
static void freeSplitList(ry::ListHeader *h) {
    if (!h) return;
    for (int64_t i = 0; i < h->len; ++i) freeStringSlot(h->data[i]);
    free(h->data);                      // NOLINT(cppcoreguidelines-no-malloc)
    arc_free(h);
}

TEST(StrSplit, BasicAscii) {
    // "a,b,c" split by ","
    const char *s = "a,b,c";
    const char *d = ",";
    ry::ListHeader *h = doSplit(s, 5, d, 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 3);
    EXPECT_EQ(stringByteLen(h->data[0]), 1); EXPECT_EQ(h->data[0][0], 'a');
    EXPECT_EQ(stringByteLen(h->data[1]), 1); EXPECT_EQ(h->data[1][0], 'b');
    EXPECT_EQ(stringByteLen(h->data[2]), 1); EXPECT_EQ(h->data[2][0], 'c');
    freeSplitList(h);
}

TEST(StrSplit, EmbeddedNulInHaystack) {
    // "a\0b,c\0d" split by ","
    const char s[] = {'a', '\0', 'b', ',', 'c', '\0', 'd'};
    const char d[] = {','};
    ry::ListHeader *h = doSplit(s, 7, d, 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 2);
    // elem[0] = "a\0b" (3 bytes)
    EXPECT_EQ(stringByteLen(h->data[0]), 3);
    EXPECT_EQ(memcmp(h->data[0], "a\0b", 3), 0);
    // elem[1] = "c\0d" (3 bytes)
    EXPECT_EQ(stringByteLen(h->data[1]), 3);
    EXPECT_EQ(memcmp(h->data[1], "c\0d", 3), 0);
    freeSplitList(h);
}

TEST(StrSplit, NulInDelim) {
    // "x\0|y\0|z" split by "\0|"
    const char s[] = {'x', '\0', '|', 'y', '\0', '|', 'z'};
    const char d[] = {'\0', '|'};
    ry::ListHeader *h = doSplit(s, 7, d, 2);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 3);
    EXPECT_EQ(stringByteLen(h->data[0]), 1); EXPECT_EQ(h->data[0][0], 'x');
    EXPECT_EQ(stringByteLen(h->data[1]), 1); EXPECT_EQ(h->data[1][0], 'y');
    EXPECT_EQ(stringByteLen(h->data[2]), 1); EXPECT_EQ(h->data[2][0], 'z');
    freeSplitList(h);
}

TEST(StrSplit, SingleNulDelim) {
    // "a\0b\0c" split by "\0"
    const char s[] = {'a', '\0', 'b', '\0', 'c'};
    const char d[] = {'\0'};
    ry::ListHeader *h = doSplit(s, 5, d, 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 3);
    EXPECT_EQ(stringByteLen(h->data[0]), 1); EXPECT_EQ(h->data[0][0], 'a');
    EXPECT_EQ(stringByteLen(h->data[1]), 1); EXPECT_EQ(h->data[1][0], 'b');
    EXPECT_EQ(stringByteLen(h->data[2]), 1); EXPECT_EQ(h->data[2][0], 'c');
    freeSplitList(h);
}

TEST(StrSplit, DelimNotFound) {
    // "a\0b" split by "|" → ["a\0b"] (single element)
    const char s[] = {'a', '\0', 'b'};
    const char d[] = {'|'};
    ry::ListHeader *h = doSplit(s, 3, d, 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 1);
    EXPECT_EQ(stringByteLen(h->data[0]), 3);
    EXPECT_EQ(memcmp(h->data[0], "a\0b", 3), 0);
    freeSplitList(h);
}

TEST(StrSplit, EmptyHaystack) {
    ry::ListHeader *h = doSplit("", 0, ",", 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 1);
    EXPECT_EQ(stringByteLen(h->data[0]), 0);
    freeSplitList(h);
}

TEST(StrSplit, DelimLongerThanHaystack) {
    // delim longer than subject → no match → [subject]
    const char s[] = {'a', 'b'};
    const char d[] = {'a', 'b', 'c'};
    ry::ListHeader *h = doSplit(s, 2, d, 3);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 1);
    EXPECT_EQ(stringByteLen(h->data[0]), 2);
    EXPECT_EQ(memcmp(h->data[0], "ab", 2), 0);
    freeSplitList(h);
}

TEST(StrSplit, TrailingDelim) {
    // "a,b," split by "," → ["a", "b", ""]
    ry::ListHeader *h = doSplit("a,b,", 4, ",", 1);
    ASSERT_NE(h, nullptr);
    ASSERT_EQ(h->len, 3);
    EXPECT_EQ(stringByteLen(h->data[0]), 1);
    EXPECT_EQ(stringByteLen(h->data[1]), 1);
    EXPECT_EQ(stringByteLen(h->data[2]), 0);  // empty tail
    freeSplitList(h);
}
