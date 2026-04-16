#include "ry/runtime_string.hpp"
#include "ry/ry_layout.hpp"
#include <gtest/gtest.h>
#include <cstring>


using namespace ry;

// ===== Layout constants =====

TEST(StringHeader, LayoutConstants) {
    EXPECT_EQ(STRING_HEADER_EXTRA, 8u);
    EXPECT_EQ(STRING_HEADER_SIZE,  24u);
    EXPECT_EQ(STRING_BYTELEN_OFFSET, 8);
    // STRING_HEADER_SIZE = ARC_HEADER_SIZE (16) + byte_len field (8)
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
    // strong_count at offset 0, weak_count at offset 8, byte_len at offset 16
    auto *block = reinterpret_cast<int64_t *>(hdr);
    EXPECT_EQ(block[0], 1);   // strong_count = 1 (not immortal for dynamic alloc)
    EXPECT_EQ(block[1], 0);   // weak_count = 0
    EXPECT_EQ(block[2], 4);   // byte_len = 4
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
