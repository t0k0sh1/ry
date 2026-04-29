#include <gtest/gtest.h>
#include "ry/runtime_string.hpp"
#include <cstdlib>


extern "C" {
int64_t __ry_utf8_len(const char *s);
char *__ry_utf8_char_at(const char *s, int64_t i);
char *__ry_utf8_substring(const char *s, int64_t byteLen, int64_t start, int64_t end);
char *__ry_utf8_reverse(const char *s, int64_t byteLen);
int64_t __ry_utf8_char_index(const char *s, int64_t byte_offset);
char *__ry_utf8_char_at_checked(const char *s, int64_t byteLen, int64_t i);
void *__ry_split_chars(const char *s, int64_t byteLen);
}

// Helper: compare and free (runtime_utf8 returns StringHeader-managed pointers)
static void expectStr(char *got, const char *expected) {
    EXPECT_STREQ(got, expected);
    ry::freeStringSlot(got);
}

TEST(RuntimeUtf8, LenAscii) {
    EXPECT_EQ(__ry_utf8_len("hello"), 5);
    EXPECT_EQ(__ry_utf8_len(""), 0);
}

TEST(RuntimeUtf8, Len2Byte) {
    // "café" = c(1) a(1) f(1) é(2) = 4 chars
    EXPECT_EQ(__ry_utf8_len("caf\xC3\xA9"), 4);
}

TEST(RuntimeUtf8, Len3Byte) {
    // "あいう" = 3 chars, 9 bytes
    EXPECT_EQ(__ry_utf8_len("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86"), 3);
}

TEST(RuntimeUtf8, Len4Byte) {
    // "😀😁" = 2 chars
    EXPECT_EQ(__ry_utf8_len("\xF0\x9F\x98\x80\xF0\x9F\x98\x81"), 2);
}

TEST(RuntimeUtf8, LenTruncated2Byte) {
    // Truncated 2-byte sequence: lead byte 0xC3 followed by '\0'
    // Should treat the lead byte as 1 char
    EXPECT_EQ(__ry_utf8_len("\xC3"), 1);
}

TEST(RuntimeUtf8, LenTruncated3Byte) {
    // Truncated 3-byte: lead byte + 1 continuation, missing 2nd continuation
    // 0xE3 0x81 → 2 invalid bytes, each counted as 1 char
    EXPECT_EQ(__ry_utf8_len("\xE3\x81"), 2);
    // Lead byte only: 0xE3 → 1 char
    EXPECT_EQ(__ry_utf8_len("\xE3"), 1);
}

TEST(RuntimeUtf8, LenTruncated4Byte) {
    // Truncated 4-byte: lead + 1 continuation
    // 0xF0 0x9F → 2 invalid bytes
    EXPECT_EQ(__ry_utf8_len("\xF0\x9F"), 2);
    // Truncated 4-byte: lead + 2 continuations
    // 0xF0 0x9F 0x98 → 3 invalid bytes
    EXPECT_EQ(__ry_utf8_len("\xF0\x9F\x98"), 3);
    // Lead byte only: 0xF0 → 1 char
    EXPECT_EQ(__ry_utf8_len("\xF0"), 1);
}

TEST(RuntimeUtf8, CharAtAscii) {
    expectStr(__ry_utf8_char_at("hello", 0), "h");
    expectStr(__ry_utf8_char_at("hello", 4), "o");
}

TEST(RuntimeUtf8, CharAt3Byte) {
    // "あいう" index 1 = "い"
    expectStr(__ry_utf8_char_at("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 1),
              "\xE3\x81\x84");
}

TEST(RuntimeUtf8, CharAt4Byte) {
    // "😀😁" index 1 = "😁"
    expectStr(__ry_utf8_char_at("\xF0\x9F\x98\x80\xF0\x9F\x98\x81", 1),
              "\xF0\x9F\x98\x81");
}

TEST(RuntimeUtf8, SubstringAscii) {
    expectStr(__ry_utf8_substring("hello world", 11, 0, 5), "hello");
    expectStr(__ry_utf8_substring("hello world", 11, 6, 11), "world");
}

TEST(RuntimeUtf8, Substring3Byte) {
    // "あいう" substr(0,2) = "あい"
    expectStr(__ry_utf8_substring("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 9, 0, 2),
              "\xE3\x81\x82\xE3\x81\x84");
}

TEST(RuntimeUtf8, ReverseAscii) {
    expectStr(__ry_utf8_reverse("hello", 5), "olleh");
}

TEST(RuntimeUtf8, Reverse3Byte) {
    // "あいう" reversed = "ういあ"
    expectStr(__ry_utf8_reverse("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 9),
              "\xE3\x81\x86\xE3\x81\x84\xE3\x81\x82");
}

TEST(RuntimeUtf8, Reverse4Byte) {
    // "😀😁" reversed = "😁😀"
    expectStr(__ry_utf8_reverse("\xF0\x9F\x98\x80\xF0\x9F\x98\x81", 8),
              "\xF0\x9F\x98\x81\xF0\x9F\x98\x80");
}

TEST(RuntimeUtf8, CharIndexAscii) {
    EXPECT_EQ(__ry_utf8_char_index("hello world", 6), 6);
}

TEST(RuntimeUtf8, CharIndex3Byte) {
    // "あいう" byte offset 3 (start of い) → char index 1
    EXPECT_EQ(__ry_utf8_char_index("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 3), 1);
    // byte offset 6 (start of う) → char index 2
    EXPECT_EQ(__ry_utf8_char_index("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 6), 2);
}

TEST(RuntimeUtf8, CharIndexMixed) {
    // "café" = c(1) a(1) f(1) é(2) → byte offset 3 = char index 3
    EXPECT_EQ(__ry_utf8_char_index("caf\xC3\xA9", 3), 3);
}

// --- __ry_utf8_char_at_checked tests ---

TEST(RuntimeUtf8, CharAtCheckedAscii) {
    expectStr(__ry_utf8_char_at_checked("hello", 5, 0), "h");
    expectStr(__ry_utf8_char_at_checked("hello", 5, 4), "o");
}

TEST(RuntimeUtf8, CharAtCheckedUtf8) {
    // "あいう" index 1 = "い"
    expectStr(__ry_utf8_char_at_checked("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 9, 1),
              "\xE3\x81\x84");
}

TEST(RuntimeUtf8, CharAtChecked4Byte) {
    // "😀😁" index 1 = "😁"
    expectStr(__ry_utf8_char_at_checked("\xF0\x9F\x98\x80\xF0\x9F\x98\x81", 8, 1),
              "\xF0\x9F\x98\x81");
}

TEST(RuntimeUtf8, CharAtCheckedNegativeIndex) {
    expectStr(__ry_utf8_char_at_checked("hello", 5, -1), "o");
    expectStr(__ry_utf8_char_at_checked("hello", 5, -5), "h");
}

TEST(RuntimeUtf8, CharAtCheckedNegativeUtf8) {
    // "あいう" index -1 = "う"
    expectStr(__ry_utf8_char_at_checked("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 9, -1),
              "\xE3\x81\x86");
}

// OOB behavior for __ry_utf8_char_at_checked is covered by codegen-level
// death tests: StringCharAtOutOfRange, StringCharAtNegativeOOB,
// StringCharAtEmptyString, StringCharAtUTF8OutOfRange.

// --- NUL-safe tests (#1049) ---

TEST(RuntimeUtf8, SubstringNulEmbedded) {
    // "a\0b" (byteLen=3): substr(0,3) must return all 3 bytes
    char s[] = {'a', '\0', 'b'};
    char *r = __ry_utf8_substring(s, 3, 0, 3);
    // Compare byte by byte since STREQ stops at NUL
    EXPECT_EQ(r[0], 'a');
    EXPECT_EQ(r[1], '\0');
    EXPECT_EQ(r[2], 'b');
    ry::freeStringSlot(r);
}

TEST(RuntimeUtf8, SubstringNulOnly) {
    char s[] = {'\0', '\0'};
    char *r = __ry_utf8_substring(s, 2, 0, 2);
    EXPECT_EQ(r[0], '\0');
    EXPECT_EQ(r[1], '\0');
    ry::freeStringSlot(r);
}

TEST(RuntimeUtf8, SubstringNulSuffix) {
    // "ab\0" substr(1,3) = "b\0"
    char s[] = {'a', 'b', '\0'};
    char *r = __ry_utf8_substring(s, 3, 1, 3);
    EXPECT_EQ(r[0], 'b');
    EXPECT_EQ(r[1], '\0');
    ry::freeStringSlot(r);
}

TEST(RuntimeUtf8, CharAtCheckedNulEmbedded) {
    // "a\0b": charAt 0="a", 1=NUL, 2="b"
    char s[] = {'a', '\0', 'b'};
    char *r0 = __ry_utf8_char_at_checked(s, 3, 0);
    EXPECT_EQ(r0[0], 'a');
    ry::freeStringSlot(r0);

    char *r1 = __ry_utf8_char_at_checked(s, 3, 1);
    EXPECT_EQ(r1[0], '\0');
    ry::freeStringSlot(r1);

    char *r2 = __ry_utf8_char_at_checked(s, 3, 2);
    EXPECT_EQ(r2[0], 'b');
    ry::freeStringSlot(r2);
}

TEST(RuntimeUtf8, CharAtCheckedNulNegative) {
    // "a\0b": charAt -1="b", -2=NUL, -3="a"
    char s[] = {'a', '\0', 'b'};
    char *rm1 = __ry_utf8_char_at_checked(s, 3, -1);
    EXPECT_EQ(rm1[0], 'b');
    ry::freeStringSlot(rm1);

    char *rm2 = __ry_utf8_char_at_checked(s, 3, -2);
    EXPECT_EQ(rm2[0], '\0');
    ry::freeStringSlot(rm2);
}

TEST(RuntimeUtf8, ReverseNulEmbedded) {
    // reverse("a\0b") = "b\0a"
    char s[] = {'a', '\0', 'b'};
    char *r = __ry_utf8_reverse(s, 3);
    EXPECT_EQ(r[0], 'b');
    EXPECT_EQ(r[1], '\0');
    EXPECT_EQ(r[2], 'a');
    ry::freeStringSlot(r);
}

TEST(RuntimeUtf8, ReverseNulOnly) {
    char s[] = {'\0'};
    char *r = __ry_utf8_reverse(s, 1);
    EXPECT_EQ(r[0], '\0');
    ry::freeStringSlot(r);
}
