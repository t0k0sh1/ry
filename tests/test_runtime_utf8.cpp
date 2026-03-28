#include <gtest/gtest.h>
#include <cstdlib>

extern "C" {
int64_t __ry_utf8_len(const char *s);
char *__ry_utf8_char_at(const char *s, int64_t i);
char *__ry_utf8_substring(const char *s, int64_t start, int64_t end);
char *__ry_utf8_reverse(const char *s);
int64_t __ry_utf8_char_index(const char *s, int64_t byte_offset);
}

// Helper: compare and free
static void expectStr(char *got, const char *expected) {
    EXPECT_STREQ(got, expected);
    free(got);
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
    expectStr(__ry_utf8_substring("hello world", 0, 5), "hello");
    expectStr(__ry_utf8_substring("hello world", 6, 11), "world");
}

TEST(RuntimeUtf8, Substring3Byte) {
    // "あいう" substring(0,2) = "あい"
    expectStr(__ry_utf8_substring("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86", 0, 2),
              "\xE3\x81\x82\xE3\x81\x84");
}

TEST(RuntimeUtf8, ReverseAscii) {
    expectStr(__ry_utf8_reverse("hello"), "olleh");
}

TEST(RuntimeUtf8, Reverse3Byte) {
    // "あいう" reversed = "ういあ"
    expectStr(__ry_utf8_reverse("\xE3\x81\x82\xE3\x81\x84\xE3\x81\x86"),
              "\xE3\x81\x86\xE3\x81\x84\xE3\x81\x82");
}

TEST(RuntimeUtf8, Reverse4Byte) {
    // "😀😁" reversed = "😁😀"
    expectStr(__ry_utf8_reverse("\xF0\x9F\x98\x80\xF0\x9F\x98\x81"),
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
