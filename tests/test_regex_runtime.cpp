#include <gtest/gtest.h>
#include "ry/runtime_regex.hpp"
#include <cstdlib>
#include <cstring>

// ============================================================
// regex_match tests
// ============================================================

TEST(RegexRuntime, MatchLiteral) {
    EXPECT_EQ(__ry_regex_match("hello", "hello"), 1);
    EXPECT_EQ(__ry_regex_match("hello", "world"), 0);
}

TEST(RegexRuntime, MatchConcat) {
    EXPECT_EQ(__ry_regex_match("ab", "ab"), 1);
    EXPECT_EQ(__ry_regex_match("ab", "a"), 0);
    EXPECT_EQ(__ry_regex_match("ab", "abc"), 0);
}

TEST(RegexRuntime, MatchAlternation) {
    EXPECT_EQ(__ry_regex_match("cat|dog", "cat"), 1);
    EXPECT_EQ(__ry_regex_match("cat|dog", "dog"), 1);
    EXPECT_EQ(__ry_regex_match("cat|dog", "fish"), 0);
}

TEST(RegexRuntime, MatchStar) {
    EXPECT_EQ(__ry_regex_match("a*", ""), 1);
    EXPECT_EQ(__ry_regex_match("a*", "a"), 1);
    EXPECT_EQ(__ry_regex_match("a*", "aaa"), 1);
    EXPECT_EQ(__ry_regex_match("a*", "b"), 0);
    EXPECT_EQ(__ry_regex_match("ab*c", "ac"), 1);
    EXPECT_EQ(__ry_regex_match("ab*c", "abc"), 1);
    EXPECT_EQ(__ry_regex_match("ab*c", "abbbc"), 1);
}

TEST(RegexRuntime, MatchPlus) {
    EXPECT_EQ(__ry_regex_match("a+", ""), 0);
    EXPECT_EQ(__ry_regex_match("a+", "a"), 1);
    EXPECT_EQ(__ry_regex_match("a+", "aaa"), 1);
}

TEST(RegexRuntime, MatchQuestion) {
    EXPECT_EQ(__ry_regex_match("a?", ""), 1);
    EXPECT_EQ(__ry_regex_match("a?", "a"), 1);
    EXPECT_EQ(__ry_regex_match("a?b", "b"), 1);
    EXPECT_EQ(__ry_regex_match("a?b", "ab"), 1);
    EXPECT_EQ(__ry_regex_match("a?b", "aab"), 0);
}

TEST(RegexRuntime, MatchDot) {
    EXPECT_EQ(__ry_regex_match(".", "a"), 1);
    EXPECT_EQ(__ry_regex_match(".", "Z"), 1);
    EXPECT_EQ(__ry_regex_match(".", ""), 0);
    EXPECT_EQ(__ry_regex_match("..", "ab"), 1);
    EXPECT_EQ(__ry_regex_match("a.c", "abc"), 1);
    EXPECT_EQ(__ry_regex_match("a.c", "aXc"), 1);
}

TEST(RegexRuntime, MatchCharClass) {
    EXPECT_EQ(__ry_regex_match("[abc]", "a"), 1);
    EXPECT_EQ(__ry_regex_match("[abc]", "b"), 1);
    EXPECT_EQ(__ry_regex_match("[abc]", "d"), 0);
    EXPECT_EQ(__ry_regex_match("[a-z]", "m"), 1);
    EXPECT_EQ(__ry_regex_match("[a-z]", "M"), 0);
    EXPECT_EQ(__ry_regex_match("[a-z]+", "hello"), 1);
}

TEST(RegexRuntime, MatchCharClassNegated) {
    EXPECT_EQ(__ry_regex_match("[^0-9]", "a"), 1);
    EXPECT_EQ(__ry_regex_match("[^0-9]", "5"), 0);
    EXPECT_EQ(__ry_regex_match("[^abc]+", "xyz"), 1);
}

TEST(RegexRuntime, MatchGroup) {
    EXPECT_EQ(__ry_regex_match("(ab)+", "ab"), 1);
    EXPECT_EQ(__ry_regex_match("(ab)+", "abab"), 1);
    EXPECT_EQ(__ry_regex_match("(ab)+", "a"), 0);
    EXPECT_EQ(__ry_regex_match("(a|b)*", "abba"), 1);
}

TEST(RegexRuntime, MatchAnchors) {
    EXPECT_EQ(__ry_regex_match("^hello$", "hello"), 1);
    EXPECT_EQ(__ry_regex_match("^hello$", "hello world"), 0);
    EXPECT_EQ(__ry_regex_match("^a.*z$", "abcz"), 1);
}

TEST(RegexRuntime, MatchEmpty) {
    EXPECT_EQ(__ry_regex_match("", ""), 1);
    EXPECT_EQ(__ry_regex_match("", "a"), 0);
}

TEST(RegexRuntime, MatchComplex) {
    EXPECT_EQ(__ry_regex_match("[a-zA-Z_][a-zA-Z0-9_]*", "hello_world"), 1);
    EXPECT_EQ(__ry_regex_match("[a-zA-Z_][a-zA-Z0-9_]*", "_foo123"), 1);
    EXPECT_EQ(__ry_regex_match("[a-zA-Z_][a-zA-Z0-9_]*", "123abc"), 0);
}

TEST(RegexRuntime, MatchEscapedChars) {
    EXPECT_EQ(__ry_regex_match("a\\.b", "a.b"), 1);
    EXPECT_EQ(__ry_regex_match("a\\.b", "axb"), 0);
    EXPECT_EQ(__ry_regex_match("a\\*b", "a*b"), 1);
}

TEST(RegexRuntime, MatchShorthandClasses) {
    EXPECT_EQ(__ry_regex_match("\\d+", "123"), 1);
    EXPECT_EQ(__ry_regex_match("\\d+", "abc"), 0);
    EXPECT_EQ(__ry_regex_match("\\w+", "hello_123"), 1);
    EXPECT_EQ(__ry_regex_match("\\s+", "  \t"), 1);
    EXPECT_EQ(__ry_regex_match("\\D+", "abc"), 1);
    EXPECT_EQ(__ry_regex_match("\\D+", "123"), 0);
}

// ============================================================
// regex_search tests
// ============================================================

TEST(RegexRuntime, SearchBasic) {
    EXPECT_EQ(__ry_regex_search("world", "hello world"), 6);
    EXPECT_EQ(__ry_regex_search("xyz", "hello world"), -1);
}

TEST(RegexRuntime, SearchPattern) {
    EXPECT_EQ(__ry_regex_search("[0-9]+", "abc123def"), 3);
    EXPECT_EQ(__ry_regex_search("\\d+", "abc123def"), 3);
}

TEST(RegexRuntime, SearchAtStart) {
    EXPECT_EQ(__ry_regex_search("hello", "hello world"), 0);
}

TEST(RegexRuntime, SearchNotFound) {
    EXPECT_EQ(__ry_regex_search("xyz", "abc"), -1);
}

// ============================================================
// regex_replace tests
// ============================================================

TEST(RegexRuntime, ReplaceBasic) {
    const char *result = __ry_regex_replace("world", "hello world", "universe");
    EXPECT_STREQ(result, "hello universe");
    free((void *)result);
}

TEST(RegexRuntime, ReplaceAll) {
    const char *result = __ry_regex_replace("[0-9]+", "a1b2c3", "X");
    EXPECT_STREQ(result, "aXbXcX");
    free((void *)result);
}

TEST(RegexRuntime, ReplaceNoMatch) {
    const char *result = __ry_regex_replace("xyz", "hello", "abc");
    EXPECT_STREQ(result, "hello");
    free((void *)result);
}

TEST(RegexRuntime, ReplaceEmpty) {
    const char *result = __ry_regex_replace("x", "xxx", "");
    EXPECT_STREQ(result, "");
    free((void *)result);
}

// ============================================================
// regex_split tests
// ============================================================

struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

static void freeStringList(ListHeader *list) {
    for (int64_t i = 0; i < list->len; ++i) free(list->data[i]);
    free(list->data);
    free(list);
}

TEST(RegexRuntime, SplitBasic) {
    auto *list = (ListHeader *)__ry_regex_split(",", "a,b,c");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "a");
    EXPECT_STREQ(list->data[1], "b");
    EXPECT_STREQ(list->data[2], "c");
    freeStringList(list);
}

TEST(RegexRuntime, SplitPattern) {
    auto *list = (ListHeader *)__ry_regex_split("\\s+", "hello  world\tfoo");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "hello");
    EXPECT_STREQ(list->data[1], "world");
    EXPECT_STREQ(list->data[2], "foo");
    freeStringList(list);
}

TEST(RegexRuntime, SplitNoMatch) {
    auto *list = (ListHeader *)__ry_regex_split(",", "hello");
    ASSERT_EQ(list->len, 1);
    EXPECT_STREQ(list->data[0], "hello");
    freeStringList(list);
}

// ============================================================
// regex_find_all tests
// ============================================================

TEST(RegexRuntime, FindAllBasic) {
    auto *list = (ListHeader *)__ry_regex_find_all("[0-9]+", "a1b23c456");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "1");
    EXPECT_STREQ(list->data[1], "23");
    EXPECT_STREQ(list->data[2], "456");
    freeStringList(list);
}

TEST(RegexRuntime, FindAllWords) {
    auto *list = (ListHeader *)__ry_regex_find_all("[a-z]+", "hello world foo");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "hello");
    EXPECT_STREQ(list->data[1], "world");
    EXPECT_STREQ(list->data[2], "foo");
    freeStringList(list);
}

TEST(RegexRuntime, FindAllNoMatch) {
    auto *list = (ListHeader *)__ry_regex_find_all("[0-9]+", "hello");
    ASSERT_EQ(list->len, 0);
    freeStringList(list);
}
