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

// ============================================================
// Range quantifier tests {n}, {n,m}, {n,}
// ============================================================

TEST(RegexRuntime, QuantifierExact) {
    EXPECT_EQ(__ry_regex_match("a{3}", "aaa"), 1);
    EXPECT_EQ(__ry_regex_match("a{3}", "aa"), 0);
    EXPECT_EQ(__ry_regex_match("a{3}", "aaaa"), 0);
}

TEST(RegexRuntime, QuantifierRange) {
    EXPECT_EQ(__ry_regex_match("a{2,4}", "a"), 0);
    EXPECT_EQ(__ry_regex_match("a{2,4}", "aa"), 1);
    EXPECT_EQ(__ry_regex_match("a{2,4}", "aaa"), 1);
    EXPECT_EQ(__ry_regex_match("a{2,4}", "aaaa"), 1);
    EXPECT_EQ(__ry_regex_match("a{2,4}", "aaaaa"), 0);
}

TEST(RegexRuntime, QuantifierMinOnly) {
    EXPECT_EQ(__ry_regex_match("a{2,}", "a"), 0);
    EXPECT_EQ(__ry_regex_match("a{2,}", "aa"), 1);
    EXPECT_EQ(__ry_regex_match("a{2,}", "aaaaaaa"), 1);
}

TEST(RegexRuntime, QuantifierWithGroup) {
    EXPECT_EQ(__ry_regex_match("(ab){2}", "abab"), 1);
    EXPECT_EQ(__ry_regex_match("(ab){2}", "ab"), 0);
    EXPECT_EQ(__ry_regex_match("(ab){2,3}", "ababab"), 1);
}

TEST(RegexRuntime, QuantifierWithCharClass) {
    // Phone number pattern: \d{3}-\d{4}
    EXPECT_EQ(__ry_regex_match("\\d{3}-\\d{4}", "123-4567"), 1);
    EXPECT_EQ(__ry_regex_match("\\d{3}-\\d{4}", "12-4567"), 0);
}

TEST(RegexRuntime, QuantifierEdgeCases) {
    EXPECT_EQ(__ry_regex_match("a{0}", ""), 1);
    EXPECT_EQ(__ry_regex_match("a{1}", "a"), 1);
    EXPECT_EQ(__ry_regex_match("a{1}", ""), 0);
    EXPECT_EQ(__ry_regex_match("a{0,}", ""), 1);   // same as a*
    EXPECT_EQ(__ry_regex_match("a{0,}", "aaa"), 1); // same as a*
    EXPECT_EQ(__ry_regex_match("a{1,}", ""), 0);    // same as a+
    EXPECT_EQ(__ry_regex_match("a{0,1}", ""), 1);   // same as a?
    EXPECT_EQ(__ry_regex_match("a{0,1}", "a"), 1);  // same as a?
}

TEST(RegexRuntime, QuantifierBraceLiteralFallback) {
    // Invalid brace pattern should be treated as literal '{'
    EXPECT_EQ(__ry_regex_match("\\{abc\\}", "{abc}"), 1);
}

TEST(RegexRuntime, QuantifierFindAll) {
    auto *list = (ListHeader *)__ry_regex_find_all("\\d{2,3}", "1 23 456 7890");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "23");
    EXPECT_STREQ(list->data[1], "456");
    EXPECT_STREQ(list->data[2], "789");
    freeStringList(list);
}

// ============================================================
// Non-greedy (lazy) match tests
// ============================================================

TEST(RegexRuntime, LazyStarReplace) {
    // Greedy: ".*" matches the longest string between first and last quote
    const char *greedy = __ry_regex_replace("\".*\"", "\"a\" and \"b\"", "X");
    EXPECT_STREQ(greedy, "X");
    free((void *)greedy);

    // Lazy: ".*?" matches the shortest string between quotes
    const char *lazy = __ry_regex_replace("\".*?\"", "\"a\" and \"b\"", "X");
    EXPECT_STREQ(lazy, "X and X");
    free((void *)lazy);
}

TEST(RegexRuntime, LazyPlusSearch) {
    // a+? should match single 'a' (shortest)
    EXPECT_EQ(__ry_regex_search("a+?", "aaa"), 0);
    // Verify it matched just 1 character by using replace
    const char *result = __ry_regex_replace("a+?", "aaa", "X");
    EXPECT_STREQ(result, "XXX");
    free((void *)result);
}

TEST(RegexRuntime, LazyQuestion) {
    // a?? prefers matching 0 'a's (non-greedy)
    const char *result = __ry_regex_replace("a??", "aaa", "X");
    // a?? matches empty string before each char and after last
    EXPECT_STREQ(result, "XaXaXaX");
    free((void *)result);
}

TEST(RegexRuntime, LazyQuantifierBrace) {
    // a{2,4}? prefers matching 2 'a's (minimum)
    const char *result = __ry_regex_replace("a{2,4}?", "aaaa", "X");
    EXPECT_STREQ(result, "XX");
    free((void *)result);
}

TEST(RegexRuntime, LazyFullMatch) {
    // fullMatch always matches entire string regardless of greedy/lazy
    EXPECT_EQ(__ry_regex_match("a+?", "aaa"), 1);
    EXPECT_EQ(__ry_regex_match("a*?", "aaa"), 1);
    EXPECT_EQ(__ry_regex_match("a{2,4}?", "aaa"), 1);
}

TEST(RegexRuntime, LazyPracticalExample) {
    // Replace individual quoted strings
    const char *result = __ry_regex_replace("\"[^\"]*\"", "say \"hello\" and \"world\"", "X");
    EXPECT_STREQ(result, "say X and X");
    free((void *)result);
}

TEST(RegexRuntime, LazyFindAll) {
    // .*? in findAll should find shortest matches
    auto *list = (ListHeader *)__ry_regex_find_all("<.*?>", "<a> <bb> <ccc>");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "<a>");
    EXPECT_STREQ(list->data[1], "<bb>");
    EXPECT_STREQ(list->data[2], "<ccc>");
    freeStringList(list);
}
