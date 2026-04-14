#include <gtest/gtest.h>
#include "ry/runtime_regex.hpp"
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <string>


using namespace ry;
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
// Capture group backreference tests (#829)
// ============================================================

TEST(RegexRuntime, ReplaceCaptureFastPath) {
    // No backreferences → existing fast path, groups have no effect
    const char *r = __ry_regex_replace("(\\d+)", "a1b2", "X");
    EXPECT_STREQ(r, "aXbX");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureWholeMatch) {
    // $0 expands to the whole match
    const char *r = __ry_regex_replace("\\w+", "hello world", "[$0]");
    EXPECT_STREQ(r, "[hello] [world]");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureGroup1) {
    // $1 expands to first capture group
    const char *r = __ry_regex_replace("(\\w+)", "hello world", "[$1]");
    EXPECT_STREQ(r, "[hello] [world]");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureSwapGroups) {
    // Swap two captured words: $2 $1
    const char *r = __ry_regex_replace("(\\w+)@(\\w+)", "user@host", "$2@$1");
    EXPECT_STREQ(r, "host@user");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureThreeGroups) {
    // Date reformat: YYYY-MM-DD → DD/MM/YYYY
    const char *r = __ry_regex_replace("(\\d+)-(\\d+)-(\\d+)", "2026-04-10", "$3/$2/$1");
    EXPECT_STREQ(r, "10/04/2026");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureMultipleMatches) {
    // Multiple matches, each gets its own capture extraction
    const char *r = __ry_regex_replace("(\\w)(\\d)", "a1 b2 c3", "$2$1");
    EXPECT_STREQ(r, "1a 2b 3c");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureLiteralDollar) {
    // $$ → literal $
    const char *r = __ry_regex_replace("(\\d+)", "price 100", "$$$1");
    EXPECT_STREQ(r, "price $100");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureOutOfRange) {
    // $2 when only 1 group → empty string
    const char *r = __ry_regex_replace("(a)", "a", "$2");
    EXPECT_STREQ(r, "");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureNoGroupsWithDollar0) {
    // $0 works even with no capture groups (whole match)
    const char *r = __ry_regex_replace("\\d+", "num 42 num", "($0)");
    EXPECT_STREQ(r, "num (42) num");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureDollarNoDigit) {
    // $ not followed by digit → literal $
    const char *r = __ry_regex_replace("a", "abc", "$ b");
    EXPECT_STREQ(r, "$ bbc");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceCaptureMultiDigitBrace) {
    // ${N} syntax for groups beyond $9
    // Build pattern with 10 groups: (a)(b)(c)(d)(e)(f)(g)(h)(i)(j)
    const char *r = __ry_regex_replace(
        "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)",
        "abcdefghij",
        "${10}${9}");
    EXPECT_STREQ(r, "ji");
    free((void *)r);
}

TEST(RegexRuntime, ReplaceMalformedBrace) {
    // Malformed ${...} tokens must NOT trigger the capture backtracker
    // and must be emitted literally in the output.
    const char *r1 = __ry_regex_replace("(a)", "a", "${foo}");
    EXPECT_STREQ(r1, "${foo}");  // non-digit content: literal
    free((void *)r1);

    const char *r2 = __ry_regex_replace("(a)", "a", "${}");
    EXPECT_STREQ(r2, "${}");   // empty braces: literal
    free((void *)r2);
}

// ============================================================
// regex_split tests
// ============================================================

struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

// Layout of each element in a List<Match> returned by __ry_regex_find_all.
// Must match MatchData in runtime_list.hpp: {char* full, void* groups}.
struct MatchEntry {
    char  *full;
    void  *groups; // ListHeader* for captured groups
};

static void freeStringList(ListHeader *list) {
    for (int64_t i = 0; i < list->len; ++i) free(list->data[i]);
    free(list->data);
    free(list);
}

static void freeMatchList(ListHeader *list) {
    auto *entries = (MatchEntry *)list->data;
    for (int64_t i = 0; i < list->len; ++i) {
        free(entries[i].full);
        auto *groups = (ListHeader *)entries[i].groups;
        for (int64_t g = 0; g < groups->len; ++g) free(groups->data[g]);
        free(groups->data);
        free(groups);
    }
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
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "1");
    EXPECT_STREQ(e[1].full, "23");
    EXPECT_STREQ(e[2].full, "456");
    // No capture groups -> each groups list has length 0
    EXPECT_EQ(((ListHeader *)e[0].groups)->len, 0);
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllWords) {
    auto *list = (ListHeader *)__ry_regex_find_all("[a-z]+", "hello world foo");
    ASSERT_EQ(list->len, 3);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "hello");
    EXPECT_STREQ(e[1].full, "world");
    EXPECT_STREQ(e[2].full, "foo");
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllNoMatch) {
    auto *list = (ListHeader *)__ry_regex_find_all("[0-9]+", "hello");
    ASSERT_EQ(list->len, 0);
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllWithCaptureGroups) {
    auto *list = (ListHeader *)__ry_regex_find_all("(\\w+)@(\\w+)", "a@b x@y");
    ASSERT_EQ(list->len, 2);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "a@b");
    auto *g0 = (ListHeader *)e[0].groups;
    ASSERT_EQ(g0->len, 2);
    EXPECT_STREQ(g0->data[0], "a");
    EXPECT_STREQ(g0->data[1], "b");
    EXPECT_STREQ(e[1].full, "x@y");
    auto *g1 = (ListHeader *)e[1].groups;
    ASSERT_EQ(g1->len, 2);
    EXPECT_STREQ(g1->data[0], "x");
    EXPECT_STREQ(g1->data[1], "y");
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllUnmatchedOptionalGroup) {
    // (a)? doesn't match in "b" -> group should be empty string
    auto *list = (ListHeader *)__ry_regex_find_all("(a)?b", "b");
    ASSERT_EQ(list->len, 1);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "b");
    auto *g = (ListHeader *)e[0].groups;
    ASSERT_EQ(g->len, 1);
    EXPECT_STREQ(g->data[0], "");
    freeMatchList(list);
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
    // Explicitly escaped literal brace
    EXPECT_EQ(__ry_regex_match("\\{abc\\}", "{abc}"), 1);

    // Invalid brace pattern should be treated as literal '{'
    EXPECT_EQ(__ry_regex_match("a{,}b", "a{,}b"), 1);
}

TEST(RegexRuntime, QuantifierFindAll) {
    auto *list = (ListHeader *)__ry_regex_find_all("\\d{2,3}", "1 23 456 7890");
    ASSERT_EQ(list->len, 3);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "23");
    EXPECT_STREQ(e[1].full, "456");
    EXPECT_STREQ(e[2].full, "789");
    freeMatchList(list);
}

// ============================================================
// Non-greedy (lazy) when tests
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
    // a+? should when single 'a' (shortest)
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
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "<a>");
    EXPECT_STREQ(e[1].full, "<bb>");
    EXPECT_STREQ(e[2].full, "<ccc>");
    freeMatchList(list);
}

// ============================================================
// Word boundary \b / \B tests
// ============================================================

TEST(RegexRuntime, WordBoundarySearch) {
    // \bworld\b matches "world" in "hello world"
    EXPECT_EQ(__ry_regex_search("\\bworld\\b", "hello world"), 6);
    // \bword\b does NOT when in "helloworld" (no boundary)
    EXPECT_EQ(__ry_regex_search("\\bword\\b", "helloworld"), -1);
}

TEST(RegexRuntime, WordBoundaryNonBoundary) {
    // \Bword matches in "helloworld" (non-boundary before 'w')
    EXPECT_EQ(__ry_regex_search("\\Bworld", "helloworld"), 5);
    // \Bworld should NOT when at start of "world test"
    EXPECT_EQ(__ry_regex_search("\\Bworld", "world test"), -1);
}

TEST(RegexRuntime, WordBoundaryStartEnd) {
    // \b at start of string
    EXPECT_EQ(__ry_regex_search("\\bhello", "hello world"), 0);
    // \b at end of string
    EXPECT_EQ(__ry_regex_search("world\\b", "hello world"), 6);
}

TEST(RegexRuntime, WordBoundaryDigitUnderscore) {
    // digits and underscores are word characters
    EXPECT_EQ(__ry_regex_match("\\b\\w+\\b", "hello_123"), 1);
    EXPECT_EQ(__ry_regex_search("\\b123\\b", "abc 123 def"), 4);
    EXPECT_EQ(__ry_regex_search("\\b_foo\\b", "x _foo y"), 2);
}

TEST(RegexRuntime, WordBoundaryFullMatch) {
    EXPECT_EQ(__ry_regex_match("\\btest\\b", "test"), 1);
    EXPECT_EQ(__ry_regex_match("\\btest\\b", "testing"), 0);
}

TEST(RegexRuntime, WordBoundaryFindAll) {
    auto *list = (ListHeader *)__ry_regex_find_all("\\b\\w+\\b", "hello world foo");
    ASSERT_EQ(list->len, 3);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "hello");
    EXPECT_STREQ(e[1].full, "world");
    EXPECT_STREQ(e[2].full, "foo");
    freeMatchList(list);
}

// ============================================================
// Case-insensitive (?i) tests
// ============================================================

TEST(RegexRuntime, CaseInsensitiveMatch) {
    EXPECT_EQ(__ry_regex_match("(?i)hello", "HELLO"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)hello", "Hello"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)hello", "hello"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)hello", "hElLo"), 1);
}

TEST(RegexRuntime, CaseInsensitiveCharClass) {
    EXPECT_EQ(__ry_regex_match("(?i)[a-z]+", "ABC"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)[a-z]+", "AbCdE"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)[a-f]+", "ABCDEF"), 1);
    EXPECT_EQ(__ry_regex_match("(?i)[a-f]+", "abcdef"), 1);
}

TEST(RegexRuntime, CaseSensitiveDefault) {
    // Without (?i), should be case-sensitive
    EXPECT_EQ(__ry_regex_match("hello", "HELLO"), 0);
    EXPECT_EQ(__ry_regex_match("[a-z]+", "ABC"), 0);
}

TEST(RegexRuntime, CaseInsensitiveSearch) {
    EXPECT_EQ(__ry_regex_search("(?i)world", "Hello WORLD"), 6);
    EXPECT_EQ(__ry_regex_search("(?i)\\bworld\\b", "Hello WORLD"), 6);
}

TEST(RegexRuntime, CaseInsensitiveReplace) {
    const char *result = __ry_regex_replace("(?i)hello", "Hello HELLO hello", "X");
    EXPECT_STREQ(result, "X X X");
    free((void *)result);
}

TEST(RegexRuntime, CaseInsensitiveFindAll) {
    auto *list = (ListHeader *)__ry_regex_find_all("(?i)hello", "Hello HELLO hello");
    ASSERT_EQ(list->len, 3);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "Hello");
    EXPECT_STREQ(e[1].full, "HELLO");
    EXPECT_STREQ(e[2].full, "hello");
    freeMatchList(list);
}

// ============================================================
// Performance regression tests (issue #107)
// ============================================================

TEST(RegexRuntime, PerfSearchLongNonMatch) {
    // Pattern "a" on 10000 'b's: previously O(n^2), now O(n*s)
    std::string text(10000, 'b');
    auto start = std::chrono::steady_clock::now();
    int64_t result = __ry_regex_search("a", text.c_str());
    auto elapsed = std::chrono::steady_clock::now() - start;
    EXPECT_EQ(result, -1);
    EXPECT_LT(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count(), 1000);
}

TEST(RegexRuntime, PerfSearchDotStarNonMatch) {
    // Pattern ".*x" on 10000 'a's: worst case for naive approach
    std::string text(10000, 'a');
    auto start = std::chrono::steady_clock::now();
    int64_t result = __ry_regex_search(".*x", text.c_str());
    auto elapsed = std::chrono::steady_clock::now() - start;
    EXPECT_EQ(result, -1);
    EXPECT_LT(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count(), 1000);
}

TEST(RegexRuntime, PerfFindAllManyMatches) {
    // Pattern "[a-z]+" on 10000 lowercase chars interspersed with spaces
    std::string text;
    text.reserve(20000);
    for (int i = 0; i < 10000; ++i) {
        text += (char)('a' + (i % 26));
        if (i % 5 == 4) text += ' ';
    }
    auto start = std::chrono::steady_clock::now();
    auto *list = (ListHeader *)__ry_regex_find_all("[a-z]+", text.c_str());
    auto elapsed = std::chrono::steady_clock::now() - start;
    EXPECT_GT(list->len, 0);
    EXPECT_LT(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count(), 1000);
    freeMatchList(list);
}

// Regression test: lazy search must preserve leftmost-start semantics
TEST(RegexRuntime, LazySearchLeftmostStart) {
    // Pattern "a.+?b|c" on "acb": the 'c' alternative matches at pos 1,
    // but "a.+?b" matches starting at pos 0 (leftmost wins).
    EXPECT_EQ(__ry_regex_search("a.+?b|c", "acb"), 0);

    // Pattern "a.+?x|b" on "bax": 'b' matches at pos 0 (leftmost)
    EXPECT_EQ(__ry_regex_search("a.+?x|b", "bax"), 0);
}

TEST(RegexRuntime, LazyFindAllLeftmostStart) {
    // findAll must also respect leftmost-start ordering
    auto *list = (ListHeader *)__ry_regex_find_all("a.+?b|c", "acb");
    ASSERT_EQ(list->len, 1);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "acb");
    freeMatchList(list);
}

// --- ReDoS protection tests ---

TEST(RegexSecurity, ModerateGroupNestingSucceeds) {
    // 10 nested groups should work fine
    std::string pattern = std::string(10, '(') + "a" + std::string(10, ')');
    EXPECT_TRUE(__ry_regex_match(pattern.c_str(), "a"));
}

TEST(RegexSecurity, NormalPatternWithStepLimit) {
    // Normal patterns should complete well within the step limit
    EXPECT_TRUE(__ry_regex_match("(a+)(b+)(c+)", "aaabbbccc"));
}
