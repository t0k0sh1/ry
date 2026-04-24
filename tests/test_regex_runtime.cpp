#include <gtest/gtest.h>
#include "ry/runtime_arc.hpp"
#include "ry/runtime_regex.hpp"
#include "ry/runtime_string.hpp"
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>


using namespace ry;

// ============================================================
// Local helpers matching the runtime list layout
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
    for (int64_t i = 0; i < list->len; ++i) freeStringSlot(list->data[i]);
    free(list->data);
    arc_free(list); // ListHeader is arc_alloc'd by makeStringList
}

static void freeMatchList(ListHeader *list) {
    auto *entries = (MatchEntry *)list->data;
    for (int64_t i = 0; i < list->len; ++i) {
        freeStringSlot(entries[i].full);
        auto *groups = (ListHeader *)entries[i].groups;
        for (int64_t g = 0; g < groups->len; ++g) freeStringSlot(groups->data[g]);
        free(groups->data);
        arc_free(groups); // groups ListHeader is arc_alloc'd by makeStringList
    }
    free(list->data);
    arc_free(list); // ListHeader is arc_alloc'd by makeMatchList
}

// Convenience wrappers: adapt NUL-terminated C-string calls to the new
// length-aware ABI.  Existing tests that don't embed NUL bytes use these.
static int64_t rm(const char *p, const char *t) {
    return __ry_regex_match(p, (int64_t)strlen(p), t, (int64_t)strlen(t));
}
static int64_t rs(const char *p, const char *t) {
    return __ry_regex_search(p, (int64_t)strlen(p), t, (int64_t)strlen(t));
}
static int64_t rim(const char *p, const char *t) {
    return __ry_regex_is_match(p, (int64_t)strlen(p), t, (int64_t)strlen(t));
}
static const char *rr(const char *p, const char *t, const char *r) {
    return __ry_regex_replace(p, (int64_t)strlen(p), t, (int64_t)strlen(t),
                              r, (int64_t)strlen(r));
}
static ListHeader *rsp(const char *p, const char *t) {
    return (ListHeader *)__ry_regex_split(p, (int64_t)strlen(p),
                                          t, (int64_t)strlen(t));
}
static ListHeader *rfa(const char *p, const char *t) {
    return (ListHeader *)__ry_regex_find_all(p, (int64_t)strlen(p),
                                             t, (int64_t)strlen(t));
}

static std::string regexLastError() {
    const char *err = __ry_regex_get_last_error();
    std::string msg = err ? err : "";
    if (err) freeStringSlot(const_cast<char *>(err));
    return msg;
}

// ============================================================
// regex_match tests
// ============================================================

TEST(RegexRuntime, MatchLiteral) {
    EXPECT_EQ(rm("hello", "hello"), 1);
    EXPECT_EQ(rm("hello", "world"), 0);
}

TEST(RegexRuntime, MatchConcat) {
    EXPECT_EQ(rm("ab", "ab"), 1);
    EXPECT_EQ(rm("ab", "a"), 0);
    EXPECT_EQ(rm("ab", "abc"), 0);
}

TEST(RegexRuntime, MatchAlternation) {
    EXPECT_EQ(rm("cat|dog", "cat"), 1);
    EXPECT_EQ(rm("cat|dog", "dog"), 1);
    EXPECT_EQ(rm("cat|dog", "fish"), 0);
}

TEST(RegexRuntime, MatchStar) {
    EXPECT_EQ(rm("a*", ""), 1);
    EXPECT_EQ(rm("a*", "a"), 1);
    EXPECT_EQ(rm("a*", "aaa"), 1);
    EXPECT_EQ(rm("a*", "b"), 0);
    EXPECT_EQ(rm("ab*c", "ac"), 1);
    EXPECT_EQ(rm("ab*c", "abc"), 1);
    EXPECT_EQ(rm("ab*c", "abbbc"), 1);
}

TEST(RegexRuntime, MatchPlus) {
    EXPECT_EQ(rm("a+", ""), 0);
    EXPECT_EQ(rm("a+", "a"), 1);
    EXPECT_EQ(rm("a+", "aaa"), 1);
}

TEST(RegexRuntime, MatchQuestion) {
    EXPECT_EQ(rm("a?", ""), 1);
    EXPECT_EQ(rm("a?", "a"), 1);
    EXPECT_EQ(rm("a?b", "b"), 1);
    EXPECT_EQ(rm("a?b", "ab"), 1);
    EXPECT_EQ(rm("a?b", "aab"), 0);
}

TEST(RegexRuntime, MatchDot) {
    EXPECT_EQ(rm(".", "a"), 1);
    EXPECT_EQ(rm(".", "Z"), 1);
    EXPECT_EQ(rm(".", ""), 0);
    EXPECT_EQ(rm("..", "ab"), 1);
    EXPECT_EQ(rm("a.c", "abc"), 1);
    EXPECT_EQ(rm("a.c", "aXc"), 1);
}

TEST(RegexRuntime, MatchCharClass) {
    EXPECT_EQ(rm("[abc]", "a"), 1);
    EXPECT_EQ(rm("[abc]", "b"), 1);
    EXPECT_EQ(rm("[abc]", "d"), 0);
    EXPECT_EQ(rm("[a-z]", "m"), 1);
    EXPECT_EQ(rm("[a-z]", "M"), 0);
    EXPECT_EQ(rm("[a-z]+", "hello"), 1);
}

TEST(RegexRuntime, MatchCharClassNegated) {
    EXPECT_EQ(rm("[^0-9]", "a"), 1);
    EXPECT_EQ(rm("[^0-9]", "5"), 0);
    EXPECT_EQ(rm("[^abc]+", "xyz"), 1);
}

TEST(RegexRuntime, MatchGroup) {
    EXPECT_EQ(rm("(ab)+", "ab"), 1);
    EXPECT_EQ(rm("(ab)+", "abab"), 1);
    EXPECT_EQ(rm("(ab)+", "a"), 0);
    EXPECT_EQ(rm("(a|b)*", "abba"), 1);
}

TEST(RegexRuntime, MatchAnchors) {
    EXPECT_EQ(rm("^hello$", "hello"), 1);
    EXPECT_EQ(rm("^hello$", "hello world"), 0);
    EXPECT_EQ(rm("^a.*z$", "abcz"), 1);
}

TEST(RegexRuntime, MatchEmpty) {
    EXPECT_EQ(rm("", ""), 1);
    EXPECT_EQ(rm("", "a"), 0);
}

TEST(RegexRuntime, MatchComplex) {
    EXPECT_EQ(rm("[a-zA-Z_][a-zA-Z0-9_]*", "hello_world"), 1);
    EXPECT_EQ(rm("[a-zA-Z_][a-zA-Z0-9_]*", "_foo123"), 1);
    EXPECT_EQ(rm("[a-zA-Z_][a-zA-Z0-9_]*", "123abc"), 0);
}

TEST(RegexRuntime, MatchEscapedChars) {
    EXPECT_EQ(rm("a\\.b", "a.b"), 1);
    EXPECT_EQ(rm("a\\.b", "axb"), 0);
    EXPECT_EQ(rm("a\\*b", "a*b"), 1);
}

TEST(RegexRuntime, MatchShorthandClasses) {
    EXPECT_EQ(rm("\\d+", "123"), 1);
    EXPECT_EQ(rm("\\d+", "abc"), 0);
    EXPECT_EQ(rm("\\w+", "hello_123"), 1);
    EXPECT_EQ(rm("\\s+", "  \t"), 1);
    EXPECT_EQ(rm("\\D+", "abc"), 1);
    EXPECT_EQ(rm("\\D+", "123"), 0);
}

// ============================================================
// regex_search tests
// ============================================================

TEST(RegexRuntime, SearchBasic) {
    EXPECT_EQ(rs("world", "hello world"), 6);
    EXPECT_EQ(rs("xyz", "hello world"), -1);
}

TEST(RegexRuntime, SearchPattern) {
    EXPECT_EQ(rs("[0-9]+", "abc123def"), 3);
    EXPECT_EQ(rs("\\d+", "abc123def"), 3);
}

TEST(RegexRuntime, SearchAtStart) {
    EXPECT_EQ(rs("hello", "hello world"), 0);
}

TEST(RegexRuntime, SearchNotFound) {
    EXPECT_EQ(rs("xyz", "abc"), -1);
}

TEST(RegexRuntime, InvalidPatternReturnsRecoverableErrorForMatch) {
    EXPECT_EQ(rm("[", "abc"), -1);
    EXPECT_EQ(regexLastError(), "regex error: unmatched '[' in pattern '['");
}

TEST(RegexRuntime, InvalidPatternReturnsRecoverableErrorForSearch) {
    EXPECT_EQ(rs("(", "abc"), std::numeric_limits<int64_t>::min());
    EXPECT_EQ(regexLastError(), "regex error: unmatched '(' in pattern '('");
}

// ============================================================
// regex_replace tests
// ============================================================

TEST(RegexRuntime, ReplaceBasic) {
    const char *result = rr("world", "hello world", "universe");
    EXPECT_STREQ(result, "hello universe");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, ReplaceAll) {
    const char *result = rr("[0-9]+", "a1b2c3", "X");
    EXPECT_STREQ(result, "aXbXcX");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, ReplaceNoMatch) {
    const char *result = rr("xyz", "hello", "abc");
    EXPECT_STREQ(result, "hello");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, ReplaceEmpty) {
    const char *result = rr("x", "xxx", "");
    EXPECT_STREQ(result, "");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, InvalidPatternReturnsNullForReplace) {
    EXPECT_EQ(rr("(", "abc", "x"), nullptr);
    EXPECT_EQ(regexLastError(), "regex error: unmatched '(' in pattern '('");
}

// ============================================================
// Capture group backreference tests (#829)
// ============================================================

TEST(RegexRuntime, ReplaceCaptureFastPath) {
    // No backreferences → existing fast path, groups have no effect
    const char *r = rr("(\\d+)", "a1b2", "X");
    EXPECT_STREQ(r, "aXbX");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureWholeMatch) {
    // $0 expands to the whole match
    const char *r = rr("\\w+", "hello world", "[$0]");
    EXPECT_STREQ(r, "[hello] [world]");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureGroup1) {
    // $1 expands to first capture group
    const char *r = rr("(\\w+)", "hello world", "[$1]");
    EXPECT_STREQ(r, "[hello] [world]");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureSwapGroups) {
    // Swap two captured words: $2 $1
    const char *r = rr("(\\w+)@(\\w+)", "user@host", "$2@$1");
    EXPECT_STREQ(r, "host@user");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, InvalidPatternReturnsNullForSplitAndFindAll) {
    EXPECT_EQ(rsp("[", "abc"), nullptr);
    EXPECT_EQ(regexLastError(), "regex error: unmatched '[' in pattern '['");
    EXPECT_EQ(rfa("\\", "abc"), nullptr);
    EXPECT_EQ(regexLastError(), "regex error: trailing backslash in pattern '\\'");
}

TEST(RegexRuntime, ReplaceCaptureThreeGroups) {
    // Date reformat: YYYY-MM-DD → DD/MM/YYYY
    const char *r = rr("(\\d+)-(\\d+)-(\\d+)", "2026-04-10", "$3/$2/$1");
    EXPECT_STREQ(r, "10/04/2026");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureMultipleMatches) {
    // Multiple matches, each gets its own capture extraction
    const char *r = rr("(\\w)(\\d)", "a1 b2 c3", "$2$1");
    EXPECT_STREQ(r, "1a 2b 3c");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureLiteralDollar) {
    // $$ → literal $
    const char *r = rr("(\\d+)", "price 100", "$$$1");
    EXPECT_STREQ(r, "price $100");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureOutOfRange) {
    // $2 when only 1 group → empty string
    const char *r = rr("(a)", "a", "$2");
    EXPECT_STREQ(r, "");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureNoGroupsWithDollar0) {
    // $0 works even with no capture groups (whole match)
    const char *r = rr("\\d+", "num 42 num", "($0)");
    EXPECT_STREQ(r, "num (42) num");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureDollarNoDigit) {
    // $ not followed by digit → literal $
    const char *r = rr("a", "abc", "$ b");
    EXPECT_STREQ(r, "$ bbc");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceCaptureMultiDigitBrace) {
    // ${N} syntax for groups beyond $9
    // Build pattern with 10 groups: (a)(b)(c)(d)(e)(f)(g)(h)(i)(j)
    const char *r = rr(
        "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)",
        "abcdefghij",
        "${10}${9}");
    EXPECT_STREQ(r, "ji");
    freeStringSlot(const_cast<char *>(r));
}

TEST(RegexRuntime, ReplaceMalformedBrace) {
    // Malformed ${...} tokens must NOT trigger the capture backtracker
    // and must be emitted literally in the output.
    const char *r1 = rr("(a)", "a", "${foo}");
    EXPECT_STREQ(r1, "${foo}");  // non-digit content: literal
    freeStringSlot(const_cast<char *>(r1));

    const char *r2 = rr("(a)", "a", "${}");
    EXPECT_STREQ(r2, "${}");   // empty braces: literal
    freeStringSlot(const_cast<char *>(r2));
}

// ============================================================
// regex_split tests
// ============================================================

TEST(RegexRuntime, SplitBasic) {
    auto *list = rsp(",", "a,b,c");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "a");
    EXPECT_STREQ(list->data[1], "b");
    EXPECT_STREQ(list->data[2], "c");
    freeStringList(list);
}

TEST(RegexRuntime, SplitPattern) {
    auto *list = rsp("\\s+", "hello  world\tfoo");
    ASSERT_EQ(list->len, 3);
    EXPECT_STREQ(list->data[0], "hello");
    EXPECT_STREQ(list->data[1], "world");
    EXPECT_STREQ(list->data[2], "foo");
    freeStringList(list);
}

TEST(RegexRuntime, SplitNoMatch) {
    auto *list = rsp(",", "hello");
    ASSERT_EQ(list->len, 1);
    EXPECT_STREQ(list->data[0], "hello");
    freeStringList(list);
}

// ============================================================
// regex_find_all tests
// ============================================================

TEST(RegexRuntime, FindAllBasic) {
    auto *list = rfa("[0-9]+", "a1b23c456");
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
    auto *list = rfa("[a-z]+", "hello world foo");
    ASSERT_EQ(list->len, 3);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "hello");
    EXPECT_STREQ(e[1].full, "world");
    EXPECT_STREQ(e[2].full, "foo");
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllNoMatch) {
    auto *list = rfa("[0-9]+", "hello");
    ASSERT_EQ(list->len, 0);
    freeMatchList(list);
}

TEST(RegexRuntime, FindAllWithCaptureGroups) {
    auto *list = rfa("(\\w+)@(\\w+)", "a@b x@y");
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
    auto *list = rfa("(a)?b", "b");
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
    EXPECT_EQ(rm("a{3}", "aaa"), 1);
    EXPECT_EQ(rm("a{3}", "aa"), 0);
    EXPECT_EQ(rm("a{3}", "aaaa"), 0);
}

TEST(RegexRuntime, QuantifierRange) {
    EXPECT_EQ(rm("a{2,4}", "a"), 0);
    EXPECT_EQ(rm("a{2,4}", "aa"), 1);
    EXPECT_EQ(rm("a{2,4}", "aaa"), 1);
    EXPECT_EQ(rm("a{2,4}", "aaaa"), 1);
    EXPECT_EQ(rm("a{2,4}", "aaaaa"), 0);
}

TEST(RegexRuntime, QuantifierMinOnly) {
    EXPECT_EQ(rm("a{2,}", "a"), 0);
    EXPECT_EQ(rm("a{2,}", "aa"), 1);
    EXPECT_EQ(rm("a{2,}", "aaaaaaa"), 1);
}

TEST(RegexRuntime, QuantifierWithGroup) {
    EXPECT_EQ(rm("(ab){2}", "abab"), 1);
    EXPECT_EQ(rm("(ab){2}", "ab"), 0);
    EXPECT_EQ(rm("(ab){2,3}", "ababab"), 1);
}

TEST(RegexRuntime, QuantifierWithCharClass) {
    // Phone number pattern: \d{3}-\d{4}
    EXPECT_EQ(rm("\\d{3}-\\d{4}", "123-4567"), 1);
    EXPECT_EQ(rm("\\d{3}-\\d{4}", "12-4567"), 0);
}

TEST(RegexRuntime, QuantifierEdgeCases) {
    EXPECT_EQ(rm("a{0}", ""), 1);
    EXPECT_EQ(rm("a{1}", "a"), 1);
    EXPECT_EQ(rm("a{1}", ""), 0);
    EXPECT_EQ(rm("a{0,}", ""), 1);   // same as a*
    EXPECT_EQ(rm("a{0,}", "aaa"), 1); // same as a*
    EXPECT_EQ(rm("a{1,}", ""), 0);    // same as a+
    EXPECT_EQ(rm("a{0,1}", ""), 1);   // same as a?
    EXPECT_EQ(rm("a{0,1}", "a"), 1);  // same as a?
}

TEST(RegexRuntime, QuantifierBraceLiteralFallback) {
    // Explicitly escaped literal brace
    EXPECT_EQ(rm("\\{abc\\}", "{abc}"), 1);

    // Invalid brace pattern should be treated as literal '{'
    EXPECT_EQ(rm("a{,}b", "a{,}b"), 1);
}

TEST(RegexRuntime, QuantifierFindAll) {
    auto *list = rfa("\\d{2,3}", "1 23 456 7890");
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
    const char *greedy = rr("\".*\"", "\"a\" and \"b\"", "X");
    EXPECT_STREQ(greedy, "X");
    freeStringSlot(const_cast<char *>(greedy));

    // Lazy: ".*?" matches the shortest string between quotes
    const char *lazy = rr("\".*?\"", "\"a\" and \"b\"", "X");
    EXPECT_STREQ(lazy, "X and X");
    freeStringSlot(const_cast<char *>(lazy));
}

TEST(RegexRuntime, LazyPlusSearch) {
    // a+? should when single 'a' (shortest)
    EXPECT_EQ(rs("a+?", "aaa"), 0);
    // Verify it matched just 1 character by using replace
    const char *result = rr("a+?", "aaa", "X");
    EXPECT_STREQ(result, "XXX");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, LazyQuestion) {
    // a?? prefers matching 0 'a's (non-greedy)
    const char *result = rr("a??", "aaa", "X");
    // a?? matches empty string before each char and after last
    EXPECT_STREQ(result, "XaXaXaX");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, LazyQuantifierBrace) {
    // a{2,4}? prefers matching 2 'a's (minimum)
    const char *result = rr("a{2,4}?", "aaaa", "X");
    EXPECT_STREQ(result, "XX");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, LazyFullMatch) {
    // fullMatch always matches entire string regardless of greedy/lazy
    EXPECT_EQ(rm("a+?", "aaa"), 1);
    EXPECT_EQ(rm("a*?", "aaa"), 1);
    EXPECT_EQ(rm("a{2,4}?", "aaa"), 1);
}

TEST(RegexRuntime, LazyPracticalExample) {
    // Replace individual quoted strings
    const char *result = rr("\"[^\"]*\"", "say \"hello\" and \"world\"", "X");
    EXPECT_STREQ(result, "say X and X");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, LazyFindAll) {
    // .*? in findAll should find shortest matches
    auto *list = rfa("<.*?>", "<a> <bb> <ccc>");
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
    EXPECT_EQ(rs("\\bworld\\b", "hello world"), 6);
    // \bword\b does NOT when in "helloworld" (no boundary)
    EXPECT_EQ(rs("\\bword\\b", "helloworld"), -1);
}

TEST(RegexRuntime, WordBoundaryNonBoundary) {
    // \Bword matches in "helloworld" (non-boundary before 'w')
    EXPECT_EQ(rs("\\Bworld", "helloworld"), 5);
    // \Bworld should NOT when at start of "world test"
    EXPECT_EQ(rs("\\Bworld", "world test"), -1);
}

TEST(RegexRuntime, WordBoundaryStartEnd) {
    // \b at start of string
    EXPECT_EQ(rs("\\bhello", "hello world"), 0);
    // \b at end of string
    EXPECT_EQ(rs("world\\b", "hello world"), 6);
}

TEST(RegexRuntime, WordBoundaryDigitUnderscore) {
    // digits and underscores are word characters
    EXPECT_EQ(rm("\\b\\w+\\b", "hello_123"), 1);
    EXPECT_EQ(rs("\\b123\\b", "abc 123 def"), 4);
    EXPECT_EQ(rs("\\b_foo\\b", "x _foo y"), 2);
}

TEST(RegexRuntime, WordBoundaryFullMatch) {
    EXPECT_EQ(rm("\\btest\\b", "test"), 1);
    EXPECT_EQ(rm("\\btest\\b", "testing"), 0);
}

TEST(RegexRuntime, WordBoundaryFindAll) {
    auto *list = rfa("\\b\\w+\\b", "hello world foo");
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
    EXPECT_EQ(rm("(?i)hello", "HELLO"), 1);
    EXPECT_EQ(rm("(?i)hello", "Hello"), 1);
    EXPECT_EQ(rm("(?i)hello", "hello"), 1);
    EXPECT_EQ(rm("(?i)hello", "hElLo"), 1);
}

TEST(RegexRuntime, CaseInsensitiveCharClass) {
    EXPECT_EQ(rm("(?i)[a-z]+", "ABC"), 1);
    EXPECT_EQ(rm("(?i)[a-z]+", "AbCdE"), 1);
    EXPECT_EQ(rm("(?i)[a-f]+", "ABCDEF"), 1);
    EXPECT_EQ(rm("(?i)[a-f]+", "abcdef"), 1);
}

TEST(RegexRuntime, CaseSensitiveDefault) {
    // Without (?i), should be case-sensitive
    EXPECT_EQ(rm("hello", "HELLO"), 0);
    EXPECT_EQ(rm("[a-z]+", "ABC"), 0);
}

TEST(RegexRuntime, CaseInsensitiveSearch) {
    EXPECT_EQ(rs("(?i)world", "Hello WORLD"), 6);
    EXPECT_EQ(rs("(?i)\\bworld\\b", "Hello WORLD"), 6);
}

TEST(RegexRuntime, CaseInsensitiveReplace) {
    const char *result = rr("(?i)hello", "Hello HELLO hello", "X");
    EXPECT_STREQ(result, "X X X");
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexRuntime, CaseInsensitiveFindAll) {
    auto *list = rfa("(?i)hello", "Hello HELLO hello");
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
    int64_t result = __ry_regex_search("a", 1, text.c_str(), (int64_t)text.size());
    auto elapsed = std::chrono::steady_clock::now() - start;
    EXPECT_EQ(result, -1);
    EXPECT_LT(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count(), 1000);
}

TEST(RegexRuntime, PerfSearchDotStarNonMatch) {
    // Pattern ".*x" on 10000 'a's: worst case for naive approach
    std::string text(10000, 'a');
    auto start = std::chrono::steady_clock::now();
    int64_t result = __ry_regex_search(".*x", 3, text.c_str(), (int64_t)text.size());
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
    auto *list = (ListHeader *)__ry_regex_find_all(
        "[a-z]+", 6, text.c_str(), (int64_t)text.size());
    auto elapsed = std::chrono::steady_clock::now() - start;
    EXPECT_GT(list->len, 0);
    EXPECT_LT(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count(), 1000);
    freeMatchList(list);
}

// Regression test: lazy search must preserve leftmost-start semantics
TEST(RegexRuntime, LazySearchLeftmostStart) {
    // Pattern "a.+?b|c" on "acb": the 'c' alternative matches at pos 1,
    // but "a.+?b" matches starting at pos 0 (leftmost wins).
    EXPECT_EQ(rs("a.+?b|c", "acb"), 0);

    // Pattern "a.+?x|b" on "bax": 'b' matches at pos 0 (leftmost)
    EXPECT_EQ(rs("a.+?x|b", "bax"), 0);
}

TEST(RegexRuntime, LazyFindAllLeftmostStart) {
    // findAll must also respect leftmost-start ordering
    auto *list = rfa("a.+?b|c", "acb");
    ASSERT_EQ(list->len, 1);
    auto *e = (MatchEntry *)list->data;
    EXPECT_STREQ(e[0].full, "acb");
    freeMatchList(list);
}

// --- ReDoS protection tests ---

TEST(RegexSecurity, ModerateGroupNestingSucceeds) {
    // 10 nested groups should work fine
    std::string pattern = std::string(10, '(') + "a" + std::string(10, ')');
    EXPECT_TRUE(rm(pattern.c_str(), "a"));
}

TEST(RegexSecurity, NormalPatternWithStepLimit) {
    // Normal patterns should complete well within the step limit
    EXPECT_TRUE(rm("(a+)(b+)(c+)", "aaabbbccc"));
}

// ============================================================
// NUL byte safety tests (#1052)
// ============================================================

TEST(RegexNul, MatchWithNulSubject) {
    // "a.b" should match "a\0b" because '.' matches any byte including NUL
    const char text[] = {'a', '\0', 'b'};
    EXPECT_EQ(__ry_regex_match("a.b", 3, text, 3), 1);
    // Exact literal match of NUL byte in pattern (passed as raw bytes)
    const char pat[] = {'a', '\0', 'b'};
    EXPECT_EQ(__ry_regex_match(pat, 3, text, 3), 1);
}

TEST(RegexNul, SearchWithNulSubject) {
    // Pattern "X" should be found at index 3 in "a\0bXc"
    const char text[] = {'a', '\0', 'b', 'X', 'c'};
    EXPECT_EQ(__ry_regex_search("X", 1, text, 5), 3);
}

TEST(RegexNul, ReplaceSubjectWithNul) {
    // Replace "b" in "a\0b\0c" → "a\0Z\0c"; result must preserve NUL bytes
    const char text[] = {'a', '\0', 'b', '\0', 'c'};
    const char *result = __ry_regex_replace("b", 1, text, 5, "Z", 1);
    ASSERT_NE(result, nullptr);
    // Byte-level comparison: "a\0Z\0c"
    const char expected[] = {'a', '\0', 'Z', '\0', 'c'};
    EXPECT_EQ(memcmp(result - 8 + 8, result, 1), 0); // smoke: handle is valid
    // Read byte_len from StringHeader (handle - 8)
    int64_t blen = 0;
    memcpy(&blen, result - 8, sizeof(int64_t));
    EXPECT_EQ(blen, 5);
    EXPECT_EQ(memcmp(result, expected, 5), 0);
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexNul, ReplaceReplacementWithNul) {
    // Replace "X" in "aXb" with "\0\0"; result must be "a\0\0b" (byte_len=4)
    const char repl[] = {'\0', '\0'};
    const char *result = __ry_regex_replace("X", 1, "aXb", 3, repl, 2);
    ASSERT_NE(result, nullptr);
    int64_t blen = 0;
    memcpy(&blen, result - 8, sizeof(int64_t));
    EXPECT_EQ(blen, 4);
    const char expected[] = {'a', '\0', '\0', 'b'};
    EXPECT_EQ(memcmp(result, expected, 4), 0);
    freeStringSlot(const_cast<char *>(result));
}

TEST(RegexNul, SplitSubjectWithNul) {
    // Split "a\0b,c\0d" on "," → 2 elements: "a\0b" and "c\0d"
    const char text[] = {'a', '\0', 'b', ',', 'c', '\0', 'd'};
    auto *list = (ListHeader *)__ry_regex_split(",", 1, text, 7);
    ASSERT_EQ(list->len, 2);
    int64_t b0 = 0, b1 = 0;
    memcpy(&b0, list->data[0] - 8, sizeof(int64_t));
    memcpy(&b1, list->data[1] - 8, sizeof(int64_t));
    EXPECT_EQ(b0, 3);
    EXPECT_EQ(b1, 3);
    const char seg0[] = {'a', '\0', 'b'};
    const char seg1[] = {'c', '\0', 'd'};
    EXPECT_EQ(memcmp(list->data[0], seg0, 3), 0);
    EXPECT_EQ(memcmp(list->data[1], seg1, 3), 0);
    freeStringList(list);
}

TEST(RegexNul, FindAllSubjectWithNul) {
    // "." should match every byte (including NUL) in "a\0b" → 3 matches
    const char text[] = {'a', '\0', 'b'};
    auto *list = (ListHeader *)__ry_regex_find_all(".", 1, text, 3);
    EXPECT_EQ(list->len, 3);
    freeMatchList(list);
}

TEST(RegexNul, PatternWithNul) {
    // Pattern containing NUL byte: only matches text with the same NUL at
    // the same position.
    const char pat[]  = {'a', '\0', 'b'};
    const char text[] = {'a', '\0', 'b'};
    const char bad[]  = {'a', 'X',  'b'};
    EXPECT_EQ(__ry_regex_match(pat, 3, text, 3), 1);
    EXPECT_EQ(__ry_regex_match(pat, 3, bad,  3), 0);
}

// ============================================================
// __ry_regex_is_match tests — partial (unanchored) match semantics (#1197)
// ============================================================

TEST(RegexRuntime, IsMatchPartial_FoundAnywhere) {
    EXPECT_EQ(rim("[0-9]+", "Hello 123 World"), 1);
    EXPECT_EQ(rim("World",  "Hello 123 World"), 1);
    EXPECT_EQ(rim("Hello",  "Hello 123 World"), 1);
}

TEST(RegexRuntime, IsMatchPartial_FullInputMatch) {
    EXPECT_EQ(rim("[a-z]+", "hello"), 1);
    EXPECT_EQ(rim("[0-9]+", "123"),   1);
}

TEST(RegexRuntime, IsMatchPartial_NoMatch) {
    EXPECT_EQ(rim("[a-z]+", "123"),            0);
    EXPECT_EQ(rim("[0-9]+", "no digits here"), 0);
}

TEST(RegexRuntime, IsMatchPartial_DifferentFromFullMatch) {
    // Key API contract: is_match is partial, regex_match is full-string.
    EXPECT_EQ(rim("[0-9]+", "abc123def"), 1);
    EXPECT_EQ(rm ("[0-9]+", "abc123def"), 0);
}

TEST(RegexRuntime, IsMatchPartial_NullInputs) {
    EXPECT_EQ(__ry_regex_is_match(nullptr, 0, "text", 4), 0);
    EXPECT_EQ(__ry_regex_is_match("[a-z]+", 6, nullptr, 0), 0);
}

TEST(RegexRuntime, IsMatchPartial_NULInSubject) {
    // "a.b" (. matches any byte including NUL) should match "a<NUL>b" anywhere
    const char subj[] = {'X', 'a', '\0', 'b', 'Y'};
    EXPECT_EQ(__ry_regex_is_match("a.b", 3, subj, 5), 1);
}
