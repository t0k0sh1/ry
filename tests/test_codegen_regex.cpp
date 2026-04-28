#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// regexMatch
// ============================================================

TEST_F(CodeGenTest, RegexMatchTrue) {
    EXPECT_EQ(runSource(R"(
m = regexMatch("hello", "[a-z]+")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexMatchFalse) {
    EXPECT_EQ(runSource(R"(
m = regexMatch("hello", "[0-9]+")
print(m)
)"), "false\n");
}

TEST_F(CodeGenTest, RegexMatchInIf) {
    EXPECT_EQ(runSource(R"(
if regexMatch("hello", "[a-z]+"):
    print("yes")
else:
    print("no")
)"), "yes\n");
}

// ============================================================
// regexSearch
// ============================================================

TEST_F(CodeGenTest, RegexSearchFound) {
    EXPECT_EQ(runSource(R"(
pos = regexSearch("abc123def", "[0-9]+")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexSearchNotFound) {
    EXPECT_EQ(runSource(R"(
pos = regexSearch("hello", "[0-9]+")
print(pos)
)"), "-1\n");
}

// ============================================================
// regexReplace
// ============================================================

TEST_F(CodeGenTest, RegexReplaceBasic) {
    EXPECT_EQ(runSource(R"(
s = regexReplace("a1b2c3", "[0-9]+", "X")
print(s)
)"), "aXbXcX\n");
}

TEST_F(CodeGenTest, RegexReplaceNoMatch) {
    EXPECT_EQ(runSource(R"(
s = regexReplace("hello", "[0-9]+", "X")
print(s)
)"), "hello\n");
}

// ============================================================
// regexSplit
// ============================================================

TEST_F(CodeGenTest, RegexSplitBasic) {
    EXPECT_EQ(runSource(R"(
parts = regexSplit("a,b,c", ",")
print(len(parts))
print(parts[0])
print(parts[1])
print(parts[2])
)"), "3\na\nb\nc\n");
}

TEST_F(CodeGenTest, RegexSplitPattern) {
    EXPECT_EQ(runSource(R"(
parts = regexSplit("hello  world", "\\s+")
print(len(parts))
print(parts[0])
print(parts[1])
)"), "2\nhello\nworld\n");
}

// ============================================================
// regexFindAll
// ============================================================

TEST_F(CodeGenTest, RegexFindAllBasic) {
    EXPECT_EQ(runSource(R"(
matches = regexFindAll("a1b23c456", "[0-9]+")
print(len(matches))
print(matches[0].full)
print(matches[1].full)
print(matches[2].full)
)"), "3\n1\n23\n456\n");
}

TEST_F(CodeGenTest, RegexFindAllNoMatch) {
    EXPECT_EQ(runSource(R"(
matches = regexFindAll("hello", "[0-9]+")
print(len(matches))
)"), "0\n");
}

TEST_F(CodeGenTest, RegexSearchInvalidPatternExitsThroughLanguageErrorPath) {
    EXPECT_EXIT(runSource(R"(
print("before")
print(regexSearch("abc", "["))
)"),
                ::testing::ExitedWithCode(1),
                "error: regex error: unmatched '\\[' in pattern '\\['");
}

TEST_F(CodeGenTest, RegexReplaceInvalidPatternExitsThroughLanguageErrorPath) {
    EXPECT_EXIT(runSource(R"(
print(regexReplace("abc", "(", "x"))
)"),
                ::testing::ExitedWithCode(1),
                "error: regex error: unmatched '\\(' in pattern '\\('");
}

TEST_F(CodeGenTest, RegexSplitInvalidPatternExitsThroughLanguageErrorPath) {
    EXPECT_EXIT(runSource(R"(
parts = regexSplit("abc", "[")
print(len(parts))
)"),
                ::testing::ExitedWithCode(1),
                "error: regex error: unmatched '\\[' in pattern '\\['");
}

// ============================================================
// UFCS: text.regexMatch(pattern)
// ============================================================

TEST_F(CodeGenTest, RegexMatchUFCS) {
    EXPECT_EQ(runSource(R"(
m = "hello".regexMatch("[a-z]+")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexSearchUFCS) {
    EXPECT_EQ(runSource(R"(
pos = "abc123".regexSearch("[0-9]+")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexReplaceUFCS) {
    EXPECT_EQ(runSource(R"(
s = "a1b2c3".regexReplace("[0-9]+", "X")
print(s)
)"), "aXbXcX\n");
}

// ============================================================
// Range quantifiers
// ============================================================

TEST_F(CodeGenTest, RegexQuantifierExact) {
    EXPECT_EQ(runSource(R"(
print(regexMatch("123", "\\d{3}"))
print(regexMatch("12", "\\d{3}"))
print(regexMatch("1234", "\\d{3}"))
)"), "true\nfalse\nfalse\n");
}

// ============================================================
// Non-greedy when
// ============================================================

TEST_F(CodeGenTest, RegexLazyReplace) {
    EXPECT_EQ(runSource(R"(
s = regexReplace("\"a\" and \"b\"", "\".*?\"", "X")
print(s)
)"), "X and X\n");
}

TEST_F(CodeGenTest, RegexLazyFindAll) {
    EXPECT_EQ(runSource(R"(
tags = regexFindAll("<x> <yy>", "<.*?>")
print(len(tags))
print(tags[0].full)
print(tags[1].full)
)"), "2\n<x>\n<yy>\n");
}

// ============================================================
// Regex literal syntax /.../
// ============================================================

TEST_F(CodeGenTest, RegexLiteralMatch) {
    EXPECT_EQ(runSource(R"(
print(isMatch("hello", /[a-z]+/))
print(isMatch("123", /[a-z]+/))
)"), "true\nfalse\n");
}

TEST_F(CodeGenTest, RegexLiteralSearch) {
    EXPECT_EQ(runSource(R"(
print(search("abc123", /[0-9]+/))
)"), "3\n");
}

TEST_F(CodeGenTest, RegexLiteralReplace) {
    EXPECT_EQ(runSource(R"(
print(replace("abc123def", /[0-9]+/, "X"))
print(replace("a1b2c3", /[0-9]/, "X"))
)"), "abcXdef\naXbXcX\n");
}

TEST_F(CodeGenTest, RegexLiteralSplit) {
    EXPECT_EQ(runSource(R"(
parts = split("a1b2c", /[0-9]/)
print(parts[0])
print(parts[1])
print(parts[2])
)"), "a\nb\nc\n");
}

TEST_F(CodeGenTest, RegexLiteralFindAll) {
    EXPECT_EQ(runSource(R"(
nums = findAll("a1b2c3", /[0-9]/)
print(len(nums))
print(nums[0].full)
print(nums[1].full)
print(nums[2].full)
)"), "3\n1\n2\n3\n");
}

TEST_F(CodeGenTest, RegexLiteralUFCS) {
    EXPECT_EQ(runSource(R"(
print("hello".isMatch(/[a-z]+/))
print("abc123".search(/[0-9]+/))
nums = "a1b2c3".findAll(/[0-9]/)
print(len(nums))
)"), "true\n3\n3\n");
}

TEST_F(CodeGenTest, RegexLiteralUFCSSplit) {
    EXPECT_EQ(runSource(R"(
parts = "hello world".split(/\s+/)
print(parts[0])
print(parts[1])
)"), "hello\nworld\n");
}

TEST_F(CodeGenTest, RegexLiteralUFCSReplace) {
    EXPECT_EQ(runSource(R"(
print("abc123".replace(/[0-9]+/, "X"))
)"), "abcX\n");
}

TEST_F(CodeGenTest, RegexLiteralVariable) {
    EXPECT_EQ(runSource(R"(
pat = /[a-z]+/
print(isMatch("hello", pat))
print("world".isMatch(pat))
)"), "true\ntrue\n");
}

TEST_F(CodeGenTest, RegexLiteralDivisionCoexist) {
    EXPECT_EQ(runSource(R"(
x = 10 / 2
print(x)
y = 100 // 3
print(y)
)"), "5.0\n33\n");
}

TEST_F(CodeGenTest, RegexLiteralEscapedSlash) {
    EXPECT_EQ(runSource(R"(
print(isMatch("a/b", /a\/b/))
)"), "true\n");
}

TEST_F(CodeGenTest, RegexLiteralNulEscape) {
    EXPECT_EQ(runSource("print(isMatch(\"a\\0b\", /a\\0b/))"), "true\n");
    EXPECT_EQ(runSource("print(isMatch(\"aXb\", /a\\0b/))"), "false\n");
    EXPECT_EQ(runSource("print(isMatch(\"\\0\", /\\0/))"), "true\n");
}

TEST_F(CodeGenTest, RegexLiteralWithPrefixedFunctions) {
    EXPECT_EQ(runSource(R"(
print(regexMatch("hello", "[a-z]+"))
print(regexSearch("abc123", "[0-9]+"))
)"), "true\n3\n");
}

TEST_F(CodeGenTest, RegexLiteralStringSplitStillWorks) {
    EXPECT_EQ(runSource(R"(
parts = split("a,b,c", ",")
print(parts[0])
print(parts[1])
print(parts[2])
)"), "a\nb\nc\n");
}

TEST_F(CodeGenTest, RegexLiteralStringReplaceStillWorks) {
    EXPECT_EQ(runSource(R"(
print(replace("hello world", "world", "ry"))
)"), "hello ry\n");
}
