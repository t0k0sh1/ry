#include "test_codegen_common.hpp"

// ============================================================
// regex_match
// ============================================================

TEST_F(CodeGenTest, RegexMatchTrue) {
    EXPECT_EQ(runSource(R"(
m = regex_match("[a-z]+", "hello")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexMatchFalse) {
    EXPECT_EQ(runSource(R"(
m = regex_match("[0-9]+", "hello")
print(m)
)"), "false\n");
}

TEST_F(CodeGenTest, RegexMatchInIf) {
    EXPECT_EQ(runSource(R"(
if regex_match("[a-z]+", "hello"):
    print("yes")
else:
    print("no")
)"), "yes\n");
}

// ============================================================
// regex_search
// ============================================================

TEST_F(CodeGenTest, RegexSearchFound) {
    EXPECT_EQ(runSource(R"(
pos = regex_search("[0-9]+", "abc123def")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexSearchNotFound) {
    EXPECT_EQ(runSource(R"(
pos = regex_search("[0-9]+", "hello")
print(pos)
)"), "-1\n");
}

// ============================================================
// regex_replace
// ============================================================

TEST_F(CodeGenTest, RegexReplaceBasic) {
    EXPECT_EQ(runSource(R"(
s = regex_replace("[0-9]+", "a1b2c3", "X")
print(s)
)"), "aXbXcX\n");
}

TEST_F(CodeGenTest, RegexReplaceNoMatch) {
    EXPECT_EQ(runSource(R"(
s = regex_replace("[0-9]+", "hello", "X")
print(s)
)"), "hello\n");
}

// ============================================================
// regex_split
// ============================================================

TEST_F(CodeGenTest, RegexSplitBasic) {
    EXPECT_EQ(runSource(R"(
parts = regex_split(",", "a,b,c")
print(length(parts))
print(parts[0])
print(parts[1])
print(parts[2])
)"), "3\na\nb\nc\n");
}

TEST_F(CodeGenTest, RegexSplitPattern) {
    EXPECT_EQ(runSource(R"(
parts = regex_split("\\s+", "hello  world")
print(length(parts))
print(parts[0])
print(parts[1])
)"), "2\nhello\nworld\n");
}

// ============================================================
// regex_find_all
// ============================================================

TEST_F(CodeGenTest, RegexFindAllBasic) {
    EXPECT_EQ(runSource(R"(
matches = regex_find_all("[0-9]+", "a1b23c456")
print(length(matches))
print(matches[0])
print(matches[1])
print(matches[2])
)"), "3\n1\n23\n456\n");
}

TEST_F(CodeGenTest, RegexFindAllNoMatch) {
    EXPECT_EQ(runSource(R"(
matches = regex_find_all("[0-9]+", "hello")
print(length(matches))
)"), "0\n");
}

// ============================================================
// UFCS: pattern.regex_match(text)
// ============================================================

TEST_F(CodeGenTest, RegexMatchUFCS) {
    EXPECT_EQ(runSource(R"(
m = "[a-z]+".regex_match("hello")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexSearchUFCS) {
    EXPECT_EQ(runSource(R"(
pos = "[0-9]+".regex_search("abc123")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexReplaceUFCS) {
    EXPECT_EQ(runSource(R"(
s = "[0-9]+".regex_replace("a1b2c3", "X")
print(s)
)"), "aXbXcX\n");
}

// ============================================================
// Range quantifiers
// ============================================================

TEST_F(CodeGenTest, RegexQuantifierExact) {
    EXPECT_EQ(runSource(R"(
print(regex_match("\\d{3}", "123"))
print(regex_match("\\d{3}", "12"))
print(regex_match("\\d{3}", "1234"))
)"), "true\nfalse\nfalse\n");
}

// ============================================================
// Non-greedy when
// ============================================================

TEST_F(CodeGenTest, RegexLazyReplace) {
    EXPECT_EQ(runSource(R"(
s = regex_replace("\".*?\"", "\"a\" and \"b\"", "X")
print(s)
)"), "X and X\n");
}

TEST_F(CodeGenTest, RegexLazyFindAll) {
    EXPECT_EQ(runSource(R"(
tags = regex_find_all("<.*?>", "<x> <yy>")
print(length(tags))
print(tags[0])
print(tags[1])
)"), "2\n<x>\n<yy>\n");
}

// ============================================================
// Regex literal syntax /.../
// ============================================================

TEST_F(CodeGenTest, RegexLiteralMatch) {
    EXPECT_EQ(runSource(R"(
print(match("hello", /[a-z]+/))
print(match("123", /[a-z]+/))
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
nums = find_all("a1b2c3", /[0-9]/)
print(length(nums))
print(nums[0])
print(nums[1])
print(nums[2])
)"), "3\n1\n2\n3\n");
}

TEST_F(CodeGenTest, RegexLiteralUFCS) {
    EXPECT_EQ(runSource(R"(
print("hello".match(/[a-z]+/))
print("abc123".search(/[0-9]+/))
nums = "a1b2c3".find_all(/[0-9]/)
print(length(nums))
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
print(match("hello", pat))
print("world".match(pat))
)"), "true\ntrue\n");
}

TEST_F(CodeGenTest, RegexLiteralDivisionCoexist) {
    EXPECT_EQ(runSource(R"(
x = 10 / 2
print(x)
y = 100 // 3
print(y)
)"), "5\n33\n");
}

TEST_F(CodeGenTest, RegexLiteralEscapedSlash) {
    EXPECT_EQ(runSource(R"(
print(match("a/b", /a\/b/))
)"), "true\n");
}

TEST_F(CodeGenTest, RegexLiteralWithPrefixedFunctions) {
    EXPECT_EQ(runSource(R"(
print(regex_match("[a-z]+", "hello"))
print(regex_search("[0-9]+", "abc123"))
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
