#include "test_codegen_common.hpp"

// ============================================================
// regex_match
// ============================================================

TEST_F(CodeGenTest, RegexMatchTrue) {
    EXPECT_EQ(runSource(R"(
let m = regex_match("[a-z]+", "hello")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexMatchFalse) {
    EXPECT_EQ(runSource(R"(
let m = regex_match("[0-9]+", "hello")
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
let pos = regex_search("[0-9]+", "abc123def")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexSearchNotFound) {
    EXPECT_EQ(runSource(R"(
let pos = regex_search("[0-9]+", "hello")
print(pos)
)"), "-1\n");
}

// ============================================================
// regex_replace
// ============================================================

TEST_F(CodeGenTest, RegexReplaceBasic) {
    EXPECT_EQ(runSource(R"(
let s = regex_replace("[0-9]+", "a1b2c3", "X")
print(s)
)"), "aXbXcX\n");
}

TEST_F(CodeGenTest, RegexReplaceNoMatch) {
    EXPECT_EQ(runSource(R"(
let s = regex_replace("[0-9]+", "hello", "X")
print(s)
)"), "hello\n");
}

// ============================================================
// regex_split
// ============================================================

TEST_F(CodeGenTest, RegexSplitBasic) {
    EXPECT_EQ(runSource(R"(
let parts = regex_split(",", "a,b,c")
print(len(parts))
print(parts[0])
print(parts[1])
print(parts[2])
)"), "3\na\nb\nc\n");
}

TEST_F(CodeGenTest, RegexSplitPattern) {
    EXPECT_EQ(runSource(R"(
let parts = regex_split("\\s+", "hello  world")
print(len(parts))
print(parts[0])
print(parts[1])
)"), "2\nhello\nworld\n");
}

// ============================================================
// regex_find_all
// ============================================================

TEST_F(CodeGenTest, RegexFindAllBasic) {
    EXPECT_EQ(runSource(R"(
let matches = regex_find_all("[0-9]+", "a1b23c456")
print(len(matches))
print(matches[0])
print(matches[1])
print(matches[2])
)"), "3\n1\n23\n456\n");
}

TEST_F(CodeGenTest, RegexFindAllNoMatch) {
    EXPECT_EQ(runSource(R"(
let matches = regex_find_all("[0-9]+", "hello")
print(len(matches))
)"), "0\n");
}

// ============================================================
// UFCS: pattern.regex_match(text)
// ============================================================

TEST_F(CodeGenTest, RegexMatchUFCS) {
    EXPECT_EQ(runSource(R"(
let m = "[a-z]+".regex_match("hello")
print(m)
)"), "true\n");
}

TEST_F(CodeGenTest, RegexSearchUFCS) {
    EXPECT_EQ(runSource(R"(
let pos = "[0-9]+".regex_search("abc123")
print(pos)
)"), "3\n");
}

TEST_F(CodeGenTest, RegexReplaceUFCS) {
    EXPECT_EQ(runSource(R"(
let s = "[0-9]+".regex_replace("a1b2c3", "X")
print(s)
)"), "aXbXcX\n");
}
