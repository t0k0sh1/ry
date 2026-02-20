#include <gtest/gtest.h>
#include "ry.hpp"

// 全トークンを取得するヘルパー
static std::vector<Token> tokenize(const std::string &src) {
    Lexer lex(src);
    std::vector<Token> tokens;
    for (;;) {
        Token t = lex.next();
        tokens.push_back(t);
        if (t.kind == TokenKind::Eof) break;
    }
    return tokens;
}

TEST(LexerTest, Integer) {
    auto toks = tokenize("42");
    ASSERT_EQ(toks.size(), 2u); // Number + Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "42");
}

TEST(LexerTest, Float) {
    auto toks = tokenize("3.14");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "3.14");
}

TEST(LexerTest, Identifier) {
    // "android" は And キーワードではなく Ident
    auto toks = tokenize("android");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "android");
}

TEST(LexerTest, KeywordAnd) {
    auto toks = tokenize("and");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::And);
}

TEST(LexerTest, KeywordOr) {
    auto toks = tokenize("or");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Or);
}

TEST(LexerTest, KeywordNot) {
    auto toks = tokenize("not");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Not);
}

TEST(LexerTest, IdentStartsWithKeyword) {
    // android / orbit / nothing は全部 Ident
    for (const auto &word : {"android", "orbit", "nothing"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
        EXPECT_EQ(toks[0].value, word) << "word: " << word;
    }
}

TEST(LexerTest, StarVsStarStar) {
    auto toks1 = tokenize("*");
    EXPECT_EQ(toks1[0].kind, TokenKind::Star);

    auto toks2 = tokenize("**");
    EXPECT_EQ(toks2[0].kind, TokenKind::StarStar);
    EXPECT_EQ(toks2[0].value, "**");
}

TEST(LexerTest, SlashVsSlashSlash) {
    auto toks1 = tokenize("/");
    EXPECT_EQ(toks1[0].kind, TokenKind::Slash);

    auto toks2 = tokenize("//");
    EXPECT_EQ(toks2[0].kind, TokenKind::SlashSlash);
    EXPECT_EQ(toks2[0].value, "//");
}

TEST(LexerTest, EqVsEqEq) {
    auto toks1 = tokenize("=");
    EXPECT_EQ(toks1[0].kind, TokenKind::Equals);

    auto toks2 = tokenize("==");
    EXPECT_EQ(toks2[0].kind, TokenKind::EqEq);
    EXPECT_EQ(toks2[0].value, "==");
}

TEST(LexerTest, BangEq) {
    auto toks1 = tokenize("!=");
    EXPECT_EQ(toks1[0].kind, TokenKind::BangEq);
    EXPECT_EQ(toks1[0].value, "!=");

    // 単独の '!' はエラー
    auto toks2 = tokenize("!");
    EXPECT_EQ(toks2[0].kind, TokenKind::Error);
}

TEST(LexerTest, ComparisonOperators) {
    auto toks = tokenize("< <= > >=");
    EXPECT_EQ(toks[0].kind, TokenKind::Less);
    EXPECT_EQ(toks[1].kind, TokenKind::LessEq);
    EXPECT_EQ(toks[2].kind, TokenKind::Greater);
    EXPECT_EQ(toks[3].kind, TokenKind::GreaterEq);
}

TEST(LexerTest, NewlineAndLineNumber) {
    Lexer lex("x\ny");
    Token t1 = lex.next(); // x (line 1)
    EXPECT_EQ(t1.kind, TokenKind::Ident);
    EXPECT_EQ(t1.line, 1);

    Token t2 = lex.next(); // newline (line 1, then line_ becomes 2)
    EXPECT_EQ(t2.kind, TokenKind::Newline);
    EXPECT_EQ(t2.line, 1);

    Token t3 = lex.next(); // y (line 2)
    EXPECT_EQ(t3.kind, TokenKind::Ident);
    EXPECT_EQ(t3.line, 2);
}

TEST(LexerTest, CRLFNewline) {
    // \r\n は1つの Newline トークンになること
    auto toks = tokenize("x\r\ny");
    ASSERT_EQ(toks.size(), 4u); // Ident + Newline + Ident + Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].kind, TokenKind::Newline);
    EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    EXPECT_EQ(toks[3].kind, TokenKind::Eof);
}

TEST(LexerTest, PeekDoesNotConsume) {
    Lexer lex("42");
    Token p1 = lex.peek();
    Token p2 = lex.peek(); // 2回 peek しても同じ
    Token n  = lex.next(); // next で取得

    EXPECT_EQ(p1.kind, TokenKind::Number);
    EXPECT_EQ(p1.value, "42");
    EXPECT_EQ(p2.kind, TokenKind::Number);
    EXPECT_EQ(p2.value, "42");
    EXPECT_EQ(n.kind, TokenKind::Number);
    EXPECT_EQ(n.value, "42");

    // next() の後は Eof
    EXPECT_EQ(lex.peek().kind, TokenKind::Eof);
}

TEST(LexerTest, EmptyInput) {
    auto toks = tokenize("");
    ASSERT_EQ(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::Eof);
}

TEST(LexerTest, WhitespaceOnly) {
    auto toks = tokenize("   \t  ");
    ASSERT_EQ(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::Eof);
}

TEST(LexerTest, UnknownCharIsError) {
    auto toks = tokenize("@");
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, "@");
}
