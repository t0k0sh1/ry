#include <gtest/gtest.h>
#include "ry/lexer.hpp"

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

TEST(LexerTest, KeywordTrue) {
    auto toks = tokenize("true");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::True);
    EXPECT_EQ(toks[0].value, "true");
}

TEST(LexerTest, KeywordFalse) {
    auto toks = tokenize("false");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::False);
    EXPECT_EQ(toks[0].value, "false");
}

TEST(LexerTest, IdentStartsWithKeyword) {
    // android / orbit / nothing は全部 Ident
    for (const auto &word : {"android", "orbit", "nothing", "trueblood", "falsehood"}) {
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
    auto toks = tokenize("$");
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, "$");
}

TEST(LexerTest, AtToken) {
    auto toks = tokenize("@");
    EXPECT_EQ(toks[0].kind, TokenKind::At);
    EXPECT_EQ(toks[0].value, "@");
}

TEST(LexerTest, BitwiseSingleCharTokens) {
    auto toks = tokenize("& | ^ ~");
    EXPECT_EQ(toks[0].kind, TokenKind::Amp);
    EXPECT_EQ(toks[0].value, "&");
    EXPECT_EQ(toks[1].kind, TokenKind::Pipe);
    EXPECT_EQ(toks[1].value, "|");
    EXPECT_EQ(toks[2].kind, TokenKind::Caret);
    EXPECT_EQ(toks[2].value, "^");
    EXPECT_EQ(toks[3].kind, TokenKind::Tilde);
    EXPECT_EQ(toks[3].value, "~");
}

TEST(LexerTest, LessLessVsLessAndLessEq) {
    // << は LessLess
    auto toks1 = tokenize("<<");
    EXPECT_EQ(toks1[0].kind, TokenKind::LessLess);
    EXPECT_EQ(toks1[0].value, "<<");

    // 回帰: < と <= は変わらず
    auto toks2 = tokenize("< <=");
    EXPECT_EQ(toks2[0].kind, TokenKind::Less);
    EXPECT_EQ(toks2[1].kind, TokenKind::LessEq);
}

TEST(LexerTest, GreaterGreaterVsGreaterAndGreaterEq) {
    // >> は GreaterGreater
    auto toks1 = tokenize(">>");
    EXPECT_EQ(toks1[0].kind, TokenKind::GreaterGreater);
    EXPECT_EQ(toks1[0].value, ">>");

    // 回帰: > と >= は変わらず
    auto toks2 = tokenize("> >=");
    EXPECT_EQ(toks2[0].kind, TokenKind::Greater);
    EXPECT_EQ(toks2[1].kind, TokenKind::GreaterEq);
}

TEST(LexerTest, ColonToken) {
    auto toks = tokenize(":");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Colon);
    EXPECT_EQ(toks[0].value, ":");
}

TEST(LexerTest, CommentAtEndOfLine) {
    auto toks = tokenize("x = 10 # comment");
    ASSERT_EQ(toks.size(), 4u); // Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "x");
    EXPECT_EQ(toks[1].kind, TokenKind::Equals);
    EXPECT_EQ(toks[2].kind, TokenKind::Number);
    EXPECT_EQ(toks[2].value, "10");
    EXPECT_EQ(toks[3].kind, TokenKind::Eof);
}

TEST(LexerTest, CommentAtStartOfLine) {
    auto toks = tokenize("# full line comment\nx = 1");
    ASSERT_EQ(toks.size(), 5u); // Newline Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Newline);
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "x");
    EXPECT_EQ(toks[2].kind, TokenKind::Equals);
    EXPECT_EQ(toks[3].kind, TokenKind::Number);
    EXPECT_EQ(toks[3].value, "1");
    EXPECT_EQ(toks[4].kind, TokenKind::Eof);
}

TEST(LexerTest, CommentOnly) {
    auto toks = tokenize("# just a comment");
    ASSERT_EQ(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::Eof);
}

TEST(LexerTest, ConsecutiveCommentLines) {
    auto toks = tokenize("# line 1\n# line 2\nx = 1");
    ASSERT_EQ(toks.size(), 6u); // Newline Newline Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Newline);
    EXPECT_EQ(toks[1].kind, TokenKind::Newline);
    EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    EXPECT_EQ(toks[3].kind, TokenKind::Equals);
    EXPECT_EQ(toks[4].kind, TokenKind::Number);
    EXPECT_EQ(toks[5].kind, TokenKind::Eof);
}

TEST(LexerTest, KeywordLet) {
    auto toks = tokenize("let");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Let);
    EXPECT_EQ(toks[0].value, "let");
}

TEST(LexerTest, KeywordVar) {
    auto toks = tokenize("var");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Var);
    EXPECT_EQ(toks[0].value, "var");
}

TEST(LexerTest, LetterAndVariableAreIdent) {
    for (const auto &word : {"letter", "variable", "letting", "variant"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
        EXPECT_EQ(toks[0].value, word) << "word: " << word;
    }
}

TEST(LexerTest, TypeAnnotationTokens) {
    auto toks = tokenize("a: int = 10");
    ASSERT_EQ(toks.size(), 6u); // Ident Colon Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "a");
    EXPECT_EQ(toks[1].kind, TokenKind::Colon);
    EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    EXPECT_EQ(toks[2].value, "int");
    EXPECT_EQ(toks[3].kind, TokenKind::Equals);
    EXPECT_EQ(toks[4].kind, TokenKind::Number);
    EXPECT_EQ(toks[4].value, "10");
}

// ===== if/elif/else キーワード =====

TEST(LexerTest, KeywordIf) {
    auto toks = tokenize("if");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::If);
    EXPECT_EQ(toks[0].value, "if");
}

TEST(LexerTest, KeywordElif) {
    auto toks = tokenize("elif");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Elif);
    EXPECT_EQ(toks[0].value, "elif");
}

TEST(LexerTest, KeywordElse) {
    auto toks = tokenize("else");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Else);
    EXPECT_EQ(toks[0].value, "else");
}

TEST(LexerTest, IfElifElseAreNotIdent) {
    for (const auto &word : {"iffy", "elsewhere", "elbow", "iffier"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
}

// ===== INDENT/DEDENT =====

TEST(LexerTest, IndentDedentBasic) {
    // "if x:\n    y\nz"
    auto toks = tokenize("if x:\n    y\nz");
    // If Ident(x) Colon Newline Indent Ident(y) Newline Dedent Ident(z) Eof
    ASSERT_GE(toks.size(), 10u);
    EXPECT_EQ(toks[0].kind, TokenKind::If);
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[2].kind, TokenKind::Colon);
    EXPECT_EQ(toks[3].kind, TokenKind::Newline);
    EXPECT_EQ(toks[4].kind, TokenKind::Indent);
    EXPECT_EQ(toks[5].kind, TokenKind::Ident);
    EXPECT_EQ(toks[5].value, "y");
    EXPECT_EQ(toks[6].kind, TokenKind::Newline);
    EXPECT_EQ(toks[7].kind, TokenKind::Dedent);
    EXPECT_EQ(toks[8].kind, TokenKind::Ident);
    EXPECT_EQ(toks[8].value, "z");
    EXPECT_EQ(toks[9].kind, TokenKind::Eof);
}

TEST(LexerTest, MultipleDedents) {
    // 2段インデントからの復帰
    auto toks = tokenize("a:\n    b:\n        c\nd");
    // a Colon Newline Indent b Colon Newline Indent Ident(c) Newline Dedent Dedent Ident(d) Eof
    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Dedent, TokenKind::Dedent, TokenKind::Ident, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

TEST(LexerTest, DedentAtEof) {
    // EOF時に残ったインデントスタック分のDEDENTが生成される
    auto toks = tokenize("a:\n    b");
    // Ident(a) Colon Newline Indent Ident(b) Dedent Eof
    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident,
        TokenKind::Dedent, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

TEST(LexerTest, BlankLineDoesNotChangeIndent) {
    // 空行はインデントに影響しない
    auto toks = tokenize("a:\n    b\n\n    c\nd");
    // Ident Colon Newline Indent Ident(b) Newline Newline Ident(c) Newline Dedent Ident(d) Eof
    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Newline, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Dedent, TokenKind::Ident, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

// ===== while キーワード =====

TEST(LexerTest, KeywordWhile) {
    auto toks = tokenize("while");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::While);
    EXPECT_EQ(toks[0].value, "while");
}

TEST(LexerTest, WhilingIsIdent) {
    auto toks = tokenize("whiling");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "whiling");
}

// ===== fn / return / -> トークン =====

TEST(LexerTest, KeywordFn) {
    auto toks = tokenize("fn");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Fn);
    EXPECT_EQ(toks[0].value, "fn");
}

TEST(LexerTest, KeywordReturn) {
    auto toks = tokenize("return");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Return);
    EXPECT_EQ(toks[0].value, "return");
}

TEST(LexerTest, FnordAndReturningAreIdent) {
    for (const auto &word : {"fnord", "returning"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
}

TEST(LexerTest, ArrowToken) {
    auto toks = tokenize("->");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Arrow);
    EXPECT_EQ(toks[0].value, "->");
}

TEST(LexerTest, MinusStillWorks) {
    auto toks = tokenize("-");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Minus);
    EXPECT_EQ(toks[0].value, "-");
}

TEST(LexerTest, MinusNumberNotArrow) {
    auto toks = tokenize("-5");
    ASSERT_EQ(toks.size(), 3u);
    EXPECT_EQ(toks[0].kind, TokenKind::Minus);
    EXPECT_EQ(toks[1].kind, TokenKind::Number);
    EXPECT_EQ(toks[1].value, "5");
}

// ===== from / import / dot トークン =====

TEST(LexerTest, KeywordFrom) {
    auto toks = tokenize("from");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::From);
    EXPECT_EQ(toks[0].value, "from");
}

TEST(LexerTest, KeywordImport) {
    auto toks = tokenize("import");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Import);
    EXPECT_EQ(toks[0].value, "import");
}

TEST(LexerTest, DotToken) {
    auto toks = tokenize(".");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Dot);
    EXPECT_EQ(toks[0].value, ".");
}

TEST(LexerTest, FromImportTokenSequence) {
    auto toks = tokenize("from math import add");
    ASSERT_EQ(toks.size(), 5u); // From Ident Import Ident Eof
    EXPECT_EQ(toks[0].kind, TokenKind::From);
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "math");
    EXPECT_EQ(toks[2].kind, TokenKind::Import);
    EXPECT_EQ(toks[3].kind, TokenKind::Ident);
    EXPECT_EQ(toks[3].value, "add");
    EXPECT_EQ(toks[4].kind, TokenKind::Eof);
}

TEST(LexerTest, FromDotPathTokenSequence) {
    auto toks = tokenize("from utils.math import add");
    ASSERT_EQ(toks.size(), 7u); // From Ident Dot Ident Import Ident Eof
    EXPECT_EQ(toks[0].kind, TokenKind::From);
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "utils");
    EXPECT_EQ(toks[2].kind, TokenKind::Dot);
    EXPECT_EQ(toks[3].kind, TokenKind::Ident);
    EXPECT_EQ(toks[3].value, "math");
    EXPECT_EQ(toks[4].kind, TokenKind::Import);
    EXPECT_EQ(toks[5].kind, TokenKind::Ident);
    EXPECT_EQ(toks[5].value, "add");
    EXPECT_EQ(toks[6].kind, TokenKind::Eof);
}

TEST(LexerTest, FromAndImportPrefixAreIdent) {
    for (const auto &word : {"fromage", "imported", "frothy"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
}

// ===== 文字列リテラル =====

TEST(LexerTest, StringLiteral) {
    auto toks = tokenize("\"hello\"");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "hello");
}

TEST(LexerTest, EmptyString) {
    auto toks = tokenize("\"\"");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "");
}

TEST(LexerTest, UnterminatedStringThrows) {
    EXPECT_THROW(tokenize("\"hello"), std::runtime_error);
}

TEST(LexerTest, UnterminatedStringNewlineThrows) {
    EXPECT_THROW(tokenize("\"hello\nworld\""), std::runtime_error);
}

TEST(LexerTest, StringInExpression) {
    auto toks = tokenize("let s = \"world\"");
    ASSERT_EQ(toks.size(), 5u); // Let Ident Equals String Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Let);
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "s");
    EXPECT_EQ(toks[2].kind, TokenKind::Equals);
    EXPECT_EQ(toks[3].kind, TokenKind::String);
    EXPECT_EQ(toks[3].value, "world");
    EXPECT_EQ(toks[4].kind, TokenKind::Eof);
}

TEST(LexerTest, KeywordType) {
    auto toks = tokenize("type");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Type);
    EXPECT_EQ(toks[0].value, "type");
}

TEST(LexerTest, TypePrefixIsIdent) {
    for (const auto &word : {"typedef", "types", "typer"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
}

TEST(LexerTest, CommentLineDoesNotChangeIndent) {
    auto toks = tokenize("a:\n    b\n    # comment\n    c\nd");
    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Newline, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Dedent, TokenKind::Ident, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

// ===== Map: LBrace/RBrace トークンテスト =====

TEST(LexerTest, LBrace) {
    auto toks = tokenize("{");
    ASSERT_GE(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::LBrace);
    EXPECT_EQ(toks[0].value, "{");
}

TEST(LexerTest, RBrace) {
    auto toks = tokenize("}");
    ASSERT_GE(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::RBrace);
    EXPECT_EQ(toks[0].value, "}");
}

TEST(LexerTest, MapLiteralTokens) {
    auto toks = tokenize("{\"a\": 1}");
    std::vector<TokenKind> expected = {
        TokenKind::LBrace, TokenKind::String, TokenKind::Colon,
        TokenKind::Number, TokenKind::RBrace, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

// ===== Hex / Binary literal tests =====

TEST(LexerTest, HexLiteral) {
    auto toks = tokenize("0xFF");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0xFF");
}

TEST(LexerTest, HexLiteralUpperCase) {
    auto toks = tokenize("0X1A2B");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0X1A2B");
}

TEST(LexerTest, HexLiteralZero) {
    auto toks = tokenize("0x0");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0x0");
}

TEST(LexerTest, HexLiteralInvalidThrows) {
    EXPECT_THROW(tokenize("0x"), std::runtime_error);
    EXPECT_THROW(tokenize("0xG"), std::runtime_error);
}

TEST(LexerTest, BinaryLiteral) {
    auto toks = tokenize("0b1010");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0b1010");
}

TEST(LexerTest, BinaryLiteralUpperCase) {
    auto toks = tokenize("0B1100");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0B1100");
}

TEST(LexerTest, BinaryLiteralZero) {
    auto toks = tokenize("0b0");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0b0");
}

TEST(LexerTest, BinaryLiteralInvalidThrows) {
    EXPECT_THROW(tokenize("0b"), std::runtime_error);
    EXPECT_THROW(tokenize("0b2"), std::runtime_error);
}

// ===== >>> トークンテスト =====

TEST(LexerTest, GreaterGreaterGreater) {
    auto toks = tokenize(">>>");
    ASSERT_GE(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::GreaterGreaterGreater);
    EXPECT_EQ(toks[0].value, ">>>");
}

TEST(LexerTest, GreaterGreaterGreaterDoesNotBreakOthers) {
    // >> is still >>
    auto toks1 = tokenize(">>");
    EXPECT_EQ(toks1[0].kind, TokenKind::GreaterGreater);
    EXPECT_EQ(toks1[0].value, ">>");

    // >= is still >=
    auto toks2 = tokenize(">=");
    EXPECT_EQ(toks2[0].kind, TokenKind::GreaterEq);

    // > is still >
    auto toks3 = tokenize(">");
    EXPECT_EQ(toks3[0].kind, TokenKind::Greater);
}

// ===== Contract keywords =====

TEST(LexerTest, KeywordRequire) {
    auto toks = tokenize("require");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Require);
}

TEST(LexerTest, KeywordEnsure) {
    auto toks = tokenize("ensure");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ensure);
}

TEST(LexerTest, KeywordInvariant) {
    auto toks = tokenize("invariant");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Invariant);
}

TEST(LexerTest, KeywordOld) {
    auto toks = tokenize("old");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Old);
}

TEST(LexerTest, KeywordResult) {
    auto toks = tokenize("result");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Result);
}

// ===== f-string tokens =====

TEST(LexerTest, FStringNoInterpolation) {
    auto toks = tokenize("f\"hello\"");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
    EXPECT_EQ(toks[0].value, "hello");
}

TEST(LexerTest, FStringSingleInterpolation) {
    auto toks = tokenize("f\"hello {name}\"");
    ASSERT_GE(toks.size(), 4u);
    EXPECT_EQ(toks[0].kind, TokenKind::FStringStart);
    EXPECT_EQ(toks[0].value, "hello ");
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "name");
    EXPECT_EQ(toks[2].kind, TokenKind::FStringEnd);
}

TEST(LexerTest, FStringMultipleInterpolations) {
    auto toks = tokenize("f\"{a} + {b}\"");
    EXPECT_EQ(toks[0].kind, TokenKind::FStringStart);
    EXPECT_EQ(toks[0].value, "");
    // a
    EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].value, "a");
    // " + "
    EXPECT_EQ(toks[2].kind, TokenKind::FStringMid);
    EXPECT_EQ(toks[2].value, " + ");
    // b
    EXPECT_EQ(toks[3].kind, TokenKind::Ident);
    EXPECT_EQ(toks[3].value, "b");
    EXPECT_EQ(toks[4].kind, TokenKind::FStringEnd);
    EXPECT_EQ(toks[4].value, "");
}

TEST(LexerTest, FStringEscapedBraces) {
    auto toks = tokenize("f\"{{braces}}\"");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
    EXPECT_EQ(toks[0].value, "{braces}");
}

// ===== as / Ok / Err keywords =====

// ===== Compound assignment tokens =====

TEST(LexerTest, SlashSlashEqToken) {
    auto toks = tokenize("//=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::SlashSlashEq);
    EXPECT_EQ(toks[0].value, "//=");
}

TEST(LexerTest, StarStarEqToken) {
    auto toks = tokenize("**=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::StarStarEq);
    EXPECT_EQ(toks[0].value, "**=");
}

TEST(LexerTest, AmpEqToken) {
    auto toks = tokenize("&=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::AmpEq);
    EXPECT_EQ(toks[0].value, "&=");
}

TEST(LexerTest, PipeEqToken) {
    auto toks = tokenize("|=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::PipeEq);
    EXPECT_EQ(toks[0].value, "|=");
}

TEST(LexerTest, CaretEqToken) {
    auto toks = tokenize("^=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::CaretEq);
    EXPECT_EQ(toks[0].value, "^=");
}

TEST(LexerTest, LessLessEqToken) {
    auto toks = tokenize("<<=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::LessLessEq);
    EXPECT_EQ(toks[0].value, "<<=");
}

TEST(LexerTest, GreaterGreaterEqToken) {
    auto toks = tokenize(">>=");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::GreaterGreaterEq);
    EXPECT_EQ(toks[0].value, ">>=");
}

TEST(LexerTest, CompoundAssignDoesNotBreakExisting) {
    // // is still //
    auto toks1 = tokenize("//");
    EXPECT_EQ(toks1[0].kind, TokenKind::SlashSlash);
    // ** is still **
    auto toks2 = tokenize("**");
    EXPECT_EQ(toks2[0].kind, TokenKind::StarStar);
    // & is still &
    auto toks3 = tokenize("&");
    EXPECT_EQ(toks3[0].kind, TokenKind::Amp);
    // | is still |
    auto toks4 = tokenize("|");
    EXPECT_EQ(toks4[0].kind, TokenKind::Pipe);
    // ^ is still ^
    auto toks5 = tokenize("^");
    EXPECT_EQ(toks5[0].kind, TokenKind::Caret);
    // << is still <<
    auto toks6 = tokenize("<<");
    EXPECT_EQ(toks6[0].kind, TokenKind::LessLess);
    // >> is still >>
    auto toks7 = tokenize(">>");
    EXPECT_EQ(toks7[0].kind, TokenKind::GreaterGreater);
    // >>> is still >>>
    auto toks8 = tokenize(">>>");
    EXPECT_EQ(toks8[0].kind, TokenKind::GreaterGreaterGreater);
}

// ===== as / Ok / Err keywords =====

TEST(LexerTest, KeywordAs) {
    auto toks = tokenize("as");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::As);
    EXPECT_EQ(toks[0].value, "as");
}

TEST(LexerTest, KeywordError) {
    auto toks = tokenize("Error");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::ErrorKw);
    EXPECT_EQ(toks[0].value, "Error");
}

TEST(LexerTest, BangBangOperator) {
    auto toks = tokenize("!!");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::BangBang);
    EXPECT_EQ(toks[0].value, "!!");
}

// ===== r-string =====

TEST(LexerTest, RawStringEscapeN) {
    auto toks = tokenize(R"(r"\n")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "\\n");
}

TEST(LexerTest, RawStringEscapeT) {
    auto toks = tokenize(R"(r"\t")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "\\t");
}

TEST(LexerTest, RawStringPlain) {
    auto toks = tokenize(R"(r"hello")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "hello");
}

TEST(LexerTest, RawStringEmpty) {
    auto toks = tokenize(R"(r"")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "");
}

TEST(LexerTest, RawStringNotPrefix) {
    // 'r' followed by something other than '"' should be an identifier
    auto toks = tokenize("r_foo");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "r_foo");
}

// ===== Column tracking tests =====

TEST(LexerTest, ColumnSimple) {
    auto toks = tokenize("let x = 42");
    // let(col=1) x(col=5) =(col=7) 42(col=9) Eof
    EXPECT_EQ(toks[0].col, 1);  // let
    EXPECT_EQ(toks[1].col, 5);  // x
    EXPECT_EQ(toks[2].col, 7);  // =
    EXPECT_EQ(toks[3].col, 9);  // 42
}

TEST(LexerTest, ColumnMultiLine) {
    auto toks = tokenize("a\n  b");
    // a(line=1,col=1) Newline Indent b(line=2,col=3)
    EXPECT_EQ(toks[0].col, 1);  // a
    EXPECT_EQ(toks[0].line, 1);
    // Find 'b' token
    for (const auto &t : toks) {
        if (t.kind == TokenKind::Ident && t.value == "b") {
            EXPECT_EQ(t.line, 2);
            EXPECT_EQ(t.col, 3);
        }
    }
}

TEST(LexerTest, ColumnOperators) {
    auto toks = tokenize("x + y == z");
    // x(1) +(3) y(5) ==(7) z(10)
    EXPECT_EQ(toks[0].col, 1);  // x
    EXPECT_EQ(toks[1].col, 3);  // +
    EXPECT_EQ(toks[2].col, 5);  // y
    EXPECT_EQ(toks[3].col, 7);  // ==
    EXPECT_EQ(toks[4].col, 10); // z
}

TEST(LexerTest, ColumnString) {
    auto toks = tokenize("let s = \"hello\"");
    // let(1) s(5) =(7) "hello"(9)
    EXPECT_EQ(toks[0].col, 1);  // let
    EXPECT_EQ(toks[1].col, 5);  // s
    EXPECT_EQ(toks[2].col, 7);  // =
    EXPECT_EQ(toks[3].col, 9);  // "hello"
}
