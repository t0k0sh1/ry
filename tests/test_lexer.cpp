#include <gtest/gtest.h>
#include "ry/lexer/lexer.hpp"


using namespace ry;
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

// ===== Basic tokens =====

TEST(LexerTest, BasicTokens) {
    // Integer
    {
        auto toks = tokenize("42");
        ASSERT_EQ(toks.size(), 2u); // Number + Eof
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "42");
    }
    // Float
    {
        auto toks = tokenize("3.14");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, "3.14");
    }
    // Identifier
    {
        // "android" は And キーワードではなく Ident
        auto toks = tokenize("android");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "android");
    }
}

// ===== Keyword recognition =====

TEST(LexerTest, KeywordRecognition) {
    // and
    {
        auto toks = tokenize("and");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::And);
    }
    // or
    {
        auto toks = tokenize("or");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Or);
    }
    // not
    {
        auto toks = tokenize("not");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Not);
    }
    // not in (fused token)
    {
        auto toks = tokenize("x not in s");
        ASSERT_EQ(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::NotIn);
        EXPECT_EQ(toks[1].value, "not in");
        EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    }
    // not in with multiple spaces
    {
        auto toks = tokenize("x not  in s");
        ASSERT_EQ(toks.size(), 4u);
        EXPECT_EQ(toks[1].kind, TokenKind::NotIn);
    }
    // not followed by non-in identifier
    {
        auto toks = tokenize("not inside");
        ASSERT_EQ(toks.size(), 3u);
        EXPECT_EQ(toks[0].kind, TokenKind::Not);
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].value, "inside");
    }
    // standalone not (unary)
    {
        auto toks = tokenize("not x");
        ASSERT_EQ(toks.size(), 3u);
        EXPECT_EQ(toks[0].kind, TokenKind::Not);
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
    }
    // true
    {
        auto toks = tokenize("true");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::True);
        EXPECT_EQ(toks[0].value, "true");
    }
    // false
    {
        auto toks = tokenize("false");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::False);
        EXPECT_EQ(toks[0].value, "false");
    }
    // if
    {
        auto toks = tokenize("if");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::If);
        EXPECT_EQ(toks[0].value, "if");
    }
    // `when` is no longer a keyword (removed in #800) — should lex as Ident
    {
        auto toks = tokenize("when");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "when");
    }
    // `match` is no longer a keyword (removed in #800) — should lex as Ident
    {
        auto toks = tokenize("match");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "match");
    }
    // `try` is no longer a keyword (reverted in #2312) — should lex as Ident
    {
        auto toks = tokenize("try");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "try");
    }
    // else
    {
        auto toks = tokenize("else");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Else);
        EXPECT_EQ(toks[0].value, "else");
    }
    // while
    {
        auto toks = tokenize("while");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::While);
        EXPECT_EQ(toks[0].value, "while");
    }
    // `function` is not a keyword — lexes as Ident (alias removed; use `fn`)
    {
        auto toks = tokenize("function");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "function");
    }
    // fn
    {
        auto toks = tokenize("fn");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Fn);
        EXPECT_EQ(toks[0].value, "fn");
    }
    // return
    {
        auto toks = tokenize("return");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Return);
        EXPECT_EQ(toks[0].value, "return");
    }
    // from
    {
        auto toks = tokenize("from");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::From);
        EXPECT_EQ(toks[0].value, "from");
    }
    // import
    {
        auto toks = tokenize("import");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Import);
        EXPECT_EQ(toks[0].value, "import");
    }
    // type
    {
        auto toks = tokenize("type");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Type);
        EXPECT_EQ(toks[0].value, "type");
    }
    // require
    {
        auto toks = tokenize("require");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Require);
    }
    // ensure
    {
        auto toks = tokenize("ensure");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ensure);
    }
    // invariant
    {
        auto toks = tokenize("invariant");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Invariant);
    }
    // as
    {
        auto toks = tokenize("as");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::As);
        EXPECT_EQ(toks[0].value, "as");
    }
    // Error
    {
        auto toks = tokenize("Error");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::ErrorKw);
        EXPECT_EQ(toks[0].value, "Error");
    }
}

// ===== Identifier vs keyword =====

TEST(LexerTest, IdentNotKeyword) {
    // IdentStartsWithKeyword
    for (const auto &word : {"android", "orbit", "nothing", "trueblood", "falsehood"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
        EXPECT_EQ(toks[0].value, word) << "word: " << word;
    }
    // IfElifElseAreNotIdent
    for (const auto &word : {"iffy", "elsewhere", "elbow", "iffier"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
    // WhilingIsIdent
    {
        auto toks = tokenize("whiling");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "whiling");
    }
    // FnordAndReturningAreIdent
    for (const auto &word : {"fnord", "returning"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
    // FromAndImportPrefixAreIdent
    for (const auto &word : {"fromage", "imported", "frothy"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
    // TypePrefixIsIdent
    for (const auto &word : {"typedef", "types", "typer"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
    }
    // LetVarAreIdentifiers
    for (const auto &word : {"let", "var"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
        EXPECT_EQ(toks[0].value, word) << "word: " << word;
    }
    // LetterAndVariableAreIdent
    for (const auto &word : {"letter", "variable", "letting", "variant"}) {
        auto toks = tokenize(word);
        ASSERT_EQ(toks.size(), 2u) << "word: " << word;
        EXPECT_EQ(toks[0].kind, TokenKind::Ident) << "word: " << word;
        EXPECT_EQ(toks[0].value, word) << "word: " << word;
    }
    // OldIsIdentifier
    {
        auto toks = tokenize("old");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "old");
    }
    // ResultIsIdentifier
    {
        auto toks = tokenize("result");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "result");
    }
}

// ===== Multi-char operator tokens =====

TEST(LexerTest, MultiCharOperators) {
    // StarVsStarStar
    {
        auto toks1 = tokenize("*");
        EXPECT_EQ(toks1[0].kind, TokenKind::Star);

        auto toks2 = tokenize("**");
        EXPECT_EQ(toks2[0].kind, TokenKind::StarStar);
        EXPECT_EQ(toks2[0].value, "**");
    }
    // SlashVsSlashSlash
    {
        // After a value-producing token, / is division
        auto toks1 = tokenize("a /");
        EXPECT_EQ(toks1[1].kind, TokenKind::Slash);

        auto toks2 = tokenize("//");
        EXPECT_EQ(toks2[0].kind, TokenKind::SlashSlash);
        EXPECT_EQ(toks2[0].value, "//");

        // At statement start, /pattern/ is a regex literal
        auto toks3 = tokenize("/abc/");
        EXPECT_EQ(toks3[0].kind, TokenKind::RegexLiteral);
        EXPECT_EQ(toks3[0].value, "abc");
    }
    // EqVsEqEq
    {
        auto toks1 = tokenize("=");
        EXPECT_EQ(toks1[0].kind, TokenKind::Equals);

        auto toks2 = tokenize("==");
        EXPECT_EQ(toks2[0].kind, TokenKind::EqEq);
        EXPECT_EQ(toks2[0].value, "==");
    }
}

// ===== Regex literal escape sequences =====

TEST(LexerTest, RegexLiteralNulEscape) {
    // \0 inside a regex literal must be translated to a NUL byte (same as in string literals)
    {
        auto toks = tokenize("/a\\0b/");
        ASSERT_EQ(toks[0].kind, TokenKind::RegexLiteral);
        EXPECT_EQ(toks[0].value.size(), 3u);
        EXPECT_EQ(toks[0].value[0], 'a');
        EXPECT_EQ(toks[0].value[1], '\0');
        EXPECT_EQ(toks[0].value[2], 'b');
    }
    {
        auto toks = tokenize("/\\0/");
        ASSERT_EQ(toks[0].kind, TokenKind::RegexLiteral);
        EXPECT_EQ(toks[0].value.size(), 1u);
        EXPECT_EQ(toks[0].value[0], '\0');
    }
    // Regression: other escapes continue to pass through verbatim
    {
        auto toks = tokenize("/a\\/b/");
        ASSERT_EQ(toks[0].kind, TokenKind::RegexLiteral);
        EXPECT_EQ(toks[0].value, "a\\/b");
    }
}

TEST(LexerTest, RegexLiteralCrlfUnterminated) {
    // A backslash before \r\n (CRLF) must yield an unterminated-regex error,
    // not silently consume \r into the pattern.
    {
        auto toks = tokenize("/abc\\\r\n/");
        EXPECT_EQ(toks[0].kind, TokenKind::Error);
    }
    // A bare \r (CR-only) without a closing / also signals unterminated.
    {
        auto toks = tokenize("/abc\r/");
        EXPECT_EQ(toks[0].kind, TokenKind::Error);
    }
}

// ===== Shift operator tokens =====

TEST(LexerTest, ShiftOperatorTokens) {
    // LessLessVsLessAndLessEq
    {
        // << は LessLess
        auto toks1 = tokenize("<<");
        EXPECT_EQ(toks1[0].kind, TokenKind::LessLess);
        EXPECT_EQ(toks1[0].value, "<<");

        // 回帰: < と <= は変わらず
        auto toks2 = tokenize("< <=");
        EXPECT_EQ(toks2[0].kind, TokenKind::Less);
        EXPECT_EQ(toks2[1].kind, TokenKind::LessEq);
    }
    // GreaterGreaterVsGreaterAndGreaterEq
    {
        // >> は GreaterGreater
        auto toks1 = tokenize(">>");
        EXPECT_EQ(toks1[0].kind, TokenKind::GreaterGreater);
        EXPECT_EQ(toks1[0].value, ">>");

        // 回帰: > と >= は変わらず
        auto toks2 = tokenize("> >=");
        EXPECT_EQ(toks2[0].kind, TokenKind::Greater);
        EXPECT_EQ(toks2[1].kind, TokenKind::GreaterEq);
    }
    // GreaterGreaterGreater
    {
        auto toks = tokenize(">>>");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::GreaterGreaterGreater);
        EXPECT_EQ(toks[0].value, ">>>");
    }
    // GreaterGreaterGreaterDoesNotBreakOthers
    {
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
}

// ===== Compound assignment tokens =====

TEST(LexerTest, CompoundAssignTokens) {
    // SlashSlashEqToken
    {
        auto toks = tokenize("//=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::SlashSlashEq);
        EXPECT_EQ(toks[0].value, "//=");
    }
    // StarStarEqToken
    {
        auto toks = tokenize("**=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::StarStarEq);
        EXPECT_EQ(toks[0].value, "**=");
    }
    // AmpEqToken
    {
        auto toks = tokenize("&=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::AmpEq);
        EXPECT_EQ(toks[0].value, "&=");
    }
    // PipeEqToken
    {
        auto toks = tokenize("|=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::PipeEq);
        EXPECT_EQ(toks[0].value, "|=");
    }
    // CaretEqToken
    {
        auto toks = tokenize("^=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::CaretEq);
        EXPECT_EQ(toks[0].value, "^=");
    }
    // LessLessEqToken
    {
        auto toks = tokenize("<<=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::LessLessEq);
        EXPECT_EQ(toks[0].value, "<<=");
    }
    // GreaterGreaterEqToken
    {
        auto toks = tokenize(">>=");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::GreaterGreaterEq);
        EXPECT_EQ(toks[0].value, ">>=");
    }
    // CompoundAssignDoesNotBreakExisting
    {
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
}

// ===== Hex/Binary literal tokens =====

TEST(LexerTest, HexBinaryLiteralTokens) {
    // HexLiteral
    {
        auto toks = tokenize("0xFF");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0xFF");
    }
    // HexLiteralUpperCase
    {
        auto toks = tokenize("0X1A2B");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0X1A2B");
    }
    // HexLiteralZero
    {
        auto toks = tokenize("0x0");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0x0");
    }
    // HexLiteralInvalidThrows
    {
        EXPECT_THROW(tokenize("0x"), std::runtime_error);
        EXPECT_THROW(tokenize("0xG"), std::runtime_error);
    }
    // BinaryLiteral
    {
        auto toks = tokenize("0b1010");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0b1010");
    }
    // BinaryLiteralUpperCase
    {
        auto toks = tokenize("0B1100");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0B1100");
    }
    // BinaryLiteralZero
    {
        auto toks = tokenize("0b0");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0b0");
    }
    // BinaryLiteralInvalidThrows
    {
        EXPECT_THROW(tokenize("0b"), std::runtime_error);
        EXPECT_THROW(tokenize("0b2"), std::runtime_error);
    }
}

// ===== String literal tokens =====

TEST(LexerTest, StringLiteralTokens) {
    // StringLiteral
    {
        auto toks = tokenize("\"hello\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "hello");
    }
    // EmptyString
    {
        auto toks = tokenize("\"\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "");
    }
    // UnterminatedStringThrows
    {
        EXPECT_THROW(tokenize("\"hello"), std::runtime_error);
    }
    // UnterminatedStringNewlineThrows
    {
        EXPECT_THROW(tokenize("\"hello\nworld\""), std::runtime_error);
    }
    // StringInExpression
    {
        auto toks = tokenize("s = \"world\"");
        ASSERT_EQ(toks.size(), 4u); // Ident Equals String Eof
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "s");
        EXPECT_EQ(toks[1].kind, TokenKind::Equals);
        EXPECT_EQ(toks[2].kind, TokenKind::String);
        EXPECT_EQ(toks[2].value, "world");
        EXPECT_EQ(toks[3].kind, TokenKind::Eof);
    }
}

// ===== Unicode escape \u{HHHH} (#2427) =====
//
// Verified across regular strings, block strings, and f-strings since the
// three lexer sites share `decodeUnicodeEscape` but each has its own
// surrounding loop; a regression in one would not surface in the others.

TEST(LexerTest, UnicodeEscapeRegularString) {
    // ASCII
    {
        auto toks = tokenize("\"\\u{41}\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "A");
    }
    // 1-byte boundary
    {
        auto toks = tokenize("\"\\u{7F}\"");
        EXPECT_EQ(toks[0].value, std::string("\x7F"));
    }
    // 2-byte UTF-8 boundary (cp = 0x80)
    {
        auto toks = tokenize("\"\\u{80}\"");
        EXPECT_EQ(toks[0].value, std::string("\xC2\x80"));
    }
    // 2-byte UTF-8 upper bound (cp = 0x7FF)
    {
        auto toks = tokenize("\"\\u{7FF}\"");
        EXPECT_EQ(toks[0].value, std::string("\xDF\xBF"));
    }
    // 3-byte UTF-8 boundary (cp = 0x800)
    {
        auto toks = tokenize("\"\\u{800}\"");
        EXPECT_EQ(toks[0].value, std::string("\xE0\xA0\x80"));
    }
    // 3-byte UTF-8 upper bound (cp = 0xFFFF)
    {
        auto toks = tokenize("\"\\u{FFFF}\"");
        EXPECT_EQ(toks[0].value, std::string("\xEF\xBF\xBF"));
    }
    // 4-byte UTF-8 boundary (cp = 0x10000)
    {
        auto toks = tokenize("\"\\u{10000}\"");
        EXPECT_EQ(toks[0].value, std::string("\xF0\x90\x80\x80"));
    }
    // U+1F600 GRINNING FACE
    {
        auto toks = tokenize("\"\\u{1F600}\"");
        EXPECT_EQ(toks[0].value, std::string("\xF0\x9F\x98\x80"));
    }
    // Max valid code point (cp = 0x10FFFF)
    {
        auto toks = tokenize("\"\\u{10FFFF}\"");
        EXPECT_EQ(toks[0].value, std::string("\xF4\x8F\xBF\xBF"));
    }
    // Lowercase hex
    {
        auto toks = tokenize("\"\\u{1f600}\"");
        EXPECT_EQ(toks[0].value, std::string("\xF0\x9F\x98\x80"));
    }
    // NUL: \u{0} produces a single NUL byte
    {
        auto toks = tokenize("\"\\u{0}\"");
        // Avoid embedded `\0` in C-string ctor.
        EXPECT_EQ(toks[0].value, std::string(1, '\0'));
    }
    // Mixed with neighbouring content
    {
        auto toks = tokenize("\"a\\u{41}b\"");
        EXPECT_EQ(toks[0].value, "aAb");
    }
    // Mixed with other single-char escapes
    {
        auto toks = tokenize("\"\\n\\u{41}\\t\"");
        EXPECT_EQ(toks[0].value, "\nA\t");
    }
    // Adjacent unicode escapes
    {
        auto toks = tokenize("\"\\u{41}\\u{42}\\u{43}\"");
        EXPECT_EQ(toks[0].value, "ABC");
    }
}

TEST(LexerTest, UnicodeEscapeErrors) {
    // Missing '{' (e.g. user typed `\u41` instead of `\u{41}`)
    EXPECT_THROW(tokenize("\"\\u41\""), std::runtime_error);
    // Missing '}' inside a single-line string: greedy parse hits the
    // closing quote as a non-hex char.
    EXPECT_THROW(tokenize("\"\\u{41\""), std::runtime_error);
    // Empty: `\u{}`
    EXPECT_THROW(tokenize("\"\\u{}\""), std::runtime_error);
    // Non-hex digit
    EXPECT_THROW(tokenize("\"\\u{ZZZ}\""), std::runtime_error);
    // Above 0x10FFFF
    EXPECT_THROW(tokenize("\"\\u{110000}\""), std::runtime_error);
    // Surrogate range
    EXPECT_THROW(tokenize("\"\\u{D800}\""), std::runtime_error);
    EXPECT_THROW(tokenize("\"\\u{DBFF}\""), std::runtime_error);
    EXPECT_THROW(tokenize("\"\\u{DC00}\""), std::runtime_error);
    EXPECT_THROW(tokenize("\"\\u{DFFF}\""), std::runtime_error);
    // More than 6 hex digits — caught before value validation so we don't
    // silently accept 7-digit values that happen to land in range.
    EXPECT_THROW(tokenize("\"\\u{1234567}\""), std::runtime_error);
    // Abrupt EOF after '\u' (no '{')
    EXPECT_THROW(tokenize("\"\\u"), std::runtime_error);
    // Abrupt EOF inside `\u{...` (no '}')
    EXPECT_THROW(tokenize("\"\\u{1F"), std::runtime_error);
    // [regression: #2442] — a non-ASCII char like α inside `\u{...}` must
    // be rendered as itself in the message, not as the leading byte alone
    // (which the terminal would show as U+FFFD).
    try {
        tokenize("\"\\u{\xCE\xB1}\"");
        FAIL() << "expected std::runtime_error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("\xCE\xB1"), std::string::npos) << msg;
    }
}

TEST(LexerTest, UnicodeEscapeBlockString) {
    {
        auto toks = tokenize("\"\"\"\\u{1F600}\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, std::string("\xF0\x9F\x98\x80"));
    }
    {
        auto toks = tokenize("\"\"\"a\\u{41}b\"\"\"");
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "aAb");
    }
    // Block-string error path uses the same helper, so a malformed escape
    // still throws.
    EXPECT_THROW(tokenize("\"\"\"\\u{}\"\"\""), std::runtime_error);
}

TEST(LexerTest, UnicodeEscapeFString) {
    {
        auto toks = tokenize("f\"\\u{1F600}\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, std::string("\xF0\x9F\x98\x80"));
    }
    {
        auto toks = tokenize("f\"hello\\u{41}\"");
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, "helloA");
    }
    // `\u{...}` must not collide with f-string `{expr}` interpolation —
    // the backslash routes through the escape switch before the `{`-as-
    // interpolation-open branch is considered.
    {
        auto toks = tokenize("f\"\\u{41}{x}\"");
        ASSERT_GE(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringStart);
        EXPECT_EQ(toks[0].value, "A");
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].value, "x");
        EXPECT_EQ(toks[2].kind, TokenKind::FStringEnd);
    }
    // f-string error path mentions "in f-string" suffix
    EXPECT_THROW(tokenize("f\"\\u{ZZZ}\""), std::runtime_error);
}

// Raw strings (`r"..."`) intentionally pass through escapes verbatim
// (lexer.cpp:534), so `\u{...}` remains the literal byte sequence rather
// than being decoded. The lexer must not invoke the unicode decoder on
// this path.
TEST(LexerTest, UnicodeEscapeRawStringPassthrough) {
    auto toks = tokenize(R"(r"\u{1F600}")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "\\u{1F600}");
}

// ===== Hex escape \xNN (#2440) =====
//
// `\xNN` decodes exactly two hex digits into a single raw byte (0x00 - 0xFF),
// distinct from `\u{HHHH}` which encodes a Unicode code point as UTF-8.
// Verified across regular strings, block strings, and f-strings since the
// three lexer sites share `decodeHexEscape` but each has its own surrounding
// loop; a regression in one would not surface in the others.

TEST(LexerTest, HexEscapeRegularString) {
    // ASCII (the repro from the issue)
    {
        auto toks = tokenize("\"\\x41\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "A");
    }
    // 0x00: \x00 produces a single NUL byte
    {
        auto toks = tokenize("\"\\x00\"");
        EXPECT_EQ(toks[0].value, std::string(1, '\0'));
    }
    // 0x7F (ASCII boundary)
    {
        auto toks = tokenize("\"\\x7F\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0x7F)));
    }
    // 0x80 (non-UTF-8 single byte)
    {
        auto toks = tokenize("\"\\x80\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0x80)));
    }
    // 0xFF (max single byte)
    {
        auto toks = tokenize("\"\\xFF\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0xFF)));
    }
    // Lowercase hex
    {
        auto toks = tokenize("\"\\xab\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0xAB)));
    }
    // Uppercase hex
    {
        auto toks = tokenize("\"\\xAB\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0xAB)));
    }
    // Mixed case
    {
        auto toks = tokenize("\"\\xaB\"");
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0xAB)));
    }
    // Mixed with neighbouring content
    {
        auto toks = tokenize("\"a\\x41b\"");
        EXPECT_EQ(toks[0].value, "aAb");
    }
    // Mixed with other single-char escapes
    {
        auto toks = tokenize("\"\\n\\x41\\t\"");
        EXPECT_EQ(toks[0].value, "\nA\t");
    }
    // Adjacent hex escapes
    {
        auto toks = tokenize("\"\\x41\\x42\\x43\"");
        EXPECT_EQ(toks[0].value, "ABC");
    }
    // Mixed with `\u{...}`
    {
        auto toks = tokenize("\"\\x41\\u{42}\"");
        EXPECT_EQ(toks[0].value, "AB");
    }
}

TEST(LexerTest, HexEscapeErrors) {
    // Abrupt EOF after '\x' (no hex digits, string unterminated)
    EXPECT_THROW(tokenize("\"\\x"), std::runtime_error);
    // Closing quote where the first hex digit was expected
    EXPECT_THROW(tokenize("\"\\x\""), std::runtime_error);
    // Only one hex digit before the closing quote
    EXPECT_THROW(tokenize("\"\\x4\""), std::runtime_error);
    // Non-hex first digit
    EXPECT_THROW(tokenize("\"\\xZZ\""), std::runtime_error);
    // Non-hex second digit
    EXPECT_THROW(tokenize("\"\\x4Z\""), std::runtime_error);
    // Abrupt EOF in the middle of the escape
    EXPECT_THROW(tokenize("\"\\x4"), std::runtime_error);
    // [regression: #2442] — α as the first hex digit must render as α, not
    // a stray leading byte.
    try {
        tokenize("\"\\x\xCE\xB1\"");
        FAIL() << "expected std::runtime_error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("\xCE\xB1"), std::string::npos) << msg;
    }
    // [regression: #2442] — same for the second hex digit position.
    try {
        tokenize("\"\\x4\xCE\xB1\"");
        FAIL() << "expected std::runtime_error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("\xCE\xB1"), std::string::npos) << msg;
    }
}

TEST(LexerTest, HexEscapeBlockString) {
    {
        auto toks = tokenize("\"\"\"\\x41\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "A");
    }
    {
        auto toks = tokenize("\"\"\"a\\x41b\"\"\"");
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "aAb");
    }
    // Non-UTF-8 byte still produced in block string form
    {
        auto toks = tokenize("\"\"\"\\xFF\"\"\"");
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, std::string(1, static_cast<char>(0xFF)));
    }
    // Block-string error path uses the same helper
    EXPECT_THROW(tokenize("\"\"\"\\xZZ\"\"\""), std::runtime_error);
}

TEST(LexerTest, HexEscapeFString) {
    {
        auto toks = tokenize("f\"\\x41\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, "A");
    }
    {
        auto toks = tokenize("f\"hello\\x41\"");
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, "helloA");
    }
    // `\xNN` must not collide with f-string `{expr}` interpolation
    {
        auto toks = tokenize("f\"\\x41{x}\"");
        ASSERT_GE(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringStart);
        EXPECT_EQ(toks[0].value, "A");
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].value, "x");
        EXPECT_EQ(toks[2].kind, TokenKind::FStringEnd);
    }
    // f-string error path (message includes "in f-string" suffix)
    EXPECT_THROW(tokenize("f\"\\xZZ\""), std::runtime_error);
}

// Raw strings pass `\xNN` through verbatim, same as `\u{...}`.
TEST(LexerTest, HexEscapeRawStringPassthrough) {
    auto toks = tokenize(R"(r"\x41")");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::String);
    EXPECT_EQ(toks[0].value, "\\x41");
}

// ===== F-string tokens =====

TEST(LexerTest, FStringTokens) {
    // FStringNoInterpolation
    {
        auto toks = tokenize("f\"hello\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, "hello");
    }
    // FStringSingleInterpolation
    {
        auto toks = tokenize("f\"hello {name}\"");
        ASSERT_GE(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringStart);
        EXPECT_EQ(toks[0].value, "hello ");
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].value, "name");
        EXPECT_EQ(toks[2].kind, TokenKind::FStringEnd);
    }
    // FStringMultipleInterpolations
    {
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
    // FStringEscapedBraces
    {
        auto toks = tokenize("f\"{{braces}}\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::FStringEnd);
        EXPECT_EQ(toks[0].value, "{braces}");
    }
}

// ===== Raw string tokens =====

TEST(LexerTest, RawStringTokens) {
    // RawStringEscapeN
    {
        auto toks = tokenize(R"(r"\n")");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "\\n");
    }
    // RawStringEscapeT
    {
        auto toks = tokenize(R"(r"\t")");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "\\t");
    }
    // RawStringPlain
    {
        auto toks = tokenize(R"(r"hello")");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "hello");
    }
    // RawStringEmpty
    {
        auto toks = tokenize(R"(r"")");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::String);
        EXPECT_EQ(toks[0].value, "");
    }
    // RawStringNotPrefix
    {
        // 'r' followed by something other than '"' should be an identifier
        auto toks = tokenize("r_foo");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "r_foo");
    }
}

// ===== Block string tokens =====

TEST(LexerTest, BlockStringTokens) {
    // SameLineSimple: """hello""" → "hello"
    {
        auto toks = tokenize("\"\"\"hello\"\"\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "hello");
    }
    // EmptySameLine: """""" → ""
    {
        auto toks = tokenize("\"\"\"\"\"\"");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "");
    }
    // LeadingNewlineDrop: """\nhello\n""" → "hello"
    {
        auto toks = tokenize("\"\"\"\nhello\n\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "hello");
    }
    // IssueExactExample: """\n  a\n    b\n  c\n  """ → "a\n  b\nc"
    {
        auto toks = tokenize("\"\"\"\n  a\n    b\n  c\n  \"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "a\n  b\nc");
    }
    // BlankLinePreservation: """\n  a\n\n  b\n  """ → "a\n\nb"
    {
        auto toks = tokenize("\"\"\"\n  a\n\n  b\n  \"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "a\n\nb");
    }
    // EscapeSequenceDecoded: """\n\\n\n""" → "\n" (literal \n escape → newline byte)
    {
        auto toks = tokenize("\"\"\"\n\\n\n\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "\n");
    }
    // EscapeBackslash: """\\""" → "\\"
    {
        auto toks = tokenize("\"\"\"\\\\\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "\\");
    }
    // EmbeddedSingleQuote: """a"b""" → a"b
    {
        auto toks = tokenize("\"\"\"a\"b\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "a\"b");
    }
    // EmbeddedDoubleQuote: """a""b""" → a""b
    {
        auto toks = tokenize("\"\"\"a\"\"b\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "a\"\"b");
    }
    // EscapedTripleQuote: lets us embed """ inside the block
    {
        auto toks = tokenize("\"\"\"a\\\"\"\"b\"\"\"");
        ASSERT_GE(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[0].value, "a\"\"\"b");
    }
    // UnterminatedThrows
    {
        EXPECT_THROW(tokenize("\"\"\"hello"), std::runtime_error);
    }
    // UnterminatedAcrossNewlinesThrows
    {
        EXPECT_THROW(tokenize("\"\"\"hello\nworld"), std::runtime_error);
    }
    // UnknownEscapeThrows
    {
        EXPECT_THROW(tokenize("\"\"\"\\q\"\"\""), std::runtime_error);
    }
    // BlockStringInExpression: assignment context
    {
        auto toks = tokenize("s = \"\"\"hi\"\"\"");
        ASSERT_EQ(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::Equals);
        EXPECT_EQ(toks[2].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[2].value, "hi");
        EXPECT_EQ(toks[3].kind, TokenKind::Eof);
    }
    // IndentStateRegression: block string inside an indented block must not
    // disturb the surrounding indent. After the block string ends with its
    // closing """, the next line at the same indent must stay in the if block.
    // Source layout (indent 4):
    //   if true:
    //       s = """
    //   multi
    //       """
    //       print(s)
    {
        auto toks = tokenize("if true:\n    s = \"\"\"\nmulti\n    \"\"\"\n    print(s)\n");
        // We don't enumerate every token but assert there is no spurious
        // Dedent emitted before `print(s)` — the print sits in the if body.
        // Walk tokens and find the BlockString followed by Newline then `print`
        // without any intervening Dedent.
        size_t bs = 0;
        for (; bs < toks.size(); ++bs) {
            if (toks[bs].kind == TokenKind::BlockString) break;
        }
        ASSERT_LT(bs, toks.size());
        EXPECT_EQ(toks[bs].value, "multi");
        // Expect: BlockString, Newline, Ident("print"), ...
        // No Dedent between BlockString and the next Newline / Ident.
        ASSERT_LT(bs + 2, toks.size());
        EXPECT_EQ(toks[bs + 1].kind, TokenKind::Newline);
        EXPECT_EQ(toks[bs + 2].kind, TokenKind::Ident);
        EXPECT_EQ(toks[bs + 2].value, "print");
    }
    // PrevKindRegressionSlash: `/` after a block string must lex as division,
    // not as the start of a regex literal. The disambiguation guard for `/`
    // must include TokenKind::BlockString alongside TokenKind::String.
    {
        auto toks = tokenize("x = \"\"\"a\"\"\" / 2");
        // Token sequence: Ident("x"), Equals, BlockString("a"), Slash, Number("2"), Eof
        ASSERT_EQ(toks.size(), 6u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::Equals);
        EXPECT_EQ(toks[2].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[2].value, "a");
        EXPECT_EQ(toks[3].kind, TokenKind::Slash);
        EXPECT_EQ(toks[4].kind, TokenKind::Number);
        EXPECT_EQ(toks[4].value, "2");
        EXPECT_EQ(toks[5].kind, TokenKind::Eof);
    }
    // PrevKindRegressionDotDigit: `.<digit>` after a block string must NOT
    // re-enter leading-dot float scanning (which would happen if the
    // disambiguation guard ignored BlockString and treated the prior token
    // as not value-producing). We expect Dot + Number instead of a Float.
    {
        auto toks = tokenize("x = \"\"\"a\"\"\".0");
        // Token sequence: Ident, Equals, BlockString, Dot, Number, Eof
        ASSERT_EQ(toks.size(), 6u);
        EXPECT_EQ(toks[2].kind, TokenKind::BlockString);
        EXPECT_EQ(toks[3].kind, TokenKind::Dot);
        EXPECT_EQ(toks[4].kind, TokenKind::Number);
        EXPECT_EQ(toks[4].value, "0");
    }
}

// ===== ++/-- tokens =====

TEST(LexerTest, IncrDecrTokens) {
    // PlusPlusToken
    {
        auto toks = tokenize("++");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::PlusPlus);
        EXPECT_EQ(toks[0].value, "++");
    }
    // MinusMinusToken
    {
        auto toks = tokenize("--");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::MinusMinus);
        EXPECT_EQ(toks[0].value, "--");
    }
    // PlusPlusDoesNotBreakExisting
    {
        // + is still +
        auto toks1 = tokenize("+");
        EXPECT_EQ(toks1[0].kind, TokenKind::Plus);
        // += is still +=
        auto toks2 = tokenize("+=");
        EXPECT_EQ(toks2[0].kind, TokenKind::PlusEq);
        EXPECT_EQ(toks2[0].value, "+=");
    }
    // MinusMinusDoesNotBreakExisting
    {
        // - is still -
        auto toks1 = tokenize("-");
        EXPECT_EQ(toks1[0].kind, TokenKind::Minus);
        // -= is still -=
        auto toks2 = tokenize("-=");
        EXPECT_EQ(toks2[0].kind, TokenKind::MinusEq);
        EXPECT_EQ(toks2[0].value, "-=");
        // -> is still ->
        auto toks3 = tokenize("->");
        EXPECT_EQ(toks3[0].kind, TokenKind::Arrow);
        EXPECT_EQ(toks3[0].value, "->");
    }
    // PlusPlusNotCombinedBeforeOperand
    {
        // ++1 should lex as Plus, Plus, Number (not PlusPlus, Number)
        auto toks = tokenize("++1");
        ASSERT_GE(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Plus);
        EXPECT_EQ(toks[1].kind, TokenKind::Plus);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "1");
    }
    // MinusMinusNotCombinedBeforeOperand
    {
        // --y should lex as Minus, Minus, Ident (not MinusMinus, Ident)
        auto toks = tokenize("--y");
        ASSERT_GE(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Minus);
        EXPECT_EQ(toks[1].kind, TokenKind::Minus);
        EXPECT_EQ(toks[2].kind, TokenKind::Ident);
        EXPECT_EQ(toks[2].value, "y");
    }
    // PlusPlusAtEndOfLine
    {
        // x++ at end of line should produce Ident, PlusPlus
        auto toks = tokenize("x++\n");
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::PlusPlus);
    }
    // PlusPlusAtEof
    {
        // x++ at EOF should produce Ident, PlusPlus
        auto toks = tokenize("x++");
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::PlusPlus);
    }
    // PlusPlusBeforeComment
    {
        // x++ # comment should produce Ident, PlusPlus
        auto toks = tokenize("x++ # comment");
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::PlusPlus);
    }
}

// ===== Dot/Ellipsis tokens =====

TEST(LexerTest, DotEllipsisTokens) {
    // Ellipsis
    {
        auto toks = tokenize("...");
        ASSERT_EQ(toks.size(), 2u); // Ellipsis + Eof
        EXPECT_EQ(toks[0].kind, TokenKind::Ellipsis);
        EXPECT_EQ(toks[0].value, "...");
    }
    // EllipsisDoesNotBreakDotDot
    {
        auto toks = tokenize("..");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::DotDot);
        EXPECT_EQ(toks[0].value, "..");
    }
    // EllipsisDoesNotBreakDot
    {
        auto toks = tokenize(".");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Dot);
        EXPECT_EQ(toks[0].value, ".");
    }
}

// ===== Column tracking =====

TEST(LexerTest, ColumnTracking) {
    // ColumnSimple
    {
        auto toks = tokenize("x = 42");
        // x(col=1) =(col=3) 42(col=5) Eof
        EXPECT_EQ(toks[0].col, 1);  // x
        EXPECT_EQ(toks[1].col, 3);  // =
        EXPECT_EQ(toks[2].col, 5);  // 42
    }
    // ColumnMultiLine
    {
        auto toks = tokenize("a\n  b");
        // a(line=1,col=1) Newline Indent b(line=2,col=3)
        EXPECT_EQ(toks[0].col, 1);  // a
        EXPECT_EQ(toks[0].line, 1);
        // Find 'b' token
        bool found = false;
        for (const auto &t : toks) {
            if (t.kind == TokenKind::Ident && t.value == "b") {
                found = true;
                EXPECT_EQ(t.line, 2);
                EXPECT_EQ(t.col, 3);
                break;
            }
        }
        ASSERT_TRUE(found) << "Identifier token 'b' was not found in tokens";
    }
    // ColumnOperators
    {
        auto toks = tokenize("x + y == z");
        // x(1) +(3) y(5) ==(7) z(10)
        EXPECT_EQ(toks[0].col, 1);  // x
        EXPECT_EQ(toks[1].col, 3);  // +
        EXPECT_EQ(toks[2].col, 5);  // y
        EXPECT_EQ(toks[3].col, 7);  // ==
        EXPECT_EQ(toks[4].col, 10); // z
    }
    // ColumnString
    {
        auto toks = tokenize("s = \"hello\"");
        // s(1) =(3) "hello"(5)
        EXPECT_EQ(toks[0].col, 1);  // s
        EXPECT_EQ(toks[1].col, 3);  // =
        EXPECT_EQ(toks[2].col, 5);  // "hello"
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

// ===== Individually kept tests =====

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

// ===== UTF-8 unexpected-token diagnostic (#2442) =====
// Pre-fix the lexer advanced one byte at a time on UTF-8 multi-byte
// sequences, emitting the leading byte alone as the Error token value.
// The byte was invalid UTF-8 on its own and the terminal rendered it as
// U+FFFD (`�`), so the diagnostic was illegible. The fix decodes the
// sequence, stores the full code point on the token, and advances `pos_`
// by the full byte length so trailing bytes don't cascade as separate
// errors.

TEST(LexerTest, NonAsciiTwoByteEmitsFullCodePointAsErrorValue) {
    // [regression: #2442] — α (U+03B1, UTF-8 CE B1).
    auto toks = tokenize("\xCE\xB1");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, std::string("\xCE\xB1"));
    EXPECT_EQ(toks[1].kind, TokenKind::Eof);
}

TEST(LexerTest, NonAsciiThreeByteEmitsFullCodePointAsErrorValue) {
    // [regression: #2442] — ℕ (U+2115, UTF-8 E2 84 95) used to produce three
    // separate Error tokens (one per byte). Now it produces one.
    auto toks = tokenize("\xE2\x84\x95");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, std::string("\xE2\x84\x95"));
    EXPECT_EQ(toks[1].kind, TokenKind::Eof);
}

TEST(LexerTest, NonAsciiFourByteEmitsFullCodePointAsErrorValue) {
    // [regression: #2442] — 😀 (U+1F600, UTF-8 F0 9F 98 80).
    auto toks = tokenize("\xF0\x9F\x98\x80");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, std::string("\xF0\x9F\x98\x80"));
    EXPECT_EQ(toks[1].kind, TokenKind::Eof);
}

TEST(LexerTest, InvalidUtf8LeadByteEmitsHexEscape) {
    // [regression: #2442] — 0xFF is not a valid UTF-8 leading byte. The
    // lexer must still produce a deterministic Error token (no decoder UB).
    auto toks = tokenize("\xFF");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, "\\xFF");
    EXPECT_EQ(toks[1].kind, TokenKind::Eof);
}

TEST(LexerTest, TruncatedUtf8SequenceEmitsHexEscape) {
    // [regression: #2442] — a 2-byte UTF-8 lead with no continuation byte
    // must not be silently consumed as if the trailing nothing were valid.
    auto toks = tokenize("\xCE");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, "\\xCE");
    EXPECT_EQ(toks[1].kind, TokenKind::Eof);
}

TEST(LexerTest, NonAsciiAfterNumericLiteralProducesNumberThenError) {
    // [regression: #2442] — `123ℕ` used to cascade as Number + three
    // byte-level Error tokens (one per UTF-8 byte). `checkNoTrailingIdentStart`
    // intentionally stays ASCII-only so the non-ASCII follow-on falls through
    // to the default fallback; the parser then surfaces it as a proper
    // DiagnosticError with a source snippet + caret rather than the
    // SourceManager-less std::runtime_error path here. The lexer now emits
    // exactly Number + one Error whose value is the full 3-byte sequence.
    auto toks = tokenize("123\xE2\x84\x95");
    ASSERT_EQ(toks.size(), 3u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "123");
    EXPECT_EQ(toks[1].kind, TokenKind::Error);
    EXPECT_EQ(toks[1].value, std::string("\xE2\x84\x95"));
    EXPECT_EQ(toks[2].kind, TokenKind::Eof);
}

TEST(LexerTest, AsciiAlphaAfterNumericLiteralIncludesCharInMessage) {
    // [regression: #2442] — the pre-fix message was the bare phrase
    // "invalid character after numeric literal" with no character. The new
    // formatter quotes the offending character.
    try {
        tokenize("123abc");
        FAIL() << "expected std::runtime_error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("invalid character"), std::string::npos) << msg;
        EXPECT_NE(msg.find("'a'"), std::string::npos) << msg;
    }
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
    // #2137: a comment-only line is transparent at the token level.
    // The lexer suppresses its trailing Newline and iterates so
    // multiline UFCS chains can carry mid-chain comments without the
    // drain loop tripping on a second Newline (see
    // ParserTest.UfcsMultilineChainCommentBetweenHopsTransparent).
    auto toks = tokenize("# full line comment\nx = 1");
    ASSERT_EQ(toks.size(), 4u); // Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "x");
    EXPECT_EQ(toks[1].kind, TokenKind::Equals);
    EXPECT_EQ(toks[2].kind, TokenKind::Number);
    EXPECT_EQ(toks[2].value, "1");
    EXPECT_EQ(toks[3].kind, TokenKind::Eof);
}

TEST(LexerTest, CommentOnly) {
    auto toks = tokenize("# just a comment");
    ASSERT_EQ(toks.size(), 1u);
    EXPECT_EQ(toks[0].kind, TokenKind::Eof);
}

TEST(LexerTest, ConsecutiveCommentLines) {
    // #2137: consecutive comment-only lines are all transparent at the
    // token level — the lexer's iterative loop walks through any number
    // of consecutive `#`-leading lines emitting no Newline for each.
    auto toks = tokenize("# line 1\n# line 2\nx = 1");
    ASSERT_EQ(toks.size(), 4u); // Ident Equals Number Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[1].kind, TokenKind::Equals);
    EXPECT_EQ(toks[2].kind, TokenKind::Number);
    EXPECT_EQ(toks[3].kind, TokenKind::Eof);
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

TEST(LexerTest, CommentLineDoesNotChangeIndent) {
    // #2137: comment-only lines emit nothing at all (no Newline either).
    // Indent/Dedent were already unaffected by comments via the
    // `has_content` gate at Step 2; the post-#2137 spec additionally
    // suppresses the trailing Newline of comment-only lines.
    auto toks = tokenize("a:\n    b\n    # comment\n    c\nd");
    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::Colon, TokenKind::Newline,
        TokenKind::Indent, TokenKind::Ident, TokenKind::Newline,
        TokenKind::Ident, TokenKind::Newline,
        TokenKind::Dedent, TokenKind::Ident, TokenKind::Eof
    };
    ASSERT_EQ(toks.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i)
        EXPECT_EQ(toks[i].kind, expected[i]) << "index: " << i;
}

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

TEST(LexerTest, BangBangIsLexedAsTwoErrorBangs) {
    auto toks = tokenize("!!");
    ASSERT_EQ(toks.size(), 3u);
    EXPECT_EQ(toks[0].kind, TokenKind::Error);
    EXPECT_EQ(toks[0].value, "!");
    EXPECT_EQ(toks[1].kind, TokenKind::Error);
    EXPECT_EQ(toks[1].value, "!");
    EXPECT_EQ(toks[2].kind, TokenKind::Eof);
}

TEST(LexerTest, BangBangAfterIdentifierAbsorbsFirstBang) {
    auto toks = tokenize("r!!");
    ASSERT_EQ(toks.size(), 3u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "r!");
    EXPECT_EQ(toks[1].kind, TokenKind::Error);
    EXPECT_EQ(toks[1].value, "!");
    EXPECT_EQ(toks[2].kind, TokenKind::Eof);
}

TEST(LexerTest, MutatingMethodIdentifier) {
    auto toks = tokenize("sort!(xs)");
    ASSERT_EQ(toks.size(), 5u);
    EXPECT_EQ(toks[0].kind, TokenKind::Ident);
    EXPECT_EQ(toks[0].value, "sort!");
    EXPECT_EQ(toks[1].kind, TokenKind::LParen);
    EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    EXPECT_EQ(toks[2].value, "xs");
    EXPECT_EQ(toks[3].kind, TokenKind::RParen);
    EXPECT_EQ(toks[4].kind, TokenKind::Eof);
}

TEST(LexerTest, ManyConsecutiveComments) {
    // Generate many comment lines to verify no stack overflow
    std::string src;
    // Reserve enough capacity to avoid repeated reallocations while building src
    const std::size_t perLine = std::string("# comment line ").size() + 4 + 1; // prefix + up to 4 digits + '\n'
    src.reserve(perLine * 10000 + 3); // +3 for "42\n"
    for (int i = 0; i < 10000; ++i) {
        src.append("# comment line ");
        src.append(std::to_string(i));
        src.push_back('\n');
    }
    src.append("42\n");
    auto toks = tokenize(src);
    // Should find the number token after all comments
    bool found = false;
    for (auto &t : toks) {
        if (t.kind == TokenKind::Number && t.value == "42") {
            found = true;
            break;
        }
    }
    EXPECT_TRUE(found);
}

TEST(LexerTest, IntDotIdentifier) {
    auto toks = tokenize("5.double()");
    ASSERT_EQ(toks.size(), 6u); // Number, Dot, Ident, LParen, RParen, Eof
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "5");
    EXPECT_EQ(toks[1].kind, TokenKind::Dot);
    EXPECT_EQ(toks[2].kind, TokenKind::Ident);
    EXPECT_EQ(toks[2].value, "double");
    EXPECT_EQ(toks[3].kind, TokenKind::LParen);
    EXPECT_EQ(toks[4].kind, TokenKind::RParen);
    EXPECT_EQ(toks[5].kind, TokenKind::Eof);
}

TEST(LexerTest, IntDotDigitIsFloat) {
    auto toks = tokenize("3.14");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "3.14");
}

TEST(LexerTest, LeadingDotFloat) {
    {
        auto toks = tokenize(".5");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".5");
    }
    {
        auto toks = tokenize(".01");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".01");
    }
    {
        auto toks = tokenize(".5f64");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".5f64");
    }
    {
        auto toks = tokenize(".5f32");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".5f32");
    }
    {
        auto toks = tokenize(".5i32");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".5i32");
    }
    {
        auto toks = tokenize(".foo");
        ASSERT_EQ(toks.size(), 3u);
        EXPECT_EQ(toks[0].kind, TokenKind::Dot);
        EXPECT_EQ(toks[1].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].value, "foo");
    }
    {
        auto toks = tokenize("..5");
        ASSERT_EQ(toks.size(), 3u);
        EXPECT_EQ(toks[0].kind, TokenKind::DotDot);
        EXPECT_EQ(toks[1].kind, TokenKind::Number);
        EXPECT_EQ(toks[1].value, "5");
    }
    {
        auto toks = tokenize("t.0");
        ASSERT_EQ(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "t");
        EXPECT_EQ(toks[1].kind, TokenKind::Dot);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "0");
    }
}

TEST(LexerTest, DotAfterDotSuppressesFractionAbsorption) {
    // Nested tuple/record field access: a.0.0 must lex as five tokens, not
    // [Ident, Dot, Float("0.0")]. The integer literal '0' after the leading
    // '.' must NOT greedily absorb the trailing ".0" as a fraction part.
    {
        auto toks = tokenize("a.0.0");
        ASSERT_EQ(toks.size(), 6u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[0].value, "a");
        EXPECT_EQ(toks[1].kind, TokenKind::Dot);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "0");
        EXPECT_EQ(toks[3].kind, TokenKind::Dot);
        EXPECT_EQ(toks[4].kind, TokenKind::Number);
        EXPECT_EQ(toks[4].value, "0");
        EXPECT_EQ(toks[5].kind, TokenKind::Eof);
    }
    // Three-level chain: a.0.1.2 (5 numeric segments → 7 non-Eof tokens).
    {
        auto toks = tokenize("a.0.1.2");
        ASSERT_EQ(toks.size(), 8u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::Dot);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "0");
        EXPECT_EQ(toks[3].kind, TokenKind::Dot);
        EXPECT_EQ(toks[4].kind, TokenKind::Number);
        EXPECT_EQ(toks[4].value, "1");
        EXPECT_EQ(toks[5].kind, TokenKind::Dot);
        EXPECT_EQ(toks[6].kind, TokenKind::Number);
        EXPECT_EQ(toks[6].value, "2");
        EXPECT_EQ(toks[7].kind, TokenKind::Eof);
    }
    // Tuple literal directly followed by chained numeric field access.
    {
        auto toks = tokenize("(1,2).0.1");
        ASSERT_EQ(toks.size(), 10u);
        EXPECT_EQ(toks[0].kind, TokenKind::LParen);
        EXPECT_EQ(toks[1].kind, TokenKind::Number);
        EXPECT_EQ(toks[1].value, "1");
        EXPECT_EQ(toks[2].kind, TokenKind::Comma);
        EXPECT_EQ(toks[3].kind, TokenKind::Number);
        EXPECT_EQ(toks[3].value, "2");
        EXPECT_EQ(toks[4].kind, TokenKind::RParen);
        EXPECT_EQ(toks[5].kind, TokenKind::Dot);
        EXPECT_EQ(toks[6].kind, TokenKind::Number);
        EXPECT_EQ(toks[6].value, "0");
        EXPECT_EQ(toks[7].kind, TokenKind::Dot);
        EXPECT_EQ(toks[8].kind, TokenKind::Number);
        EXPECT_EQ(toks[8].value, "1");
        EXPECT_EQ(toks[9].kind, TokenKind::Eof);
    }
    // Non-regression: a regular float literal after an arithmetic operator
    // must still tokenize as Float, even when the LHS uses .index access.
    {
        auto toks = tokenize("a.1 + 1.5");
        ASSERT_EQ(toks.size(), 6u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::Dot);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "1");
        EXPECT_EQ(toks[3].kind, TokenKind::Plus);
        EXPECT_EQ(toks[4].kind, TokenKind::Float);
        EXPECT_EQ(toks[4].value, "1.5");
        EXPECT_EQ(toks[5].kind, TokenKind::Eof);
    }
    // Non-regression: leading-dot float `.5` after Plus must still tokenize
    // as Float, even when the LHS contains nested numeric field access.
    {
        auto toks = tokenize("a.0.0 + .5");
        ASSERT_EQ(toks.size(), 8u);
        EXPECT_EQ(toks[0].kind, TokenKind::Ident);
        EXPECT_EQ(toks[1].kind, TokenKind::Dot);
        EXPECT_EQ(toks[2].kind, TokenKind::Number);
        EXPECT_EQ(toks[2].value, "0");
        EXPECT_EQ(toks[3].kind, TokenKind::Dot);
        EXPECT_EQ(toks[4].kind, TokenKind::Number);
        EXPECT_EQ(toks[4].value, "0");
        EXPECT_EQ(toks[5].kind, TokenKind::Plus);
        EXPECT_EQ(toks[6].kind, TokenKind::Float);
        EXPECT_EQ(toks[6].value, ".5");
        EXPECT_EQ(toks[7].kind, TokenKind::Eof);
    }
}

TEST(LexerTest, NumericUnderscoreSeparators) {
    // Decimal integers
    {
        auto toks = tokenize("100_000");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "100_000");
    }
    {
        auto toks = tokenize("1_000_000");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "1_000_000");
    }
    // Hex
    {
        auto toks = tokenize("0xFF_FF");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0xFF_FF");
    }
    // Binary
    {
        auto toks = tokenize("0b1010_0101");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "0b1010_0101");
    }
    // Float
    {
        auto toks = tokenize("3.14_159");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, "3.14_159");
    }
    // Leading-dot float
    {
        auto toks = tokenize(".5_0");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, ".5_0");
    }
    // With suffix
    {
        auto toks = tokenize("100_000i32");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "100_000i32");
    }
    {
        auto toks = tokenize("3.14_159f64");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, "3.14_159f64");
    }
    // Invalid: consecutive underscores
    EXPECT_THROW(tokenize("100__000"), std::runtime_error);
    // Invalid: trailing underscore
    EXPECT_THROW(tokenize("100_"), std::runtime_error);
    // Invalid: underscore after hex prefix
    EXPECT_THROW(tokenize("0x_FF"), std::runtime_error);
    // Invalid: underscore after binary prefix
    EXPECT_THROW(tokenize("0b_1010"), std::runtime_error);
    // Invalid: trailing underscore in float fractional part
    EXPECT_THROW(tokenize("3.14_"), std::runtime_error);
}

TEST(LexerTest, InvalidTrailingAlphaAfterNumeric) {
    // Decimal integer followed by letter
    EXPECT_THROW(tokenize("1num"), std::runtime_error);
    EXPECT_THROW(tokenize("1abc"), std::runtime_error);
    EXPECT_THROW(tokenize("42x"), std::runtime_error);
    // Hex literal followed by invalid letter
    EXPECT_THROW(tokenize("0xFFgg"), std::runtime_error);
    // Binary literal followed by letter
    EXPECT_THROW(tokenize("0b101abc"), std::runtime_error);
    // Float followed by letter
    EXPECT_THROW(tokenize("3.14abc"), std::runtime_error);
    // Leading-dot float followed by letter
    EXPECT_THROW(tokenize(".5abc"), std::runtime_error);
    // Underscore-separated number followed by letter
    EXPECT_THROW(tokenize("100_000abc"), std::runtime_error);
    // Valid suffix followed by extra letter
    EXPECT_THROW(tokenize("42i32x"), std::runtime_error);
    EXPECT_THROW(tokenize("3.14f64z"), std::runtime_error);
    // Underscore immediately after number
    EXPECT_THROW(tokenize("42_abc"), std::runtime_error);

    // Valid cases: whitespace separates number from identifier
    {
        auto toks = tokenize("1 + num");
        ASSERT_EQ(toks.size(), 4u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "1");
        EXPECT_EQ(toks[2].kind, TokenKind::Ident);
        EXPECT_EQ(toks[2].value, "num");
    }
    // Valid: number with valid suffix
    {
        auto toks = tokenize("42i32");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Number);
        EXPECT_EQ(toks[0].value, "42i32");
    }
    {
        auto toks = tokenize("3.14f64");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Float);
        EXPECT_EQ(toks[0].value, "3.14f64");
    }
}

// ===== #1027 octal literal diagnostic =====

TEST(LexerTest, RejectOctalLiteralWithSuggestion) {
    auto expectOctalError = [](const std::string &src) {
        try {
            tokenize(src);
            FAIL() << "Expected octal-literal error for: " << src;
        } catch (const std::runtime_error &e) {
            std::string msg = e.what();
            EXPECT_NE(msg.find("octal literals (0o...) are not supported"), std::string::npos)
                << "Missing 'not supported' fragment in: " << msg;
            EXPECT_NE(msg.find("use hex (0x...) or binary (0b...) instead"), std::string::npos)
                << "Missing suggestion fragment in: " << msg;
        }
    };
    // Lowercase prefix
    expectOctalError("0o17");
    expectOctalError("0o755");
    expectOctalError("0o0");
    // Uppercase prefix
    expectOctalError("0O17");
    expectOctalError("0O755");
    // Bare prefix (no digits)
    expectOctalError("0o");
    expectOctalError("0O");
    // Non-octal digits after prefix — still rejected early
    expectOctalError("0o9");
    expectOctalError("0o89");

    // Line number appears in message
    try {
        tokenize("x = 1\n0o17\n");
        FAIL() << "Expected octal-literal error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("line 2"), std::string::npos) << "Expected line 2, got: " << msg;
        EXPECT_NE(msg.find("octal literals"), std::string::npos) << "Missing octal fragment, got: " << msg;
    }
}

// ===== #819 scientific notation =====

TEST(LexerTest, NumericLiteralScientificBasic) {
    auto toks = tokenize("1e10");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1e10");
}

TEST(LexerTest, NumericLiteralScientificUppercaseE) {
    auto toks = tokenize("1E10");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1E10");
}

TEST(LexerTest, NumericLiteralScientificPositiveSign) {
    auto toks = tokenize("1e+10");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1e+10");
}

TEST(LexerTest, NumericLiteralScientificNegativeSign) {
    auto toks = tokenize("1.5e-10");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1.5e-10");
}

TEST(LexerTest, NumericLiteralScientificWithFraction) {
    auto toks = tokenize("3.14e2");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "3.14e2");
}

TEST(LexerTest, NumericLiteralScientificWithUnderscoreInMantissa) {
    auto toks = tokenize("1_000e3");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1_000e3");
}

TEST(LexerTest, NumericLiteralScientificWithUnderscoreInExponent) {
    auto toks = tokenize("1e1_000");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1e1_000");
}

TEST(LexerTest, NumericLiteralScientificWithF32Suffix) {
    auto toks = tokenize("1e10f32");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, "1e10f32");
}

TEST(LexerTest, NumericLiteralScientificMissingDigitsThrows) {
    EXPECT_THROW(tokenize("1e"), std::runtime_error);
    EXPECT_THROW(tokenize("1e-"), std::runtime_error);
}

TEST(LexerTest, NumericLiteralIdentifierLikeEDoesNotSteal) {
    // Regression: `1exp` must NOT be tokenized as a scientific float.
    // The lexer should emit the existing "invalid character after numeric
    // literal" error because `e` is the start of an identifier.
    EXPECT_THROW(tokenize("1exp"), std::runtime_error);
}

TEST(LexerTest, NumericLiteralHexELettersUnchanged) {
    // Regression: hex literals use `e`/`E` as hex digits and must not enter
    // the scientific-notation branch.
    auto toks = tokenize("0xFE");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Number);
    EXPECT_EQ(toks[0].value, "0xFE");
}

TEST(LexerTest, NumericLiteralLeadingDotScientific) {
    // `.5e10` must be tokenized as a single Float, matching `0.5e10`.
    auto toks = tokenize(".5e10");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::Float);
    EXPECT_EQ(toks[0].value, ".5e10");
}
