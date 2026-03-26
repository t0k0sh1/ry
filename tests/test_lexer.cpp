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
    // elif
    {
        auto toks = tokenize("elif");
        ASSERT_EQ(toks.size(), 2u);
        EXPECT_EQ(toks[0].kind, TokenKind::Elif);
        EXPECT_EQ(toks[0].value, "elif");
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
        auto toks1 = tokenize("/");
        EXPECT_EQ(toks1[0].kind, TokenKind::Slash);

        auto toks2 = tokenize("//");
        EXPECT_EQ(toks2[0].kind, TokenKind::SlashSlash);
        EXPECT_EQ(toks2[0].value, "//");
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

TEST(LexerTest, BangBangOperator) {
    auto toks = tokenize("!!");
    ASSERT_EQ(toks.size(), 2u);
    EXPECT_EQ(toks[0].kind, TokenKind::BangBang);
    EXPECT_EQ(toks[0].value, "!!");
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
