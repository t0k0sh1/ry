#pragma once

#include <queue>
#include <string>
#include <vector>

enum class TokenKind {
    Number, Float, String, Ident, Plus, Minus, Star, Slash, Equals,
    LParen, RParen, Comma, Newline, Eof, Error,
    Percent,     // %
    StarStar,    // **
    SlashSlash,  // //
    EqEq,        // ==
    BangEq,      // !=
    Less,        // <
    LessEq,      // <=
    Greater,     // >
    GreaterEq,   // >=
    And,         // and
    Or,          // or
    Not,         // not
    True,        // true
    False,       // false
    // --- ビット演算子 ---
    Amp,            // &
    Pipe,           // |
    Caret,          // ^
    Tilde,          // ~
    LessLess,       // <<
    GreaterGreater, // >>
    Colon,          // :
    Let,            // let
    Const,          // const
    // --- インデント ---
    Indent,         // インデントレベル増加
    Dedent,         // インデントレベル減少
    // --- 制御構文 ---
    If,             // if
    Elif,           // elif
    Else,           // else
    While,          // while
    // --- 関数定義 ---
    Fn,             // fn
    Return,         // return
    Arrow,          // ->
    // --- import ---
    From,           // from
    Import,         // import
    Dot,            // .
    // --- type ---
    Type,           // type
    // --- list ---
    LBracket,       // [
    RBracket,       // ]
    // --- map ---
    LBrace,         // {
    RBrace,         // }
};

struct Token {
    TokenKind kind;
    std::string value;
    int line;
};

class Lexer {
public:
    explicit Lexer(std::string src) : src_(std::move(src)), pos_(0), line_(1) {
        current_ = readToken();
    }

    const Token& peek() const { return current_; }
    Token next();

private:
    std::string src_;
    size_t pos_;
    int line_;
    Token current_;

    bool at_line_start_ = true;
    std::vector<int> indent_stack_ = {0};
    std::queue<Token> pending_;

    Token readToken();
};
