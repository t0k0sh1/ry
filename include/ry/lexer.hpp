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
    GreaterGreaterGreater, // >>>
    Colon,          // :
    Let,            // let
    Var,            // var
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
    Record,         // record
    Operator,       // operator
    // --- list ---
    LBracket,       // [
    RBracket,       // ]
    // --- map ---
    LBrace,         // {
    RBrace,         // }
    // --- range ---
    DotDot,         // ..
    Ellipsis,       // ...
    // --- null coalescing ---
    QuestionQuestion, // ??
    NoneKw,         // none
    // --- for loop ---
    For,            // for
    In,             // in
    // --- loop control ---
    Break,          // break
    Continue,       // continue
    // --- compound assignment ---
    PlusEq,         // +=
    MinusEq,        // -=
    StarEq,         // *=
    SlashEq,        // /=
    PercentEq,      // %=
    SlashSlashEq,   // //=
    StarStarEq,     // **=
    AmpEq,          // &=
    PipeEq,         // |=
    CaretEq,        // ^=
    LessLessEq,     // <<=
    GreaterGreaterEq, // >>=
    // --- enum ---
    Enum,           // enum
    ColonColon,     // ::
    // --- match ---
    Match,          // match
    Case,           // case
    // --- test ---
    Expect,         // expect
    // --- contract (Design by Contract) ---
    Require,        // require
    Ensure,         // ensure
    Invariant,      // invariant
    Old,            // old
    Result,         // result
    // --- directive ---
    At,             // @
    // --- f-string ---
    FStringStart,   // f"...{
    FStringMid,     // }...{
    FStringEnd,     // }..." or f"..."
    // --- type cast ---
    As,             // as
    // --- Error type ---
    ErrorKw,        // Error
    BangBang,       // !!
    Question,       // ?
};

struct Token {
    TokenKind kind;
    std::string value;
    int line;
    int col = 0;
};

class Lexer {
public:
    explicit Lexer(std::string src) : src_(std::move(src)), pos_(0), line_(1), col_(1) {
        current_ = readToken();
    }

    const Token& peek() const { return current_; }
    Token next();

    // State save/restore for backtracking (used by lambda parsing)
    struct State {
        size_t pos;
        int line;
        int col;
        bool at_line_start;
        std::vector<int> indent_stack;
        std::queue<Token> pending;
        Token current;
        int fstring_brace_depth;
    };
    State saveState() const;
    void restoreState(State s);

private:
    std::string src_;
    size_t pos_;
    int line_;
    int col_;
    Token current_;

    bool at_line_start_ = true;
    std::vector<int> indent_stack_ = {0};
    std::queue<Token> pending_;
    int fstring_brace_depth_ = 0;

    Token readToken();
    Token readFStringSegment(bool isStart);
};
