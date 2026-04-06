#pragma once

#include <queue>
#include <string>
#include <vector>


namespace ry {

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
    NotIn,       // not in
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
    Semi,           // ;
    // --- インデント ---
    Indent,         // インデントレベル増加
    Dedent,         // インデントレベル減少
    // --- 制御構文 ---
    If,             // if
    Else,           // else
    When,           // when
    Match,          // match
    While,          // while
    // --- 関数定義 ---
    Fn,             // fn
    Return,         // return
    Arrow,          // ->
    FatArrow,       // =>
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
    // --- increment/decrement ---
    PlusPlus,         // ++
    MinusMinus,       // --
    // --- enum ---
    Enum,           // enum
    ColonColon,     // ::
    Case,           // case
    // --- test ---
    Expect,         // expect
    // --- contract (Design by Contract) ---
    Require,        // require
    Ensure,         // ensure
    Invariant,      // invariant
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
    Async,          // async
    Await,          // await
    // --- regex literal ---
    RegexLiteral,   // /pattern/
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
    bool consumeGreaterInTypeContext();

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
        TokenKind prev_kind;
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
    TokenKind prev_kind_ = TokenKind::Newline;

    Token readToken();
    Token readFStringSegment(bool isStart);
    void tryConsumeNumericSuffix(TokenKind &kind);
};

} // namespace ry
