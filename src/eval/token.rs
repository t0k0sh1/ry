use super::value::Value;

/// Token types for the lexer
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Number(Value),
    Ident(String),

    // Operators
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    StarStar,     // **
    SlashSlash,   // //
    Equal,        // =
    EqualEqual,   // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=

    // Delimiters
    LParen, // (
    RParen, // )
    Colon,  // :

    // Keywords
    If,
    Elif,
    Else,
    True,
    False,

    // Indentation
    Indent,
    Dedent,
    Newline,

    // End of file
    Eof,
}

impl Token {
    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::If | Token::Elif | Token::Else | Token::True | Token::False
        )
    }
}
