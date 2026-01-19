use super::value::Value;

/// Token types for the lexer
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Number(Value),
    StringLiteral(String),
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

    // Type keywords
    IntType,   // int
    FloatType, // float
    BoolType,  // bool
    StrType,   // str

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
            Token::If
                | Token::Elif
                | Token::Else
                | Token::True
                | Token::False
                | Token::IntType
                | Token::FloatType
                | Token::BoolType
                | Token::StrType
        )
    }
}
