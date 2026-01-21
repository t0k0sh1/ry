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
    Comma,  // ,
    Pipe,   // |

    // Keywords
    If,
    Elif,
    Else,
    While,
    True,
    False,
    Fn,     // fn
    Return, // return
    And,    // and
    Or,     // or
    Not,    // not

    // Arrow
    Arrow, // ->

    // Type keywords
    IntType,   // int
    FloatType, // float
    BoolType,  // bool
    StrType,   // str
    AnyType,   // any
    FuncType,  // func

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
                | Token::While
                | Token::True
                | Token::False
                | Token::Fn
                | Token::Return
                | Token::And
                | Token::Or
                | Token::Not
                | Token::IntType
                | Token::FloatType
                | Token::BoolType
                | Token::StrType
                | Token::AnyType
                | Token::FuncType
        )
    }
}
