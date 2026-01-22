use super::error::{EvalError, Result};
use super::token::Token;
use super::value::Value;

/// Indent-aware lexer for the ry language
pub struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    position: usize,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    pending_tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            position: 0,
            indent_stack: vec![0],
            at_line_start: true,
            pending_tokens: Vec::new(),
        }
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token == Token::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Result<Token> {
        // Return pending tokens first (DEDENTs)
        if let Some(token) = self.pending_tokens.pop() {
            return Ok(token);
        }

        // Handle indentation at line start
        if self.at_line_start {
            return self.handle_indentation();
        }

        // Skip whitespace (but not newlines)
        self.skip_whitespace();

        // Check for newline
        if let Some('\n') = self.peek() {
            self.advance();
            self.at_line_start = true;
            return Ok(Token::Newline);
        }

        // Check for EOF
        if self.peek().is_none() {
            self.generate_remaining_dedents();
            return Ok(self.pending_or_eof());
        }

        // Parse other tokens
        self.parse_token()
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    /// Peek at the next character (one position ahead of peek)
    fn peek_next_char(&self) -> Option<char> {
        self.input[self.position..].chars().nth(1)
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if ch.is_some() {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Generate remaining DEDENTs for all open indentation levels
    fn generate_remaining_dedents(&mut self) {
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            self.pending_tokens.push(Token::Dedent);
        }
    }

    /// Return a pending token if available, otherwise EOF
    fn pending_or_eof(&mut self) -> Token {
        self.pending_tokens.pop().unwrap_or(Token::Eof)
    }

    fn handle_indentation(&mut self) -> Result<Token> {
        self.at_line_start = false;

        // Count leading spaces
        let mut indent = 0;
        while let Some(ch) = self.peek() {
            match ch {
                ' ' => {
                    indent += 1;
                    self.advance();
                }
                '\t' => {
                    // Treat tab as 2 spaces (matching project convention)
                    indent += 2;
                    self.advance();
                }
                '\n' => {
                    // Empty line, skip
                    self.advance();
                    self.at_line_start = true;
                    return self.next_token();
                }
                '#' => {
                    // Comment line, skip to end
                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.advance();
                    }
                    if self.peek().is_some() {
                        self.advance(); // consume newline
                    }
                    self.at_line_start = true;
                    return self.next_token();
                }
                _ => break,
            }
        }

        // Check for EOF at line start
        if self.peek().is_none() {
            self.generate_remaining_dedents();
            return Ok(self.pending_or_eof());
        }

        let current_indent = *self.indent_stack.last().unwrap();

        if indent > current_indent {
            // Indent
            self.indent_stack.push(indent);
            Ok(Token::Indent)
        } else if indent < current_indent {
            // Dedent (possibly multiple)
            while let Some(&top) = self.indent_stack.last() {
                if top > indent {
                    self.indent_stack.pop();
                    self.pending_tokens.push(Token::Dedent);
                } else {
                    break;
                }
            }

            // Check for invalid indent level
            if *self.indent_stack.last().unwrap() != indent {
                return Err(EvalError::IndentationError(
                    "unindent does not match any outer indentation level".to_string(),
                ));
            }

            // Return first DEDENT, rest are pending
            self.pending_tokens.pop().ok_or_else(|| {
                EvalError::IndentationError("unexpected indentation error".to_string())
            })
        } else {
            // Same level, continue parsing
            self.parse_token()
        }
    }

    fn parse_token(&mut self) -> Result<Token> {
        let ch = match self.peek() {
            Some(ch) => ch,
            None => return Ok(Token::Eof),
        };

        match ch {
            // Single-character tokens
            '+' => {
                self.advance();
                Ok(Token::Plus)
            }
            '-' => {
                self.advance();
                if self.peek() == Some('>') {
                    self.advance();
                    Ok(Token::Arrow)
                } else {
                    Ok(Token::Minus)
                }
            }
            '%' => {
                self.advance();
                Ok(Token::Percent)
            }
            '(' => {
                self.advance();
                Ok(Token::LParen)
            }
            ')' => {
                self.advance();
                Ok(Token::RParen)
            }
            ':' => {
                self.advance();
                Ok(Token::Colon)
            }
            ',' => {
                self.advance();
                Ok(Token::Comma)
            }
            '|' => {
                self.advance();
                Ok(Token::Pipe)
            }

            // Two-character operators
            '*' => {
                self.advance();
                if self.peek() == Some('*') {
                    self.advance();
                    Ok(Token::StarStar)
                } else {
                    Ok(Token::Star)
                }
            }
            '/' => {
                self.advance();
                if self.peek() == Some('/') {
                    self.advance();
                    Ok(Token::SlashSlash)
                } else {
                    Ok(Token::Slash)
                }
            }
            '=' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::EqualEqual)
                } else {
                    Ok(Token::Equal)
                }
            }
            '!' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::NotEqual)
                } else {
                    Err(EvalError::LexerError("unexpected character: !".to_string()))
                }
            }
            '<' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            }
            '>' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            }

            // Dot (for UFCS method call syntax)
            '.' => {
                self.advance();
                Ok(Token::Dot)
            }

            // Numbers
            '0'..='9' => self.parse_number(),

            // String literals
            '"' => self.parse_string(),

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.parse_identifier(),

            _ => Err(EvalError::LexerError(format!(
                "unexpected character: {}",
                ch
            ))),
        }
    }

    fn parse_number(&mut self) -> Result<Token> {
        let start = self.position;

        // Check for special prefixes (0x, 0o, 0b)
        if self.peek() == Some('0') {
            self.advance();
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    return self.parse_hex_number();
                }
                Some('o') | Some('O') => {
                    self.advance();
                    return self.parse_octal_number();
                }
                Some('b') | Some('B') => {
                    self.advance();
                    return self.parse_binary_number();
                }
                _ => {
                    // Continue parsing as decimal (might be 0, 0.5, 0e1, etc.)
                }
            }
        }

        let mut has_dot = false;
        let mut has_exponent = false;

        // Continue from current position (after initial '0' if present, or from start)
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    self.advance();
                }
                '.' if !has_dot && !has_exponent => {
                    // Look ahead to distinguish float (10.5) from UFCS (10.add())
                    if let Some(next_char) = self.peek_next_char() {
                        if next_char.is_ascii_digit() {
                            // Float literal: 10.5
                            has_dot = true;
                            self.advance();
                        } else {
                            // UFCS syntax: 10.add() - stop number parsing
                            break;
                        }
                    } else {
                        // No next char after '.', stop number parsing
                        break;
                    }
                }
                'e' | 'E' if !has_exponent => {
                    has_exponent = true;
                    self.advance();
                    // Handle optional sign after exponent
                    if let Some('+' | '-') = self.peek() {
                        self.advance();
                    }
                    // Must have at least one digit after exponent
                    if !matches!(self.peek(), Some('0'..='9')) {
                        return Err(EvalError::LexerError(
                            "invalid number: expected digit after exponent".to_string(),
                        ));
                    }
                }
                _ => break,
            }
        }

        let num_str = &self.input[start..self.position];

        // Scientific notation always results in float
        if has_dot || has_exponent {
            let value: f64 = num_str
                .parse()
                .map_err(|_| EvalError::LexerError(format!("invalid number: {}", num_str)))?;
            Ok(Token::Number(Value::Float(value)))
        } else {
            let value: i64 = num_str
                .parse()
                .map_err(|_| EvalError::LexerError(format!("invalid number: {}", num_str)))?;
            Ok(Token::Number(Value::Int(value)))
        }
    }

    fn parse_hex_number(&mut self) -> Result<Token> {
        let start = self.position;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }
        let hex_str = &self.input[start..self.position];
        if hex_str.is_empty() {
            return Err(EvalError::LexerError(
                "invalid number: expected hex digit after 0x".to_string(),
            ));
        }
        let value = i64::from_str_radix(hex_str, 16)
            .map_err(|_| EvalError::LexerError(format!("invalid hex number: 0x{}", hex_str)))?;
        Ok(Token::Number(Value::Int(value)))
    }

    fn parse_octal_number(&mut self) -> Result<Token> {
        let start = self.position;
        while let Some(ch) = self.peek() {
            if ('0'..='7').contains(&ch) {
                self.advance();
            } else {
                break;
            }
        }
        let oct_str = &self.input[start..self.position];
        if oct_str.is_empty() {
            return Err(EvalError::LexerError(
                "invalid number: expected octal digit after 0o".to_string(),
            ));
        }
        let value = i64::from_str_radix(oct_str, 8)
            .map_err(|_| EvalError::LexerError(format!("invalid octal number: 0o{}", oct_str)))?;
        Ok(Token::Number(Value::Int(value)))
    }

    fn parse_binary_number(&mut self) -> Result<Token> {
        let start = self.position;
        while let Some(ch) = self.peek() {
            if ch == '0' || ch == '1' {
                self.advance();
            } else {
                break;
            }
        }
        let bin_str = &self.input[start..self.position];
        if bin_str.is_empty() {
            return Err(EvalError::LexerError(
                "invalid number: expected binary digit after 0b".to_string(),
            ));
        }
        let value = i64::from_str_radix(bin_str, 2)
            .map_err(|_| EvalError::LexerError(format!("invalid binary number: 0b{}", bin_str)))?;
        Ok(Token::Number(Value::Int(value)))
    }

    fn parse_string(&mut self) -> Result<Token> {
        // Consume opening quote
        self.advance();

        let mut result = String::new();

        loop {
            match self.peek() {
                Some('"') => {
                    // End of string
                    self.advance();
                    return Ok(Token::StringLiteral(result));
                }
                Some('\\') => {
                    // Escape sequence
                    self.advance();
                    match self.peek() {
                        Some('n') => {
                            self.advance();
                            result.push('\n');
                        }
                        Some('t') => {
                            self.advance();
                            result.push('\t');
                        }
                        Some('r') => {
                            self.advance();
                            result.push('\r');
                        }
                        Some('\\') => {
                            self.advance();
                            result.push('\\');
                        }
                        Some('"') => {
                            self.advance();
                            result.push('"');
                        }
                        Some('u') => {
                            self.advance();
                            // Parse 4 hex digits
                            let mut hex_str = String::with_capacity(4);
                            for _ in 0..4 {
                                match self.peek() {
                                    Some(ch) if ch.is_ascii_hexdigit() => {
                                        hex_str.push(ch);
                                        self.advance();
                                    }
                                    _ => {
                                        return Err(EvalError::LexerError(
                                            "invalid unicode escape: expected 4 hex digits"
                                                .to_string(),
                                        ));
                                    }
                                }
                            }
                            let code_point = u32::from_str_radix(&hex_str, 16).unwrap();
                            match char::from_u32(code_point) {
                                Some(ch) => result.push(ch),
                                None => {
                                    return Err(EvalError::LexerError(format!(
                                        "invalid unicode code point: \\u{}",
                                        hex_str
                                    )));
                                }
                            }
                        }
                        Some(ch) => {
                            return Err(EvalError::LexerError(format!(
                                "unknown escape sequence: \\{}",
                                ch
                            )));
                        }
                        None => {
                            return Err(EvalError::LexerError(
                                "unterminated string literal".to_string(),
                            ));
                        }
                    }
                }
                Some('\n') | None => {
                    return Err(EvalError::LexerError(
                        "unterminated string literal".to_string(),
                    ));
                }
                Some(ch) => {
                    self.advance();
                    result.push(ch);
                }
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Token> {
        let start = self.position;

        while let Some(ch) = self.peek() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.advance();
                }
                _ => break,
            }
        }

        let ident = &self.input[start..self.position];

        // Check for keywords
        match ident {
            "if" => Ok(Token::If),
            "elif" => Ok(Token::Elif),
            "else" => Ok(Token::Else),
            "while" => Ok(Token::While),
            "true" => Ok(Token::True),
            "false" => Ok(Token::False),
            "fn" => Ok(Token::Fn),
            "return" => Ok(Token::Return),
            "and" => Ok(Token::And),
            "or" => Ok(Token::Or),
            "not" => Ok(Token::Not),
            "int" => Ok(Token::IntType),
            "float" => Ok(Token::FloatType),
            "bool" => Ok(Token::BoolType),
            "str" => Ok(Token::StrType),
            "any" => Ok(Token::AnyType),
            "func" => Ok(Token::FuncType),
            _ => Ok(Token::Ident(ident.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_expression() {
        let mut lexer = Lexer::new("1 + 2");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(1)),
                Token::Plus,
                Token::Number(Value::Int(2)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("if elif else true false");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::Elif,
                Token::Else,
                Token::True,
                Token::False,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / % ** // = == != < > <= >=");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Percent,
                Token::StarStar,
                Token::SlashSlash,
                Token::Equal,
                Token::EqualEqual,
                Token::NotEqual,
                Token::Less,
                Token::Greater,
                Token::LessEqual,
                Token::GreaterEqual,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_indentation() {
        let input = "if true:\n    x = 1\n    y = 2\nz = 3";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        // Expected: If, True, Colon, Newline, Indent, Ident(x), Equal, Number(1), Newline,
        //           Ident(y), Equal, Number(2), Newline, Dedent, Ident(z), Equal, Number(3), Eof
        assert!(tokens.contains(&Token::Indent));
        assert!(tokens.contains(&Token::Dedent));
    }

    #[test]
    fn test_nested_indentation() {
        let input = "if true:\n    if false:\n        x = 1\ny = 2";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        // Should have 2 Indents and 2 Dedents
        let indent_count = tokens.iter().filter(|t| **t == Token::Indent).count();
        let dedent_count = tokens.iter().filter(|t| **t == Token::Dedent).count();
        assert_eq!(indent_count, 2);
        assert_eq!(dedent_count, 2);
    }

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("my_var x123 _private");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("my_var".to_string()),
                Token::Ident("x123".to_string()),
                Token::Ident("_private".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_float_number() {
        let mut lexer = Lexer::new("3.14 2.0");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Float(3.14)),
                Token::Number(Value::Float(2.0)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_scientific_notation() {
        let mut lexer = Lexer::new("1e10 2.5e-3 1E+6");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Float(1e10)),
                Token::Number(Value::Float(2.5e-3)),
                Token::Number(Value::Float(1e6)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_scientific_notation_error() {
        let mut lexer = Lexer::new("1e");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected digit after exponent"));
    }

    #[test]
    fn test_hex_number() {
        let mut lexer = Lexer::new("0xFF 0x10 0XAB");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(255)),
                Token::Number(Value::Int(16)),
                Token::Number(Value::Int(171)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_hex_number_lowercase() {
        let mut lexer = Lexer::new("0xabcdef");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::Number(Value::Int(0xabcdef)), Token::Eof,]
        );
    }

    #[test]
    fn test_hex_number_error() {
        let mut lexer = Lexer::new("0x");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected hex digit after 0x"));
    }

    #[test]
    fn test_octal_number() {
        let mut lexer = Lexer::new("0o77 0o10 0O755");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(63)),
                Token::Number(Value::Int(8)),
                Token::Number(Value::Int(493)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_octal_number_error() {
        let mut lexer = Lexer::new("0o");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected octal digit after 0o"));
    }

    #[test]
    fn test_binary_number() {
        let mut lexer = Lexer::new("0b1010 0b0 0B1111");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(10)),
                Token::Number(Value::Int(0)),
                Token::Number(Value::Int(15)),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_binary_number_error() {
        let mut lexer = Lexer::new("0b");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected binary digit after 0b"));
    }

    #[test]
    fn test_zero_literal() {
        // Ensure plain 0 still works
        let mut lexer = Lexer::new("0");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(Value::Int(0)), Token::Eof,]);
    }

    #[test]
    fn test_zero_with_decimal() {
        // Ensure 0.5 still works
        let mut lexer = Lexer::new("0.5");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(Value::Float(0.5)), Token::Eof,]);
    }

    #[test]
    fn test_string_literal() {
        let mut lexer = Lexer::new("\"hello\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("hello".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_string_with_spaces() {
        let mut lexer = Lexer::new("\"hello world\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("hello world".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_string_escape_sequences() {
        let mut lexer = Lexer::new("\"hello\\nworld\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("hello\nworld".to_string()), Token::Eof,]
        );

        let mut lexer = Lexer::new("\"tab\\there\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("tab\there".to_string()), Token::Eof,]
        );

        let mut lexer = Lexer::new("\"quote\\\"here\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("quote\"here".to_string()), Token::Eof,]
        );

        let mut lexer = Lexer::new("\"backslash\\\\here\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral("backslash\\here".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_unicode_escape_basic() {
        let mut lexer = Lexer::new("\"\\u0041\""); // A
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("A".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_unicode_escape_japanese() {
        let mut lexer = Lexer::new("\"\\u3042\""); // あ (hiragana a)
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("あ".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_unicode_escape_in_string() {
        let mut lexer = Lexer::new("\"hello\\u0020world\""); // space
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("hello world".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_unicode_escape_multiple() {
        let mut lexer = Lexer::new("\"\\u0048\\u0069\""); // Hi
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("Hi".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_unicode_escape_too_few_digits() {
        let mut lexer = Lexer::new("\"\\u00\"");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected 4 hex digits"));
    }

    #[test]
    fn test_unicode_escape_invalid_hex() {
        let mut lexer = Lexer::new("\"\\uGGGG\"");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("expected 4 hex digits"));
    }

    #[test]
    fn test_unicode_escape_surrogate() {
        // Surrogates (0xD800-0xDFFF) are invalid Unicode code points
        let mut lexer = Lexer::new("\"\\uD800\"");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("invalid unicode code point"));
    }

    #[test]
    fn test_empty_string() {
        let mut lexer = Lexer::new("\"\"");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("".to_string()), Token::Eof,]
        );
    }

    #[test]
    fn test_unterminated_string() {
        let mut lexer = Lexer::new("\"hello");
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_unknown_escape_sequence() {
        let mut lexer = Lexer::new("\"hello\\x\"");
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_str_type_keyword() {
        let mut lexer = Lexer::new("str");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::StrType, Token::Eof,]);
    }

    #[test]
    fn test_any_type_keyword() {
        let mut lexer = Lexer::new("any");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::AnyType, Token::Eof,]);
    }

    #[test]
    fn test_fn_keyword() {
        let mut lexer = Lexer::new("fn");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Fn, Token::Eof]);
    }

    #[test]
    fn test_return_keyword() {
        let mut lexer = Lexer::new("return");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Return, Token::Eof]);
    }

    #[test]
    fn test_arrow_operator() {
        let mut lexer = Lexer::new("->");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Arrow, Token::Eof]);
    }

    #[test]
    fn test_function_definition_tokens() {
        let mut lexer = Lexer::new("fn add(a: int, b: int) -> int:");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Ident("a".to_string()),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Ident("b".to_string()),
                Token::Colon,
                Token::IntType,
                Token::RParen,
                Token::Arrow,
                Token::IntType,
                Token::Colon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_dot_token() {
        let mut lexer = Lexer::new(".");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Dot, Token::Eof]);
    }

    #[test]
    fn test_ufcs_lexing() {
        // 10.add(20) should tokenize as: Number(10), Dot, Ident("add"), LParen, Number(20), RParen
        let mut lexer = Lexer::new("10.add(20)");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(10)),
                Token::Dot,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Number(Value::Int(20)),
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_ufcs_vs_float_literal() {
        // 10.5 should be a float literal
        let mut lexer = Lexer::new("10.5");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(Value::Float(10.5)), Token::Eof]);

        // 10.add should be UFCS
        let mut lexer = Lexer::new("10.add");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(10)),
                Token::Dot,
                Token::Ident("add".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_chained_ufcs_lexing() {
        // 10.add(5).mul(2)
        let mut lexer = Lexer::new("10.add(5).mul(2)");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(Value::Int(10)),
                Token::Dot,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Number(Value::Int(5)),
                Token::RParen,
                Token::Dot,
                Token::Ident("mul".to_string()),
                Token::LParen,
                Token::Number(Value::Int(2)),
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_ufcs_on_expression() {
        // (1 + 2).double()
        let mut lexer = Lexer::new("(1 + 2).double()");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Number(Value::Int(1)),
                Token::Plus,
                Token::Number(Value::Int(2)),
                Token::RParen,
                Token::Dot,
                Token::Ident("double".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Eof,
            ]
        );
    }
}
