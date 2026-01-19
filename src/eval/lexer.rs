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
            // Generate remaining DEDENTs
            while self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                self.pending_tokens.push(Token::Dedent);
            }
            if let Some(token) = self.pending_tokens.pop() {
                return Ok(token);
            }
            return Ok(Token::Eof);
        }

        // Parse other tokens
        self.parse_token()
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
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
            // Generate remaining DEDENTs
            while self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                self.pending_tokens.push(Token::Dedent);
            }
            if let Some(token) = self.pending_tokens.pop() {
                return Ok(token);
            }
            return Ok(Token::Eof);
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
                Ok(Token::Minus)
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
                    Err(EvalError::IndentationError(
                        "unexpected character: !".to_string(),
                    ))
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

            // Numbers
            '0'..='9' => self.parse_number(),

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.parse_identifier(),

            _ => Err(EvalError::IndentationError(format!(
                "unexpected character: {}",
                ch
            ))),
        }
    }

    fn parse_number(&mut self) -> Result<Token> {
        let start = self.position;
        let mut has_dot = false;

        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    self.advance();
                }
                '.' if !has_dot => {
                    has_dot = true;
                    self.advance();
                }
                _ => break,
            }
        }

        let num_str = &self.input[start..self.position];

        if has_dot {
            let value: f64 = num_str
                .parse()
                .map_err(|_| EvalError::IndentationError(format!("invalid number: {}", num_str)))?;
            Ok(Token::Number(Value::Float(value)))
        } else {
            let value: i64 = num_str
                .parse()
                .map_err(|_| EvalError::IndentationError(format!("invalid number: {}", num_str)))?;
            Ok(Token::Number(Value::Int(value)))
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
            "true" => Ok(Token::True),
            "false" => Ok(Token::False),
            "int" => Ok(Token::IntType),
            "float" => Ok(Token::FloatType),
            "bool" => Ok(Token::BoolType),
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
}
