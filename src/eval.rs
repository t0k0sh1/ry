#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Eof,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.input.get(self.position).copied()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Result<f64, String> {
        let start = self.position;
        let mut has_dot = false;

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch == '.' && !has_dot {
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let num_str: String = self.input[start..self.position].iter().collect();
        num_str
            .parse::<f64>()
            .map_err(|_| format!("Invalid number: {}", num_str))
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();

        match self.current_char() {
            None => Ok(Token::Eof),
            Some(ch) => {
                if ch.is_ascii_digit() || ch == '.' {
                    let num = self.read_number()?;
                    Ok(Token::Number(num))
                } else {
                    self.advance();
                    match ch {
                        '+' => Ok(Token::Plus),
                        '-' => Ok(Token::Minus),
                        '*' => Ok(Token::Multiply),
                        '/' => Ok(Token::Divide),
                        _ => Err(format!("Unexpected character: {}", ch)),
                    }
                }
            }
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(input: &str) -> Result<Self, String> {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
        })
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        match &self.current_token {
            Token::Number(n) => {
                let value = *n;
                self.advance()?;
                Ok(Expr::Number(value))
            }
            _ => Err("Expected a number".to_string()),
        }
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;

        loop {
            match &self.current_token {
                Token::Multiply => {
                    self.advance()?;
                    let right = self.parse_factor()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::Divide => {
                    self.advance()?;
                    let right = self.parse_factor()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Divide,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;

        loop {
            match &self.current_token {
                Token::Plus => {
                    self.advance()?;
                    let right = self.parse_term()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Add,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::Minus => {
                    self.advance()?;
                    let right = self.parse_term()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Subtract,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::Eof => break,
                _ => return Err("Unexpected token".to_string()),
            }
        }

        Ok(expr)
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        let expr = self.parse_expression()?;
        if !matches!(self.current_token, Token::Eof) {
            return Err("Unexpected token after expression".to_string());
        }
        Ok(expr)
    }
}

pub fn evaluate(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::BinaryOp { op, left, right } => {
            let left_val = evaluate(left)?;
            let right_val = evaluate(right)?;

            match op {
                BinaryOp::Add => Ok(left_val + right_val),
                BinaryOp::Subtract => Ok(left_val - right_val),
                BinaryOp::Multiply => Ok(left_val * right_val),
                BinaryOp::Divide => {
                    if right_val == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(left_val / right_val)
                    }
                }
            }
        }
    }
}

pub fn evaluate_expression(input: &str) -> Result<f64, String> {
    let mut parser = Parser::new(input)?;
    let expr = parser.parse()?;
    evaluate(&expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let mut lexer = Lexer::new("1+2");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(2.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_lexer_with_spaces() {
        let mut lexer = Lexer::new("1 + 2");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(2.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_lexer_all_operators() {
        let mut lexer = Lexer::new("1+2-3*4/5");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(2.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Minus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(3.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Multiply);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(4.0));
        assert_eq!(lexer.next_token().unwrap(), Token::Divide);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(5.0));
    }

    #[test]
    fn test_evaluate_addition() {
        assert_eq!(evaluate_expression("1+1").unwrap(), 2.0);
        assert_eq!(evaluate_expression("1 + 1").unwrap(), 2.0);
        assert_eq!(evaluate_expression("10+20").unwrap(), 30.0);
    }

    #[test]
    fn test_evaluate_subtraction() {
        assert_eq!(evaluate_expression("2-1").unwrap(), 1.0);
        assert_eq!(evaluate_expression("2 - 1").unwrap(), 1.0);
        assert_eq!(evaluate_expression("10-5").unwrap(), 5.0);
    }

    #[test]
    fn test_evaluate_multiplication() {
        assert_eq!(evaluate_expression("3*6").unwrap(), 18.0);
        assert_eq!(evaluate_expression("3 * 6").unwrap(), 18.0);
        assert_eq!(evaluate_expression("4*5").unwrap(), 20.0);
    }

    #[test]
    fn test_evaluate_division() {
        assert_eq!(evaluate_expression("5/10").unwrap(), 0.5);
        assert_eq!(evaluate_expression("5 / 10").unwrap(), 0.5);
        assert_eq!(evaluate_expression("10/2").unwrap(), 5.0);
    }

    #[test]
    fn test_operator_precedence() {
        // 2 * 3 + 4 = 6 + 4 = 10
        assert_eq!(evaluate_expression("2*3+4").unwrap(), 10.0);
        assert_eq!(evaluate_expression("2 * 3 + 4").unwrap(), 10.0);

        // 10 / 2 - 1 = 5 - 1 = 4
        assert_eq!(evaluate_expression("10/2-1").unwrap(), 4.0);
        assert_eq!(evaluate_expression("10 / 2 - 1").unwrap(), 4.0);

        // 1 + 2 * 3 = 1 + 6 = 7
        assert_eq!(evaluate_expression("1+2*3").unwrap(), 7.0);

        // 8 / 2 * 2 = 4 * 2 = 8 (left associative)
        assert_eq!(evaluate_expression("8/2*2").unwrap(), 8.0);
    }

    #[test]
    fn test_division_by_zero() {
        let result = evaluate_expression("5/0");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Division by zero"));
    }

    #[test]
    fn test_invalid_expression() {
        let result = evaluate_expression("1+");
        assert!(result.is_err());

        let result = evaluate_expression("+1");
        assert!(result.is_err());

        let result = evaluate_expression("1++2");
        assert!(result.is_err());
    }

    #[test]
    fn test_decimal_numbers() {
        assert_eq!(evaluate_expression("1.5+2.5").unwrap(), 4.0);
        assert_eq!(evaluate_expression("3.14*2").unwrap(), 6.28);
        assert_eq!(evaluate_expression("10.0/2.0").unwrap(), 5.0);
    }
}
