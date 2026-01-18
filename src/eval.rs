use std::fmt;
use std::ops::{Add, Mul, Sub};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => {
                // 整数として表示できる場合は整数として表示
                if *fl == (*fl as i64 as f64) {
                    write!(f, "{}", *fl as i64)
                } else {
                    write!(f, "{}", fl)
                }
            }
        }
    }
}

impl Value {
    /// 整数を浮動小数点数に変換
    pub fn promote_to_float(self) -> Self {
        match self {
            Value::Int(i) => Value::Float(i as f64),
            Value::Float(f) => Value::Float(f),
        }
    }

    /// 加算（内部実装）
    fn add_impl(self, other: Self) -> Self {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                // オーバーフローチェック
                match a.checked_add(b) {
                    Some(result) => Value::Int(result),
                    None => Value::Float(a as f64 + b as f64),
                }
            }
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
        }
    }

    /// 減算（内部実装）
    fn subtract_impl(self, other: Self) -> Self {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => match a.checked_sub(b) {
                Some(result) => Value::Int(result),
                None => Value::Float(a as f64 - b as f64),
            },
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
        }
    }

    /// 乗算（内部実装）
    fn multiply_impl(self, other: Self) -> Self {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => match a.checked_mul(b) {
                Some(result) => Value::Int(result),
                None => Value::Float(a as f64 * b as f64),
            },
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
        }
    }

    /// 除算（除算は常に浮動小数点数を返す）
    pub fn divide(self, other: Self) -> Result<Self, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a as f64 / b as f64))
                }
            }
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a as f64 / b))
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a / b as f64))
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a / b))
                }
            }
        }
    }

    /// f64に変換（後方互換性のため）
    pub fn to_f64(self) -> f64 {
        match self {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
        }
    }

    /// 剰余算
    pub fn modulo(self, other: Self) -> Result<Self, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Int(a % b))
                }
            }
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a as f64 % b))
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a % b as f64))
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a % b))
                }
            }
        }
    }

    /// 冪乗（指数に浮動小数点数を指定可能）
    pub fn power(self, other: Self) -> Result<Self, String> {
        let base = self.to_f64();
        let exponent = other.to_f64();
        Ok(Value::Float(base.powf(exponent)))
    }

    /// 切り捨て除算
    pub fn floor_divide(self, other: Self) -> Result<Self, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float((a as f64 / b).floor()))
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float((a / b as f64).floor()))
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float((a / b).floor()))
                }
            }
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        self.add_impl(other)
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        self.subtract_impl(other)
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        self.multiply_impl(other)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(Value),
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Power,
    FloorDivide,
    Eof,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(Value),
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
    Modulo,
    Power,
    FloorDivide,
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

    fn read_number(&mut self) -> Result<Value, String> {
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

        if has_dot {
            // 浮動小数点数として解析
            num_str
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| format!("Invalid number: {}", num_str))
        } else {
            // 整数として解析
            num_str
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| format!("Invalid number: {}", num_str))
        }
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
                        '*' => {
                            // ** をチェック
                            if let Some('*') = self.current_char() {
                                self.advance();
                                Ok(Token::Power)
                            } else {
                                Ok(Token::Multiply)
                            }
                        }
                        '/' => {
                            // // をチェック
                            if let Some('/') = self.current_char() {
                                self.advance();
                                Ok(Token::FloorDivide)
                            } else {
                                Ok(Token::Divide)
                            }
                        }
                        '%' => Ok(Token::Modulo),
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
                let value = n.clone();
                self.advance()?;
                Ok(Expr::Number(value))
            }
            _ => Err("Expected a number".to_string()),
        }
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;

        // 冪乗は右結合
        while matches!(&self.current_token, Token::Power) {
            self.advance()?;
            let right = self.parse_power()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::Power,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_power()?;

        loop {
            match &self.current_token {
                Token::Multiply => {
                    self.advance()?;
                    let right = self.parse_power()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::Divide => {
                    self.advance()?;
                    let right = self.parse_power()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Divide,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::Modulo => {
                    self.advance()?;
                    let right = self.parse_power()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::Modulo,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                Token::FloorDivide => {
                    self.advance()?;
                    let right = self.parse_power()?;
                    expr = Expr::BinaryOp {
                        op: BinaryOp::FloorDivide,
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

pub fn evaluate(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => Ok(n.clone()),
        Expr::BinaryOp { op, left, right } => {
            let left_val = evaluate(left)?;
            let right_val = evaluate(right)?;

            match op {
                BinaryOp::Add => Ok(left_val + right_val),
                BinaryOp::Subtract => Ok(left_val - right_val),
                BinaryOp::Multiply => Ok(left_val * right_val),
                BinaryOp::Divide => left_val.divide(right_val),
                BinaryOp::Modulo => left_val.modulo(right_val),
                BinaryOp::Power => left_val.power(right_val),
                BinaryOp::FloorDivide => left_val.floor_divide(right_val),
            }
        }
    }
}

pub fn evaluate_expression(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input)?;
    let expr = parser.parse()?;
    evaluate(&expr)
}

/// 後方互換性のためのf64を返すラッパー関数
pub fn evaluate_expression_f64(input: &str) -> Result<f64, String> {
    evaluate_expression(input).map(|v| v.to_f64())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let mut lexer = Lexer::new("1+2");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(1)));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(2)));
        assert_eq!(lexer.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_lexer_with_spaces() {
        let mut lexer = Lexer::new("1 + 2");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(1)));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(2)));
        assert_eq!(lexer.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_lexer_all_operators() {
        let mut lexer = Lexer::new("1+2-3*4/5");
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(1)));
        assert_eq!(lexer.next_token().unwrap(), Token::Plus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(2)));
        assert_eq!(lexer.next_token().unwrap(), Token::Minus);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(3)));
        assert_eq!(lexer.next_token().unwrap(), Token::Multiply);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(4)));
        assert_eq!(lexer.next_token().unwrap(), Token::Divide);
        assert_eq!(lexer.next_token().unwrap(), Token::Number(Value::Int(5)));
    }

    #[test]
    fn test_evaluate_addition() {
        assert_eq!(evaluate_expression("1+1").unwrap(), Value::Int(2));
        assert_eq!(evaluate_expression("1 + 1").unwrap(), Value::Int(2));
        assert_eq!(evaluate_expression("10+20").unwrap(), Value::Int(30));
    }

    #[test]
    fn test_evaluate_subtraction() {
        assert_eq!(evaluate_expression("2-1").unwrap(), Value::Int(1));
        assert_eq!(evaluate_expression("2 - 1").unwrap(), Value::Int(1));
        assert_eq!(evaluate_expression("10-5").unwrap(), Value::Int(5));
    }

    #[test]
    fn test_evaluate_multiplication() {
        assert_eq!(evaluate_expression("3*6").unwrap(), Value::Int(18));
        assert_eq!(evaluate_expression("3 * 6").unwrap(), Value::Int(18));
        assert_eq!(evaluate_expression("4*5").unwrap(), Value::Int(20));
    }

    #[test]
    fn test_evaluate_division() {
        assert_eq!(evaluate_expression("5/10").unwrap(), Value::Float(0.5));
        assert_eq!(evaluate_expression("5 / 10").unwrap(), Value::Float(0.5));
        // 除算は常にFloatを返す
        assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_operator_precedence() {
        // 2 * 3 + 4 = 6 + 4 = 10
        assert_eq!(evaluate_expression("2*3+4").unwrap(), Value::Int(10));
        assert_eq!(evaluate_expression("2 * 3 + 4").unwrap(), Value::Int(10));

        // 10 / 2 - 1 = 5.0 - 1 = 4.0 (除算の結果はFloat)
        assert_eq!(evaluate_expression("10/2-1").unwrap(), Value::Float(4.0));
        assert_eq!(
            evaluate_expression("10 / 2 - 1").unwrap(),
            Value::Float(4.0)
        );

        // 1 + 2 * 3 = 1 + 6 = 7
        assert_eq!(evaluate_expression("1+2*3").unwrap(), Value::Int(7));

        // 8 / 2 * 2 = 4.0 * 2 = 8.0 (除算の結果はFloat)
        assert_eq!(evaluate_expression("8/2*2").unwrap(), Value::Float(8.0));
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
        assert_eq!(evaluate_expression("1.5+2.5").unwrap(), Value::Float(4.0));
        assert_eq!(evaluate_expression("3.14*2").unwrap(), Value::Float(6.28));
        assert_eq!(evaluate_expression("10.0/2.0").unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_type_promotion_int_to_float() {
        // 整数 + 浮動小数点数 = 浮動小数点数
        assert_eq!(evaluate_expression("1+1.5").unwrap(), Value::Float(2.5));
        assert_eq!(evaluate_expression("1.5+1").unwrap(), Value::Float(2.5));

        // 整数 - 浮動小数点数 = 浮動小数点数
        assert_eq!(evaluate_expression("5-2.5").unwrap(), Value::Float(2.5));
        assert_eq!(evaluate_expression("5.5-2").unwrap(), Value::Float(3.5));

        // 整数 * 浮動小数点数 = 浮動小数点数
        assert_eq!(evaluate_expression("2*1.5").unwrap(), Value::Float(3.0));
        assert_eq!(evaluate_expression("2.5*2").unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_integer_operations() {
        // 整数同士の加算、減算、乗算は整数を返す
        assert_eq!(evaluate_expression("1+2").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("5-2").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("2*3").unwrap(), Value::Int(6));

        // 整数同士の除算は浮動小数点数を返す
        assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
        assert_eq!(evaluate_expression("7/2").unwrap(), Value::Float(3.5));
    }

    #[test]
    fn test_evaluate_modulo() {
        assert_eq!(evaluate_expression("10%3").unwrap(), Value::Int(1));
        assert_eq!(evaluate_expression("10 % 3").unwrap(), Value::Int(1));
        assert_eq!(evaluate_expression("15%4").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("10.5%3").unwrap(), Value::Float(1.5));
    }

    #[test]
    fn test_evaluate_power() {
        assert_eq!(evaluate_expression("2**3").unwrap(), Value::Float(8.0));
        assert_eq!(evaluate_expression("2 ** 3").unwrap(), Value::Float(8.0));
        assert_eq!(evaluate_expression("3**2").unwrap(), Value::Float(9.0));
        assert_eq!(
            evaluate_expression("2**3.5").unwrap(),
            Value::Float(11.313708498984761)
        );
        assert_eq!(evaluate_expression("10**0").unwrap(), Value::Float(1.0));
    }

    #[test]
    fn test_evaluate_floor_divide() {
        assert_eq!(evaluate_expression("10//3").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("10 // 3").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("7//2").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("10.7//3").unwrap(), Value::Float(3.0));
        assert_eq!(evaluate_expression("10.7//3.2").unwrap(), Value::Float(3.0));
    }

    #[test]
    fn test_new_operator_precedence() {
        // 2 ** 3 + 4 = 8 + 4 = 12.0 (冪乗が最高優先度)
        assert_eq!(evaluate_expression("2**3+4").unwrap(), Value::Float(12.0));

        // 2 * 3 % 2 = 6 % 2 = 0 (乗算と剰余算は同じ優先度)
        assert_eq!(evaluate_expression("2*3%2").unwrap(), Value::Int(0));

        // 10 // 3 * 2 = 3 * 2 = 6 (切り捨て除算と乗算は同じ優先度)
        assert_eq!(evaluate_expression("10//3*2").unwrap(), Value::Int(6));

        // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512.0 (右結合)
        assert_eq!(evaluate_expression("2**3**2").unwrap(), Value::Float(512.0));

        // 2 * 3 ** 2 = 2 * 9 = 18.0 (冪乗が乗算より優先)
        assert_eq!(evaluate_expression("2*3**2").unwrap(), Value::Float(18.0));
    }

    #[test]
    fn test_modulo_by_zero() {
        let result = evaluate_expression("5%0");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Division by zero"));

        let result = evaluate_expression("5.5%0");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Division by zero"));
    }

    #[test]
    fn test_floor_divide_by_zero() {
        let result = evaluate_expression("5//0");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Division by zero"));

        let result = evaluate_expression("5.5//0");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Division by zero"));
    }

    #[test]
    fn test_mixed_types_with_new_operators() {
        // 整数と浮動小数点数の混合テスト
        assert_eq!(evaluate_expression("10.5%3").unwrap(), Value::Float(1.5));
        assert_eq!(
            evaluate_expression("2**3.5").unwrap(),
            Value::Float(11.313708498984761)
        );
        assert_eq!(evaluate_expression("10.7//3").unwrap(), Value::Float(3.0));
        assert_eq!(evaluate_expression("10//3.2").unwrap(), Value::Float(3.0));
    }
}
