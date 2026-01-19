use chumsky::prelude::*;

use super::ast::{BinaryOp, Block, ConditionalBranch, Expr, Program, Statement};
use super::token::Token;
use super::value::{TypeAnnotation, Value};

/// Parse and return an expression from the input string (for REPL single-line mode)
pub fn parse(input: &str) -> Result<Expr, Vec<Rich<'_, char>>> {
    parser().parse(input).into_result()
}

/// Parse a program from tokens
pub fn parse_program(tokens: &[Token]) -> Result<Program, String> {
    let mut parser = ProgramParser::new(tokens);
    parser.parse_program()
}

/// Token-based program parser for if statements
struct ProgramParser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> ProgramParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("expected {:?}, got {:?}", expected, token)),
            None => Err(format!("expected {:?}, got end of input", expected)),
        }
    }

    fn skip_newlines(&mut self) {
        while let Some(Token::Newline) = self.peek() {
            self.advance();
        }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();

        self.skip_newlines();

        while let Some(token) = self.peek() {
            if *token == Token::Eof {
                break;
            }

            let stmt = self.parse_statement()?;
            statements.push(stmt);

            self.skip_newlines();
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::If) => self.parse_if_statement(),
            _ => {
                let expr = self.parse_expression()?;
                // Consume optional newline after expression
                if let Some(Token::Newline) = self.peek() {
                    self.advance();
                }
                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        // Parse if branch
        self.expect(&Token::If)?;
        let if_condition = self.parse_expression()?;
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        let if_body = self.parse_block()?;

        let if_branch = ConditionalBranch {
            condition: if_condition,
            body: if_body,
        };

        // Parse elif branches
        let mut elif_branches = Vec::new();
        while let Some(Token::Elif) = self.peek() {
            self.advance();
            let condition = self.parse_expression()?;
            self.expect(&Token::Colon)?;
            self.expect(&Token::Newline)?;
            let body = self.parse_block()?;
            elif_branches.push(ConditionalBranch { condition, body });
        }

        // Parse optional else
        let else_body = if let Some(Token::Else) = self.peek() {
            self.advance();
            self.expect(&Token::Colon)?;
            self.expect(&Token::Newline)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            if_branch,
            elif_branches,
            else_body,
        })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(&Token::Indent)?;

        let mut statements = Vec::new();

        loop {
            // Check for dedent or EOF
            match self.peek() {
                Some(Token::Dedent) | Some(Token::Eof) | None => break,
                Some(Token::Newline) => {
                    self.advance();
                    continue;
                }
                _ => {}
            }

            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }

        // Consume dedent if present
        if let Some(Token::Dedent) = self.peek() {
            self.advance();
        }

        Ok(Block { statements })
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_assignment()
    }

    fn parse_type_annotation(&mut self) -> Result<Option<TypeAnnotation>, String> {
        match self.peek() {
            Some(Token::IntType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Int))
            }
            Some(Token::FloatType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Float))
            }
            Some(Token::BoolType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Bool))
            }
            _ => Ok(None),
        }
    }

    fn parse_assignment(&mut self) -> Result<Expr, String> {
        let expr = self.parse_comparison()?;

        // Check for type annotation pattern: ident: type = value
        if let Expr::Variable(name) = &expr {
            if let Some(Token::Colon) = self.peek() {
                self.advance(); // consume ':'
                let type_annotation = self.parse_type_annotation()?;
                if type_annotation.is_some() {
                    self.expect(&Token::Equal)?;
                    let value = self.parse_assignment()?;
                    return Ok(Expr::Assign {
                        name: name.clone(),
                        type_annotation,
                        value: Box::new(value),
                    });
                } else {
                    return Err(
                        "expected type annotation (int, float, or bool) after ':'".to_string()
                    );
                }
            }
        }

        if let Some(Token::Equal) = self.peek() {
            // Check if lhs is a variable
            if let Expr::Variable(name) = expr {
                self.advance();
                let value = self.parse_assignment()?;
                return Ok(Expr::Assign {
                    name,
                    type_annotation: None,
                    value: Box::new(value),
                });
            }
            // If not a variable, just return the expression
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_additive()?;

        loop {
            let op = match self.peek() {
                Some(Token::EqualEqual) => BinaryOp::Equal,
                Some(Token::NotEqual) => BinaryOp::NotEqual,
                Some(Token::Less) => BinaryOp::LessThan,
                Some(Token::Greater) => BinaryOp::GreaterThan,
                Some(Token::LessEqual) => BinaryOp::LessThanOrEqual,
                Some(Token::GreaterEqual) => BinaryOp::GreaterThanOrEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek() {
                Some(Token::Plus) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Subtract,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_power()?;

        loop {
            let op = match self.peek() {
                Some(Token::Star) => BinaryOp::Multiply,
                Some(Token::Slash) => BinaryOp::Divide,
                Some(Token::Percent) => BinaryOp::Modulo,
                Some(Token::SlashSlash) => BinaryOp::FloorDivide,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let base = self.parse_primary()?;

        if let Some(Token::StarStar) = self.peek() {
            self.advance();
            // Right-associative
            let exponent = self.parse_power()?;
            return Ok(Expr::BinaryOp {
                op: BinaryOp::Power,
                left: Box::new(base),
                right: Box::new(exponent),
            });
        }

        Ok(base)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Number(value)) => {
                let value = value.clone();
                self.advance();
                Ok(Expr::Number(value))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Number(Value::Bool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Number(Value::Bool(false)))
            }
            Some(Token::Ident(name)) => {
                let name = name.clone();
                self.advance();
                Ok(Expr::Variable(name))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Some(token) => Err(format!("unexpected token: {:?}", token)),
            None => Err("unexpected end of input".to_string()),
        }
    }
}

/// Create the expression parser
fn parser<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
    recursive(|expr| {
        // Number literal (integer or float)
        let number = text::int(10)
            .then(just('.').then(text::digits(10)).or_not())
            .to_slice()
            .map(|s: &str| {
                if s.contains('.') {
                    Expr::Number(Value::Float(s.parse().unwrap()))
                } else {
                    Expr::Number(Value::Int(s.parse().unwrap()))
                }
            });

        // Boolean literals
        let bool_lit = choice((
            text::keyword("true").to(Expr::Number(Value::Bool(true))),
            text::keyword("false").to(Expr::Number(Value::Bool(false))),
        ));

        // Identifier (variable reference)
        // Pattern: [a-zA-Z_][a-zA-Z0-9_]*
        // Must not be 'true' or 'false' (handled by trying bool_lit first)
        let ident = text::ident().map(|s: &str| Expr::Variable(s.to_string()));

        // Atom: number, bool, identifier, or parenthesized expression
        let atom = choice((
            number,
            bool_lit,
            ident,
            expr.clone().delimited_by(just('('), just(')')),
        ))
        .padded();

        // Power operator (right-associative, highest precedence)
        // Collect all power operands and fold from right for right-associativity
        // Pattern: a ** b ** c => a ** (b ** c)
        let power = {
            let power_op = just("**").padded();
            atom.clone()
                .separated_by(power_op)
                .at_least(1)
                .collect::<Vec<_>>()
                .map(|exprs| {
                    // Fold from right to left for right-associativity
                    exprs
                        .into_iter()
                        .rev()
                        .reduce(|acc, item| Expr::BinaryOp {
                            op: BinaryOp::Power,
                            left: Box::new(item),
                            right: Box::new(acc),
                        })
                        .unwrap()
                })
        };

        // Multiplicative operators (*, /, %, //)
        // Note: // must be checked before /
        let multiplicative = power.clone().foldl(
            choice((
                just("//").padded().to(BinaryOp::FloorDivide),
                just('*').padded().to(BinaryOp::Multiply),
                just('/').padded().to(BinaryOp::Divide),
                just('%').padded().to(BinaryOp::Modulo),
            ))
            .then(power)
            .repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            },
        );

        // Additive operators (+, -)
        let additive = multiplicative.clone().foldl(
            choice((
                just('+').padded().to(BinaryOp::Add),
                just('-').padded().to(BinaryOp::Subtract),
            ))
            .then(multiplicative)
            .repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            },
        );

        // Comparison operators (==, !=, <=, >=, <, >)
        // Note: <= and >= must be checked before < and >
        // Note: == must be checked before = (assignment)
        let comparison = additive.clone().foldl(
            choice((
                just("==").padded().to(BinaryOp::Equal),
                just("!=").padded().to(BinaryOp::NotEqual),
                just("<=").padded().to(BinaryOp::LessThanOrEqual),
                just(">=").padded().to(BinaryOp::GreaterThanOrEqual),
                just('<').padded().to(BinaryOp::LessThan),
                just('>').padded().to(BinaryOp::GreaterThan),
            ))
            .then(additive)
            .repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            },
        );

        // Type annotation parser
        let type_annot = choice((
            text::keyword("int").to(TypeAnnotation::Int),
            text::keyword("float").to(TypeAnnotation::Float),
            text::keyword("bool").to(TypeAnnotation::Bool),
        ))
        .boxed();

        // Typed assignment: x: int = 5
        // Parse name: type pair first, then combine with value
        let name_and_type = text::ident()
            .padded()
            .then_ignore(just(':').padded())
            .then(type_annot.padded());

        fn make_typed_assign(((name, ta), value): ((&str, TypeAnnotation), Expr)) -> Expr {
            Expr::Assign {
                name: name.to_string(),
                type_annotation: Some(ta),
                value: Box::new(value),
            }
        }

        let typed_assign = name_and_type
            .then_ignore(just('=').padded())
            .then(comparison.clone())
            .map(make_typed_assign);

        // Assignment operator (right-associative, lowest precedence)
        // Pattern: x = y = 5 => x = (y = 5)
        // Only valid when left side is a variable
        let assign_op = just('=').padded();
        let untyped_assign = comparison
            .clone()
            .separated_by(assign_op)
            .at_least(1)
            .collect::<Vec<_>>()
            .map(|exprs| {
                // Fold from right to left for right-associativity
                exprs
                    .into_iter()
                    .rev()
                    .reduce(|acc, item| {
                        // For assignment, left side must be a variable
                        match item {
                            Expr::Variable(name) => Expr::Assign {
                                name,
                                type_annotation: None,
                                value: Box::new(acc),
                            },
                            // If left side is not a variable, we still create an Assign
                            // but it will fail at evaluation time with proper error
                            _ => Expr::Assign {
                                name: String::new(),
                                type_annotation: None,
                                value: Box::new(acc),
                            },
                        }
                    })
                    .unwrap()
            });

        // Try typed assignment first, then fall back to untyped
        typed_assign.or(untyped_assign)
    })
    .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer() {
        let result = parse("42").unwrap();
        assert!(matches!(result, Expr::Number(Value::Int(42))));
    }

    #[test]
    fn test_parse_float() {
        let result = parse("3.14").unwrap();
        assert!(matches!(result, Expr::Number(Value::Float(f)) if (f - 3.14).abs() < 0.001));
    }

    #[test]
    fn test_parse_bool() {
        assert!(matches!(
            parse("true").unwrap(),
            Expr::Number(Value::Bool(true))
        ));
        assert!(matches!(
            parse("false").unwrap(),
            Expr::Number(Value::Bool(false))
        ));
    }

    #[test]
    fn test_parse_addition() {
        let result = parse("1+2").unwrap();
        assert!(matches!(
            result,
            Expr::BinaryOp {
                op: BinaryOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_with_spaces() {
        let result = parse("1 + 2").unwrap();
        assert!(matches!(
            result,
            Expr::BinaryOp {
                op: BinaryOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_power() {
        let result = parse("2**3").unwrap();
        assert!(matches!(
            result,
            Expr::BinaryOp {
                op: BinaryOp::Power,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_floor_divide() {
        let result = parse("10//3").unwrap();
        assert!(matches!(
            result,
            Expr::BinaryOp {
                op: BinaryOp::FloorDivide,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_comparison() {
        assert!(matches!(
            parse("1==2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::Equal,
                ..
            }
        ));
        assert!(matches!(
            parse("1!=2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::NotEqual,
                ..
            }
        ));
        assert!(matches!(
            parse("1<2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::LessThan,
                ..
            }
        ));
        assert!(matches!(
            parse("1>2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::GreaterThan,
                ..
            }
        ));
        assert!(matches!(
            parse("1<=2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::LessThanOrEqual,
                ..
            }
        ));
        assert!(matches!(
            parse("1>=2").unwrap(),
            Expr::BinaryOp {
                op: BinaryOp::GreaterThanOrEqual,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_invalid() {
        assert!(parse("1+").is_err());
        assert!(parse("+1").is_err());
    }

    #[test]
    fn test_parse_variable() {
        let result = parse("x").unwrap();
        assert!(matches!(result, Expr::Variable(name) if name == "x"));

        let result = parse("my_var").unwrap();
        assert!(matches!(result, Expr::Variable(name) if name == "my_var"));

        let result = parse("_private").unwrap();
        assert!(matches!(result, Expr::Variable(name) if name == "_private"));
    }

    #[test]
    fn test_parse_assignment() {
        let result = parse("x = 5").unwrap();
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                ..
            } if name == "x"
        ));
    }

    #[test]
    fn test_parse_chain_assignment() {
        let result = parse("x = y = 5").unwrap();
        // Should be Assign { name: "x", value: Assign { name: "y", ... } }
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                value,
                ..
            } if name == "x" && matches!(*value, Expr::Assign { name: ref n, .. } if n == "y")
        ));
    }

    #[test]
    fn test_parse_assignment_with_expression() {
        let result = parse("x = 1 + 2").unwrap();
        // x = (1 + 2) - assignment has lowest precedence
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                value,
                ..
            } if name == "x" && matches!(*value, Expr::BinaryOp { op: BinaryOp::Add, .. })
        ));
    }

    #[test]
    fn test_parse_typed_assignment() {
        let result = parse("x: int = 5").unwrap();
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                type_annotation: Some(TypeAnnotation::Int),
                ..
            } if name == "x"
        ));

        let result = parse("y: float = 3.14").unwrap();
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                type_annotation: Some(TypeAnnotation::Float),
                ..
            } if name == "y"
        ));

        let result = parse("flag: bool = true").unwrap();
        assert!(matches!(
            result,
            Expr::Assign {
                name,
                type_annotation: Some(TypeAnnotation::Bool),
                ..
            } if name == "flag"
        ));
    }
}
