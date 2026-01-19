use chumsky::prelude::*;

use super::ast::{BinaryOp, Expr};
use super::value::Value;

/// Parse and return an expression from the input string
pub fn parse(input: &str) -> Result<Expr, Vec<Rich<'_, char>>> {
    parser().parse(input).into_result()
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

        // Atom: number, bool, or parenthesized expression
        let atom = choice((number, bool_lit, expr.delimited_by(just('('), just(')')))).padded();

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
        additive.clone().foldl(
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
        )
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
}
