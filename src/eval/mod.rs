pub mod ast;
pub mod error;
pub mod parser;
pub mod value;

use ast::{BinaryOp, Expr};
use error::Result;

// Re-exports for public API
pub use error::EvalError;
pub use value::Value;

/// Evaluate an AST expression
pub fn evaluate(expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Number(n) => Ok(n.clone()),
        Expr::BinaryOp { op, left, right } => {
            let left_val = evaluate(left)?;
            let right_val = evaluate(right)?;

            match op {
                BinaryOp::Add => left_val.safe_add(right_val),
                BinaryOp::Subtract => left_val.safe_sub(right_val),
                BinaryOp::Multiply => left_val.safe_mul(right_val),
                BinaryOp::Divide => left_val.divide(right_val),
                BinaryOp::Modulo => left_val.modulo(right_val),
                BinaryOp::Power => left_val.power(right_val),
                BinaryOp::FloorDivide => left_val.floor_divide(right_val),
                BinaryOp::Equal => left_val.compare_eq(right_val),
                BinaryOp::NotEqual => left_val.compare_ne(right_val),
                BinaryOp::LessThan => left_val.compare_lt(right_val),
                BinaryOp::GreaterThan => left_val.compare_gt(right_val),
                BinaryOp::LessThanOrEqual => left_val.compare_le(right_val),
                BinaryOp::GreaterThanOrEqual => left_val.compare_ge(right_val),
            }
        }
    }
}

/// Parse and evaluate an expression string
pub fn evaluate_expression(input: &str) -> std::result::Result<Value, String> {
    let expr = parser::parse(input).map_err(|errs| format_parse_errors(errs, input))?;
    evaluate(&expr).map_err(|e| e.to_string())
}

/// Format parse errors into a human-readable string
fn format_parse_errors(errs: Vec<chumsky::error::Rich<'_, char>>, input: &str) -> String {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    use std::fmt::Write;

    let mut output = String::new();

    for err in errs {
        let mut report = Report::build(ReportKind::Error, (), err.span().start);

        report = report.with_message(err.to_string()).with_label(
            Label::new(err.span().into_range())
                .with_message(err.reason().to_string())
                .with_color(Color::Red),
        );

        let mut buf = Vec::new();
        report
            .finish()
            .write(Source::from(input), &mut buf)
            .unwrap();
        let _ = write!(output, "{}", String::from_utf8_lossy(&buf));
    }

    if output.is_empty() {
        "Parse error".to_string()
    } else {
        output
    }
}

/// Backward compatibility wrapper that returns f64
pub fn evaluate_expression_f64(input: &str) -> std::result::Result<f64, String> {
    evaluate_expression(input).map(|v| v.to_f64())
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_operator_precedence() {
        assert_eq!(evaluate_expression("2*3+4").unwrap(), Value::Int(10));
        assert_eq!(evaluate_expression("2 * 3 + 4").unwrap(), Value::Int(10));
        assert_eq!(evaluate_expression("10/2-1").unwrap(), Value::Float(4.0));
        assert_eq!(
            evaluate_expression("10 / 2 - 1").unwrap(),
            Value::Float(4.0)
        );
        assert_eq!(evaluate_expression("1+2*3").unwrap(), Value::Int(7));
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
        assert_eq!(evaluate_expression("1+1.5").unwrap(), Value::Float(2.5));
        assert_eq!(evaluate_expression("1.5+1").unwrap(), Value::Float(2.5));
        assert_eq!(evaluate_expression("5-2.5").unwrap(), Value::Float(2.5));
        assert_eq!(evaluate_expression("5.5-2").unwrap(), Value::Float(3.5));
        assert_eq!(evaluate_expression("2*1.5").unwrap(), Value::Float(3.0));
        assert_eq!(evaluate_expression("2.5*2").unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_integer_operations() {
        assert_eq!(evaluate_expression("1+2").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("5-2").unwrap(), Value::Int(3));
        assert_eq!(evaluate_expression("2*3").unwrap(), Value::Int(6));
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
        assert_eq!(evaluate_expression("2**3+4").unwrap(), Value::Float(12.0));
        assert_eq!(evaluate_expression("2*3%2").unwrap(), Value::Int(0));
        assert_eq!(evaluate_expression("10//3*2").unwrap(), Value::Int(6));
        assert_eq!(evaluate_expression("2**3**2").unwrap(), Value::Float(512.0));
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
        assert_eq!(evaluate_expression("10.5%3").unwrap(), Value::Float(1.5));
        assert_eq!(
            evaluate_expression("2**3.5").unwrap(),
            Value::Float(11.313708498984761)
        );
        assert_eq!(evaluate_expression("10.7//3").unwrap(), Value::Float(3.0));
        assert_eq!(evaluate_expression("10//3.2").unwrap(), Value::Float(3.0));
    }

    #[test]
    fn test_bool_literals() {
        assert_eq!(evaluate_expression("true").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_comparison_equal() {
        assert_eq!(evaluate_expression("1==1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1 == 1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1==2").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1.5==1.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1==1.0").unwrap(), Value::Bool(true));
        assert_eq!(
            evaluate_expression("true==true").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_expression("true==false").unwrap(),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_comparison_not_equal() {
        assert_eq!(evaluate_expression("1!=1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1 != 1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1!=2").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1.5!=1.5").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1!=1.0").unwrap(), Value::Bool(false));
        assert_eq!(
            evaluate_expression("true!=true").unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            evaluate_expression("true!=false").unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_comparison_less_than() {
        assert_eq!(evaluate_expression("1<2").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1 < 2").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2<1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1<1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1.5<2.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1<2.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2.5<1").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_comparison_greater_than() {
        assert_eq!(evaluate_expression("2>1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2 > 1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1>2").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1>1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("2.5>1.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2.5>1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1>2.5").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_comparison_less_than_or_equal() {
        assert_eq!(evaluate_expression("1<=2").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1 <= 2").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1<=1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2<=1").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("1.5<=2.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1.5<=1.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2.5<=1.5").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_comparison_greater_than_or_equal() {
        assert_eq!(evaluate_expression("2>=1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2 >= 1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1>=1").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1>=2").unwrap(), Value::Bool(false));
        assert_eq!(evaluate_expression("2.5>=1.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1.5>=1.5").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1.5>=2.5").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_comparison_operator_precedence() {
        assert_eq!(evaluate_expression("1+2<3*4").unwrap(), Value::Bool(true));
        assert_eq!(
            evaluate_expression("1 + 2 < 3 * 4").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(evaluate_expression("10/2>3").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("2*3==6").unwrap(), Value::Bool(true));
        assert_eq!(evaluate_expression("1+2<=3").unwrap(), Value::Bool(true));
    }
}
