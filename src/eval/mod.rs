pub mod ast;
pub mod context;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;

use ast::{BinaryOp, Block, Expr, Program, Statement};
use error::Result;

// Re-exports for public API
pub use context::Context;
pub use context::ScopeGuard;
pub use context::VariableInfo;
pub use error::EvalError;
pub use lexer::Lexer;
pub use parser::parse_program;
pub use token::Token;
pub use value::TypeAnnotation;
pub use value::Value;

/// Evaluate an AST expression with a context
pub fn evaluate(expr: &Expr, ctx: &mut Context) -> Result<Value> {
    match expr {
        Expr::Number(n) => Ok(n.clone()),
        Expr::Variable(name) => ctx
            .get(name)
            .cloned()
            .ok_or_else(|| EvalError::UndefinedVariable(name.clone())),
        Expr::Assign {
            name,
            type_annotation,
            value,
        } => {
            let val = evaluate(value, ctx)?;

            // Check type constraint from declaration or existing variable
            let effective_constraint = type_annotation
                .or_else(|| ctx.get_info(name).and_then(|info| info.type_constraint));

            // Validate type if constraint exists
            if let Some(constraint) = effective_constraint {
                if !constraint.matches(&val) {
                    return Err(EvalError::TypeMismatch {
                        expected: constraint.type_name().to_string(),
                        actual: val.type_name().to_string(),
                        variable: name.clone(),
                    });
                }
            }

            // Set the variable with type constraint
            ctx.set_typed(name.clone(), val.clone(), *type_annotation);
            Ok(val)
        }
        Expr::BinaryOp { op, left, right } => {
            let left_val = evaluate(left, ctx)?;
            let right_val = evaluate(right, ctx)?;

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

/// Check if a value is truthy
fn is_truthy(value: &Value) -> Result<bool> {
    match value {
        Value::Bool(b) => Ok(*b),
        Value::Int(i) => Ok(*i != 0),
        Value::Float(f) => Ok(*f != 0.0),
        Value::Str(s) => Ok(!s.is_empty()),
    }
}

/// Execute a program with a context
pub fn execute_program(program: &Program, ctx: &mut Context) -> Result<Option<Value>> {
    let mut last_value = None;
    for stmt in &program.statements {
        last_value = execute_statement(stmt, ctx)?;
    }
    Ok(last_value)
}

/// Execute a statement with a context
pub fn execute_statement(stmt: &Statement, ctx: &mut Context) -> Result<Option<Value>> {
    match stmt {
        Statement::Expression(expr) => Ok(Some(evaluate(expr, ctx)?)),
        Statement::If {
            if_branch,
            elif_branches,
            else_body,
        } => {
            // Check if condition
            let cond_value = evaluate(&if_branch.condition, ctx)?;
            if is_truthy(&cond_value)? {
                return execute_block(&if_branch.body, ctx);
            }

            // Check elif conditions
            for elif_branch in elif_branches {
                let cond_value = evaluate(&elif_branch.condition, ctx)?;
                if is_truthy(&cond_value)? {
                    return execute_block(&elif_branch.body, ctx);
                }
            }

            // Execute else body if present
            if let Some(else_block) = else_body {
                return execute_block(else_block, ctx);
            }

            Ok(None)
        }
    }
}

/// Execute a block in a new scope
fn execute_block(block: &Block, ctx: &mut Context) -> Result<Option<Value>> {
    let mut guard = ScopeGuard::new(ctx);
    let mut last_value = None;
    for stmt in &block.statements {
        last_value = execute_statement(stmt, guard.context())?;
    }
    Ok(last_value)
} // ScopeGuard is dropped here, automatically calling pop_scope()

/// Parse and evaluate an expression string with a context
pub fn evaluate_expression_with_context(
    input: &str,
    ctx: &mut Context,
) -> std::result::Result<Value, String> {
    let expr = parser::parse(input).map_err(|errs| format_parse_errors(errs, input))?;
    evaluate(&expr, ctx).map_err(|e| e.to_string())
}

/// Parse and evaluate an expression string (backward compatible, uses fresh context)
pub fn evaluate_expression(input: &str) -> std::result::Result<Value, String> {
    let mut ctx = Context::new();
    evaluate_expression_with_context(input, &mut ctx)
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

    #[test]
    fn test_variable_assignment() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("x = 5", &mut ctx).unwrap(),
            Value::Int(5)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
    }

    #[test]
    fn test_variable_reference() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        assert_eq!(
            evaluate_expression_with_context("x", &mut ctx).unwrap(),
            Value::Int(5)
        );
    }

    #[test]
    fn test_variable_in_expression() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        assert_eq!(
            evaluate_expression_with_context("x + 1", &mut ctx).unwrap(),
            Value::Int(6)
        );
        assert_eq!(
            evaluate_expression_with_context("x * 2", &mut ctx).unwrap(),
            Value::Int(10)
        );
    }

    #[test]
    fn test_assignment_precedence() {
        let mut ctx = Context::new();
        // x = 1 + 2 should be x = (1 + 2) = 3
        assert_eq!(
            evaluate_expression_with_context("x = 1 + 2", &mut ctx).unwrap(),
            Value::Int(3)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(3)));
    }

    #[test]
    fn test_chain_assignment() {
        let mut ctx = Context::new();
        // x = y = 5 should set both x and y to 5
        assert_eq!(
            evaluate_expression_with_context("x = y = 5", &mut ctx).unwrap(),
            Value::Int(5)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
        assert_eq!(ctx.get("y"), Some(&Value::Int(5)));
    }

    #[test]
    fn test_undefined_variable() {
        let mut ctx = Context::new();
        let result = evaluate_expression_with_context("x", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Undefined variable"));
    }

    #[test]
    fn test_variable_reassignment() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        evaluate_expression_with_context("x = 10", &mut ctx).unwrap();
        assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
    }

    #[test]
    fn test_variable_with_underscore() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("my_var = 42", &mut ctx).unwrap(),
            Value::Int(42)
        );
        assert_eq!(
            evaluate_expression_with_context("_private = 1", &mut ctx).unwrap(),
            Value::Int(1)
        );
    }

    #[test]
    fn test_variable_assign_expression() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        evaluate_expression_with_context("y = 10", &mut ctx).unwrap();
        assert_eq!(
            evaluate_expression_with_context("z = x + y", &mut ctx).unwrap(),
            Value::Int(15)
        );
        assert_eq!(ctx.get("z"), Some(&Value::Int(15)));
    }

    #[test]
    fn test_variable_with_comparison() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        assert_eq!(
            evaluate_expression_with_context("x == 5", &mut ctx).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_expression_with_context("x > 3", &mut ctx).unwrap(),
            Value::Bool(true)
        );
    }

    // Tests for if statements
    fn execute_test_program(
        input: &str,
        ctx: &mut Context,
    ) -> std::result::Result<Option<Value>, String> {
        let mut lexer = super::lexer::Lexer::new(input);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        let program = super::parser::parse_program(&tokens)?;
        super::execute_program(&program, ctx).map_err(|e| e.to_string())
    }

    #[test]
    fn test_if_true_branch() {
        let mut ctx = Context::new();
        let input = "x = 10\nif x > 5:\n    y = 20\n    y\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(20)));
    }

    #[test]
    fn test_if_false_branch() {
        let mut ctx = Context::new();
        let input = "x = 3\nif x > 5:\n    y = 20\n    y\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_if_else() {
        let mut ctx = Context::new();
        let input = "x = 3\nif x > 5:\n    10\nelse:\n    20\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(20)));
    }

    #[test]
    fn test_if_elif_else() {
        let mut ctx = Context::new();
        let input = "x = 5\nif x > 10:\n    1\nelif x > 3:\n    2\nelse:\n    3\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(2)));
    }

    #[test]
    fn test_scope_isolation_in_if() {
        let mut ctx = Context::new();
        let input = "x = 10\nif true:\n    y = 20\n";
        execute_test_program(input, &mut ctx).unwrap();

        // x is accessible
        assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
        // y is not accessible (was in if block scope)
        assert!(ctx.get("y").is_none());
    }

    #[test]
    fn test_outer_variable_update_in_if() {
        let mut ctx = Context::new();
        let input = "x = 10\nif true:\n    x = 100\n";
        execute_test_program(input, &mut ctx).unwrap();

        // x was updated in the outer scope
        assert_eq!(ctx.get("x"), Some(&Value::Int(100)));
    }

    #[test]
    fn test_nested_if() {
        let mut ctx = Context::new();
        // y is defined outside the inner if, so it can be accessed after
        let input = "x = 10\ny = 0\nif x > 5:\n    if x > 8:\n        y = 1\n    else:\n        y = 2\n    y\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(1)));
        // y was updated in outer scope
        assert_eq!(ctx.get("y"), Some(&Value::Int(1)));
    }

    #[test]
    fn test_if_with_complex_condition() {
        let mut ctx = Context::new();
        let input = "x = 10\ny = 5\nif x > y:\n    z = x + y\n    z\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(15)));
    }

    #[test]
    fn test_truthy_integer() {
        let mut ctx = Context::new();
        let input = "if 1:\n    10\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(10)));
    }

    #[test]
    fn test_falsy_integer() {
        let mut ctx = Context::new();
        let input = "if 0:\n    10\nelse:\n    20\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(20)));
    }

    // Type annotation tests
    #[test]
    fn test_typed_int_assignment() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap(),
            Value::Int(5)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
    }

    #[test]
    fn test_typed_float_assignment() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("y: float = 3.14", &mut ctx).unwrap(),
            Value::Float(3.14)
        );
        assert_eq!(ctx.get("y"), Some(&Value::Float(3.14)));
    }

    #[test]
    fn test_typed_bool_assignment() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("flag: bool = true", &mut ctx).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(ctx.get("flag"), Some(&Value::Bool(true)));
    }

    #[test]
    fn test_typed_mismatch_int_expects_float() {
        let mut ctx = Context::new();
        let result = evaluate_expression_with_context("x: int = 3.14", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_typed_mismatch_float_expects_int() {
        let mut ctx = Context::new();
        let result = evaluate_expression_with_context("x: float = 5", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_typed_mismatch_bool_expects_int() {
        let mut ctx = Context::new();
        let result = evaluate_expression_with_context("x: bool = 5", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_typed_variable_reassignment_same_type() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap();
        // Reassigning with same type should work
        assert_eq!(
            evaluate_expression_with_context("x = 10", &mut ctx).unwrap(),
            Value::Int(10)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
    }

    #[test]
    fn test_typed_variable_reassignment_different_type() {
        let mut ctx = Context::new();
        evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap();
        // Reassigning with different type should fail
        let result = evaluate_expression_with_context("x = 3.14", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_untyped_variable_allows_type_change() {
        let mut ctx = Context::new();
        // Untyped variable assignment
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
        // Can reassign to different type
        assert_eq!(
            evaluate_expression_with_context("x = 3.14", &mut ctx).unwrap(),
            Value::Float(3.14)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Float(3.14)));
    }

    #[test]
    fn test_typed_assignment_with_expression() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("x: int = 2 + 3", &mut ctx).unwrap(),
            Value::Int(5)
        );
        assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
    }

    #[test]
    fn test_typed_variable_in_program() {
        let mut ctx = Context::new();
        let input = "x: int = 10\ny: float = 3.14\nx + 1\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(11)));
    }

    #[test]
    fn test_typed_variable_reassignment_in_program() {
        let mut ctx = Context::new();
        let input = "x: int = 10\nx = 20\nx\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(20)));
    }

    #[test]
    fn test_typed_variable_type_error_in_program() {
        let mut ctx = Context::new();
        let input = "x: int = 10\nx = 3.14\n";
        let result = execute_test_program(input, &mut ctx);
        assert!(result.is_err());
    }

    // String tests
    #[test]
    fn test_string_literal() {
        assert_eq!(
            evaluate_expression("\"hello\"").unwrap(),
            Value::Str("hello".to_string())
        );
    }

    #[test]
    fn test_string_concatenation() {
        assert_eq!(
            evaluate_expression("\"hello\" + \" world\"").unwrap(),
            Value::Str("hello world".to_string())
        );
    }

    #[test]
    fn test_string_variable() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("x = \"test\"", &mut ctx).unwrap(),
            Value::Str("test".to_string())
        );
        assert_eq!(
            evaluate_expression_with_context("x", &mut ctx).unwrap(),
            Value::Str("test".to_string())
        );
    }

    #[test]
    fn test_typed_string_variable() {
        let mut ctx = Context::new();
        assert_eq!(
            evaluate_expression_with_context("x: str = \"hello\"", &mut ctx).unwrap(),
            Value::Str("hello".to_string())
        );
        assert_eq!(ctx.get("x"), Some(&Value::Str("hello".to_string())));
    }

    #[test]
    fn test_typed_string_mismatch() {
        let mut ctx = Context::new();
        let result = evaluate_expression_with_context("x: str = 42", &mut ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_string_equality() {
        assert_eq!(
            evaluate_expression("\"hello\" == \"hello\"").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_expression("\"hello\" == \"world\"").unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            evaluate_expression("\"hello\" != \"world\"").unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_string_with_int_error() {
        let result = evaluate_expression("\"hello\" + 1");
        assert!(result.is_err());
    }

    #[test]
    fn test_string_truthy_in_program() {
        let mut ctx = Context::new();
        let input = "if \"hello\":\n    10\nelse:\n    20\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(10)));
    }

    #[test]
    fn test_empty_string_falsy_in_program() {
        let mut ctx = Context::new();
        let input = "if \"\":\n    10\nelse:\n    20\n";
        let result = execute_test_program(input, &mut ctx).unwrap();
        assert_eq!(result, Some(Value::Int(20)));
    }
}
