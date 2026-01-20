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
            let effective_constraint = type_annotation.clone().or_else(|| {
                ctx.get_info(name)
                    .and_then(|info| info.type_constraint.clone())
            });

            // Validate type if constraint exists
            if let Some(ref constraint) = effective_constraint {
                if !constraint.matches(&val) {
                    return Err(EvalError::TypeMismatch {
                        expected: constraint.type_name(),
                        actual: val.type_name(),
                        variable: name.clone(),
                    });
                }
            }

            // Set the variable with type constraint
            ctx.set_typed(name.clone(), val.clone(), type_annotation.clone());
            Ok(val)
        }
        Expr::Tuple(exprs) => {
            let values: Result<Vec<Value>> = exprs.iter().map(|e| evaluate(e, ctx)).collect();
            Ok(Value::Tuple(values?))
        }
        Expr::TupleUnpack { targets, value } => {
            let val = evaluate(value, ctx)?;
            match val {
                Value::Tuple(values) => {
                    if targets.len() != values.len() {
                        return Err(EvalError::TupleUnpackMismatch {
                            expected: targets.len(),
                            actual: values.len(),
                        });
                    }
                    for ((name, type_annotation), v) in targets.iter().zip(values.iter()) {
                        // Check type constraint from declaration or existing variable
                        let effective_constraint = type_annotation.clone().or_else(|| {
                            ctx.get_info(name)
                                .and_then(|info| info.type_constraint.clone())
                        });

                        // Validate type if constraint exists
                        if let Some(ref constraint) = effective_constraint {
                            if !constraint.matches(v) {
                                return Err(EvalError::TypeMismatch {
                                    expected: constraint.type_name(),
                                    actual: v.type_name(),
                                    variable: name.clone(),
                                });
                            }
                        }

                        ctx.set_typed(name.clone(), v.clone(), type_annotation.clone());
                    }
                    Ok(Value::Tuple(values))
                }
                _ => Err(EvalError::CannotUnpackNonTuple(val.type_name())),
            }
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
        Value::Tuple(values) => Ok(!values.is_empty()),
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
        Statement::While { condition, body } => {
            let mut last_value = None;
            loop {
                let cond_value = evaluate(condition, ctx)?;
                if !is_truthy(&cond_value)? {
                    break;
                }
                last_value = execute_block(body, ctx)?;
            }
            Ok(last_value)
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
    let value = evaluate_expression(input)?;
    value.to_f64().map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests;
