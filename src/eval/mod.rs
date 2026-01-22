pub mod ast;
pub mod context;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;

use std::sync::Arc;

use ast::{BinaryOp, Block, Expr, Program, Statement, UnaryOp};
use error::Result;
use value::{FuncDef, FuncOverloads, FuncParam};

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

/// Assign a value to a variable with type checking
fn assign_with_type_check(
    ctx: &mut Context,
    name: &str,
    value: Value,
    type_annotation: Option<TypeAnnotation>,
) -> Result<Value> {
    // Check type constraint from declaration or existing variable
    let effective_constraint = type_annotation.clone().or_else(|| {
        ctx.get_info(name)
            .and_then(|info| info.type_constraint.clone())
    });

    // Validate type if constraint exists
    if let Some(ref constraint) = effective_constraint {
        if !constraint.matches(&value) {
            return Err(EvalError::TypeMismatch {
                expected: constraint.type_name(),
                actual: value.type_name(),
                variable: name.to_string(),
            });
        }
    }

    // Set the variable with type constraint
    ctx.set_typed(name.to_string(), value.clone(), type_annotation);
    Ok(value)
}

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
            assign_with_type_check(ctx, name, val, type_annotation.clone())
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
                        assign_with_type_check(ctx, name, v.clone(), type_annotation.clone())?;
                    }
                    Ok(Value::Tuple(values))
                }
                _ => Err(EvalError::CannotUnpackNonTuple(val.type_name())),
            }
        }
        Expr::BinaryOp { op, left, right } => {
            // Short-circuit evaluation for logical operators
            match op {
                BinaryOp::And => {
                    let left_val = evaluate(left, ctx)?;
                    if !is_truthy(&left_val)? {
                        return Ok(left_val);
                    }
                    evaluate(right, ctx)
                }
                BinaryOp::Or => {
                    let left_val = evaluate(left, ctx)?;
                    if is_truthy(&left_val)? {
                        return Ok(left_val);
                    }
                    evaluate(right, ctx)
                }
                _ => {
                    // Non-short-circuit operators: evaluate both operands first
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
                        BinaryOp::And | BinaryOp::Or => unreachable!(),
                    }
                }
            }
        }
        Expr::UnaryOp { op, operand } => match op {
            UnaryOp::Not => {
                let val = evaluate(operand, ctx)?;
                let truthy = is_truthy(&val)?;
                Ok(Value::Bool(!truthy))
            }
            UnaryOp::Neg => {
                let val = evaluate(operand, ctx)?;
                match val {
                    Value::Int(i) => {
                        // Use checked negation to handle overflow (e.g., -i64::MIN)
                        match i.checked_neg() {
                            Some(negated) => Ok(Value::Int(negated)),
                            None => Ok(Value::Float(-(i as f64))),
                        }
                    }
                    Value::Float(f) => Ok(Value::Float(-f)),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot negate value of type {}",
                        val.type_name()
                    ))),
                }
            }
        },
        Expr::FuncCall { callee, args } => {
            let callee_value = evaluate(callee, ctx)?;

            // Evaluate arguments first
            let arg_values: Vec<Value> = args
                .iter()
                .map(|a| evaluate(a, ctx))
                .collect::<Result<Vec<_>>>()?;

            match callee_value {
                Value::Func(overloads) => {
                    if overloads.overloads.len() == 1 {
                        // Single function: use direct call for better error messages
                        call_function(&overloads.overloads[0], &arg_values, ctx)
                    } else {
                        // Multiple overloads: use resolution
                        let resolved = overloads.resolve(&arg_values)?;
                        call_function(&resolved, &arg_values, ctx)
                    }
                }
                _ => Err(EvalError::NotCallable(callee_value.type_name())),
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
        Value::Func(_) => Ok(true), // Functions are always truthy
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
                match execute_block(body, ctx) {
                    Ok(val) => last_value = val,
                    Err(EvalError::ReturnValue(val)) => return Err(EvalError::ReturnValue(val)),
                    Err(e) => return Err(e),
                }
            }
            Ok(last_value)
        }
        Statement::FuncDef {
            name,
            params,
            return_type,
            body,
        } => {
            let func_def = Arc::new(FuncDef {
                name: Some(name.clone()),
                params: params
                    .iter()
                    .map(|(n, t)| FuncParam {
                        name: n.clone(),
                        type_annotation: t.clone(),
                    })
                    .collect(),
                return_type: return_type.clone(),
                body: body.clone(),
            });

            // Check if there's already a function with this name
            match ctx.get(name) {
                Some(Value::Func(overloads)) => {
                    // Add to existing overload collection
                    let mut new_overloads = overloads.as_ref().clone();
                    new_overloads.add_overload(func_def)?;
                    let value = Value::Func(Arc::new(new_overloads));
                    ctx.set(name.clone(), value.clone());
                    Ok(Some(value))
                }
                _ => {
                    // First definition: create single-function overload collection
                    let overloads = FuncOverloads::single(func_def);
                    let value = Value::Func(Arc::new(overloads));
                    ctx.set(name.clone(), value.clone());
                    Ok(Some(value))
                }
            }
        }
        Statement::Return(expr) => {
            let value = match expr {
                Some(e) => Some(evaluate(e, ctx)?),
                None => None,
            };
            Err(EvalError::ReturnValue(value))
        }
    }
}

/// Validate return value against expected type
fn validate_return_value(
    return_value: Option<Value>,
    expected_type: &Option<TypeAnnotation>,
    func_name: &str,
) -> Result<Value> {
    let value = return_value.unwrap_or(Value::Tuple(vec![]));
    if let Some(ref constraint) = expected_type {
        if !constraint.matches(&value) {
            return Err(EvalError::ReturnTypeMismatch {
                expected: constraint.type_name(),
                actual: value.type_name(),
                func_name: func_name.to_string(),
            });
        }
    }
    Ok(value)
}

/// Call a function with pre-evaluated arguments
fn call_function(func_def: &FuncDef, arg_values: &[Value], ctx: &mut Context) -> Result<Value> {
    let func_name = func_def.name.as_deref().unwrap_or("<anonymous>");

    // Check argument count
    if arg_values.len() != func_def.params.len() {
        return Err(EvalError::ArgumentCountMismatch {
            expected: func_def.params.len(),
            actual: arg_values.len(),
            func_name: func_name.to_string(),
        });
    }

    // Check argument types
    for (param, value) in func_def.params.iter().zip(arg_values.iter()) {
        if let Some(ref constraint) = param.type_annotation {
            if !constraint.matches(value) {
                // Use FunctionSignatureMismatch for function type mismatches
                if matches!(
                    constraint,
                    TypeAnnotation::Func | TypeAnnotation::FuncSig { .. }
                ) {
                    return Err(EvalError::FunctionSignatureMismatch {
                        expected: constraint.type_name(),
                        actual: value.type_name(),
                        param_name: param.name.clone(),
                        func_name: func_name.to_string(),
                    });
                }
                return Err(EvalError::ArgumentTypeMismatch {
                    expected: constraint.type_name(),
                    actual: value.type_name(),
                    param_name: param.name.clone(),
                    func_name: func_name.to_string(),
                });
            }
        }
    }

    // Create new scope and bind parameters
    let mut guard = ScopeGuard::new(ctx);
    for (param, value) in func_def.params.iter().zip(arg_values.iter().cloned()) {
        guard
            .context()
            .set_typed(param.name.clone(), value, param.type_annotation.clone());
    }

    // Execute function body
    let result = execute_function_body(&func_def.body, guard.context());

    // Handle return value
    match result {
        Ok(val) => validate_return_value(val, &func_def.return_type, func_name),
        Err(EvalError::ReturnValue(val)) => {
            validate_return_value(val, &func_def.return_type, func_name)
        }
        Err(e) => Err(e),
    }
}

/// Execute function body (separate from execute_block to handle returns properly)
fn execute_function_body(block: &Block, ctx: &mut Context) -> Result<Option<Value>> {
    let mut last_value = None;
    for stmt in &block.statements {
        match execute_statement(stmt, ctx) {
            Ok(val) => last_value = val,
            Err(EvalError::ReturnValue(val)) => return Err(EvalError::ReturnValue(val)),
            Err(e) => return Err(e),
        }
    }
    Ok(last_value)
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
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let program = parse_program(&tokens)?;
    execute_program(&program, ctx)
        .map_err(|e| e.to_string())?
        .ok_or_else(|| "No expression to evaluate".to_string())
}

/// Parse and evaluate an expression string (backward compatible, uses fresh context)
pub fn evaluate_expression(input: &str) -> std::result::Result<Value, String> {
    let mut ctx = Context::new();
    evaluate_expression_with_context(input, &mut ctx)
}

/// Backward compatibility wrapper that returns f64
pub fn evaluate_expression_f64(input: &str) -> std::result::Result<f64, String> {
    let value = evaluate_expression(input)?;
    value.to_f64().map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests;
