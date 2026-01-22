//! Built-in functions for the ry language.

use std::sync::Arc;

use super::context::Context;
use super::error::EvalError;
use super::value::{FuncDef, FuncOverloads, FuncParam, TypeAnnotation, Value};

/// Create the int() builtin function overloads
fn create_int_builtin() -> FuncOverloads {
    let mut overloads = FuncOverloads::new("int".to_string());

    // int(int) -> int: identity
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "int".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Int),
            }],
            Some(TypeAnnotation::Int),
            |args| Ok(args[0].clone()),
        )))
        .unwrap();

    // int(float) -> int: truncate
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "int".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Float),
            }],
            Some(TypeAnnotation::Int),
            |args| {
                if let Value::Float(f) = &args[0] {
                    Ok(Value::Int(*f as i64))
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    // int(bool) -> int: true -> 1, false -> 0
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "int".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Bool),
            }],
            Some(TypeAnnotation::Int),
            |args| {
                if let Value::Bool(b) = &args[0] {
                    Ok(Value::Int(if *b { 1 } else { 0 }))
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    // int(str) -> int: parse
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "int".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Str),
            }],
            Some(TypeAnnotation::Int),
            |args| {
                if let Value::Str(s) = &args[0] {
                    s.trim().parse::<i64>().map(Value::Int).map_err(|_| {
                        EvalError::ConversionError {
                            from_type: "str".to_string(),
                            to_type: "int".to_string(),
                            value: s.clone(),
                        }
                    })
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    overloads
}

/// Create the float() builtin function overloads
fn create_float_builtin() -> FuncOverloads {
    let mut overloads = FuncOverloads::new("float".to_string());

    // float(int) -> float: convert
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "float".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Int),
            }],
            Some(TypeAnnotation::Float),
            |args| {
                if let Value::Int(i) = &args[0] {
                    Ok(Value::Float(*i as f64))
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    // float(float) -> float: identity
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "float".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Float),
            }],
            Some(TypeAnnotation::Float),
            |args| Ok(args[0].clone()),
        )))
        .unwrap();

    // float(bool) -> float: true -> 1.0, false -> 0.0
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "float".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Bool),
            }],
            Some(TypeAnnotation::Float),
            |args| {
                if let Value::Bool(b) = &args[0] {
                    Ok(Value::Float(if *b { 1.0 } else { 0.0 }))
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    // float(str) -> float: parse
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "float".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Str),
            }],
            Some(TypeAnnotation::Float),
            |args| {
                if let Value::Str(s) = &args[0] {
                    s.trim().parse::<f64>().map(Value::Float).map_err(|_| {
                        EvalError::ConversionError {
                            from_type: "str".to_string(),
                            to_type: "float".to_string(),
                            value: s.clone(),
                        }
                    })
                } else {
                    unreachable!("Type checked by overload resolution")
                }
            },
        )))
        .unwrap();

    overloads
}

/// Create the str() builtin function overloads
fn create_str_builtin() -> FuncOverloads {
    let mut overloads = FuncOverloads::new("str".to_string());

    // str(any) -> str: convert to string representation
    overloads
        .add_overload(Arc::new(FuncDef::native(
            "str".to_string(),
            vec![FuncParam {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Any),
            }],
            Some(TypeAnnotation::Str),
            |args| {
                let s = match &args[0] {
                    Value::Int(i) => i.to_string(),
                    Value::Float(f) => {
                        if *f == (*f as i64 as f64) && f.abs() < 1e15 {
                            format!("{}.0", *f as i64)
                        } else {
                            f.to_string()
                        }
                    }
                    Value::Bool(b) => b.to_string(),
                    Value::Str(s) => s.clone(),
                    Value::Tuple(values) => {
                        let parts: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                        format!("({})", parts.join(", "))
                    }
                    Value::Func(overloads) => {
                        format!("<func {}>", overloads.name)
                    }
                };
                Ok(Value::Str(s))
            },
        )))
        .unwrap();

    overloads
}

/// Register all built-in functions in the given context
pub fn register_builtins(ctx: &mut Context) {
    // Register int()
    let int_overloads = create_int_builtin();
    ctx.set("int".to_string(), Value::Func(Arc::new(int_overloads)));

    // Register float()
    let float_overloads = create_float_builtin();
    ctx.set("float".to_string(), Value::Func(Arc::new(float_overloads)));

    // Register str()
    let str_overloads = create_str_builtin();
    ctx.set("str".to_string(), Value::Func(Arc::new(str_overloads)));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::error::Result;

    fn eval_with_builtins(input: &str) -> Result<Value> {
        use crate::eval::{execute_program, parse_program, Lexer};

        let mut ctx = Context::new();
        register_builtins(&mut ctx);

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize()?;
        let program = parse_program(&tokens).map_err(|e| EvalError::ParseError(e))?;
        execute_program(&program, &mut ctx)?
            .ok_or_else(|| EvalError::TypeError("No value returned".to_string()))
    }

    // int() tests
    #[test]
    fn test_int_from_int() {
        assert_eq!(eval_with_builtins("int(42)").unwrap(), Value::Int(42));
        assert_eq!(eval_with_builtins("int(-10)").unwrap(), Value::Int(-10));
        assert_eq!(eval_with_builtins("int(0)").unwrap(), Value::Int(0));
    }

    #[test]
    fn test_int_from_float() {
        assert_eq!(eval_with_builtins("int(3.7)").unwrap(), Value::Int(3));
        assert_eq!(eval_with_builtins("int(3.2)").unwrap(), Value::Int(3));
        assert_eq!(eval_with_builtins("int(-3.7)").unwrap(), Value::Int(-3));
        assert_eq!(eval_with_builtins("int(0.0)").unwrap(), Value::Int(0));
    }

    #[test]
    fn test_int_from_bool() {
        assert_eq!(eval_with_builtins("int(true)").unwrap(), Value::Int(1));
        assert_eq!(eval_with_builtins("int(false)").unwrap(), Value::Int(0));
    }

    #[test]
    fn test_int_from_str() {
        assert_eq!(eval_with_builtins(r#"int("42")"#).unwrap(), Value::Int(42));
        assert_eq!(
            eval_with_builtins(r#"int("-100")"#).unwrap(),
            Value::Int(-100)
        );
        assert_eq!(eval_with_builtins(r#"int("0")"#).unwrap(), Value::Int(0));
        assert_eq!(
            eval_with_builtins(r#"int("  123  ")"#).unwrap(),
            Value::Int(123)
        );
    }

    #[test]
    fn test_int_from_str_invalid() {
        assert!(eval_with_builtins(r#"int("hello")"#).is_err());
        assert!(eval_with_builtins(r#"int("3.14")"#).is_err());
        assert!(eval_with_builtins(r#"int("")"#).is_err());
    }

    // float() tests
    #[test]
    fn test_float_from_int() {
        assert_eq!(eval_with_builtins("float(42)").unwrap(), Value::Float(42.0));
        assert_eq!(
            eval_with_builtins("float(-10)").unwrap(),
            Value::Float(-10.0)
        );
        assert_eq!(eval_with_builtins("float(0)").unwrap(), Value::Float(0.0));
    }

    #[test]
    fn test_float_from_float() {
        assert_eq!(
            eval_with_builtins("float(3.14)").unwrap(),
            Value::Float(3.14)
        );
        assert_eq!(
            eval_with_builtins("float(-2.5)").unwrap(),
            Value::Float(-2.5)
        );
        assert_eq!(eval_with_builtins("float(0.0)").unwrap(), Value::Float(0.0));
    }

    #[test]
    fn test_float_from_bool() {
        assert_eq!(
            eval_with_builtins("float(true)").unwrap(),
            Value::Float(1.0)
        );
        assert_eq!(
            eval_with_builtins("float(false)").unwrap(),
            Value::Float(0.0)
        );
    }

    #[test]
    fn test_float_from_str() {
        assert_eq!(
            eval_with_builtins(r#"float("3.14")"#).unwrap(),
            Value::Float(3.14)
        );
        assert_eq!(
            eval_with_builtins(r#"float("-2.5")"#).unwrap(),
            Value::Float(-2.5)
        );
        assert_eq!(
            eval_with_builtins(r#"float("42")"#).unwrap(),
            Value::Float(42.0)
        );
        assert_eq!(
            eval_with_builtins(r#"float("  1.5  ")"#).unwrap(),
            Value::Float(1.5)
        );
    }

    #[test]
    fn test_float_from_str_invalid() {
        assert!(eval_with_builtins(r#"float("hello")"#).is_err());
        assert!(eval_with_builtins(r#"float("")"#).is_err());
    }

    // str() tests
    #[test]
    fn test_str_from_int() {
        assert_eq!(
            eval_with_builtins("str(42)").unwrap(),
            Value::Str("42".to_string())
        );
        assert_eq!(
            eval_with_builtins("str(-10)").unwrap(),
            Value::Str("-10".to_string())
        );
        assert_eq!(
            eval_with_builtins("str(0)").unwrap(),
            Value::Str("0".to_string())
        );
    }

    #[test]
    fn test_str_from_float() {
        assert_eq!(
            eval_with_builtins("str(3.14)").unwrap(),
            Value::Str("3.14".to_string())
        );
        assert_eq!(
            eval_with_builtins("str(42.0)").unwrap(),
            Value::Str("42.0".to_string())
        );
    }

    #[test]
    fn test_str_from_bool() {
        assert_eq!(
            eval_with_builtins("str(true)").unwrap(),
            Value::Str("true".to_string())
        );
        assert_eq!(
            eval_with_builtins("str(false)").unwrap(),
            Value::Str("false".to_string())
        );
    }

    #[test]
    fn test_str_from_str() {
        assert_eq!(
            eval_with_builtins(r#"str("hello")"#).unwrap(),
            Value::Str("hello".to_string())
        );
        assert_eq!(
            eval_with_builtins(r#"str("")"#).unwrap(),
            Value::Str("".to_string())
        );
    }

    #[test]
    fn test_str_from_tuple() {
        assert_eq!(
            eval_with_builtins("str((1, 2, 3))").unwrap(),
            Value::Str("(1, 2, 3)".to_string())
        );
    }
}
