use std::fmt;

use super::value::Value;

/// Evaluation errors that can occur during expression evaluation
#[derive(Debug, Clone)]
pub enum EvalError {
    /// Division by zero error
    DivisionByZero,
    /// Unsupported types for the given operation
    UnsupportedTypes(&'static str),
    /// Undefined variable error
    UndefinedVariable(String),
    /// Indentation error
    IndentationError(String),
    /// Lexer error (invalid characters, unterminated strings, etc.)
    LexerError(String),
    /// Type error in condition
    TypeError(String),
    /// Type mismatch error for typed variable assignment
    TypeMismatch {
        expected: String,
        actual: String,
        variable: String,
    },
    /// Tuple unpack element count mismatch
    TupleUnpackMismatch { expected: usize, actual: usize },
    /// Cannot unpack a non-tuple value
    CannotUnpackNonTuple(String),
    /// Argument count mismatch in function call
    ArgumentCountMismatch {
        expected: usize,
        actual: usize,
        func_name: String,
    },
    /// Argument type mismatch in function call
    ArgumentTypeMismatch {
        expected: String,
        actual: String,
        param_name: String,
        func_name: String,
    },
    /// Function signature mismatch when passing function as argument
    FunctionSignatureMismatch {
        expected: String,
        actual: String,
        param_name: String,
        func_name: String,
    },
    /// Return type mismatch
    ReturnTypeMismatch {
        expected: String,
        actual: String,
        func_name: String,
    },
    /// Calling a non-callable value
    NotCallable(String),
    /// Return statement outside of function
    ReturnOutsideFunction,
    /// Return control flow (used internally, not a real error)
    ReturnValue(Option<Value>),
    /// No matching overload found for function call
    NoMatchingOverload {
        func_name: String,
        arg_types: Vec<String>,
        available_signatures: Vec<String>,
    },
    /// Multiple overloads match with same priority (ambiguous)
    AmbiguousOverload {
        func_name: String,
        arg_types: Vec<String>,
        matching_signatures: Vec<String>,
    },
    /// Duplicate function signature in overload
    DuplicateSignature {
        func_name: String,
        signature: String,
    },
    /// Type conversion error (e.g., parsing string to int)
    ConversionError {
        from_type: String,
        to_type: String,
        value: String,
    },
    /// Parse error wrapper
    ParseError(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::DivisionByZero => write!(f, "Division by zero"),
            EvalError::UnsupportedTypes(op) => {
                write!(f, "Unsupported types for {}", op)
            }
            EvalError::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            EvalError::IndentationError(msg) => {
                write!(f, "Indentation error: {}", msg)
            }
            EvalError::LexerError(msg) => {
                write!(f, "Lexer error: {}", msg)
            }
            EvalError::TypeError(msg) => {
                write!(f, "Type error: {}", msg)
            }
            EvalError::TypeMismatch {
                expected,
                actual,
                variable,
            } => {
                write!(
                    f,
                    "Type mismatch for '{}': expected {}, got {}",
                    variable, expected, actual
                )
            }
            EvalError::TupleUnpackMismatch { expected, actual } => {
                write!(
                    f,
                    "Tuple unpack mismatch: expected {} elements, got {}",
                    expected, actual
                )
            }
            EvalError::CannotUnpackNonTuple(type_name) => {
                write!(f, "Cannot unpack non-tuple value of type {}", type_name)
            }
            EvalError::ArgumentCountMismatch {
                expected,
                actual,
                func_name,
            } => {
                write!(
                    f,
                    "Function '{}' expects {} arguments, got {}",
                    func_name, expected, actual
                )
            }
            EvalError::ArgumentTypeMismatch {
                expected,
                actual,
                param_name,
                func_name,
            } => {
                write!(
                    f,
                    "Type mismatch for parameter '{}' in function '{}': expected {}, got {}",
                    param_name, func_name, expected, actual
                )
            }
            EvalError::FunctionSignatureMismatch {
                expected,
                actual,
                param_name,
                func_name,
            } => {
                write!(
                    f,
                    "Function signature mismatch for parameter '{}' in function '{}': expected {}, got {}",
                    param_name, func_name, expected, actual
                )
            }
            EvalError::ReturnTypeMismatch {
                expected,
                actual,
                func_name,
            } => {
                write!(
                    f,
                    "Return type mismatch in function '{}': expected {}, got {}",
                    func_name, expected, actual
                )
            }
            EvalError::NotCallable(type_name) => {
                write!(f, "Cannot call value of type {}", type_name)
            }
            EvalError::ReturnOutsideFunction => {
                write!(f, "Return statement outside of function")
            }
            EvalError::ReturnValue(_) => {
                // This should not be displayed to users - it's internal control flow
                write!(f, "Internal: return value")
            }
            EvalError::NoMatchingOverload {
                func_name,
                arg_types,
                available_signatures,
            } => {
                write!(
                    f,
                    "No matching overload for function '{}' with arguments ({}). Available signatures: {}",
                    func_name,
                    arg_types.join(", "),
                    available_signatures.join(", ")
                )
            }
            EvalError::AmbiguousOverload {
                func_name,
                arg_types,
                matching_signatures,
            } => {
                write!(
                    f,
                    "Ambiguous overload for function '{}' with arguments ({}). Matching signatures: {}",
                    func_name,
                    arg_types.join(", "),
                    matching_signatures.join(", ")
                )
            }
            EvalError::DuplicateSignature {
                func_name,
                signature,
            } => {
                write!(
                    f,
                    "Duplicate function signature for '{}': {}",
                    func_name, signature
                )
            }
            EvalError::ConversionError {
                from_type,
                to_type,
                value,
            } => {
                write!(
                    f,
                    "Cannot convert {} value '{}' to {}",
                    from_type, value, to_type
                )
            }
            EvalError::ParseError(msg) => {
                write!(f, "Parse error: {}", msg)
            }
        }
    }
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EvalError::DivisionByZero, EvalError::DivisionByZero) => true,
            (EvalError::UnsupportedTypes(a), EvalError::UnsupportedTypes(b)) => a == b,
            (EvalError::UndefinedVariable(a), EvalError::UndefinedVariable(b)) => a == b,
            (EvalError::IndentationError(a), EvalError::IndentationError(b)) => a == b,
            (EvalError::LexerError(a), EvalError::LexerError(b)) => a == b,
            (EvalError::TypeError(a), EvalError::TypeError(b)) => a == b,
            (
                EvalError::TypeMismatch {
                    expected: e1,
                    actual: a1,
                    variable: v1,
                },
                EvalError::TypeMismatch {
                    expected: e2,
                    actual: a2,
                    variable: v2,
                },
            ) => e1 == e2 && a1 == a2 && v1 == v2,
            (
                EvalError::TupleUnpackMismatch {
                    expected: e1,
                    actual: a1,
                },
                EvalError::TupleUnpackMismatch {
                    expected: e2,
                    actual: a2,
                },
            ) => e1 == e2 && a1 == a2,
            (EvalError::CannotUnpackNonTuple(a), EvalError::CannotUnpackNonTuple(b)) => a == b,
            (
                EvalError::ArgumentCountMismatch {
                    expected: e1,
                    actual: a1,
                    func_name: f1,
                },
                EvalError::ArgumentCountMismatch {
                    expected: e2,
                    actual: a2,
                    func_name: f2,
                },
            ) => e1 == e2 && a1 == a2 && f1 == f2,
            (
                EvalError::ArgumentTypeMismatch {
                    expected: e1,
                    actual: a1,
                    param_name: p1,
                    func_name: f1,
                },
                EvalError::ArgumentTypeMismatch {
                    expected: e2,
                    actual: a2,
                    param_name: p2,
                    func_name: f2,
                },
            ) => e1 == e2 && a1 == a2 && p1 == p2 && f1 == f2,
            (
                EvalError::FunctionSignatureMismatch {
                    expected: e1,
                    actual: a1,
                    param_name: p1,
                    func_name: f1,
                },
                EvalError::FunctionSignatureMismatch {
                    expected: e2,
                    actual: a2,
                    param_name: p2,
                    func_name: f2,
                },
            ) => e1 == e2 && a1 == a2 && p1 == p2 && f1 == f2,
            (
                EvalError::ReturnTypeMismatch {
                    expected: e1,
                    actual: a1,
                    func_name: f1,
                },
                EvalError::ReturnTypeMismatch {
                    expected: e2,
                    actual: a2,
                    func_name: f2,
                },
            ) => e1 == e2 && a1 == a2 && f1 == f2,
            (EvalError::NotCallable(a), EvalError::NotCallable(b)) => a == b,
            (EvalError::ReturnOutsideFunction, EvalError::ReturnOutsideFunction) => true,
            (EvalError::ReturnValue(_), EvalError::ReturnValue(_)) => {
                // ReturnValue comparison is not meaningful - always false
                false
            }
            (
                EvalError::NoMatchingOverload {
                    func_name: f1,
                    arg_types: a1,
                    available_signatures: s1,
                },
                EvalError::NoMatchingOverload {
                    func_name: f2,
                    arg_types: a2,
                    available_signatures: s2,
                },
            ) => f1 == f2 && a1 == a2 && s1 == s2,
            (
                EvalError::AmbiguousOverload {
                    func_name: f1,
                    arg_types: a1,
                    matching_signatures: s1,
                },
                EvalError::AmbiguousOverload {
                    func_name: f2,
                    arg_types: a2,
                    matching_signatures: s2,
                },
            ) => f1 == f2 && a1 == a2 && s1 == s2,
            (
                EvalError::DuplicateSignature {
                    func_name: f1,
                    signature: s1,
                },
                EvalError::DuplicateSignature {
                    func_name: f2,
                    signature: s2,
                },
            ) => f1 == f2 && s1 == s2,
            (
                EvalError::ConversionError {
                    from_type: f1,
                    to_type: t1,
                    value: v1,
                },
                EvalError::ConversionError {
                    from_type: f2,
                    to_type: t2,
                    value: v2,
                },
            ) => f1 == f2 && t1 == t2 && v1 == v2,
            (EvalError::ParseError(a), EvalError::ParseError(b)) => a == b,
            _ => false,
        }
    }
}

impl std::error::Error for EvalError {}

/// Result type alias for evaluation operations
pub type Result<T> = std::result::Result<T, EvalError>;
