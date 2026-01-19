use std::fmt;

/// Evaluation errors that can occur during expression evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// Division by zero error
    DivisionByZero,
    /// Unsupported types for the given operation
    UnsupportedTypes(&'static str),
    /// Undefined variable error
    UndefinedVariable(String),
    /// Indentation error
    IndentationError(String),
    /// Type error in condition
    TypeError(String),
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
            EvalError::TypeError(msg) => {
                write!(f, "Type error: {}", msg)
            }
        }
    }
}

impl std::error::Error for EvalError {}

/// Result type alias for evaluation operations
pub type Result<T> = std::result::Result<T, EvalError>;
