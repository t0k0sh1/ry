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
        }
    }
}

impl std::error::Error for EvalError {}

/// Result type alias for evaluation operations
pub type Result<T> = std::result::Result<T, EvalError>;
