use std::fmt;
use std::ops::{Add, Mul, Sub};

use super::error::{EvalError, Result};

/// Maximum number of elements allowed in a tuple
pub const MAX_TUPLE_ELEMENTS: usize = 1024;

/// Type annotation for variable declarations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotation {
    Any,
    Int,
    Float,
    Bool,
    Str,
    Tuple(Vec<TypeAnnotation>),
}

impl TypeAnnotation {
    /// Check if a value matches this type annotation
    pub fn matches(&self, value: &Value) -> bool {
        match (self, value) {
            (TypeAnnotation::Any, _) => true,
            (TypeAnnotation::Int, Value::Int(_)) => true,
            (TypeAnnotation::Float, Value::Float(_)) => true,
            (TypeAnnotation::Bool, Value::Bool(_)) => true,
            (TypeAnnotation::Str, Value::Str(_)) => true,
            (TypeAnnotation::Tuple(types), Value::Tuple(values)) => {
                if types.len() != values.len() {
                    return false;
                }
                types.iter().zip(values.iter()).all(|(t, v)| t.matches(v))
            }
            _ => false,
        }
    }

    /// Get the type name as a string
    pub fn type_name(&self) -> String {
        match self {
            TypeAnnotation::Any => "any".to_string(),
            TypeAnnotation::Int => "int".to_string(),
            TypeAnnotation::Float => "float".to_string(),
            TypeAnnotation::Bool => "bool".to_string(),
            TypeAnnotation::Str => "str".to_string(),
            TypeAnnotation::Tuple(types) => {
                let inner: Vec<String> = types.iter().map(|t| t.type_name()).collect();
                format!("({})", inner.join(", "))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Tuple(Vec<Value>),
}

impl Value {
    /// Get the type name of this value
    pub fn type_name(&self) -> String {
        match self {
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Str(_) => "str".to_string(),
            Value::Tuple(values) => {
                let inner: Vec<String> = values.iter().map(|v| v.type_name()).collect();
                format!("({})", inner.join(", "))
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => {
                if *fl == (*fl as i64 as f64) {
                    write!(f, "{}", *fl as i64)
                } else {
                    write!(f, "{}", fl)
                }
            }
            Value::Bool(b) => write!(f, "{}", b),
            Value::Str(s) => write!(f, "{}", s),
            Value::Tuple(values) => {
                let inner: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", inner.join(", "))
            }
        }
    }
}

impl Value {
    /// Convert to float
    pub fn promote_to_float(self) -> Self {
        match self {
            Value::Int(i) => Value::Float(i as f64),
            Value::Float(f) => Value::Float(f),
            Value::Bool(b) => Value::Float(if b { 1.0 } else { 0.0 }),
            Value::Str(_) => panic!("Cannot convert string to float"),
            Value::Tuple(_) => panic!("Cannot convert tuple to float"),
        }
    }

    /// Convert to f64 (for backward compatibility)
    pub fn to_f64(self) -> f64 {
        match self {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            Value::Bool(b) => {
                if b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Str(_) => panic!("Cannot convert string to f64"),
            Value::Tuple(_) => panic!("Cannot convert tuple to f64"),
        }
    }

    /// Extract numeric pair for operations, returning (left, right, left_is_int, right_is_int)
    fn into_numeric_pair(self, other: Self) -> Result<(f64, f64, bool, bool)> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok((a as f64, b as f64, true, true)),
            (Value::Int(a), Value::Float(b)) => Ok((a as f64, b, true, false)),
            (Value::Float(a), Value::Int(b)) => Ok((a, b as f64, false, true)),
            (Value::Float(a), Value::Float(b)) => Ok((a, b, false, false)),
            _ => Err(EvalError::UnsupportedTypes("arithmetic")),
        }
    }

    /// Check if divisor is zero
    fn check_zero(divisor: f64) -> Result<()> {
        if divisor == 0.0 {
            Err(EvalError::DivisionByZero)
        } else {
            Ok(())
        }
    }

    /// Addition (internal implementation)
    fn add_impl(self, other: Self) -> Result<Self> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => match a.checked_add(b) {
                Some(result) => Ok(Value::Int(result)),
                None => Ok(Value::Float(a as f64 + b as f64)),
            },
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(a + &b)),
            _ => Err(EvalError::UnsupportedTypes("addition")),
        }
    }

    /// Subtraction (internal implementation)
    fn subtract_impl(self, other: Self) -> Result<Self> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => match a.checked_sub(b) {
                Some(result) => Ok(Value::Int(result)),
                None => Ok(Value::Float(a as f64 - b as f64)),
            },
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            _ => Err(EvalError::UnsupportedTypes("subtraction")),
        }
    }

    /// Multiplication (internal implementation)
    fn multiply_impl(self, other: Self) -> Result<Self> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => match a.checked_mul(b) {
                Some(result) => Ok(Value::Int(result)),
                None => Ok(Value::Float(a as f64 * b as f64)),
            },
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            _ => Err(EvalError::UnsupportedTypes("multiplication")),
        }
    }

    /// Safe addition that returns Result
    pub fn safe_add(self, other: Self) -> Result<Self> {
        self.add_impl(other)
    }

    /// Safe subtraction that returns Result
    pub fn safe_sub(self, other: Self) -> Result<Self> {
        self.subtract_impl(other)
    }

    /// Safe multiplication that returns Result
    pub fn safe_mul(self, other: Self) -> Result<Self> {
        self.multiply_impl(other)
    }

    /// Division (always returns Float)
    pub fn divide(self, other: Self) -> Result<Self> {
        let (a, b, _, _) = self.into_numeric_pair(other)?;
        Self::check_zero(b)?;
        Ok(Value::Float(a / b))
    }

    /// Modulo operation
    pub fn modulo(self, other: Self) -> Result<Self> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                Self::check_zero(b as f64)?;
                Ok(Value::Int(a % b))
            }
            (left, right) => {
                let (a, b, _, _) = left.into_numeric_pair(right)?;
                Self::check_zero(b)?;
                Ok(Value::Float(a % b))
            }
        }
    }

    /// Power operation
    pub fn power(self, other: Self) -> Result<Self> {
        let base = self.to_f64();
        let exponent = other.to_f64();
        Ok(Value::Float(base.powf(exponent)))
    }

    /// Floor division
    pub fn floor_divide(self, other: Self) -> Result<Self> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                Self::check_zero(b as f64)?;
                Ok(Value::Int(a / b))
            }
            (left, right) => {
                let (a, b, _, _) = left.into_numeric_pair(right)?;
                Self::check_zero(b)?;
                Ok(Value::Float((a / b).floor()))
            }
        }
    }

    /// Generic comparison operation
    fn compare<F>(self, other: Self, cmp: F, op_name: &'static str) -> Result<Self>
    where
        F: Fn(f64, f64) -> bool,
    {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(cmp(a as f64, b as f64))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(cmp(a as f64, b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(cmp(a, b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(cmp(a, b))),
            _ => Err(EvalError::UnsupportedTypes(op_name)),
        }
    }

    /// Equality comparison
    pub fn compare_eq(self, other: Self) -> Result<Self> {
        match (&self, &other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a == b)),
            (Value::Tuple(a), Value::Tuple(b)) => {
                if a.len() != b.len() {
                    return Ok(Value::Bool(false));
                }
                for (av, bv) in a.iter().zip(b.iter()) {
                    match av.clone().compare_eq(bv.clone())? {
                        Value::Bool(false) => return Ok(Value::Bool(false)),
                        _ => continue,
                    }
                }
                Ok(Value::Bool(true))
            }
            _ => self.compare(other, |a, b| a == b, "comparison"),
        }
    }

    /// Not equal comparison
    pub fn compare_ne(self, other: Self) -> Result<Self> {
        match (&self, &other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a != b)),
            (Value::Tuple(a), Value::Tuple(b)) => {
                if a.len() != b.len() {
                    return Ok(Value::Bool(true));
                }
                for (av, bv) in a.iter().zip(b.iter()) {
                    match av.clone().compare_ne(bv.clone())? {
                        Value::Bool(true) => return Ok(Value::Bool(true)),
                        _ => continue,
                    }
                }
                Ok(Value::Bool(false))
            }
            _ => self.compare(other, |a, b| a != b, "comparison"),
        }
    }

    /// Less than comparison
    pub fn compare_lt(self, other: Self) -> Result<Self> {
        self.compare(other, |a, b| a < b, "comparison")
    }

    /// Greater than comparison
    pub fn compare_gt(self, other: Self) -> Result<Self> {
        self.compare(other, |a, b| a > b, "comparison")
    }

    /// Less than or equal comparison
    pub fn compare_le(self, other: Self) -> Result<Self> {
        self.compare(other, |a, b| a <= b, "comparison")
    }

    /// Greater than or equal comparison
    pub fn compare_ge(self, other: Self) -> Result<Self> {
        self.compare(other, |a, b| a >= b, "comparison")
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        self.add_impl(other)
            .expect("Unsupported types for addition")
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        self.subtract_impl(other)
            .expect("Unsupported types for subtraction")
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        self.multiply_impl(other)
            .expect("Unsupported types for multiplication")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_display() {
        assert_eq!(format!("{}", Value::Int(42)), "42");
        assert_eq!(format!("{}", Value::Float(3.14)), "3.14");
        assert_eq!(format!("{}", Value::Float(5.0)), "5");
        assert_eq!(format!("{}", Value::Bool(true)), "true");
        assert_eq!(format!("{}", Value::Bool(false)), "false");
    }

    #[test]
    fn test_addition() {
        assert_eq!(
            Value::Int(1).safe_add(Value::Int(2)).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            Value::Int(1).safe_add(Value::Float(1.5)).unwrap(),
            Value::Float(2.5)
        );
        assert_eq!(
            Value::Float(1.5).safe_add(Value::Int(1)).unwrap(),
            Value::Float(2.5)
        );
        assert_eq!(
            Value::Float(1.5).safe_add(Value::Float(1.5)).unwrap(),
            Value::Float(3.0)
        );
    }

    #[test]
    fn test_subtraction() {
        assert_eq!(
            Value::Int(5).safe_sub(Value::Int(3)).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            Value::Float(5.5).safe_sub(Value::Int(2)).unwrap(),
            Value::Float(3.5)
        );
    }

    #[test]
    fn test_multiplication() {
        assert_eq!(
            Value::Int(3).safe_mul(Value::Int(4)).unwrap(),
            Value::Int(12)
        );
        assert_eq!(
            Value::Float(2.5).safe_mul(Value::Int(2)).unwrap(),
            Value::Float(5.0)
        );
    }

    #[test]
    fn test_division() {
        assert_eq!(
            Value::Int(10).divide(Value::Int(2)).unwrap(),
            Value::Float(5.0)
        );
        assert_eq!(
            Value::Int(7).divide(Value::Int(2)).unwrap(),
            Value::Float(3.5)
        );
    }

    #[test]
    fn test_division_by_zero() {
        assert!(Value::Int(5).divide(Value::Int(0)).is_err());
        assert!(Value::Float(5.0).divide(Value::Float(0.0)).is_err());
    }

    #[test]
    fn test_modulo() {
        assert_eq!(Value::Int(10).modulo(Value::Int(3)).unwrap(), Value::Int(1));
        assert_eq!(
            Value::Float(10.5).modulo(Value::Float(3.0)).unwrap(),
            Value::Float(1.5)
        );
    }

    #[test]
    fn test_power() {
        assert_eq!(
            Value::Int(2).power(Value::Int(3)).unwrap(),
            Value::Float(8.0)
        );
    }

    #[test]
    fn test_floor_divide() {
        assert_eq!(
            Value::Int(10).floor_divide(Value::Int(3)).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            Value::Float(10.7).floor_divide(Value::Float(3.0)).unwrap(),
            Value::Float(3.0)
        );
    }

    #[test]
    fn test_comparisons() {
        assert_eq!(
            Value::Int(1).compare_eq(Value::Int(1)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Int(1).compare_ne(Value::Int(2)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Int(1).compare_lt(Value::Int(2)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Int(2).compare_gt(Value::Int(1)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Int(1).compare_le(Value::Int(1)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Int(1).compare_ge(Value::Int(1)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_bool_comparison() {
        assert_eq!(
            Value::Bool(true).compare_eq(Value::Bool(true)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Bool(true).compare_ne(Value::Bool(false)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_unsupported_types() {
        assert!(Value::Bool(true).safe_add(Value::Int(1)).is_err());
        assert!(Value::Bool(true).safe_sub(Value::Int(1)).is_err());
        assert!(Value::Bool(true).safe_mul(Value::Int(1)).is_err());
    }

    // TypeAnnotation tests
    #[test]
    fn test_type_annotation_matches() {
        assert!(TypeAnnotation::Int.matches(&Value::Int(42)));
        assert!(!TypeAnnotation::Int.matches(&Value::Float(3.14)));
        assert!(!TypeAnnotation::Int.matches(&Value::Bool(true)));

        assert!(TypeAnnotation::Float.matches(&Value::Float(3.14)));
        assert!(!TypeAnnotation::Float.matches(&Value::Int(42)));
        assert!(!TypeAnnotation::Float.matches(&Value::Bool(true)));

        assert!(TypeAnnotation::Bool.matches(&Value::Bool(true)));
        assert!(TypeAnnotation::Bool.matches(&Value::Bool(false)));
        assert!(!TypeAnnotation::Bool.matches(&Value::Int(42)));
        assert!(!TypeAnnotation::Bool.matches(&Value::Float(3.14)));
    }

    #[test]
    fn test_type_annotation_type_name() {
        assert_eq!(TypeAnnotation::Int.type_name(), "int");
        assert_eq!(TypeAnnotation::Float.type_name(), "float");
        assert_eq!(TypeAnnotation::Bool.type_name(), "bool");
    }

    #[test]
    fn test_value_type_name() {
        assert_eq!(Value::Int(42).type_name(), "int");
        assert_eq!(Value::Float(3.14).type_name(), "float");
        assert_eq!(Value::Bool(true).type_name(), "bool");
        assert_eq!(Value::Str("hello".to_string()).type_name(), "str");
    }

    // String tests
    #[test]
    fn test_string_display() {
        assert_eq!(format!("{}", Value::Str("hello".to_string())), "hello");
        assert_eq!(format!("{}", Value::Str("".to_string())), "");
        assert_eq!(
            format!("{}", Value::Str("hello world".to_string())),
            "hello world"
        );
    }

    #[test]
    fn test_string_concatenation() {
        assert_eq!(
            Value::Str("hello".to_string())
                .safe_add(Value::Str(" world".to_string()))
                .unwrap(),
            Value::Str("hello world".to_string())
        );
        assert_eq!(
            Value::Str("".to_string())
                .safe_add(Value::Str("test".to_string()))
                .unwrap(),
            Value::Str("test".to_string())
        );
    }

    #[test]
    fn test_string_equality() {
        assert_eq!(
            Value::Str("hello".to_string())
                .compare_eq(Value::Str("hello".to_string()))
                .unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Str("hello".to_string())
                .compare_eq(Value::Str("world".to_string()))
                .unwrap(),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_string_inequality() {
        assert_eq!(
            Value::Str("hello".to_string())
                .compare_ne(Value::Str("world".to_string()))
                .unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            Value::Str("hello".to_string())
                .compare_ne(Value::Str("hello".to_string()))
                .unwrap(),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_string_unsupported_ops() {
        // String + Int should fail
        assert!(Value::Str("hello".to_string())
            .safe_add(Value::Int(1))
            .is_err());
        // String - String should fail
        assert!(Value::Str("hello".to_string())
            .safe_sub(Value::Str("h".to_string()))
            .is_err());
        // String * Int should fail
        assert!(Value::Str("hello".to_string())
            .safe_mul(Value::Int(2))
            .is_err());
    }

    #[test]
    fn test_string_type_annotation_matches() {
        assert!(TypeAnnotation::Str.matches(&Value::Str("test".to_string())));
        assert!(!TypeAnnotation::Str.matches(&Value::Int(42)));
        assert!(!TypeAnnotation::Str.matches(&Value::Float(3.14)));
        assert!(!TypeAnnotation::Str.matches(&Value::Bool(true)));
        // Other types don't match string
        assert!(!TypeAnnotation::Int.matches(&Value::Str("test".to_string())));
    }

    #[test]
    fn test_string_type_annotation_type_name() {
        assert_eq!(TypeAnnotation::Str.type_name(), "str");
    }

    // Any type tests
    #[test]
    fn test_any_type_annotation_matches_all() {
        assert!(TypeAnnotation::Any.matches(&Value::Int(42)));
        assert!(TypeAnnotation::Any.matches(&Value::Float(3.14)));
        assert!(TypeAnnotation::Any.matches(&Value::Bool(true)));
        assert!(TypeAnnotation::Any.matches(&Value::Bool(false)));
        assert!(TypeAnnotation::Any.matches(&Value::Str("test".to_string())));
    }

    #[test]
    fn test_any_type_annotation_type_name() {
        assert_eq!(TypeAnnotation::Any.type_name(), "any");
    }
}
