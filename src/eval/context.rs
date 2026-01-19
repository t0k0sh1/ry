use std::collections::HashMap;

use super::value::Value;

/// Evaluation context that stores variables
#[derive(Debug, Clone, Default)]
pub struct Context {
    variables: HashMap<String, Value>,
}

impl Context {
    /// Create a new empty context
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a variable in the context
    pub fn set(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    /// Get a variable from the context
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_new() {
        let ctx = Context::new();
        assert!(ctx.get("x").is_none());
    }

    #[test]
    fn test_context_set_and_get() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(42));
        assert_eq!(ctx.get("x"), Some(&Value::Int(42)));
    }

    #[test]
    fn test_context_overwrite() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(1));
        ctx.set("x".to_string(), Value::Int(2));
        assert_eq!(ctx.get("x"), Some(&Value::Int(2)));
    }

    #[test]
    fn test_context_multiple_vars() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(1));
        ctx.set("y".to_string(), Value::Float(2.5));
        ctx.set("z".to_string(), Value::Bool(true));
        assert_eq!(ctx.get("x"), Some(&Value::Int(1)));
        assert_eq!(ctx.get("y"), Some(&Value::Float(2.5)));
        assert_eq!(ctx.get("z"), Some(&Value::Bool(true)));
    }
}
