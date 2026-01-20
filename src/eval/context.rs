use std::collections::HashMap;

use super::value::{TypeAnnotation, Value};

/// Information about a variable including its value and optional type constraint
#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub value: Value,
    pub type_constraint: Option<TypeAnnotation>,
}

/// A single scope containing variables
#[derive(Debug, Clone, Default)]
struct Scope {
    variables: HashMap<String, VariableInfo>,
}

/// Evaluation context that stores variables with scope support
#[derive(Debug, Clone)]
pub struct Context {
    scopes: Vec<Scope>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// RAII scope guard - automatically pops scope when dropped
///
/// This ensures that scopes are always properly cleaned up, even when
/// errors occur (e.g., when using the `?` operator).
pub struct ScopeGuard<'a> {
    context: &'a mut Context,
}

impl<'a> ScopeGuard<'a> {
    /// Create a new scope guard, pushing a new scope onto the context
    pub fn new(context: &'a mut Context) -> Self {
        context.push_scope();
        Self { context }
    }

    /// Get a mutable reference to the underlying context
    pub fn context(&mut self) -> &mut Context {
        self.context
    }
}

impl Drop for ScopeGuard<'_> {
    fn drop(&mut self) {
        self.context.pop_scope();
    }
}

impl Context {
    /// Create a new context with a global scope
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    /// Push a new scope onto the stack
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    /// Pop the current scope from the stack
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Set a variable in the context (without type constraint)
    /// If the variable exists in any outer scope, update it there
    /// Otherwise, create it in the current (innermost) scope
    pub fn set(&mut self, name: String, value: Value) {
        // Search from innermost to outermost scope for existing variable
        for scope in self.scopes.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut e) =
                scope.variables.entry(name.clone())
            {
                // Preserve existing type constraint
                let type_constraint = e.get().type_constraint.clone();
                e.insert(VariableInfo {
                    value,
                    type_constraint,
                });
                return;
            }
        }
        // Not found, create in current scope without type constraint
        if let Some(current) = self.scopes.last_mut() {
            current.variables.insert(
                name,
                VariableInfo {
                    value,
                    type_constraint: None,
                },
            );
        }
    }

    /// Set a typed variable in the context
    /// If the variable exists in any outer scope, update it there
    /// Otherwise, create it in the current (innermost) scope with the type constraint
    pub fn set_typed(
        &mut self,
        name: String,
        value: Value,
        type_constraint: Option<TypeAnnotation>,
    ) {
        // Search from innermost to outermost scope for existing variable
        for scope in self.scopes.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut e) =
                scope.variables.entry(name.clone())
            {
                // Keep the original type constraint if no new one is provided
                let constraint = type_constraint.or_else(|| e.get().type_constraint.clone());
                e.insert(VariableInfo {
                    value,
                    type_constraint: constraint,
                });
                return;
            }
        }
        // Not found, create in current scope
        if let Some(current) = self.scopes.last_mut() {
            current.variables.insert(
                name,
                VariableInfo {
                    value,
                    type_constraint,
                },
            );
        }
    }

    /// Get a variable's value from the context
    /// Searches from innermost to outermost scope
    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.variables.get(name) {
                return Some(&info.value);
            }
        }
        None
    }

    /// Get full variable info including type constraint
    /// Searches from innermost to outermost scope
    pub fn get_info(&self, name: &str) -> Option<&VariableInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.variables.get(name) {
                return Some(info);
            }
        }
        None
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

    #[test]
    fn test_scope_isolation() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(10));

        // Push a new scope
        ctx.push_scope();
        ctx.set("y".to_string(), Value::Int(20));

        // Both x and y are accessible
        assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
        assert_eq!(ctx.get("y"), Some(&Value::Int(20)));

        // Pop the scope
        ctx.pop_scope();

        // x is still accessible, but y is not
        assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
        assert!(ctx.get("y").is_none());
    }

    #[test]
    fn test_outer_variable_update() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(10));

        // Push a new scope and update x
        ctx.push_scope();
        ctx.set("x".to_string(), Value::Int(100));

        // x should be updated
        assert_eq!(ctx.get("x"), Some(&Value::Int(100)));

        // Pop the scope
        ctx.pop_scope();

        // x should still be 100 (updated in outer scope)
        assert_eq!(ctx.get("x"), Some(&Value::Int(100)));
    }

    #[test]
    fn test_nested_scopes() {
        let mut ctx = Context::new();
        ctx.set("a".to_string(), Value::Int(1));

        ctx.push_scope();
        ctx.set("b".to_string(), Value::Int(2));

        ctx.push_scope();
        ctx.set("c".to_string(), Value::Int(3));

        // All variables accessible
        assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
        assert_eq!(ctx.get("b"), Some(&Value::Int(2)));
        assert_eq!(ctx.get("c"), Some(&Value::Int(3)));

        ctx.pop_scope();
        assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
        assert_eq!(ctx.get("b"), Some(&Value::Int(2)));
        assert!(ctx.get("c").is_none());

        ctx.pop_scope();
        assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
        assert!(ctx.get("b").is_none());
        assert!(ctx.get("c").is_none());
    }

    #[test]
    fn test_shadow_and_restore() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(10));

        // This updates outer x, not shadows
        ctx.push_scope();
        ctx.set("x".to_string(), Value::Int(20));
        assert_eq!(ctx.get("x"), Some(&Value::Int(20)));

        ctx.pop_scope();
        // x was updated, not shadowed
        assert_eq!(ctx.get("x"), Some(&Value::Int(20)));
    }

    #[test]
    fn test_scope_guard_basic() {
        let mut ctx = Context::new();
        ctx.set("outer".to_string(), Value::Int(1));

        {
            let mut guard = ScopeGuard::new(&mut ctx);
            guard.context().set("inner".to_string(), Value::Int(2));

            // Both variables accessible inside guard
            assert_eq!(guard.context().get("outer"), Some(&Value::Int(1)));
            assert_eq!(guard.context().get("inner"), Some(&Value::Int(2)));
        } // ScopeGuard dropped here, pops scope

        // outer still accessible, inner is gone
        assert_eq!(ctx.get("outer"), Some(&Value::Int(1)));
        assert!(ctx.get("inner").is_none());
    }

    #[test]
    fn test_scope_guard_nested() {
        let mut ctx = Context::new();
        ctx.set("a".to_string(), Value::Int(1));

        {
            let mut guard1 = ScopeGuard::new(&mut ctx);
            guard1.context().set("b".to_string(), Value::Int(2));

            {
                let mut guard2 = ScopeGuard::new(guard1.context());
                guard2.context().set("c".to_string(), Value::Int(3));

                // All accessible
                assert_eq!(guard2.context().get("a"), Some(&Value::Int(1)));
                assert_eq!(guard2.context().get("b"), Some(&Value::Int(2)));
                assert_eq!(guard2.context().get("c"), Some(&Value::Int(3)));
            } // guard2 dropped

            // c is gone, a and b still accessible
            assert_eq!(guard1.context().get("a"), Some(&Value::Int(1)));
            assert_eq!(guard1.context().get("b"), Some(&Value::Int(2)));
            assert!(guard1.context().get("c").is_none());
        } // guard1 dropped

        // Only a remains
        assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
        assert!(ctx.get("b").is_none());
        assert!(ctx.get("c").is_none());
    }

    #[test]
    fn test_scope_guard_updates_outer() {
        let mut ctx = Context::new();
        ctx.set("x".to_string(), Value::Int(10));

        {
            let mut guard = ScopeGuard::new(&mut ctx);
            // Update outer variable
            guard.context().set("x".to_string(), Value::Int(100));
            assert_eq!(guard.context().get("x"), Some(&Value::Int(100)));
        }

        // x was updated in outer scope
        assert_eq!(ctx.get("x"), Some(&Value::Int(100)));
    }
}
