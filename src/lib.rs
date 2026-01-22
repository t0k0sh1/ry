pub mod eval;
mod repl;

use std::path::Path;

pub use eval::{
    evaluate_expression, evaluate_expression_with_context, execute_program, parse_program, Context,
    EvalError, Lexer, Token, Value,
};
pub use repl::run_repl;

pub fn validate_ry_file(path: &str) -> Result<(), String> {
    let path_obj = Path::new(path);
    match path_obj.extension() {
        Some(ext) if ext == "ry" => Ok(()),
        Some(_) => Err("Not a Ry language file".to_string()),
        None => Err("Not a Ry language file".to_string()),
    }
}

pub fn run_file(path: &str) -> Result<(), String> {
    validate_ry_file(path)?;
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
    let mut ctx = Context::with_builtins();
    repl::execute_input(&content, &mut ctx, true)?;
    Ok(())
}
