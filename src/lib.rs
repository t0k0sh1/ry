pub mod eval;

use std::io::{self, BufRead, Write};
use std::path::Path;

pub use eval::{evaluate_expression, evaluate_expression_with_context, Context, EvalError, Value};

pub fn validate_ry_file(path: &str) -> Result<(), String> {
    let path_obj = Path::new(path);
    match path_obj.extension() {
        Some(ext) if ext == "ry" => Ok(()),
        Some(_) => Err("Not a Ry language file".to_string()),
        None => Err("Not a Ry language file".to_string()),
    }
}

pub fn run_file(path: &str) -> Result<(), String> {
    // Validate file extension
    validate_ry_file(path)?;

    // Read file
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file '{}': {}", path, e))?;

    // Create context for variable persistence across lines
    let mut ctx = Context::new();

    // Evaluate each line
    for (line_num, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        match evaluate_expression_with_context(trimmed, &mut ctx) {
            Ok(result) => println!("{}", result),
            Err(e) => {
                return Err(format!("Error at line {}: {}", line_num + 1, e));
            }
        }
    }

    Ok(())
}

pub fn run_repl() {
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    // Create context for variable persistence across REPL lines
    let mut ctx = Context::new();

    loop {
        print!("ry> ");
        io::stdout().flush().expect("Failed to flush stdout");

        let mut line = String::new();
        match handle.read_line(&mut line) {
            Ok(0) => {
                // EOF (Ctrl+D)
                println!();
                break;
            }
            Ok(_) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                if trimmed == "exit" || trimmed == "quit" {
                    break;
                }
                // Evaluate the input
                match evaluate_expression_with_context(trimmed, &mut ctx) {
                    Ok(result) => println!("{}", result),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }
    }
}
