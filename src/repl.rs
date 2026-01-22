use std::io::{self, BufRead, Write};

use crate::eval::{execute_statement, parse_program, Context, Lexer, Value};

const INDENT_SIZE: usize = 2;

/// Calculate the current indent level based on the buffer content
fn calculate_indent_level(buffer: &str) -> usize {
    let mut indent_stack: Vec<usize> = vec![0];

    for line in buffer.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Count leading spaces
        let line_indent = line.len() - line.trim_start().len();

        // Adjust indent stack based on current line's indentation
        while indent_stack.len() > 1 && line_indent < *indent_stack.last().unwrap() {
            indent_stack.pop();
        }

        // If line ends with colon, next line should be indented
        if trimmed.ends_with(':') {
            indent_stack.push(line_indent + INDENT_SIZE);
        }
    }

    *indent_stack.last().unwrap_or(&0)
}

/// Check if input needs continuation (ends with colon or is indented)
fn needs_continuation(input: &str) -> bool {
    let trimmed = input.trim_end();
    // Line ends with colon (starting a block)
    if trimmed.ends_with(':') {
        return true;
    }
    // Check if there's an unclosed block (has indent but no matching dedent)
    // Simple heuristic: if the last non-empty line is indented, continue
    let lines: Vec<&str> = input.lines().collect();
    if let Some(last_line) = lines.last() {
        let last_trimmed = last_line.trim();
        if !last_trimmed.is_empty() {
            // Check if last line starts with whitespace (indented)
            if last_line.starts_with(' ') || last_line.starts_with('\t') {
                return true;
            }
        }
    }
    false
}

/// Check if input starts with continuation keywords at current indentation
fn starts_with_continuation_keyword(input: &str) -> bool {
    let trimmed = input.trim();
    trimmed.starts_with("elif ") || trimmed.starts_with("else:")
}

/// Execute input string with context
/// Used by both file execution and REPL
pub fn execute_input(
    input: &str,
    ctx: &mut Context,
    print_results: bool,
) -> Result<Option<Value>, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let program = parse_program(&tokens)?;

    let mut last_value = None;
    for stmt in &program.statements {
        match execute_statement(stmt, ctx) {
            Ok(Some(value)) => {
                if print_results {
                    println!("{}", value);
                }
                last_value = Some(value);
            }
            Ok(None) => {}
            Err(e) => return Err(e.to_string()),
        }
    }
    Ok(last_value)
}

pub fn run_repl() {
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    // Create context for variable persistence across REPL lines
    let mut ctx = Context::with_builtins();
    let mut buffer = String::new();
    let mut in_multiline = false;

    loop {
        if in_multiline {
            let indent_level = calculate_indent_level(&buffer);
            let indent = " ".repeat(indent_level);
            print!("... {}", indent);
        } else {
            print!("ry> ");
        }
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

                // Handle exit commands
                if !in_multiline && (trimmed == "exit" || trimmed == "quit") {
                    break;
                }

                // Handle empty line in multiline mode
                if in_multiline && trimmed.is_empty() {
                    // Empty line in multiline mode - try to execute
                    buffer.push('\n');

                    // Execute the buffered input
                    execute_multiline_input(&buffer, &mut ctx);
                    buffer.clear();
                    in_multiline = false;
                    continue;
                }

                // Empty line in single-line mode
                if trimmed.is_empty() {
                    continue;
                }

                // In multiline mode, prepend auto-indent if the user didn't provide their own
                if in_multiline && !line.starts_with(' ') && !line.starts_with('\t') {
                    let indent_level = calculate_indent_level(&buffer);
                    let indent = " ".repeat(indent_level);
                    buffer.push_str(&indent);
                }

                // Accumulate input
                buffer.push_str(&line);

                // Check if we need to continue reading
                if needs_continuation(&buffer) || starts_with_continuation_keyword(trimmed) {
                    in_multiline = true;
                    continue;
                }

                // Check if this is a multiline block that needs more input
                // (e.g., after elif or else that ends with :)
                if in_multiline {
                    continue;
                }

                // Execute input using unified code path
                execute_multiline_input(&buffer, &mut ctx);

                buffer.clear();
                in_multiline = false;
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }
    }
}

fn execute_multiline_input(input: &str, ctx: &mut Context) {
    if let Err(e) = execute_input(input, ctx, true) {
        eprintln!("Error: {}", e);
    }
}
