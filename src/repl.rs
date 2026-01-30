use std::env;
use std::path::PathBuf;

use rustyline::error::ReadlineError;
use rustyline::hint::HistoryHinter;
use rustyline::{Completer, Editor, Helper, Highlighter, Hinter, Validator};

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

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct RyHelper {
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl RyHelper {
    fn new() -> Self {
        Self {
            hinter: HistoryHinter::new(),
        }
    }
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
    let helper = RyHelper::new();
    let mut rl = Editor::new().expect("Failed to create editor");
    rl.set_helper(Some(helper));

    // Load history from file
    let history_path = env::var_os("HOME")
        .map(|h| PathBuf::from(h).join(".ry_history"))
        .unwrap_or_else(|| PathBuf::from(".ry_history"));
    let _ = rl.load_history(&history_path);

    // Create context for variable persistence across REPL lines
    let mut ctx = Context::with_builtins();
    let mut buffer = String::new();
    let mut in_multiline = false;

    loop {
        let prompt = if in_multiline { "... " } else { "ry> " };

        let result = if in_multiline {
            let indent = " ".repeat(calculate_indent_level(&buffer));
            rl.readline_with_initial(prompt, (&indent, ""))
        } else {
            rl.readline(prompt)
        };

        match result {
            Ok(line) => {
                let trimmed = line.trim();

                // Handle exit commands (only in single-line mode)
                if !in_multiline && (trimmed == "exit" || trimmed == "quit") {
                    break;
                }

                // Handle empty line in multiline mode - execute buffered input
                if in_multiline && trimmed.is_empty() {
                    buffer.push('\n');

                    // Add to history before execution
                    let _ = rl.add_history_entry(&buffer);

                    // Execute the buffered input
                    if let Err(e) = execute_input(&buffer, &mut ctx, true) {
                        eprintln!("Error: {}", e);
                    }
                    buffer.clear();
                    in_multiline = false;
                    continue;
                }

                // Skip empty lines in single-line mode
                if !in_multiline && trimmed.is_empty() {
                    continue;
                }

                // Accumulate input
                if !buffer.is_empty() {
                    buffer.push('\n');
                }
                buffer.push_str(&line);

                // Check if we need to continue reading
                if needs_continuation(&buffer) || starts_with_continuation_keyword(trimmed) {
                    in_multiline = true;
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(&buffer);

                // Execute input
                if let Err(e) = execute_input(&buffer, &mut ctx, true) {
                    eprintln!("Error: {}", e);
                }

                buffer.clear();
                in_multiline = false;
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C - cancel current input
                buffer.clear();
                in_multiline = false;
                continue;
            }
            Err(ReadlineError::Eof) => {
                // Ctrl+D - exit
                println!();
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    let _ = rl.save_history(&history_path);
}
