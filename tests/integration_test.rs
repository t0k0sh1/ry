use ry::eval::Value;
use ry::{evaluate_expression_with_context, validate_ry_file, Context};
use std::fs;
use std::io::Write;
use std::path::Path;

#[test]
fn test_validate_ry_file_function_with_ry_extension() {
    assert!(validate_ry_file("test.ry").is_ok());
    assert!(validate_ry_file("/path/to/file.ry").is_ok());
    assert!(validate_ry_file("dir/subdir/test.ry").is_ok());
}

#[test]
fn test_validate_ry_file_function_with_non_ry_extension() {
    assert!(validate_ry_file("test.txt").is_err());
    assert!(validate_ry_file("test.rs").is_err());
    assert!(validate_ry_file("test.py").is_err());
    assert!(validate_ry_file("test.js").is_err());
}

#[test]
fn test_validate_ry_file_function_with_no_extension() {
    assert!(validate_ry_file("test").is_err());
    assert!(validate_ry_file("/path/to/file").is_err());
}

#[test]
fn test_validate_ry_file_error_message() {
    let result = validate_ry_file("test.txt");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Not a Ry language file");

    let result = validate_ry_file("test");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Not a Ry language file");
}

// Integration test for file reading
#[test]
fn test_read_existing_ry_file() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_read.ry");
    let test_content = "println(\"Hello, Ry!\");";

    // Create a temporary .ry file
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Read the file
    let content = fs::read_to_string(&test_file).unwrap();
    assert_eq!(content, test_content);

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_read_nonexistent_file_returns_error() {
    let nonexistent_file = Path::new("/nonexistent/path/file.ry");
    assert!(!nonexistent_file.exists());

    let result = fs::read_to_string(nonexistent_file);
    assert!(result.is_err());
}

#[test]
fn test_run_file_with_valid_ry_file() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_run.ry");
    let test_content = "1+1";

    // Create a temporary .ry file with a valid arithmetic expression
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_run_file_with_invalid_extension() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_run.txt");
    let test_content = "some content";

    // Create a temporary .txt file
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function with invalid extension
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Not a Ry language file");

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_run_file_with_nonexistent_file() {
    let nonexistent_file = "/nonexistent/path/file.ry";
    let result = ry::run_file(nonexistent_file);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Failed to read file"));
}

// Note: Basic arithmetic tests (addition, subtraction, multiplication, division, operator precedence)
// are covered in src/eval/tests.rs. Integration tests below focus on file I/O and multi-line scripts.

#[test]
fn test_run_file_with_arithmetic_expressions() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_arithmetic.ry");
    let test_content = "1+1\n2-1\n3*6\n5/10";

    // Create a temporary .ry file with arithmetic expressions
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_run_file_with_complex_arithmetic() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_complex.ry");
    let test_content = "2*3+4\n10/2-1\n1+2*3";

    // Create a temporary .ry file with complex arithmetic expressions
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_run_file_with_invalid_expression() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_invalid.ry");
    let test_content = "1+\n2++3";

    // Create a temporary .ry file with invalid expressions
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function should return error
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_err());
    // Error message contains information about the unexpected token
    let error_msg = result.unwrap_err();
    assert!(
        error_msg.contains("unexpected") || error_msg.contains("Error"),
        "Expected error message, got: {}",
        error_msg
    );

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

// Integration tests for variable assignment
#[test]
fn test_variable_assignment_integration() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_expression_with_context("y = 10", &mut ctx).unwrap(),
        Value::Int(10)
    );
    assert_eq!(
        evaluate_expression_with_context("x + y", &mut ctx).unwrap(),
        Value::Int(15)
    );
}

#[test]
fn test_variable_persistence_across_lines() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    evaluate_expression_with_context("y = 10", &mut ctx).unwrap();
    assert_eq!(
        evaluate_expression_with_context("z = x * y + 1", &mut ctx).unwrap(),
        Value::Int(51)
    );
    assert_eq!(
        evaluate_expression_with_context("z", &mut ctx).unwrap(),
        Value::Int(51)
    );
}

#[test]
fn test_run_file_with_variables() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_variables.ry");
    let test_content = "x = 5\ny = 10\nx + y";

    // Create a temporary .ry file with variable expressions
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_chain_assignment_integration() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x = y = z = 42", &mut ctx).unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_expression_with_context("x", &mut ctx).unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_expression_with_context("y", &mut ctx).unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_expression_with_context("z", &mut ctx).unwrap(),
        Value::Int(42)
    );
}

#[test]
fn test_undefined_variable_error() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("undefined_var", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Undefined variable"));
}

// Helper function to parse and execute a program with context
fn run_program_with_context(input: &str, ctx: &mut Context) -> Result<Option<Value>, String> {
    use ry::{execute_program, parse_program, Lexer};
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let program = parse_program(&tokens)?;
    execute_program(&program, ctx).map_err(|e| e.to_string())
}

// ========================================
// UFCS (Uniform Function Call Syntax) Integration Tests
// ========================================

#[test]
fn test_ufcs_basic_integration() {
    // Define a function and call it using UFCS
    let mut ctx = Context::new();

    // Define add function
    let result = run_program_with_context("fn add(a, b):\n    return a + b\n", &mut ctx);
    assert!(result.is_ok());

    // Call using UFCS: 10.add(20) -> add(10, 20)
    let result = evaluate_expression_with_context("10.add(20)", &mut ctx);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Int(30));
}

#[test]
fn test_ufcs_chained_integration() {
    let mut ctx = Context::new();

    // Define add and mul functions
    run_program_with_context(
        "fn add(a, b):\n    return a + b\nfn mul(a, b):\n    return a * b\n",
        &mut ctx,
    )
    .unwrap();

    // Chained UFCS: 10.add(5).mul(2) -> mul(add(10, 5), 2) = 30
    let result = evaluate_expression_with_context("10.add(5).mul(2)", &mut ctx);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Int(30));
}

#[test]
fn test_ufcs_on_expression_integration() {
    let mut ctx = Context::new();

    // Define double function
    run_program_with_context("fn double(x):\n    return x * 2\n", &mut ctx).unwrap();

    // UFCS on grouped expression: (1 + 2).double() -> double(3) = 6
    let result = evaluate_expression_with_context("(1 + 2).double()", &mut ctx);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Int(6));
}

#[test]
fn test_ufcs_with_variable_integration() {
    let mut ctx = Context::new();

    // Define clamp function
    run_program_with_context(
        "fn clamp(x, lo, hi):\n    if x < lo:\n        return lo\n    if x > hi:\n        return hi\n    return x\n",
        &mut ctx,
    )
    .unwrap();

    // Set variable
    evaluate_expression_with_context("x = 15", &mut ctx).unwrap();

    // UFCS with variable: x.clamp(0, 10) -> clamp(15, 0, 10) = 10
    let result = evaluate_expression_with_context("x.clamp(0, 10)", &mut ctx);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Int(10));
}

#[test]
fn test_float_literal_not_ufcs() {
    // 10.5 should remain a float literal
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("10.5", &mut ctx);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Float(10.5));
}

// ========================================
// Command Syntax Integration Tests
// ========================================

#[test]
fn test_command_syntax_basic_integration() {
    let mut ctx = Context::new();

    // Define add function
    run_program_with_context("fn add(a, b):\n    return a + b\n", &mut ctx).unwrap();

    // Call using Command Syntax: add 10, 20 -> add(10, 20)
    let result = run_program_with_context("add 10, 20\n", &mut ctx);
    assert!(result.is_ok());
}

#[test]
fn test_command_syntax_single_arg_integration() {
    let mut ctx = Context::new();

    // Define square function
    run_program_with_context("fn square(x):\n    return x * x\n", &mut ctx).unwrap();

    // Call using Command Syntax: square 5 -> square(5)
    let result = run_program_with_context("square 5\n", &mut ctx);
    assert!(result.is_ok());
}

#[test]
fn test_command_syntax_expression_args_integration() {
    let mut ctx = Context::new();

    // Define add function
    run_program_with_context("fn add(a, b):\n    return a + b\n", &mut ctx).unwrap();

    // Call using Command Syntax with expression arguments:
    // add 10 + 5, 20 * 2 -> add(15, 40) = 55
    let result = run_program_with_context("add 10 + 5, 20 * 2\n", &mut ctx);
    assert!(result.is_ok());
}

#[test]
fn test_command_syntax_in_file() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_command_syntax.ry");
    let test_content = "fn greet(name):\n    return name\n\ngreet \"World\"\n";

    // Create a temporary .ry file with command syntax
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}

#[test]
fn test_ufcs_in_file() {
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("test_ufcs.ry");
    let test_content = "fn double(x):\n    return x * 2\n\n5.double()\n";

    // Create a temporary .ry file with UFCS
    let mut file = fs::File::create(&test_file).unwrap();
    file.write_all(test_content.as_bytes()).unwrap();
    drop(file);

    // Test run_file function
    let result = ry::run_file(test_file.to_str().unwrap());
    assert!(result.is_ok());

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}
