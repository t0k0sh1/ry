use ry::{evaluate_expression, validate_ry_file};
use ry::eval::Value;
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

// Integration tests for arithmetic operations
#[test]
fn test_evaluate_expression_addition() {
    assert_eq!(evaluate_expression("1+1").unwrap(), Value::Int(2));
    assert_eq!(evaluate_expression("1 + 1").unwrap(), Value::Int(2));
    assert_eq!(evaluate_expression("10+20").unwrap(), Value::Int(30));
}

#[test]
fn test_evaluate_expression_subtraction() {
    assert_eq!(evaluate_expression("2-1").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("2 - 1").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("10-5").unwrap(), Value::Int(5));
}

#[test]
fn test_evaluate_expression_multiplication() {
    assert_eq!(evaluate_expression("3*6").unwrap(), Value::Int(18));
    assert_eq!(evaluate_expression("3 * 6").unwrap(), Value::Int(18));
    assert_eq!(evaluate_expression("4*5").unwrap(), Value::Int(20));
}

#[test]
fn test_evaluate_expression_division() {
    assert_eq!(evaluate_expression("5/10").unwrap(), Value::Float(0.5));
    assert_eq!(evaluate_expression("5 / 10").unwrap(), Value::Float(0.5));
    assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
}

#[test]
fn test_evaluate_expression_operator_precedence() {
    // 2 * 3 + 4 = 6 + 4 = 10
    assert_eq!(evaluate_expression("2*3+4").unwrap(), Value::Int(10));
    assert_eq!(evaluate_expression("2 * 3 + 4").unwrap(), Value::Int(10));

    // 10 / 2 - 1 = 5.0 - 1 = 4.0 (除算の結果はFloat)
    assert_eq!(evaluate_expression("10/2-1").unwrap(), Value::Float(4.0));
    assert_eq!(evaluate_expression("10 / 2 - 1").unwrap(), Value::Float(4.0));

    // 1 + 2 * 3 = 1 + 6 = 7
    assert_eq!(evaluate_expression("1+2*3").unwrap(), Value::Int(7));
}

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
    assert!(result.unwrap_err().contains("Error at line"));

    // Cleanup
    fs::remove_file(&test_file).unwrap();
}
