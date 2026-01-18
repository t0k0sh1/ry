use ry::validate_ry_file;
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
    let test_content = "println(\"Hello, Ry!\");";

    // Create a temporary .ry file
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
