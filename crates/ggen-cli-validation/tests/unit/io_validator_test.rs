//! Unit tests for IO validator

use ggen_cli_validation::IoValidator;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use tempfile::tempdir;

#[test]
fn test_validate_read_success() {
    let dir = tempdir().expect("Failed to create temp dir");
    let file_path = dir.path().join("test.txt");
    let mut file = File::create(&file_path).expect("Failed to create file");
    writeln!(file, "test").expect("Failed to write");

    let validator = IoValidator::new();
    assert!(validator.validate_read(&file_path).is_ok());
}

#[test]
fn test_validate_read_missing_file() {
    let validator = IoValidator::new();
    let result = validator.validate_read(Path::new("/nonexistent.txt"));
    assert!(result.is_err());
}

#[test]
fn test_validate_write_success() {
    let dir = tempdir().expect("Failed to create temp dir");
    let file_path = dir.path().join("output.txt");

    let validator = IoValidator::new();
    assert!(validator.validate_write(&file_path).is_ok());
}

#[test]
fn test_validate_write_missing_parent() {
    let validator = IoValidator::new();
    let result = validator.validate_write(Path::new("/nonexistent/dir/file.txt"));
    assert!(result.is_err());
}

#[test]
fn test_batch_validation() {
    let dir = tempdir().expect("Failed to create temp dir");
    let file1 = dir.path().join("file1.txt");
    let file2 = dir.path().join("file2.txt");

    File::create(&file1).expect("Failed to create file1");
    File::create(&file2).expect("Failed to create file2");

    let validator = IoValidator::new();
    let paths = vec![file1.as_path(), file2.as_path()];
    assert!(validator.validate_reads(&paths).is_ok());
}
