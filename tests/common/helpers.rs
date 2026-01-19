//! Helper functions for integration tests
//!
//! Provides utility functions for test setup, assertions, and common operations.

use std::fs;
use std::path::Path;
use tempfile::TempDir;

/// Writes content to a file in the temp directory
pub fn write_file_in_temp(temp_dir: &TempDir, relative_path: &str, content: &str) -> String {
    let file_path = temp_dir.path().join(relative_path);

    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create parent directories");
    }

    fs::write(&file_path, content).expect("Failed to write file");
    file_path.to_string_lossy().to_string()
}

/// Reads content from a file
pub fn read_file<P: AsRef<Path>>(path: P) -> String {
    fs::read_to_string(path).expect("Failed to read file")
}

/// Checks if a file exists
pub fn file_exists<P: AsRef<Path>>(path: P) -> bool {
    path.as_ref().exists()
}

/// Checks if a directory exists
pub fn dir_exists<P: AsRef<Path>>(path: P) -> bool {
    path.as_ref().is_dir()
}

/// Creates a directory structure for testing
pub fn create_test_structure(temp_dir: &TempDir, dirs: &[&str]) {
    for dir in dirs {
        let dir_path = temp_dir.path().join(dir);
        fs::create_dir_all(dir_path).expect("Failed to create test directory");
    }
}

/// Asserts that a string contains a substring
pub fn assert_contains(haystack: &str, needle: &str) {
    assert!(
        haystack.contains(needle),
        "Expected '{}' to contain '{}'",
        haystack,
        needle
    );
}

/// Asserts that a file contains specific content
pub fn assert_file_contains<P: AsRef<Path>>(path: P, expected: &str) {
    let content = read_file(path);
    assert_contains(&content, expected);
}

/// Creates a mock command that always succeeds
pub fn mock_success_command() -> String {
    if cfg!(windows) {
        "cmd /c exit 0".to_string()
    } else {
        "true".to_string()
    }
}

/// Creates a mock command that always fails
pub fn mock_failure_command() -> String {
    if cfg!(windows) {
        "cmd /c exit 1".to_string()
    } else {
        "false".to_string()
    }
}

/// Returns a cross-platform echo command
pub fn echo_command(message: &str) -> String {
    if cfg!(windows) {
        format!("cmd /c echo {}", message)
    } else {
        format!("echo {}", message)
    }
}

/// Measures execution time of a closure
pub fn measure_time<F, R>(f: F) -> (R, std::time::Duration)
where
    F: FnOnce() -> R,
{
    let start = std::time::Instant::now();
    let result = f();
    let duration = start.elapsed();
    (result, duration)
}

/// Asserts that execution completes within a time bound
pub fn assert_time_bound<F, R>(f: F, max_duration: std::time::Duration) -> R
where
    F: FnOnce() -> R,
{
    let (result, duration) = measure_time(f);
    assert!(
        duration <= max_duration,
        "Execution took {:?}, expected <= {:?}",
        duration,
        max_duration
    );
    result
}
