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
