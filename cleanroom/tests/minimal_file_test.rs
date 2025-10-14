//! Minimal file persistence test without container operations
//!
//! This test demonstrates file operations without using container operations
//! that cause runtime conflicts in async tests.

use cleanroom::{CleanroomConfig, CleanroomEnvironment, CleanroomGuard};
use std::fs;
use std::path::Path;
use std::sync::Arc;

/// Test file creation and cleanup in Cleanroom environment
#[tokio::test]
async fn test_minimal_file_operations() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Test file path
    let test_file = "cleanroom_minimal_test.txt";

    // Ensure file doesn't exist initially
    if Path::new(test_file).exists() {
        fs::remove_file(test_file).unwrap();
    }

    // Test file creation using direct filesystem operations
    let test_content = "Hello from Cleanroom minimal test!";
    fs::write(test_file, test_content).unwrap();

    // Verify file was created
    assert!(Path::new(test_file).exists());
    let content = fs::read_to_string(test_file).unwrap();
    assert_eq!(content, test_content);

    // Test file modification
    let modified_content = "Modified content from Cleanroom!";
    fs::write(test_file, modified_content).unwrap();

    let updated_content = fs::read_to_string(test_file).unwrap();
    assert_eq!(updated_content, modified_content);

    // Test file metadata
    let metadata = fs::metadata(test_file).unwrap();
    assert!(metadata.len() > 0);

    // Clean up the file
    fs::remove_file(test_file).unwrap();

    // Verify file is gone
    assert!(!Path::new(test_file).exists());

    // Verify environment was created successfully
    let metrics = environment_arc.get_metrics().await;
    assert!(metrics.resource_usage.cpu_usage_percent >= 0.0);
}

/// Test directory operations
#[tokio::test]
async fn test_minimal_directory_operations() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_dir = "cleanroom_minimal_dir";
    let test_file = format!("{}/test_file.txt", test_dir);

    // Clean up any existing directory
    if Path::new(test_dir).exists() {
        fs::remove_dir_all(test_dir).unwrap();
    }

    // Create directory
    fs::create_dir(test_dir).unwrap();

    // Verify directory exists
    assert!(Path::new(test_dir).is_dir());

    // Create file in directory
    let content = "Hello from subdirectory!";
    fs::write(&test_file, content).unwrap();

    // Verify file exists in directory
    assert!(Path::new(&test_file).exists());
    let read_content = fs::read_to_string(&test_file).unwrap();
    assert_eq!(read_content, content);

    // Test directory listing
    let entries: Vec<_> = fs::read_dir(test_dir).unwrap().collect();
    assert_eq!(entries.len(), 1);

    // Clean up directory and contents
    fs::remove_dir_all(test_dir).unwrap();
    assert!(!Path::new(test_dir).exists());
}

/// Test multiple file operations
#[tokio::test]
async fn test_minimal_multiple_file_operations() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_files = vec![
        "minimal_test1.txt",
        "minimal_test2.txt",
        "minimal_test3.txt",
    ];

    // Clean up any existing files
    for file in &test_files {
        if Path::new(file).exists() {
            fs::remove_file(file).unwrap();
        }
    }

    // Create multiple files
    for (i, file) in test_files.iter().enumerate() {
        let content = format!("Minimal test content {}", i + 1);
        fs::write(file, content).unwrap();
    }

    // Verify all files exist and have correct content
    for (i, file) in test_files.iter().enumerate() {
        assert!(Path::new(file).exists());
        let content = fs::read_to_string(file).unwrap();
        assert_eq!(content, format!("Minimal test content {}", i + 1));
    }

    // Clean up all files
    for file in &test_files {
        fs::remove_file(file).unwrap();
        assert!(!Path::new(file).exists());
    }
}

/// Test file operations with different content types
#[tokio::test]
async fn test_minimal_different_file_content_types() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_cases = vec![
        ("minimal_simple.txt", "Simple text content"),
        ("minimal_multiline.txt", "Line 1\nLine 2\nLine 3"),
        (
            "minimal_special_chars.txt",
            "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?",
        ),
        ("minimal_unicode.txt", "Unicode: 🚀 🧪 ✅ ❌ 📝 📊"),
    ];

    // Clean up any existing files
    for (file, _) in &test_cases {
        if Path::new(file).exists() {
            fs::remove_file(file).unwrap();
        }
    }

    // Create files with different content
    for (file, content) in &test_cases {
        fs::write(file, content).unwrap();
    }

    // Verify all files exist and have correct content
    for (file, expected_content) in &test_cases {
        assert!(Path::new(file).exists());
        let actual_content = fs::read_to_string(file).unwrap();
        assert_eq!(actual_content, *expected_content);
    }

    // Clean up all files
    for (file, _) in &test_cases {
        fs::remove_file(file).unwrap();
        assert!(!Path::new(file).exists());
    }
}
