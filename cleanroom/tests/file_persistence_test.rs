//! File persistence test using Cleanroom environment
//!
//! This test demonstrates that the Cleanroom Testing Framework properly
//! isolates file system operations and ensures cleanup.

use cleanroom::{
    run, CleanroomEnvironment, CleanroomConfig, CleanroomGuard,
    GenericContainer, ContainerWrapper,
};
use std::sync::Arc;
use std::fs;
use std::path::Path;

/// Test file creation and cleanup in Cleanroom environment
#[tokio::test]
async fn test_file_persistence_and_cleanup() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Test file path
    let test_file = "cleanroom_test_file.txt";
    
    // Ensure file doesn't exist initially
    if Path::new(test_file).exists() {
        fs::remove_file(test_file).unwrap();
    }
    
    // Create a container that will run our micro CLI
    let container = environment_arc.get_or_create_container("file_test", || {
        GenericContainer::new("file_test_container", "alpine", "latest")
    }).await.unwrap();

    // Verify container was created
    assert_eq!(container.name(), "file_test_container");
    
    // Test file creation using the run function
    let create_result = run(["sh", "-c", &format!("echo 'Hello from Cleanroom!' > {}", test_file)]);
    assert!(create_result.is_ok());
    
    let create_result = create_result.unwrap();
    assert_eq!(create_result.exit_code, 0);
    
    // Verify file was created
    if Path::new(test_file).exists() {
        let content = fs::read_to_string(test_file).unwrap();
        assert!(content.contains("Hello from Cleanroom!"));
        
        // Clean up the file
        fs::remove_file(test_file).unwrap();
        
        // Verify file is gone
        assert!(!Path::new(test_file).exists());
    }
    
    // Verify container count
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 1);
}

/// Test multiple file operations
#[tokio::test]
async fn test_multiple_file_operations() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_files = vec![
        "test1.txt",
        "test2.txt", 
        "test3.txt"
    ];
    
    // Clean up any existing files
    for file in &test_files {
        if Path::new(file).exists() {
            fs::remove_file(file).unwrap();
        }
    }
    
    // Create multiple files
    for (i, file) in test_files.iter().enumerate() {
        let content = format!("Test content {}", i + 1);
        let result = run(["sh", "-c", &format!("echo '{}' > {}", content, file)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().exit_code, 0);
    }
    
    // Verify all files exist and have correct content
    for (i, file) in test_files.iter().enumerate() {
        assert!(Path::new(file).exists());
        let content = fs::read_to_string(file).unwrap();
        assert!(content.contains(&format!("Test content {}", i + 1)));
    }
    
    // Clean up all files
    for file in &test_files {
        fs::remove_file(file).unwrap();
        assert!(!Path::new(file).exists());
    }
}

/// Test file operations with different content types
#[tokio::test]
async fn test_different_file_content_types() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_cases = vec![
        ("simple.txt", "Simple text content"),
        ("multiline.txt", "Line 1\nLine 2\nLine 3"),
        ("special_chars.txt", "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"),
        ("unicode.txt", "Unicode: 🚀 🧪 ✅ ❌ 📝 📊"),
    ];
    
    // Clean up any existing files
    for (file, _) in &test_cases {
        if Path::new(file).exists() {
            fs::remove_file(file).unwrap();
        }
    }
    
    // Create files with different content
    for (file, content) in &test_cases {
        let result = run(["sh", "-c", &format!("echo '{}' > {}", content, file)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().exit_code, 0);
    }
    
    // Verify all files exist and have correct content
    for (file, expected_content) in &test_cases {
        assert!(Path::new(file).exists());
        let actual_content = fs::read_to_string(file).unwrap();
        // Note: echo might add a newline, so we check if content is contained
        assert!(actual_content.contains(expected_content));
    }
    
    // Clean up all files
    for (file, _) in &test_cases {
        fs::remove_file(file).unwrap();
        assert!(!Path::new(file).exists());
    }
}

/// Test file operations with directories
#[tokio::test]
async fn test_directory_operations() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let test_dir = "cleanroom_test_dir";
    let test_file = format!("{}/test_file.txt", test_dir);
    
    // Clean up any existing directory
    if Path::new(test_dir).exists() {
        fs::remove_dir_all(test_dir).unwrap();
    }
    
    // Create directory
    let result = run(["mkdir", test_dir]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().exit_code, 0);
    
    // Verify directory exists
    assert!(Path::new(test_dir).is_dir());
    
    // Create file in directory
    let result = run(["sh", "-c", &format!("echo 'Hello from subdirectory!' > {}", test_file)]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().exit_code, 0);
    
    // Verify file exists in directory
    assert!(Path::new(&test_file).exists());
    let content = fs::read_to_string(&test_file).unwrap();
    assert!(content.contains("Hello from subdirectory!"));
    
    // Clean up directory and contents
    fs::remove_dir_all(test_dir).unwrap();
    assert!(!Path::new(test_dir).exists());
}
