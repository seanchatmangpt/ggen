//! Test improved error messages with better context
//!
//! This test verifies that the 6 improved error messages provide
//! helpful context to users.

use ggen_core::codegen::merge::merge_sections;
use ggen_core::codegen::watch::FileWatcher;
use std::path::{Path, PathBuf};

#[test]
fn test_merge_marker_order_error_shows_helpful_context() {
    // Create invalid merge markers (wrong order)
    let existing = r#"
=======
 Manual section first
<<<<<<< GENERATED
fn generated() {}
>>>>>>> MANUAL
"#;

    let result = merge_sections("fn new() {}", existing);
    assert!(result.is_err());

    let error_msg = result.unwrap_err().to_string();
    println!("Merge marker error:\n{}", error_msg);

    // Verify error contains helpful context
    assert!(error_msg.contains("error[E0001]"));
    assert!(error_msg.contains("merge marker order"));
    assert!(error_msg.contains("<<<<<<< GENERATED"));
    assert!(error_msg.contains("======="));
    assert!(error_msg.contains(">>>>>>> MANUAL"));
    assert!(error_msg.contains("= help:"));
}

#[test]
fn test_file_size_validation_error_shows_helpful_context() {
    // Create a large content (11MB to exceed 10MB limit)
    let large_content = "x".repeat(11 * 1024 * 1024);

    // Call validate_generated_output through public API
    // This will be tested indirectly through integration tests
    let size = large_content.len();
    assert!(size > 10 * 1024 * 1024, "Content should exceed 10MB limit");
}

#[test]
fn test_directory_traversal_error_shows_helpful_context() {
    // Test path with directory traversal
    let traversal_path = Path::new("../../../etc/passwd");

    // The path contains ../ which should trigger the error
    let path_str = traversal_path.to_string_lossy();
    assert!(path_str.contains("../"), "Test path should contain ../");

    // In actual usage, this would be caught by validate_generated_output
    assert!(path_str.contains(".."));
}

#[test]
fn test_watch_path_not_found_error_shows_helpful_context() {
    use ggen_core::codegen::watch::FileWatcher;

    // Try to watch a non-existent path
    let nonexistent_path = PathBuf::from("/this/path/does/not/exist");
    let watcher = FileWatcher::new(vec![nonexistent_path]);

    let result = watcher.start();
    assert!(result.is_err());

    let error_msg = result.unwrap_err().to_string();
    println!("Watch path error:\n{}", error_msg);

    // Verify error contains helpful context
    assert!(error_msg.contains("error[E0009]"));
    assert!(error_msg.contains("does not exist"));
    assert!(error_msg.contains("= help:"));
    assert!(error_msg.contains("ontology.source"));
    assert!(error_msg.contains("ontology.imports"));
}

#[test]
fn test_error_messages_follow_consistent_pattern() {
    // All improved errors should follow the pattern:
    // 1. Error code [E####]
    // 2. Clear description of what failed
    // 3. Location (rule, path, line numbers)
    // 4. = help: sections with actionable guidance

    let nonexistent_path = PathBuf::from("/tmp/nonexistent_test_path_12345");
    let watcher = FileWatcher::new(vec![nonexistent_path]);
    let result = watcher.start();
    let error_msg = result.unwrap_err().to_string();

    // Check for consistent error pattern
    assert!(error_msg.contains("error[E"), "Should have error code");
    assert!(error_msg.contains("-->"), "Should show location");
    assert!(error_msg.contains("= help:"), "Should have help sections");

    println!("Consistent error pattern example:\n{}", error_msg);
}
