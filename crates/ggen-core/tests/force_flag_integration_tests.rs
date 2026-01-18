//! Integration tests for --force flag and path protection using Chicago School TDD (AAA pattern)
//!
//! Tests verify observable state (path protection logic) using real TempDir and files.
//! No mocks - tests verify actual poka-yoke protection behavior.

use ggen_core::codegen::{GenerationPipeline, SyncOptions};
use ggen_core::types::path_protection::PathProtectionConfig;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test that protected files are blocked by PathProtectionConfig
#[test]
fn test_protected_paths_blocked_by_validation() {
    // Arrange: Create protection config with protected paths
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    // Act: Try to validate write to protected path
    let result_protected = protection.validate_write("src/domain/user.rs", true);

    // Assert: Protected path should be blocked
    assert!(
        result_protected.is_err(),
        "Writing to protected path src/domain/user.rs should fail"
    );

    // Verify error message contains path
    let error_msg = format!("{}", result_protected.unwrap_err());
    assert!(
        error_msg.contains("src/domain/user.rs"),
        "Error message should contain protected path"
    );
}

/// Test that regeneratable paths allow writes
#[test]
fn test_regeneratable_paths_allow_writes() {
    // Arrange: Create protection config with regenerate paths
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let regenerate_file = temp_dir.path().join("src/generated/user_generated.rs");

    fs::create_dir_all(regenerate_file.parent().unwrap()).expect("Failed to create generated dir");

    // Write existing regeneratable file
    fs::write(&regenerate_file, "// OLD: Original generated content\n")
        .expect("Failed to write regeneratable file");

    // Act: Validate write to regeneratable path (should succeed WITHOUT force)
    let result_without_force = protection.validate_write("src/generated/user_generated.rs", true);

    // Assert: Regeneratable paths should allow overwrite without force
    assert!(
        result_without_force.is_ok(),
        "Writing to regeneratable path should succeed without force"
    );

    // Verify file can be overwritten
    fs::write(&regenerate_file, "// NEW: Regenerated content\n")
        .expect("Failed to overwrite regeneratable file");

    let new_content = fs::read_to_string(&regenerate_file).expect("Failed to read after regen");

    assert_eq!(
        new_content, "// NEW: Regenerated content\n",
        "Regeneratable file should be overwritten freely"
    );
}

/// Test that GenerationPipeline force_overwrite API exists
#[test]
fn test_pipeline_force_overwrite_api_exists() {
    // This test verifies that the GenerationPipeline struct has the set_force_overwrite method
    // by checking it compiles. The actual behavior is tested through path protection.

    // Note: We can't easily construct a GenerationPipeline without a valid manifest,
    // so this test just verifies the API exists at compile time.
    // The runtime behavior is tested through PathProtectionConfig tests.

    // If this test compiles, it proves the API exists
    let _type_check: fn(&mut GenerationPipeline, bool) = GenerationPipeline::set_force_overwrite;

    // Success: API exists
}

/// Test SyncOptions has force flag
#[test]
fn test_sync_options_force_flag() {
    // Arrange: Create SyncOptions with force enabled
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,
        audit: false,
        dry_run: false,
        verbose: false,
        ..Default::default()
    };

    // Act & Assert: Verify force flag is set correctly
    assert!(options.force, "force flag should be enabled");

    // Test with both flags enabled
    let options_both = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,
        audit: true,
        dry_run: false,
        verbose: false,
        ..Default::default()
    };

    assert!(
        options_both.force && options_both.audit,
        "Both flags should be usable together"
    );
}

/// Test path protection glob patterns
#[test]
fn test_path_protection_glob_patterns() {
    // Arrange: Create protection config with various glob patterns
    let protection = PathProtectionConfig::new(
        &["src/domain/**/*.rs", "config/*.toml"],
        &["src/generated/**", "target/**"],
    )
    .expect("Failed to create protection config");

    // Act & Assert: Test protected path matching
    assert!(
        protection.is_protected("src/domain/user/model.rs"),
        "domain/*.rs should be protected"
    );
    assert!(
        protection.is_protected("src/domain/product.rs"),
        "domain/*.rs should be protected"
    );
    assert!(
        protection.is_protected("config/app.toml"),
        "config/*.toml should be protected"
    );

    // Test regeneratable path matching
    assert!(
        protection.is_regeneratable("src/generated/user.rs"),
        "generated/** should be regeneratable"
    );
    assert!(
        protection.is_regeneratable("src/generated/nested/product.rs"),
        "generated/** should match nested paths"
    );
    assert!(
        protection.is_regeneratable("target/debug/build.rs"),
        "target/** should be regeneratable"
    );

    // Test non-matching paths
    assert!(
        !protection.is_protected("src/main.rs"),
        "main.rs should not be protected"
    );
    assert!(
        !protection.is_regeneratable("src/lib.rs"),
        "lib.rs should not be regeneratable"
    );
}

/// Test implicit protection of existing files
#[test]
fn test_implicit_protection_of_existing_files() {
    // Arrange: Create protection config
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let existing_file = temp_dir.path().join("src/utils.rs");

    fs::create_dir_all(existing_file.parent().unwrap()).expect("Failed to create src dir");

    fs::write(&existing_file, "// Existing utility file\n").expect("Failed to write existing file");

    // Act: Validate write to existing file not in any pattern list
    let result = protection.validate_write("src/utils.rs", true);

    // Assert: Existing files should be implicitly protected
    assert!(
        result.is_err(),
        "Existing files not in regenerate_paths should be implicitly protected"
    );

    // But new files (file_exists=false) should be allowed
    let result_new_file = protection.validate_write("src/new_utils.rs", false);

    assert!(
        result_new_file.is_ok(),
        "New files not in protected_paths should be allowed"
    );
}

/// Test that protection validation provides helpful error messages
#[test]
fn test_protection_error_messages() {
    // Arrange: Create protection config
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    // Act: Try to write to protected path
    let result = protection.validate_write("src/domain/user.rs", true);

    // Assert: Error message should be informative
    assert!(result.is_err(), "Should fail for protected path");

    let error = result.unwrap_err();
    let error_msg = format!("{}", error);

    assert!(
        error_msg.contains("src/domain/user.rs"),
        "Error should mention the path"
    );

    // Test implicit protection error
    let result_implicit = protection.validate_write("src/random.rs", true);
    assert!(
        result_implicit.is_err(),
        "Should fail for implicitly protected file"
    );
}

/// Test pattern matching edge cases
#[test]
fn test_pattern_matching_edge_cases() {
    // Arrange: Create protection config with specific patterns
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    // Act & Assert: Test various path formats
    assert!(
        protection.is_protected("src/domain/user.rs"),
        "Flat file in domain should be protected"
    );

    assert!(
        protection.is_protected("src/domain/models/user.rs"),
        "Nested file in domain should be protected"
    );

    assert!(
        protection.is_protected("src/domain/models/entities/user.rs"),
        "Deeply nested file should be protected"
    );

    // Test that partial matches don't work
    assert!(
        !protection.is_protected("src/notdomain/user.rs"),
        "Similar but different path should not match"
    );

    assert!(
        !protection.is_regeneratable("src/notgenerated/file.rs"),
        "Similar but different path should not match"
    );
}
