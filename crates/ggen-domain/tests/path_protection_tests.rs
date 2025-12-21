//! Integration tests for path protection functionality
//!
//! Tests the `[generation].protected_paths` and `[generation].regenerate_paths`
//! configuration from ggen.toml to prevent accidental overwrites of domain logic.

use ggen_domain::generation::protection::{GenerationWriteResult, PathProtectionValidator};
use std::fs;
use tempfile::TempDir;

/// Test: Path protection blocks writes to protected paths
#[test]
fn test_protected_path_blocks_write() {
    let validator = PathProtectionValidator::default_protection();

    // Typical protected path: src/domain/**/*
    let result = validator.validate_write("src/domain/user.rs", true);

    match result {
        GenerationWriteResult::BlockedProtected { .. } => {
            // Expected: protected paths are blocked
        }
        _ => panic!("Expected BlockedProtected, got {:?}", result),
    }
}

/// Test: Regenerate paths allow writes
#[test]
fn test_regenerate_path_allows_write() {
    let validator = PathProtectionValidator::default_protection();

    // Typical regenerate path: src/generated/**/*
    let result = validator.validate_write("src/generated/user/mod.rs", true);

    match result {
        GenerationWriteResult::AllowRegenerate { .. } => {
            // Expected: regenerate paths are allowed
        }
        _ => panic!("Expected AllowRegenerate, got {:?}", result),
    }
}

/// Test: Implicit protection for existing files not in regenerate_paths
#[test]
fn test_implicit_protection_existing_file() {
    let validator = PathProtectionValidator::default_protection();

    // File exists but is not in protected or regenerate paths
    let result = validator.validate_write("src/lib.rs", true);

    match result {
        GenerationWriteResult::BlockedImplicit { .. } => {
            // Expected: existing files not in regenerate paths are implicitly protected
        }
        _ => panic!("Expected BlockedImplicit, got {:?}", result),
    }
}

/// Test: New files not in any path list are allowed
#[test]
fn test_new_file_not_in_paths_allowed() {
    let validator = PathProtectionValidator::default_protection();

    // New file (doesn't exist) not in any path list
    let result = validator.validate_write("src/new_file.rs", false);

    match result {
        GenerationWriteResult::AllowWrite => {
            // Expected: new files not in any list are allowed
        }
        _ => panic!("Expected AllowWrite, got {:?}", result),
    }
}

/// Test: is_protected() convenience method
#[test]
fn test_is_protected_method() {
    let validator = PathProtectionValidator::default_protection();

    assert!(validator.is_protected("src/domain/user.rs"));
    assert!(!validator.is_protected("src/generated/user.rs"));
}

/// Test: is_regeneratable() convenience method
#[test]
fn test_is_regeneratable_method() {
    let validator = PathProtectionValidator::default_protection();

    assert!(validator.is_regeneratable("src/generated/user.rs"));
    assert!(!validator.is_regeneratable("src/domain/user.rs"));
}

/// Test: Protection works with real filesystem
#[test]
fn test_protection_with_real_filesystem() -> std::io::Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Create protected and regenerate directories
    fs::create_dir_all(base_path.join("src/domain"))?;
    fs::create_dir_all(base_path.join("src/generated/user"))?;

    // Create a protected file
    let protected_file = base_path.join("src/domain/user.rs");
    fs::write(&protected_file, "// Domain implementation")?;

    // Create a regenerate file
    let regen_file = base_path.join("src/generated/user/mod.rs");
    fs::write(&regen_file, "// Generated trait")?;

    let validator = PathProtectionValidator::default_protection();

    // Protected file exists and should be blocked
    assert!(matches!(
        validator.validate_write("src/domain/user.rs", true),
        GenerationWriteResult::BlockedProtected { .. }
    ));

    // Regenerate file should be allowed
    assert!(matches!(
        validator.validate_write("src/generated/user/mod.rs", true),
        GenerationWriteResult::AllowRegenerate { .. }
    ));

    Ok(())
}

/// Test: Protection with glob patterns
#[test]
fn test_protection_with_glob_patterns() {
    let validator = PathProtectionValidator::default_protection();

    // Test various paths
    assert!(validator.is_protected("src/domain/users/mod.rs"));
    assert!(validator.is_protected("src/domain/products/impl.rs"));
    assert!(validator.is_regeneratable("src/generated/users/traits.rs"));
    assert!(validator.is_regeneratable("src/generated/products/interfaces.rs"));
}

/// Test: Pattern matching for nested paths
#[test]
fn test_pattern_matching_nested_paths() {
    let validator = PathProtectionValidator::default_protection();

    // Test deeply nested paths
    assert!(validator.is_protected("src/domain/products/store/impl.rs"));
    assert!(validator.is_regeneratable("src/generated/products/store/traits.rs"));
}

/// Test: Protection error messages are informative
#[test]
fn test_protection_error_messages() {
    let validator = PathProtectionValidator::default_protection();

    let result = validator.validate_write("src/domain/user.rs", true);

    match result {
        GenerationWriteResult::BlockedProtected { path, pattern } => {
            assert_eq!(path, "src/domain/user.rs");
            assert!(!pattern.is_empty(), "Pattern should be provided in error");
        }
        _ => panic!("Expected BlockedProtected with message"),
    }
}
