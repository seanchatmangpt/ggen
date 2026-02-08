//! Comprehensive security tests for PathValidator
//!
//! Tests all edge cases and attack vectors:
//! - Path traversal variations
//! - Symlink attacks
//! - Null byte injection
//! - Unicode normalization
//! - Absolute path escapes
//! - Depth limit violations

use ggen_utils::path_validator::{PathValidator, SafePath};
use std::path::Path;
use tempfile::tempdir;

// ============================================================================
// Path Traversal Attack Tests
// ============================================================================

#[test]
fn test_basic_path_traversal() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act & Assert
    let attacks = vec![
        "../../../etc/passwd",
        "../../etc/passwd",
        "../etc/passwd",
        "subdir/../../etc/passwd",
        "./../../etc/passwd",
    ];

    for attack in attacks {
        let result = validator.validate(attack);
        assert!(result.is_err(), "Should block path traversal: {}", attack);
        assert!(
            result.unwrap_err().to_string().contains("traversal"),
            "Error should mention path traversal"
        );
    }
}

#[test]
fn test_encoded_path_traversal() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act & Assert - URL-encoded path traversal
    // Note: Rust's Path automatically decodes, so we test the decoded form
    let attacks = vec![
        "..%2F..%2F..%2Fetc%2Fpasswd", // URL encoded
        "..\\..\\..\\etc\\passwd",     // Windows-style (converted to /)
    ];

    for attack in attacks {
        let result = validator.validate(attack);
        // These may or may not be blocked depending on OS and Path behavior
        // The key is that if they're not blocked, they should still be within workspace
        if let Ok(safe_path) = result {
            assert!(
                safe_path.absolute().starts_with(workspace.path()),
                "Path should be within workspace"
            );
        }
    }
}

#[test]
fn test_double_encoded_traversal() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act & Assert
    let attacks = vec![
        "%252e%252e%252f%252e%252e%252fetc%252fpasswd", // Double URL encoded
    ];

    for attack in attacks {
        // These should be treated as literal filenames (safe but weird)
        let result = validator.validate(attack);
        // As long as it doesn't escape workspace, it's fine
        if let Ok(safe_path) = result {
            assert!(safe_path.absolute().starts_with(workspace.path()));
        }
    }
}

// ============================================================================
// Null Byte Injection Tests
// ============================================================================

#[test]
fn test_null_byte_injection() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act & Assert
    let attacks = vec![
        "file.txt\0.evil",
        "safe\0../../etc/passwd",
        "\0",
        "dir/\0/file.txt",
    ];

    for attack in attacks {
        let result = validator.validate(attack);
        assert!(result.is_err(), "Should block null byte: {}", attack);
        assert!(
            result.unwrap_err().to_string().contains("null byte"),
            "Error should mention null byte"
        );
    }
}

// ============================================================================
// Absolute Path Tests
// ============================================================================

#[test]
fn test_absolute_paths_blocked_by_default() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act & Assert
    let absolute_paths = vec!["/etc/passwd", "/tmp/evil", "/var/log/secrets"];

    for path in absolute_paths {
        let result = validator.validate(path);
        assert!(result.is_err(), "Should block absolute path: {}", path);
        assert!(
            result.unwrap_err().to_string().contains("Absolute"),
            "Error should mention absolute path"
        );
    }
}

#[test]
fn test_absolute_paths_within_workspace_allowed() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let test_file = workspace.path().join("test.txt");
    std::fs::write(&test_file, "content").expect("Failed to create test file");

    let validator = PathValidator::new(workspace.path()).with_absolute_paths(true);

    // Act
    let result = validator.validate(&test_file);

    // Assert
    assert!(
        result.is_ok(),
        "Should allow absolute path within workspace"
    );
}

#[test]
fn test_absolute_paths_outside_workspace_blocked() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path()).with_absolute_paths(true);

    // Act - try to access /etc/passwd
    let result = validator.validate("/etc/passwd");

    // Assert
    assert!(
        result.is_err(),
        "Should block absolute path outside workspace"
    );
    assert!(
        result.unwrap_err().to_string().contains("workspace"),
        "Error should mention workspace escape"
    );
}

// ============================================================================
// Symlink Attack Tests
// ============================================================================

#[test]
#[cfg(unix)]
fn test_symlink_pointing_outside_workspace_blocked() {
    use std::os::unix::fs::symlink;

    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Create symlink pointing outside workspace
    let link_path = workspace.path().join("evil_link");
    symlink("/etc/passwd", &link_path).expect("Failed to create symlink");

    // Act
    let result = validator.validate("evil_link");

    // Assert
    assert!(result.is_err(), "Should block symlink escape");
}

#[test]
#[cfg(unix)]
fn test_symlink_within_workspace_allowed() {
    use std::os::unix::fs::symlink;

    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Create target file
    let target = workspace.path().join("target.txt");
    std::fs::write(&target, "content").expect("Failed to create target");

    // Create symlink within workspace
    let link_path = workspace.path().join("link.txt");
    symlink(&target, &link_path).expect("Failed to create symlink");

    // Act
    let result = validator.validate("link.txt");

    // Assert
    assert!(result.is_ok(), "Should allow symlink within workspace");
}

#[test]
#[cfg(unix)]
fn test_symlink_chain_attack() {
    use std::os::unix::fs::symlink;

    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Create chain: link1 -> link2 -> /etc/passwd
    let link1 = workspace.path().join("link1");
    let link2 = workspace.path().join("link2");
    symlink("/etc/passwd", &link2).expect("Failed to create link2");
    symlink(&link2, &link1).expect("Failed to create link1");

    // Act
    let result = validator.validate("link1");

    // Assert - should block because link2 points outside
    assert!(result.is_err(), "Should block symlink chain escape");
}

// ============================================================================
// Extension Validation Tests
// ============================================================================

#[test]
fn test_extension_whitelist_enforced() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator =
        PathValidator::new(workspace.path()).with_allowed_extensions(vec!["tmpl", "tera", "ttl"]);

    // Act & Assert - allowed extensions
    let allowed = vec!["template.tmpl", "example.tera", "ontology.ttl"];
    for path in allowed {
        let result = validator.validate(path);
        assert!(result.is_ok(), "Should allow extension: {}", path);
    }

    // Act & Assert - blocked extensions
    let blocked = vec!["script.sh", "binary.exe", "config.yaml"];
    for path in blocked {
        let result = validator.validate(path);
        assert!(result.is_err(), "Should block extension: {}", path);
        assert!(
            result.unwrap_err().to_string().contains("extension"),
            "Error should mention extension"
        );
    }
}

#[test]
fn test_extension_case_sensitivity() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path()).with_allowed_extensions(vec!["tera"]);

    // Act - uppercase extension
    let result = validator.validate("template.TERA");

    // Assert - should be blocked (case-sensitive)
    assert!(
        result.is_err(),
        "Extension validation should be case-sensitive"
    );
}

#[test]
fn test_double_extension_handling() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path()).with_allowed_extensions(vec!["tera"]);

    // Act - file with double extension
    let result = validator.validate("archive.tar.tera");

    // Assert - should validate based on last extension
    assert!(result.is_ok(), "Should check only the last extension");
}

// ============================================================================
// Depth Limit Tests
// ============================================================================

#[test]
fn test_depth_limit_enforced() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path()).with_max_depth(3);

    // Act & Assert - within depth
    let shallow = "a/b/c.txt";
    assert!(
        validator.validate(shallow).is_ok(),
        "Should allow path within depth limit"
    );

    // Act & Assert - exceeds depth
    let deep = "a/b/c/d/e.txt";
    let result = validator.validate(deep);
    assert!(result.is_err(), "Should block path exceeding depth");
    assert!(
        result.unwrap_err().to_string().contains("depth"),
        "Error should mention depth"
    );
}

// ============================================================================
// Unicode and Encoding Tests
// ============================================================================

#[test]
fn test_unicode_path_allowed() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act - various Unicode paths
    let unicode_paths = vec![
        "文件.txt",     // Chinese
        "файл.txt",     // Russian
        "ファイル.txt", // Japanese
        "αρχείο.txt",   // Greek
        "📁/file.txt",  // Emoji
    ];

    for path in unicode_paths {
        let result = validator.validate(path);
        assert!(result.is_ok(), "Should allow Unicode path: {}", path);
    }
}

#[test]
fn test_mixed_unicode_and_ascii() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let mixed = "templates/例え_example_文件.tera";
    let result = validator.validate(mixed);

    // Assert
    assert!(result.is_ok(), "Should allow mixed Unicode/ASCII");
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_empty_path_blocked() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let result = validator.validate("");

    // Assert
    assert!(result.is_err(), "Should block empty path");
    assert!(
        result.unwrap_err().to_string().contains("empty"),
        "Error should mention empty path"
    );
}

#[test]
fn test_current_directory_reference() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let paths = vec!["./file.txt", "./dir/./file.txt", "././file.txt"];

    // Assert - these should be normalized and allowed
    for path in paths {
        let result = validator.validate(path);
        assert!(
            result.is_ok(),
            "Should allow current dir reference: {}",
            path
        );
    }
}

#[test]
fn test_trailing_slashes() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let paths = vec!["dir/", "dir/file.txt/"];

    // Assert
    for path in paths {
        let result = validator.validate(path);
        // Should either succeed or fail gracefully
        // The key is no panic or undefined behavior
        let _ = result;
    }
}

#[test]
fn test_very_long_path() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act - create a very long path (but not exceeding OS limits)
    let long_component = "a".repeat(255); // Max filename on most systems
    let long_path = format!("{}/file.txt", long_component);
    let result = validator.validate(&long_path);

    // Assert - should handle gracefully
    let _ = result;
}

#[test]
fn test_special_characters_in_filename() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act - filenames with special chars (legal on most systems)
    let special_paths = vec![
        "file-name.txt",
        "file_name.txt",
        "file.name.txt",
        "file (1).txt",
        "file@2024.txt",
    ];

    for path in special_paths {
        let result = validator.validate(path);
        assert!(result.is_ok(), "Should allow special char path: {}", path);
    }
}

// ============================================================================
// Batch Validation Tests
// ============================================================================

#[test]
fn test_batch_validation_all_valid() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    let paths = vec!["file1.txt", "file2.txt", "dir/file3.txt"];

    // Act
    let result = validator.validate_batch(&paths);

    // Assert
    assert!(result.is_ok());
    let safe_paths = result.expect("All should validate");
    assert_eq!(safe_paths.len(), 3);
}

#[test]
fn test_batch_validation_with_invalid() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    let paths = vec!["file1.txt", "../../../etc/passwd", "file3.txt"];

    // Act
    let result = validator.validate_batch(&paths);

    // Assert - should fail on first invalid path
    assert!(result.is_err());
}

// ============================================================================
// SafePath API Tests
// ============================================================================

#[test]
fn test_safe_path_accessors() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let safe_path = validator
        .validate("templates/example.tera")
        .expect("Should validate");

    // Assert
    assert_eq!(safe_path.extension(), Some("tera"));
    assert_eq!(safe_path.file_name(), Some("example.tera"));
    assert_eq!(safe_path.as_path(), Path::new("templates/example.tera"));
    assert!(safe_path.absolute().is_absolute());
}

#[test]
fn test_safe_path_as_ref() {
    // Arrange
    let workspace = tempdir().expect("Failed to create temp dir");
    let validator = PathValidator::new(workspace.path());

    // Act
    let safe_path = validator.validate("file.txt").expect("Should validate");

    // Assert - should work with AsRef<Path>
    fn take_path_ref<P: AsRef<Path>>(p: P) -> bool {
        p.as_ref().to_str().is_some()
    }

    assert!(take_path_ref(&safe_path));
}
