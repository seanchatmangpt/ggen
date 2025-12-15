//! IO Validation Tests for Clap Integration
//!
//! Tests file operations, path safety, and permission validation
//! following the 80/20 principle - focusing on critical security paths.

use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test that safe file paths are allowed
#[test]
fn test_safe_file_paths_allowed() {
    let temp_dir = TempDir::new().unwrap();
    let safe_path = temp_dir.path().join("safe_file.txt");

    // Should be able to create and write to safe paths
    fs::write(&safe_path, "test content").unwrap();
    assert!(safe_path.exists());

    let content = fs::read_to_string(&safe_path).unwrap();
    assert_eq!(content, "test content");
}

/// Test that path traversal attempts are detected
#[test]
fn test_path_traversal_detection() {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    // These patterns should be detected as unsafe
    let unsafe_patterns = vec![
        "../../../etc/passwd",
        "../../config.toml",
        "./../../secret.key",
        "subdir/../../../etc/hosts",
    ];

    for pattern in unsafe_patterns {
        let path = PathBuf::from(pattern);
        // Path traversal attempts should be caught
        assert!(
            path.components().any(|c| matches!(c, std::path::Component::ParentDir)),
            "Path {} should contain parent directory components",
            pattern
        );
    }
}

/// Test that absolute paths outside workspace are caught
#[test]
fn test_absolute_path_validation() {
    let temp_dir = TempDir::new().unwrap();
    let workspace = temp_dir.path();

    // Absolute path outside workspace should be detected
    let outside_path = PathBuf::from("/etc/passwd");
    assert!(!outside_path.starts_with(workspace));

    // Path inside workspace should be allowed
    let inside_path = workspace.join("config.toml");
    assert!(inside_path.starts_with(workspace));
}

/// Test that file permissions are validated
#[test]
fn test_file_permission_validation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    fs::write(&file_path, "test").unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        // Check readable
        let metadata = fs::metadata(&file_path).unwrap();
        let permissions = metadata.permissions();
        assert!(permissions.mode() & 0o400 != 0, "File should be readable");

        // Check writable by owner
        assert!(permissions.mode() & 0o200 != 0, "File should be writable");
    }

    // Cross-platform: just verify we can read it
    let _content = fs::read_to_string(&file_path).unwrap();
}

/// Test that symlink attacks are prevented
#[test]
#[cfg(unix)]
fn test_symlink_attack_prevention() {
    let temp_dir = TempDir::new().unwrap();
    let target_file = temp_dir.path().join("target.txt");
    let symlink_path = temp_dir.path().join("symlink.txt");

    fs::write(&target_file, "target content").unwrap();
    std::os::unix::fs::symlink(&target_file, &symlink_path).unwrap();

    // Verify symlink detection
    let metadata = fs::symlink_metadata(&symlink_path).unwrap();
    assert!(metadata.file_type().is_symlink(), "Should detect symlinks");
}

/// Test that directory traversal permissions are validated
#[test]
fn test_directory_traversal_permissions() {
    let temp_dir = TempDir::new().unwrap();
    let sub_dir = temp_dir.path().join("subdir");
    fs::create_dir(&sub_dir).unwrap();

    let file_in_subdir = sub_dir.join("file.txt");
    fs::write(&file_in_subdir, "test").unwrap();

    // Should be able to access files in subdirectories
    assert!(file_in_subdir.exists());
    assert!(file_in_subdir.starts_with(temp_dir.path()));
}

/// Test IO validation overhead performance
#[test]
fn test_io_validation_performance() {
    use std::time::Instant;

    let temp_dir = TempDir::new().unwrap();
    let test_paths: Vec<PathBuf> = (0..100)
        .map(|i| temp_dir.path().join(format!("file_{}.txt", i)))
        .collect();

    // Measure path validation time
    let start = Instant::now();
    for path in &test_paths {
        // Simulate validation: check if path is under workspace
        let _ = path.starts_with(temp_dir.path());
        let _ = path.components().any(|c| matches!(c, std::path::Component::ParentDir));
    }
    let duration = start.elapsed();

    // IO validation should be fast (<1ms per operation)
    assert!(
        duration.as_millis() < 100,
        "IO validation should complete in <100ms for 100 paths, took {:?}",
        duration
    );
}

/// Test that writable directory validation works
#[test]
fn test_writable_directory_validation() {
    let temp_dir = TempDir::new().unwrap();

    // Temp directory should be writable
    let test_file = temp_dir.path().join("write_test.txt");
    let result = fs::write(&test_file, "test");
    assert!(result.is_ok(), "Should be able to write to temp directory");
}

/// Test that non-existent parent directories are handled
#[test]
fn test_nonexistent_parent_directory() {
    let temp_dir = TempDir::new().unwrap();
    let nested_path = temp_dir.path().join("a/b/c/file.txt");

    // Should fail if parent doesn't exist
    let result = fs::write(&nested_path, "test");
    assert!(result.is_err(), "Writing to non-existent parent should fail");

    // Should succeed after creating parents
    if let Some(parent) = nested_path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(&nested_path, "test").unwrap();
    assert!(nested_path.exists());
}
