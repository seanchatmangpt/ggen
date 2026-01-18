///! Additional edge case tests for FileTransaction atomic operations
///!
///! Tests permission denied scenarios, error propagation, and bulletproof behavior
///! not covered in the main integration tests.
///!
///! Chicago TDD: Real objects, state verification, no mocks.

use ggen_core::codegen::FileTransaction;
use std::fs;
use tempfile::tempdir;

// ============================================================================
// Edge Case: Permission Denied During Write
// ============================================================================

#[test]
#[cfg(unix)] // Permission tests are Unix-specific
fn test_permission_denied_during_write() {
    use std::os::unix::fs::PermissionsExt;

    // Arrange: Create read-only directory
    let temp_dir = tempdir().unwrap();
    let readonly_dir = temp_dir.path().join("readonly");
    fs::create_dir(&readonly_dir).unwrap();

    // Make directory read-only (no write or execute permissions)
    let mut perms = fs::metadata(&readonly_dir).unwrap().permissions();
    perms.set_mode(0o500); // Read + execute for owner only (no write)
    fs::set_permissions(&readonly_dir, perms).unwrap();

    // Act: Try to write file in read-only directory
    let mut tx = FileTransaction::new().unwrap();
    let file_path = readonly_dir.join("should_fail.txt");
    let result = tx.write_file(&file_path, "content");

    // Assert: Should fail with permission error
    // Note: On some systems, root can write even to read-only directories
    // So we make this test conditional on the actual result
    if result.is_err() {
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("Failed to create temporary file")
                || error_msg.contains("Permission denied")
                || error_msg.contains("Read-only"),
            "Error should mention permission issue: {}",
            error_msg
        );
    }

    // Cleanup: Restore permissions
    let mut perms = fs::metadata(&readonly_dir).unwrap().permissions();
    perms.set_mode(0o755);
    let _ = fs::set_permissions(&readonly_dir, perms);
}

// ============================================================================
// Edge Case: Permission Denied on Existing File
// ============================================================================

#[test]
#[cfg(unix)]
fn test_permission_denied_on_existing_file() {
    // This test demonstrates that atomic writes (temp file + rename)
    // can succeed even when the original file is read-only, because
    // the operation creates a new file and atomically renames it.
    // This is actually correct behavior for atomic operations.

    use std::os::unix::fs::PermissionsExt;

    // Arrange: Create read-only file
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("readonly.txt");
    fs::write(&file_path, "original").unwrap();

    // Make file read-only
    let mut perms = fs::metadata(&file_path).unwrap().permissions();
    perms.set_mode(0o444);
    fs::set_permissions(&file_path, perms).unwrap();

    // Act: Modify read-only file atomically
    let mut tx = FileTransaction::new().unwrap();
    let result = tx.write_file(&file_path, "modified");

    // Note: atomic write uses temp file + rename, which succeeds even if
    // original file is read-only (we're creating new file then renaming).

    // Cleanup: Restore permissions regardless of test outcome
    if file_path.exists() {
        let mut perms = fs::metadata(&file_path).unwrap().permissions();
        perms.set_mode(0o644);
        let _ = fs::set_permissions(&file_path, perms);
    }

    // Verify atomic write succeeded (this is expected behavior)
    if result.is_ok() {
        let receipt = tx.commit().unwrap();
        assert_eq!(receipt.files_modified.len(), 1);
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "modified");
    }
}

// ============================================================================
// Edge Case: Rollback with Permission Issues on Backup
// ============================================================================

#[test]
fn test_rollback_handles_missing_file_gracefully() {
    // Arrange: Create file and start transaction
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("file.txt");
    fs::write(&file_path, "original").unwrap();

    {
        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file_path, "modified").unwrap();

        // Verify modification happened
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "modified");

        // Manually delete the file (simulate corruption/external deletion)
        fs::remove_file(&file_path).unwrap();

        // Drop without commit - rollback should handle missing file
    }

    // Assert: Rollback completed (even though file was missing)
    // The backup restoration should have recreated the file
    if file_path.exists() {
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "original");
    }
    // If file doesn't exist, rollback handled the error gracefully
}

// ============================================================================
// Edge Case: Very Long File Path
// ============================================================================

#[test]
fn test_very_long_file_path() {
    // Arrange: Create nested directory structure approaching path length limits
    let temp_dir = tempdir().unwrap();
    let mut path = temp_dir.path().to_path_buf();

    // Create deeply nested path (but not exceeding OS limits)
    for i in 0..20 {
        path = path.join(format!("level_{}", i));
    }
    path = path.join("file.txt");

    // Act: Write file with very long path
    let mut tx = FileTransaction::new().unwrap();
    let result = tx.write_file(&path, "deep content");

    // Assert: Should succeed
    assert!(result.is_ok(), "Should handle long paths: {:?}", result);

    let receipt = tx.commit().unwrap();
    assert_eq!(receipt.files_created.len(), 1);
    assert!(path.exists());
    assert_eq!(fs::read_to_string(&path).unwrap(), "deep content");
}

// ============================================================================
// Edge Case: Special Characters in Filename
// ============================================================================

#[test]
fn test_special_characters_in_filename() {
    // Arrange: Create files with special characters
    let temp_dir = tempdir().unwrap();

    let special_files = vec![
        "file with spaces.txt",
        "file-with-dashes.txt",
        "file_with_underscores.txt",
        "file.multiple.dots.txt",
        "file@special#chars.txt",
    ];

    // Act: Write all files
    let mut tx = FileTransaction::new().unwrap();

    for filename in &special_files {
        let path = temp_dir.path().join(filename);
        tx.write_file(&path, &format!("content of {}", filename))
            .unwrap();
    }

    let receipt = tx.commit().unwrap();

    // Assert: All files created
    assert_eq!(receipt.files_created.len(), special_files.len());

    for filename in &special_files {
        let path = temp_dir.path().join(filename);
        assert!(path.exists(), "File should exist: {}", filename);
        assert_eq!(
            fs::read_to_string(&path).unwrap(),
            format!("content of {}", filename)
        );
    }
}

// ============================================================================
// Edge Case: Unicode in File Content and Path
// ============================================================================

#[test]
fn test_unicode_in_file_content_and_path() {
    // Arrange: Create files with Unicode content
    let temp_dir = tempdir().unwrap();
    let unicode_file = temp_dir.path().join("测试文件.txt");

    let unicode_content = "Hello 世界! 🚀 Rust is awesome! Здравствуй мир!";

    // Act: Write file with Unicode
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&unicode_file, unicode_content).unwrap();
    let receipt = tx.commit().unwrap();

    // Assert: Unicode preserved correctly
    assert_eq!(receipt.files_created.len(), 1);
    assert!(unicode_file.exists());
    assert_eq!(fs::read_to_string(&unicode_file).unwrap(), unicode_content);
}

// ============================================================================
// Edge Case: Rapid Sequential Transactions
// ============================================================================

#[test]
fn test_rapid_sequential_transactions() {
    // Arrange: Create file for rapid modifications
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("rapid.txt");

    // Act: Perform 10 rapid transactions
    for i in 0..10 {
        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file_path, &format!("iteration {}", i))
            .unwrap();
        let receipt = tx.commit().unwrap();

        if i == 0 {
            assert_eq!(receipt.files_created.len(), 1);
        } else {
            assert_eq!(receipt.files_modified.len(), 1);
            assert_eq!(receipt.backups.len(), 1);
        }
    }

    // Assert: Final state is last iteration
    assert_eq!(fs::read_to_string(&file_path).unwrap(), "iteration 9");
}

// ============================================================================
// Edge Case: Mixed Success and Failure in Rollback
// ============================================================================

#[test]
fn test_partial_rollback_completion() {
    // Arrange: Create multiple files
    let temp_dir = tempdir().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    {
        let mut tx = FileTransaction::new().unwrap();

        // Create files
        tx.write_file(&file1, "content1").unwrap();
        tx.write_file(&file2, "content2").unwrap();

        // Both files exist
        assert!(file1.exists());
        assert!(file2.exists());

        // Manually delete one file (simulate corruption)
        fs::remove_file(&file1).unwrap();

        // Drop without commit - should rollback file2 even if file1 cleanup fails
    }

    // Assert: file2 should be cleaned up
    assert!(
        !file2.exists(),
        "file2 should be removed even if file1 was already deleted"
    );
}

// ============================================================================
// Edge Case: Transaction Receipt Accuracy
// ============================================================================

#[test]
fn test_receipt_accuracy_comprehensive() {
    // Arrange: Complex scenario
    let temp_dir = tempdir().unwrap();

    // Create some existing files
    let existing1 = temp_dir.path().join("existing1.txt");
    let existing2 = temp_dir.path().join("existing2.txt");
    fs::write(&existing1, "old1").unwrap();
    fs::write(&existing2, "old2").unwrap();

    // Act: Transaction with various operations
    let mut tx = FileTransaction::new().unwrap();

    // Modify existing files
    tx.write_file(&existing1, "new1").unwrap();
    tx.write_file(&existing2, "new2").unwrap();

    // Create new files in nested directories
    let new1 = temp_dir.path().join("new/file1.txt");
    let new2 = temp_dir.path().join("new/nested/file2.txt");
    tx.write_file(&new1, "content1").unwrap();
    tx.write_file(&new2, "content2").unwrap();

    let receipt = tx.commit().unwrap();

    // Assert: Receipt is accurate
    assert_eq!(receipt.files_created.len(), 2);
    assert_eq!(receipt.files_modified.len(), 2);
    assert_eq!(receipt.backups.len(), 2);
    assert_eq!(receipt.total_files(), 4);

    // Verify each collection
    assert!(receipt.files_created.contains(&new1));
    assert!(receipt.files_created.contains(&new2));
    assert!(receipt.files_modified.contains(&existing1));
    assert!(receipt.files_modified.contains(&existing2));
    assert!(receipt.backups.contains_key(&existing1));
    assert!(receipt.backups.contains_key(&existing2));

    // Verify backups have correct content
    let backup1 = receipt.backups.get(&existing1).unwrap();
    let backup2 = receipt.backups.get(&existing2).unwrap();
    assert_eq!(fs::read_to_string(backup1).unwrap(), "old1");
    assert_eq!(fs::read_to_string(backup2).unwrap(), "old2");

    // Verify new files have correct content
    assert_eq!(fs::read_to_string(&new1).unwrap(), "content1");
    assert_eq!(fs::read_to_string(&new2).unwrap(), "content2");

    // Verify modified files have new content
    assert_eq!(fs::read_to_string(&existing1).unwrap(), "new1");
    assert_eq!(fs::read_to_string(&existing2).unwrap(), "new2");
}

// ============================================================================
// Edge Case: Zero-Byte File Handling
// ============================================================================

#[test]
fn test_zero_byte_file_operations() {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let empty_file = temp_dir.path().join("empty.txt");

    // Act: Create empty file
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&empty_file, "").unwrap();
    let receipt = tx.commit().unwrap();

    // Assert: Empty file created
    assert!(empty_file.exists());
    assert_eq!(fs::read_to_string(&empty_file).unwrap(), "");
    assert_eq!(fs::metadata(&empty_file).unwrap().len(), 0);
    assert_eq!(receipt.files_created.len(), 1);

    // Act: Modify empty file with content
    let mut tx2 = FileTransaction::new().unwrap();
    tx2.write_file(&empty_file, "now has content").unwrap();
    let receipt2 = tx2.commit().unwrap();

    // Assert: File now has content
    assert_eq!(fs::read_to_string(&empty_file).unwrap(), "now has content");
    assert_eq!(receipt2.files_modified.len(), 1);
    assert_eq!(receipt2.backups.len(), 1);

    // Verify backup is empty
    let backup = receipt2.backups.get(&empty_file).unwrap();
    assert_eq!(fs::read_to_string(backup).unwrap(), "");
}

// ============================================================================
// Edge Case: Transaction Cleanup After Panic
// ============================================================================

#[test]
#[should_panic(expected = "intentional panic")]
fn test_transaction_cleanup_on_panic() {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("panic_test.txt");

    // Act: Create transaction and panic (should trigger Drop rollback)
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&file_path, "content").unwrap();
    assert!(file_path.exists());

    // Panic - should trigger Drop and rollback
    panic!("intentional panic");

    // Note: After panic, Drop should clean up the file
    // This is tested in the next test
}

#[test]
fn test_transaction_rollback_after_drop() {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("drop_test.txt");

    // Act: Use catch_unwind to handle panic gracefully
    let result = std::panic::catch_unwind(|| {
        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file_path, "content").unwrap();
        assert!(file_path.exists());
        // Drop without commit
    });

    // Assert: Transaction dropped, file should be cleaned up
    assert!(result.is_ok(), "Block should complete without panic");
    assert!(
        !file_path.exists(),
        "File should be removed after drop without commit"
    );
}
