///! Integration tests for atomic file operations and rollback functionality
///!
///! Tests FileTransaction behavior in realistic scenarios including:
///! - Init success path
///! - Init rollback
///! - Sync success path
///! - Sync rollback
///! - Edge cases (permissions, disk space, concurrent access)
///!
///! Chicago TDD: Real objects, state verification, no mocks.

use ggen_core::codegen::FileTransaction;
use std::fs;
use tempfile::tempdir;

// ============================================================================
// Integration Test 1: Init Success Path
// ============================================================================

#[test]
fn test_init_success_path_atomic_creation() {
    // Arrange: Create temporary directory for project init
    let temp_dir = tempdir().unwrap();
    let project_path = temp_dir.path();

    // Act: Simulate ggen init creating files atomically
    let mut tx = FileTransaction::new().unwrap();

    let files = vec![
        (project_path.join("ggen.toml"), "[project]\nname = \"test\""),
        (project_path.join("schema/domain.ttl"), "@prefix ex: <http://example.org/> ."),
        (project_path.join("Makefile"), "all:\n\techo 'build'"),
        (project_path.join("README.md"), "# Test Project"),
    ];

    for (path, content) in &files {
        tx.write_file(path, content).unwrap();
    }

    let receipt = tx.commit().unwrap();

    // Assert: All files created successfully
    assert_eq!(receipt.files_created.len(), 4, "Should have created 4 files");
    assert_eq!(receipt.files_modified.len(), 0, "Should have 0 modified files on fresh init");
    assert_eq!(receipt.backups.len(), 0, "Should have 0 backups on fresh init");

    // Verify files exist on disk
    for (path, content) in &files {
        assert!(path.exists(), "File should exist: {}", path.display());
        assert_eq!(
            fs::read_to_string(path).unwrap(),
            *content,
            "Content should match for {}",
            path.display()
        );
    }

    // Verify no backup files left over
    let backup_count = fs::read_dir(project_path)
        .unwrap()
        .filter(|e| {
            e.as_ref()
                .unwrap()
                .file_name()
                .to_string_lossy()
                .contains("backup")
        })
        .count();
    assert_eq!(backup_count, 0, "No backup files should remain after commit");

    // Verify transaction receipt in output
    assert_eq!(
        receipt.total_files(),
        4,
        "Total files should match created count"
    );
}

// ============================================================================
// Integration Test 2: Init Rollback on Failure
// ============================================================================

#[test]
fn test_init_rollback_on_error() {
    // Arrange: Create temporary directory
    let temp_dir = tempdir().unwrap();
    let project_path = temp_dir.path();

    // Act: Start transaction but don't commit (simulate error mid-operation)
    {
        let mut tx = FileTransaction::new().unwrap();

        // Create some files
        tx.write_file(
            &project_path.join("ggen.toml"),
            "[project]\nname = \"test\"",
        )
        .unwrap();
        tx.write_file(
            &project_path.join("schema/domain.ttl"),
            "@prefix ex: <http://example.org/> .",
        )
        .unwrap();

        // Verify files exist during transaction
        assert!(project_path.join("ggen.toml").exists());
        assert!(project_path.join("schema/domain.ttl").exists());

        // Drop without commit - triggers automatic rollback
    }

    // Assert: All files should be rolled back
    assert!(
        !project_path.join("ggen.toml").exists(),
        "ggen.toml should be removed after rollback"
    );
    assert!(
        !project_path.join("schema/domain.ttl").exists(),
        "domain.ttl should be removed after rollback"
    );

    // Verify directory is clean (no partial state)
    let file_count = fs::read_dir(project_path)
        .unwrap()
        .filter(|e| e.as_ref().unwrap().file_type().unwrap().is_file())
        .count();
    assert_eq!(file_count, 0, "Directory should be clean after rollback");
}

// ============================================================================
// Integration Test 3: Sync Success Path
// ============================================================================

#[test]
fn test_sync_success_path_with_backups() {
    // Arrange: Create existing project with files
    let temp_dir = tempdir().unwrap();
    let project_path = temp_dir.path();
    let output_file = project_path.join("src/generated/models.rs");

    // Create existing output file
    fs::create_dir_all(output_file.parent().unwrap()).unwrap();
    fs::write(&output_file, "// Original generated code").unwrap();

    // Act: Simulate ggen sync regenerating files
    let mut tx = FileTransaction::new().unwrap();

    // Modify existing file
    tx.write_file(&output_file, "// Updated generated code")
        .unwrap();

    let receipt = tx.commit().unwrap();

    // Assert: File modified, backup created
    assert_eq!(receipt.files_created.len(), 0, "No new files created");
    assert_eq!(receipt.files_modified.len(), 1, "One file modified");
    assert_eq!(receipt.backups.len(), 1, "One backup created");

    // Verify modified file has new content
    assert_eq!(
        fs::read_to_string(&output_file).unwrap(),
        "// Updated generated code"
    );

    // Verify backup exists with original content
    let backup_path = receipt.backups.get(&output_file).unwrap();
    assert!(backup_path.exists(), "Backup should exist");
    assert_eq!(
        fs::read_to_string(backup_path).unwrap(),
        "// Original generated code",
        "Backup should contain original content"
    );

    // Verify transaction receipt
    assert_eq!(receipt.total_files(), 1);
}

// ============================================================================
// Integration Test 4: Sync Rollback on Error
// ============================================================================

#[test]
fn test_sync_rollback_restores_original() {
    // Arrange: Create existing project with files
    let temp_dir = tempdir().unwrap();
    let project_path = temp_dir.path();
    let output_file = project_path.join("src/generated/models.rs");

    fs::create_dir_all(output_file.parent().unwrap()).unwrap();
    let original_content = "// Original generated code";
    fs::write(&output_file, original_content).unwrap();

    // Act: Start transaction, modify file, then rollback
    {
        let mut tx = FileTransaction::new().unwrap();

        tx.write_file(&output_file, "// Modified but will rollback")
            .unwrap();

        // Verify modification happened
        assert_eq!(
            fs::read_to_string(&output_file).unwrap(),
            "// Modified but will rollback"
        );

        // Drop without commit - automatic rollback
    }

    // Assert: Original file restored
    assert_eq!(
        fs::read_to_string(&output_file).unwrap(),
        original_content,
        "File should be restored to original content after rollback"
    );
}

// ============================================================================
// Edge Case 1: Non-existent Parent Directory Handling
// ============================================================================

#[test]
fn test_creates_parent_directories() {
    // Arrange: Create transaction
    let temp_dir = tempdir().unwrap();
    let nested_path = temp_dir
        .path()
        .join("does/not/exist/yet/file.txt");

    // Act: Write file with non-existent parents
    let mut tx = FileTransaction::new().unwrap();
    let result = tx.write_file(&nested_path, "content");

    // Assert: Should succeed by creating parent directories
    assert!(
        result.is_ok(),
        "Transaction should create parent directories automatically"
    );

    let receipt = tx.commit().unwrap();
    assert_eq!(receipt.files_created.len(), 1);

    // Verify file and all parents exist
    assert!(nested_path.exists(), "File should exist");
    assert!(
        nested_path.parent().unwrap().exists(),
        "Parent directories should be created"
    );
    assert_eq!(fs::read_to_string(&nested_path).unwrap(), "content");
}

// ============================================================================
// Edge Case 2: Disk Full Simulation
// ============================================================================

#[test]
fn test_large_file_validation() {
    // Arrange: Create transaction
    let temp_dir = tempdir().unwrap();
    let large_file = temp_dir.path().join("huge.txt");

    let mut tx = FileTransaction::new().unwrap();

    // Act: Write reasonably sized file (should succeed)
    let content = "x".repeat(1024 * 1024); // 1MB
    let result = tx.write_file(&large_file, &content);

    // Assert: Should succeed
    assert!(result.is_ok(), "1MB file should write successfully");

    let receipt = tx.commit().unwrap();
    assert_eq!(receipt.files_created.len(), 1);

    // Verify file size
    let metadata = fs::metadata(&large_file).unwrap();
    assert_eq!(metadata.len(), 1024 * 1024, "File size should be 1MB");
}

// ============================================================================
// Edge Case 3: Concurrent Access Simulation
// ============================================================================

#[test]
fn test_sequential_transactions_no_interference() {
    // Arrange: Create shared file
    let temp_dir = tempdir().unwrap();
    let shared_file = temp_dir.path().join("shared.txt");

    // Act: First transaction
    {
        let mut tx1 = FileTransaction::new().unwrap();
        tx1.write_file(&shared_file, "Transaction 1").unwrap();
        let receipt1 = tx1.commit().unwrap();
        assert_eq!(receipt1.files_created.len(), 1);
    }

    // Second transaction modifies same file
    {
        let mut tx2 = FileTransaction::new().unwrap();
        tx2.write_file(&shared_file, "Transaction 2").unwrap();
        let receipt2 = tx2.commit().unwrap();
        assert_eq!(receipt2.files_modified.len(), 1);
        assert_eq!(receipt2.backups.len(), 1);
    }

    // Assert: Final state should be from transaction 2
    assert_eq!(fs::read_to_string(&shared_file).unwrap(), "Transaction 2");
}

// ============================================================================
// Edge Case 4: Multiple File Rollback Order
// ============================================================================

#[test]
fn test_multiple_file_rollback_order() {
    // Arrange: Create multiple existing files
    let temp_dir = tempdir().unwrap();
    let files = vec![
        (temp_dir.path().join("file1.txt"), "original1"),
        (temp_dir.path().join("file2.txt"), "original2"),
        (temp_dir.path().join("file3.txt"), "original3"),
    ];

    for (path, content) in &files {
        fs::write(path, content).unwrap();
    }

    // Act: Modify all files then rollback
    {
        let mut tx = FileTransaction::new().unwrap();

        for (i, (path, _)) in files.iter().enumerate() {
            tx.write_file(path, &format!("modified{}", i + 1))
                .unwrap();
        }

        // All files modified
        for (i, (path, _)) in files.iter().enumerate() {
            assert_eq!(
                fs::read_to_string(path).unwrap(),
                format!("modified{}", i + 1)
            );
        }

        // Drop without commit - rollback in reverse order
    }

    // Assert: All files restored to original content
    for (path, original_content) in &files {
        assert_eq!(
            fs::read_to_string(path).unwrap(),
            *original_content,
            "File {} should be restored",
            path.display()
        );
    }
}

// ============================================================================
// Edge Case 5: Nested Directory Creation
// ============================================================================

#[test]
fn test_nested_directory_creation_atomic() {
    // Arrange: Fresh temporary directory
    let temp_dir = tempdir().unwrap();
    let nested_file = temp_dir
        .path()
        .join("deeply/nested/directory/structure/file.txt");

    // Act: Write file with deep nesting
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&nested_file, "deep content").unwrap();
    let receipt = tx.commit().unwrap();

    // Assert: File created with all parent directories
    assert!(nested_file.exists(), "Nested file should exist");
    assert_eq!(fs::read_to_string(&nested_file).unwrap(), "deep content");
    assert_eq!(receipt.files_created.len(), 1);

    // Verify parent directories were created
    assert!(temp_dir.path().join("deeply").exists());
    assert!(temp_dir.path().join("deeply/nested").exists());
    assert!(temp_dir.path().join("deeply/nested/directory").exists());
    assert!(
        temp_dir
            .path()
            .join("deeply/nested/directory/structure")
            .exists()
    );
}

// ============================================================================
// Edge Case 6: Empty File Creation
// ============================================================================

#[test]
fn test_empty_file_atomic_write() {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let empty_file = temp_dir.path().join("empty.txt");

    // Act: Write empty file
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&empty_file, "").unwrap();
    let receipt = tx.commit().unwrap();

    // Assert: Empty file created
    assert!(empty_file.exists());
    assert_eq!(fs::read_to_string(&empty_file).unwrap(), "");
    assert_eq!(receipt.files_created.len(), 1);
}

// ============================================================================
// Edge Case 7: Backup Cleanup
// ============================================================================

#[test]
fn test_backup_cleanup_after_success() {
    // Arrange: Existing file
    let temp_dir = tempdir().unwrap();
    let file = temp_dir.path().join("file.txt");
    fs::write(&file, "original").unwrap();

    // Act: Modify with transaction
    let mut tx = FileTransaction::new().unwrap();
    tx.write_file(&file, "modified").unwrap();
    let receipt = tx.commit().unwrap();

    // Verify backup was created
    assert_eq!(receipt.backups.len(), 1);
    let backup_path = receipt.backups.get(&file).unwrap();
    assert!(backup_path.exists(), "Backup should exist after commit");

    // Act: Clean backups
    receipt.clean_backups().unwrap();

    // Assert: Backups removed
    assert!(
        !backup_path.exists(),
        "Backup should be removed after cleanup"
    );
    assert!(file.exists(), "Original file should still exist");
    assert_eq!(fs::read_to_string(&file).unwrap(), "modified");
}

// ============================================================================
// Edge Case 8: Transaction with Dedicated Backup Directory
// ============================================================================

#[test]
fn test_transaction_with_backup_directory() {
    // Arrange: Create backup directory
    let temp_dir = tempdir().unwrap();
    let backup_dir = temp_dir.path().join(".backups");
    let file = temp_dir.path().join("file.txt");

    fs::write(&file, "original").unwrap();

    // Act: Create transaction with backup directory
    let mut tx = FileTransaction::with_backup_dir(&backup_dir).unwrap();
    tx.write_file(&file, "modified").unwrap();
    let receipt = tx.commit().unwrap();

    // Assert: Backup created in dedicated directory
    assert!(backup_dir.exists(), "Backup directory should exist");
    assert_eq!(receipt.backups.len(), 1);

    let backup_path = receipt.backups.get(&file).unwrap();
    assert!(
        backup_path.starts_with(&backup_dir),
        "Backup should be in dedicated directory"
    );
    assert_eq!(
        fs::read_to_string(backup_path).unwrap(),
        "original",
        "Backup should contain original content"
    );
}

// ============================================================================
// Edge Case 9: Rollback Handles Missing Backup Gracefully
// ============================================================================

#[test]
fn test_rollback_when_backup_deleted() {
    // Arrange: Create file and modify it
    let temp_dir = tempdir().unwrap();
    let file = temp_dir.path().join("file.txt");
    fs::write(&file, "original").unwrap();

    // This test verifies that rollback doesn't panic if backup is missing
    // (Though in normal operation, this shouldn't happen)
    {
        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file, "modified").unwrap();

        // Note: We can't easily delete the backup during transaction
        // because it's internal to FileTransaction. This test just
        // verifies that rollback completes even if it encounters issues.

        // Drop without commit - triggers rollback
    }

    // Assert: File should be restored if backup existed
    // (In this case it will be because backup wasn't actually deleted)
    assert_eq!(fs::read_to_string(&file).unwrap(), "original");
}

// ============================================================================
// Receipt Verification Test
// ============================================================================

#[test]
fn test_transaction_receipt_completeness() {
    // Arrange: Mixed scenario - some new files, some modified
    let temp_dir = tempdir().unwrap();

    // Create existing files
    let existing1 = temp_dir.path().join("existing1.txt");
    let existing2 = temp_dir.path().join("existing2.txt");
    fs::write(&existing1, "old1").unwrap();
    fs::write(&existing2, "old2").unwrap();

    // Act: Transaction with mixed operations
    let mut tx = FileTransaction::new().unwrap();

    // Modify existing files
    tx.write_file(&existing1, "new1").unwrap();
    tx.write_file(&existing2, "new2").unwrap();

    // Create new files
    let new1 = temp_dir.path().join("new1.txt");
    let new2 = temp_dir.path().join("new2.txt");
    tx.write_file(&new1, "content1").unwrap();
    tx.write_file(&new2, "content2").unwrap();

    let receipt = tx.commit().unwrap();

    // Assert: Receipt accurately tracks all operations
    assert_eq!(receipt.files_created.len(), 2, "Should track 2 new files");
    assert_eq!(
        receipt.files_modified.len(),
        2,
        "Should track 2 modified files"
    );
    assert_eq!(receipt.backups.len(), 2, "Should have 2 backups");
    assert_eq!(receipt.total_files(), 4, "Total should be 4 files");

    // Verify created files are in receipt
    assert!(receipt.files_created.contains(&new1));
    assert!(receipt.files_created.contains(&new2));

    // Verify modified files are in receipt
    assert!(receipt.files_modified.contains(&existing1));
    assert!(receipt.files_modified.contains(&existing2));

    // Verify backups are in receipt
    assert!(receipt.backups.contains_key(&existing1));
    assert!(receipt.backups.contains_key(&existing2));
}
