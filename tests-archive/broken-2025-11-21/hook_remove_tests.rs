//! Unit tests for hook removal using Chicago TDD
//!
//! Tests the remove module with real objects and state-based assertions.

use chicago_tdd_tools::prelude::*;
use ggen_domain::hook::create::*;
use ggen_domain::hook::remove::*;
use std::fs;
use std::path::PathBuf;

/// Helper to get test hooks directory
fn test_hooks_dir() -> PathBuf {
    dirs::home_dir()
        .expect("Home directory should exist")
        .join(".ggen")
        .join("hooks")
}

/// Helper to create a test hook
async fn create_test_hook(hook_id: &str, trigger: &str, action: &str) {
    let input = CreateInput {
        trigger: trigger.to_string(),
        action: action.to_string(),
        name: Some(hook_id.to_string()),
    };
    execute_create(input).await.unwrap();
}

/// Helper to clean up test hooks
fn cleanup_test_hooks(hook_id: &str) {
    let hook_file = test_hooks_dir().join(format!("{}.json", hook_id));
    let _ = fs::remove_file(hook_file);
}

async_test!(test_remove_hook_success, {
    // Arrange
    create_test_hook("test-hook-remove-1", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-remove-1".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_ok!(&result);
    let hook_result = result.unwrap();
    assert_eq!(hook_result.hook_id, "test-hook-remove-1");
    assert!(matches!(hook_result.status, HookStatus::Removed));

    // Verify file is deleted
    let hook_file = test_hooks_dir().join("test-hook-remove-1.json");
    assert!(!hook_file.exists(), "Hook file should be deleted");
});

async_test!(test_remove_hook_without_force_fails, {
    // Arrange
    create_test_hook("test-hook-remove-no-force", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-remove-no-force".to_string(),
        force: false,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_err!(&result);
    let err = result.unwrap_err();
    assert!(err.to_string().contains("force"));

    // Verify file still exists
    let hook_file = test_hooks_dir().join("test-hook-remove-no-force.json");
    assert!(
        hook_file.exists(),
        "Hook file should not be deleted without force"
    );

    // Cleanup
    cleanup_test_hooks("test-hook-remove-no-force");
});

async_test!(test_remove_nonexistent_hook_fails, {
    // Arrange
    let input = RemoveInput {
        hook_id: "nonexistent-hook-12345".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_err!(&result);
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not found"));
});

async_test!(test_remove_hook_with_empty_id_fails, {
    // Arrange
    let input = RemoveInput {
        hook_id: "".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_err!(&result);
});

async_test!(test_remove_hook_multiple_times_fails_second_time, {
    // Arrange
    create_test_hook("test-hook-double-remove", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-double-remove".to_string(),
        force: true,
    };

    // Act
    let result1 = execute_remove(input.clone()).await;
    let result2 = execute_remove(input).await;

    // Assert
    assert_ok!(&result1, "First removal should succeed");
    assert_err!(&result2, "Second removal should fail");
});

async_test!(test_remove_hook_with_special_chars_in_id, {
    // Arrange
    create_test_hook("test-hook-special-123", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-special-123".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_ok!(&result);

    // Verify file is deleted
    let hook_file = test_hooks_dir().join("test-hook-special-123.json");
    assert!(!hook_file.exists());
});

async_test!(test_remove_hook_preserves_other_hooks, {
    // Arrange
    create_test_hook("test-hook-preserve-1", "test1", "action1").await;
    create_test_hook("test-hook-preserve-2", "test2", "action2").await;

    let input = RemoveInput {
        hook_id: "test-hook-preserve-1".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_ok!(&result);

    // Verify first hook is deleted
    let hook_file1 = test_hooks_dir().join("test-hook-preserve-1.json");
    assert!(!hook_file1.exists());

    // Verify second hook still exists
    let hook_file2 = test_hooks_dir().join("test-hook-preserve-2.json");
    assert!(hook_file2.exists(), "Other hooks should be preserved");

    // Cleanup
    cleanup_test_hooks("test-hook-preserve-2");
});

async_test!(test_remove_hook_default_input, {
    // Arrange
    create_test_hook("test-hook-default", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-default".to_string(),
        ..Default::default()
    };

    // Act
    let result = execute_remove(input).await;

    // Assert - Default force is false, should fail
    assert_err!(&result);

    // Cleanup
    cleanup_test_hooks("test-hook-default");
});

async_test!(test_remove_hook_with_path_traversal_attempt, {
    // Arrange - This tests that the function properly handles malicious input
    let input = RemoveInput {
        hook_id: "../../../etc/passwd".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert - Should fail to find the hook (path traversal contained)
    assert_err!(&result);
});

async_test!(test_remove_hook_verification_status, {
    // Arrange
    create_test_hook("test-hook-status", "test", "action").await;

    let input = RemoveInput {
        hook_id: "test-hook-status".to_string(),
        force: true,
    };

    // Act
    let result = execute_remove(input).await;

    // Assert
    assert_ok!(&result);
    let hook_result = result.unwrap();

    // Verify status is Removed
    match hook_result.status {
        HookStatus::Removed => {}
        _ => panic!("Expected HookStatus::Removed"),
    }
});
