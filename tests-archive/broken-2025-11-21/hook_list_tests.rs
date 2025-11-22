//! Unit tests for hook listing using Chicago TDD
//!
//! Tests the list module with real objects and state-based assertions.

use chicago_tdd_tools::prelude::*;
use ggen_domain::hook::create::*;
use ggen_domain::hook::list::*;
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

async_test!(test_list_hooks_empty_directory, {
    // Arrange
    let hooks_dir = test_hooks_dir();
    let _ = fs::remove_dir_all(&hooks_dir); // Remove if exists

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();
    assert_eq!(
        hooks.len(),
        0,
        "Should return empty list for non-existent directory"
    );
});

async_test!(test_list_hooks_single_hook, {
    // Arrange
    create_test_hook("test-hook-list-1", "file:change", "echo 'test'").await;

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();
    assert_eq!(hooks.len(), 1);
    assert_eq!(hooks[0].id, "test-hook-list-1");
    assert_eq!(hooks[0].trigger, "file:change");
    assert_eq!(hooks[0].action, "echo 'test'");
    assert!(!hooks[0].created_at.is_empty());

    // Cleanup
    cleanup_test_hooks("test-hook-list-1");
});

async_test!(test_list_hooks_multiple_hooks, {
    // Arrange
    create_test_hook("test-hook-list-2a", "file:change", "action1").await;
    create_test_hook("test-hook-list-2b", "template:render", "action2").await;
    create_test_hook("test-hook-list-2c", "git:commit", "action3").await;

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();
    assert!(hooks.len() >= 3, "Should have at least 3 hooks");

    // Verify all test hooks are present
    let hook_ids: Vec<&str> = hooks.iter().map(|h| h.id.as_str()).collect();
    assert!(hook_ids.contains(&"test-hook-list-2a"));
    assert!(hook_ids.contains(&"test-hook-list-2b"));
    assert!(hook_ids.contains(&"test-hook-list-2c"));

    // Cleanup
    cleanup_test_hooks("test-hook-list-2a");
    cleanup_test_hooks("test-hook-list-2b");
    cleanup_test_hooks("test-hook-list-2c");
});

async_test!(test_list_hooks_with_filter_match, {
    // Arrange
    create_test_hook("test-hook-filter-1", "file:change", "action1").await;
    create_test_hook("test-hook-filter-2", "file:delete", "action2").await;
    create_test_hook("test-hook-filter-3", "template:render", "action3").await;

    let input = ListInput {
        verbose: false,
        filter: Some("file".to_string()),
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();

    // Should only contain hooks with "file" in trigger
    let filtered_hooks: Vec<_> = hooks
        .iter()
        .filter(|h| h.id.starts_with("test-hook-filter-"))
        .collect();
    assert!(filtered_hooks.len() >= 2);

    for hook in filtered_hooks {
        assert!(hook.trigger.contains("file"));
    }

    // Cleanup
    cleanup_test_hooks("test-hook-filter-1");
    cleanup_test_hooks("test-hook-filter-2");
    cleanup_test_hooks("test-hook-filter-3");
});

async_test!(test_list_hooks_with_filter_no_match, {
    // Arrange
    create_test_hook("test-hook-no-match", "file:change", "action1").await;

    let input = ListInput {
        verbose: false,
        filter: Some("nonexistent".to_string()),
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();

    // Should not contain test hook
    let test_hooks: Vec<_> = hooks
        .iter()
        .filter(|h| h.id == "test-hook-no-match")
        .collect();
    assert_eq!(test_hooks.len(), 0);

    // Cleanup
    cleanup_test_hooks("test-hook-no-match");
});

async_test!(test_list_hooks_verbose_flag, {
    // Arrange
    create_test_hook("test-hook-verbose", "test", "action").await;

    let input = ListInput {
        verbose: true,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert - verbose flag doesn't affect output structure, but tests it's accepted
    assert_ok!(&result);
    let hooks = result.unwrap();
    assert!(!hooks.is_empty());

    // Cleanup
    cleanup_test_hooks("test-hook-verbose");
});

async_test!(test_list_hooks_skips_invalid_json, {
    // Arrange
    fs::create_dir_all(test_hooks_dir()).unwrap();

    // Create valid hook
    create_test_hook("test-hook-valid", "test", "action").await;

    // Create invalid JSON file
    let invalid_file = test_hooks_dir().join("invalid-hook.json");
    fs::write(&invalid_file, "{ invalid json }").unwrap();

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert - Should succeed and skip invalid file
    assert_ok!(&result);
    let hooks = result.unwrap();

    // Should not contain invalid hook
    let invalid_hooks: Vec<_> = hooks.iter().filter(|h| h.id == "invalid-hook").collect();
    assert_eq!(invalid_hooks.len(), 0);

    // Cleanup
    cleanup_test_hooks("test-hook-valid");
    let _ = fs::remove_file(invalid_file);
});

async_test!(test_list_hooks_handles_missing_fields, {
    // Arrange
    fs::create_dir_all(test_hooks_dir()).unwrap();

    // Create hook with missing fields
    let partial_hook = test_hooks_dir().join("partial-hook.json");
    let partial_data = serde_json::json!({
        "id": "partial-hook"
        // Missing trigger, action, created_at
    });
    fs::write(&partial_hook, serde_json::to_string(&partial_data).unwrap()).unwrap();

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert - Should succeed with empty strings for missing fields
    assert_ok!(&result);
    let hooks = result.unwrap();

    let partial = hooks.iter().find(|h| h.id == "partial-hook");
    if let Some(hook) = partial {
        assert_eq!(hook.trigger, "");
        assert_eq!(hook.action, "");
        assert_eq!(hook.created_at, "");
    }

    // Cleanup
    let _ = fs::remove_file(partial_hook);
});

async_test!(test_list_hooks_only_reads_json_files, {
    // Arrange
    fs::create_dir_all(test_hooks_dir()).unwrap();

    // Create .json file
    create_test_hook("test-hook-json-ext", "test", "action").await;

    // Create non-.json file
    let txt_file = test_hooks_dir().join("not-a-hook.txt");
    fs::write(&txt_file, "test").unwrap();

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert - Should only list .json files
    assert_ok!(&result);
    let hooks = result.unwrap();

    // Should contain .json hook
    assert!(hooks.iter().any(|h| h.id == "test-hook-json-ext"));

    // Should not contain .txt file
    assert!(!hooks.iter().any(|h| h.id == "not-a-hook"));

    // Cleanup
    cleanup_test_hooks("test-hook-json-ext");
    let _ = fs::remove_file(txt_file);
});

async_test!(test_list_hooks_preserves_special_characters, {
    // Arrange
    create_test_hook("test-hook-special", "file:*.rs", "echo 'test'").await;

    let input = ListInput {
        verbose: false,
        filter: None,
    };

    // Act
    let result = execute_list(input).await;

    // Assert
    assert_ok!(&result);
    let hooks = result.unwrap();

    let special_hook = hooks.iter().find(|h| h.id == "test-hook-special").unwrap();
    assert_eq!(special_hook.trigger, "file:*.rs");

    // Cleanup
    cleanup_test_hooks("test-hook-special");
});
