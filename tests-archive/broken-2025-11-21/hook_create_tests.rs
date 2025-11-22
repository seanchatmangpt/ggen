//! Unit tests for hook creation using Chicago TDD
//!
//! Tests the create module with real objects and state-based assertions.

use chicago_tdd_tools::prelude::*;
use ggen_domain::hook::create::*;
use std::fs;
use std::path::PathBuf;

/// Helper to get test hooks directory
fn test_hooks_dir() -> PathBuf {
    dirs::home_dir()
        .expect("Home directory should exist")
        .join(".ggen")
        .join("hooks")
}

/// Helper to clean up test hooks
fn cleanup_test_hooks(hook_id: &str) {
    let hook_file = test_hooks_dir().join(format!("{}.json", hook_id));
    let _ = fs::remove_file(hook_file);
}

async_test!(test_create_hook_with_name, {
    // Arrange
    let input = CreateInput {
        trigger: "file:change".to_string(),
        action: "echo 'test'".to_string(),
        name: Some("test-hook-create-1".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);
    let hook_result = result.unwrap();
    assert_eq!(hook_result.hook_id, "test-hook-create-1");
    assert!(matches!(hook_result.status, HookStatus::Created));

    // Verify file exists
    let hook_file = test_hooks_dir().join("test-hook-create-1.json");
    assert!(hook_file.exists(), "Hook file should be created");

    // Verify file contents
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["id"], "test-hook-create-1");
    assert_eq!(hook_data["trigger"], "file:change");
    assert_eq!(hook_data["action"], "echo 'test'");
    assert!(hook_data.get("created_at").is_some());

    // Cleanup
    cleanup_test_hooks("test-hook-create-1");
});

async_test!(test_create_hook_without_name_generates_id, {
    // Arrange
    let input = CreateInput {
        trigger: "template:render".to_string(),
        action: "cargo build".to_string(),
        name: None,
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);
    let hook_result = result.unwrap();
    assert!(hook_result.hook_id.starts_with("hook_template_render_"));
    assert!(matches!(hook_result.status, HookStatus::Created));

    // Verify file exists
    let hook_file = test_hooks_dir().join(format!("{}.json", hook_result.hook_id));
    assert!(hook_file.exists(), "Hook file should be created");

    // Cleanup
    cleanup_test_hooks(&hook_result.hook_id);
});

async_test!(test_create_hook_with_special_chars_in_trigger, {
    // Arrange
    let input = CreateInput {
        trigger: "file:change:*.rs".to_string(),
        action: "rustfmt".to_string(),
        name: Some("test-hook-create-special".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);
    let hook_result = result.unwrap();

    // Verify file contents preserve special characters
    let hook_file = test_hooks_dir().join("test-hook-create-special.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["trigger"], "file:change:*.rs");

    // Cleanup
    cleanup_test_hooks("test-hook-create-special");
});

async_test!(test_create_hook_creates_hooks_directory_if_missing, {
    // Arrange
    let hooks_dir = test_hooks_dir();
    let _ = fs::remove_dir_all(&hooks_dir); // Remove if exists

    let input = CreateInput {
        trigger: "test".to_string(),
        action: "test".to_string(),
        name: Some("test-hook-create-mkdir".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);
    assert!(hooks_dir.exists(), "Hooks directory should be created");

    // Cleanup
    cleanup_test_hooks("test-hook-create-mkdir");
});

async_test!(test_create_hook_with_empty_trigger, {
    // Arrange
    let input = CreateInput {
        trigger: "".to_string(),
        action: "echo 'test'".to_string(),
        name: Some("test-hook-empty-trigger".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert - Should succeed (validation is caller's responsibility)
    assert_ok!(&result);
    let hook_result = result.unwrap();

    // Verify file contains empty trigger
    let hook_file = test_hooks_dir().join("test-hook-empty-trigger.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["trigger"], "");

    // Cleanup
    cleanup_test_hooks("test-hook-empty-trigger");
});

async_test!(test_create_hook_with_long_action, {
    // Arrange
    let long_action = "echo ".to_string() + &"x".repeat(1000);
    let input = CreateInput {
        trigger: "test".to_string(),
        action: long_action.clone(),
        name: Some("test-hook-long-action".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);

    // Verify long action is preserved
    let hook_file = test_hooks_dir().join("test-hook-long-action.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["action"], long_action);

    // Cleanup
    cleanup_test_hooks("test-hook-long-action");
});

async_test!(test_create_hook_with_multiline_action, {
    // Arrange
    let multiline_action = "#!/bin/bash\necho 'line 1'\necho 'line 2'";
    let input = CreateInput {
        trigger: "test".to_string(),
        action: multiline_action.to_string(),
        name: Some("test-hook-multiline".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);

    // Verify multiline action is preserved
    let hook_file = test_hooks_dir().join("test-hook-multiline.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["action"], multiline_action);

    // Cleanup
    cleanup_test_hooks("test-hook-multiline");
});

async_test!(test_create_hook_overwrites_existing_hook, {
    // Arrange
    let input1 = CreateInput {
        trigger: "trigger1".to_string(),
        action: "action1".to_string(),
        name: Some("test-hook-overwrite".to_string()),
    };
    let input2 = CreateInput {
        trigger: "trigger2".to_string(),
        action: "action2".to_string(),
        name: Some("test-hook-overwrite".to_string()),
    };

    // Act
    execute_create(input1).await.unwrap();
    let result = execute_create(input2).await;

    // Assert
    assert_ok!(&result);

    // Verify second hook overwrote the first
    let hook_file = test_hooks_dir().join("test-hook-overwrite.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(hook_data["trigger"], "trigger2");
    assert_eq!(hook_data["action"], "action2");

    // Cleanup
    cleanup_test_hooks("test-hook-overwrite");
});

async_test!(test_create_hook_timestamp_format, {
    // Arrange
    let input = CreateInput {
        trigger: "test".to_string(),
        action: "test".to_string(),
        name: Some("test-hook-timestamp".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);

    // Verify created_at is valid RFC3339 timestamp
    let hook_file = test_hooks_dir().join("test-hook-timestamp.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    let hook_data: serde_json::Value = serde_json::from_str(&content).unwrap();
    let created_at = hook_data["created_at"].as_str().unwrap();

    // Should parse as valid timestamp
    assert!(chrono::DateTime::parse_from_rfc3339(created_at).is_ok());

    // Cleanup
    cleanup_test_hooks("test-hook-timestamp");
});

async_test!(test_create_hook_json_formatting, {
    // Arrange
    let input = CreateInput {
        trigger: "test".to_string(),
        action: "test".to_string(),
        name: Some("test-hook-json".to_string()),
    };

    // Act
    let result = execute_create(input).await;

    // Assert
    assert_ok!(&result);

    // Verify JSON is pretty-printed (contains newlines)
    let hook_file = test_hooks_dir().join("test-hook-json.json");
    let content = fs::read_to_string(&hook_file).unwrap();
    assert!(content.contains('\n'), "JSON should be pretty-printed");

    // Cleanup
    cleanup_test_hooks("test-hook-json");
});
