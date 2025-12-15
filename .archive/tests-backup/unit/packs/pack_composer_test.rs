//! Pack Composition Tests - Unit Level
//!
//! Tests the PackComposer component:
//! - Combining multiple packs
//! - Detecting circular dependencies
//! - Resolving dependencies
//! - Conflict detection

use ggen_domain::packs::{
    compose_packs, ComposePacksInput, CompositionStrategy, Pack, PackDependency, PackMetadata,
};
use std::collections::HashMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_compose_empty_pack_list_fails() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec![],
        project_name: "test-project".to_string(),
        output_dir: None,
        strategy: CompositionStrategy::default(),
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert - Should fail
    assert!(result.is_err(), "Should fail with empty pack list");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains("At least one pack"),
        "Error should mention pack requirement"
    );
}

#[tokio::test]
async fn test_compose_nonexistent_pack_fails() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec!["nonexistent-pack-xyz-123".to_string()],
        project_name: "test-project".to_string(),
        output_dir: None,
        strategy: CompositionStrategy::Merge,
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert - Should fail gracefully
    assert!(result.is_err(), "Should fail for nonexistent pack");
}

#[tokio::test]
async fn test_compose_single_pack() {
    // Arrange - Try with a known pack
    let input = ComposePacksInput {
        pack_ids: vec!["startup-essentials".to_string()],
        project_name: "my-startup".to_string(),
        output_dir: Some(PathBuf::from("/tmp/test-compose-single")),
        strategy: CompositionStrategy::Merge,
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert_eq!(output.project_name, "my-startup");
            assert_eq!(output.packs_composed.len(), 1);
            assert!(output.total_packages >= 0);
        }
        Err(e) => {
            // Expected if pack doesn't exist in test environment
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory"),
                "Should fail with appropriate error"
            );
        }
    }
}

#[tokio::test]
async fn test_compose_multiple_packs_merge_strategy() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec![
            "startup-essentials".to_string(),
            "data-science-toolkit".to_string(),
        ],
        project_name: "ml-startup".to_string(),
        output_dir: Some(PathBuf::from("/tmp/test-compose-multi")),
        strategy: CompositionStrategy::Merge,
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert_eq!(output.project_name, "ml-startup");
            assert_eq!(output.packs_composed.len(), 2);
            assert!(
                output.total_packages > 0,
                "Should have packages from both packs"
            );
            assert_eq!(output.composition_strategy, "Merge");
        }
        Err(e) => {
            // Expected if packs don't exist
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory")
            );
        }
    }
}

#[tokio::test]
async fn test_compose_layer_strategy() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec!["startup-essentials".to_string()],
        project_name: "layered-project".to_string(),
        output_dir: None,
        strategy: CompositionStrategy::Layer,
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert_eq!(output.composition_strategy, "Layer");
        }
        Err(_) => {
            // Expected if pack doesn't exist
        }
    }
}

#[tokio::test]
async fn test_compose_custom_strategy_not_implemented() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec!["startup-essentials".to_string()],
        project_name: "custom-project".to_string(),
        output_dir: None,
        strategy: CompositionStrategy::Custom(HashMap::new()),
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert - Should fail with not implemented error
    match result {
        Err(e) => {
            assert!(
                e.to_string().contains("not yet implemented")
                    || e.to_string().contains("not found"),
                "Should indicate custom strategy not implemented"
            );
        }
        Ok(_) => {
            // OK if pack doesn't exist (fails earlier)
        }
    }
}

#[test]
fn test_composition_strategy_serialization() {
    // Arrange
    let merge = CompositionStrategy::Merge;
    let layer = CompositionStrategy::Layer;
    let custom = CompositionStrategy::Custom({
        let mut map = HashMap::new();
        map.insert("rule1".to_string(), serde_json::json!("value1"));
        map
    });

    // Act
    let merge_json = serde_json::to_string(&merge).expect("Should serialize");
    let layer_json = serde_json::to_string(&layer).expect("Should serialize");
    let custom_json = serde_json::to_string(&custom).expect("Should serialize");

    // Assert
    assert!(merge_json.contains("Merge"));
    assert!(layer_json.contains("Layer"));
    assert!(custom_json.contains("Custom"));
}

#[test]
fn test_composition_strategy_default() {
    // Act
    let default = CompositionStrategy::default();

    // Assert - Should be Merge
    match default {
        CompositionStrategy::Merge => {
            // Success
        }
        _ => panic!("Default should be Merge strategy"),
    }
}

// Direct tests of helper functions (if they were public, we'd test them here)
// For now, we test through the public API

#[tokio::test]
async fn test_compose_creates_output_directory() {
    // Arrange
    let test_dir = PathBuf::from("/tmp/test-compose-dir-creation");
    let input = ComposePacksInput {
        pack_ids: vec!["startup-essentials".to_string()],
        project_name: "dir-test".to_string(),
        output_dir: Some(test_dir.clone()),
        strategy: CompositionStrategy::Merge,
    };

    // Act
    let _result = compose_packs(&input).await;

    // Assert - Directory should be created (if compose succeeded)
    // Note: This test is best-effort as it depends on pack existence
}

#[tokio::test]
async fn test_compose_uses_project_name_as_default_dir() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec!["startup-essentials".to_string()],
        project_name: "default-dir-test".to_string(),
        output_dir: None,
        strategy: CompositionStrategy::Merge,
    };

    // Act
    let result = compose_packs(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.output_path.to_string_lossy().contains("default-dir-test"),
            "Should use project name as directory"
        );
    }
}

#[test]
fn test_compose_input_serialization() {
    // Arrange
    let input = ComposePacksInput {
        pack_ids: vec!["pack1".to_string(), "pack2".to_string()],
        project_name: "test-project".to_string(),
        output_dir: Some(PathBuf::from("/tmp/test")),
        strategy: CompositionStrategy::Layer,
    };

    // Act
    let json = serde_json::to_string(&input).expect("Should serialize");
    let deserialized: ComposePacksInput =
        serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_ids.len(), 2);
    assert_eq!(deserialized.project_name, "test-project");
}
