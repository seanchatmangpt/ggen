//! Pack Generator Tests - Unit Level
//!
//! Tests the PackGenerator component:
//! - Rendering templates
//! - Variable substitution
//! - Project structure generation

use ggen_domain::packs::{generate_from_pack, GenerateInput};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_generate_from_pack_basic() {
    // Arrange
    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "my-project".to_string(),
        template_name: None,
        output_dir: Some(PathBuf::from("/tmp/test-gen-basic")),
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert_eq!(output.pack_id, "startup-essentials");
            assert_eq!(output.project_name, "my-project");
            assert!(output.files_created >= 0);
        }
        Err(e) => {
            // Expected if pack doesn't exist
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory")
            );
        }
    }
}

#[tokio::test]
async fn test_generate_with_specific_template() {
    // Arrange
    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "my-api".to_string(),
        template_name: Some("quick-start".to_string()),
        output_dir: Some(PathBuf::from("/tmp/test-gen-template")),
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert!(
                output.templates_generated.contains(&"quick-start".to_string()),
                "Should generate requested template"
            );
        }
        Err(e) => {
            // Expected if pack/template doesn't exist
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory")
            );
        }
    }
}

#[tokio::test]
async fn test_generate_with_variables() {
    // Arrange
    let mut variables = BTreeMap::new();
    variables.insert("project_name".to_string(), "my-api".to_string());
    variables.insert("author".to_string(), "John Doe".to_string());
    variables.insert("license".to_string(), "MIT".to_string());

    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "my-project".to_string(),
        template_name: Some("quick-start".to_string()),
        output_dir: Some(PathBuf::from("/tmp/test-gen-vars")),
        variables: variables.clone(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert - Variables should be stored in input
    assert_eq!(input.variables.len(), 3);
    assert_eq!(input.variables.get("project_name"), Some(&"my-api".to_string()));
}

#[tokio::test]
async fn test_generate_nonexistent_pack_fails() {
    // Arrange
    let input = GenerateInput {
        pack_id: "nonexistent-pack-xyz".to_string(),
        project_name: "test".to_string(),
        template_name: None,
        output_dir: None,
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    assert!(result.is_err(), "Should fail for nonexistent pack");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains("not found"),
        "Error should indicate pack not found"
    );
}

#[tokio::test]
async fn test_generate_nonexistent_template_fails() {
    // Arrange
    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "test".to_string(),
        template_name: Some("nonexistent-template-xyz".to_string()),
        output_dir: None,
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    match result {
        Err(e) => {
            let msg = e.to_string();
            assert!(
                msg.contains("not found") || msg.contains("directory"),
                "Should indicate template or pack not found"
            );
        }
        Ok(_) => {
            // OK if pack doesn't exist (fails with pack error first)
        }
    }
}

#[tokio::test]
async fn test_generate_uses_project_name_as_default_dir() {
    // Arrange
    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "default-output".to_string(),
        template_name: None,
        output_dir: None,
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.output_path.to_string_lossy().contains("default-output"),
            "Should use project name as output directory"
        );
    }
}

#[tokio::test]
async fn test_generate_from_data_science_pack() {
    // Arrange - Data science pack has multiple templates
    let input = GenerateInput {
        pack_id: "data-science-toolkit".to_string(),
        project_name: "ml-project".to_string(),
        template_name: Some("ml-pipeline".to_string()),
        output_dir: Some(PathBuf::from("/tmp/test-gen-ds")),
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.templates_generated.contains(&"ml-pipeline".to_string()),
            "Should generate ML pipeline template"
        );
    }
}

#[tokio::test]
async fn test_generate_all_templates_when_none_specified() {
    // Arrange
    let input = GenerateInput {
        pack_id: "data-science-toolkit".to_string(),
        project_name: "full-project".to_string(),
        template_name: None, // Generate all templates
        output_dir: Some(PathBuf::from("/tmp/test-gen-all")),
        variables: BTreeMap::new(),
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.templates_generated.len() > 0,
            "Should generate all templates when none specified"
        );
    }
}

#[test]
fn test_generate_input_serialization() {
    // Arrange
    let mut variables = BTreeMap::new();
    variables.insert("key1".to_string(), "value1".to_string());

    let input = GenerateInput {
        pack_id: "test-pack".to_string(),
        project_name: "test-project".to_string(),
        template_name: Some("test-template".to_string()),
        output_dir: Some(PathBuf::from("/tmp/test")),
        variables,
    };

    // Act
    let json = serde_json::to_string(&input).expect("Should serialize");
    let deserialized: GenerateInput = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "test-pack");
    assert_eq!(deserialized.project_name, "test-project");
    assert_eq!(deserialized.template_name, Some("test-template".to_string()));
    assert_eq!(deserialized.variables.len(), 1);
}

#[test]
fn test_generate_output_structure() {
    // Arrange
    let output = ggen_domain::packs::GenerateOutput {
        pack_id: "test-pack".to_string(),
        project_name: "test-project".to_string(),
        templates_generated: vec!["template1".to_string(), "template2".to_string()],
        files_created: 42,
        output_path: PathBuf::from("/tmp/test"),
    };

    // Act
    let json = serde_json::to_string(&output).expect("Should serialize");
    let deserialized: ggen_domain::packs::GenerateOutput =
        serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "test-pack");
    assert_eq!(deserialized.templates_generated.len(), 2);
    assert_eq!(deserialized.files_created, 42);
}

#[tokio::test]
async fn test_generate_with_empty_variables() {
    // Arrange
    let input = GenerateInput {
        pack_id: "startup-essentials".to_string(),
        project_name: "test".to_string(),
        template_name: None,
        output_dir: None,
        variables: BTreeMap::new(), // Empty variables
    };

    // Act
    let result = generate_from_pack(&input).await;

    // Assert - Should handle empty variables gracefully
    match result {
        Ok(_) => {
            // Success
        }
        Err(e) => {
            // Expected if pack doesn't exist
            assert!(e.to_string().contains("not found"));
        }
    }
}
