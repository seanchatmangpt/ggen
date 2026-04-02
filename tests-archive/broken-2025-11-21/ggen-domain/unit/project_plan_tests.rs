//! Unit tests for project planning logic
//!
//! Tests the create_plan function and related utilities

use chicago_tdd_tools::prelude::*;
use ggen_domain::project::plan::{create_plan, PlanInput};
use std::fs;
use tempfile::tempdir;

test!(test_create_plan_json_success, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("test-plan.json");

    let input = PlanInput {
        template_ref: "io.ggen.test-template".to_string(),
        vars: vec!["name=TestProject".to_string(), "version=1.0.0".to_string()],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();
    assert!(
        std::path::Path::new(&plan_result.output_path).exists(),
        "Plan file should exist"
    );
    assert_eq!(plan_result.variables_count, 2, "Should have 2 variables");

    // Verify JSON content
    let content = fs::read_to_string(&plan_result.output_path).unwrap();
    assert!(content.contains("io.ggen.test-template"));
    assert!(content.contains("TestProject"));
});

test!(test_create_plan_yaml_success, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("test-plan.yaml");

    let input = PlanInput {
        template_ref: "io.ggen.yaml-template".to_string(),
        vars: vec!["key=value".to_string()],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "yaml".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();
    assert!(std::path::Path::new(&plan_result.output_path).exists());

    // Verify YAML content
    let content = fs::read_to_string(&plan_result.output_path).unwrap();
    assert!(content.contains("io.ggen.yaml-template"));
    assert!(content.contains("key"));
});

test!(test_create_plan_toml_success, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("test-plan.toml");

    let input = PlanInput {
        template_ref: "io.ggen.toml-template".to_string(),
        vars: vec!["author=TestUser".to_string()],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "toml".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();
    assert!(std::path::Path::new(&plan_result.output_path).exists());

    // Verify TOML content
    let content = fs::read_to_string(&plan_result.output_path).unwrap();
    assert!(content.contains("io.ggen.toml-template"));
    assert!(content.contains("TestUser"));
});

test!(test_create_plan_empty_template_ref_error, {
    // Arrange
    let input = PlanInput {
        template_ref: "".to_string(),
        vars: vec![],
        output: None,
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("cannot be empty"),
        "Should error on empty template reference"
    );
});

test!(test_create_plan_invalid_variable_format_error, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("plan.json");

    let input = PlanInput {
        template_ref: "io.ggen.test".to_string(),
        vars: vec!["invalid_format".to_string()], // Missing '='
        output: Some(output_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Expected key=value"),
        "Should error on invalid variable format"
    );
});

test!(test_create_plan_unsupported_format_error, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("plan.xml");

    let input = PlanInput {
        template_ref: "io.ggen.test".to_string(),
        vars: vec![],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "xml".to_string(), // Unsupported
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Unsupported format"),
        "Should error on unsupported format"
    );
});

test!(test_create_plan_path_traversal_prevention, {
    // Arrange
    let input = PlanInput {
        template_ref: "io.ggen.test".to_string(),
        vars: vec![],
        output: Some("../../../etc/passwd".to_string()),
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Path traversal"),
        "Should prevent directory traversal attacks"
    );
});

test!(test_create_plan_default_output_path, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    let input = PlanInput {
        template_ref: "io.ggen.test".to_string(),
        vars: vec!["key=value".to_string()],
        output: None, // Use default
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Restore directory
    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();
    assert!(
        plan_result.output_path.contains("ggen-plan.json"),
        "Should use default filename"
    );
});

test!(test_create_plan_multiple_variables, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("multi-var-plan.json");

    let input = PlanInput {
        template_ref: "io.ggen.multi-var".to_string(),
        vars: vec![
            "name=MyProject".to_string(),
            "version=2.1.0".to_string(),
            "author=TestUser".to_string(),
            "license=MIT".to_string(),
        ],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();
    assert_eq!(plan_result.variables_count, 4, "Should have 4 variables");

    // Verify all variables are in the plan
    let content = fs::read_to_string(&plan_result.output_path).unwrap();
    assert!(content.contains("MyProject"));
    assert!(content.contains("2.1.0"));
    assert!(content.contains("TestUser"));
    assert!(content.contains("MIT"));
});

test!(test_create_plan_variable_with_equals_in_value, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let output_path = temp_dir.path().join("equals-plan.json");

    let input = PlanInput {
        template_ref: "io.ggen.url-test".to_string(),
        vars: vec!["url=https://example.com?foo=bar&baz=qux".to_string()],
        output: Some(output_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    // Act
    let result = create_plan(&input);

    // Assert
    assert_ok!(result);
    let plan_result = result.unwrap();

    // Verify URL is preserved correctly
    let content = fs::read_to_string(&plan_result.output_path).unwrap();
    assert!(
        content.contains("https://example.com?foo=bar&baz=qux"),
        "Should handle URLs with query parameters"
    );
});
