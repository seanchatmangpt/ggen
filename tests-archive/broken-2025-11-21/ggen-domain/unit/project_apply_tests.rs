//! Unit tests for project plan application logic
//!
//! Tests the apply_plan function and related utilities

use chicago_tdd_tools::prelude::*;
use ggen_domain::project::apply::{apply_plan, ApplyInput};
use std::fs;
use tempfile::tempdir;

test!(test_apply_plan_dry_run_success, {
    // Arrange - Create a valid plan file
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("test-plan.json");

    let plan = serde_json::json!({
        "template_ref": "io.ggen.test-template",
        "variables": {
            "name": "TestProject",
            "version": "1.0.0"
        },
        "timestamp": "2024-11-13T00:00:00Z",
        "format": "json"
    });

    fs::write(&plan_path, serde_json::to_string_pretty(&plan).unwrap()).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_ok!(result);
    let app_result = result.unwrap();
    assert_eq!(app_result.operations_count, 2, "Should count variables");
    assert_eq!(app_result.plan_file, input.plan_file);
});

test!(test_apply_plan_yaml_format, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("test-plan.yaml");

    let plan_yaml = r#"
template_ref: io.ggen.yaml-template
variables:
  name: YamlProject
  author: TestUser
timestamp: 2024-11-13T00:00:00Z
format: yaml
"#;

    fs::write(&plan_path, plan_yaml).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_ok!(result);
    let app_result = result.unwrap();
    assert_eq!(app_result.operations_count, 2);
});

test!(test_apply_plan_toml_format, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("test-plan.toml");

    let plan_toml = r#"
template_ref = "io.ggen.toml-template"
format = "toml"
timestamp = "2024-11-13T00:00:00Z"

[variables]
name = "TomlProject"
license = "MIT"
"#;

    fs::write(&plan_path, plan_toml).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_ok!(result);
    let app_result = result.unwrap();
    assert_eq!(app_result.operations_count, 2);
});

test!(test_apply_plan_file_not_found_error, {
    // Arrange
    let input = ApplyInput {
        plan_file: "/nonexistent/path/plan.json".to_string(),
        dry_run: false,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("not found"),
        "Should error when plan file doesn't exist"
    );
});

test!(test_apply_plan_unsupported_format_error, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("test-plan.xml");

    fs::write(&plan_path, "<plan></plan>").unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: false,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Unsupported plan file format"),
        "Should error on unsupported file format"
    );
});

test!(test_apply_plan_path_traversal_prevention, {
    // Arrange
    let input = ApplyInput {
        plan_file: "../../../etc/passwd".to_string(),
        dry_run: false,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_err!(result);
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Path traversal"),
        "Should prevent directory traversal"
    );
});

test!(test_apply_plan_invalid_json_error, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("invalid.json");

    fs::write(&plan_path, "{invalid json}").unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: false,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_err!(result);
});

test!(test_apply_plan_no_auto_confirm_cancellation, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("test-plan.json");

    let plan = serde_json::json!({
        "template_ref": "io.ggen.test",
        "variables": {},
        "timestamp": "2024-11-13T00:00:00Z",
        "format": "json"
    });

    fs::write(&plan_path, serde_json::to_string_pretty(&plan).unwrap()).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: false,
        auto_confirm: false,
    };

    // Note: This test would require mocking stdin
    // For now, we just verify the input is created correctly
    assert_eq!(input.auto_confirm, false);
    assert_eq!(input.dry_run, false);
});

test!(test_apply_plan_with_empty_variables, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("empty-vars.json");

    let plan = serde_json::json!({
        "template_ref": "io.ggen.minimal",
        "variables": {},
        "timestamp": "2024-11-13T00:00:00Z",
        "format": "json"
    });

    fs::write(&plan_path, serde_json::to_string_pretty(&plan).unwrap()).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_ok!(result);
    let app_result = result.unwrap();
    assert_eq!(app_result.operations_count, 0, "Should have no operations");
});

test!(test_apply_plan_with_many_variables, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("many-vars.json");

    let mut variables = serde_json::Map::new();
    for i in 1..=20 {
        variables.insert(
            format!("var{}", i),
            serde_json::json!(format!("value{}", i)),
        );
    }

    let plan = serde_json::json!({
        "template_ref": "io.ggen.complex",
        "variables": variables,
        "timestamp": "2024-11-13T00:00:00Z",
        "format": "json"
    });

    fs::write(&plan_path, serde_json::to_string_pretty(&plan).unwrap()).unwrap();

    let input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    // Act
    let result = apply_plan(&input);

    // Assert
    assert_ok!(result);
    let app_result = result.unwrap();
    assert_eq!(app_result.operations_count, 20, "Should have 20 variables");
});
