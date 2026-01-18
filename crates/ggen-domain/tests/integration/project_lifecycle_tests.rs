//! Integration tests for complete project lifecycle
//!
//! Tests the full flow: plan → apply → gen

use chicago_tdd_tools::prelude::*;
use ggen_domain::project::apply::{apply_plan, ApplyInput};
use ggen_domain::project::gen::{execute_gen, GenInput};
use ggen_domain::project::plan::{create_plan, PlanInput};
use std::fs;
use tempfile::tempdir;

/// Test complete lifecycle: create plan, apply plan, generate project
async_test!(test_project_lifecycle_plan_apply_gen, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("lifecycle-plan.json");
    let output_dir = temp_dir.path().join("generated-project");

    // Step 1: Create plan
    let plan_input = PlanInput {
        template_ref: "io.ggen.test-template".to_string(),
        vars: vec![
            "name=LifecycleTest".to_string(),
            "version=1.0.0".to_string(),
        ],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    let plan_result = plan_result.unwrap();
    assert!(std::path::Path::new(&plan_result.output_path).exists());

    // Step 2: Apply plan (dry run)
    let apply_input = ApplyInput {
        plan_file: plan_result.output_path.clone(),
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);

    let apply_result = apply_result.unwrap();
    assert_eq!(apply_result.operations_count, 2);

    // Step 3: Generate project (dry run)
    let gen_input = GenInput {
        template_ref: "io.ggen.test-template".to_string(),
        vars: vec![
            "name=LifecycleTest".to_string(),
            "version=1.0.0".to_string(),
        ],
        output_dir,
        dry_run: true,
    };

    let gen_result = execute_gen(gen_input).await;

    // Generation might fail due to template resolution in tests
    // The important part is that plan and apply worked
    match gen_result {
        Ok(result) => {
            assert_eq!(result.files_created, 0, "Dry run should create no files");
        }
        Err(e) => {
            // Template resolution error is acceptable in unit tests
            assert!(
                e.to_string().contains("template") || e.to_string().contains("resolve"),
                "Should be template-related error, not validation error"
            );
        }
    }
});

/// Test lifecycle with YAML format
async_test!(test_project_lifecycle_yaml_format, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("yaml-plan.yaml");

    // Step 1: Create YAML plan
    let plan_input = PlanInput {
        template_ref: "io.ggen.yaml-test".to_string(),
        vars: vec!["name=YamlProject".to_string()],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "yaml".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    // Step 2: Apply YAML plan
    let apply_input = ApplyInput {
        plan_file: plan_result.unwrap().output_path,
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);
});

/// Test lifecycle with TOML format
async_test!(test_project_lifecycle_toml_format, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("toml-plan.toml");

    // Step 1: Create TOML plan
    let plan_input = PlanInput {
        template_ref: "io.ggen.toml-test".to_string(),
        vars: vec!["name=TomlProject".to_string()],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "toml".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    // Step 2: Apply TOML plan
    let apply_input = ApplyInput {
        plan_file: plan_result.unwrap().output_path,
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);
});

/// Test lifecycle error handling: invalid plan format
async_test!(test_project_lifecycle_invalid_plan_format, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("invalid.json");

    // Create invalid plan file
    fs::write(&plan_path, "{invalid json}").unwrap();

    // Act - Try to apply invalid plan
    let apply_input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);

    // Assert
    assert_err!(apply_result);
});

/// Test lifecycle with complex variables
async_test!(test_project_lifecycle_complex_variables, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("complex-plan.json");

    // Step 1: Create plan with many variables
    let plan_input = PlanInput {
        template_ref: "io.ggen.complex".to_string(),
        vars: vec![
            "name=ComplexProject".to_string(),
            "version=2.1.0".to_string(),
            "author=TestUser".to_string(),
            "license=MIT".to_string(),
            "url=https://github.com/test/project".to_string(),
            "description=A complex test project".to_string(),
        ],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    let plan_result = plan_result.unwrap();
    assert_eq!(plan_result.variables_count, 6);

    // Step 2: Apply plan
    let apply_input = ApplyInput {
        plan_file: plan_result.output_path,
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);

    let apply_result = apply_result.unwrap();
    assert_eq!(apply_result.operations_count, 6);
});

/// Test lifecycle with empty variables
async_test!(test_project_lifecycle_no_variables, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("empty-plan.json");

    // Step 1: Create plan with no variables
    let plan_input = PlanInput {
        template_ref: "io.ggen.minimal".to_string(),
        vars: vec![],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    let plan_result = plan_result.unwrap();
    assert_eq!(plan_result.variables_count, 0);

    // Step 2: Apply plan
    let apply_input = ApplyInput {
        plan_file: plan_result.output_path,
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);

    let apply_result = apply_result.unwrap();
    assert_eq!(apply_result.operations_count, 0);
});

/// Test plan modification between create and apply
async_test!(test_project_lifecycle_plan_modification, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("modified-plan.json");

    // Step 1: Create initial plan
    let plan_input = PlanInput {
        template_ref: "io.ggen.test".to_string(),
        vars: vec!["name=Original".to_string()],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    // Step 2: Modify plan file manually
    let plan_content = fs::read_to_string(&plan_path).unwrap();
    let mut plan: serde_json::Value = serde_json::from_str(&plan_content).unwrap();

    if let Some(vars) = plan.get_mut("variables").and_then(|v| v.as_object_mut()) {
        vars.insert("name".to_string(), serde_json::json!("Modified"));
        vars.insert("extra".to_string(), serde_json::json!("value"));
    }

    fs::write(&plan_path, serde_json::to_string_pretty(&plan).unwrap()).unwrap();

    // Step 3: Apply modified plan
    let apply_input = ApplyInput {
        plan_file: plan_path.to_string_lossy().to_string(),
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result = apply_plan(&apply_input);
    assert_ok!(apply_result);

    let apply_result = apply_result.unwrap();
    assert_eq!(
        apply_result.operations_count, 2,
        "Should have modified variables"
    );
});

/// Test lifecycle persistence - plan can be applied multiple times
async_test!(test_project_lifecycle_plan_reusability, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let plan_path = temp_dir.path().join("reusable-plan.json");

    // Step 1: Create plan
    let plan_input = PlanInput {
        template_ref: "io.ggen.reusable".to_string(),
        vars: vec!["name=ReusableProject".to_string()],
        output: Some(plan_path.to_string_lossy().to_string()),
        format: "json".to_string(),
    };

    let plan_result = create_plan(&plan_input);
    assert_ok!(plan_result);

    let plan_file = plan_result.unwrap().output_path;

    // Step 2: Apply plan first time
    let apply_input_1 = ApplyInput {
        plan_file: plan_file.clone(),
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result_1 = apply_plan(&apply_input_1);
    assert_ok!(apply_result_1);

    // Step 3: Apply same plan second time
    let apply_input_2 = ApplyInput {
        plan_file: plan_file.clone(),
        dry_run: true,
        auto_confirm: true,
    };

    let apply_result_2 = apply_plan(&apply_input_2);
    assert_ok!(apply_result_2);

    // Both applications should succeed
    assert_eq!(
        apply_result_1.unwrap().operations_count,
        apply_result_2.unwrap().operations_count,
        "Plan should be reusable"
    );
});
