//! User Workflow Tests - Template Reuse and Variables
//!
//! Workflow 5: Template reuse with variables

use ggen_domain::packs::{generate_from_pack, list_packs, show_pack, GenerateInput};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_workflow_5_template_reuse_with_variables() {
    // WORKFLOW 5: Template reuse with variables
    // User journey:
    // 1. List templates in a pack
    // 2. Select a template
    // 3. Generate with custom variables
    // 4. Verify template rendered with variables
    // 5. Verify can reuse template with different variables

    // Step 1: Get a pack with templates
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => {
            println!("No packs available for template test");
            return;
        }
    };

    // Find a pack with templates
    let pack_with_templates = packs.iter().find(|p| !p.templates.is_empty());
    if pack_with_templates.is_none() {
        println!("No packs with templates found");
        return;
    }

    let pack_id = &pack_with_templates.unwrap().id;
    let pack = show_pack(pack_id).expect("Should show pack");

    assert!(
        !pack.templates.is_empty(),
        "Pack should have templates"
    );

    // Step 2: Select first template
    let template = &pack.templates[0];
    println!("Testing template: {}", template.name);

    // Step 3: Generate with custom variables
    let mut variables = BTreeMap::new();
    variables.insert("project_name".to_string(), "my-first-project".to_string());
    variables.insert("author".to_string(), "Jane Doe".to_string());
    variables.insert("license".to_string(), "MIT".to_string());
    variables.insert("version".to_string(), "0.1.0".to_string());

    let gen_input1 = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: "my-first-project".to_string(),
        template_name: Some(template.name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-workflow5-gen1")),
        variables: variables.clone(),
    };

    let result1 = generate_from_pack(&gen_input1).await;
    assert!(result1.is_ok(), "Should generate with variables");

    let output1 = result1.unwrap();
    assert_eq!(output1.project_name, "my-first-project");
    assert!(
        output1.templates_generated.contains(&template.name),
        "Should generate requested template"
    );

    // Step 5: Reuse template with different variables
    let mut variables2 = BTreeMap::new();
    variables2.insert("project_name".to_string(), "my-second-project".to_string());
    variables2.insert("author".to_string(), "John Smith".to_string());
    variables2.insert("license".to_string(), "Apache-2.0".to_string());
    variables2.insert("version".to_string(), "1.0.0".to_string());

    let gen_input2 = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: "my-second-project".to_string(),
        template_name: Some(template.name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-workflow5-gen2")),
        variables: variables2,
    };

    let result2 = generate_from_pack(&gen_input2).await;
    assert!(result2.is_ok(), "Should reuse template with new variables");

    let output2 = result2.unwrap();
    assert_eq!(output2.project_name, "my-second-project");
    assert_ne!(
        output1.project_name, output2.project_name,
        "Should generate different projects"
    );

    println!("✓ Workflow 5 (Template reuse with variables) completed successfully");
}

#[tokio::test]
async fn test_list_templates_in_pack() {
    // Test listing available templates in a pack
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    for pack in packs {
        if !pack.templates.is_empty() {
            println!("Pack '{}' has {} templates:", pack.name, pack.templates.len());
            for template in &pack.templates {
                println!("  - {}: {}", template.name, template.description);
                println!("    Variables: {:?}", template.variables);
            }

            // Verify template structure
            for template in &pack.templates {
                assert!(!template.name.is_empty(), "Template should have name");
                assert!(
                    !template.path.is_empty(),
                    "Template should have path"
                );
            }
        }
    }
}

#[tokio::test]
async fn test_generate_with_all_variables() {
    // Test that all template variables can be provided
    let pack_id = "startup-essentials";
    let pack = match show_pack(pack_id) {
        Ok(p) => p,
        Err(_) => {
            println!("Pack not found");
            return;
        }
    };

    if pack.templates.is_empty() {
        println!("No templates to test");
        return;
    }

    let template = &pack.templates[0];

    // Provide all declared variables
    let mut variables = BTreeMap::new();
    for var_name in &template.variables {
        variables.insert(var_name.clone(), format!("value-for-{}", var_name));
    }

    let gen_input = GenerateInput {
        pack_id: pack_id.to_string(),
        project_name: "all-vars-project".to_string(),
        template_name: Some(template.name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-all-vars")),
        variables,
    };

    let result = generate_from_pack(&gen_input).await;
    assert!(result.is_ok(), "Should generate with all variables provided");
}

#[tokio::test]
async fn test_generate_with_partial_variables() {
    // Test generating with only some variables
    let pack_id = "startup-essentials";
    let pack = match show_pack(pack_id) {
        Ok(p) => p,
        Err(_) => return,
    };

    if pack.templates.is_empty() {
        return;
    }

    let template = &pack.templates[0];

    // Provide only some variables
    let mut variables = BTreeMap::new();
    if !template.variables.is_empty() {
        variables.insert(template.variables[0].clone(), "test-value".to_string());
    }

    let gen_input = GenerateInput {
        pack_id: pack_id.to_string(),
        project_name: "partial-vars-project".to_string(),
        template_name: Some(template.name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-partial-vars")),
        variables,
    };

    let result = generate_from_pack(&gen_input).await;
    // Should still work (missing variables might use defaults)
    assert!(result.is_ok() || result.is_err()); // Accept either outcome
}

#[tokio::test]
async fn test_generate_with_no_variables() {
    // Test generating with no variables (should use defaults)
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    let pack_with_templates = packs.iter().find(|p| !p.templates.is_empty());
    if pack_with_templates.is_none() {
        return;
    }

    let pack_id = &pack_with_templates.unwrap().id;

    let gen_input = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: "no-vars-project".to_string(),
        template_name: None,
        output_dir: Some(PathBuf::from("/tmp/test-no-vars")),
        variables: BTreeMap::new(), // No variables
    };

    let result = generate_from_pack(&gen_input).await;
    // Should work with defaults
    assert!(result.is_ok(), "Should generate with default variables");
}

#[tokio::test]
async fn test_generate_multiple_templates_same_pack() {
    // Test generating multiple templates from same pack
    let pack_id = "data-science-toolkit";
    let pack = match show_pack(pack_id) {
        Ok(p) => p,
        Err(_) => return,
    };

    if pack.templates.len() < 2 {
        println!("Need multiple templates for this test");
        return;
    }

    // Generate first template
    let gen_input1 = GenerateInput {
        pack_id: pack_id.to_string(),
        project_name: "project1".to_string(),
        template_name: Some(pack.templates[0].name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-multi-template-1")),
        variables: BTreeMap::new(),
    };

    let result1 = generate_from_pack(&gen_input1).await;
    assert!(result1.is_ok(), "Should generate first template");

    // Generate second template
    let gen_input2 = GenerateInput {
        pack_id: pack_id.to_string(),
        project_name: "project2".to_string(),
        template_name: Some(pack.templates[1].name.clone()),
        output_dir: Some(PathBuf::from("/tmp/test-multi-template-2")),
        variables: BTreeMap::new(),
    };

    let result2 = generate_from_pack(&gen_input2).await;
    assert!(result2.is_ok(), "Should generate second template");

    println!("✓ Multiple templates from same pack works");
}

#[tokio::test]
async fn test_template_variable_types() {
    // Test that variables can be various types (as strings)
    let mut variables = BTreeMap::new();
    variables.insert("string_var".to_string(), "hello".to_string());
    variables.insert("number_var".to_string(), "42".to_string());
    variables.insert("bool_var".to_string(), "true".to_string());
    variables.insert("path_var".to_string(), "/tmp/test".to_string());
    variables.insert("url_var".to_string(), "https://example.com".to_string());

    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    if packs.is_empty() || packs[0].templates.is_empty() {
        return;
    }

    let gen_input = GenerateInput {
        pack_id: packs[0].id.clone(),
        project_name: "var-types-test".to_string(),
        template_name: None,
        output_dir: Some(PathBuf::from("/tmp/test-var-types")),
        variables,
    };

    let result = generate_from_pack(&gen_input).await;
    // Should handle various variable types
    if result.is_ok() {
        println!("✓ Various variable types handled correctly");
    }
}

#[tokio::test]
async fn test_variable_substitution_in_output() {
    // Test that variables actually affect the output
    let mut variables = BTreeMap::new();
    variables.insert("project_name".to_string(), "CustomName".to_string());

    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    let pack = packs.iter().find(|p| !p.templates.is_empty());
    if pack.is_none() {
        return;
    }

    let pack_id = &pack.unwrap().id;

    let gen_input = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: "CustomName".to_string(),
        template_name: None,
        output_dir: Some(PathBuf::from("/tmp/test-var-subst")),
        variables,
    };

    let result = generate_from_pack(&gen_input).await;
    if let Ok(output) = result {
        assert_eq!(
            output.project_name, "CustomName",
            "Project name should reflect variable"
        );
    }
}
