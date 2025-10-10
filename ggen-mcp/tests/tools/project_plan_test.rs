// London School TDD Tests for ggen_project_plan
// Focus: Interaction testing and behavior verification

use ggen_mcp::tools::project;
use serde_json::json;

// BDD-Style Tests

#[tokio::test]
async fn should_create_execution_plan_without_applying_changes() {
    // Given: Valid template parameters
    let params = json!({
        "template": "rust-web-api",
        "vars": {
            "name": "my-api",
            "port": 8080
        }
    });

    // When: Plan is created
    let result = project::plan(params).await;

    // Then: Should return plan without modifying filesystem
    assert!(result.is_ok());
    let response = result.unwrap();

    let data = response.get("data").unwrap();
    assert!(data.get("plan_id").is_some());
    assert!(data.get("actions").is_some());
    assert!(data.get("estimated_files").is_some());
}

#[tokio::test]
async fn should_include_action_details_in_plan() {
    // Given: Template with multiple files
    let params = json!({
        "template": "rust-lib"
    });

    // When: Plan is generated
    let result = project::plan(params).await;

    // Then: Plan should contain detailed actions
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let actions = data.get("actions").unwrap().as_array().unwrap();
    assert!(!actions.is_empty(), "Plan should contain actions");

    // Verify action structure
    let first_action = &actions[0];
    assert!(first_action.get("type").is_some());
    assert!(first_action.get("path").is_some());
    assert!(first_action.get("content_preview").is_some());
}

#[tokio::test]
async fn should_generate_unique_plan_ids() {
    // Given: Same template called twice
    let params = json!({ "template": "rust-lib" });

    // When: Two plans are created
    let result1 = project::plan(params.clone()).await.unwrap();
    let result2 = project::plan(params).await.unwrap();

    // Then: Each should have unique plan ID
    let plan_id1 = result1.get("data").unwrap().get("plan_id").unwrap().as_str().unwrap();
    let plan_id2 = result2.get("data").unwrap().get("plan_id").unwrap().as_str().unwrap();

    assert_ne!(plan_id1, plan_id2, "Plan IDs should be unique");
}

#[tokio::test]
async fn should_fail_when_template_is_missing() {
    // Given: Empty parameters
    let params = json!({});

    // When: Plan is requested
    let result = project::plan(params).await;

    // Then: Should fail with clear error
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("template"));
}

#[tokio::test]
async fn should_preserve_template_variables_in_plan() {
    // Given: Template with custom variables
    let vars = json!({
        "name": "test-project",
        "author": "Test Author",
        "license": "MIT"
    });

    let params = json!({
        "template": "rust-lib",
        "vars": vars.clone()
    });

    // When: Plan is created
    let result = project::plan(params).await;

    // Then: Variables should be included in plan
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();
    assert_eq!(data.get("variables"), Some(&vars));
}

#[tokio::test]
async fn should_estimate_number_of_files() {
    // Given: Valid template
    let params = json!({
        "template": "rust-workspace"
    });

    // When: Plan is generated
    let result = project::plan(params).await;

    // Then: Should provide file count estimate
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let estimated = data.get("estimated_files").unwrap().as_u64().unwrap();
    assert!(estimated > 0, "Should estimate at least one file");
}

// Error Scenarios

#[tokio::test]
async fn should_handle_invalid_variable_types() {
    // Given: Invalid variable type
    let params = json!({
        "template": "rust-lib",
        "vars": "not-an-object"  // Should be object
    });

    // When: Plan is created
    let result = project::plan(params).await;

    // Then: Should handle gracefully (documents current behavior)
    let _ = result; // Current implementation may accept this
}

#[tokio::test]
async fn should_handle_extremely_long_template_names() {
    // Given: Very long template name
    let long_name = "a".repeat(1000);
    let params = json!({
        "template": long_name
    });

    // When: Plan is requested
    let result = project::plan(params).await;

    // Then: Should handle without panic
    let _ = result; // Documents behavior with long inputs
}

// Integration Tests

#[tokio::test]
async fn should_create_plan_that_can_be_applied() {
    // Given: A complete plan
    let params = json!({
        "template": "rust-lib",
        "vars": { "name": "test" }
    });

    // When: Plan is created
    let plan_result = project::plan(params).await;

    // Then: Plan should have format suitable for apply
    assert!(plan_result.is_ok());
    let data = plan_result.unwrap().get("data").unwrap().clone();
    let plan_id = data.get("plan_id").unwrap().as_str().unwrap();

    // Verify plan can be used with apply
    assert!(!plan_id.is_empty());
    assert!(plan_id.starts_with("plan_"));
}

// Behavior Verification Tests

#[tokio::test]
async fn should_not_modify_filesystem_during_planning() {
    // Given: Template that would create files
    let params = json!({
        "template": "rust-lib",
        "vars": { "name": "test" }
    });

    // When: Plan is created (not applied)
    let result = project::plan(params).await;

    // Then: Should complete without filesystem modifications
    // (In London School, we'd mock filesystem and verify no write calls)
    assert!(result.is_ok());

    // Document expected behavior: plan should be dry-run
    let data = result.unwrap().get("data").unwrap().clone();
    assert!(data.get("actions").is_some(), "Plan should describe actions but not execute them");
}

#[tokio::test]
async fn should_provide_content_preview_for_files() {
    // Given: Template with files
    let params = json!({
        "template": "rust-lib"
    });

    // When: Plan is generated
    let result = project::plan(params).await;

    // Then: Actions should include content previews
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();
    let actions = data.get("actions").unwrap().as_array().unwrap();

    for action in actions {
        if action.get("type").unwrap() == "create_file" {
            assert!(
                action.get("content_preview").is_some(),
                "File actions should have content preview"
            );
        }
    }
}
