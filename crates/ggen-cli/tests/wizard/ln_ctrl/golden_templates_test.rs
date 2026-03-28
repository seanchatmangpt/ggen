//! Tests for ln_ctrl golden template generation
//!
//! Verifies that golden templates generate valid, deterministic JSON output
//! suitable for test scenario definitions and expected outputs.

use serde_json::{json, Value};
use tera::{Context, Tera};

const TEMPLATES_DIR: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/templates/wizard/ln_ctrl/templates/goldens/"
);

/// Load Tera templates from goldens directory
fn load_templates() -> Result<Tera, Box<dyn std::error::Error>> {
    let pattern = format!("{}*.tera", TEMPLATES_DIR);
    let tera = Tera::new(&pattern)?;
    Ok(tera)
}

#[test]
fn test_scenario_template_renders() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let mut context = Context::new();

    context.insert("scenario_name", "test_beta_reduction");
    context.insert("initial_expression", "(λx.x+1) 5");
    context.insert("expected_steps", &3);
    context.insert("budget_steps", &10000);

    // Act
    let rendered = tera.render("scenario.json.tera", &context)?;

    // Assert - parse as JSON to verify validity
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["scenario_name"], "test_beta_reduction");
    assert_eq!(parsed["test_parameters"]["initial_expression"], "(λx.x+1) 5");
    assert_eq!(parsed["test_parameters"]["expected_steps"], 3);
    assert_eq!(parsed["normalization_rules"]["timestamp_placeholder"], "{{TIMESTAMP}}");

    Ok(())
}

#[test]
fn test_scenario_template_with_defaults() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange - use all defaults
    let tera = load_templates()?;
    let context = Context::new();

    // Act
    let rendered = tera.render("scenario.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["scenario_name"], "basic_beta_reduction");
    assert_eq!(parsed["scenario_type"], "positive");
    assert!(parsed["expected_outcomes"]["should_complete"].as_bool().unwrap());

    Ok(())
}

#[test]
fn test_expected_receipts_template() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let mut context = Context::new();

    let receipts = vec![
        json!({
            "operation": "reduce",
            "redex_type": "beta",
            "redex_expression": "(λx.x+1) 5",
            "frontier_terms": ["5+1"],
            "step_index": 0,
        }),
        json!({
            "operation": "evaluate",
            "redex_type": "delta",
            "redex_expression": "5+1",
            "frontier_terms": ["6"],
            "step_index": 1,
        }),
    ];

    context.insert("receipts", &receipts);
    context.insert("workflow_id", "550e8400-e29b-41d4-a716-446655440000");

    // Act
    let rendered = tera.render("expected_receipts.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["receipts"].as_array().unwrap().len(), 2);
    assert_eq!(parsed["receipts"][0]["operation"], "reduce");
    assert_eq!(parsed["receipts"][0]["timestamp"], "{{TIMESTAMP}}");
    assert_eq!(parsed["receipts"][0]["hash_chain"], "{{HASH}}");
    assert_eq!(parsed["chain_summary"]["total_receipts"], 2);

    Ok(())
}

#[test]
fn test_expected_receipts_placeholders() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let mut context = Context::new();

    let receipts = vec![json!({
        "operation": "reduce",
        "redex_type": "beta",
        "redex_expression": "(λx.x) 42",
        "frontier_terms": ["42"],
        "step_index": 0,
    })];

    context.insert("receipts", &receipts);

    // Act
    let rendered = tera.render("expected_receipts.json.tera", &context)?;

    // Assert - verify all placeholders are present
    assert!(rendered.contains("{{TIMESTAMP}}"));
    assert!(rendered.contains("{{HASH}}"));
    assert!(rendered.contains("{{SIGNATURE}}"));

    Ok(())
}

#[test]
fn test_expected_verdict_template_positive() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange - positive test case (all valid)
    let tera = load_templates()?;
    let mut context = Context::new();

    context.insert("valid", &true);
    context.insert("causal_chain_valid", &true);
    context.insert("hash_chain_valid", &true);
    context.insert("signatures_valid", &true);
    context.insert("total_checks", &11);
    context.insert("passed_checks", &11);
    context.insert("failed_checks", &0);

    // Act
    let rendered = tera.render("expected_verdict.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["verdict"]["valid"], true);
    assert_eq!(parsed["verdict"]["chain_integrity"]["causal_chain_valid"], true);
    assert_eq!(parsed["validation_results"]["passed_checks"], 11);
    assert_eq!(parsed["errors"].as_array().unwrap().len(), 0);

    Ok(())
}

#[test]
fn test_expected_verdict_template_with_errors() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange - test case with errors
    let tera = load_templates()?;
    let mut context = Context::new();

    context.insert("valid", &false);
    context.insert("total_checks", &11);
    context.insert("passed_checks", &9);
    context.insert("failed_checks", &2);

    let errors = vec![
        json!({
            "error_code": "E_HASH_MISMATCH",
            "error_message": "Hash chain break detected",
            "receipt_index": 2,
            "severity": "error",
        }),
    ];
    context.insert("errors", &errors);

    // Act
    let rendered = tera.render("expected_verdict.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["verdict"]["valid"], false);
    assert_eq!(parsed["validation_results"]["failed_checks"], 2);
    assert_eq!(parsed["errors"].as_array().unwrap().len(), 1);
    assert_eq!(parsed["errors"][0]["error_code"], "E_HASH_MISMATCH");

    Ok(())
}

#[test]
fn test_expected_divergence_template() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange - negative test case
    let tera = load_templates()?;
    let mut context = Context::new();

    context.insert("divergence_detected", &true);
    context.insert("divergence_type", "hash_chain_break");
    context.insert("detected_at_step", &2);
    context.insert("severity", "critical");

    let validation_failures = vec![json!({
        "failure_type": "hash_mismatch",
        "failure_message": "Expected hash does not match actual",
        "receipt_index": 2,
        "field_path": "hash_chain",
        "severity": "error",
    })];
    context.insert("validation_failures", &validation_failures);

    // Act
    let rendered = tera.render("expected_divergence.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["divergence_detected"], true);
    assert_eq!(parsed["divergence_type"], "hash_chain_break");
    assert_eq!(parsed["divergence_details"]["severity"], "critical");
    assert_eq!(parsed["validation_failures"].as_array().unwrap().len(), 1);
    assert_eq!(parsed["expected_behavior"]["should_reject"], true);

    Ok(())
}

#[test]
fn test_divergence_template_with_chain_breaks() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let mut context = Context::new();

    let chain_breaks = vec![json!({
        "break_location": 2,
        "break_type": "hash_chain",
        "parent_receipt": 1,
        "child_receipt": 2,
        "description": "Child receipt hash does not match parent",
    })];
    context.insert("chain_breaks", &chain_breaks);

    // Act
    let rendered = tera.render("expected_divergence.json.tera", &context)?;

    // Assert
    let parsed: Value = serde_json::from_str(&rendered)?;
    assert_eq!(parsed["chain_analysis"]["chain_breaks"].as_array().unwrap().len(), 1);
    assert_eq!(parsed["chain_analysis"]["chain_breaks"][0]["break_type"], "hash_chain");

    Ok(())
}

#[test]
fn test_all_templates_render_valid_json() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let context = Context::new(); // Use defaults

    let templates = vec![
        "scenario.json.tera",
        "expected_receipts.json.tera",
        "expected_verdict.json.tera",
        "expected_divergence.json.tera",
    ];

    // Act & Assert - all should render and parse as valid JSON
    for template_name in templates {
        let rendered = tera.render(template_name, &context)?;
        let parsed: Value = serde_json::from_str(&rendered)
            .map_err(|e| format!("Template {} failed to parse: {}", template_name, e))?;

        // Verify all have metadata
        assert!(
            parsed.get("metadata").is_some(),
            "Template {} missing metadata",
            template_name
        );
    }

    Ok(())
}

#[test]
fn test_deterministic_output() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let tera = load_templates()?;
    let mut context = Context::new();
    context.insert("scenario_name", "determinism_test");

    // Act - render twice
    let rendered1 = tera.render("scenario.json.tera", &context)?;
    let rendered2 = tera.render("scenario.json.tera", &context)?;

    // Assert - should be identical
    assert_eq!(rendered1, rendered2, "Template output is not deterministic");

    Ok(())
}
