//! Template Validator Agent Integration Test
//!
//! This test validates the template validator agent functionality by testing:
//! - Template syntax validation
//! - Variable extraction and validation
//! - SPARQL integration
//! - Error handling and fix suggestions

use ggen_ai::swarm::agents::template_validator::{
    TemplateValidatorAgent, IssueType, IssueSeverity, FixType, ValidationReport
};

fn create_test_agent() -> TemplateValidatorAgent {
    // Create agent with dummy SPARQL results for testing
    let sparql_results = Some(serde_json::json!({
        "head": {
            "vars": ["name", "age", "email"]
        },
        "results": {
            "bindings": [
                {
                    "name": {"type": "literal", "value": "Alice"},
                    "age": {"type": "literal", "value": "30"},
                    "email": {"type": "literal", "value": "alice@example.com"}
                }
            ]
        }
    }));

    TemplateValidatorAgent::new(sparql_results, true) // dry run mode
}

#[test]
fn test_template_syntax_validation() {
    println!("Testing template syntax validation...");

    let agent = create_test_agent();

    // Test valid template
    let valid_template = r#"
        {{ name }}
        {{ age|default(value=0) }}
        {{ email|upper }}
    "#;

    let report = agent.validate_and_fix(valid_template, "valid.tera").unwrap();

    assert!(report.is_valid, "Valid template should pass validation");
    assert!(report.issues.is_empty(), "Valid template should have no issues");
    assert_eq!(report.quality_score, 1.0, "Valid template should have perfect quality score");

    println!("✓ Valid template test passed");
}

#[test]
fn test_template_syntax_errors() {
    println!("Testing template syntax error detection...");

    let agent = create_test_agent();

    // Test template with syntax error (missing closing brace)
    let invalid_template = r#"
        {{ name }
        {{ age|default(value=0) }}
    "#;

    let report = agent.validate_and_fix(invalid_template, "invalid.tera").unwrap();

    assert!(!report.is_valid, "Invalid template should fail validation");
    assert!(!report.issues.is_empty(), "Invalid template should have issues");

    // Check that syntax error is detected
    let syntax_errors: Vec<_> = report.issues.iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::SyntaxError))
        .collect();
    assert!(!syntax_errors.is_empty(), "Should detect syntax errors");

    println!("✓ Syntax error detection test passed");
}

#[test]
fn test_variable_extraction() {
    println!("Testing variable extraction...");

    let agent = create_test_agent();

    let template = r#"
        {{ user.name }}
        {{ user.age|default(value=25) }}
        {{ profile.email|lower }}
        {% for item in items %}
            {{ item.title }}
        {% endfor %}
    "#;

    let vars = agent.extract_template_variables(template);

    assert!(vars.contains("user.name"));
    assert!(vars.contains("user.age"));
    assert!(vars.contains("profile.email"));
    assert!(vars.contains("items"));
    assert!(vars.contains("item.title"));

    println!("✓ Variable extraction test passed");
}

#[test]
fn test_sparql_variable_cross_reference() {
    println!("Testing SPARQL variable cross-reference...");

    let agent = create_test_agent();

    let template = r#"
        {{ name }}
        {{ age }}
        {{ address }}  // This should be flagged as undefined
    "#;

    let report = agent.validate_and_fix(template, "cross_ref.tera").unwrap();

    // Should detect that 'address' is not in SPARQL results
    let undefined_vars: Vec<_> = report.issues.iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::UndefinedVariable))
        .collect();

    assert!(!undefined_vars.is_empty(), "Should detect undefined variables");
    assert!(undefined_vars.iter().any(|issue| issue.description.contains("address")));

    // Should suggest fixes
    let fixes: Vec<_> = report.fixes.iter()
        .filter(|fix| matches!(fix.fix_type, FixType::MarkOptional))
        .collect();

    assert!(!fixes.is_empty(), "Should suggest fixes for undefined variables");

    println!("✓ SPARQL cross-reference test passed");
}

#[test]
fn test_filter_validation() {
    println!("Testing filter validation...");

    let agent = create_test_agent();

    let template = r#"
        {{ name|upper }}
        {{ age|default(value=0) }}
        {{ email|unknown_filter }}  // This should be flagged
    "#;

    let report = agent.validate_and_fix(template, "filters.tera").unwrap();

    // Should detect unknown filter
    let filter_errors: Vec<_> = report.issues.iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::InvalidFilter))
        .collect();

    assert!(!filter_errors.is_empty(), "Should detect invalid filters");
    assert!(filter_errors.iter().any(|issue| issue.description.contains("unknown_filter")));

    println!("✓ Filter validation test passed");
}

#[test]
fn test_quality_scoring() {
    println!("Testing quality scoring...");

    let agent = create_test_agent();

    // Perfect template
    let perfect_template = "{{ name }} is {{ age }} years old.";
    let perfect_report = agent.validate_and_fix(perfect_template, "perfect.tera").unwrap();
    assert_eq!(perfect_report.quality_score, 1.0);

    // Template with issues
    let problematic_template = "{{ name }} is {{ ag }} years old."; // typo 'ag' instead of 'age'
    let problematic_report = agent.validate_and_fix(problematic_template, "problematic.tera").unwrap();
    assert!(problematic_report.quality_score < 1.0);

    println!("✓ Quality scoring test passed");
}

#[test]
fn test_fix_application() {
    println!("Testing fix application...");

    let agent = create_test_agent();

    let template = "{{ unknown_var }}";
    let report = agent.validate_and_fix(template, "fix_test.tera").unwrap();

    if !report.fixes.is_empty() {
        // Test applying fixes
        let fixed_template = agent.apply_fixes(template, &report.fixes).unwrap();

        // Fixed template should have default value
        assert!(fixed_template.contains("default(value=\"\")"));
    }

    println!("✓ Fix application test passed");
}

#[test]
fn test_error_handling() {
    println!("Testing error handling...");

    let agent = create_test_agent();

    // Test with empty template
    let empty_template = "";
    let report = agent.validate_and_fix(empty_template, "empty.tera").unwrap();

    // Should handle gracefully
    assert!(report.issues.is_empty()); // Empty template has no syntax errors
    assert!(report.is_valid); // Empty template is considered valid

    // Test with template that has only whitespace
    let whitespace_template = "   \n\t  \n  ";
    let whitespace_report = agent.validate_and_fix(whitespace_template, "whitespace.tera").unwrap();

    assert!(whitespace_report.issues.is_empty());
    assert!(whitespace_report.is_valid);

    println!("✓ Error handling test passed");
}

#[test]
fn test_context_extraction() {
    println!("Testing context extraction...");

    let agent = create_test_agent();

    let template = r#"
        User: {{ name }}
        Age: {{ age }}
        Email: {{ email|upper }}
    "#;

    // Simulate a syntax error on line 3
    let line_number = 3;
    let context = agent.extract_context(template, Some(line_number), 2);

    assert!(context.is_some(), "Should extract context");
    let context_str = context.unwrap();
    assert!(context_str.contains("Age:"), "Context should include relevant lines");
    assert!(context_str.contains(line_number.to_string()), "Context should highlight error line");

    println!("✓ Context extraction test passed");
}

// Integration test with multiple validation steps
#[test]
fn test_comprehensive_validation_workflow() {
    println!("Testing comprehensive validation workflow...");

    let agent = create_test_agent();

    let template = r#"
        {{ name }}
        {{ age|default(value=0) }}
        {{ unknown_field|upper }}  // Unknown field with invalid filter
        {{ user.email|unknown_filter }}  // Unknown filter
    "#;

    let report = agent.validate_and_fix(template, "comprehensive.tera").unwrap();

    // Should detect multiple types of issues
    let issues_by_type: std::collections::HashMap<_, _> = report.issues.iter()
        .map(|issue| (&issue.issue_type, issue))
        .collect();

    // Should have undefined variable
    assert!(issues_by_type.contains_key(&IssueType::UndefinedVariable));

    // Should have invalid filter
    assert!(issues_by_type.contains_key(&IssueType::InvalidFilter));

    // Quality should be reduced due to issues
    assert!(report.quality_score < 1.0);

    println!("✓ Comprehensive validation workflow test passed");
}

fn main() {
    println!("Running Template Validator Agent Integration Tests...\n");

    // Run all tests
    test_template_syntax_validation();
    test_template_syntax_errors();
    test_variable_extraction();
    test_sparql_variable_cross_reference();
    test_filter_validation();
    test_quality_scoring();
    test_fix_application();
    test_error_handling();
    test_context_extraction();
    test_comprehensive_validation_workflow();

    println!("\n🎉 All Template Validator Agent integration tests passed!");
}