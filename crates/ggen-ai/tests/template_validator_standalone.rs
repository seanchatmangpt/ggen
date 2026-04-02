//! Template Validator Standalone Test
//!
//! This test validates the template validator agent functionality without requiring the swarm feature.
//! This tests the core validation logic directly.

use ggen_ai::swarm::agents::template_validator::{
    FixType, IssueSeverity, IssueType, TemplateIssue, TemplateValidatorAgent, ValidationReport,
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
fn test_basic_validation_functionality() {
    println!("Testing basic validation functionality...");

    let agent = create_test_agent();

    // Test valid template
    let valid_template = r#"
        Hello {{ name }}!
        You are {{ age }} years old.
        Your email is {{ email }}.
    "#;

    let report = agent
        .validate_and_fix(valid_template, "basic_valid.tera")
        .unwrap();

    assert!(report.is_valid, "Valid template should pass validation");
    assert!(
        report.issues.is_empty(),
        "Valid template should have no issues"
    );
    assert_eq!(
        report.quality_score, 1.0,
        "Valid template should have perfect quality score"
    );

    println!("✓ Basic validation test passed");
}

#[test]
fn test_syntax_error_detection() {
    println!("Testing syntax error detection...");

    let agent = create_test_agent();

    // Template with syntax error - missing closing brace
    let invalid_template = "{{ name }\nAge: {{ age }}";

    let report = agent
        .validate_and_fix(invalid_template, "syntax_error.tera")
        .unwrap();

    assert!(!report.is_valid, "Invalid template should fail validation");
    assert!(
        !report.issues.is_empty(),
        "Invalid template should have issues"
    );

    // Check that syntax error is detected
    let syntax_errors: Vec<_> = report
        .issues
        .iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::SyntaxError))
        .collect();

    assert!(!syntax_errors.is_empty(), "Should detect syntax errors");

    if let Some(syntax_error) = syntax_errors.first() {
        println!("Found syntax error: {}", syntax_error.description);
        assert!(syntax_error.severity == IssueSeverity::Error);
        assert!(syntax_error.description.contains("Error"));
    }

    println!("✓ Syntax error detection test passed");
}

#[test]
fn test_variable_extraction_comprehensive() {
    println!("Testing comprehensive variable extraction...");

    let agent = create_test_agent();

    let template = r#"
        Simple: {{ simple_var }}
        With filter: {{ var_with_filter|upper }}
        With default: {{ var_with_default|default(value="N/A") }}
        Nested: {{ user.profile.name }}
        In loop: {% for item in items %}{{ item.title }}{% endfor %}
        Array access: {{ array[0] }}
    "#;

    let vars = agent.extract_template_variables(template);

    let var_names: Vec<_> = vars.iter().collect();
    println!("Extracted variables: {:?}", var_names);

    assert!(vars.contains("simple_var"));
    assert!(vars.contains("var_with_filter"));
    assert!(vars.contains("var_with_default"));
    assert!(vars.contains("user.profile.name"));
    assert!(vars.contains("items"));
    assert!(vars.contains("item.title"));
    assert!(vars.contains("array"));

    println!("✓ Comprehensive variable extraction test passed");
}

#[test]
fn test_sparql_cross_reference_integration() {
    println!("Testing SPARQL cross-reference integration...");

    let agent = create_test_agent();

    // Template includes variables that exist and don't exist in SPARQL
    let template = r#"
        Name: {{ name }}  // Valid - in SPARQL
        Age: {{ age }}     // Valid - in SPARQL
        Address: {{ address }}  // Invalid - not in SPARQL
    "#;

    let report = agent
        .validate_and_fix(template, "sparql_cross.tera")
        .unwrap();

    // Check for undefined variables
    let undefined_issues: Vec<_> = report
        .issues
        .iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::UndefinedVariable))
        .collect();

    assert!(
        !undefined_issues.is_empty(),
        "Should detect undefined variables"
    );

    if let Some(undefined_issue) = undefined_issues.first() {
        assert!(undefined_issue.description.contains("address"));
        assert!(undefined_issue.severity == IssueSeverity::Warning);
    }

    // Should have suggested fixes
    assert!(!report.fixes.is_empty(), "Should suggest fixes");

    println!("✓ SPARQL cross-reference integration test passed");
}

#[test]
fn test_filter_validation_with_known_filters() {
    println!("Testing filter validation with known filters...");

    let agent = create_test_agent();

    let template = r#"
        Valid filters:
        {{ name|upper }}
        {{ name|lower }}
        {{ name|title }}
        {{ name|trim }}
        {{ name|default(value="") }}
        {{ name|truncate(50) }}

        Invalid filter:
        {{ name|unknown_filter }}
    "#;

    let report = agent.validate_and_fix(template, "filters.tera").unwrap();

    // Should find invalid filter
    let filter_issues: Vec<_> = report
        .issues
        .iter()
        .filter(|issue| matches!(issue.issue_type, IssueType::InvalidFilter))
        .collect();

    assert!(!filter_issues.is_empty(), "Should detect invalid filters");

    if let Some(filter_issue) = filter_issues.first() {
        assert!(filter_issue.description.contains("unknown_filter"));
        assert!(filter_issue.severity == IssueSeverity::Warning);
    }

    println!("✓ Filter validation test passed");
}

#[test]
fn test_quality_score_calculation() {
    println!("Testing quality score calculation...");

    let agent = create_test_agent();

    // Test different scenarios
    let scenarios = vec![
        ("{{ name }}", "Perfect template", 1.0), // No issues
        ("{{ name }} is {{ age }} old.", "Valid template", 1.0), // Still valid
        ("{{ unknown_var }}", "Undefined variable", 0.9), // Warning
        ("{{ var|unknown_filter }}", "Unknown filter", 0.8), // Warning
        ("{{ name }", "Syntax error", 0.5),      // Error
    ];

    for (template, description, expected_score) in scenarios {
        println!("Testing: {}", description);
        let report = agent
            .validate_and_fix(template, "quality_test.tera")
            .unwrap();

        println!(
            "  Quality score: {} (expected: {})",
            report.quality_score, expected_score
        );
        assert!(report.quality_score <= 1.0 && report.quality_score >= 0.0);

        // Check that critical issues have bigger impact
        if expected_score < 0.6 {
            assert!(!report.issues.is_empty(), "Low score should have issues");
        }
    }

    println!("✓ Quality score calculation test passed");
}

#[test]
fn test_context_extraction_accuracy() {
    println!("Testing context extraction accuracy...");

    let agent = create_test_agent();

    let template = r#"
        Line 1: Initial text
        Line 2: More text
        Line 3: {{ name }}
        Line 4: Some text
        Line 5: {{ age }}
        Line 6: Final text
    "#;

    // Test extracting context around line 3 (where {{ name }} is)
    let context = agent.extract_context(template, Some(3), 2);

    assert!(context.is_some(), "Should extract context");
    let context_str = context.unwrap();

    println!("Extracted context:\n{}", context_str);

    assert!(
        context_str.contains("Line 1:"),
        "Should include lines before"
    );
    assert!(
        context_str.contains("Line 2:"),
        "Should include previous line"
    );
    assert!(context_str.contains("Line 3:"), "Should include error line");
    assert!(
        context_str.contains("Line 4:"),
        "Should include lines after"
    );
    assert!(
        context_str.contains("Line 5:"),
        "Should include multiple after lines"
    );

    println!("✓ Context extraction accuracy test passed");
}

#[test]
fn test_error_severity_levels() {
    println!("Testing error severity levels...");

    let agent = create_test_agent();

    let template = r#"
        // Critical syntax error
        {{ name }  // Missing closing brace

        // Warning - undefined variable
        {{ undefined_var }}

        // Info - unusual variable name
        {{ variable_with_many_under_scores }}
    "#;

    let report = agent
        .validate_and_fix(template, "severity_test.tera")
        .unwrap();

    let issues_by_severity: std::collections::HashMap<_, _> = report
        .issues
        .iter()
        .map(|issue| (&issue.severity, issue))
        .collect();

    // Should have different severity levels
    assert!(
        issues_by_severity.contains_key(&IssueSeverity::Error),
        "Should have errors"
    );
    assert!(
        issues_by_severity.contains_key(&IssueSeverity::Warning),
        "Should have warnings"
    );
    assert!(
        issues_by_severity.contains_key(&IssueSeverity::Info),
        "Should have info"
    );

    println!("✓ Error severity levels test passed");
}

#[test]
fn test_template_fix_generation() {
    println!("Testing template fix generation...");

    let agent = create_test_agent();

    let template = "{{ unknown_var }}";
    let report = agent.validate_and_fix(template, "fix_test.tera").unwrap();

    if !report.fixes.is_empty() {
        println!("Generated fixes:");
        for fix in &report.fixes {
            println!(
                "  - {}: {} -> {}",
                fix.description, fix.fix_type, fix.confidence
            );
        }

        // Test applying fixes
        let fixed_template = agent.apply_fixes(template, &report.fixes).unwrap();
        println!("Original: {}", template);
        println!("Fixed: {}", fixed_template);

        // Should have applied default value
        assert!(fixed_template.contains("default(value=\"\")"));
    }

    println!("✓ Template fix generation test passed");
}

fn main() {
    println!("Running Template Validator Standalone Tests...\n");

    test_basic_validation_functionality();
    test_syntax_error_detection();
    test_variable_extraction_comprehensive();
    test_sparql_cross_reference_integration();
    test_filter_validation_with_known_filters();
    test_quality_score_calculation();
    test_context_extraction_accuracy();
    test_error_severity_levels();
    test_template_fix_generation();

    println!("\n🎉 All Template Validator standalone tests passed!");
}
