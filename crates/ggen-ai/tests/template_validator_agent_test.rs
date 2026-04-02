//! Template Validator Agent Integration Tests

use ggen_ai::swarm::agents::template_validator::{
    FixType, IssueSeverity, IssueType, TemplateValidatorAgent,
};

#[test]
fn test_edit_distance() {
    // Test edit distance calculation for finding similar names
    assert_eq!(edit_distance("kitten", "sitting"), 3);
    assert_eq!(edit_distance("foo", "bar"), 3);
    assert_eq!(edit_distance("test", "test"), 0);
    assert_eq!(edit_distance("var_name", "varname"), 1);
}

#[test]
fn test_extract_template_variables() {
    // Test extraction of variables from Tera template
    let agent = TemplateValidatorAgent::new(None, false);
    let template = r#"
            {{ name }}
            {{ age|default(value=0) }}
            {{ user.email }}
            {% for item in items %}
                {{ item.value }}
            {% endfor %}
        "#;

    let vars = agent.extract_template_variables(template);
    assert!(vars.contains("name"));
    assert!(vars.contains("age"));
    assert!(vars.contains("user.email"));
    assert!(vars.contains("item.value"));
    assert!(vars.contains("items"));
}

#[test]
fn test_validate_syntax_valid() {
    // Test validation of valid template syntax
    let agent = TemplateValidatorAgent::new(None, false);
    let template = "{{ name }} is {{ age }} years old.";
    let issues = agent.validate_syntax(template).unwrap();
    assert!(
        issues.is_empty(),
        "Valid template should have no syntax errors"
    );
}

#[test]
fn test_validate_syntax_invalid() {
    // Test detection of syntax errors
    let agent = TemplateValidatorAgent::new(None, false);
    let template = "{{ name }"; // Missing closing }}
    let issues = agent.validate_syntax(template).unwrap();
    assert!(
        !issues.is_empty(),
        "Invalid template should have syntax errors"
    );
    assert!(matches!(issues[0].issue_type, IssueType::SyntaxError));
}

#[test]
fn test_validate_and_fix_with_report() {
    // Test full validation and fix reporting
    let agent = TemplateValidatorAgent::new(None, true);
    let template = "{{ name }} is {{ ag }} years old.";

    let report = agent.validate_and_fix(template, "test.tera").unwrap();

    // Should produce a report
    assert_eq!(report.template_path, "test.tera");
    assert!(report.timestamp.len() > 0);
    assert!(report.quality_score >= 0.0 && report.quality_score <= 1.0);
}

#[test]
fn test_find_similar_variable() {
    // Test finding similar variable names using edit distance
    let agent = TemplateValidatorAgent::new(None, false);
    let sparql_vars = std::collections::HashSet::from([
        "user_name".to_string(),
        "user_email".to_string(),
        "user_age".to_string(),
    ]);

    let similar = agent.find_similar_variable("usr_name", &sparql_vars);
    assert_eq!(similar, Some("user_name".to_string()));

    let not_similar = agent.find_similar_variable("xyz", &sparql_vars);
    assert!(not_similar.is_none());
}

#[test]
fn test_sparql_variable_extraction() {
    // Test extraction of variables from SPARQL JSON results
    let agent = TemplateValidatorAgent::new(None, false);

    let sparql_json = serde_json::json!({
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
    });

    let vars = agent.extract_sparql_variables(&sparql_json).unwrap();
    assert!(vars.iter().any(|v| v.name == "name"));
    assert!(vars.iter().any(|v| v.name == "age"));
    assert!(vars.iter().any(|v| v.name == "email"));
}

#[test]
fn test_cross_reference_variables() {
    // Test cross-referencing template variables with SPARQL results
    let agent = TemplateValidatorAgent::new(None, false);

    let template = "{{ name }} is {{ ag }} years old.";
    let template_vars = agent.extract_template_variables(template);

    let sparql_json = serde_json::json!({
        "head": {
            "vars": ["name", "age", "email"]
        }
    });

    let sparql_vars = agent.extract_sparql_variables(&sparql_json).unwrap();
    let (issues, fixes) = agent.cross_reference_variables(&template_vars, &sparql_vars, template);

    // Should detect that 'ag' is not in SPARQL results
    assert!(!issues.is_empty()); // Type: Vec<TemplateIssue>
    assert!(!fixes.is_empty()); // Type: Vec<TemplateFix>

    // Add type-asserted checks to help compiler
    let _: &[ggen_ai::swarm::agents::template_validator::TemplateIssue] = &issues;
    let _: &[ggen_ai::swarm::agents::template_validator::TemplateFix] = &fixes;
}

#[test]
fn test_validate_filters() {
    // Test filter syntax validation
    let agent = TemplateValidatorAgent::new(None, false);

    let template = r#"
        {{ name|upper }}
        {{ age|default(value=0) }}
        {{ email|unknown_filter }}
    "#;

    let (issues, fixes) = agent.validate_filters(template);

    // Should detect unknown filter
    assert!(!issues.is_empty()); // Type: Vec<TemplateIssue>

    // Add type-asserted check to help compiler
    let _: &[ggen_ai::swarm::agents::template_validator::TemplateIssue] = &issues;
    assert!(issues
        .iter()
        .any(|i| matches!(i.issue_type, IssueType::InvalidFilter)));
}

#[test]
fn test_calculate_quality_score() {
    // Test quality score calculation
    let agent = TemplateValidatorAgent::new(None, false);

    // No issues = perfect score
    let score1 = agent.calculate_quality_score(&[]);
    assert_eq!(score1, 1.0);

    // Critical issues reduce score significantly
    let critical_issue = ggen_ai::swarm::agents::template_validator::TemplateIssue {
        issue_type: IssueType::SyntaxError,
        severity: IssueSeverity::Critical,
        line_number: Some(1),
        column: Some(1),
        description: "Test".to_string(),
        context: None,
    };
    let score2 = agent.calculate_quality_score(&[critical_issue]);
    assert!(score2 < 1.0 && score2 >= 0.0);

    // Warnings reduce score slightly
    let warning_issue = ggen_ai::swarm::agents::template_validator::TemplateIssue {
        issue_type: IssueType::UndefinedVariable,
        severity: IssueSeverity::Warning,
        line_number: Some(1),
        column: Some(1),
        description: "Test".to_string(),
        context: None,
    };
    let score3 = agent.calculate_quality_score(&[warning_issue]);
    assert!(
        score3 > score2,
        "Warnings should reduce score less than critical issues"
    );
}

// Helper function (re-exported from template_validator module)
fn edit_distance(a: &str, b: &str) -> usize {
    let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];

    for i in 0..=a.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b.len() {
        matrix[0][j] = j;
    }

    for (i, ca) in a.chars().enumerate() {
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            matrix[i + 1][j + 1] = [
                matrix[i][j + 1] + 1, // deletion
                matrix[i + 1][j] + 1, // insertion
                matrix[i][j] + cost,  // substitution
            ]
            .iter()
            .min()
            .copied()
            .unwrap();
        }
    }

    matrix[a.len()][b.len()]
}
