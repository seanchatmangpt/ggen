//! Unit tests for template linting
//! Tests individual lint rules and helper functions

use ggen_domain::template::lint::*;
use ggen_utils::error::Result;

/// Helper to create test fixture path
fn fixture_path(name: &str) -> String {
    format!("tests/fixtures/{}", name)
}

#[test]
fn test_lint_report_has_errors() {
    let report = LintReport {
        errors: vec![LintError {
            line: Some(1),
            message: "Test error".to_string(),
        }],
        warnings: vec![],
    };
    assert!(report.has_errors());
    assert!(!report.has_warnings());
}

#[test]
fn test_lint_report_has_warnings() {
    let report = LintReport {
        errors: vec![],
        warnings: vec![LintWarning {
            line: Some(1),
            message: "Test warning".to_string(),
        }],
    };
    assert!(!report.has_errors());
    assert!(report.has_warnings());
}

#[test]
fn test_lint_report_empty() {
    let report = LintReport {
        errors: vec![],
        warnings: vec![],
    };
    assert!(!report.has_errors());
    assert!(!report.has_warnings());
}

#[test]
fn test_lint_valid_template() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("valid_template.toml"), &options)?;
    assert_eq!(
        result.errors.len(),
        0,
        "Valid template should have no errors"
    );
    Ok(())
}

#[test]
fn test_lint_invalid_template() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;
    assert!(result.has_errors(), "Invalid template should have errors");
    assert!(
        result.has_warnings(),
        "Invalid template should have warnings"
    );

    // Should detect unclosed variables
    let has_unclosed = result.errors.iter().any(|e| e.message.contains("Unclosed"));
    assert!(has_unclosed, "Should detect unclosed template variable");

    // Should detect empty variables
    let has_empty = result.warnings.iter().any(|w| w.message.contains("Empty"));
    assert!(has_empty, "Should detect empty template variable");

    Ok(())
}

#[test]
fn test_lint_missing_file() {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template("nonexistent.toml", &options);
    assert!(result.is_ok(), "Should return Ok with error report");

    let report = result.unwrap();
    assert!(report.has_errors(), "Should have errors for missing file");
    assert!(report.errors[0].message.contains("not found"));
}

#[test]
fn test_lint_no_frontmatter() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("no_frontmatter.toml"), &options)?;

    // Should warn about missing frontmatter
    let has_frontmatter_warning = result
        .warnings
        .iter()
        .any(|w| w.message.contains("frontmatter"));
    assert!(
        has_frontmatter_warning,
        "Should warn about missing frontmatter"
    );

    Ok(())
}

#[test]
fn test_lint_incomplete_frontmatter() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("incomplete_frontmatter.toml"), &options)?;

    // Should warn about missing vars field
    let has_vars_warning = result.warnings.iter().any(|w| w.message.contains("vars"));
    assert!(has_vars_warning, "Should warn about missing vars field");

    Ok(())
}

#[test]
fn test_lint_sparql_queries() -> Result<()> {
    let options = LintOptions {
        check_sparql: true,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("template_with_sparql.toml"), &options)?;

    // Should detect SPARQL issues when check_sparql is enabled
    assert!(result.has_warnings(), "Should have SPARQL warnings");

    // Should warn about SELECT without WHERE
    let has_where_warning = result.warnings.iter().any(|w| w.message.contains("WHERE"));
    assert!(
        has_where_warning,
        "Should warn about SELECT without WHERE clause"
    );

    Ok(())
}

#[test]
fn test_lint_rdf_schema() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: true,
    };

    let result = lint_template(&fixture_path("template_with_rdf.toml"), &options)?;

    // RDF template with proper format should pass
    assert_eq!(result.errors.len(), 0, "Valid RDF should have no errors");

    Ok(())
}

#[test]
fn test_lint_gpack_templates_not_supported() {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template("gpack:some-package", &options);
    assert!(result.is_err(), "gpack templates should return error");

    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("gpack"),
        "Error should mention gpack"
    );
}

#[test]
fn test_lint_options_defaults() {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    assert!(!options.check_sparql);
    assert!(!options.check_schema);
}

#[test]
fn test_lint_error_line_numbers() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Errors should have line numbers
    let has_line_numbers = result.errors.iter().any(|e| e.line.is_some());
    assert!(has_line_numbers, "Errors should include line numbers");

    Ok(())
}

#[test]
fn test_unclosed_variable_detection() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Should detect "{{ name" without closing "}}"
    let unclosed_count = result
        .errors
        .iter()
        .filter(|e| e.message.contains("Unclosed"))
        .count();
    assert!(unclosed_count > 0, "Should detect unclosed variables");

    Ok(())
}

#[test]
fn test_empty_variable_detection() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Should detect "{{ }}" or "{{}}"
    let empty_count = result
        .warnings
        .iter()
        .filter(|w| w.message.contains("Empty"))
        .count();
    assert!(empty_count > 0, "Should detect empty variables");

    Ok(())
}

#[tokio::test]
async fn test_execute_lint_async() -> Result<()> {
    let input = LintInput {
        template: fixture_path("valid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    let output = execute_lint(input).await?;
    assert_eq!(output.errors_found, 0);
    assert!(output.template_path.contains("valid_template.toml"));

    Ok(())
}

#[tokio::test]
async fn test_execute_lint_with_errors() -> Result<()> {
    let input = LintInput {
        template: fixture_path("invalid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    let output = execute_lint(input).await?;
    assert!(output.errors_found > 0);
    assert!(output.warnings_found > 0);

    Ok(())
}
