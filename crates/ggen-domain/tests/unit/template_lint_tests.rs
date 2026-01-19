//! Unit tests for template linting
//! Tests individual lint rules and helper functions

use chicago_tdd_tools::prelude::*;
use ggen_domain::template::lint::*;
use ggen_utils::error::Result;
use std::fs;
use std::path::Path;

/// Helper to create test fixture path
fn fixture_path(name: &str) -> String {
    format!("tests/fixtures/{}", name)
}

test!(test_lint_report_has_errors, {
    // Arrange & Act
    let report = LintReport {
        errors: vec![LintError {
            line: Some(1),
            message: "Test error".to_string(),
        }],
        warnings: vec![],
    };

    // Assert
    assert!(report.has_errors());
    assert!(!report.has_warnings());
});

test!(test_lint_report_has_warnings, {
    // Arrange & Act
    let report = LintReport {
        errors: vec![],
        warnings: vec![LintWarning {
            line: Some(1),
            message: "Test warning".to_string(),
        }],
    };

    // Assert
    assert!(!report.has_errors());
    assert!(report.has_warnings());
});

test!(test_lint_report_empty, {
    // Arrange & Act
    let report = LintReport {
        errors: vec![],
        warnings: vec![],
    };

    // Assert
    assert!(!report.has_errors());
    assert!(!report.has_warnings());
});

test!(test_lint_valid_template, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("valid_template.toml"), &options)?;

    // Assert
    assert_eq!(
        result.errors.len(),
        0,
        "Valid template should have no errors"
    );
});

test!(test_lint_invalid_template, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Assert
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
});

test!(test_lint_missing_file, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template("nonexistent.toml", &options);

    // Assert
    assert_ok!(&result, "Should return Ok with error report");
    let report = result.unwrap();
    assert!(report.has_errors(), "Should have errors for missing file");
    assert!(report.errors[0].message.contains("not found"));
});

test!(test_lint_no_frontmatter, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("no_frontmatter.toml"), &options)?;

    // Assert - Should warn about missing frontmatter
    let has_frontmatter_warning = result
        .warnings
        .iter()
        .any(|w| w.message.contains("frontmatter"));
    assert!(
        has_frontmatter_warning,
        "Should warn about missing frontmatter"
    );
});

test!(test_lint_incomplete_frontmatter, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("incomplete_frontmatter.toml"), &options)?;

    // Assert - Should warn about missing vars field
    let has_vars_warning = result.warnings.iter().any(|w| w.message.contains("vars"));
    assert!(has_vars_warning, "Should warn about missing vars field");
});

test!(test_lint_sparql_queries, {
    // Arrange
    let options = LintOptions {
        check_sparql: true,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("template_with_sparql.toml"), &options)?;

    // Assert - Should detect SPARQL issues when check_sparql is enabled
    assert!(result.has_warnings(), "Should have SPARQL warnings");

    // Should warn about SELECT without WHERE
    let has_where_warning = result.warnings.iter().any(|w| w.message.contains("WHERE"));
    assert!(
        has_where_warning,
        "Should warn about SELECT without WHERE clause"
    );
});

test!(test_lint_rdf_schema, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: true,
    };

    // Act
    let result = lint_template(&fixture_path("template_with_rdf.toml"), &options)?;

    // Assert - RDF template with proper format should pass
    assert_eq!(result.errors.len(), 0, "Valid RDF should have no errors");
});

test!(test_lint_gpack_templates_not_supported, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template("gpack:some-package", &options);

    // Assert
    assert_err!(&result, "gpack templates should return error");
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("gpack"),
        "Error should mention gpack"
    );
});

test!(test_lint_options_defaults, {
    // Arrange & Act
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Assert
    assert!(!options.check_sparql);
    assert!(!options.check_schema);
});

test!(test_lint_error_line_numbers, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Assert - Errors should have line numbers
    let has_line_numbers = result.errors.iter().any(|e| e.line.is_some());
    assert!(has_line_numbers, "Errors should include line numbers");
});

test!(test_unclosed_variable_detection, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Assert - Should detect "{{ name" without closing "}}"
    let unclosed_count = result
        .errors
        .iter()
        .filter(|e| e.message.contains("Unclosed"))
        .count();
    assert!(unclosed_count > 0, "Should detect unclosed variables");
});

test!(test_empty_variable_detection, {
    // Arrange
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let result = lint_template(&fixture_path("invalid_template.toml"), &options)?;

    // Assert - Should detect "{{ }}" or "{{}}"
    let empty_count = result
        .warnings
        .iter()
        .filter(|w| w.message.contains("Empty"))
        .count();
    assert!(empty_count > 0, "Should detect empty variables");
});

async_test!(test_execute_lint_async, async {
    // Arrange
    let input = LintInput {
        template: fixture_path("valid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let output = execute_lint(input).await?;

    // Assert
    assert_eq!(output.errors_found, 0);
    assert!(output.template_path.contains("valid_template.toml"));
});

async_test!(test_execute_lint_with_errors, async {
    // Arrange
    let input = LintInput {
        template: fixture_path("invalid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    // Act
    let output = execute_lint(input).await?;

    // Assert
    assert!(output.errors_found > 0);
    assert!(output.warnings_found > 0);
});
