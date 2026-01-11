//! Integration tests for template linting
//! Tests end-to-end linting workflows with real files

use ggen_domain::template::lint::*;
use ggen_utils::error::Result;
use std::fs;
use std::io::Write;
use tempfile::TempDir;

/// Helper to create test fixture path
fn fixture_path(name: &str) -> String {
    format!("tests/fixtures/{}", name)
}

#[test]
fn test_lint_workflow_valid_template() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let report = lint_template(&fixture_path("valid_template.toml"), &options)?;

    assert_eq!(
        report.errors.len(),
        0,
        "Valid template should pass all checks"
    );
    assert!(!report.has_errors());

    Ok(())
}

#[test]
fn test_lint_workflow_all_checks_enabled() -> Result<()> {
    let options = LintOptions {
        check_sparql: true,
        check_schema: true,
    };

    let report = lint_template(&fixture_path("template_with_sparql.toml"), &options)?;

    // Should perform all validations
    assert!(report.warnings.len() > 0, "Should find SPARQL warnings");

    Ok(())
}

#[test]
fn test_lint_multiple_templates() -> Result<()> {
    let templates = vec![
        "valid_template.toml",
        "invalid_template.toml",
        "template_with_sparql.toml",
        "template_with_rdf.toml",
    ];

    let options = LintOptions {
        check_sparql: true,
        check_schema: true,
    };

    let mut total_errors = 0;

    for template in templates {
        let report = lint_template(&fixture_path(template), &options)?;
        total_errors += report.errors.len();
    }

    // At least the invalid template should have errors
    assert!(total_errors > 0, "Should find errors across templates");

    Ok(())
}

#[test]
fn test_lint_with_temporary_file() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let temp_file = temp_dir.path().join("test_template.toml");

    let content = r#"---
to: output.rs
vars:
  name: test
---
pub struct {{ name }} {
    field: String,
}
"#;

    let mut file = fs::File::create(&temp_file)?;
    file.write_all(content.as_bytes())?;

    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let report = lint_template(temp_file.to_str().unwrap(), &options)?;

    assert_eq!(
        report.errors.len(),
        0,
        "Valid temporary template should pass"
    );

    Ok(())
}

#[test]
fn test_lint_with_complex_variables() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let temp_file = temp_dir.path().join("complex_template.toml");

    let content = r#"---
to: src/{{ module }}/{{ submodule }}.rs
vars:
  module: core
  submodule: utils
---
// {{ module }}::{{ submodule }}

pub struct {{ module | capitalize }}{{ submodule | capitalize }} {
    value: {{ type }},
}

impl {{ module | capitalize }}{{ submodule | capitalize }} {
    pub fn new(value: {{ type }}) -> Self {
        Self { value }
    }
}
"#;

    let mut file = fs::File::create(&temp_file)?;
    file.write_all(content.as_bytes())?;

    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let report = lint_template(temp_file.to_str().unwrap(), &options)?;

    // All variables are properly closed
    assert_eq!(report.errors.len(), 0);

    Ok(())
}

#[test]
fn test_lint_sparql_integration() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let temp_file = temp_dir.path().join("sparql_template.toml");

    let content = r#"---
to: queries/{{ name }}.sparql
vars:
  name: example
---
# Good SPARQL query
SELECT ?s ?p ?o
WHERE {
  ?s ?p ?o .
}

# Bad SPARQL query (missing WHERE)
SELECT ?s ?p ?o

# CONSTRUCT query
CONSTRUCT { ?s ?p ?o }
WHERE { ?s ?p ?o }
"#;

    let mut file = fs::File::create(&temp_file)?;
    file.write_all(content.as_bytes())?;

    let options = LintOptions {
        check_sparql: true,
        check_schema: false,
    };

    let report = lint_template(temp_file.to_str().unwrap(), &options)?;

    // Should warn about SELECT without WHERE
    let has_where_warning = report.warnings.iter().any(|w| w.message.contains("WHERE"));
    assert!(has_where_warning, "Should detect SELECT without WHERE");

    Ok(())
}

#[test]
fn test_lint_rdf_integration() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let temp_file = temp_dir.path().join("rdf_template.toml");

    let content = r#"---
to: ontology/{{ name }}.ttl
vars:
  name: test
---
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix ex: <http://example.org/{{ name }}#> .

ex:{{ name | capitalize }}
  rdf:type rdfs:Class .
"#;

    let mut file = fs::File::create(&temp_file)?;
    file.write_all(content.as_bytes())?;

    let options = LintOptions {
        check_sparql: false,
        check_schema: true,
    };

    let report = lint_template(temp_file.to_str().unwrap(), &options)?;

    // RDF validation should pass for valid RDF
    assert_eq!(report.errors.len(), 0);

    Ok(())
}

#[tokio::test]
async fn test_full_lint_pipeline() -> Result<()> {
    let input = LintInput {
        template: fixture_path("valid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    let output = execute_lint(input).await?;

    assert_eq!(output.errors_found, 0);
    assert!(output.template_path.ends_with("valid_template.toml"));
    assert!(!output.report.has_errors());

    Ok(())
}

#[tokio::test]
async fn test_full_lint_pipeline_with_errors() -> Result<()> {
    let input = LintInput {
        template: fixture_path("invalid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    let output = execute_lint(input).await?;

    assert!(output.errors_found > 0);
    assert!(output.warnings_found > 0);
    assert!(output.report.has_errors());
    assert!(output.report.has_warnings());

    Ok(())
}

#[test]
fn test_lint_template_path_resolution() -> Result<()> {
    // Test that template path is correctly resolved
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    // Absolute path
    let result = lint_template(&fixture_path("valid_template.toml"), &options)?;
    assert_eq!(result.errors.len(), 0);

    Ok(())
}

#[test]
fn test_lint_serialization() -> Result<()> {
    let report = LintReport {
        errors: vec![LintError {
            line: Some(42),
            message: "Test error".to_string(),
        }],
        warnings: vec![LintWarning {
            line: Some(10),
            message: "Test warning".to_string(),
        }],
    };

    // Test that report can be serialized
    let json = serde_json::to_string(&report)?;
    assert!(json.contains("Test error"));
    assert!(json.contains("Test warning"));
    assert!(json.contains("42"));
    assert!(json.contains("10"));

    Ok(())
}

#[tokio::test]
async fn test_lint_output_serialization() -> Result<()> {
    let input = LintInput {
        template: fixture_path("invalid_template.toml"),
        check_sparql: false,
        check_schema: false,
    };

    let output = execute_lint(input).await?;

    // Test that output can be serialized
    let json = serde_json::to_string(&output)?;
    assert!(json.contains("errors_found"));
    assert!(json.contains("warnings_found"));
    assert!(json.contains("template_path"));
    assert!(json.contains("report"));

    Ok(())
}
