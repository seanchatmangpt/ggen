//! Test helpers for refactoring validation
//!
//! Shared utilities for all refactoring validation tests

use anyhow::Result;
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;
use tempfile::TempDir;
use std::time::Instant;

/// Setup a clean test workspace
pub fn setup_workspace() -> Result<TempDir> {
    TempDir::new().map_err(Into::into)
}

/// Verify a file exists and contains expected content
pub fn verify_file_contains(file_path: &Path, expected: &str) -> Result<()> {
    let content = std::fs::read_to_string(file_path)?;
    if !content.contains(expected) {
        anyhow::bail!(
            "File {} does not contain expected text: {}",
            file_path.display(),
            expected
        );
    }
    Ok(())
}

/// Get the ggen binary path
pub fn ggen_bin() -> PathBuf {
    assert_cmd::cargo::cargo_bin("ggen")
}

/// Measure execution time of a function
pub fn measure_time<F, R>(f: F) -> (R, std::time::Duration)
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let result = f();
    let duration = start.elapsed();
    (result, duration)
}

/// Create sample RDF data for testing
pub fn create_sample_rdf(workspace: &Path, name: &str) -> Result<PathBuf> {
    let rdf_data = format!(
        r#"@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix schema: <http://schema.org/> .

:Person a schema:Class ;
    :hasField :name ;
    :hasField :email ;
    :hasField :age .

:name a :Field ;
    :fieldName "name" ;
    :fieldType "String" .

:email a :Field ;
    :fieldName "email" ;
    :fieldType "String" .

:age a :Field ;
    :fieldName "age" ;
    :fieldType "i32" .

:project :name "{}" ;
    :version "1.0.0" ;
    :author "Test User" .
"#,
        name
    );

    let rdf_file = workspace.join(format!("{}.ttl", name));
    std::fs::write(&rdf_file, rdf_data)?;
    Ok(rdf_file)
}

/// Create sample SPARQL query
pub fn create_sample_sparql_query() -> &'static str {
    "SELECT ?field WHERE { :Person :hasField ?field }"
}

/// Create sample template
pub fn create_sample_template(workspace: &Path, name: &str) -> Result<PathBuf> {
    let template = r#"
# Project: {{ project_name | default(value="Unknown") }}
Version: {{ version | default(value="0.1.0") }}
Author: {{ author | default(value="Anonymous") }}

## Fields
{% for field in fields %}
- {{ field.name }}: {{ field.type }}
{% endfor %}
"#;

    let template_file = workspace.join(format!("{}.tmpl", name));
    std::fs::write(&template_file, template)?;
    Ok(template_file)
}

/// Verify Rust project compiles
pub fn verify_rust_project_builds(project_dir: &Path) -> Result<()> {
    let output = StdCommand::new("cargo")
        .arg("check")
        .arg("--quiet")
        .current_dir(project_dir)
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Rust project failed to build:\n{}", stderr);
    }

    Ok(())
}

/// Verify command output contains expected string
pub fn verify_output_contains(output: &str, expected: &str) -> Result<()> {
    if !output.contains(expected) {
        anyhow::bail!("Output does not contain expected text: {}", expected);
    }
    Ok(())
}

/// Compare performance between two operations
pub struct PerformanceComparison {
    pub baseline_ms: u128,
    pub current_ms: u128,
    pub delta_ms: i128,
    pub delta_percent: f64,
}

impl PerformanceComparison {
    pub fn new(baseline: std::time::Duration, current: std::time::Duration) -> Self {
        let baseline_ms = baseline.as_millis();
        let current_ms = current.as_millis();
        let delta_ms = current_ms as i128 - baseline_ms as i128;
        let delta_percent = if baseline_ms > 0 {
            (delta_ms as f64 / baseline_ms as f64) * 100.0
        } else {
            0.0
        };

        Self {
            baseline_ms,
            current_ms,
            delta_ms,
            delta_percent,
        }
    }

    pub fn is_regression(&self, threshold_percent: f64) -> bool {
        self.delta_percent > threshold_percent
    }

    pub fn is_improvement(&self, threshold_percent: f64) -> bool {
        self.delta_percent < -threshold_percent
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_comparison() {
        let baseline = std::time::Duration::from_millis(100);
        let current = std::time::Duration::from_millis(120);

        let comparison = PerformanceComparison::new(baseline, current);

        assert_eq!(comparison.baseline_ms, 100);
        assert_eq!(comparison.current_ms, 120);
        assert_eq!(comparison.delta_ms, 20);
        assert!((comparison.delta_percent - 20.0).abs() < 0.01);
        assert!(comparison.is_regression(10.0));
    }
}
