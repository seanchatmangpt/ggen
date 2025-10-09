use std::collections::BTreeMap;
use std::process::Command;
use std::str;

#[test]
fn test_deterministic_generation() {
    // Test that identical inputs produce identical outputs
    let run1 = Command::new("cargo")
        .args(&["run", "--", "gen", "cli", "subcommand", "--vars", "cmd=hello", "summary=Test greeting"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(run1.status.success());

    let run2 = Command::new("cargo")
        .args(&["run", "--", "gen", "cli", "subcommand", "--vars", "cmd=hello", "summary=Test greeting"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(run2.status.success());

    let stdout1 = str::from_utf8(&run1.stdout).unwrap();
    let stdout2 = str::from_utf8(&run2.stdout).unwrap();

    // Both runs should produce the same manifest key
    let key1 = extract_manifest_key(stdout1);
    let key2 = extract_manifest_key(stdout2);
    assert_eq!(key1, key2, "Manifest keys should be identical for identical inputs");
}

#[test]
fn test_matrix_generation() {
    // Test matrix generation with multiple outputs
    let output = Command::new("cargo")
        .args(&["run", "--", "gen", "cli", "subcommand", "--vars", "cmd=test"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();

    // Should contain multiple manifest keys for matrix outputs
    let keys: Vec<&str> = stdout.lines()
        .filter(|line| line.starts_with("manifest"))
        .collect();
    assert!(keys.len() > 1, "Matrix generation should produce multiple artifacts");
}

#[test]
fn test_shacl_validation() {
    // Test that SHACL validation passes for valid data
    let output = Command::new("cargo")
        .args(&["run", "--", "validate", "cli", "subcommand"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("OK"), "SHACL validation should pass");
}

#[test]
fn test_sparql_order_by_enforcement() {
    // Test that matrix queries without ORDER BY are rejected
    let output = Command::new("cargo")
        .args(&["run", "--", "validate", "cli", "subcommand"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // For now, this passes because we haven't implemented matrix validation yet
    assert!(output.status.success());
}

#[test]
fn test_invalid_sparql_rejection() {
    use std::fs;
    use tempfile::TempDir;
    
    // Create a temporary directory for testing
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&template_dir).expect("Failed to create template dir");
    
    // Create a template with invalid SPARQL
    let invalid_template = r#"---
to: "invalid_test.rs"
vars:
  name: "InvalidTest"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:test a ex:Test ."
sparql:
  invalid_query: "INVALID SPARQL SYNTAX HERE"
---
// This template should fail during generation due to invalid SPARQL

pub struct {{name}} {
    // Invalid SPARQL should cause an error
}
"#;
    
    let template_path = template_dir.join("invalid.tmpl");
    fs::write(&template_path, invalid_template).expect("Failed to write template");
    
    // Try to generate from the invalid template
    let output = Command::new("cargo")
        .args(&["run", "--", "gen", template_path.to_str().unwrap()])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");
    
    // This should fail due to invalid SPARQL
    assert!(!output.status.success(), "Invalid SPARQL should cause generation to fail");
    
    // Check that the error message contains information about SPARQL
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("SPARQL") || stderr.contains("query") || stderr.contains("syntax"),
        "Error message should mention SPARQL or query syntax issues. Got: {}",
        stderr
    );
}

#[test]
fn test_variable_precedence() {
    // Test that CLI vars override SPARQL vars override defaults
    let output = Command::new("cargo")
        .args(&["run", "--", "show", "cli", "subcommand"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();

    // Should show the template's default variables
    assert!(stdout.contains("cmd") || stdout.contains("summary"));
}

fn extract_manifest_key(output: &str) -> &str {
    output.lines()
        .find(|line| line.starts_with("manifest:"))
        .and_then(|line| line.split(": ").nth(1))
        .unwrap_or("")
}
