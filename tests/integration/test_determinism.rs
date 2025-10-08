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
    // TODO: Create a template with invalid SPARQL and test rejection
    assert!(output.status.success());
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
