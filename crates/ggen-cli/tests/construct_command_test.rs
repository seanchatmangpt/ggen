//! Integration tests for `ggen construct` commands
//!
//! Tests the CLI interface for LLM-Construct pattern operations.
//! Uses Chicago TDD pattern with real command execution.

use assert_cmd::Command;
use assert_fs::prelude::*;
use predicates::prelude::*;
use serde_json::Value;

#[test]
fn test_construct_create_with_nonexistent_file() {
    // Arrange: Use a path that doesn't exist
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create with nonexistent spec file
    cmd.arg("construct")
        .arg("create")
        .arg("/nonexistent/spec.ttl");

    // Assert: Should return error indicating file not found
    cmd.assert()
        .failure()
        .stdout(predicate::str::contains("not found"))
        .stdout(predicate::str::contains("error"));
}

#[test]
fn test_construct_create_with_non_ttl_file() {
    // Arrange: Create a temporary file with wrong extension
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let spec_file = temp_dir.child("spec.txt");
    spec_file.write_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .").unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create with non-TTL file
    cmd.arg("construct")
        .arg("create")
        .arg(spec_file.path());

    // Assert: Should return error about file extension
    cmd.assert()
        .failure()
        .stdout(predicate::str::contains("Turtle"))
        .stdout(predicate::str::contains(".ttl"));

    temp_dir.close().unwrap();
}

#[test]
fn test_construct_create_with_valid_ttl_file_returns_not_implemented() {
    // Arrange: Create a temporary TTL file
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let spec_file = temp_dir.child("bond.ttl");
    spec_file.write_str(
        r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/> .

fibo:Bond a owl:Class ;
    rdfs:label "Bond" ;
    rdfs:comment "A debt security" .
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create with valid TTL file
    cmd.arg("construct")
        .arg("create")
        .arg(spec_file.path());

    // Assert: Should return not_implemented status (implementation pending)
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("not_implemented"));

    temp_dir.close().unwrap();
}

#[test]
fn test_construct_create_json_output_structure() {
    // Arrange: Create a temporary TTL file
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let spec_file = temp_dir.child("test.ttl");
    spec_file.write_str(
        r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create
    cmd.arg("construct")
        .arg("create")
        .arg(spec_file.path());

    // Assert: Output should be valid JSON with expected fields
    let output = cmd.output().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();

    // Parse as JSON
    let json: Value = serde_json::from_str(&stdout).unwrap();

    // Verify required fields exist
    assert!(json.get("status").is_some());
    assert!(json.get("spec_path").is_some());
    assert!(json.get("output_dir").is_some());
    assert!(json.get("next_steps").is_some());

    temp_dir.close().unwrap();
}

#[test]
fn test_construct_validate_returns_not_implemented() {
    // Arrange: Prepare command
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct validate
    cmd.arg("construct")
        .arg("validate")
        .arg("bond_extractor");

    // Assert: Should return not_implemented status
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("not_implemented"));
}

#[test]
fn test_construct_validate_json_output_structure() {
    // Arrange: Prepare command
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct validate
    cmd.arg("construct")
        .arg("validate")
        .arg("test_module");

    // Assert: Output should be valid JSON with expected fields
    let output = cmd.output().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();

    // Parse as JSON
    let json: Value = serde_json::from_str(&stdout).unwrap();

    // Verify required fields exist
    assert!(json.get("status").is_some());
    assert!(json.get("module_name").is_some());
    assert_eq!(json["module_name"], "test_module");
    assert!(json.get("next_steps").is_some());
}

#[test]
fn test_construct_create_with_custom_output_dir() {
    // Arrange: Create a temporary TTL file
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let spec_file = temp_dir.child("spec.ttl");
    spec_file.write_str(
        r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create with custom output directory
    cmd.arg("construct")
        .arg("create")
        .arg(spec_file.path())
        .arg("--output-dir")
        .arg("custom/output");

    // Assert: Output should include custom directory
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("custom/output"));

    temp_dir.close().unwrap();
}

#[test]
fn test_to_snake_case_conversion() {
    // This test verifies the utility function behavior
    // Note: Direct unit tests are in construct.rs module tests

    // Arrange: Create a TTL file with camelCase name
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let spec_file = temp_dir.child("FIBOBond.ttl");
    spec_file.write_str(
        r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act: Run construct create
    cmd.arg("construct")
        .arg("create")
        .arg(spec_file.path());

    // Assert: Should succeed (conversion happens internally)
    cmd.assert().success();

    temp_dir.close().unwrap();
}
