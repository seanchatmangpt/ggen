//! End-to-end integration tests for ontology commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations for ontology files
//! - REAL state verification of generated code
//! - NO mocking of ontology processing
//!
//! **Critical User Workflows (80/20)**:
//! 1. Initialize ontology project
//! 2. Validate ontology schema
//! 3. Generate code from ontology

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Helper to create test ontology file
fn create_test_ontology(temp_dir: &TempDir) -> std::path::PathBuf {
    let ontology_file = temp_dir.path().join("test.ttl");

    let ontology_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .
"#;

    fs::write(&ontology_file, ontology_content).expect("Failed to write ontology file");
    ontology_file
}

#[test]
fn test_ontology_init_creates_project() {
    // Chicago TDD: Verify ontology project initialization
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ontology")
        .arg("init")
        .arg("test-ontology")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Ontology file created
    let ontology_path = temp_dir.path().join("ontologies/example.ttl");
    assert!(ontology_path.exists(), "Ontology file should be created");
}

#[test]
fn test_ontology_validate_passes() {
    // Chicago TDD: Validate well-formed ontology
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("ontology")
        .arg("validate")
        .arg(ontology_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ontology_validate_strict() {
    // Chicago TDD: Verify strict validation mode
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("ontology")
        .arg("validate")
        .arg(ontology_file.to_str().unwrap())
        .arg("--strict")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ontology_generate_creates_code() {
    // Chicago TDD: Generate code from ontology
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("ontology")
        .arg("generate")
        .arg(ontology_file.to_str().unwrap())
        .arg("--language")
        .arg("typescript")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Generated files created
    let generated_dir = temp_dir.path().join("generated");
    assert!(generated_dir.exists(), "Generated directory should be created");
}

#[test]
fn test_ontology_generate_with_output() {
    // Chicago TDD: Verify custom output directory
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    let output_dir = temp_dir.path().join("custom-output");

    ggen()
        .arg("ontology")
        .arg("generate")
        .arg(ontology_file.to_str().unwrap())
        .arg("--language")
        .arg("rust")
        .arg("--output")
        .arg(output_dir.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Custom output directory used
    assert!(output_dir.exists(), "Custom output directory should be created");
}

#[test]
fn test_ontology_generate_with_zod() {
    // Chicago TDD: Verify Zod utilities generation
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("ontology")
        .arg("generate")
        .arg(ontology_file.to_str().unwrap())
        .arg("--language")
        .arg("typescript")
        .arg("--zod")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ontology_generate_with_utilities() {
    // Chicago TDD: Verify utilities generation
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("ontology")
        .arg("generate")
        .arg(ontology_file.to_str().unwrap())
        .arg("--utilities")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ontology_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("ontology")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("validate"))
        .stdout(predicate::str::contains("generate"));
}

#[test]
fn test_ontology_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("ontology")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
fn test_ontology_validate_missing_file() {
    // Chicago TDD: Verify error state for missing file
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ontology")
        .arg("validate")
        .arg("/nonexistent/file.ttl")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("error")));
}

#[test]
fn test_ontology_generate_missing_file() {
    // Chicago TDD: Verify error state for missing ontology file
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ontology")
        .arg("generate")
        .arg("/nonexistent/schema.ttl")
        .arg("--language")
        .arg("typescript")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("error")));
}
