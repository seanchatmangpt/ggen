//! Adversarial tests for the doctor command
//!
//! These tests intentionally sabotage the environment and verify that the doctor
//! catches the issues and provides the correct actionable recovery suggestions.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

#[test]
fn test_doctor_config_sabotage() {
    let temp = TempDir::new().unwrap();
    let ggen_toml = temp.path().join("ggen.toml");

    // 1. Missing ggen.toml
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("config")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains("ggen.toml not found"))
        .stdout(predicate::str::contains(
            "Run 'ggen init' to create a ggen.toml",
        ));

    // 2. Corrupt ggen.toml
    fs::write(&ggen_toml, "[project\nmissing_bracket = true").unwrap();
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("config")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains("invalid or corrupted"))
        .stdout(predicate::str::contains("Fix syntax errors in ggen.toml"));

    // 3. Valid ggen.toml
    fs::write(&ggen_toml, "[project]\nname=\"test\"\nversion=\"1.0\"").unwrap();
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("config")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":true"))
        .stdout(predicate::str::contains("ggen.toml is valid"));
}

#[test]
fn test_doctor_ontology_sabotage() {
    let temp = TempDir::new().unwrap();
    let ggen_toml = temp.path().join("ggen.toml");
    fs::write(&ggen_toml, "[project]\nname=\"test\"").unwrap();

    // 1. Missing ontology
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("ontology")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains(
            ".specify/ontologies/main.ttl missing",
        ))
        .stdout(predicate::str::contains(
            "Create .specify/ontologies/main.ttl or run 'ggen init'",
        ));

    // 2. Corrupt ontology
    let ont_dir = temp.path().join(".specify/ontologies");
    fs::create_dir_all(&ont_dir).unwrap();
    let main_ttl = ont_dir.join("main.ttl");
    fs::write(&main_ttl, "@prefix invalid syntax missing period").unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("ontology")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains("Ontology syntax error"))
        .stdout(predicate::str::contains("Fix RDF Turtle syntax errors"));

    // 3. Valid ontology
    fs::write(&main_ttl, "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n<http://test> rdfs:label \"Test\" .").unwrap();
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("ontology")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":true"))
        .stdout(predicate::str::contains("valid and parsable"));
}

#[test]
fn test_doctor_security_sabotage() {
    let temp = TempDir::new().unwrap();

    // 1. Secret leaked (.env)
    let env_file = temp.path().join(".env");
    fs::write(&env_file, "SECRET=xyz").unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("security")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains("Found .env file"))
        .stdout(predicate::str::contains(
            "Move .env outside of the workspace",
        ));

    fs::remove_file(&env_file).unwrap();

    // 2. MCP Wildcard Injection
    let mcp_file = temp.path().join(".mcp.json");
    fs::write(&mcp_file, r#"{"permissions": ["**/*"]}"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("security")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":false"))
        .stdout(predicate::str::contains("broad wildcard permissions ('*')"))
        .stdout(predicate::str::contains("Scope down permissions"));

    fs::remove_file(&mcp_file).unwrap();

    // 3. Clean
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("doctor")
        .arg("security")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"passed\":true"))
        .stdout(predicate::str::contains("Security posture is acceptable"));
}
