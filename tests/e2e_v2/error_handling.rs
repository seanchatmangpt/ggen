// Scenario 4: Error handling and recovery
// Chicago TDD: Test REAL error scenarios users encounter

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::test_helpers::*;

#[path = "../common/mod.rs"]
mod test_config;
use test_config::integration_timeout;

#[test]
fn test_template_not_found_error() {
    let workspace = setup_workspace().unwrap();

    // Try to generate with non-existent template
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg("nonexistent-template.tmpl")
        .current_dir(workspace.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("No such file")));

    println!("✅ Template not found error: PASSED");
}

#[test]
fn test_invalid_rdf_syntax_error() {
    let workspace = setup_workspace().unwrap();

    // Create file with invalid RDF syntax
    let bad_rdf = "@prefix : <>.\nINVALID RDF SYNTAX HERE!!!";
    let rdf_file = workspace.path().join("bad.ttl");
    fs::write(&rdf_file, bad_rdf).unwrap();

    // Try to validate invalid RDF
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("validate")
        .arg(&rdf_file)
        .current_dir(workspace.path())
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("parse")
                .or(predicate::str::contains("invalid").or(predicate::str::contains("error"))),
        );

    println!("✅ Invalid RDF syntax error: PASSED");
}

#[test]
fn test_missing_required_variables_error() {
    let workspace = setup_workspace().unwrap();

    // Create template with required variables
    let template = "Hello {{ required_var }}! Welcome to {{ app_name }}.";
    let template_file = workspace.path().join("incomplete.tmpl");
    fs::write(&template_file, template).unwrap();

    // Try to render without providing variables
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .current_dir(workspace.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("variable").or(
            predicate::str::contains("undefined").or(
                predicate::str::contains("not found")
            )
        ));

    println!("✅ Missing required variables error: PASSED");
}

#[test]
fn test_invalid_project_name_error() {
    let workspace = setup_workspace().unwrap();

    // Try to create project with invalid name (spaces, special chars)
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("my invalid project!")
        .current_dir(workspace.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("invalid").or(predicate::str::contains("name")));

    println!("✅ Invalid project name error: PASSED");
}

#[test]
fn test_project_already_exists_error() {
    let workspace = setup_workspace().unwrap();

    // Create project first time (should succeed)
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("duplicate-app")
        .current_dir(workspace.path())
        .assert()
        .success();

    // Try to create same project again (should fail)
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("duplicate-app")
        .current_dir(workspace.path())
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("already exists").or(predicate::str::contains("destination")),
        );

    println!("✅ Project already exists error: PASSED");
}

#[test]
fn test_graceful_network_failure() {
    let workspace = setup_workspace().unwrap();

    // Try marketplace search with invalid registry URL
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("marketplace")
        .arg("search")
        .arg("rust")
        .env(
            "GGEN_REGISTRY_URL",
            "https://invalid.nonexistent.domain.xyz/",
        )
        .current_dir(workspace.path())
        .timeout(integration_timeout())
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("network")
                .or(predicate::str::contains("connect").or(predicate::str::contains("failed"))),
        );

    println!("✅ Network failure handling: PASSED");
}
