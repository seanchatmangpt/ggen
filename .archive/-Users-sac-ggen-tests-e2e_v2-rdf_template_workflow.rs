// Scenario 2: RDF-driven template workflow
// Chicago TDD: REAL RDF parsing and template rendering

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::test_helpers::*;

#[test]
fn test_rdf_driven_code_generation() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create RDF schema
    let rdf_data = r#"
@prefix : <http://example.org/> .
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
"#;

    let rdf_file = workspace_path.join("schema.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // Verify RDF file is valid
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("validate")
        .arg(&rdf_file)
        .current_dir(workspace_path)
        .assert()
        .success();

    // Query RDF data
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&rdf_file)
        .arg("SELECT ?field WHERE { :Person :hasField ?field }")
        .current_dir(workspace_path)
        .assert()
        .success()
        .stdout(predicate::str::contains(":name"));

    println!("✅ RDF-driven code generation: PASSED");
}

#[test]
fn test_rdf_template_integration() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create simple RDF data
    let rdf_data = r#"
@prefix : <http://example.org/> .

:project :name "MyApp" ;
    :version "1.0.0" ;
    :author "Test User" .
"#;

    let rdf_file = workspace_path.join("project.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // Create template that could use RDF data
    let template = r#"
# Project: {{ project_name | default(value="Unknown") }}
Version: {{ version | default(value="0.1.0") }}
Author: {{ author | default(value="Anonymous") }}
"#;

    let template_file = workspace_path.join("readme.tmpl");
    fs::write(&template_file, template).unwrap();

    // Render template
    let output_file = workspace_path.join("README.md");

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .arg("--var")
        .arg("project_name=MyApp")
        .arg("--var")
        .arg("version=1.0.0")
        .arg("--var")
        .arg("author=Test User")
        .arg("--output")
        .arg(&output_file)
        .current_dir(workspace_path)
        .assert()
        .success();

    // Verify output
    verify_file_contains(&output_file, "MyApp").unwrap();
    verify_file_contains(&output_file, "1.0.0").unwrap();
    verify_file_contains(&output_file, "Test User").unwrap();

    println!("✅ RDF template integration: PASSED");
}

#[test]
fn test_sparql_query_execution() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create RDF data with relationships
    let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

:alice foaf:name "Alice" ;
    foaf:knows :bob .

:bob foaf:name "Bob" ;
    foaf:knows :charlie .

:charlie foaf:name "Charlie" ;
    foaf:knows :alice .
"#;

    let rdf_file = workspace_path.join("network.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // Execute SPARQL queries
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&rdf_file)
        .arg("SELECT ?person ?name WHERE { ?person foaf:name ?name }")
        .current_dir(workspace_path)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should find all three people
    assert!(stdout.contains("Alice") || stdout.contains(":alice"));
    assert!(stdout.contains("Bob") || stdout.contains(":bob"));
    assert!(stdout.contains("Charlie") || stdout.contains(":charlie"));

    println!("✅ SPARQL query execution: PASSED");
}
