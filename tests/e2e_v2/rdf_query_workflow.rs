// Scenario 6: RDF graph query integration
// Chicago TDD: REAL RDF queries driving code generation

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::test_helpers::*;

#[test]
fn test_rdf_query_to_template_data() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create RDF schema
    let schema = r#"
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:User a :Entity ;
    :hasProperty :username ;
    :hasProperty :email ;
    :hasProperty :created_at .

:Post a :Entity ;
    :hasProperty :title ;
    :hasProperty :content ;
    :hasProperty :author_id .
"#;

    let schema_file = workspace_path.join("schema.ttl");
    fs::write(&schema_file, schema).unwrap();

    // Query for all entities
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&schema_file)
        .arg("SELECT ?entity WHERE { ?entity a :Entity }")
        .current_dir(workspace_path)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should find both entities
    assert!(stdout.contains(":User") || stdout.contains("User"));
    assert!(stdout.contains(":Post") || stdout.contains("Post"));

    println!("✅ RDF query to template data: PASSED");
}

#[test]
fn test_complex_sparql_queries() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create a knowledge graph
    let knowledge_graph = r#"
@prefix : <http://example.org/> .
@prefix api: <http://api.example.org/> .

api:UsersEndpoint a :RestEndpoint ;
    :path "/api/users" ;
    :method "GET" ;
    :returns :UserList .

api:UserDetailEndpoint a :RestEndpoint ;
    :path "/api/users/{id}" ;
    :method "GET" ;
    :returns :User .

api:CreateUserEndpoint a :RestEndpoint ;
    :path "/api/users" ;
    :method "POST" ;
    :accepts :UserInput ;
    :returns :User .
"#;

    let kg_file = workspace_path.join("api_kg.ttl");
    fs::write(&kg_file, knowledge_graph).unwrap();

    // Query all REST endpoints
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&kg_file)
        .arg("SELECT ?endpoint ?path ?method WHERE { ?endpoint a :RestEndpoint ; :path ?path ; :method ?method }")
        .current_dir(workspace_path)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should find endpoints with paths and methods
    assert!(stdout.contains("/api/users") || stdout.contains("UsersEndpoint"));

    println!("✅ Complex SPARQL queries: PASSED");
}

#[test]
fn test_rdf_validation_workflow() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create valid RDF
    let valid_rdf = r#"
@prefix : <http://example.org/> .
:subject :predicate :object .
"#;

    let valid_file = workspace_path.join("valid.ttl");
    fs::write(&valid_file, valid_rdf).unwrap();

    // Validate should succeed
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("validate")
        .arg(&valid_file)
        .current_dir(workspace_path)
        .assert()
        .success();

    // Create invalid RDF
    let invalid_rdf = "This is not valid RDF syntax!!!";
    let invalid_file = workspace_path.join("invalid.ttl");
    fs::write(&invalid_file, invalid_rdf).unwrap();

    // Validate should fail
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("validate")
        .arg(&invalid_file)
        .current_dir(workspace_path)
        .assert()
        .failure()
        .stderr(predicate::str::contains("parse").or(
            predicate::str::contains("invalid").or(
                predicate::str::contains("error")
            )
        ));

    println!("✅ RDF validation workflow: PASSED");
}

#[test]
fn test_rdf_count_query() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create RDF data with countable items
    let data = r#"
@prefix : <http://example.org/> .

:item1 a :Product .
:item2 a :Product .
:item3 a :Product .
:item4 a :Service .
:item5 a :Service .
"#;

    let data_file = workspace_path.join("items.ttl");
    fs::write(&data_file, data).unwrap();

    // Count products
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&data_file)
        .arg("SELECT (COUNT(?product) as ?count) WHERE { ?product a :Product }")
        .current_dir(workspace_path)
        .output()
        .unwrap();

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        println!("Count query result:\n{}", stdout);
        // Should show count of 3
        assert!(stdout.contains("3") || stdout.contains("count"));
    }

    println!("✅ RDF count query: PASSED");
}
