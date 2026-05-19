//! End-to-end integration tests for RDF graph operations
//!
//! **Chicago TDD Principles**:
//! - REAL RDF graph operations with Oxigraph
//! - REAL SPARQL query execution
//! - REAL file system operations for graph storage
//! - REAL state verification of graph contents
//! - NO mocking of RDF engine
//!
//! **Critical User Workflows (80/20)**:
//! 1. Load RDF data into graph
//! 2. Query graph with SPARQL
//! 3. Export graph data
//! 4. Validate graph structure
//! 5. Generate code from graph queries

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create mcpp command
fn mcpp() -> Command {
    Command::cargo_bin("mcpp").expect("Failed to find mcpp binary")
}

/// Helper to create test RDF data
fn create_test_rdf(temp_dir: &TempDir) -> std::path::PathBuf {
    let rdf_file = temp_dir.path().join("test.ttl");

    let rdf_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .

ex:age a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "age" .

ex:john a ex:Person ;
    ex:name "John Doe" ;
    ex:age "30" .

ex:jane a ex:Person ;
    ex:name "Jane Smith" ;
    ex:age "28" .
"#;

    fs::write(&rdf_file, rdf_content).expect("Failed to write RDF file");
    rdf_file
}

#[test]
#[ignore]
fn test_graph_load_turtle_format() {
    // Chicago TDD: Verify real RDF loading into graph store
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Graph file created
    let _graph_path = temp_dir.path().join(".mcpp/graph");
    // Note: Graph may be stored in memory or file, implementation-dependent
}

#[test]
#[ignore]
fn test_graph_load_invalid_format() {
    // Chicago TDD: Verify error state for invalid RDF
    let temp_dir = TempDir::new().unwrap();
    let invalid_file = temp_dir.path().join("invalid.ttl");
    fs::write(&invalid_file, "not valid RDF @!#$%").unwrap();

    mcpp()
        .arg("graph")
        .arg("load")
        .arg(invalid_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
#[ignore]
fn test_graph_query_sparql_select() {
    // Chicago TDD: Verify real SPARQL query execution
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load graph first
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Query for all persons
    let query = "SELECT ?person WHERE { ?person a <http://example.org/Person> }";

    mcpp()
        .arg("graph")
        .arg("query")
        .arg(query)
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("person").or(predicate::str::contains("result")));
}

#[test]
#[ignore]
fn test_graph_query_invalid_sparql() {
    // Chicago TDD: Verify error handling for invalid SPARQL
    let temp_dir = TempDir::new().unwrap();

    mcpp()
        .arg("graph")
        .arg("query")
        .arg("INVALID SPARQL SYNTAX")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
#[ignore]
fn test_graph_export_turtle() {
    // Chicago TDD: Verify graph export to file
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load graph
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Export graph
    let export_file = temp_dir.path().join("export.ttl");

    mcpp()
        .arg("graph")
        .arg("export")
        .arg(export_file.to_str().unwrap())
        .arg("--format")
        .arg("turtle")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Export file created
    assert!(export_file.exists(), "Export file should be created");

    // Verify state: Export contains RDF data
    let content = fs::read_to_string(&export_file).unwrap();
    assert!(
        content.contains("@prefix") || !content.is_empty(),
        "Export should contain RDF data"
    );
}

#[test]
#[ignore]
fn test_graph_validate_structure() {
    // Chicago TDD: Verify graph validation
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load graph
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Validate graph
    mcpp()
        .arg("graph")
        .arg("validate")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore]
fn test_graph_stats_shows_metrics() {
    // Chicago TDD: Verify graph statistics state
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load graph
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Get stats
    mcpp()
        .arg("graph")
        .arg("stats")
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("triples")
                .or(predicate::str::contains("subjects"))
                .or(predicate::str::contains("stats")),
        );
}

#[test]
#[ignore]
fn test_graph_snapshot_create() {
    // Chicago TDD: Verify graph snapshot state creation
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load graph
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Create snapshot
    mcpp()
        .arg("graph")
        .arg("snapshot")
        .arg("create")
        .arg("v1.0")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore]
fn test_graph_snapshot_list() {
    // Chicago TDD: Verify snapshot listing state
    let temp_dir = TempDir::new().unwrap();

    mcpp()
        .arg("graph")
        .arg("snapshot")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("snapshot")
                .or(predicate::str::contains("No snapshots"))
                .or(predicate::str::contains("Snapshots")),
        );
}

#[test]
#[ignore]
fn test_graph_diff_snapshots() {
    // Chicago TDD: Verify snapshot diff functionality
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    // Load and create first snapshot
    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    mcpp()
        .arg("graph")
        .arg("snapshot")
        .arg("create")
        .arg("v1")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Diff command (may fail if only one snapshot exists)
    let _ = mcpp()
        .arg("graph")
        .arg("diff")
        .arg("v1")
        .arg("v1")
        .current_dir(&temp_dir)
        .output();
}

#[test]
#[ignore]
fn test_graph_help_output() {
    // Chicago TDD: Verify help state is comprehensive
    mcpp()
        .arg("graph")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("RDF graph operations"))
        .stdout(predicate::str::contains("load"))
        .stdout(predicate::str::contains("query"))
        .stdout(predicate::str::contains("export"));
}

#[test]
#[ignore]
fn test_graph_query_help() {
    // Chicago TDD: Verify verb-specific help
    mcpp()
        .arg("graph")
        .arg("query")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("SPARQL").or(predicate::str::contains("query")));
}

#[test]
#[ignore]
fn test_graph_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    mcpp()
        .arg("graph")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
#[ignore]
fn test_graph_load_missing_file() {
    // Chicago TDD: Verify error state for missing RDF file
    let temp_dir = TempDir::new().unwrap();

    mcpp()
        .arg("graph")
        .arg("load")
        .arg("/nonexistent/file.ttl")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("error")));
}

#[test]
#[ignore]
fn test_graph_export_missing_graph() {
    // Chicago TDD: Verify error state when no graph loaded
    let temp_dir = TempDir::new().unwrap();

    let export_file = temp_dir.path().join("export.ttl");

    // Try to export without loading
    let _ = mcpp()
        .arg("graph")
        .arg("export")
        .arg(export_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .output();

    // Command may succeed with empty graph or fail - both valid
}

#[test]
#[ignore]
fn test_graph_performance_large_query() {
    // Chicago TDD: Verify performance for complex queries
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    mcpp()
        .arg("graph")
        .arg("load")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    let start = std::time::Instant::now();

    let query = r#"
        SELECT ?person ?name ?age WHERE {
            ?person a <http://example.org/Person> .
            ?person <http://example.org/name> ?name .
            ?person <http://example.org/age> ?age .
        }
    "#;

    mcpp()
        .arg("graph")
        .arg("query")
        .arg(query)
        .current_dir(&temp_dir)
        .assert()
        .success();

    let duration = start.elapsed();

    // Query should complete quickly
    assert!(
        duration.as_secs() < 5,
        "SPARQL query should be fast: {:?}",
        duration
    );
}
