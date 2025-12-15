//! Chicago TDD tests for SPARQL query execution
//!
//! Uses REAL in-memory RDF graphs and ACTUAL SPARQL queries

use anyhow::Result;
use ggen_cli::domain::graph::{execute_sparql, QueryOptions};
use ggen_core::Graph;
use std::io::Write;
use tempfile::NamedTempFile;

#[test]
fn test_query_real_rdf_graph() -> Result<()> {
    // Create REAL Turtle data
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice a foaf:Person ;
            foaf:name "Alice" ;
            foaf:age "30" .

        ex:bob a foaf:Person ;
            foaf:name "Bob" ;
            foaf:age "25" .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Execute REAL SPARQL query
    let options = QueryOptions {
        query: r#"
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            SELECT ?name ?age
            WHERE {
                ?person foaf:name ?name ;
                       foaf:age ?age .
            }
            ORDER BY ?name
        "#
        .to_string(),
        graph_file: Some(temp_path),
        output_format: "json".to_string(),
    };

    let result = execute_sparql(options)?;

    // Verify REAL query results
    assert_eq!(result.variables, vec!["name", "age"]);
    assert_eq!(result.result_count, 2);
    assert_eq!(result.bindings.len(), 2);

    Ok(())
}

#[test]
fn test_query_with_filter() -> Result<()> {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:alice ex:age "30"^^xsd:integer .
        ex:bob ex:age "25"^^xsd:integer .
        ex:charlie ex:age "35"^^xsd:integer .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Query with REAL SPARQL FILTER
    let options = QueryOptions {
        query: r#"
            PREFIX ex: <http://example.org/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?person ?age
            WHERE {
                ?person ex:age ?age .
                FILTER(?age > "28"^^xsd:integer)
            }
        "#
        .to_string(),
        graph_file: Some(temp_path),
        output_format: "json".to_string(),
    };

    let result = execute_sparql(options)?;

    // Verify filtering works
    assert_eq!(result.result_count, 2); // alice and charlie

    Ok(())
}

#[test]
fn test_query_ask_boolean() -> Result<()> {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Execute ASK query
    let options = QueryOptions {
        query: "ASK { ?s ?p ?o }".to_string(),
        graph_file: Some(temp_path),
        output_format: "json".to_string(),
    };

    let result = execute_sparql(options)?;

    // Verify boolean result
    assert_eq!(result.variables, vec!["result"]);
    assert!(result.bindings[0].get("result").unwrap().contains("true"));

    Ok(())
}

#[test]
fn test_query_empty_graph() -> Result<()> {
    // Query empty graph (no file)
    let options = QueryOptions {
        query: "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
        graph_file: None,
        output_format: "json".to_string(),
    };

    let result = execute_sparql(options)?;

    // Verify no results
    assert_eq!(result.result_count, 0);
    assert_eq!(result.bindings.len(), 0);

    Ok(())
}

#[test]
fn test_query_with_optional() -> Result<()> {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice foaf:name "Alice" ;
            foaf:email "alice@example.org" .

        ex:bob foaf:name "Bob" .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Query with OPTIONAL
    let options = QueryOptions {
        query: r#"
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            SELECT ?name ?email
            WHERE {
                ?person foaf:name ?name .
                OPTIONAL { ?person foaf:email ?email }
            }
        "#
        .to_string(),
        graph_file: Some(temp_path),
        output_format: "json".to_string(),
    };

    let result = execute_sparql(options)?;

    // Verify both results returned (one with email, one without)
    assert_eq!(result.result_count, 2);

    Ok(())
}
