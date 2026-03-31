//! SPARQL Validator - Integration Tests
//!
//! These tests demonstrate the SPARQL validator's ability to
//! validate SPARQL syntax using LLM-assisted validation.

use ggen_ai::sparql_validator::{validate_sparql, SparqlIssueType};

#[tokio::test]
async fn test_validate_valid_query() {
    let query = "SELECT * WHERE { ?s ?p ?o }";
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(result.valid);
    assert!(result.issues.is_empty());
}

#[tokio::test]
async fn test_validate_invalid_query() {
    let query = "SELECT * WHERE { ?s ?p ?o"; // Missing closing brace
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(!result.valid);
    assert!(!result.issues.is_empty());
    assert!(!result.suggestions.is_empty());
}

#[tokio::test]
async fn test_sparql_issue_types() {
    let query = "SELECT * WHERE { ?s ?p ?o FILTER(?o > 42"; // Unbalanced parentheses
    let result = validate_sparql(query).await.expect("Should succeed");

    // Should detect syntax issues
    assert!(!result.valid);
    assert!(result
        .issues
        .iter()
        .any(|issue| matches!(issue.issue_type, SparqlIssueType::Syntax)));
}

#[tokio::test]
async fn test_empty_query() {
    let query = "";
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(!result.valid);
    assert!(!result.issues.is_empty());
}

#[tokio::test]
async fn test_select_without_where() {
    let query = "SELECT * { ?s ?p ?o }";
    let result = validate_sparql(query).await.expect("Should succeed");

    // Should detect missing WHERE clause
    assert!(!result.valid);
}

#[tokio::test]
async fn test_complex_valid_query() {
    let query = r#"
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?subject ?label ?type
        WHERE {
            ?subject a rdfs:Class .
            ?subject rdfs:label ?label .
            OPTIONAL {
                ?subject rdfs:subClassOf ?type .
            }
            FILTER(LANG(?label) = "en")
        }
        LIMIT 10
    "#;

    let result = validate_sparql(query).await.expect("Should succeed");
    assert!(result.valid);
}

#[tokio::test]
async fn test_construct_query() {
    let query = r#"
        PREFIX ex: <http://example.org/>
        CONSTRUCT { ?s ?p ?o }
        WHERE {
            ?s ?p ?o .
        }
    "#;

    let result = validate_sparql(query).await.expect("Should succeed");
    assert!(result.valid);
}

#[tokio::test]
async fn test_ask_query() {
    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK WHERE { ?s ?p ?o }
    "#;

    let result = validate_sparql(query).await.expect("Should succeed");
    assert!(result.valid);
}

#[tokio::test]
async fn test_async_validation() {
    let query = "SELECT * WHERE { ?s ?p ?o }";
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(result.valid);
    assert!(result.issues.is_empty());
}

#[tokio::test]
async fn test_async_invalid_query() {
    let query = "SELECT * WHERE { ?s ?p ?o"; // Invalid
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(!result.valid);
    assert!(!result.issues.is_empty());
}
