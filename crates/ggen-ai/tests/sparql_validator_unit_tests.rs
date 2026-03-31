//! SPARQL Validator - Unit Tests
//!
//! These tests use the fast_syntax_check function to avoid LLM dependencies.

use ggen_ai::sparql_validator::{validate_sparql, SparqlIssueType};

#[tokio::test]
async fn test_validate_valid_query_fast_path() {
    // Test a simple valid query that should pass fast syntax check
    let query = "SELECT * WHERE { ?s ?p ?o }";
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(result.valid);
    assert!(result.issues.is_empty());
}

#[tokio::test]
async fn test_validate_invalid_query_fast_path() {
    // Test invalid query that should be caught by fast syntax check
    let query = "SELECT * WHERE { ?s ?p ?o"; // Missing closing brace
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(!result.valid);
    assert!(!result.issues.is_empty());
    assert!(!result.suggestions.is_empty());
}

#[tokio::test]
async fn test_sparql_issue_types_fast_path() {
    // Test invalid query that should trigger syntax issues
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
async fn test_empty_query_fast_path() {
    let query = "";
    let result = validate_sparql(query).await.expect("Should succeed");

    assert!(!result.valid);
    assert!(!result.issues.is_empty());
}

#[tokio::test]
async fn test_select_without_where_fast_path() {
    let query = "SELECT * { ?s ?p ?o }"; // Missing WHERE clause
    let result = validate_sparql(query).await.expect("Should succeed");

    // Should detect missing WHERE clause
    assert!(!result.valid);
    assert!(result
        .issues
        .iter()
        .any(|issue| matches!(issue.issue_type, SparqlIssueType::MissingClause)));
}

#[tokio::test]
async fn test_construct_query_fast_path() {
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
async fn test_ask_query_fast_path() {
    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK WHERE { ?s ?p ?o }
    "#;

    let result = validate_sparql(query).await.expect("Should succeed");
    assert!(result.valid);
}
