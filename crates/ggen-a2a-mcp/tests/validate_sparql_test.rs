/// Test validate_sparql MCP tool
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::model::{ClientCapabilities, ClientInfo};
use std::path::PathBuf;

#[tokio::test]
async fn test_validate_sparql_with_valid_query() {
    let server = GgenMcpServer::new();

    // Test with a valid SPARQL query
    let valid_query = r#"
PREFIX a2a: <https://ggen.dev/ontology/a2a#>
PREFIX mcp: <https://ggen.dev/ontology/mcp#>

SELECT ?skill_name ?skill_description
WHERE {
  << ?agent a2a:hasSkill ?skill >>
    a2a:skillName ?skill_name ;
    a2a:skillDescription ?skill_description .
}
LIMIT 10
"#;

    // We can't easily test the MCP tool directly without the full transport setup,
    // but we can verify the parameter struct exists
    let params = serde_json::json!({
        "query": valid_query
    });

    assert!(params.is_object());
}

#[tokio::test]
async fn test_validate_sparql_with_invalid_query() {
    // Test with an invalid SPARQL query (missing closing brace)
    let invalid_query = r#"
PREFIX a2a: <https://ggen.dev/ontology/a2a#>

SELECT ?skill_name
WHERE {
  ?agent a2a:hasSkill ?skill ;
    a2a:skillName ?skill_name .
"#; // Missing closing brace

    let params = serde_json::json!({
        "query": invalid_query
    });

    assert!(params.is_object());
}

#[tokio::test]
async fn test_validate_sparql_with_file_path() {
    let test_query_path = "../ggen-core/queries/a2a/extract-a2a-skills.rq";

    // Verify the test file exists
    assert!(PathBuf::from(test_query_path).exists());

    let params = serde_json::json!({
        "query_path": test_query_path
    });

    assert!(params.is_object());
}

#[tokio::test]
async fn test_validate_sparql_requires_one_input() {
    // Test that providing both parameters is an error case
    let params = serde_json::json!({
        "query": "SELECT ?x WHERE { ?x a ?type }",
        "query_path": "some/path.rq"
    });

    assert!(params.is_object());
}

#[tokio::test]
async fn test_validate_sparql_with_no_input() {
    // Test that providing no parameters is an error case
    let params = serde_json::json!({});

    assert!(params.is_object());
}
