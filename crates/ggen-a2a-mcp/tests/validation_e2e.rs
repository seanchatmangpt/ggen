//! Chicago TDD integration tests for validation MCP tools
//!
//! Tests use REAL collaborators:
//! - Real file I/O with TempDir
//! - Real Oxigraph parser for Turtle
//! - Real Tera parser for templates
//! - Real SPARQL parser
//! - No mocks, no test doubles

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};
use std::fs;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Test client handler
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helper: start server
// ---------------------------------------------------------------------------

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

// ---------------------------------------------------------------------------
// ggen.toml manifest validation tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_manifest_parse_success() {
    // Arrange: Create valid ggen.toml content
    let mut client = start_server().await.unwrap();
    let valid_toml = r#"
        [project]
        name = "test-project"
        version = "1.0.0"

        [ontology]
        path = "ontology/templates.ttl"

        [[generation.rules]]
        template = "hello.tmpl"
        output = "generated/hello.txt"
    "#;

    // Act: Call validate_manifest_parse via MCP protocol
    let result = client
        .call_tool(
            "validate_manifest_parse",
            None,
            None,
            Some(serde_json::json!({
                "manifest": valid_toml
            })),
            None,
        )
        .await;

    // Assert: Should succeed with is_valid=true
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
}

#[tokio::test]
async fn test_validate_manifest_parse_missing_required_field() {
    // Arrange: Create invalid ggen.toml (missing project.name)
    let mut client = start_server().await.unwrap();
    let invalid_toml = r#"
        [project]
        version = "1.0.0"

        [ontology]
        path = "ontology/templates.ttl"
    "#;

    // Act: Call validate_manifest_parse
    let result = client
        .call_tool(
            "validate_manifest_parse",
            None,
            None,
            Some(serde_json::json!({
                "manifest": invalid_toml
            })),
            None,
        )
        .await;

    // Assert: Should fail with is_valid=false
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("MISSING_PROJECT_NAME") || json_str.contains("project.name"));
}

#[tokio::test]
async fn test_validate_manifest_dependencies_success() {
    // Arrange: Create TempDir with valid project structure
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    // Create ggen.toml
    let manifest = r#"
        [project]
        name = "test-project"
        version = "1.0.0"

        [ontology]
        path = "ontology/templates.ttl"
    "#;
    fs::write(project_root.join("ggen.toml"), manifest).unwrap();

    // Create ontology directory and file
    fs::create_dir_all(project_root.join("ontology")).unwrap();
    fs::write(
        project_root.join("ontology/templates.ttl"),
        "@prefix ex: <http://example.org> . ex:Subject a ex:Resource .",
    )
    .unwrap();

    // Act: Validate dependencies
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_manifest_dependencies",
            None,
            None,
            Some(serde_json::json!({
                "manifest": manifest,
                "project_root": project_root.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should pass - all files exist
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("\"is_valid\":true") || json_str.contains("true"));
}

#[tokio::test]
async fn test_validate_manifest_dependencies_missing_file() {
    // Arrange: Create TempDir with missing ontology file
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    // Create ggen.toml that references non-existent file
    let manifest = r#"
        [project]
        name = "test-project"
        version = "1.0.0"

        [ontology]
        path = "ontology/missing.ttl"
    "#;
    fs::write(project_root.join("ggen.toml"), manifest).unwrap();

    // Act: Validate dependencies
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_manifest_dependencies",
            None,
            None,
            Some(serde_json::json!({
                "manifest": manifest,
                "project_root": project_root.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should fail - file not found
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

#[tokio::test]
async fn test_validate_manifest_quality_gates_success() {
    // Arrange: Create TempDir with valid project
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    let manifest = r#"
        [project]
        name = "test-project"
        version = "1.0.0"
        description = "A test project"

        [ontology]
        path = "ontology/templates.ttl"
    "#;
    fs::write(project_root.join("ggen.toml"), manifest).unwrap();
    fs::create_dir_all(project_root.join("ontology")).unwrap();
    fs::write(
        project_root.join("ontology/templates.ttl"),
        "@prefix ex: <http://example.org> . ex:Subject a ex:Resource .",
    )
    .unwrap();

    // Act: Run quality gates
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_manifest_quality_gates",
            None,
            None,
            Some(serde_json::json!({
                "manifest": manifest
            })),
            None,
        )
        .await;

    // Assert: Should pass quality gates
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
}

// ---------------------------------------------------------------------------
// Turtle validation tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_ttl_syntax_valid() {
    // Arrange: Valid TTL content
    let mut client = start_server().await.unwrap();
    let valid_ttl = r#"
        @prefix ex: <http://example.org/ns#> .

        ex:Subject a ex:Resource ;
            ex:name "Test" ;
            ex:value 42 .
    "#;

    // Act: Validate TTL syntax
    let result = client
        .call_tool(
            "validate_ttl_syntax",
            None,
            None,
            Some(serde_json::json!({
                "ttl": valid_ttl
            })),
            None,
        )
        .await;

    // Assert: Should parse successfully
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("\"is_valid\":true"));
}

#[tokio::test]
async fn test_validate_ttl_syntax_invalid() {
    // Arrange: Invalid TTL (missing object)
    let mut client = start_server().await.unwrap();
    let invalid_ttl = "@prefix ex: <http://example.org> . ex:Subject a"; // Missing object

    // Act: Validate TTL syntax
    let result = client
        .call_tool(
            "validate_ttl_syntax",
            None,
            None,
            Some(serde_json::json!({
                "ttl": invalid_ttl
            })),
            None,
        )
        .await;

    // Assert: Should report parse errors
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("\"is_valid\":false"));
}

#[tokio::test]
async fn test_validate_ttl_structure_prefixes() {
    // Arrange: TTL with multiple prefixes
    let mut client = start_server().await.unwrap();
    let ttl_with_prefixes = r#"
        @prefix ex: <http://example.org/ns#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Subject a ex:Resource .
    "#;

    // Act: Validate TTL structure
    let result = client
        .call_tool(
            "validate_ttl_structure",
            None,
            None,
            Some(serde_json::json!({
                "ttl": ttl_with_prefixes,
                "check_imports": false
            })),
            None,
        )
        .await;

    // Assert: Should detect all prefixes
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("\"is_valid\":true"));
}

#[tokio::test]
async fn test_validate_ttl_shacl_conforms() {
    // Arrange: Valid data and SHACL shapes
    let mut client = start_server().await.unwrap();
    let data_ttl = r#"
        @prefix ex: <http://example.org/ns#> .

        ex:Subject a ex:Resource ;
            ex:name "Test" .
    "#;

    let shacl_shapes = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/ns#> .

        ex:ResourceShape a sh:NodeShape ;
            sh:targetClass ex:Resource ;
            sh:property [
                sh:path ex:name ;
                sh:datatype xsd:string ;
                sh:minCount 1 ;
            ] .
    "#;

    // Act: Validate with SHACL
    let result = client
        .call_tool(
            "validate_ttl_shacl",
            None,
            None,
            Some(serde_json::json!({
                "ttl": data_ttl,
                "shacl": shacl_shapes
            })),
            None,
        )
        .await;

    // Assert: Should conform
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    let json_str = content.text.as_ref().unwrap();
    assert!(json_str.contains("\"conforms\":true") || json_str.contains("\"is_valid\":true"));
}

// ---------------------------------------------------------------------------
// SPARQL validation tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_sparql_syntax_valid() {
    // Arrange: Create temp file with valid SPARQL
    let temp_dir = TempDir::new().unwrap();
    let query_path = temp_dir.path().join("query.rq");

    let valid_sparql = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10";
    fs::write(&query_path, valid_sparql).unwrap();

    // Act: Validate SPARQL syntax
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_sparql",
            None,
            None,
            Some(serde_json::json!({
                "query_path": query_path.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should be valid
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    assert!(content
        .text
        .as_ref()
        .unwrap()
        .to_lowercase()
        .contains("valid"));
}

#[tokio::test]
async fn test_validate_sparql_syntax_invalid() {
    // Arrange: Create temp file with invalid SPARQL
    let temp_dir = TempDir::new().unwrap();
    let query_path = temp_dir.path().join("query.rq");

    let invalid_sparql = "SELECT ?s WHERE { ?s "; // Unclosed braces
    fs::write(&query_path, invalid_sparql).unwrap();

    // Act: Validate SPARQL syntax
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_sparql",
            None,
            None,
            Some(serde_json::json!({
                "query_path": query_path.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should report syntax error
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

// ---------------------------------------------------------------------------
// Template validation tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_template_syntax_valid() {
    // Arrange: Create temp file with valid Tera template
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("template.tera");

    let valid_template = "Hello {{ name }}! You have {{ count }} messages.";
    fs::write(&template_path, valid_template).unwrap();

    // Act: Validate template syntax
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_templates",
            None,
            None,
            Some(serde_json::json!({
                "template_path": template_path.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should be valid
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    assert!(content
        .text
        .as_ref()
        .unwrap()
        .to_lowercase()
        .contains("valid"));
}

#[tokio::test]
async fn test_validate_template_syntax_invalid() {
    // Arrange: Create temp file with invalid Tera template
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("template.tera");

    let invalid_template = "Hello {{ name }!"; // Unclosed brace
    fs::write(&template_path, invalid_template).unwrap();

    // Act: Validate template syntax
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_templates",
            None,
            None,
            Some(serde_json::json!({
                "template_path": template_path.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: Should report syntax error
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

// ---------------------------------------------------------------------------
// Orchestration tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_pipeline_full() {
    // Arrange: Create complete valid project
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Create ggen.toml
    let manifest = r#"
        [project]
        name = "test-project"
        version = "1.0.0"
        description = "A complete test project"

        [ontology]
        path = "ontology/templates.ttl"

        [[generation.rules]]
        template = "hello.tmpl"
        output = "generated/hello.txt"
    "#;
    fs::write(project_path.join("ggen.toml"), manifest).unwrap();

    // Create ontology
    fs::create_dir_all(project_path.join("ontology")).unwrap();
    fs::write(
        project_path.join("ontology/templates.ttl"),
        "@prefix ex: <http://example.org> . ex:Subject a ex:Resource .",
    )
    .unwrap();

    // Act: Run full pipeline validation
    let mut client = start_server().await.unwrap();
    let result = client
        .call_tool(
            "validate_pipeline",
            None,
            None,
            Some(serde_json::json!({
                "project_path": project_path.display().to_string()
            })),
            None,
        )
        .await;

    // Assert: All quality gates should pass
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    let content = response.content.first().unwrap();
    let text = content.text.as_ref().unwrap();
    assert!(
        text.contains("✅")
            || text.to_lowercase().contains("pass")
            || text.to_lowercase().contains("all")
    );
}

// ---------------------------------------------------------------------------
// Edge case tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_empty_manifest() {
    // Arrange: Empty manifest
    let mut client = start_server().await.unwrap();
    let empty_manifest = "";

    // Act: Try to validate
    let result = client
        .call_tool(
            "validate_manifest_parse",
            None,
            None,
            Some(serde_json::json!({
                "manifest": empty_manifest
            })),
            None,
        )
        .await;

    // Assert: Should fail with parse error
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

#[tokio::test]
async fn test_validate_missing_required_params() {
    // Arrange: Server with no params
    let mut client = start_server().await.unwrap();

    // Act: Call validate_manifest_parse without manifest or manifest_path
    let result = client
        .call_tool(
            "validate_manifest_parse",
            None,
            None,
            Some(serde_json::json!({})),
            None,
        )
        .await;

    // Assert: Should return error about missing params
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

#[tokio::test]
async fn test_validate_nonexistent_file_path() {
    // Arrange: Path to non-existent file
    let mut client = start_server().await.unwrap();
    let nonexistent_path = "/tmp/nonexistent_file_12345.ttl";

    // Act: Try to validate non-existent file
    let result = client
        .call_tool(
            "validate_ttl_syntax",
            None,
            None,
            Some(serde_json::json!({
                "ttl_path": nonexistent_path
            })),
            None,
        )
        .await;

    // Assert: Should report file not found
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.is_error);
}

// ---------------------------------------------------------------------------
// Performance tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_large_ttl_performance() {
    // Arrange: Large TTL file (1000 triples)
    let mut client = start_server().await.unwrap();
    let mut large_ttl = String::from("@prefix ex: <http://example.org/ns#> .\n");

    for i in 0..1000 {
        large_ttl.push_str(&format!("ex:subj{} a ex:Resource ; ex:value {} .\n", i, i));
    }

    // Act: Validate with timing
    let start = std::time::Instant::now();
    let result = client
        .call_tool(
            "validate_ttl_syntax",
            None,
            None,
            Some(serde_json::json!({
                "ttl": large_ttl
            })),
            None,
        )
        .await;

    // Assert: Should complete in reasonable time (< 5 seconds)
    let elapsed = start.elapsed();
    assert!(result.is_ok());
    assert!(
        elapsed.as_secs() < 5,
        "Large TTL validation should complete in < 5s"
    );
}

// ---------------------------------------------------------------------------
// Test count summary
// ---------------------------------------------------------------------------

// Test counts by category:
// - ggen.toml manifest validation: 5 tests
// - Turtle validation: 4 tests
// - SPARQL validation: 2 tests
// - Template validation: 2 tests
// - Orchestration: 1 test
// - Edge cases: 3 tests
// - Performance: 1 test
//
// Total: 18 comprehensive Chicago TDD integration tests
