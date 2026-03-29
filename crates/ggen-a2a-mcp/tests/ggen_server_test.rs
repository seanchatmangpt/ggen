//! Chicago TDD tests for GgenMcpServer
//!
//! Tests cover all MCP primitives: Tools, Resources, Prompts, Completions.
//! Pattern: rmcp 1.3.0 in-process duplex transport (tokio::io::duplex).
//! AAA: Arrange / Act / Assert

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler required by rmcp to negotiate the connection.
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helper: spin up GgenMcpServer over an in-process duplex transport.
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
// Test 1: Server name
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_server_creates_successfully_and_has_correct_name() {
    // Arrange
    let server = GgenMcpServer::new();

    // Act
    let info = server.get_info();

    // Assert
    assert_eq!(
        info.server_info.name, "ggen",
        "server_info.name should be 'ggen', got '{}'",
        info.server_info.name
    );
}

// ---------------------------------------------------------------------------
// Test 2: Capabilities advertised
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_server_advertises_tools_resources_prompts_completions() {
    // Arrange
    let server = GgenMcpServer::new();

    // Act
    let info = server.get_info();

    // Assert — all four primitives must be enabled
    assert!(info.capabilities.tools.is_some(), "tools capability must be enabled");
    assert!(info.capabilities.resources.is_some(), "resources capability must be enabled");
    assert!(info.capabilities.prompts.is_some(), "prompts capability must be enabled");
    assert!(info.capabilities.completions.is_some(), "completions capability must be enabled");
}

// ---------------------------------------------------------------------------
// Test 3: list_tools returns at least 9 ggen tools
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_list_tools_returns_ggen_tools() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client.list_tools(None).await?;
    let tool_names: Vec<&str> = result.tools.iter().map(|t| t.name.as_ref()).collect();

    // Assert — all canonical ggen tools must be present
    let required = [
        "generate",
        "validate",
        "sync",
        "list_generators",
        "list_examples",
        "get_example",
        "search",
        "scaffold_from_example",
        "query_ontology",
    ];
    for name in required {
        assert!(
            tool_names.contains(&name),
            "Expected tool '{}' in list, but got: {:?}",
            name,
            tool_names
        );
    }
    assert!(
        result.tools.len() >= 9,
        "Expected at least 9 tools, got {}",
        result.tools.len()
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: list_generators returns content
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_list_generators_returns_content() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await?;

    // Assert
    assert_ne!(result.is_error, Some(true), "list_generators should not return an error");
    assert!(!result.content.is_empty(), "list_generators should return non-empty content");

    // Content should mention known generators
    let text = result.content.iter().find_map(|c| {
        if let rmcp::model::RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });
    assert!(text.is_some(), "Expected text content");
    let text: String = text.unwrap();
    assert!(text.contains("go"), "Expected 'go' in generators list");
    assert!(text.contains("rust"), "Expected 'rust' in generators list");

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: validate with valid TTL succeeds
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_validate_tool_with_valid_ttl() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let valid_ttl = "@prefix ex: <http://example.org/> . ex:Foo a ex:Bar .";
    let args = serde_json::json!({ "ttl": valid_ttl })
        .as_object()
        .unwrap()
        .clone();

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
        .await?;

    // Assert
    assert_ne!(
        result.is_error,
        Some(true),
        "validate with valid TTL should succeed, got is_error={:?}",
        result.is_error
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 6: validate with invalid TTL returns error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_validate_tool_with_invalid_ttl_returns_error() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let invalid_ttl = "THIS IS NOT TURTLE @@@INVALID";
    let args = serde_json::json!({ "ttl": invalid_ttl })
        .as_object()
        .unwrap()
        .clone();

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
        .await?;

    // Assert — invalid TTL must produce is_error=true
    assert_eq!(
        result.is_error,
        Some(true),
        "validate with invalid TTL should return is_error=true"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 7: generate with missing ontology_path returns error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_generate_tool_with_missing_file_returns_error() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let args = serde_json::json!({
        "ontology_path": "/nonexistent/path/that/does/not/exist.ttl"
    })
    .as_object()
    .unwrap()
    .clone();

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("generate").with_arguments(args))
        .await?;

    // Assert
    assert_eq!(
        result.is_error,
        Some(true),
        "generate with missing ontology_path should return is_error=true"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 8: list_examples returns (possibly empty) content without error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_list_examples_returns_content() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;

    // Assert — must not error; content must be present
    assert_ne!(result.is_error, Some(true), "list_examples should not return an error");
    assert!(!result.content.is_empty(), "list_examples should return non-empty content");

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 9: get_example with unknown name returns error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_get_example_with_unknown_name_returns_error() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let args = serde_json::json!({ "name": "this-example-does-not-exist-xyz-9999" })
        .as_object()
        .unwrap()
        .clone();

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("get_example").with_arguments(args))
        .await?;

    // Assert
    assert_eq!(
        result.is_error,
        Some(true),
        "get_example with unknown name should return is_error=true"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 10: query_ontology executes a SPARQL SELECT on inline TTL
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_query_ontology_sparql() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let ttl = "@prefix ex: <http://example.org/> . ex:Foo a ex:Bar . ex:Baz a ex:Bar .";
    let sparql = "PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s a ex:Bar }";
    let args = serde_json::json!({ "ttl": ttl, "sparql": sparql })
        .as_object()
        .unwrap()
        .clone();

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("query_ontology").with_arguments(args))
        .await?;

    // Assert
    assert_ne!(result.is_error, Some(true), "query_ontology should succeed");
    let text: Option<String> = result.content.iter().find_map(|c| {
        if let rmcp::model::RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });
    assert!(text.is_some(), "Expected text content in result");
    // Should contain at least one result row
    let text: String = text.unwrap();
    assert!(text.contains("count"), "Expected 'count' in result JSON");

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 11: list_resources returns resource list
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_list_resources_returns_resources() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client.list_resources(None).await?;

    // Assert — must succeed (examples dir may be empty in CI, that's ok)
    // What matters is the call succeeds without error
    let _ = result.resources; // just ensure the field exists

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 12: list_prompts returns exactly 3 prompts
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_list_prompts_returns_three() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client.list_prompts(None).await?;

    // Assert
    assert_eq!(
        result.prompts.len(),
        3,
        "Expected exactly 3 prompts, got {}: {:?}",
        result.prompts.len(),
        result.prompts.iter().map(|p| &p.name).collect::<Vec<_>>()
    );

    let names: Vec<&str> = result.prompts.iter().map(|p| p.name.as_str()).collect();
    assert!(names.contains(&"explain-rdf-schema"), "Missing explain-rdf-schema prompt");
    assert!(names.contains(&"generate-from-example"), "Missing generate-from-example prompt");
    assert!(names.contains(&"scaffold-project"), "Missing scaffold-project prompt");

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 13: get_prompt explain-rdf-schema returns messages
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_get_prompt_explain_rdf_schema() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let ttl = "@prefix ex: <http://example.org/> . ex:Person a owl:Class .";
    let args = serde_json::json!({ "ttl_content": ttl })
        .as_object()
        .unwrap()
        .clone();

    // Act
    let result = client
        .get_prompt(GetPromptRequestParams::new("explain-rdf-schema").with_arguments(args))
        .await?;

    // Assert
    assert!(!result.messages.is_empty(), "explain-rdf-schema should return non-empty messages");
    assert_eq!(
        result.messages[0].role,
        PromptMessageRole::User,
        "First message should be User role"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 14: get_prompt with unknown name returns error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_get_prompt_unknown_name_returns_error() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act — an unknown prompt should produce a protocol-level error
    let result = client
        .get_prompt(GetPromptRequestParams::new("nonexistent-prompt-xyz"))
        .await;

    // Assert — should be an Err (protocol error, not a tool result error)
    assert!(result.is_err(), "Unknown prompt should return a protocol error");

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 15: complete example_name argument
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_complete_example_name_argument() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let params = CompleteRequestParams::new(
        Reference::for_prompt("list_examples"),
        ArgumentInfo {
            name: "example_name".to_string(),
            value: "".to_string(), // empty prefix = return all
        },
    );

    // Act
    let result = client.complete(params).await?;

    // Assert — must not error; completion values may be empty if examples dir missing
    let _ = result.completion.values; // field exists

    client.cancel().await?;
    Ok(())
}
