//! Chicago TDD tests for GgenMcpServer (RED phase)
//!
//! These tests define the expected interface for `GgenMcpServer`, which does
//! not yet exist. They will fail to compile until Task 3 creates
//! `crates/ggen-a2a-mcp/src/ggen_server.rs`.
//!
//! Test pattern: rmcp 1.3.0 in-process duplex transport (tokio::io::duplex).
//! Server is driven via `ServiceExt::serve`; assertions are made through the
//! client `Peer` returned by `serve`.
//!
//! AAA: Arrange / Act / Assert

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{ClientHandler, RoleClient, ServiceExt, model::*, service::RunningService};

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
// Helper: spin up GgenMcpServer over an in-process duplex transport and
// return the connected client Peer.
// ---------------------------------------------------------------------------

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default()
        .serve(client_transport)
        .await?;
    Ok(client)
}

// ---------------------------------------------------------------------------
// Test 1: GgenMcpServer::new() constructs successfully and reports the
//         correct server name via get_info().
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_server_creates_successfully_and_has_correct_name() {
    // Arrange
    let server = GgenMcpServer::new();

    // Act
    let info = server.get_info();

    // Assert – the server must identify itself as "ggen"
    assert_eq!(
        info.server_info.name, "ggen",
        "server_info.name should be 'ggen', got '{}'",
        info.server_info.name
    );
}

// ---------------------------------------------------------------------------
// Test 2: list_tools returns at least four ggen tools:
//         "generate", "validate", "sync", "list_generators"
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_list_tools_returns_ggen_tools() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client.list_tools(None).await?;
    let tool_names: Vec<&str> = result.tools.iter().map(|t| t.name.as_ref()).collect();

    // Assert – all four canonical ggen tools must be present
    let required = ["generate", "validate", "sync", "list_generators"];
    for name in required {
        assert!(
            tool_names.contains(&name),
            "Expected tool '{}' in list, but got: {:?}",
            name,
            tool_names
        );
    }
    assert!(
        result.tools.len() >= 4,
        "Expected at least 4 tools, got {}",
        result.tools.len()
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: calling "list_generators" returns non-empty content without error
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_call_list_generators_returns_content() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act
    let result = client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await?;

    // Assert – must not be an error and must have at least one content item
    assert_ne!(
        result.is_error,
        Some(true),
        "list_generators should not return an error"
    );
    assert!(
        !result.content.is_empty(),
        "list_generators should return non-empty content"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: calling "validate" with a well-formed TTL string returns success
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

    // Assert – validation of a correct TTL must not produce an error result
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
// Test 5: calling "generate" with a missing / nonexistent ontology_path
//         must return an error result (is_error == Some(true))
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

    // Assert – missing file must produce a tool-level error (not a protocol error)
    assert_eq!(
        result.is_error,
        Some(true),
        "generate with missing ontology_path should return is_error=true"
    );

    client.cancel().await?;
    Ok(())
}
