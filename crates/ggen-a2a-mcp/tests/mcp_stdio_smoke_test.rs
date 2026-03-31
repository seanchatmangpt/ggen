//! Chicago TDD smoke test for MCP stdio server.
//!
//! Verifies that GgenMcpServer works correctly over stdio transport.

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler required by rmcp to negotiate the connection.
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct SmokeTestClientHandler;

impl ClientHandler for SmokeTestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helper: spin up GgenMcpServer over an in-process duplex transport.
// ---------------------------------------------------------------------------

async fn start_server() -> anyhow::Result<RunningService<RoleClient, SmokeTestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = SmokeTestClientHandler.serve(client_transport).await?;
    Ok(client)
}

// ---------------------------------------------------------------------------
// Test: MCP stdio server starts and responds to initialize
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mcp_stdio_server_starts_and_responds_to_initialize() {
    // Arrange: Create server instance
    let server = GgenMcpServer::new();

    // Act: Get server info
    let info = server.get_info();

    // Assert: Server name is correct
    assert_eq!(
        info.server_info.name, "ggen",
        "Server name should be 'ggen', got '{}'",
        info.server_info.name
    );

    // Assert: Capabilities include tools, resources, prompts, completions
    let capabilities = info.capabilities;
    assert!(
        capabilities.tools.is_some(),
        "Server should advertise tools capability"
    );
    assert!(
        capabilities.resources.is_some(),
        "Server should advertise resources capability"
    );
    assert!(
        capabilities.prompts.is_some(),
        "Server should advertise prompts capability"
    );
    assert!(
        capabilities.completions.is_some(),
        "Server should advertise completions capability"
    );
}

// ---------------------------------------------------------------------------
// Test: MCP stdio server responds to list_tools
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mcp_stdio_server_responds_to_list_tools() -> anyhow::Result<()> {
    // Arrange: Start server and connect client
    let client = start_server().await?;

    // Act: List tools
    let tools_result = client.list_tools(None).await;

    // Assert: Tools list is successful
    assert!(
        tools_result.is_ok(),
        "list_tools should succeed, got error: {:?}",
        tools_result
    );

    let tools = tools_result.unwrap();
    assert!(!tools.tools.is_empty(), "Should return at least one tool");

    // Assert: Core ggen tools are present
    let tool_names: Vec<&str> = tools.tools.iter().map(|t| t.name.as_ref()).collect();
    let required_tools = ["generate", "validate", "sync", "list_generators"];
    for tool_name in required_tools {
        assert!(
            tool_names.contains(&tool_name),
            "Expected tool '{}' not found. Got: {:?}",
            tool_name,
            tool_names
        );
    }

    // Cleanup
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test: MCP stdio server handles tool calls
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mcp_stdio_server_handles_tool_calls() -> anyhow::Result<()> {
    // Arrange: Start server and connect client
    let client = start_server().await?;

    // Act: Call list_generators tool (no arguments required)
    let result = client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await;

    // Assert: Tool call succeeded
    assert!(
        result.is_ok(),
        "list_generators tool call should succeed, got error: {:?}",
        result
    );

    let tool_result = result.unwrap();
    assert_ne!(
        tool_result.is_error,
        Some(true),
        "list_generators should not return error"
    );
    assert!(
        !tool_result.content.is_empty(),
        "list_generators should return content"
    );

    // Assert: Response contains generator names
    let text = tool_result.content.iter().find_map(|c| {
        if let rmcp::model::RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });
    assert!(text.is_some(), "Expected text content in response");
    let text = text.unwrap();
    assert!(
        text.contains("go") || text.contains("rust"),
        "Expected generator names in response, got: {}",
        text
    );

    // Cleanup
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test: MCP stdio server handles validate tool
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mcp_stdio_server_validates_ttl() -> anyhow::Result<()> {
    // Arrange: Start server and connect client
    let client = start_server().await?;

    // Act: Validate valid TTL
    let valid_ttl = "@prefix ex: <http://example.org/> . ex:Foo a ex:Bar .";
    let args = serde_json::json!({ "ttl": valid_ttl })
        .as_object()
        .unwrap()
        .clone();
    let result = client
        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
        .await?;

    // Assert: Validation succeeded
    assert_ne!(
        result.is_error,
        Some(true),
        "validate with valid TTL should succeed"
    );

    // Cleanup
    client.cancel().await?;
    Ok(())
}
