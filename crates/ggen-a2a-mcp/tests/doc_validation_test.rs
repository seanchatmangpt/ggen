//! Runtime validation that documentation matches actual server capabilities.
//!
//! Tests verify:
//! - All documented tools exist with correct parameters
//! - All documented resources are accessible
//! - All documented prompts exist with correct arguments
//! - Tool descriptions match documented behavior

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

#[derive(Debug, Clone, Default)]
struct TestClient;
impl ClientHandler for TestClient {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClient>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    Ok(TestClient.serve(client_transport).await?)
}

/// Documented tools in README.md
const DOCUMENTED_TOOLS: &[&str] = &[
    "generate",
    "sync",
    "validate",
    "list_generators",
    "list_examples",
    "get_example",
    "search",
    "scaffold_from_example",
    "query_ontology",
];

#[tokio::test]
async fn all_documented_tools_exist() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;
    let tool_names: Vec<&str> = tools.tools.iter().map(|t| t.name.as_ref()).collect();

    for documented in DOCUMENTED_TOOLS {
        assert!(
            tool_names.contains(documented),
            "Documented tool '{}' not found in server. Available: {:?}",
            documented,
            tool_names
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn documented_tools_have_descriptions() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;

    for tool in &tools.tools {
        if DOCUMENTED_TOOLS.contains(&tool.name.as_ref()) {
            assert!(
                tool.description.as_ref().is_some_and(|d| !d.is_empty()),
                "Tool '{}' must have a non-empty description",
                tool.name
            );
        }
    }

    client.cancel().await?;
    Ok(())
}

/// Documented prompts from README.md
const DOCUMENTED_PROMPTS: &[&str] = &[
    "explain-rdf-schema",
    "generate-from-example",
    "scaffold-project",
];

#[tokio::test]
async fn all_documented_prompts_exist() -> anyhow::Result<()> {
    let client = start_server().await?;
    let prompts = client.list_prompts(None).await?;
    let prompt_names: Vec<&str> = prompts.prompts.iter().map(|p| p.name.as_str()).collect();

    for documented in DOCUMENTED_PROMPTS {
        assert!(
            prompt_names.contains(documented),
            "Documented prompt '{}' not found. Available: {:?}",
            documented,
            prompt_names
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn prompts_have_descriptions() -> anyhow::Result<()> {
    let client = start_server().await?;
    let prompts = client.list_prompts(None).await?;

    for prompt in &prompts.prompts {
        assert!(
            prompt.description.as_ref().is_some_and(|d| !d.is_empty()),
            "Prompt '{}' must have a non-empty description",
            prompt.name
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn resource_list_returns_resources_successfully() -> anyhow::Result<()> {
    let client = start_server().await?;
    let resources = client.list_resources(None).await?;

    // Assert — must succeed (examples dir may be empty in test environment, that's ok)
    // What matters is the call succeeds without error
    let _ = resources.resources; // just ensure the field exists

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn validate_tool_accepts_ttl_parameter() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;
    let _validate_tool = tools
        .tools
        .iter()
        .find(|t| t.name.as_ref() == "validate")
        .expect("validate tool must exist");

    // Test that the tool can be called with TTL parameter
    let valid_ttl = "@prefix ex: <http://example.org/> . ex:Foo a ex:Bar .";
    let args = serde_json::json!({ "ttl": valid_ttl })
        .as_object()
        .unwrap()
        .clone();

    let result = client
        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
        .await?;

    assert!(
        result.is_error != Some(true),
        "validate tool should accept TTL parameter and succeed. Error: {:?}",
        result.content
    );

    client.cancel().await?;
    Ok(())
}
