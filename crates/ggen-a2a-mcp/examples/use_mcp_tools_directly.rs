//! Direct usage of ggen MCP tools via rmcp protocol
//!
//! This example demonstrates using MCP tools via rmcp protocol,
//! similar to how you would use Desktop Commander tools.
//!
//! Run with:
//!   RUST_LOG=trace,ggen_a2a_mcp=trace cargo run -p ggen-a2a-mcp --example use_mcp_tools_directly

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing to capture OTEL spans
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("ggen_a2a_mcp=trace".parse()?)
                .add_directive("ggen_core=trace".parse()?),
        )
        .init();

    println!("=== USING GGEN MCP TOOLS VIA RMCP PROTOCOL ===\n");

    let client = start_server().await?;

    // Tool 1: validate
    println!("🔧 Tool 1: validate");
    println!("   Validating Turtle syntax...\n");

    let valid_ttl = r#"
@prefix ex: <http://example.org/ns#> .
ex:Subject a ex:Resource ;
    ex:name "Test" .
"#;

    let args = serde_json::json!({ "ttl": valid_ttl })
        .as_object()
        .unwrap()
        .clone();

    let validate_result = client
        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
        .await?;

    let validate_text = validate_result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });

    println!("✅ VALIDATE RESULT:");
    println!("{}\n", validate_text.unwrap());

    // Tool 2: query_ontology
    println!("🔧 Tool 2: query_ontology");
    println!("   Querying SPARQL...\n");

    let ttl_content =
        std::fs::read_to_string("examples/basic-template-generation/ontology/templates.ttl")?;

    let args = serde_json::json!({
        "ttl": ttl_content,
        "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 3"
    })
    .as_object()
    .unwrap()
    .clone();

    let query_result = client
        .call_tool(CallToolRequestParams::new("query_ontology").with_arguments(args))
        .await?;

    let query_text = query_result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });

    println!("✅ QUERY_ONTOLOGY RESULT:");
    println!("{}\n", query_text.unwrap());

    // Tool 3: list_generators
    println!("🔧 Tool 3: list_generators");
    println!("   Listing code generators...\n");

    let list_result = client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await?;

    let list_text = list_result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    });

    println!("✅ LIST_GENERATORS RESULT:");
    println!("{}\n", list_text.unwrap());

    println!("=== ALL MCP TOOLS EXECUTED VIA RMCP PROTOCOL ===");
    println!("\n📊 OTEL spans were emitted during execution.");
    println!("   Check logs above for:");
    println!("   - operation.name = validate|query_ontology|list_generators");
    println!("   - mcp.tool.name attributes");
    println!("   - mcp.tool.duration_ms timing");

    client.cancel().await?;

    Ok(())
}
