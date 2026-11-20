//! MCP Validate Tool Example
//!
//! Demonstrates how to call the validate MCP tool via rmcp protocol.
//! This example:
//! 1. Creates a GgenMcpServer instance
//! 2. Spawns it on an in-process duplex transport
//! 3. Calls the validate tool with Turtle content
//! 4. Prints the triple count result
//!
//! Run with: cargo run --example mcp_validate_example

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct ExampleClientHandler;

impl ClientHandler for ExampleClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Spawn GgenMcpServer on an in-process duplex transport.
async fn start_server() -> anyhow::Result<RunningService<RoleClient, ExampleClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        if let Err(e) = server.serve(server_transport).await {
            eprintln!("Server error: {e}");
        }
    });

    let client = ExampleClientHandler::default()
        .serve(client_transport)
        .await?;
    Ok(client)
}

/// Extract text content from a CallToolResult.
fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

// ---------------------------------------------------------------------------
// Main example
// ---------------------------------------------------------------------------

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("ggen_a2a_mcp=info".parse()?)
                .add_directive("rmcp=info".parse()?),
        )
        .init();

    println!("🔧 MCP Validate Tool Example");
    println!("============================\n");

    // Step 1: Start the MCP server
    println!("📡 Starting GgenMcpServer...");
    let client = start_server().await?;
    println!("✅ Server started successfully\n");

    // Step 2: Prepare Turtle content to validate
    let turtle_content = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:John a ex:Person ;
    ex:name "John Doe" ;
    ex:age 30 ;
    ex:email "john@example.org" .

ex:Jane a ex:Person ;
    ex:name "Jane Smith" ;
    ex:age 25 ;
    ex:email "jane@example.org" .

ex:knows a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Person ;
    rdfs:label "knows" .

ex:John ex:knows ex:Jane .
"#;

    println!("📝 Turtle content to validate:");
    println!(
        "{}\n",
        turtle_content
            .lines()
            .take(10)
            .collect::<Vec<_>>()
            .join("\n")
    );
    println!("... ({} total characters)\n", turtle_content.len());

    // Step 3: Build tool call request
    println!("🔨 Building validate tool call...");
    let validate_request = CallToolRequestParams::new("validate").with_arguments({
        let mut map = serde_json::Map::new();
        map.insert(
            "ttl".to_string(),
            serde_json::Value::String(turtle_content.to_string()),
        );
        map
    });

    // Step 4: Call the validate tool
    println!("⚙️  Calling validate tool...\n");
    let result = client.call_tool(validate_request).await?;

    // Step 5: Extract and display results
    let response_text = extract_text(&result)
        .ok_or_else(|| anyhow::anyhow!("Failed to extract text from tool response"))?;

    println!("📊 Validation Result:");
    println!("=====================\n");
    println!("{}\n", response_text);

    // Step 6: Parse and highlight triple count
    if response_text.contains("Valid Turtle content") {
        if let Some(triple_part) = response_text.split('(').nth(1) {
            if let Some(count_str) = triple_part.split(' ').next() {
                if let Ok(count) = count_str.parse::<usize>() {
                    println!("✅ SUCCESS: Parsed {} triples from Turtle content\n", count);
                    println!("📈 Summary:");
                    println!("  • Tool: validate");
                    println!("  • Input: {} bytes of Turtle", turtle_content.len());
                    println!("  • Output: {} triples", count);
                    println!("  • Status: Valid syntax");
                }
            }
        }
    } else if response_text.contains("Invalid TTL") {
        println!("❌ Validation failed: Invalid Turtle syntax\n");
        println!("Error details: {}", response_text);
    }

    println!("\n🎉 Example completed successfully!");

    Ok(())
}
