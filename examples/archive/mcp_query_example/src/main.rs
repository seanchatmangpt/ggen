//! MCP Query Ontology Tool Example
//!
//! Demonstrates how to call the query_ontology MCP tool via rmcp protocol.
//! This example:
//! 1. Creates a GgenMcpServer instance
//! 2. Spawns it on an in-process duplex transport
//! 3. Calls the query_ontology tool with TTL content and SPARQL SELECT query
//! 4. Prints JSON results
//!
//! Run with: cargo run --example mcp_query_example

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

    let client = ExampleClientHandler::default().serve(client_transport).await?;
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

    println!("🔧 MCP Query Ontology Tool Example");
    println!("==================================\n");

    // Step 1: Start the MCP server
    println!("📡 Starting GgenMcpServer...");
    let client = start_server().await?;
    println!("✅ Server started successfully\n");

    // Step 2: Prepare Turtle content
    let turtle_content = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:Organization a rdfs:Class ;
    rdfs:label "Organization" ;
    rdfs:comment "A formal organization" .

ex:Project a rdfs:Class ;
    rdfs:label "Project" ;
    rdfs:comment "A project or initiative" .

ex:worksFor a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Organization ;
    rdfs:label "works for" .

ex:leads a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Project ;
    rdfs:label "leads" .

ex:memberOf a rdf:Property ;
    rdfs:domain ex:Project ;
    rdfs:range ex:Organization ;
    rdfs:label "member of" .

# Instances
ex:AcmeCorp a ex:Organization ;
    rdfs:label "Acme Corporation" .

ex:StartupXYZ a ex:Organization ;
    rdfs:label "Startup XYZ" .

ex:ProjectAlpha a ex:Project ;
    rdfs:label "Project Alpha" ;
    ex:memberOf ex:AcmeCorp .

ex:ProjectBeta a ex:Project ;
    rdfs:label "Project Beta" ;
    ex:memberOf ex:StartupXYZ .

ex:alice a ex:Person ;
    ex:name "Alice Chen" ;
    ex:worksFor ex:AcmeCorp ;
    ex:leads ex:ProjectAlpha .

ex:bob a ex:Person ;
    ex:name "Bob Smith" ;
    ex:worksFor ex:AcmeCorp .

ex:charlie a ex:Person ;
    ex:name "Charlie Davis" ;
    ex:worksFor ex:StartupXYZ ;
    ex:leads ex:ProjectBeta .
"#;

    println!("📝 Turtle ontology:");
    println!("===================\n");
    println!("{}", turtle_content.lines().take(15).collect::<Vec<_>>().join("\n"));
    println!("... ({} total triples)\n", turtle_content.lines().filter(|l| l.contains(' a ') || l.contains(' a')).count());

    // Step 3: Prepare SPARQL SELECT query
    let sparql_query = r#"
SELECT ?s ?type
WHERE {
    ?s a ?type
}
ORDER BY ?s
"#;

    println!("🔍 SPARQL Query:");
    println!("================\n");
    println!("{}", sparql_query.trim());
    println!("\n");

    // Step 4: Build tool call request
    println!("🔨 Building query_ontology tool call...");
    let query_request = CallToolRequestParams::new("query_ontology")
        .with_arguments({
            let mut map = serde_json::Map::new();
            map.insert("ttl".to_string(), serde_json::Value::String(turtle_content.to_string()));
            map.insert("sparql".to_string(), serde_json::Value::String(sparql_query.to_string()));
            map
        });

    // Step 5: Call the query_ontology tool
    println!("⚙️  Calling query_ontology tool...\n");
    let result = client.call_tool(query_request).await?;

    // Step 6: Extract and display results
    let response_text = extract_text(&result)
        .ok_or_else(|| anyhow::anyhow!("Failed to extract text from tool response"))?;

    println!("📊 Query Results (JSON):");
    println!("========================\n");
    println!("{}\n", response_text);

    // Step 7: Parse and pretty-print the JSON
    if let Ok(json_value) = serde_json::from_str::<serde_json::Value>(&response_text) {
        if let Some(rows) = json_value.get("rows").and_then(|r| r.as_array()) {
            println!("✅ SUCCESS: Found {} result(s)\n", rows.len());
            println!("📈 Parsed Results:");
            println!("==================\n");

            for (idx, row) in rows.iter().enumerate() {
                println!("Result #{}:", idx + 1);
                if let Some(obj) = row.as_object() {
                    for (key, value) in obj {
                        if let Some(str_val) = value.as_str() {
                            // Pretty-print URIs by extracting local name
                            let display_val = if str_val.starts_with("http://example.org/") {
                                format!("ex:{}", &str_val["http://example.org/".len()..])
                            } else if str_val.starts_with("http://www.w3.org/") {
                                // Compact common RDF prefixes
                                if str_val.contains("rdf-schema#") {
                                    format!("rdfs:{}", &str_val[str_val.rfind('#').unwrap()+1..])
                                } else if str_val.contains("rdf-syntax-ns#") {
                                    format!("rdf:{}", &str_val[str_val.rfind('#').unwrap()+1..])
                                } else {
                                    str_val.to_string()
                                }
                            } else {
                                str_val.to_string()
                            };
                            println!("  {} = {}", key, display_val);
                        }
                    }
                }
                println!();
            }

            println!("📊 Summary:");
            println!("  • Tool: query_ontology");
            println!("  • Input: {} bytes of TTL", turtle_content.len());
            println!("  • Query: SELECT ?s ?type WHERE { ?s a ?type }");
            println!("  • Output: {} result rows", rows.len());
        }
    } else {
        println!("⚠️  Could not parse JSON response\n");
    }

    println!("\n🎉 Example completed successfully!");

    Ok(())
}
