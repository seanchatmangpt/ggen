use rmcp::{Client, StdioTransport};
use serde_json::json;
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("🚀 ggen MCP Rust Client Example");
    println!("================================\n");

    // Connect to ggen MCP server via stdio
    let transport = StdioTransport::new("ggen", vec!["mcp", "start"]);
    let mut client = Client::new(transport).await?;

    println!("✓ Connected to ggen MCP server\n");

    // Example 1: List available templates
    println!("📋 Example 1: List Templates");
    println!("----------------------------");
    let templates = client.call_tool(
        "ggen_template_list",
        json!({})
    ).await?;
    println!("Available templates: {:#?}\n", templates);

    // Example 2: Search marketplace
    println!("🔍 Example 2: Search Marketplace");
    println!("--------------------------------");
    let search_results = client.call_tool(
        "ggen_market_search",
        json!({
            "query": "rust cli",
            "limit": 3
        })
    ).await?;
    println!("Search results: {:#?}\n", search_results);

    // Example 3: Generate code from template
    println!("⚙️  Example 3: Generate Code");
    println!("---------------------------");
    let gen_result = client.call_tool(
        "ggen_gen_with_vars",
        json!({
            "template": "templates/rust-struct.tmpl",
            "vars": {
                "name": "User",
                "fields": ["id", "email", "name"],
                "derive": ["Debug", "Clone", "Serialize", "Deserialize"],
                "determinism": 42
            },
            "output": "/tmp/ggen-example-user.rs"
        })
    ).await?;
    println!("Generation result: {:#?}\n", gen_result);

    // Example 4: RDF graph operations
    println!("🔗 Example 4: RDF Graph Query");
    println!("-----------------------------");

    // Load RDF data
    client.call_tool(
        "ggen_graph_load",
        json!({
            "content": "@prefix ex: <http://example.org/> . ex:User a ex:Entity ; ex:hasField \"id\", \"email\" .",
            "format": "turtle"
        })
    ).await?;
    println!("✓ RDF data loaded");

    // Execute SPARQL query
    let sparql_result = client.call_tool(
        "ggen_graph_query",
        json!({
            "query": "PREFIX ex: <http://example.org/> SELECT ?entity ?field WHERE { ?entity a ex:Entity ; ex:hasField ?field }"
        })
    ).await?;
    println!("SPARQL results: {:#?}\n", sparql_result);

    // Example 5: Get marketplace info
    println!("ℹ️  Example 5: Gpack Information");
    println!("-------------------------------");
    let gpack_info = client.call_tool(
        "ggen_market_info",
        json!({
            "gpack": "io.ggen.rust.cli-subcommand"
        })
    ).await?;
    println!("Gpack info: {:#?}\n", gpack_info);

    println!("✅ All examples completed successfully!");

    Ok(())
}
