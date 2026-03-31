//! Direct test of query_ontology MCP tool
//! Tests the tool by creating a GgenMcpServer instance and calling query_ontology

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{
    model::{CallToolRequestParams, ClientCapabilities},
    handler::server::wrapper::Parameters,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Testing query_ontology MCP Tool ===\n");

    // Create server instance
    let server = GgenMcpServer::new();
    println!("✓ GgenMcpServer created\n");

    // Test 1: Simple SELECT query
    println!("--- Test 1: Simple SELECT query ---");
    let ttl1 = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject ex:name "Test Subject" ."#;

    let sparql1 = r#"SELECT ?s WHERE { ?s ex:name ?name }"#;

    println!("TTL:\n{}\n", ttl1);
    println!("SPARQL:\n{}\n", sparql1);

    let result1 = test_query_ontology(&server, ttl1, sparql1).await?;
    println!("{}\n", result1);

    // Test 2: Query with multiple results
    println!("--- Test 2: Multiple results ---");
    let ttl2 = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject1 ex:name "Alice" .
ex:Subject2 ex:name "Bob" .
ex:Subject3 ex:name "Charlie" ."#;

    let sparql2 = r#"SELECT ?s ?name WHERE { ?s ex:name ?name } ORDER BY ?name"#;

    println!("TTL:\n{}\n", ttl2);
    println!("SPARQL:\n{}\n", sparql2);

    let result2 = test_query_ontology(&server, ttl2, sparql2).await?;
    println!("{}\n", result2);

    // Test 3: Complex query with predicates
    println!("--- Test 3: Complex query ---");
    let ttl3 = r#"@prefix ex: <http://example.org/ns#> .
ex:Person1 a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 ;
    ex:knows ex:Person2 .
ex:Person2 a ex:Person ;
    ex:name "Bob" ;
    ex:age 25 ."#;

    let sparql3 = r#"
        SELECT ?person ?name ?age
        WHERE {
            ?person a ex:Person ;
                     ex:name ?name ;
                     ex:age ?age .
        }
        ORDER BY DESC(?age)
    "#;

    println!("TTL:\n{}\n", ttl3);
    println!("SPARQL:\n{}\n", sparql3);

    let result3 = test_query_ontology(&server, ttl3, sparql3).await?;
    println!("{}\n", result3);

    println!("=== All tests complete ===");
    Ok(())
}

async fn test_query_ontology(
    server: &GgenMcpServer,
    ttl: &str,
    sparql: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    use ggen_a2a_mcp::ggen_server::QueryOntologyParams;
    use rmcp::handler::server::wrapper::Parameters;

    // Direct method call (should work if #[tool] macro makes it public)
    let params = QueryOntologyParams {
        ttl: ttl.to_string(),
        sparql: sparql.to_string(),
    };

    match server.query_ontology(Parameters(params)).await {
        Ok(result) => {
            if let Some(content) = result.content.first() {
                if let Some(text) = content.as_text() {
                    return Ok(text.clone());
                }
            }
        }
        Err(e) => {
            return Err(format!("Tool call failed: {:?}", e).into());
        }
    }

    Ok("<no text>".to_string())
}
