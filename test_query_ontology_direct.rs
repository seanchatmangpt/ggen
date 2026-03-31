//! Direct test of query_ontology MCP tool
//! Tests the tool by creating a GgenMcpServer instance and calling query_ontology

use ggen_a2a_mcp::ggen_server::{GgenMcpServer, QueryOntologyParams};
use rmcp::handler::server::wrapper::Parameters;

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

    let params1 = QueryOntologyParams {
        ttl: ttl1.to_string(),
        sparql: sparql1.to_string(),
    };

    match server.query_ontology(Parameters(params1)).await {
        Ok(result) => {
            println!("Result: {:?}", result);
            if let Some(content) = result.content.first() {
                println!("Output: {}\n", content.text.as_ref().unwrap_or(&"<no text>".to_string()));
            }
        }
        Err(e) => {
            println!("Error: {:?}\n", e);
        }
    }

    // Test 2: Query with multiple results
    println!("--- Test 2: Multiple results ---");
    let ttl2 = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject1 ex:name "Alice" .
ex:Subject2 ex:name "Bob" .
ex:Subject3 ex:name "Charlie" ."#;

    let sparql2 = r#"SELECT ?s ?name WHERE { ?s ex:name ?name } ORDER BY ?name"#;

    println!("TTL:\n{}\n", ttl2);
    println!("SPARQL:\n{}\n", sparql2);

    let params2 = QueryOntologyParams {
        ttl: ttl2.to_string(),
        sparql: sparql2.to_string(),
    };

    match server.query_ontology(Parameters(params2)).await {
        Ok(result) => {
            println!("Result: {:?}", result);
            if let Some(content) = result.content.first() {
                println!("Output: {}\n", content.text.as_ref().unwrap_or(&"<no text>".to_string()));
            }
        }
        Err(e) => {
            println!("Error: {:?}\n", e);
        }
    }

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

    let params3 = QueryOntologyParams {
        ttl: ttl3.to_string(),
        sparql: sparql3.to_string(),
    };

    match server.query_ontology(Parameters(params3)).await {
        Ok(result) => {
            println!("Result: {:?}", result);
            if let Some(content) = result.content.first() {
                println!("Output: {}\n", content.text.as_ref().unwrap_or(&"<no text>".to_string()));
            }
        }
        Err(e) => {
            println!("Error: {:?}\n", e);
        }
    }

    println!("=== All tests complete ===");
    Ok(())
}
