//! Direct test of query_ontology MCP tool (not via cargo test)

use ggen_a2a_mcp::ggen_server::{GgenMcpServer, QueryOntologyParams};
use rmcp::handler::server::wrapper::Parameters;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("Testing query_ontology MCP tool directly\n");
    println!("==========================================\n");

    // Create server instance
    let server = GgenMcpServer::new();

    // Test 1: Simple SELECT query
    println!("Test 1: Simple SELECT query");
    println!("----------------------------\n");

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
            println!("Result:\n{}\n", result.content[0].text);
        }
        Err(e) => {
            println!("Error: {}\n", e);
        }
    }

    // Test 2: More complex query with multiple bindings
    println!("\nTest 2: Query with multiple variable bindings");
    println!("---------------------------------------------\n");

    let ttl2 = r#"@prefix ex: <http://example.org/ns#> .
ex:Person1 ex:name "Alice" ; ex:age 30 .
ex:Person2 ex:name "Bob" ; ex:age 25 ."#;

    let sparql2 = r#"SELECT ?person ?name ?age WHERE {
    ?person ex:name ?name ;
             ex:age ?age .
} ORDER BY ?name"#;

    println!("TTL:\n{}\n", ttl2);
    println!("SPARQL:\n{}\n", sparql2);

    let params2 = QueryOntologyParams {
        ttl: ttl2.to_string(),
        sparql: sparql2.to_string(),
    };

    match server.query_ontology(Parameters(params2)).await {
        Ok(result) => {
            println!("Result:\n{}\n", result.content[0].text);
        }
        Err(e) => {
            println!("Error: {}\n", e);
        }
    }

    // Test 3: Query with FILTER
    println!("\nTest 3: Query with FILTER");
    println!("-------------------------\n");

    let ttl3 = r#"@prefix ex: <http://example.org/ns#> .
ex:Item1 ex:price 100 .
ex:Item2 ex:price 50 .
ex:Item3 ex:price 75 ."#;

    let sparql3 = r#"SELECT ?item ?price WHERE {
    ?item ex:price ?price .
    FILTER(?price > 60)
} ORDER BY DESC(?price)"#;

    println!("TTL:\n{}\n", ttl3);
    println!("SPARQL:\n{}\n", sparql3);

    let params3 = QueryOntologyParams {
        ttl: ttl3.to_string(),
        sparql: sparql3.to_string(),
    };

    match server.query_ontology(Parameters(params3)).await {
        Ok(result) => {
            println!("Result:\n{}\n", result.content[0].text);
        }
        Err(e) => {
            println!("Error: {}\n", e);
        }
    }

    println!("==========================================");
    println!("All tests completed!");

    Ok(())
}
