//! Graph operations example
//!
//! This example demonstrates RDF graph manipulation:
//! - Creating and manipulating RDF graphs
//! - SPARQL query execution
//! - Graph serialization (Turtle, JSON-LD)
//! - Ontology integration and reasoning

use anyhow::{Context, Result};
use ggen_core::Graph;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting graph operations example");

    // Example 1: Basic graph creation and triple insertion
    example_basic_graph().await?;

    // Example 2: SPARQL queries
    example_sparql_queries().await?;

    // Example 3: Graph serialization
    example_graph_serialization().await?;

    // Example 4: Complex graph patterns
    example_complex_patterns().await?;

    // Example 5: Ontology integration
    example_ontology_integration().await?;

    info!("All graph examples completed!");
    Ok(())
}

/// Example 1: Basic graph creation and triple insertion
async fn example_basic_graph() -> Result<()> {
    info!("=== Example 1: Basic Graph Operations ===");

    let mut graph = Graph::new().context("Failed to create graph")?;

    // Define namespaces
    let ex = "http://example.org/";
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#";

    // Add class definitions
    graph
        .add_triple(
            &format!("{}Person", ex),
            &format!("{}type", rdf),
            &format!("{}Class", rdfs),
        )
        .context("Failed to add Person class")?;

    graph
        .add_triple(
            &format!("{}Organization", ex),
            &format!("{}type", rdf),
            &format!("{}Class", rdfs),
        )
        .context("Failed to add Organization class")?;

    // Add property definitions
    graph.add_triple(
        &format!("{}name", ex),
        &format!("{}type", rdf),
        &format!("{}Property", rdf),
    )?;

    graph.add_triple(
        &format!("{}worksFor", ex),
        &format!("{}type", rdf),
        &format!("{}Property", rdf),
    )?;

    // Add instances
    graph.add_triple(
        &format!("{}alice", ex),
        &format!("{}type", rdf),
        &format!("{}Person", ex),
    )?;

    graph.add_triple(
        &format!("{}alice", ex),
        &format!("{}name", ex),
        "\"Alice Smith\"",
    )?;

    graph.add_triple(
        &format!("{}acme", ex),
        &format!("{}type", rdf),
        &format!("{}Organization", ex),
    )?;

    graph.add_triple(
        &format!("{}acme", ex),
        &format!("{}name", ex),
        "\"Acme Corporation\"",
    )?;

    graph.add_triple(
        &format!("{}alice", ex),
        &format!("{}worksFor", ex),
        &format!("{}acme", ex),
    )?;

    info!("Added triples to graph");
    info!("Graph contains {} triples", count_triples(&graph)?);

    Ok(())
}

/// Example 2: SPARQL queries
async fn example_sparql_queries() -> Result<()> {
    info!("=== Example 2: SPARQL Queries ===");

    let mut graph = Graph::new()?;
    let ex = "http://example.org/";
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    // Build a sample knowledge graph
    // People
    graph.add_triple(&format!("{}alice", ex), &format!("{}type", rdf), &format!("{}Person", ex))?;
    graph.add_triple(&format!("{}alice", ex), &format!("{}name", ex), "\"Alice\"")?;
    graph.add_triple(&format!("{}alice", ex), &format!("{}age", ex), "\"30\"")?;

    graph.add_triple(&format!("{}bob", ex), &format!("{}type", rdf), &format!("{}Person", ex))?;
    graph.add_triple(&format!("{}bob", ex), &format!("{}name", ex), "\"Bob\"")?;
    graph.add_triple(&format!("{}bob", ex), &format!("{}age", ex), "\"25\"")?;

    // Query 1: Get all people
    info!("Query 1: Get all people");
    let query1 = format!(
        r#"
        PREFIX ex: <{}>
        PREFIX rdf: <{}>
        SELECT ?person ?name WHERE {{
            ?person rdf:type ex:Person .
            ?person ex:name ?name .
        }}
    "#,
        ex, rdf
    );

    let results1 = graph.query(&query1).context("Query 1 failed")?;
    info!("Found {} people", results1.len());

    // Query 2: Filter by age
    info!("Query 2: Filter people by age >= 30");
    let query2 = format!(
        r#"
        PREFIX ex: <{}>
        PREFIX rdf: <{}>
        SELECT ?person ?name ?age WHERE {{
            ?person rdf:type ex:Person .
            ?person ex:name ?name .
            ?person ex:age ?age .
            FILTER (?age >= "30")
        }}
    "#,
        ex, rdf
    );

    let results2 = graph.query(&query2).context("Query 2 failed")?;
    info!("Found {} people aged >= 30", results2.len());

    // Query 3: Get all triples (SELECT *)
    info!("Query 3: Get all triples");
    let query3 = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
    let results3 = graph.query(query3).context("Query 3 failed")?;
    info!("Total triples in graph: {}", results3.len());

    Ok(())
}

/// Example 3: Graph serialization
async fn example_graph_serialization() -> Result<()> {
    info!("=== Example 3: Graph Serialization ===");

    let mut graph = Graph::new()?;
    let ex = "http://example.org/";
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    // Add some triples
    graph.add_triple(
        &format!("{}project1", ex),
        &format!("{}type", rdf),
        &format!("{}Project", ex),
    )?;

    graph.add_triple(
        &format!("{}project1", ex),
        &format!("{}title", ex),
        "\"My Awesome Project\"",
    )?;

    // Serialize to Turtle format
    info!("Serializing to Turtle format:");
    let turtle = graph.to_turtle().context("Failed to serialize to Turtle")?;
    info!("Turtle output:\n{}", turtle);

    // In a real implementation, you could also serialize to other formats:
    // - JSON-LD
    // - N-Triples
    // - RDF/XML

    Ok(())
}

/// Example 4: Complex graph patterns
async fn example_complex_patterns() -> Result<()> {
    info!("=== Example 4: Complex Graph Patterns ===");

    let mut graph = Graph::new()?;
    let ex = "http://example.org/";
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#";

    // Build a more complex graph: Software development team
    // Projects
    graph.add_triple(&format!("{}proj1", ex), &format!("{}type", rdf), &format!("{}Project", ex))?;
    graph.add_triple(&format!("{}proj1", ex), &format!("{}name", ex), "\"WebApp\"")?;

    graph.add_triple(&format!("{}proj2", ex), &format!("{}type", rdf), &format!("{}Project", ex))?;
    graph.add_triple(&format!("{}proj2", ex), &format!("{}name", ex), "\"MobileApp\"")?;

    // Developers
    graph.add_triple(&format!("{}dev1", ex), &format!("{}type", rdf), &format!("{}Developer", ex))?;
    graph.add_triple(&format!("{}dev1", ex), &format!("{}name", ex), "\"Alice\"")?;
    graph.add_triple(&format!("{}dev1", ex), &format!("{}skill", ex), "\"Rust\"")?;

    graph.add_triple(&format!("{}dev2", ex), &format!("{}type", rdf), &format!("{}Developer", ex))?;
    graph.add_triple(&format!("{}dev2", ex), &format!("{}name", ex), "\"Bob\"")?;
    graph.add_triple(&format!("{}dev2", ex), &format!("{}skill", ex), "\"TypeScript\"")?;

    // Relationships
    graph.add_triple(&format!("{}dev1", ex), &format!("{}worksOn", ex), &format!("{}proj1", ex))?;
    graph.add_triple(&format!("{}dev1", ex), &format!("{}worksOn", ex), &format!("{}proj2", ex))?;
    graph.add_triple(&format!("{}dev2", ex), &format!("{}worksOn", ex), &format!("{}proj1", ex))?;

    // Complex query: Find developers working on multiple projects
    let query = format!(
        r#"
        PREFIX ex: <{}>
        PREFIX rdf: <{}>
        SELECT ?dev ?name (COUNT(?project) as ?projectCount) WHERE {{
            ?dev rdf:type ex:Developer .
            ?dev ex:name ?name .
            ?dev ex:worksOn ?project .
        }}
        GROUP BY ?dev ?name
        HAVING (COUNT(?project) > 1)
    "#,
        ex, rdf
    );

    info!("Complex query: Developers working on multiple projects");
    let results = graph.query(&query).context("Complex query failed")?;
    info!("Found {} multi-project developers", results.len());

    Ok(())
}

/// Example 5: Ontology integration
async fn example_ontology_integration() -> Result<()> {
    info!("=== Example 5: Ontology Integration ===");

    let mut graph = Graph::new()?;
    let ex = "http://example.org/ontology#";
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#";
    let owl = "http://www.w3.org/2002/07/owl#";

    // Define an ontology for a simple domain
    // Class hierarchy
    graph.add_triple(&format!("{}Thing", ex), &format!("{}type", rdf), &format!("{}Class", owl))?;

    graph.add_triple(&format!("{}Person", ex), &format!("{}type", rdf), &format!("{}Class", owl))?;
    graph.add_triple(
        &format!("{}Person", ex),
        &format!("{}subClassOf", rdfs),
        &format!("{}Thing", ex),
    )?;

    graph.add_triple(&format!("{}Employee", ex), &format!("{}type", rdf), &format!("{}Class", owl))?;
    graph.add_triple(
        &format!("{}Employee", ex),
        &format!("{}subClassOf", rdfs),
        &format!("{}Person", ex),
    )?;

    graph.add_triple(&format!("{}Manager", ex), &format!("{}type", rdf), &format!("{}Class", owl))?;
    graph.add_triple(
        &format!("{}Manager", ex),
        &format!("{}subClassOf", rdfs),
        &format!("{}Employee", ex),
    )?;

    // Properties
    graph.add_triple(
        &format!("{}manages", ex),
        &format!("{}type", rdf),
        &format!("{}ObjectProperty", owl),
    )?;

    graph.add_triple(
        &format!("{}manages", ex),
        &format!("{}domain", rdfs),
        &format!("{}Manager", ex),
    )?;

    graph.add_triple(
        &format!("{}manages", ex),
        &format!("{}range", rdfs),
        &format!("{}Employee", ex),
    )?;

    // Query the class hierarchy
    let hierarchy_query = format!(
        r#"
        PREFIX ex: <{}>
        PREFIX rdfs: <{}>
        PREFIX owl: <{}>
        SELECT ?class ?superClass WHERE {{
            ?class rdfs:subClassOf ?superClass .
        }}
    "#,
        ex, rdfs, owl
    );

    info!("Querying class hierarchy:");
    let results = graph
        .query(&hierarchy_query)
        .context("Hierarchy query failed")?;
    info!("Found {} subclass relationships", results.len());

    info!("Ontology created with class hierarchy:");
    info!("  Thing -> Person -> Employee -> Manager");

    Ok(())
}

/// Helper function to count triples in a graph
fn count_triples(graph: &Graph) -> Result<usize> {
    let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
    let results = graph.query(query)?;
    Ok(results.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_basic_graph() {
        let result = example_basic_graph().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_sparql_queries() {
        let result = example_sparql_queries().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_graph_serialization() {
        let result = example_graph_serialization().await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_triple_count() {
        let mut graph = Graph::new().unwrap();
        graph
            .add_triple(
                "http://example.org/s",
                "http://example.org/p",
                "http://example.org/o",
            )
            .unwrap();

        assert_eq!(count_triples(&graph).unwrap(), 1);
    }
}
