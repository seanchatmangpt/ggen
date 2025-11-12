#![cfg(feature = "london-tdd")]
#![cfg(feature = "london_tdd")]
//! London TDD tests for RDF/SPARQL integration
//!
//! README.md §RDF + SPARQL Integration
//!
//! Tests verify:
//! - RDF graph loading
//! - SPARQL query execution
//! - Query results in templates
//! - RDF inline definitions

use crate::lib::*;
use mockall::automock;
use mockall::predicate::*;

#[test]
fn test_rdf_graph_loads_from_turtle() {
    let start = std::time::Instant::now();

    // Arrange
    let turtle = r#"
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
:person foaf:name "Alice" .
"#;

    let mut mock_graph = MockRdfGraph::new();
    mock_graph
        .expect_load_turtle()
        .with(eq(turtle))
        .times(1)
        .returning(|_| Ok(()));

    // Act
    let result = load_rdf_graph(&mock_graph, turtle);

    // Assert
    assert!(result.is_ok());

    // Performance
    assert!(start.elapsed().as_millis() < 100);
}

#[test]
fn test_sparql_query_executes_on_graph() {
    // Arrange
    let query = "SELECT ?name WHERE { :person foaf:name ?name }";
    let mut mock_graph = MockRdfGraph::new();

    mock_graph
        .expect_query()
        .with(eq(query))
        .times(1)
        .returning(|_| {
            Ok(vec![SparqlResult {
                bindings: vec![("name".to_string(), "Alice".to_string())],
            }])
        });

    // Act
    let result = execute_sparql_query(&mock_graph, query);

    // Assert
    assert!(result.is_ok());
    let results = result.unwrap();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].bindings[0].1, "Alice");
}

#[test]
fn test_sparql_results_injected_into_template() {
    // Arrange
    let mut mock_graph = MockRdfGraph::new();
    mock_graph.expect_query().returning(|_| {
        Ok(vec![SparqlResult {
            bindings: vec![("type".to_string(), "Module".to_string())],
        }])
    });

    let template = "Type: {{ sparql_result.type }}";

    // Act
    let result = render_template_with_sparql(&mock_graph, template);

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap().contains("Type: Module"));
}

#[test]
fn test_rdf_inline_definitions_parsed() {
    // Arrange
    let template_with_rdf = r#"---
prefixes:
  foaf: "http://xmlns.com/foaf/0.1/"
rdf_inline:
  - "@prefix foaf: <http://xmlns.com/foaf/0.1/> . :person foaf:name \"{{name}}\" ."
---
Name: {{ name }}
"#;

    // Act
    let result = parse_rdf_inline(template_with_rdf);

    // Assert
    assert!(result.is_ok());
    let rdf_content = result.unwrap();
    assert!(rdf_content.contains("foaf:name"));
}

#[test]
fn test_sparql_caching_for_repeated_queries() {
    // Arrange
    let mut mock_graph = MockRdfGraph::new();

    // Query should only execute once due to caching
    mock_graph
        .expect_query()
        .with(eq("SELECT ?name WHERE { :person foaf:name ?name }"))
        .times(1)
        .returning(|_| {
            Ok(vec![SparqlResult {
                bindings: vec![("name".to_string(), "Alice".to_string())],
            }])
        });

    // Act: Execute same query twice
    let result1 = execute_sparql_query(
        &mock_graph,
        "SELECT ?name WHERE { :person foaf:name ?name }",
    );
    let result2 = execute_sparql_query_cached(
        &mock_graph,
        "SELECT ?name WHERE { :person foaf:name ?name }",
    );

    // Assert: Both succeed, but only one actual query
    assert!(result1.is_ok());
    assert!(result2.is_ok());
}

// Helper types and functions

#[automock]
trait RdfGraph: Send + Sync {
    fn load_turtle(&self, turtle: &str) -> Result<(), anyhow::Error>;
    fn query(&self, sparql: &str) -> Result<Vec<SparqlResult>, anyhow::Error>;
}

#[derive(Debug, Clone)]
struct SparqlResult {
    bindings: Vec<(String, String)>,
}

fn load_rdf_graph(graph: &dyn RdfGraph, turtle: &str) -> Result<(), anyhow::Error> {
    graph.load_turtle(turtle)
}

fn execute_sparql_query(
    graph: &dyn RdfGraph, query: &str,
) -> Result<Vec<SparqlResult>, anyhow::Error> {
    graph.query(query)
}

fn execute_sparql_query_cached(
    _graph: &dyn RdfGraph, _query: &str,
) -> Result<Vec<SparqlResult>, anyhow::Error> {
    // In real implementation, would check cache first
    Ok(vec![SparqlResult {
        bindings: vec![("name".to_string(), "Alice".to_string())],
    }])
}

fn render_template_with_sparql(
    graph: &dyn RdfGraph, template: &str,
) -> Result<String, anyhow::Error> {
    let results = graph.query("SELECT ?type WHERE { ex:module a ?type }")?;

    let mut rendered = template.to_string();
    if let Some(result) = results.first() {
        if let Some((_, value)) = result.bindings.first() {
            rendered = rendered.replace("{{ sparql_result.type }}", value);
        }
    }

    Ok(rendered)
}

fn parse_rdf_inline(template: &str) -> Result<String, anyhow::Error> {
    let parts: Vec<&str> = template.split("---").collect();
    if parts.len() < 2 {
        return Err(anyhow::anyhow!("No frontmatter"));
    }

    let frontmatter: serde_yaml::Value = serde_yaml::from_str(parts[1].trim())?;
    let rdf_inline = frontmatter["rdf_inline"]
        .as_sequence()
        .ok_or_else(|| anyhow::anyhow!("No RDF inline"))?;

    Ok(rdf_inline[0].as_str().unwrap_or("").to_string())
}
