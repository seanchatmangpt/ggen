//! SPARQL query execution domain logic with real Oxigraph operations
//!
//! Chicago TDD: Uses REAL in-memory RDF stores and ACTUAL SPARQL queries
//! v4.0.0: Queries persistent GraphStore for data loaded across sessions

use ggen_core::{Graph, GraphStore};
use ggen_utils::error::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Options for SPARQL query execution
#[derive(Debug, Clone)]
pub struct QueryOptions {
    /// SPARQL query string
    pub query: String,
    /// Optional graph file to load and query
    pub graph_file: Option<String>,
    /// Output format (json, csv, table)
    pub output_format: String,
}

/// Result from SPARQL query execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryResult {
    /// Variable bindings from query results
    pub bindings: Vec<HashMap<String, String>>,
    /// Variable names from SELECT query
    pub variables: Vec<String>,
    /// Number of results returned
    pub result_count: usize,
}

/// Query input options (pure domain type - no CLI dependencies)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct QueryInput {
    /// SPARQL query string
    pub query: String,

    /// RDF graph file to query
    pub graph_file: Option<PathBuf>,

    /// Output format (json, csv, table)
    pub format: String,
}

impl QueryInput {
    pub fn new(query: String) -> Self {
        Self {
            query,
            format: "table".to_string(),
            ..Default::default()
        }
    }
}

impl QueryResult {
    /// Create new empty query result
    pub fn empty() -> Self {
        Self {
            bindings: Vec::new(),
            variables: Vec::new(),
            result_count: 0,
        }
    }

    /// Create query result from bindings
    pub fn from_bindings(bindings: Vec<HashMap<String, String>>, variables: Vec<String>) -> Self {
        let result_count = bindings.len();
        Self {
            bindings,
            variables,
            result_count,
        }
    }
}

/// Execute SPARQL query against RDF graph
///
/// Chicago TDD: This executes REAL SPARQL queries using Oxigraph
/// v4.0.0: Queries persistent store if no file specified
pub fn execute_sparql(options: QueryOptions) -> Result<QueryResult> {
    // Load graph if file provided, otherwise use persistent store
    let graph = if let Some(graph_file) = &options.graph_file {
        // One-time query: load file into temporary in-memory graph
        Graph::load_from_file(graph_file)
            .context(format!("Failed to load graph from file: {}", graph_file))?
    } else {
        // Query persistent store (data loaded via `ggen graph load`)
        let store = GraphStore::open(".ggen/rdf-store")
            .context("Failed to open persistent RDF store at .ggen/rdf-store")?;
        store
            .create_graph()
            .context("Failed to create graph from persistent store")?
    };

    // Execute REAL SPARQL query using Oxigraph
    let query_results = graph
        .query(&options.query)
        .context("Failed to execute SPARQL query")?;

    // Convert Oxigraph results to our domain model
    match query_results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            let variables: Vec<String> = solutions
                .variables()
                .iter()
                .map(|v| v.to_string())
                .collect();

            let mut bindings = Vec::new();
            for solution in solutions {
                let solution = solution
                    .map_err(|e| {
                        ggen_utils::error::Error::new(&format!(
                            "Failed to process SPARQL solution: {}",
                            e
                        ))
                    })
                    .context("Failed to process SPARQL solution")?;
                let mut binding = HashMap::new();

                for variable in &variables {
                    // Oxigraph solution.get() expects variable name without ? prefix
                    let var_name = variable.strip_prefix('?').unwrap_or(variable);
                    if let Some(value) = solution.get(var_name) {
                        binding.insert(variable.clone(), value.to_string());
                    }
                }
                bindings.push(binding);
            }

            Ok(QueryResult::from_bindings(bindings, variables))
        }
        oxigraph::sparql::QueryResults::Boolean(result) => {
            // For ASK queries, return boolean result as binding
            let mut binding = HashMap::new();
            binding.insert("result".to_string(), result.to_string());

            Ok(QueryResult::from_bindings(
                vec![binding],
                vec!["result".to_string()],
            ))
        }
        oxigraph::sparql::QueryResults::Graph(_) => {
            // For CONSTRUCT/DESCRIBE queries, return basic info
            Ok(QueryResult::from_bindings(
                vec![],
                vec!["graph".to_string()],
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Chicago TDD: Test with REAL in-memory RDF graph
    #[test]
    fn test_execute_sparql_with_real_graph() -> Result<()> {
        // Create REAL RDF graph with Oxigraph
        let graph = Graph::new()?;

        // Insert REAL RDF triples
        let turtle = r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .

            ex:alice a foaf:Person ;
                foaf:name "Alice" ;
                foaf:age "30" .

            ex:bob a foaf:Person ;
                foaf:name "Bob" ;
                foaf:age "25" .
        "#;

        graph.insert_turtle(turtle)?;

        // Create temp file with RDF data
        let temp_file = tempfile::Builder::new().suffix(".ttl").tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        // Execute REAL SPARQL query
        let options = QueryOptions {
            query: r#"
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?name ?age
                WHERE {
                    ?person foaf:name ?name ;
                           foaf:age ?age .
                }
                ORDER BY ?name
            "#
            .to_string(),
            graph_file: Some(temp_path),
            output_format: "json".to_string(),
        };

        let result = execute_sparql(options)?;

        // Verify REAL query results (SPARQL variables include ? prefix)
        assert_eq!(result.variables, vec!["?name", "?age"]);
        assert_eq!(result.result_count, 2);
        assert_eq!(result.bindings.len(), 2);

        // Verify actual data (bindings also use ? prefix)
        assert!(result.bindings[0].get("?name").unwrap().contains("Alice"));
        assert!(result.bindings[1].get("?name").unwrap().contains("Bob"));

        Ok(())
    }

    /// Chicago TDD: Test ASK query with REAL boolean result
    #[test]
    fn test_execute_ask_query_with_real_graph() -> Result<()> {
        let graph = Graph::new()?;

        let turtle = r#"
            @prefix ex: <http://example.org/> .
            ex:subject ex:predicate ex:object .
        "#;

        graph.insert_turtle(turtle)?;

        let temp_file = tempfile::Builder::new().suffix(".ttl").tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        // Execute REAL ASK query
        let options = QueryOptions {
            query: "ASK { ?s ?p ?o }".to_string(),
            graph_file: Some(temp_path),
            output_format: "json".to_string(),
        };

        let result = execute_sparql(options)?;

        // Verify boolean result
        assert_eq!(result.variables, vec!["result"]);
        assert!(result.bindings[0].get("result").unwrap().contains("true"));

        Ok(())
    }

    /// Chicago TDD: Test empty graph returns no results
    #[test]
    fn test_execute_sparql_empty_graph() -> Result<()> {
        // Execute query on empty graph
        let options = QueryOptions {
            query: "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
            graph_file: None,
            output_format: "json".to_string(),
        };

        let result = execute_sparql(options)?;

        // Verify no results from empty graph
        assert_eq!(result.result_count, 0);
        assert_eq!(result.bindings.len(), 0);

        Ok(())
    }

    /// Chicago TDD: Test filtering with REAL SPARQL FILTER
    #[test]
    fn test_execute_sparql_with_filter() -> Result<()> {
        let graph = Graph::new()?;

        let turtle = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:age "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
            ex:bob ex:age "25"^^<http://www.w3.org/2001/XMLSchema#integer> .
            ex:charlie ex:age "35"^^<http://www.w3.org/2001/XMLSchema#integer> .
        "#;

        graph.insert_turtle(turtle)?;

        let temp_file = tempfile::Builder::new().suffix(".ttl").tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        // Execute REAL SPARQL query with FILTER
        let options = QueryOptions {
            query: r#"
                PREFIX ex: <http://example.org/>
                PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                SELECT ?person ?age
                WHERE {
                    ?person ex:age ?age .
                    FILTER(?age > "28"^^xsd:integer)
                }
            "#
            .to_string(),
            graph_file: Some(temp_path),
            output_format: "json".to_string(),
        };

        let result = execute_sparql(options)?;

        // Verify FILTER works correctly
        assert_eq!(result.result_count, 2); // alice and charlie
        assert_eq!(result.bindings.len(), 2);

        Ok(())
    }
}

/// Execute query with input (pure domain function)
pub async fn execute_query(input: QueryInput) -> Result<QueryResult> {
    let options = QueryOptions {
        query: input.query,
        graph_file: input
            .graph_file
            .as_ref()
            .map(|p| p.to_string_lossy().to_string()),
        output_format: input.format,
    };

    execute_sparql(options)
        .map_err(|e| ggen_utils::error::Error::new(&format!("SPARQL query failed: {}", e)))
}
