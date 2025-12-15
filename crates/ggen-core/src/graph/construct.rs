//! CONSTRUCT query executor
//!
//! Provides specialized operations for SPARQL CONSTRUCT queries,
//! including execution and materialization (inserting results back into the graph).

use crate::graph::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::QueryResults;
use sha2::{Digest, Sha256};

/// Executor for SPARQL CONSTRUCT queries
pub struct ConstructExecutor<'a> {
    /// Reference to the graph
    graph: &'a Graph,
}

impl<'a> ConstructExecutor<'a> {
    /// Create a new CONSTRUCT executor for the given graph
    ///
    /// # Arguments
    /// * `graph` - Reference to the RDF graph
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }

    /// Execute a CONSTRUCT query and return the resulting triples as strings
    ///
    /// # Arguments
    /// * `query` - SPARQL CONSTRUCT query string
    ///
    /// # Returns
    /// * `Ok(Vec<String>)` - Resulting triples in N-Triples format
    /// * `Err(Error)` - Query execution error
    ///
    /// # Example
    /// ```rust,no_run
    /// use ggen_core::graph::{Graph, ConstructExecutor};
    ///
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice ex:knows ex:bob .
    /// "#)?;
    ///
    /// let executor = ConstructExecutor::new(&graph);
    /// let triples = executor.execute(r#"
    ///     CONSTRUCT { ?s ?p ?o }
    ///     WHERE { ?s ?p ?o }
    /// "#)?;
    /// # Ok::<(), ggen_utils::error::Error>(())
    /// ```
    pub fn execute(&self, query: &str) -> Result<Vec<String>> {
        // Execute query using the graph's query method
        let results = self.graph.query(query).map_err(|e| {
            Error::new(&format!("CONSTRUCT query failed: {}", e))
        })?;

        // Handle CONSTRUCT results (Graph variant)
        match results {
            QueryResults::Graph(quads) => {
                let mut triples = Vec::new();
                for quad_result in quads {
                    let quad = quad_result.map_err(|e| {
                        Error::new(&format!("Error reading quad: {}", e))
                    })?;
                    triples.push(quad.to_string());
                }
                Ok(triples)
            }
            QueryResults::Solutions(_) => {
                Err(Error::new("Expected CONSTRUCT query but got SELECT results"))
            }
            QueryResults::Boolean(_) => {
                Err(Error::new("Expected CONSTRUCT query but got ASK results"))
            }
        }
    }

    /// Execute a CONSTRUCT query and insert the results back into the graph
    ///
    /// This implements the "sequential materialization" pattern where each
    /// CONSTRUCT rule's output is materialized before the next rule executes.
    ///
    /// # Arguments
    /// * `query` - SPARQL CONSTRUCT query string
    ///
    /// # Returns
    /// * `Ok(usize)` - Number of triples added
    /// * `Err(Error)` - Query or insert error
    pub fn execute_and_materialize(&self, query: &str) -> Result<usize> {
        // First execute the CONSTRUCT
        let triples = self.execute(query)?;
        let count = triples.len();

        // Convert triples to N-Triples format and insert via Turtle
        // Note: N-Triples is a subset of Turtle, so this works correctly
        if !triples.is_empty() {
            let ntriples = triples.join("\n");
            self.graph.insert_turtle(&ntriples)?;
        }

        Ok(count)
    }

    /// Execute a chain of CONSTRUCT queries in order, materializing after each
    ///
    /// # Arguments
    /// * `queries` - Ordered list of (name, query) pairs
    ///
    /// # Returns
    /// * `Ok(Vec<(String, usize)>)` - List of (rule_name, triples_added)
    /// * `Err(Error)` - First error encountered
    pub fn execute_chain(&self, queries: &[(&str, &str)]) -> Result<Vec<(String, usize)>> {
        let mut results = Vec::new();

        for (name, query) in queries {
            let count = self.execute_and_materialize(query)?;
            results.push((name.to_string(), count));
        }

        Ok(results)
    }

    /// Generate a deterministic IRI from a base and optional salt
    ///
    /// This ensures reproducible IRI generation across runs.
    ///
    /// # Arguments
    /// * `base` - Base IRI prefix
    /// * `components` - Components to include in hash
    /// * `salt` - Optional salt for additional uniqueness
    ///
    /// # Returns
    /// A deterministic IRI string
    pub fn generate_iri(base: &str, components: &[&str], salt: Option<&str>) -> String {
        let mut hasher = Sha256::new();

        // Include base in hash
        hasher.update(base.as_bytes());

        // Include all components
        for component in components {
            hasher.update(component.as_bytes());
        }

        // Include salt if provided
        if let Some(s) = salt {
            hasher.update(s.as_bytes());
        }

        let hash = hasher.finalize();
        // Use first 16 bytes (32 hex chars) for reasonably unique IRI
        let hash_str: String = hash.iter().take(16).map(|b| format!("{:02x}", b)).collect();

        format!("{}{}", base, hash_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_iri_deterministic() {
        let iri1 = ConstructExecutor::generate_iri(
            "http://ggen.dev/code#",
            &["User", "struct"],
            Some("salt-v1"),
        );
        let iri2 = ConstructExecutor::generate_iri(
            "http://ggen.dev/code#",
            &["User", "struct"],
            Some("salt-v1"),
        );
        let iri3 = ConstructExecutor::generate_iri(
            "http://ggen.dev/code#",
            &["Order", "struct"],
            Some("salt-v1"),
        );

        // Same inputs produce same output
        assert_eq!(iri1, iri2);
        // Different inputs produce different output
        assert_ne!(iri1, iri3);
    }

    #[test]
    fn test_generate_iri_format() {
        let iri = ConstructExecutor::generate_iri("http://example.org/", &["test"], None);
        assert!(iri.starts_with("http://example.org/"));
        // Hash portion should be 32 hex chars
        let hash_part = iri.strip_prefix("http://example.org/").unwrap();
        assert_eq!(hash_part.len(), 32);
    }

    #[test]
    fn test_construct_executor_creation() {
        let graph = Graph::new().expect("Failed to create graph");
        let _executor = ConstructExecutor::new(&graph);
        // Just verify construction works
    }

    #[test]
    fn test_execute_simple_construct() {
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#,
            )
            .expect("Failed to insert turtle");

        let executor = ConstructExecutor::new(&graph);
        let result = executor.execute(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        );

        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 1);
    }
}
