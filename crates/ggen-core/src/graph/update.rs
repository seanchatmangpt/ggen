//! GraphUpdate - SPARQL Update operations
//!
//! Provides SPARQL 1.1 Update operations: INSERT, DELETE, UPDATE, LOAD, CLEAR, etc.

use crate::graph::core::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::SparqlEvaluator;

/// GraphUpdate provides SPARQL Update operations.
///
/// SPARQL Update allows modifying RDF graphs using declarative update operations.
///
/// # Examples
///
/// ## Insert data
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphUpdate};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// let update = GraphUpdate::new(&graph);
///
/// // Insert new triples (without INSERT DATA wrapper)
/// update.insert("<http://example.org/alice> <http://example.org/name> \"Alice\"")?;
/// # Ok(())
/// # }
/// ```
///
/// ## Delete data
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphUpdate};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// let update = GraphUpdate::new(&graph);
///
/// // Delete triples matching a pattern (without DELETE DATA wrapper)
/// update.delete("<http://example.org/alice> <http://example.org/name> \"Alice\"")?;
/// # Ok(())
/// # }
/// ```
///
/// ## Update with WHERE clause
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphUpdate};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// let update = GraphUpdate::new(&graph);
///
/// // Delete and insert in one operation
/// update.update(r#"
///     DELETE { ?s <http://example.org/old> ?o }
///     INSERT { ?s <http://example.org/new> ?o }
///     WHERE { ?s <http://example.org/old> ?o }
/// "#)?;
/// # Ok(())
/// # }
/// ```
pub struct GraphUpdate<'a> {
    graph: &'a Graph,
}

impl<'a> GraphUpdate<'a> {
    /// Create a new GraphUpdate instance for the given graph.
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }

    /// Execute a SPARQL Update operation.
    ///
    /// Supports all SPARQL 1.1 Update operations:
    /// - INSERT DATA
    /// - DELETE DATA
    /// - DELETE/INSERT (with WHERE)
    /// - LOAD
    /// - CLEAR
    /// - COPY
    /// - MOVE
    /// - ADD
    ///
    /// # Arguments
    ///
    /// * `update` - SPARQL Update string
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The update syntax is invalid
    /// - The update cannot be executed
    pub fn execute(&self, update: &str) -> Result<()> {
        let evaluator = SparqlEvaluator::new();
        let prepared = evaluator
            .parse_update(update)
            .map_err(|e| Error::with_source("SPARQL Update parse error", Box::new(e)))?;
        prepared
            .on_store(self.graph.inner())
            .execute()
            .map_err(|e| Error::with_source("SPARQL Update execution error", Box::new(e)))?;
        self.graph.bump_epoch();
        Ok(())
    }

    /// Insert data into the graph.
    ///
    /// Shorthand for `INSERT DATA { ... }`.
    ///
    /// # Arguments
    ///
    /// * `data` - SPARQL data block (triples without INSERT DATA wrapper)
    pub fn insert(&self, data: &str) -> Result<()> {
        let update = format!("INSERT DATA {{ {} }}", data);
        self.execute(&update)
    }

    /// Delete data from the graph.
    ///
    /// Shorthand for `DELETE DATA { ... }`.
    ///
    /// # Arguments
    ///
    /// * `data` - SPARQL data block (triples without DELETE DATA wrapper)
    pub fn delete(&self, data: &str) -> Result<()> {
        let update = format!("DELETE DATA {{ {} }}", data);
        self.execute(&update)
    }

    /// Delete triples matching a WHERE pattern.
    ///
    /// Shorthand for `DELETE WHERE { ... }`.
    ///
    /// # Arguments
    ///
    /// * `pattern` - SPARQL WHERE pattern (without DELETE WHERE wrapper)
    pub fn delete_where(&self, pattern: &str) -> Result<()> {
        let update = format!("DELETE WHERE {{ {} }}", pattern);
        self.execute(&update)
    }

    /// Update data (DELETE + INSERT with WHERE).
    ///
    /// Shorthand for `DELETE { ... } INSERT { ... } WHERE { ... }`.
    ///
    /// # Arguments
    ///
    /// * `delete_pattern` - Pattern to delete
    /// * `insert_pattern` - Pattern to insert
    /// * `where_pattern` - WHERE clause pattern
    pub fn update(
        &self, delete_pattern: &str, insert_pattern: &str, where_pattern: &str,
    ) -> Result<()> {
        let update = format!(
            "DELETE {{ {} }} INSERT {{ {} }} WHERE {{ {} }}",
            delete_pattern, insert_pattern, where_pattern
        );
        self.execute(&update)
    }

    /// Clear the default graph.
    ///
    /// Removes all triples from the default graph.
    pub fn clear(&self) -> Result<()> {
        self.execute("CLEAR DEFAULT")
    }

    /// Clear a named graph.
    ///
    /// # Arguments
    ///
    /// * `graph_iri` - IRI of the graph to clear
    pub fn clear_graph(&self, graph_iri: &str) -> Result<()> {
        let update = format!("CLEAR GRAPH <{}>", graph_iri);
        self.execute(&update)
    }

    /// Clear all graphs (default + named).
    pub fn clear_all(&self) -> Result<()> {
        self.execute("CLEAR ALL")
    }

    /// Load data from a URL into the default graph.
    ///
    /// # Arguments
    ///
    /// * `url` - URL to load RDF data from
    pub fn load(&self, url: &str) -> Result<()> {
        let update = format!("LOAD <{}>", url);
        self.execute(&update)
    }

    /// Load data from a URL into a named graph.
    ///
    /// # Arguments
    ///
    /// * `url` - URL to load RDF data from
    /// * `graph_iri` - IRI of the target graph
    pub fn load_into(&self, url: &str, graph_iri: &str) -> Result<()> {
        let update = format!("LOAD <{}> INTO GRAPH <{}>", url, graph_iri);
        self.execute(&update)
    }

    /// Copy all data from source graph to destination graph.
    ///
    /// # Arguments
    ///
    /// * `source` - Source graph IRI
    /// * `destination` - Destination graph IRI
    pub fn copy(&self, source: &str, destination: &str) -> Result<()> {
        let update = format!("COPY <{}> TO <{}>", source, destination);
        self.execute(&update)
    }

    /// Move all data from source graph to destination graph.
    ///
    /// # Arguments
    ///
    /// * `source` - Source graph IRI
    /// * `destination` - Destination graph IRI
    pub fn move_graph(&self, source: &str, destination: &str) -> Result<()> {
        let update = format!("MOVE <{}> TO <{}>", source, destination);
        self.execute(&update)
    }

    /// Add all data from source graph to destination graph.
    ///
    /// # Arguments
    ///
    /// * `source` - Source graph IRI
    /// * `destination` - Destination graph IRI
    pub fn add(&self, source: &str, destination: &str) -> Result<()> {
        let update = format!("ADD <{}> TO <{}>", source, destination);
        self.execute(&update)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::core::Graph;

    #[test]
    fn test_update_insert() {
        // Arrange
        let graph = Graph::new().unwrap();
        let update = GraphUpdate::new(&graph);

        // Act
        update
            .insert("<http://example.org/alice> <http://example.org/name> \"Alice\"")
            .unwrap();

        // Assert
        assert!(!graph.is_empty());
        let results = graph
            .query_cached(
                "SELECT ?name WHERE { <http://example.org/alice> <http://example.org/name> ?name }",
            )
            .unwrap();
        match results {
            crate::graph::types::CachedResult::Solutions(rows) => {
                assert!(!rows.is_empty());
                assert_eq!(rows[0].get("name"), Some(&"\"Alice\"".to_string()));
            }
            _ => panic!("Expected solutions"),
        }
    }

    #[test]
    fn test_update_delete() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" .
        "#,
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);
        let initial_len = graph.len();

        // Act
        update
            .delete("<http://example.org/alice> <http://example.org/name> \"Alice\"")
            .unwrap();

        // Assert
        assert!(graph.len() < initial_len);
    }

    #[test]
    fn test_update_delete_where() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" .
            ex:bob ex:name "Bob" .
        "#,
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);
        let initial_len = graph.len();

        // Act
        update
            .delete_where("?s <http://example.org/name> \"Alice\"")
            .unwrap();

        // Assert
        assert!(graph.len() < initial_len);
        let results = graph
            .query_cached("ASK { <http://example.org/alice> <http://example.org/name> \"Alice\" }")
            .unwrap();
        match results {
            crate::graph::types::CachedResult::Boolean(false) => {}
            _ => panic!("Expected false (triple should be deleted)"),
        }
    }

    #[test]
    fn test_update_update_operation() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:age "30" .
        "#,
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);

        // Act
        update
            .update("?s ex:age ?o", "?s ex:age \"31\"", "?s ex:age ?o")
            .unwrap();

        // Assert
        let results = graph
            .query_cached(
                "SELECT ?age WHERE { <http://example.org/alice> <http://example.org/age> ?age }",
            )
            .unwrap();
        match results {
            crate::graph::types::CachedResult::Solutions(rows) => {
                assert!(!rows.is_empty());
                assert_eq!(rows[0].get("age"), Some(&"\"31\"".to_string()));
            }
            _ => panic!("Expected solutions"),
        }
    }

    #[test]
    fn test_update_clear() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);
        assert!(!graph.is_empty());

        // Act
        update.clear().unwrap();

        // Assert
        assert!(graph.is_empty());
    }

    #[test]
    fn test_update_clear_graph() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle_in(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
                "http://example.org/graph1",
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);
        let initial_len = graph.len();

        // Act
        update.clear_graph("http://example.org/graph1").unwrap();

        // Assert
        assert!(graph.len() < initial_len);
    }

    #[test]
    fn test_update_clear_all() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);
        assert!(!graph.is_empty());

        // Act
        update.clear_all().unwrap();

        // Assert
        assert!(graph.is_empty());
    }

    #[test]
    fn test_update_execute_invalid_syntax() {
        // Arrange
        let graph = Graph::new().unwrap();
        let update = GraphUpdate::new(&graph);

        // Act & Assert
        let result = update.execute("INVALID UPDATE SYNTAX");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("SPARQL Update"));
    }

    #[test]
    fn test_update_copy() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle_in(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
                "http://example.org/graph1",
            )
            .unwrap();
        let update = GraphUpdate::new(&graph);

        // Act
        update
            .copy("http://example.org/graph1", "http://example.org/graph2")
            .unwrap();

        // Assert - Both graphs should have data
        let results1 = graph
            .query_cached("ASK { GRAPH <http://example.org/graph1> { ?s ?p ?o } }")
            .unwrap();
        let results2 = graph
            .query_cached("ASK { GRAPH <http://example.org/graph2> { ?s ?p ?o } }")
            .unwrap();
        match (results1, results2) {
            (
                crate::graph::types::CachedResult::Boolean(true),
                crate::graph::types::CachedResult::Boolean(true),
            ) => {}
            _ => panic!("Both graphs should have data after copy"),
        }
    }
}
