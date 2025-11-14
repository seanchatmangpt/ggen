//! GraphQuery - Advanced query operations
//!
//! Provides advanced SPARQL query building and execution capabilities.

use crate::graph::core::Graph;
use ggen_utils::error::Result;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use std::collections::BTreeMap;

/// GraphQuery provides advanced query building and execution.
///
/// This type provides a fluent interface for building and executing
/// SPARQL queries with additional convenience methods.
///
/// # Examples
///
/// ## Query with prefixes
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphQuery};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// let query = GraphQuery::new(&graph);
///
/// let mut prefixes = BTreeMap::new();
/// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
///
/// let results = query.execute_with_prefixes(
///     "SELECT ?name WHERE { ?s ex:name ?name }",
///     &prefixes,
///     None
/// )?;
/// # Ok(())
/// # }
/// ```
pub struct GraphQuery<'a> {
    graph: &'a Graph,
}

impl<'a> GraphQuery<'a> {
    /// Create a new GraphQuery instance for the given graph.
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }

    /// Execute a SPARQL query with PREFIX and BASE declarations.
    ///
    /// # Arguments
    ///
    /// * `sparql` - SPARQL query string (without PREFIX/BASE)
    /// * `prefixes` - Map of prefix names to namespace URIs
    /// * `base` - Optional base IRI
    pub fn execute_with_prefixes(
        &self, sparql: &str, prefixes: &BTreeMap<String, String>, base: Option<&str>,
    ) -> Result<QueryResults<'a>> {
        self.graph.query_with_prolog(sparql, prefixes, base)
    }

    /// Execute a SPARQL query and return cached results.
    ///
    /// This is a convenience method that calls `graph.query_cached()`.
    pub fn execute_cached(&self, sparql: &str) -> Result<crate::graph::types::CachedResult> {
        self.graph.query_cached(sparql)
    }

    /// Execute a SPARQL query and return raw QueryResults.
    ///
    /// This is a convenience method that calls `graph.query()`.
    pub fn execute(&self, sparql: &str) -> Result<QueryResults<'a>> {
        self.graph.query(sparql)
    }

    /// Execute a prepared SPARQL query.
    ///
    /// This is a convenience method that calls `graph.query_prepared()`.
    pub fn execute_prepared(&self, q: &str) -> Result<QueryResults<'a>> {
        self.graph.query_prepared(q)
    }

    /// Create a query builder for constructing queries programmatically.
    ///
    /// Returns a `SparqlEvaluator` that can be used to build and execute queries.
    /// The evaluator provides a fluent API for constructing SPARQL queries.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::{Graph, GraphQuery};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let graph = Graph::new()?;
    /// let query = GraphQuery::new(&graph);
    ///
    /// // Use the builder to construct and execute a query
    /// let results = query.builder()
    ///     .parse_query("SELECT ?s WHERE { ?s ?p ?o }")?
    ///     .on_store(graph.inner())
    ///     .execute()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn builder(&self) -> SparqlEvaluator {
        SparqlEvaluator::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::core::Graph;
    use chicago_tdd_tools::test;
    use std::collections::BTreeMap;

    test!(test_query_execute_cached, {
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
        let query = GraphQuery::new(&graph);

        // Act
        let result = query
            .execute_cached("SELECT ?name WHERE { ?s ex:name ?name }")
            .unwrap();

        // Assert
        match result {
            crate::graph::types::CachedResult::Solutions(rows) => {
                assert!(!rows.is_empty());
                assert!(rows[0].contains_key("name"));
            }
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_execute, {
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
        let query = GraphQuery::new(&graph);

        // Act
        let results = query
            .execute("SELECT ?name WHERE { ?s ex:name ?name }")
            .unwrap();

        // Assert
        match results {
            QueryResults::Solutions(_) => {}
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_execute_with_prefixes, {
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
        let query = GraphQuery::new(&graph);
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());

        // Act
        let results = query
            .execute_with_prefixes("SELECT ?name WHERE { ?s ex:name ?name }", &prefixes, None)
            .unwrap();

        // Assert
        match results {
            QueryResults::Solutions(_) => {}
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_execute_with_base, {
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
        let query = GraphQuery::new(&graph);
        let prefixes = BTreeMap::new();

        // Act
        let results = query
            .execute_with_prefixes(
                "SELECT ?name WHERE { ?s ex:name ?name }",
                &prefixes,
                Some("http://example.org/"),
            )
            .unwrap();

        // Assert
        match results {
            QueryResults::Solutions(_) => {}
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_execute_prepared, {
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
        let query = GraphQuery::new(&graph);

        // Act
        let results = query
            .execute_prepared("SELECT ?name WHERE { ?s ex:name ?name }")
            .unwrap();

        // Assert
        match results {
            QueryResults::Solutions(_) => {}
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_builder, {
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
        let query = GraphQuery::new(&graph);

        // Act
        let builder = query.builder();
        let results = builder
            .parse_query("SELECT ?name WHERE { ?s ex:name ?name }")
            .unwrap()
            .on_store(graph.inner())
            .execute()
            .unwrap();

        // Assert
        match results {
            QueryResults::Solutions(_) => {}
            _ => panic!("Expected solutions"),
        }
    });

    test!(test_query_ask_query, {
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
        let query = GraphQuery::new(&graph);

        // Act
        let result = query.execute_cached("ASK { ?s a ex:Person }").unwrap();

        // Assert
        match result {
            crate::graph::types::CachedResult::Boolean(true) => {}
            _ => panic!("Expected true"),
        }
    });

    test!(test_query_invalid_syntax, {
        // Arrange
        let graph = Graph::new().unwrap();
        let query = GraphQuery::new(&graph);

        // Act & Assert
        let result = query.execute_cached("INVALID SPARQL SYNTAX");
        assert!(result.is_err());
    });
}
