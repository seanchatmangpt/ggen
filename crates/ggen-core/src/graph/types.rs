//! Shared types for graph operations

use serde_json::Value as JsonValue;
use std::collections::BTreeMap;

/// Cached SPARQL query result types.
///
/// This enum represents the different types of results that can be returned
/// from SPARQL queries and cached for efficient reuse.
///
/// # Variants
///
/// * `Boolean(bool)` - Result of an ASK query (true/false)
/// * `Solutions(Vec<BTreeMap<String, String>>)` - Result of a SELECT query (rows of variable bindings)
/// * `Graph(Vec<String>)` - Result of a CONSTRUCT/DESCRIBE query (serialized triples)
///
/// # Examples
///
/// ## Boolean result
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, CachedResult};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// let result = graph.query_cached("ASK { ?s a ex:Person }")?;
/// if let CachedResult::Boolean(true) = result {
///     println!("Person exists in graph");
/// }
/// # Ok(())
/// # }
/// ```
///
/// ## Solutions result
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, CachedResult};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice ex:name "Alice" .
/// "#)?;
///
/// let result = graph.query_cached("SELECT ?name WHERE { ?s ex:name ?name }")?;
/// if let CachedResult::Solutions(rows) = result {
///     for row in rows {
///         if let Some(name) = row.get("name") {
///             println!("Name: {}", name);
///         }
///     }
/// }
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
pub enum CachedResult {
    /// Boolean result from ASK queries
    Boolean(bool),
    /// Solution bindings from SELECT queries
    Solutions(Vec<BTreeMap<String, String>>),
    /// Serialized triples from CONSTRUCT/DESCRIBE queries
    Graph(Vec<String>),
}

impl CachedResult {
    /// Convert to serde_json::Value for Tera consumption
    pub fn to_json(&self) -> JsonValue {
        match self {
            CachedResult::Boolean(b) => JsonValue::Bool(*b),
            CachedResult::Solutions(rows) => {
                let arr: Vec<JsonValue> = rows
                    .iter()
                    .map(|row| {
                        let mut obj = serde_json::Map::new();
                        for (k, v) in row {
                            obj.insert(k.clone(), JsonValue::String(v.clone()));
                        }
                        JsonValue::Object(obj)
                    })
                    .collect();
                JsonValue::Array(arr)
            }
            // Graph variant: Serialized triples are not directly JSON-serializable
            // Return empty string as placeholder (callers should use the Graph variant directly)
            CachedResult::Graph(_triples) => JsonValue::String(String::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    test!(test_cached_result_boolean, {
        // Arrange & Act
        let result = CachedResult::Boolean(true);

        // Assert
        match result {
            CachedResult::Boolean(b) => assert!(b),
            _ => panic!("Expected Boolean variant"),
        }
    });

    test!(test_cached_result_solutions, {
        // Arrange
        let mut row1 = BTreeMap::new();
        row1.insert("name".to_string(), "Alice".to_string());
        let mut row2 = BTreeMap::new();
        row2.insert("name".to_string(), "Bob".to_string());
        let solutions = vec![row1, row2];

        // Act
        let result = CachedResult::Solutions(solutions.clone());

        // Assert
        match result {
            CachedResult::Solutions(rows) => {
                assert_eq!(rows.len(), 2);
                assert_eq!(rows[0].get("name"), Some(&"Alice".to_string()));
                assert_eq!(rows[1].get("name"), Some(&"Bob".to_string()));
            }
            _ => panic!("Expected Solutions variant"),
        }
    });

    test!(test_cached_result_graph, {
        // Arrange
        let triples = vec![
            "<http://example.org/alice> <http://example.org/name> \"Alice\" .".to_string(),
            "<http://example.org/bob> <http://example.org/name> \"Bob\" .".to_string(),
        ];

        // Act
        let result = CachedResult::Graph(triples.clone());

        // Assert
        match result {
            CachedResult::Graph(t) => {
                assert_eq!(t.len(), 2);
                assert_eq!(t, triples);
            }
            _ => panic!("Expected Graph variant"),
        }
    });

    test!(test_cached_result_to_json_boolean, {
        // Arrange
        let result = CachedResult::Boolean(true);

        // Act
        let json = result.to_json();

        // Assert
        assert_eq!(json, JsonValue::Bool(true));
    });

    test!(test_cached_result_to_json_solutions, {
        // Arrange
        let mut row1 = BTreeMap::new();
        row1.insert("name".to_string(), "Alice".to_string());
        let mut row2 = BTreeMap::new();
        row2.insert("age".to_string(), "30".to_string());
        let solutions = vec![row1, row2];
        let result = CachedResult::Solutions(solutions);

        // Act
        let json = result.to_json();

        // Assert
        match json {
            JsonValue::Array(arr) => {
                assert_eq!(arr.len(), 2);
                assert!(arr[0].is_object());
                assert!(arr[1].is_object());
            }
            _ => panic!("Expected JSON array"),
        }
    });

    test!(test_cached_result_to_json_graph, {
        // Arrange
        let triples = vec!["<s> <p> <o> .".to_string()];
        let result = CachedResult::Graph(triples);

        // Act
        let json = result.to_json();

        // Assert
        // Graph variant returns empty string as placeholder
        match json {
            JsonValue::String(s) => assert!(s.is_empty()),
            _ => panic!("Expected JSON string"),
        }
    });

    test!(test_cached_result_clone, {
        // Arrange
        let result = CachedResult::Boolean(true);

        // Act
        let cloned = result.clone();

        // Assert
        match (result, cloned) {
            (CachedResult::Boolean(b1), CachedResult::Boolean(b2)) => {
                assert_eq!(b1, b2);
            }
            _ => panic!("Expected Boolean variants"),
        }
    });

    test!(test_cached_result_debug, {
        // Arrange
        let result = CachedResult::Boolean(true);

        // Act
        let debug_str = format!("{:?}", result);

        // Assert
        assert!(debug_str.contains("Boolean"));
    });

    test!(test_cached_result_solutions_empty, {
        // Arrange & Act
        let result = CachedResult::Solutions(Vec::new());

        // Assert
        match result {
            CachedResult::Solutions(rows) => assert!(rows.is_empty()),
            _ => panic!("Expected Solutions variant"),
        }
    });

    test!(test_cached_result_graph_empty, {
        // Arrange & Act
        let result = CachedResult::Graph(Vec::new());

        // Assert
        match result {
            CachedResult::Graph(triples) => assert!(triples.is_empty()),
            _ => panic!("Expected Graph variant"),
        }
    });
}
