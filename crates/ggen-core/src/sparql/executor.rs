//! SPARQL query execution engine for ggen
//!
//! Provides query execution against loaded RDF graphs with proper error handling
//! and result conversion to template-friendly formats.

use crate::graph::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::QueryResults;
use std::collections::HashMap;
use std::path::Path;

/// Execute a SPARQL SELECT query from a file and return results as rows
///
/// # Arguments
/// * `graph` - The RDF graph to query
/// * `query_file` - Path to the SPARQL query file
///
/// # Returns
/// A vector of rows, where each row is a HashMap mapping variable names (without '?' prefix)
/// to their string values. Returns an empty vector if the query produces no results.
///
/// # Errors
/// Returns an error if:
/// - The query file cannot be read
/// - The query syntax is invalid
/// - The query is not a SELECT query (CONSTRUCT/ASK are not supported by this function)
/// - Query execution fails
///
/// # Examples
///
/// ```ignore
/// let graph = Graph::new()?;
/// graph.insert_turtle("@prefix ex: <http://example.org/> . ex:alice ex:name \"Alice\" .")?;
///
/// let results = execute_query(&graph, Path::new("query.sparql"))?;
/// assert_eq!(results[0].get("name"), Some(&"Alice".to_string()));
/// ```
pub fn execute_query(
    graph: &Graph, query_file: &Path,
) -> Result<Vec<HashMap<String, String>>> {
    // 1. Read query from file
    let query_content = std::fs::read_to_string(query_file).map_err(|e| {
        Error::new(&format!(
            "Failed to read SPARQL query file '{}': {}",
            query_file.display(),
            e
        ))
    })?;

    // 2. Execute query against graph
    let results = graph.query(&query_content).map_err(|e| {
        Error::new(&format!(
            "Failed to execute SPARQL query from '{}': {}",
            query_file.display(),
            e
        ))
    })?;

    // 3. Convert results to Vec<HashMap<String, String>>
    match results {
        QueryResults::Solutions(solutions) => {
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.map_err(|e| {
                    Error::new(&format!(
                        "Error reading SPARQL solution from '{}': {}",
                        query_file.display(),
                        e
                    ))
                })?;

                let mut row = HashMap::new();
                for (var, term) in solution.iter() {
                    // Strip '?' prefix from variable name
                    let clean_var = var.as_str().strip_prefix('?').unwrap_or(var.as_str());

                    // Convert term to string, removing RDF syntax (quotes, angle brackets)
                    let value = clean_sparql_term(&term.to_string());
                    row.insert(clean_var.to_string(), value);
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => {
            Err(Error::new(&format!(
                "Query in '{}' is an ASK query (returned boolean). Expected SELECT query.",
                query_file.display()
            )))
        }
        QueryResults::Graph(_) => {
            Err(Error::new(&format!(
                "Query in '{}' is a CONSTRUCT query (returned graph). Expected SELECT query.",
                query_file.display()
            )))
        }
    }
}

/// Execute a SPARQL SELECT query from an inline string and return results as rows
///
/// # Arguments
/// * `graph` - The RDF graph to query
/// * `query` - The SPARQL query string
///
/// # Returns
/// A vector of rows, where each row is a HashMap mapping variable names (without '?' prefix)
/// to their string values.
///
/// # Errors
/// Returns an error if the query execution fails or the query is not a SELECT query.
pub fn execute_query_inline(
    graph: &Graph, query: &str,
) -> Result<Vec<HashMap<String, String>>> {
    let results = graph.query(query).map_err(|e| {
        Error::new(&format!("Failed to execute SPARQL query: {}", e))
    })?;

    match results {
        QueryResults::Solutions(solutions) => {
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.map_err(|e| {
                    Error::new(&format!("Error reading SPARQL solution: {}", e))
                })?;

                let mut row = HashMap::new();
                for (var, term) in solution.iter() {
                    let clean_var = var.as_str().strip_prefix('?').unwrap_or(var.as_str());
                    let value = clean_sparql_term(&term.to_string());
                    row.insert(clean_var.to_string(), value);
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => {
            Err(Error::new(
                "Query is an ASK query (returned boolean). Expected SELECT query.",
            ))
        }
        QueryResults::Graph(_) => {
            Err(Error::new(
                "Query is a CONSTRUCT query (returned graph). Expected SELECT query.",
            ))
        }
    }
}

/// Clean a SPARQL term string representation
///
/// Converts Oxigraph's term representation to plain values:
/// - IRIs: `<http://example.org/>` -> `http://example.org/`
/// - Literals: `"value"` or `"value"^^<xsd:string>` -> `value`
/// - Language-tagged: `"value"@en` -> `value`
fn clean_sparql_term(value: &str) -> String {
    if value.starts_with('<') && value.ends_with('>') {
        // IRI: strip angle brackets
        value[1..value.len() - 1].to_string()
    } else if let Some(without_prefix) = value.strip_prefix('"') {
        // Literal: strip quotes and optional datatype/language tag
        if let Some(quote_end) = without_prefix.find('"') {
            without_prefix[..quote_end].to_string()
        } else {
            value.to_string()
        }
    } else {
        value.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute_query_simple_select() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
                @prefix ex: <http://example.org/> .
                ex:alice ex:name "Alice" ;
                         ex:age "30" .
                ex:bob ex:name "Bob" ;
                       ex:age "25" .
            "#,
            )
            .unwrap();

        // Act
        let results = execute_query_inline(
            &graph,
            "SELECT ?name ?age WHERE { ?s <http://example.org/name> ?name ; <http://example.org/age> ?age }",
        )
        .unwrap();

        // Assert
        assert_eq!(results.len(), 2);
        assert!(results[0].contains_key("name"));
        assert!(results[0].contains_key("age"));
        // Results should contain "Alice" and "Bob"
        let names: Vec<_> = results.iter().filter_map(|r| r.get("name")).collect();
        assert!(names.contains(&&"Alice".to_string()));
        assert!(names.contains(&&"Bob".to_string()));
    }

    #[test]
    fn test_execute_query_empty_result() {
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

        // Act
        let results = execute_query_inline(
            &graph,
            "SELECT ?name WHERE { ?s <http://example.org/nonexistent> ?name }",
        )
        .unwrap();

        // Assert
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_clean_sparql_term_iri() {
        let iri = "<http://example.org/alice>";
        assert_eq!(clean_sparql_term(iri), "http://example.org/alice");
    }

    #[test]
    fn test_clean_sparql_term_literal() {
        let literal = "\"Alice\"";
        assert_eq!(clean_sparql_term(literal), "Alice");
    }

    #[test]
    fn test_clean_sparql_term_typed_literal() {
        let typed = "\"30\"^^<http://www.w3.org/2001/XMLSchema#integer>";
        assert_eq!(clean_sparql_term(typed), "30");
    }

    #[test]
    fn test_clean_sparql_term_language_tagged() {
        let lang = "\"Alice\"@en";
        assert_eq!(clean_sparql_term(lang), "Alice");
    }

    #[test]
    fn test_execute_query_ask_error() {
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

        // Act & Assert
        let result = execute_query_inline(
            &graph,
            "ASK { ?s a <http://example.org/Person> }",
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("ASK query"));
    }

    #[test]
    fn test_execute_query_construct_error() {
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

        // Act & Assert
        let result = execute_query_inline(
            &graph,
            "CONSTRUCT { ?s a <http://example.org/Result> } WHERE { ?s <http://example.org/name> ?name }",
        );
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("CONSTRUCT") || err_msg.contains("graph"));
    }

    #[test]
    fn test_variable_name_cleaning() {
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

        // Act
        let results = execute_query_inline(
            &graph,
            "SELECT ?name WHERE { ?s <http://example.org/name> ?name }",
        )
        .unwrap();

        // Assert - variable name should not have ? prefix
        assert!(!results[0].keys().any(|k| k.starts_with('?')));
        assert!(results[0].contains_key("name"));
    }
}
