//! Natural language query engine

use crate::{Error, Result};
use tracing::debug;

/// Query engine for converting between natural language and SPARQL
pub struct QueryEngine;

impl QueryEngine {
    /// Create a new query engine
    pub fn new() -> Self {
        Self
    }

    /// Convert natural language query to SPARQL
    pub fn nl_to_sparql(&self, nl_query: &str) -> Result<String> {
        debug!("Converting NL to SPARQL: {}", nl_query);

        let nl_lower = nl_query.to_lowercase();

        // Pattern matching for common queries
        if nl_lower.contains("show") && nl_lower.contains("function") {
            Ok(self.query_functions())
        } else if nl_lower.contains("list") && nl_lower.contains("module") {
            Ok(self.query_modules())
        } else if nl_lower.contains("find") {
            // Extract search term
            let search_term = nl_query.split_whitespace().last().unwrap_or("");
            Ok(self.search_query(search_term))
        } else {
            // Default query
            Ok(self.default_query())
        }
    }

    /// Convert query results to natural language
    pub fn results_to_nl(&self, results: &serde_json::Value) -> Result<String> {
        debug!("Converting results to NL");

        if let Some(array) = results.as_array() {
            if array.is_empty() {
                return Ok("No results found.".to_string());
            }

            let count = array.len();
            Ok(format!("Found {} result(s):\n{}", count, serde_json::to_string_pretty(results).unwrap_or_default()))
        } else if let Some(obj) = results.as_object() {
            if let Some(boolean) = obj.get("boolean") {
                return Ok(format!("Query result: {}", boolean));
            }
            Ok(serde_json::to_string_pretty(results).unwrap_or_default())
        } else {
            Ok("Query completed.".to_string())
        }
    }

    // Query templates

    fn query_functions(&self) -> String {
        r#"
        PREFIX code: <http://ggen.dev/ontology/code#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        SELECT ?function ?name
        WHERE {
            ?function rdf:type code:Function .
            ?function code:name ?name .
        }
        LIMIT 100
        "#.to_string()
    }

    fn query_modules(&self) -> String {
        r#"
        PREFIX code: <http://ggen.dev/ontology/code#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        SELECT ?module ?name
        WHERE {
            ?module rdf:type code:Module .
            ?module code:name ?name .
        }
        "#.to_string()
    }

    fn search_query(&self, term: &str) -> String {
        format!(
            r#"
            PREFIX code: <http://ggen.dev/ontology/code#>

            SELECT ?entity ?name
            WHERE {{
                ?entity code:name ?name .
                FILTER(CONTAINS(LCASE(?name), LCASE("{}")))
            }}
            LIMIT 50
            "#,
            term
        )
    }

    fn default_query(&self) -> String {
        r#"
        PREFIX code: <http://ggen.dev/ontology/code#>

        SELECT ?subject ?predicate ?object
        WHERE {
            ?subject ?predicate ?object .
        }
        LIMIT 10
        "#.to_string()
    }
}

impl Default for QueryEngine {
    fn default() -> Self {
        Self::new()
    }
}
