//! Ontology validation functions
//!
//! Validates RDF/XML and Turtle files for syntax and semantic correctness.

use crate::errors::Result;
use crate::triple_store::ValidationReport;
use std::path::Path;

/// Validates an RDF/XML file for syntax correctness
///
/// # Arguments
/// * `path` - Path to the RDF/XML file
///
/// # Returns
/// A ValidationReport indicating success or listing errors
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ontology_core::validators::validate_rdf_xml;
///
/// let report = validate_rdf_xml("ontology.rdf")?;
/// if report.is_valid {
///     println!("RDF is valid!");
/// } else {
///     for error in &report.errors {
///         println!("Error: {}", error);
///     }
/// }
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn validate_rdf_xml<P: AsRef<Path>>(path: P) -> Result<ValidationReport> {
    use crate::triple_store::TripleStore;
    let store = TripleStore::new()?;
    store.validate_rdf(path)
}

/// Validates a Turtle (TTL) file for syntax correctness
///
/// # Arguments
/// * `path` - Path to the Turtle file
///
/// # Returns
/// A ValidationReport indicating success or listing errors
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ontology_core::validators::validate_turtle;
///
/// let report = validate_turtle("ontology.ttl")?;
/// if !report.is_valid {
///     println!("Turtle syntax errors found!");
/// }
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn validate_turtle<P: AsRef<Path>>(path: P) -> Result<ValidationReport> {
    use crate::triple_store::TripleStore;
    let store = TripleStore::new()?;
    store.validate_turtle(path)
}

/// Validates a SPARQL query for syntax correctness
///
/// # Arguments
/// * `query` - SPARQL query string
///
/// # Returns
/// Ok if the query is valid
///
/// # Errors
/// Returns OntologyError::QueryError if the query has syntax errors
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ontology_core::validators::validate_sparql_query;
///
/// let query = "SELECT ?s WHERE { ?s ?p ?o }";
/// validate_sparql_query(query)?;
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn validate_sparql_query(query: &str) -> Result<()> {
    use crate::triple_store::TripleStore;

    // Try to prepare the query with an empty store by executing a simple query
    let store = TripleStore::new()?;

    // Execute a test query to validate syntax
    store.query_sparql(query).map_err(|e| {
        // If it's a query error (as opposed to other errors), it's a syntax issue
        match e {
            crate::errors::OntologyError::QueryError(msg) => {
                crate::errors::OntologyError::query(format!("Invalid SPARQL query: {}", msg))
            }
            _ => e,
        }
    })?;

    Ok(())
}

/// Comprehensive ontology validation
///
/// Performs multiple validation checks on an ontology file including:
/// - Syntax validation
/// - Semantic checks (optional)
/// - Completeness checks (optional)
///
/// # Arguments
/// * `path` - Path to the ontology file
/// * `file_type` - File type: "ttl" or "rdf"
///
/// # Returns
/// A comprehensive ValidationReport
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ontology_core::validators::validate_ontology;
///
/// let report = validate_ontology("ontology.ttl", "ttl")?;
/// if report.is_valid {
///     println!("All checks passed!");
/// }
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn validate_ontology<P: AsRef<Path>>(path: P, file_type: &str) -> Result<ValidationReport> {
    match file_type.to_lowercase().as_str() {
        "ttl" | "turtle" => validate_turtle(path),
        "rdf" | "xml" => validate_rdf_xml(path),
        _ => Err(crate::errors::OntologyError::config(
            format!("Unknown file type: {}", file_type),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_validate_valid_turtle() {
        let mut file = NamedTempFile::new().unwrap();
        let content = r#"
@prefix ex: <http://example.com/> .
ex:subject ex:predicate ex:object .
"#;
        file.write_all(content.as_bytes()).unwrap();
        file.flush().unwrap();

        let report = validate_turtle(file.path()).unwrap();
        assert!(report.is_valid);
    }

    #[test]
    fn test_validate_invalid_turtle() {
        let mut file = NamedTempFile::new().unwrap();
        let content = "this is not valid turtle !!!";
        file.write_all(content.as_bytes()).unwrap();
        file.flush().unwrap();

        let report = validate_turtle(file.path()).unwrap();
        assert!(!report.is_valid);
        assert!(!report.errors.is_empty());
    }

    #[test]
    fn test_validate_valid_sparql_query() {
        let query = "SELECT ?s WHERE { ?s ?p ?o }";
        let result = validate_sparql_query(query);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_invalid_sparql_query() {
        let query = "SELECT ? WHERE { invalid syntax }";
        let result = validate_sparql_query(query);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_ontology_ttl() {
        let mut file = NamedTempFile::new().unwrap();
        let content = r#"
@prefix ex: <http://example.com/> .
ex:subject ex:predicate ex:object .
"#;
        file.write_all(content.as_bytes()).unwrap();
        file.flush().unwrap();

        let report = validate_ontology(file.path(), "ttl").unwrap();
        assert!(report.is_valid);
    }

    #[test]
    fn test_validate_ontology_unknown_type() {
        let mut file = NamedTempFile::new().unwrap();
        let content = "test";
        file.write_all(content.as_bytes()).unwrap();
        file.flush().unwrap();

        let result = validate_ontology(file.path(), "unknown");
        assert!(result.is_err());
    }
}
