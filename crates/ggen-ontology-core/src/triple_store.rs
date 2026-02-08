//! Triple store implementation wrapping Oxigraph
//!
//! Provides RDF/TTL loading, querying, and validation capabilities
//! with deterministic behavior and comprehensive error handling.

use crate::errors::{OntologyError, Result};
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;
use std::io::BufReader;
use std::path::Path;

/// A deterministic, queryable RDF triple store
///
/// Wraps Oxigraph to provide type-safe operations for loading RDF/XML and Turtle files,
/// executing SPARQL queries, and validating ontology syntax.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ontology_core::triple_store::TripleStore;
///
/// let store = TripleStore::new()?;
/// store.load_turtle("ontology.ttl")?;
/// let results = store.query_sparql("SELECT ?s WHERE { ?s ?p ?o } LIMIT 10")?;
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Clone)]
pub struct TripleStore {
    /// Internal Oxigraph store holding all triples
    store: Store,
}

impl TripleStore {
    /// Creates a new empty triple store
    ///
    /// # Returns
    /// A new `TripleStore` instance, or an error if initialization fails
    ///
    /// # Errors
    /// Returns `OntologyError::TripleStoreError` if Oxigraph fails to initialize
    pub fn new() -> Result<Self> {
        let store = Store::new().map_err(|e| {
            OntologyError::triple_store(format!("Failed to initialize Oxigraph store: {}", e))
        })?;

        Ok(Self { store })
    }

    /// Loads an RDF/XML file into the triple store
    ///
    /// # Arguments
    /// * `path` - Path to the RDF/XML file
    ///
    /// # Returns
    /// Ok if the file was loaded successfully
    ///
    /// # Errors
    /// Returns:
    /// - `OntologyError::IoError` if the file cannot be read
    /// - `OntologyError::ParseError` if the RDF/XML is malformed
    ///
    /// # Determinism
    /// Loading the same file twice produces identical triple stores
    pub fn load_rdf<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let path_str = path.to_string_lossy().to_string();

        let file = std::fs::File::open(path).map_err(|e| {
            OntologyError::io(format!("Failed to open RDF file {}: {}", path_str, e))
        })?;

        let reader = BufReader::new(file);

        self.store
            .load_from_reader(RdfParser::from_format(RdfFormat::RdfXml), reader)
            .map_err(|e| {
                // Try to extract line number from error if available
                let error_msg = e.to_string();
                let line = 0; // Oxigraph doesn't always provide line numbers
                OntologyError::parse(&path_str, line, &error_msg)
            })?;

        Ok(())
    }

    /// Loads a Turtle (TTL) file into the triple store
    ///
    /// # Arguments
    /// * `path` - Path to the Turtle file
    ///
    /// # Returns
    /// Ok if the file was loaded successfully
    ///
    /// # Errors
    /// Returns:
    /// - `OntologyError::IoError` if the file cannot be read
    /// - `OntologyError::ParseError` if the Turtle is malformed
    ///
    /// # Determinism
    /// Loading the same file twice produces identical triple stores
    pub fn load_turtle<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let path_str = path.to_string_lossy().to_string();

        let file = std::fs::File::open(path).map_err(|e| {
            OntologyError::io(format!("Failed to open Turtle file {}: {}", path_str, e))
        })?;

        let reader = BufReader::new(file);

        self.store
            .load_from_reader(RdfParser::from_format(RdfFormat::Turtle), reader)
            .map_err(|e| {
                let error_msg = e.to_string();
                OntologyError::parse(&path_str, 0, &error_msg)
            })?;

        Ok(())
    }

    /// Executes a SPARQL query on the triple store
    ///
    /// # Arguments
    /// * `query` - SPARQL query string
    ///
    /// # Returns
    /// Raw SPARQL results as JSON
    ///
    /// # Errors
    /// Returns `OntologyError::QueryError` if the query is invalid or execution fails
    ///
    /// # Determinism
    /// Same query on same data produces identical results
    pub fn query_sparql(&self, query: &str) -> Result<String> {
        // Execute SPARQL query on the store
        // Note: Using the Store::query method which is currently the primary API for Oxigraph 0.5.x
        #[allow(deprecated)]
        let results = self
            .store
            .query(query)
            .map_err(|e| OntologyError::query(format!("Failed to execute SPARQL query: {}", e)))?;

        // Convert results to JSON for stable serialization
        let json = match results {
            oxigraph::sparql::QueryResults::Solutions(bindings) => {
                let mut vars = Vec::new();
                for var in bindings.variables() {
                    vars.push(var.as_str().to_string());
                }

                let mut rows = Vec::new();
                for row_result in bindings {
                    let row = row_result.map_err(|e| {
                        OntologyError::query(format!("Error reading SPARQL results: {}", e))
                    })?;

                    let mut row_obj = serde_json::json!({});
                    for (var, term) in row.iter() {
                        let var_name = var.as_str();
                        row_obj[var_name] = serde_json::json!(term.to_string());
                    }
                    rows.push(row_obj);
                }

                serde_json::json!({
                    "head": {"vars": vars},
                    "results": {"bindings": rows}
                })
            }
            oxigraph::sparql::QueryResults::Boolean(b) => {
                serde_json::json!({"boolean": b})
            }
            oxigraph::sparql::QueryResults::Graph(_) => {
                serde_json::json!({"type": "graph"})
            }
        };

        Ok(json.to_string())
    }

    /// Validates the RDF syntax of a Turtle file without loading
    ///
    /// # Arguments
    /// * `path` - Path to the Turtle file
    ///
    /// # Returns
    /// A validation report
    ///
    /// # Errors
    /// Returns `OntologyError::ParseError` if validation fails
    pub fn validate_turtle<P: AsRef<Path>>(&self, path: P) -> Result<ValidationReport> {
        let path = path.as_ref();

        let file = std::fs::File::open(path)
            .map_err(|e| OntologyError::io(format!("Cannot open file for validation: {}", e)))?;

        let reader = BufReader::new(file);

        // Try to parse without actually loading into store
        let temp_store = Store::new().map_err(|e| {
            OntologyError::triple_store(format!(
                "Failed to create temp store for validation: {}",
                e
            ))
        })?;

        match temp_store.load_from_reader(RdfParser::from_format(RdfFormat::Turtle), reader) {
            Ok(()) => Ok(ValidationReport {
                is_valid: true,
                errors: vec![],
                warnings: vec![],
            }),
            Err(e) => Ok(ValidationReport {
                is_valid: false,
                errors: vec![format!("Parse error: {}", e)],
                warnings: vec![],
            }),
        }
    }

    /// Validates the RDF syntax of an RDF/XML file without loading
    ///
    /// # Arguments
    /// * `path` - Path to the RDF/XML file
    ///
    /// # Returns
    /// A validation report
    ///
    /// # Errors
    /// Returns `OntologyError::ParseError` if validation fails
    pub fn validate_rdf<P: AsRef<Path>>(&self, path: P) -> Result<ValidationReport> {
        let path = path.as_ref();

        let file = std::fs::File::open(path)
            .map_err(|e| OntologyError::io(format!("Cannot open file for validation: {}", e)))?;

        let reader = BufReader::new(file);

        let temp_store = Store::new().map_err(|e| {
            OntologyError::triple_store(format!(
                "Failed to create temp store for validation: {}",
                e
            ))
        })?;

        match temp_store.load_from_reader(RdfParser::from_format(RdfFormat::RdfXml), reader) {
            Ok(()) => Ok(ValidationReport {
                is_valid: true,
                errors: vec![],
                warnings: vec![],
            }),
            Err(e) => Ok(ValidationReport {
                is_valid: false,
                errors: vec![format!("Parse error: {}", e)],
                warnings: vec![],
            }),
        }
    }

    /// Gets the number of triples currently in the store
    pub fn triple_count(&self) -> Result<usize> {
        // Count triples by iterating through all quads
        let count = self.store.iter().count();
        Ok(count)
    }

    /// Checks if the store contains any triples
    pub fn is_empty(&self) -> Result<bool> {
        Ok(self.triple_count()? == 0)
    }
}

/// Validation report for RDF/Turtle files
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ValidationReport {
    /// Whether the file passed validation
    pub is_valid: bool,
    /// List of validation errors found
    pub errors: Vec<String>,
    /// List of warnings (non-fatal issues)
    pub warnings: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_triple_store_creation() {
        let store = TripleStore::new();
        assert!(store.is_ok());
    }

    #[test]
    fn test_empty_store() -> Result<()> {
        let store = TripleStore::new()?;
        assert!(store.is_empty()?);
        Ok(())
    }

    #[test]
    fn test_triple_count() -> Result<()> {
        let store = TripleStore::new()?;
        let count = store.triple_count()?;
        assert_eq!(count, 0);
        Ok(())
    }

    #[test]
    fn test_load_valid_turtle() -> Result<()> {
        let mut file = NamedTempFile::new()
            .map_err(|e| OntologyError::io(format!("Failed to create temp file: {}", e)))?;
        let turtle_content = r#"
@prefix ex: <http://example.com/> .
ex:subject ex:predicate ex:object .
"#;
        file.write_all(turtle_content.as_bytes())
            .map_err(|e| OntologyError::io(format!("Failed to write test file: {}", e)))?;
        file.flush()
            .map_err(|e| OntologyError::io(format!("Failed to flush test file: {}", e)))?;

        let store = TripleStore::new()?;
        let result = store.load_turtle(file.path());
        assert!(result.is_ok());
        Ok(())
    }

    #[test]
    fn test_load_invalid_turtle() -> Result<()> {
        let mut file = NamedTempFile::new()
            .map_err(|e| OntologyError::io(format!("Failed to create temp file: {}", e)))?;
        let turtle_content = "this is not valid turtle !!!";
        file.write_all(turtle_content.as_bytes())
            .map_err(|e| OntologyError::io(format!("Failed to write test file: {}", e)))?;
        file.flush()
            .map_err(|e| OntologyError::io(format!("Failed to flush test file: {}", e)))?;

        let store = TripleStore::new()?;
        let result = store.load_turtle(file.path());
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn test_validate_turtle() -> Result<()> {
        let mut file = NamedTempFile::new()
            .map_err(|e| OntologyError::io(format!("Failed to create temp file: {}", e)))?;
        let turtle_content = r#"
@prefix ex: <http://example.com/> .
ex:subject ex:predicate ex:object .
"#;
        file.write_all(turtle_content.as_bytes())
            .map_err(|e| OntologyError::io(format!("Failed to write test file: {}", e)))?;
        file.flush()
            .map_err(|e| OntologyError::io(format!("Failed to flush test file: {}", e)))?;

        let store = TripleStore::new()?;
        let report = store.validate_turtle(file.path())?;
        assert!(report.is_valid);
        Ok(())
    }

    #[test]
    fn test_sparql_query() -> Result<()> {
        let mut file = NamedTempFile::new()
            .map_err(|e| OntologyError::io(format!("Failed to create temp file: {}", e)))?;
        let turtle_content = r#"
@prefix ex: <http://example.com/> .
ex:subject1 ex:predicate ex:object1 .
ex:subject2 ex:predicate ex:object2 .
"#;
        file.write_all(turtle_content.as_bytes())
            .map_err(|e| OntologyError::io(format!("Failed to write test file: {}", e)))?;
        file.flush()
            .map_err(|e| OntologyError::io(format!("Failed to flush test file: {}", e)))?;

        let store = TripleStore::new()?;
        store.load_turtle(file.path())?;

        let result = store.query_sparql("SELECT ?s WHERE { ?s ?p ?o }");
        assert!(result.is_ok());
        Ok(())
    }
}
