//! RDF parser for loading TTL (Turtle) files
//!
//! This module provides functionality for parsing RDF/Turtle files and loading them
//! into an in-memory graph store using Oxigraph. It supports loading schema files,
//! user-defined TTL files, and provides access to the underlying RDF store for
//! SPARQL querying.
//!
//! ## Features
//!
//! - **TTL Parsing**: Load Turtle format RDF files
//! - **Schema Loading**: Load predefined schema files
//! - **In-memory Store**: Efficient triple storage with Oxigraph
//! - **SPARQL Support**: Access to store for querying
//!
//! ## Examples
//!
//! ### Loading RDF Files
//!
//! ```rust,no_run
//! use ggen_ai::rdf::RdfParser;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let mut parser = RdfParser::new()?;
//! parser.load_schema()?;
//! parser.load_ttl(Path::new("sample-cli.ttl"))?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::{bail, error::Result};
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

/// RDF parser that loads TTL files into an oxigraph Store
///
/// # Example
///
/// ```no_run
/// use ggen_ai::rdf::RdfParser;
/// use std::path::Path;
///
/// # fn main() -> anyhow::Result<()> {
/// let mut parser = RdfParser::new()?;
/// parser.load_schema()?;
/// parser.load_ttl(Path::new("sample-cli.ttl"))?;
/// # Ok(())
/// # }
/// ```
pub struct RdfParser {
    store: Store,
}

impl RdfParser {
    /// Create a new RDF parser with an empty in-memory store
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new()?,
        })
    }

    /// Load a TTL file into the store
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the TTL file to load
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be opened
    /// - The TTL syntax is invalid
    /// - The RDF triples cannot be loaded into the store
    pub fn load_ttl(&mut self, path: &Path) -> Result<()> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        self.store
            .load_from_reader(RdfFormat::Turtle, reader)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load RDF: {}", e)))?;

        Ok(())
    }

    /// Load the project-schema.ttl file
    ///
    /// This loads the base schema/ontology that defines the CLI structure.
    /// By default, looks for the schema at:
    /// `examples/clap-noun-verb-demo/project-schema.ttl`
    ///
    /// # Errors
    ///
    /// Returns an error if the schema file cannot be found or loaded
    pub fn load_schema(&mut self) -> Result<()> {
        // Look for schema relative to project root
        let schema_path = Path::new("examples/clap-noun-verb-demo/project-schema.ttl");

        if schema_path.exists() {
            self.load_ttl(schema_path)?;
        } else {
            // Try alternative locations
            let alt_paths = [
                "../../examples/clap-noun-verb-demo/project-schema.ttl",
                "../examples/clap-noun-verb-demo/project-schema.ttl",
                "./examples/clap-noun-verb-demo/project-schema.ttl",
            ];

            let mut loaded = false;
            for alt_path in &alt_paths {
                let path = Path::new(alt_path);
                if path.exists() {
                    self.load_ttl(path)?;
                    loaded = true;
                    break;
                }
            }

            if !loaded {
                bail!(
                    "Could not find project-schema.ttl at {} or alternative locations",
                    schema_path.display()
                );
            }
        }

        Ok(())
    }

    /// Load schema from a custom path
    ///
    /// # Arguments
    ///
    /// * `schema_path` - Path to the schema TTL file
    pub fn load_schema_from(&mut self, schema_path: &Path) -> Result<()> {
        self.load_ttl(schema_path)
    }

    /// Get a reference to the underlying RDF store
    ///
    /// This allows executing SPARQL queries against the loaded triples
    pub fn get_store(&self) -> &Store {
        &self.store
    }

    /// Get the number of triples loaded in the store
    ///
    /// Useful for debugging and validation
    pub fn triple_count(&self) -> usize {
        self.store.len().unwrap_or(0)
    }

    /// Clear all triples from the store
    pub fn clear(&mut self) -> Result<()> {
        self.store.clear()?;
        Ok(())
    }
}

impl Default for RdfParser {
    fn default() -> Self {
        Self::new().expect("Failed to create default RdfParser")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn create_test_ttl() -> NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        writeln!(
            file,
            r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:subject1 rdf:type ex:Type1 .
ex:subject1 ex:predicate1 "value1" .
ex:subject2 ex:predicate2 "value2" .
        "#
        )
        .unwrap();
        file
    }

    #[test]
    fn test_parser_creation() {
        let parser = RdfParser::new();
        assert!(parser.is_ok());
    }

    #[test]
    fn test_load_ttl() {
        let mut parser = RdfParser::new().unwrap();
        let test_file = create_test_ttl();

        let result = parser.load_ttl(test_file.path());
        assert!(result.is_ok());

        // Should have loaded 3 triples
        assert_eq!(parser.triple_count(), 3);
    }

    #[test]
    fn test_load_invalid_ttl() {
        let mut parser = RdfParser::new().unwrap();
        let mut file = NamedTempFile::new().unwrap();
        writeln!(file, "invalid ttl syntax [[[").unwrap();

        let result = parser.load_ttl(file.path());
        assert!(result.is_err());
    }

    #[test]
    fn test_load_nonexistent_file() {
        let mut parser = RdfParser::new().unwrap();
        let result = parser.load_ttl(Path::new("/nonexistent/file.ttl"));
        assert!(result.is_err());
    }

    #[test]
    fn test_clear() {
        let mut parser = RdfParser::new().unwrap();
        let test_file = create_test_ttl();

        parser.load_ttl(test_file.path()).unwrap();
        assert_eq!(parser.triple_count(), 3);

        parser.clear().unwrap();
        assert_eq!(parser.triple_count(), 0);
    }

    #[test]
    fn test_multiple_loads() {
        let mut parser = RdfParser::new().unwrap();
        let test_file1 = create_test_ttl();

        parser.load_ttl(test_file1.path()).unwrap();
        let count1 = parser.triple_count();
        assert_eq!(count1, 3);

        // Create a different test file with different triples
        let mut file2 = NamedTempFile::new().unwrap();
        writeln!(
            file2,
            r#"
@prefix ex2: <http://example2.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex2:subject3 rdf:type ex2:Type2 .
ex2:subject3 ex2:predicate3 "value3" .
        "#
        )
        .unwrap();

        parser.load_ttl(file2.path()).unwrap();
        let count2 = parser.triple_count();

        // Should have accumulated triples (3 from first + 2 from second = 5)
        assert!(count2 > count1);
        assert_eq!(count2, 5);
    }

    #[test]
    fn test_default_parser() {
        let parser = RdfParser::default();
        assert_eq!(parser.triple_count(), 0);
    }

    #[test]
    fn test_get_store() {
        let parser = RdfParser::new().unwrap();
        let store = parser.get_store();
        assert_eq!(store.len().unwrap_or(0), 0);
    }
}
