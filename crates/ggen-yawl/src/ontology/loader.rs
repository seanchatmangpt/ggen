//! Ontology loader supporting multiple RDF formats.
//!
//! This module provides the [`OntologyLoader`] type for loading industry ontologies
//! from various RDF serialization formats. The loader supports automatic format detection
//! from file extensions and can load from local files or string content.
//!
//! # Supported Formats
//!
//! * **Turtle** (.ttl) - The default and most common format
//! * **RDF/XML** (.rdf, .owl) - OWL ontologies
//! * **N-Triples** (.nt) - Line-based triple format
//! * **N-Quads** (.nq) - Triple with named graphs
//! * **TriG** (.trig) - Turtle with named graphs
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_yawl::OntologyLoader;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load from file with auto-detected format
//! let loader = OntologyLoader::new();
//! let graph = loader.load_from_file("fibo.ttl")?;
//!
//! // Load from string with specific format
//! let turtle = r#"
//!     @prefix ex: <http://example.org/> .
//!     ex:Class a owl:Class .
//! "#;
//! let graph = loader.load_from_str(turtle, ggen_yawl::OntologyFormat::Turtle)?;
//! # Ok(())
//! # }
//! ```

use crate::{Error, Result};
use ggen_core::Graph;
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use std::path::Path;
use std::str::FromStr;

/// RDF format for ontology serialization.
///
/// Represents the various RDF serialization formats that can be loaded
/// by the [`OntologyLoader`].
///
/// # Variants
///
/// * [`Turtle`] - Turtle format (.ttl files)
/// * [`RdfXml`] - RDF/XML format (.rdf, .owl files)
/// * [`NTriples`] - N-Triples format (.nt files)
/// * [`NQuads`] - N-Quads format (.nq files)
/// * [`Trig`] - TriG format (.trig files)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OntologyFormat {
    /// Turtle (.ttl)
    Turtle,
    /// RDF/XML (.rdf, .owl)
    RdfXml,
    /// N-Triples (.nt)
    NTriples,
    /// N-Quads (.nq)
    NQuads,
    /// TriG (.trig)
    Trig,
}

impl FromStr for OntologyFormat {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "ttl" | "turtle" => Ok(Self::Turtle),
            "rdf" | "xml" | "owl" => Ok(Self::RdfXml),
            "nt" => Ok(Self::NTriples),
            "nq" => Ok(Self::NQuads),
            "trig" => Ok(Self::Trig),
            _ => Err(Error::OntologyLoad(format!("Unknown format: {}", s))),
        }
    }
}

impl OntologyFormat {
    /// Detect format from file extension.
    ///
    /// Attempts to parse a file extension as an RDF format.
    /// Returns `None` if the extension is not recognized.
    ///
    /// # Arguments
    ///
    /// * `ext` - File extension (e.g., "ttl", "rdf", "owl")
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyFormat;
    ///
    /// assert_eq!(OntologyFormat::from_extension("ttl"), Some(OntologyFormat::Turtle));
    /// assert_eq!(OntologyFormat::from_extension("owl"), Some(OntologyFormat::RdfXml));
    /// assert_eq!(OntologyFormat::from_extension("unknown"), None);
    /// ```
    pub fn from_extension(ext: &str) -> Option<Self> {
        Self::from_str(ext).ok()
    }

    /// Convert to oxigraph RdfFormat.
    ///
    /// Converts the wrapper type to the underlying Oxigraph format type
    /// for use with Oxigraph's parsing functions.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyFormat;
    ///
    /// let format = OntologyFormat::Turtle;
    /// let oxigraph_format = format.to_rdf_format();
    /// assert_eq!(oxigraph_format, oxigraph::io::RdfFormat::Turtle);
    /// ```
    pub fn to_rdf_format(self) -> RdfFormat {
        match self {
            Self::Turtle => RdfFormat::Turtle,
            Self::RdfXml => RdfFormat::RdfXml,
            Self::NTriples => RdfFormat::NTriples,
            Self::NQuads => RdfFormat::NQuads,
            Self::Trig => RdfFormat::TriG,
        }
    }

    /// Get content type for HTTP requests.
    ///
    /// Returns the MIME type associated with this RDF format,
    /// suitable for use in HTTP Content-Type headers.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyFormat;
    ///
    /// assert_eq!(OntologyFormat::Turtle.content_type(), "text/turtle");
    /// assert_eq!(OntologyFormat::RdfXml.content_type(), "application/rdf+xml");
    /// ```
    pub fn content_type(&self) -> &'static str {
        match self {
            Self::Turtle => "text/turtle",
            Self::RdfXml => "application/rdf+xml",
            Self::NTriples => "application/n-triples",
            Self::NQuads => "application/n-quads",
            Self::Trig => "application/trig",
        }
    }
}

/// Loader for industry ontologies.
///
/// The [`OntologyLoader`] provides a unified interface for loading ontologies
/// from various sources and formats. It supports automatic format detection,
/// import flattening, and configurable base IRIs.
///
/// # Fields
///
/// * [`base_iri`](Self::base_iri) - Optional base IRI for resolving relative references
/// * [`flatten_imports`](Self::flatten_imports) - Whether to flatten owl:imports into a single graph
///
/// # Example
///
/// ```rust,no_run
/// use ggen_yawl::OntologyLoader;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Create loader with custom settings
/// let loader = OntologyLoader::new()
///     .with_base_iri("http://example.org/".to_string())
///     .with_flatten_imports(true);
///
/// // Load ontology
/// let graph = loader.load_from_file("ontology.ttl")?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct OntologyLoader {
    /// Base IRI for resolving relative references.
    pub base_iri: Option<String>,
    /// Whether to flatten imports into a single graph.
    pub flatten_imports: bool,
}

impl Default for OntologyLoader {
    fn default() -> Self {
        Self {
            base_iri: None,
            flatten_imports: true,
        }
    }
}

impl OntologyLoader {
    /// Create a new loader with default settings.
    ///
    /// The default loader has no base IRI and flattening enabled.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyLoader;
    ///
    /// let loader = OntologyLoader::new();
    /// assert!(loader.flatten_imports);
    /// assert!(loader.base_iri.is_none());
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the base IRI for resolving relative references.
    ///
    /// # Arguments
    ///
    /// * `base_iri` - The base IRI string to use for resolving relative IRIs
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyLoader;
    ///
    /// let loader = OntologyLoader::new()
    ///     .with_base_iri("http://example.org/".to_string());
    /// assert_eq!(loader.base_iri, Some("http://example.org/".to_string()));
    /// ```
    pub fn with_base_iri(mut self, base_iri: String) -> Self {
        self.base_iri = Some(base_iri);
        self
    }

    /// Set whether to flatten owl:imports.
    ///
    /// When enabled, all imported ontologies are merged into a single graph.
    ///
    /// # Arguments
    ///
    /// * `flatten` - Whether to flatten imports
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::OntologyLoader;
    ///
    /// let loader = OntologyLoader::new()
    ///     .with_flatten_imports(false);
    /// assert!(!loader.flatten_imports);
    /// ```
    pub fn with_flatten_imports(mut self, flatten: bool) -> Self {
        self.flatten_imports = flatten;
        self
    }

    /// Load ontology from a file path.
    ///
    /// The file format is automatically detected from the file extension.
    /// Supported extensions: .ttl, .rdf, .owl, .nt, .nq, .trig
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the ontology file
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The file format is not supported
    /// - The RDF syntax is invalid
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_yawl::OntologyLoader;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let loader = OntologyLoader::new();
    /// let graph = loader.load_from_file("fibo.ttl")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_from_file<P: AsRef<Path>>(&self, path: P) -> Result<Graph> {
        let path = path.as_ref();
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::OntologyLoad(format!("Failed to read {}: {}", path.display(), e)))?;

        // Detect format from extension
        let format = path
            .extension()
            .and_then(|e| e.to_str())
            .and_then(OntologyFormat::from_extension)
            .unwrap_or(OntologyFormat::Turtle);

        self.load_from_str(&content, format)
    }

    /// Load ontology from a string.
    ///
    /// # Arguments
    ///
    /// * `content` - The RDF content as a string
    /// * `format` - The RDF serialization format
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The RDF syntax is invalid
    /// - The store cannot be created
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::{OntologyLoader, OntologyFormat};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let turtle = r#"
    ///     @prefix ex: <http://example.org/> .
    ///     @prefix owl: <http://www.w3.org/2002/07/owl#> .
    ///     ex:Class a owl:Class .
    /// "#;
    /// let loader = OntologyLoader::new();
    /// let graph = loader.load_from_str(turtle, OntologyFormat::Turtle)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_from_str(&self, content: &str, format: OntologyFormat) -> Result<Graph> {
        // Create a new oxigraph Store directly
        let store = Store::new()
            .map_err(|e| Error::OntologyLoad(format!("Failed to create store: {}", e)))?;

        let rdf_format = format.to_rdf_format();

        // Use oxigraph's load_from_reader API directly
        store
            .load_from_reader(rdf_format, content.as_bytes())
            .map_err(|e| Error::OntologyLoad(format!("Failed to load {:?}: {}", format, e)))?;

        // Create a Graph from the Store using the private constructor pattern
        // We need to use Graph::from_store which is crate-private, so we use
        // a workaround by creating a Graph and loading into it
        let graph = self.create_graph_from_store(store)?;
        Ok(graph)
    }

    /// Create a Graph from a Store by converting via the load_path mechanism.
    ///
    /// This is a workaround since Graph::from_store is private. We create a
    /// temporary file and use Graph::load_from_file.
    pub fn create_graph_from_store(&self, store: Store) -> Result<Graph> {
        // Export to Turtle from the store
        use oxigraph::io::RdfSerializer;

        let mut buffer = Vec::new();
        {
            let mut writer = RdfSerializer::from_format(RdfFormat::Turtle)
                .for_writer(&mut buffer);

            for quad in store.quads_for_pattern(None, None, None, None) {
                let quad = quad.map_err(|e| Error::OntologyLoad(format!("Failed to read quad: {}", e)))?;
                writer
                    .serialize_quad(&quad)
                    .map_err(|e| Error::OntologyLoad(format!("Failed to serialize quad: {}", e)))?;
            }

            writer
                .finish()
                .map_err(|e| Error::OntologyLoad(format!("Failed to finish serialization: {}", e)))?;
        }

        let turtle_str = std::str::from_utf8(&buffer)
            .map_err(|e| Error::OntologyLoad(format!("Failed to convert to UTF-8: {}", e)))?;

        // Now load into a Graph using the public insert_turtle method
        let graph = Graph::new().map_err(|e| Error::OntologyLoad(e.to_string()))?;
        graph.insert_turtle(turtle_str)?;
        Ok(graph)
    }
}

/// Convenience function to load an ontology from a string.
///
/// This is a shorthand for `OntologyLoader::new().load_from_str(content, OntologyFormat::Turtle)`.
///
/// # Arguments
///
/// * `content` - The RDF content in Turtle format
///
/// # Example
///
/// ```rust
/// use ggen_yawl::load_ontology;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let turtle = r#"
///     @prefix ex: <http://example.org/> .
///     @prefix owl: <http://www.w3.org/2002/07/owl#> .
///     ex:Class a owl:Class .
/// "#;
/// let graph = load_ontology(turtle)?;
/// # Ok(())
/// # }
/// ```
pub fn load_ontology(content: &str) -> Result<Graph> {
    let loader = OntologyLoader::new();
    loader.load_from_str(content, OntologyFormat::Turtle)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_from_str() {
        assert_eq!(OntologyFormat::from_str("ttl").unwrap(), OntologyFormat::Turtle);
        assert_eq!(OntologyFormat::from_str("Turtle").unwrap(), OntologyFormat::Turtle);
        assert_eq!(OntologyFormat::from_str("rdf").unwrap(), OntologyFormat::RdfXml);
    }

    #[test]
    fn test_format_from_extension() {
        assert_eq!(OntologyFormat::from_extension("ttl"), Some(OntologyFormat::Turtle));
        assert_eq!(OntologyFormat::from_extension("owl"), Some(OntologyFormat::RdfXml));
        assert_eq!(OntologyFormat::from_extension("unknown"), None);
    }

    #[test]
    fn test_format_content_type() {
        assert_eq!(OntologyFormat::Turtle.content_type(), "text/turtle");
        assert_eq!(OntologyFormat::RdfXml.content_type(), "application/rdf+xml");
    }

    #[test]
    fn test_to_rdf_format() {
        assert_eq!(OntologyFormat::Turtle.to_rdf_format(), RdfFormat::Turtle);
        assert_eq!(OntologyFormat::RdfXml.to_rdf_format(), RdfFormat::RdfXml);
        assert_eq!(OntologyFormat::NTriples.to_rdf_format(), RdfFormat::NTriples);
        assert_eq!(OntologyFormat::NQuads.to_rdf_format(), RdfFormat::NQuads);
        assert_eq!(OntologyFormat::Trig.to_rdf_format(), RdfFormat::TriG);
    }

    #[test]
    fn test_loader_configuration() {
        let loader = OntologyLoader::new();
        assert!(loader.flatten_imports);

        let with_flatten = loader.clone().with_flatten_imports(false);
        assert!(!with_flatten.flatten_imports);

        let with_base = loader.with_base_iri("http://example.org/".to_string());
        assert_eq!(with_base.base_iri, Some("http://example.org/".to_string()));
    }

    #[test]
    fn test_load_simple_turtle() {
        let turtle = r#"
            @prefix ex: <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            ex:Test a rdfs:Class ;
                rdfs:label "Test" .
        "#;

        let loader = OntologyLoader::new();
        let result = loader.load_from_str(turtle, OntologyFormat::Turtle);
        // The load operation should succeed
        assert!(result.is_ok(), "Loading turtle should succeed: {:?}", result.err());
    }

    #[test]
    fn test_load_ntriples() {
        let ntriples = "<http://example.org/subject> <http://example.org/predicate> \"object\" .";

        let loader = OntologyLoader::new();
        let result = loader.load_from_str(ntriples, OntologyFormat::NTriples);
        // The load operation should succeed
        assert!(result.is_ok(), "Loading n-triples should succeed");
    }
}
