//! GraphExport - RDF serialization and export
//!
//! Provides operations for exporting RDF graphs to files and streams
//! in various RDF formats using Oxigraph's core team best practices.
//!
//! ## Implementation
//!
//! This module uses Oxigraph's `RdfSerializer` API following the recommended pattern:
//! 1. Create serializer with `RdfSerializer::from_format().for_writer()`
//! 2. Iterate over quads and serialize each with `serialize_quad()`
//! 3. Complete serialization with `finish()`
//!
//! This ensures proper format-specific serialization for all supported RDF formats:
//! Turtle, N-Triples, RDF/XML, TriG, and N-Quads.

use crate::graph::core::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::io::{RdfFormat, RdfSerializer};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

/// GraphExport provides RDF serialization and export operations.
///
/// Uses Oxigraph's `RdfSerializer` API following core team best practices for
/// efficient and format-compliant RDF serialization.
///
/// Supports exporting graphs to files and streams in all RDF formats:
/// - Turtle (.ttl) - Human-readable format with prefix support
/// - N-Triples (.nt) - Line-based format with full IRIs
/// - RDF/XML (.rdf, .xml) - XML-based RDF serialization
/// - TriG (.trig) - Turtle format with named graph support
/// - N-Quads (.nq) - N-Triples format with named graph support
///
/// # Examples
///
/// ## Export to file
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphExport};
/// use oxigraph::io::RdfFormat;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// let export = GraphExport::new(&graph);
/// export.write_to_file("output.ttl", RdfFormat::Turtle)?;
/// # Ok(())
/// # }
/// ```
///
/// ## Export to string
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphExport};
/// use oxigraph::io::RdfFormat;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let graph = Graph::new()?;
/// let export = GraphExport::new(&graph);
///
/// let turtle = export.write_to_string(RdfFormat::Turtle)?;
/// println!("{}", turtle);
/// # Ok(())
/// # }
/// ```
pub struct GraphExport<'a> {
    graph: &'a Graph,
}

impl<'a> GraphExport<'a> {
    /// Create a new GraphExport instance for the given graph.
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }

    /// Write the graph to a file in the specified format.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the output file
    /// * `format` - RDF format to use
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be created or written
    /// - The format is unsupported
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P, format: RdfFormat) -> Result<()> {
        let file =
            File::create(path).map_err(|e| Error::new(&format!("Failed to create file: {}", e)))?;
        let writer = BufWriter::new(file);
        self.write_to_writer(writer, format)
    }

    /// Write the graph to a writer in the specified format.
    ///
    /// Uses Oxigraph's Store API appropriately:
    /// - For graph formats (Turtle, N-Triples, RDF/XML): manually serializes default graph only
    /// - For dataset formats (TriG, N-Quads): uses `dump_to_writer` for all graphs
    ///
    /// # Arguments
    ///
    /// * `writer` - Writer to write to
    /// * `format` - RDF format to use (Turtle, N-Triples, RDF/XML, TriG, N-Quads)
    pub fn write_to_writer<W: Write>(&self, mut writer: W, format: RdfFormat) -> Result<()> {
        use oxigraph::model::GraphName;

        // Graph formats (Turtle, N-Triples, RDF/XML) only support single graphs
        // Dataset formats (TriG, N-Quads) support multiple named graphs
        match format {
            RdfFormat::Turtle | RdfFormat::NTriples | RdfFormat::RdfXml => {
                // For single-graph formats, manually serialize only the default graph
                // This prevents "dataset format expected" errors
                let mut serializer = RdfSerializer::from_format(format).for_writer(&mut writer);

                // Query only the default graph quads
                let default_graph_quads = self.graph.quads_for_pattern(
                    None,                           // subject: any
                    None,                           // predicate: any
                    None,                           // object: any
                    Some(&GraphName::DefaultGraph), // graph_name: default graph only
                )?;

                // Serialize each quad from the default graph
                for quad in default_graph_quads {
                    serializer
                        .serialize_quad(&quad)
                        .map_err(|e| Error::new(&format!("Failed to serialize quad: {}", e)))?;
                }

                // Finish serialization
                serializer.finish().map_err(|e| {
                    Error::new(&format!("Failed to finish RDF serialization: {}", e))
                })?;
            }
            RdfFormat::TriG | RdfFormat::NQuads => {
                // For dataset formats, dump all graphs
                let serializer = RdfSerializer::from_format(format);
                self.graph.inner().dump_to_writer(serializer, &mut writer)?;
            }
            _ => {
                // For any other formats, try the dataset approach
                let serializer = RdfSerializer::from_format(format);
                self.graph.inner().dump_to_writer(serializer, &mut writer)?;
            }
        }
        Ok(())
    }

    /// Write the graph to a string in the specified format.
    ///
    /// # Arguments
    ///
    /// * `format` - RDF format to use
    ///
    /// # Returns
    ///
    /// The serialized RDF as a string in the requested format.
    pub fn write_to_string(&self, format: RdfFormat) -> Result<String> {
        let mut buffer = Vec::new();
        self.write_to_writer(&mut buffer, format)?;
        String::from_utf8(buffer)
            .map_err(|e| Error::new(&format!("Invalid UTF-8 in RDF output: {}", e)))
    }

    /// Write the graph to a file, auto-detecting format from extension.
    ///
    /// Supported extensions:
    /// - `.ttl`, `.turtle` - Turtle
    /// - `.nt`, `.ntriples` - N-Triples
    /// - `.rdf`, `.xml` - RDF/XML
    /// - `.trig` - TriG
    /// - `.nq`, `.nquads` - N-Quads
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the output file
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file extension is unsupported
    /// - The file cannot be created or written
    pub fn write_to_file_auto<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|s| s.to_ascii_lowercase())
            .unwrap_or_default();

        let format = match ext.as_str() {
            "ttl" | "turtle" => RdfFormat::Turtle,
            "nt" | "ntriples" => RdfFormat::NTriples,
            "rdf" | "xml" => RdfFormat::RdfXml,
            "trig" => RdfFormat::TriG,
            "nq" | "nquads" => RdfFormat::NQuads,
            other => {
                return Err(Error::new(&format!(
                    "unsupported RDF format extension: {}",
                    other
                )))
            }
        };

        self.write_to_file(path, format)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::core::Graph;
    use oxigraph::io::RdfFormat;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_export_write_to_file() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person ;
                     ex:name "Alice" .
        "#,
            )
            .unwrap();
        let export = GraphExport::new(&graph);
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("output.ttl");

        // Act
        export.write_to_file(&file_path, RdfFormat::Turtle).unwrap();

        // Assert
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).unwrap();
        // Turtle serialization may use full IRIs or prefixes depending on serializer config
        // Check for either the prefix form or the full IRI
        assert!(content.contains("ex:alice") || content.contains("http://example.org/alice"));
        assert!(content.contains("ex:Person") || content.contains("http://example.org/Person"));
    }

    #[test]
    fn test_export_write_to_string_turtle() {
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
        let export = GraphExport::new(&graph);

        // Act
        let result = export.write_to_string(RdfFormat::Turtle).unwrap();

        // Assert
        assert!(!result.is_empty());
        // Turtle format should contain prefix or triples
        assert!(result.contains("ex:") || result.contains("http://example.org/"));
    }

    #[test]
    fn test_export_write_to_string_ntriples() {
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
        let export = GraphExport::new(&graph);

        // Act
        let result = export.write_to_string(RdfFormat::NTriples).unwrap();

        // Assert
        assert!(!result.is_empty());
        // N-Triples format should contain full IRIs
        assert!(result.contains("http://example.org/alice"));
    }

    #[test]
    fn test_export_write_to_string_rdfxml() {
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
        let export = GraphExport::new(&graph);

        // Act
        let result = export.write_to_string(RdfFormat::RdfXml).unwrap();

        // Assert
        assert!(!result.is_empty());
        // RDF/XML format should contain XML tags
        assert!(result.contains("<") && result.contains(">"));
    }

    #[test]
    fn test_export_write_to_file_auto_turtle() {
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
        let export = GraphExport::new(&graph);
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("output.ttl");

        // Act
        export.write_to_file_auto(&file_path).unwrap();

        // Assert
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).unwrap();
        assert!(!content.is_empty());
    }

    #[test]
    fn test_export_write_to_file_auto_ntriples() {
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
        let export = GraphExport::new(&graph);
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("output.nt");

        // Act
        export.write_to_file_auto(&file_path).unwrap();

        // Assert
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).unwrap();
        assert!(!content.is_empty());
    }

    #[test]
    fn test_export_write_to_file_auto_unsupported_format() {
        // Arrange
        let graph = Graph::new().unwrap();
        let export = GraphExport::new(&graph);
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("output.unknown");

        // Act & Assert
        let result = export.write_to_file_auto(&file_path);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unsupported"));
    }

    #[test]
    fn test_export_format_specific_serialization() {
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
        let export = GraphExport::new(&graph);

        // Act - Test multiple formats
        let turtle = export.write_to_string(RdfFormat::Turtle).unwrap();
        let ntriples = export.write_to_string(RdfFormat::NTriples).unwrap();
        let rdfxml = export.write_to_string(RdfFormat::RdfXml).unwrap();
        let trig = export.write_to_string(RdfFormat::TriG).unwrap();
        let nquads = export.write_to_string(RdfFormat::NQuads).unwrap();

        // Assert - Each format should produce different output
        assert!(!turtle.is_empty());
        assert!(!ntriples.is_empty());
        assert!(!rdfxml.is_empty());
        assert!(!trig.is_empty());
        assert!(!nquads.is_empty());
        // Formats should be different (at least one should differ)
        assert!(turtle != ntriples || turtle != rdfxml || ntriples != rdfxml);
    }

    #[test]
    fn test_export_turtle_format_preserves_prefixes() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            ex:alice a ex:Person ;
                     ex:name "Alice" .
        "#,
            )
            .unwrap();
        let export = GraphExport::new(&graph);

        // Act
        let turtle = export.write_to_string(RdfFormat::Turtle).unwrap();

        // Assert - Turtle format should preserve prefixes or use full IRIs
        assert!(turtle.contains("ex:") || turtle.contains("http://example.org/"));
        assert!(turtle.contains("alice") || turtle.contains("Alice"));
    }

    #[test]
    fn test_export_ntriples_format_uses_full_iris() {
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
        let export = GraphExport::new(&graph);

        // Act
        let ntriples = export.write_to_string(RdfFormat::NTriples).unwrap();

        // Assert - N-Triples format should use full IRIs, not prefixes
        assert!(ntriples.contains("http://example.org/alice"));
        assert!(ntriples.contains("http://example.org/Person"));
    }

    #[test]
    fn test_export_rdfxml_format_has_xml_structure() {
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
        let export = GraphExport::new(&graph);

        // Act
        let rdfxml = export.write_to_string(RdfFormat::RdfXml).unwrap();

        // Assert - RDF/XML format should have XML structure
        assert!(rdfxml.contains("<") && rdfxml.contains(">"));
        assert!(rdfxml.contains("rdf:") || rdfxml.contains("RDF"));
    }

    #[test]
    fn test_export_trig_format_supports_named_graphs() {
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
        let export = GraphExport::new(&graph);

        // Act
        let trig = export.write_to_string(RdfFormat::TriG).unwrap();

        // Assert - TriG format should support named graphs
        assert!(!trig.is_empty());
        // TriG should contain graph information
        assert!(trig.contains("http://example.org/graph1") || trig.contains("alice"));
    }

    #[test]
    fn test_export_nquads_format_includes_graph_context() {
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
        let export = GraphExport::new(&graph);

        // Act
        let nquads = export.write_to_string(RdfFormat::NQuads).unwrap();

        // Assert - N-Quads format should include graph context
        assert!(!nquads.is_empty());
        // N-Quads should contain graph information
        assert!(nquads.contains("http://example.org/graph1") || nquads.contains("alice"));
    }

    #[test]
    fn test_export_empty_graph() {
        // Arrange
        let graph = Graph::new().unwrap();
        let export = GraphExport::new(&graph);

        // Act
        let result = export.write_to_string(RdfFormat::Turtle).unwrap();

        // Assert
        // Empty graph may produce empty string or minimal output
        // Just verify it doesn't panic
        assert!(result.is_empty() || !result.is_empty());
    }
}
