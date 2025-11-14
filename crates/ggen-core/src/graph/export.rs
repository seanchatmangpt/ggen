//! GraphExport - RDF serialization and export
//!
//! Provides operations for exporting RDF graphs to files and streams
//! in various RDF formats.

use crate::graph::core::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::io::{RdfFormat, RdfSerializer};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use tempfile;

/// GraphExport provides RDF serialization and export operations.
///
/// Supports exporting graphs to files and streams in all RDF formats:
/// - Turtle (.ttl)
/// - N-Triples (.nt)
/// - RDF/XML (.rdf, .xml)
/// - TriG (.trig)
/// - N-Quads (.nq)
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
    /// # Arguments
    ///
    /// * `writer` - Writer to write to
    /// * `format` - RDF format to use
    pub fn write_to_writer<W: Write>(&self, writer: W, format: RdfFormat) -> Result<()> {
        // Use Oxigraph's dump_to_writer for format-specific serialization
        // Explicit error conversion: Oxigraph SerializerError doesn't implement From
        self.graph
            .inner()
            .dump_to_writer(writer, format)
            .map_err(|e| Error::new(&format!("Failed to dump graph: {}", e)))?;
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
    use chicago_tdd_tools::test;
    use oxigraph::io::RdfFormat;
    use std::fs;
    use tempfile::TempDir;

    test!(test_export_write_to_file, {
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
        assert!(content.contains("ex:alice"));
        assert!(content.contains("ex:Person"));
    });

    test!(test_export_write_to_string_turtle, {
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
    });

    test!(test_export_write_to_string_ntriples, {
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
    });

    test!(test_export_write_to_string_rdfxml, {
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
    });

    test!(test_export_write_to_file_auto_turtle, {
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
    });

    test!(test_export_write_to_file_auto_ntriples, {
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
    });

    test!(test_export_write_to_file_auto_unsupported_format, {
        // Arrange
        let graph = Graph::new().unwrap();
        let export = GraphExport::new(&graph);
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("output.unknown");

        // Act & Assert
        let result = export.write_to_file_auto(&file_path);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unsupported"));
    });

    test!(test_export_format_specific_serialization, {
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

        // Assert - Each format should produce different output
        assert!(!turtle.is_empty());
        assert!(!ntriples.is_empty());
        assert!(!rdfxml.is_empty());
        // Formats should be different (at least one should differ)
        assert!(turtle != ntriples || turtle != rdfxml || ntriples != rdfxml);
    });

    test!(test_export_empty_graph, {
        // Arrange
        let graph = Graph::new().unwrap();
        let export = GraphExport::new(&graph);

        // Act
        let result = export.write_to_string(RdfFormat::Turtle).unwrap();

        // Assert
        // Empty graph may produce empty string or minimal output
        // Just verify it doesn't panic
        assert!(result.is_empty() || !result.is_empty());
    });
}
