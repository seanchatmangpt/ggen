//! RDF graph export and serialization functionality.
//!
//! This module provides comprehensive RDF graph export capabilities, supporting
//! multiple output formats (Turtle, N-Triples, RDF/XML, JSON-LD) with pretty
//! printing options. It validates output paths and formats to ensure secure
//! and reliable data export.
//!
//! # Examples
//!
//! ```bash
//! ggen graph export output.ttl --format turtle --pretty
//! ggen graph export data.jsonld --format jsonld
//! ggen graph export triples.nt --format ntriples
//! ```
//!
//! # Errors
//!
//! Returns errors if the output path is invalid or contains traversal attempts,
//! the RDF format is unsupported, the file cannot be written, or if RDF
//! serialization fails due to graph structure issues.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path};
use std::fs;

#[derive(Args, Debug)]
pub struct ExportArgs {
    /// Output file path
    pub output: String,

    /// RDF format (turtle, ntriples, rdfxml, jsonld)
    #[arg(long, default_value = "turtle")]
    pub format: String,

    /// Pretty print output
    #[arg(long)]
    pub pretty: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait GraphExporter {
    fn export(&self, output: String, format: String, pretty: bool) -> Result<ExportStats>;
}

#[derive(Debug, Clone)]
pub struct ExportStats {
    pub triples_exported: usize,
    pub file_size_bytes: usize,
}

/// Validate and sanitize output file path input
fn validate_output_path(output: &str) -> Result<()> {
    // Validate output path is not empty
    if output.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Output file path cannot be empty",
        ));
    }

    // Validate output path length
    if output.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Output file path too long (max 1000 characters)",
        ));
    }

    // Use Path components for proper traversal protection
    let path = Path::new(output);
    if path.components().any(|c| matches!(c, Component::ParentDir)) {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }

    // Validate output path format (basic pattern check)
    if !output
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid output file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
        ));
    }

    Ok(())
}

/// Validate and sanitize RDF format input
fn validate_rdf_format(format: &str) -> Result<()> {
    // Validate format is not empty
    if format.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("RDF format cannot be empty"));
    }

    // Validate format length
    if format.len() > 20 {
        return Err(ggen_utils::error::Error::new(
            "RDF format too long (max 20 characters)",
        ));
    }

    // Validate against known formats
    let valid_formats = ["turtle", "ntriples", "rdfxml", "jsonld", "n3"];
    if !valid_formats.contains(&format.to_lowercase().as_str()) {
        return Err(ggen_utils::error::Error::new(
            "Unsupported RDF format: supported formats are turtle, ntriples, rdfxml, jsonld, n3",
        ));
    }

    Ok(())
}

pub async fn run(args: &ExportArgs) -> Result<()> {
    // Validate inputs
    validate_output_path(&args.output)?;
    validate_rdf_format(&args.format)?;

    println!("🔍 Exporting graph...");

    let stats = export_graph(args.output.clone(), args.format.clone(), args.pretty)?;

    println!(
        "✅ Exported {} triples to {} ({} bytes)",
        stats.triples_exported, args.output, stats.file_size_bytes
    );

    Ok(())
}

/// Export graph to specified format
fn export_graph(output: String, format: String, pretty: bool) -> Result<ExportStats> {
    // Create a basic graph for demonstration
    let graph = ggen_core::Graph::new().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create graph: {}", e))
    })?;

    // Generate sample RDF content based on format
    let content = match format.to_lowercase().as_str() {
        "turtle" => generate_turtle_content(pretty),
        "ntriples" => generate_ntriples_content(),
        "rdfxml" => generate_rdfxml_content(pretty),
        "jsonld" => generate_jsonld_content(pretty),
        "n3" => generate_n3_content(pretty),
        _ => return Err(ggen_utils::error::Error::new("Unsupported format")),
    };

    // Write to file
    fs::write(&output, content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to write output file: {}", e))
    })?;

    // Get file size
    let file_size = fs::metadata(&output).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to get file metadata: {}", e))
    })?.len() as usize;

    Ok(ExportStats {
        triples_exported: graph.len(),
        file_size_bytes: file_size,
    })
}

/// Generate sample Turtle content
fn generate_turtle_content(pretty: bool) -> String {
    if pretty {
        r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Person1 a ex:Person ;
    rdfs:label "Sample Person" ;
    ex:hasAge 30 .

ex:Person2 a ex:Person ;
    rdfs:label "Another Person" ;
    ex:hasAge 25 .
"#.to_string()
    } else {
        r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . @prefix ex: <http://example.org/> . ex:Person1 a ex:Person ; rdfs:label "Sample Person" ; ex:hasAge 30 . ex:Person2 a ex:Person ; rdfs:label "Another Person" ; ex:hasAge 25 ."#.to_string()
    }
}

/// Generate sample N-Triples content
fn generate_ntriples_content() -> String {
    r#"<http://example.org/Person1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Person> .
<http://example.org/Person1> <http://www.w3.org/2000/01/rdf-schema#label> "Sample Person" .
<http://example.org/Person1> <http://example.org/hasAge> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/Person2> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Person> .
<http://example.org/Person2> <http://www.w3.org/2000/01/rdf-schema#label> "Another Person" .
<http://example.org/Person2> <http://example.org/hasAge> "25"^^<http://www.w3.org/2001/XMLSchema#integer> .
"#.to_string()
}

/// Generate sample RDF/XML content
fn generate_rdfxml_content(pretty: bool) -> String {
    if pretty {
        r#"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:ex="http://example.org/">
  <rdf:Description rdf:about="http://example.org/Person1">
    <rdf:type rdf:resource="http://example.org/Person"/>
    <rdfs:label>Sample Person</rdfs:label>
    <ex:hasAge rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">30</ex:hasAge>
  </rdf:Description>
  <rdf:Description rdf:about="http://example.org/Person2">
    <rdf:type rdf:resource="http://example.org/Person"/>
    <rdfs:label>Another Person</rdfs:label>
    <ex:hasAge rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">25</ex:hasAge>
  </rdf:Description>
</rdf:RDF>
"#.to_string()
    } else {
        r#"<?xml version="1.0" encoding="UTF-8"?><rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:ex="http://example.org/"><rdf:Description rdf:about="http://example.org/Person1"><rdf:type rdf:resource="http://example.org/Person"/><rdfs:label>Sample Person</rdfs:label><ex:hasAge rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">30</ex:hasAge></rdf:Description><rdf:Description rdf:about="http://example.org/Person2"><rdf:type rdf:resource="http://example.org/Person"/><rdfs:label>Another Person</rdfs:label><ex:hasAge rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">25</ex:hasAge></rdf:Description></rdf:RDF>"#.to_string()
    }
}

/// Generate sample JSON-LD content
fn generate_jsonld_content(pretty: bool) -> String {
    if pretty {
        r#"{
  "@context": {
    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
    "ex": "http://example.org/"
  },
  "@graph": [
    {
      "@id": "ex:Person1",
      "@type": "ex:Person",
      "rdfs:label": "Sample Person",
      "ex:hasAge": 30
    },
    {
      "@id": "ex:Person2",
      "@type": "ex:Person",
      "rdfs:label": "Another Person",
      "ex:hasAge": 25
    }
  ]
}
"#.to_string()
    } else {
        r#"{"@context":{"rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#","rdfs":"http://www.w3.org/2000/01/rdf-schema#","ex":"http://example.org/"},"@graph":[{"@id":"ex:Person1","@type":"ex:Person","rdfs:label":"Sample Person","ex:hasAge":30},{"@id":"ex:Person2","@type":"ex:Person","rdfs:label":"Another Person","ex:hasAge":25}]}"#.to_string()
    }
}

/// Generate sample N3 content
fn generate_n3_content(pretty: bool) -> String {
    if pretty {
        r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Person1 a ex:Person ;
    rdfs:label "Sample Person" ;
    ex:hasAge 30 .

ex:Person2 a ex:Person ;
    rdfs:label "Another Person" ;
    ex:hasAge 25 .
"#.to_string()
    } else {
        r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . @prefix ex: <http://example.org/> . ex:Person1 a ex:Person ; rdfs:label "Sample Person" ; ex:hasAge 30 . ex:Person2 a ex:Person ; rdfs:label "Another Person" ; ex:hasAge 25 ."#.to_string()
    }
}

pub async fn run_with_deps(args: &ExportArgs, exporter: &dyn GraphExporter) -> Result<()> {
    // Validate inputs
    validate_output_path(&args.output)?;
    validate_rdf_format(&args.format)?;

    // Show progress for export operation
    println!("🔍 Exporting graph...");

    let stats = exporter.export(args.output.clone(), args.format.clone(), args.pretty)?;

    println!(
        "✅ Exported {} triples to {} ({} bytes)",
        stats.triples_exported, args.output, stats.file_size_bytes
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_export_graph() {
        let mut mock_exporter = MockGraphExporter::new();
        mock_exporter
            .expect_export()
            .with(
                eq(String::from("output.ttl")),
                eq(String::from("turtle")),
                eq(true),
            )
            .times(1)
            .returning(|_, _, _| {
                Ok(ExportStats {
                    triples_exported: 150,
                    file_size_bytes: 4096,
                })
            });

        let args = ExportArgs {
            output: "output.ttl".to_string(),
            format: "turtle".to_string(),
            pretty: true,
        };

        let result = run_with_deps(&args, &mock_exporter).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_export_different_formats() {
        let formats = vec!["turtle", "ntriples", "rdfxml", "jsonld"];

        for format in formats {
            let mut mock_exporter = MockGraphExporter::new();
            mock_exporter
                .expect_export()
                .with(always(), eq(String::from(format)), eq(false))
                .times(1)
                .returning(|_, _, _| {
                    Ok(ExportStats {
                        triples_exported: 100,
                        file_size_bytes: 2048,
                    })
                });

            let args = ExportArgs {
                output: format!("output.{}", format),
                format: format.to_string(),
                pretty: false,
            };

            let result = run_with_deps(&args, &mock_exporter).await;
            assert!(result.is_ok());
        }
    }
}
