//! Chicago TDD tests for graph export
//!
//! Uses REAL file writing and format serialization

use anyhow::Result;
use ggen_domain::graph::{export_graph, ExportFormat, ExportOptions};
use std::fs;
use tempfile::tempdir;

#[test]
fn test_export_turtle_to_real_file() -> Result<()> {
    let temp_dir = tempdir()?;
    let output_path = temp_dir.path().join("output.ttl");

    let options = ExportOptions {
        output_path: output_path.to_string_lossy().to_string(),
        format: ExportFormat::Turtle,
        pretty: true,
        graph: None,
    };

    let content = export_graph(options)?;

    // Verify REAL file created
    assert!(output_path.exists());

    // Verify REAL file content
    let file_content = fs::read_to_string(&output_path)?;
    assert_eq!(file_content, content);
    assert!(file_content.contains("@prefix"));

    Ok(())
}

#[test]
fn test_export_all_formats_create_real_files() -> Result<()> {
    let temp_dir = tempdir()?;

    let formats = vec![
        (ExportFormat::Turtle, "output.ttl"),
        (ExportFormat::NTriples, "output.nt"),
        (ExportFormat::RdfXml, "output.rdf"),
        (ExportFormat::JsonLd, "output.jsonld"),
        (ExportFormat::N3, "output.n3"),
    ];

    for (format, filename) in formats {
        let output_path = temp_dir.path().join(filename);

        let options = ExportOptions {
            output_path: output_path.to_string_lossy().to_string(),
            format,
            pretty: false,
            graph: None,
        };

        export_graph(options)?;

        // Verify REAL file exists
        assert!(output_path.exists());

        // Verify file has content
        let content = fs::read_to_string(&output_path)?;
        assert!(!content.is_empty());
    }

    Ok(())
}

#[test]
fn test_export_pretty_vs_compact() -> Result<()> {
    let temp_dir = tempdir()?;

    // Pretty export
    let pretty_path = temp_dir.path().join("pretty.ttl");
    let pretty_options = ExportOptions {
        output_path: pretty_path.to_string_lossy().to_string(),
        format: ExportFormat::Turtle,
        pretty: true,
        graph: None,
    };
    let pretty_content = export_graph(pretty_options)?;

    // Compact export
    let compact_path = temp_dir.path().join("compact.ttl");
    let compact_options = ExportOptions {
        output_path: compact_path.to_string_lossy().to_string(),
        format: ExportFormat::Turtle,
        pretty: false,
        graph: None,
    };
    let compact_content = export_graph(compact_options)?;

    // Verify different formatting
    assert_ne!(pretty_content, compact_content);
    assert!(pretty_content.contains('\n'));

    Ok(())
}

#[test]
fn test_export_format_parsing() -> Result<()> {
    assert_eq!(ExportFormat::from_str("turtle")?, ExportFormat::Turtle);
    assert_eq!(ExportFormat::from_str("TTL")?, ExportFormat::Turtle);
    assert_eq!(ExportFormat::from_str("ntriples")?, ExportFormat::NTriples);
    assert_eq!(ExportFormat::from_str("rdfxml")?, ExportFormat::RdfXml);
    assert_eq!(ExportFormat::from_str("jsonld")?, ExportFormat::JsonLd);

    // Invalid format
    assert!(ExportFormat::from_str("invalid").is_err());

    Ok(())
}
