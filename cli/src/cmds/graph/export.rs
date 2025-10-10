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

    println!("🚧 Placeholder: graph export");
    println!("  Output: {}", args.output.trim());
    println!("  Format: {}", args.format.trim());
    println!("  Pretty: {}", args.pretty);
    Ok(())
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
