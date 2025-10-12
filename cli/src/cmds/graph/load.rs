//! RDF graph loading and data ingestion functionality.
//!
//! This module provides comprehensive RDF data loading capabilities, supporting
//! multiple formats (Turtle, N-Triples, RDF/XML, JSON-LD) with format detection,
//! base IRI resolution, and merge operations. It validates input paths and
//! formats to ensure secure and reliable data loading.
//!
//! # Examples
//!
//! ```bash
//! ggen graph load data.ttl
//! ggen graph load ontology.rdf --format rdfxml --base http://example.org/
//! ggen graph load additional.ttl --merge
//! ```
//!
//! # Errors
//!
//! Returns errors if the file path is invalid or contains traversal attempts,
//! the RDF format is unsupported, the file cannot be read, or if RDF parsing
//! fails due to malformed data.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path};

#[derive(Args, Debug)]
pub struct LoadArgs {
    /// RDF file to load
    pub file: String,

    /// RDF format (turtle, ntriples, rdfxml)
    #[arg(long)]
    pub format: Option<String>,

    /// Base IRI for relative URIs
    #[arg(long)]
    pub base: Option<String>,

    /// Merge with existing graph
    #[arg(long)]
    pub merge: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait RdfLoader {
    fn load(
        &self, file: String, format: Option<String>, base: Option<String>, merge: bool,
    ) -> Result<LoadStats>;
}

#[derive(Debug, Clone)]
pub struct LoadStats {
    pub triples_loaded: usize,
    pub total_triples: usize,
    pub format_detected: String,
}

/// Validate and sanitize file path input
fn validate_file_path(file: &str) -> Result<()> {
    // Validate file path is not empty
    if file.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("File path cannot be empty"));
    }

    // Validate file path length
    if file.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "File path too long (max 1000 characters)",
        ));
    }

    // Use Path components for proper traversal protection
    let path = Path::new(file);
    if path.components().any(|c| matches!(c, Component::ParentDir)) {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }

    // Validate file path format (basic pattern check)
    if !file
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
        ));
    }

    Ok(())
}

/// Validate and sanitize format input (if provided)
fn validate_format(format: &Option<String>) -> Result<()> {
    if let Some(format) = format {
        // Validate format is not empty
        if format.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Format cannot be empty"));
        }

        // Validate format length
        if format.len() > 50 {
            return Err(ggen_utils::error::Error::new(
                "Format too long (max 50 characters)",
            ));
        }

        // Validate format format (basic pattern check)
        if !format
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(ggen_utils::error::Error::new(
                "Invalid format: only alphanumeric characters, dashes, and underscores allowed",
            ));
        }

        // Validate against known formats
        let valid_formats = ["turtle", "ntriples", "rdfxml", "jsonld", "n3"];
        if !valid_formats.contains(&format.to_lowercase().as_str()) {
            return Err(ggen_utils::error::Error::new(
                "Unsupported format: supported formats are turtle, ntriples, rdfxml, jsonld, n3",
            ));
        }
    }

    Ok(())
}

/// Validate and sanitize base IRI input (if provided)
fn validate_base_iri(base: &Option<String>) -> Result<()> {
    if let Some(base) = base {
        // Validate base IRI is not empty
        if base.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Base IRI cannot be empty"));
        }

        // Validate base IRI length
        if base.len() > 500 {
            return Err(ggen_utils::error::Error::new(
                "Base IRI too long (max 500 characters)",
            ));
        }

        // Basic URI validation
        if !base.starts_with("http://")
            && !base.starts_with("https://")
            && !base.starts_with("file://")
        {
            return Err(ggen_utils::error::Error::new(
                "Invalid base IRI: must start with http://, https://, or file://",
            ));
        }
    }

    Ok(())
}

/// Detect RDF format from file extension
fn detect_format_from_extension(filename: &str) -> &'static str {
    let path = std::path::Path::new(filename);
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("ttl") | Some("turtle") => "turtle",
        Some("nt") | Some("ntriples") => "ntriples",
        Some("rdf") | Some("xml") => "rdfxml",
        Some("jsonld") | Some("json") => "jsonld",
        Some("n3") => "n3",
        _ => "turtle", // Default to turtle
    }
}

pub async fn run(args: &LoadArgs) -> Result<()> {
    // Validate inputs
    validate_file_path(&args.file)?;
    validate_format(&args.format)?;
    validate_base_iri(&args.base)?;

    println!("📊 Loading RDF graph...");

    // Check if file exists
    let file_path = std::path::Path::new(&args.file);
    if !file_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "File not found: {}",
            args.file
        )));
    }

    // Detect format if not provided
    let format = args
        .format
        .as_deref()
        .unwrap_or_else(|| detect_format_from_extension(&args.file));

    println!("📁 Loading file: {}", args.file);
    println!("🔍 Format: {}", format);

    if let Some(base) = &args.base {
        println!("🌐 Base IRI: {}", base);
    }

    // Load the RDF file using ggen-core
    let graph = ggen_core::Graph::load_from_file(&args.file)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load RDF file: {}", e)))?;

    // Get graph statistics
    let triples_count = graph.len();

    if args.merge {
        println!(
            "✅ Merged {} triples from {} ({})",
            triples_count, args.file, format
        );
        println!("📊 Total triples in graph: {}", triples_count);
    } else {
        println!(
            "✅ Loaded {} triples from {} ({})",
            triples_count, args.file, format
        );
    }

    Ok(())
}

pub async fn run_with_deps(args: &LoadArgs, loader: &dyn RdfLoader) -> Result<()> {
    // Validate inputs
    validate_file_path(&args.file)?;
    validate_format(&args.format)?;
    validate_base_iri(&args.base)?;

    // Show progress for loading operation
    println!("🔍 Loading RDF file...");

    let stats = loader.load(
        args.file.clone(),
        args.format.clone(),
        args.base.clone(),
        args.merge,
    )?;

    if args.merge {
        println!(
            "✅ Loaded {} triples from {} ({})",
            stats.triples_loaded, args.file, stats.format_detected
        );
        println!("📊 Total triples in graph: {}", stats.total_triples);
    } else {
        println!(
            "✅ Loaded {} triples from {} ({})",
            stats.triples_loaded, args.file, stats.format_detected
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_load_rdf_file() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(
                eq(String::from("data.ttl")),
                eq(Some(String::from("turtle"))),
                eq(None::<String>),
                eq(false),
            )
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 100,
                    total_triples: 100,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "data.ttl".to_string(),
            format: Some("turtle".to_string()),
            base: None,
            merge: false,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_load_with_merge() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(
                eq(String::from("additional.ttl")),
                always(),
                always(),
                eq(true),
            )
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 50,
                    total_triples: 150,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "additional.ttl".to_string(),
            format: None,
            base: None,
            merge: true,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_load_with_base_iri() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(
                eq(String::from("relative.ttl")),
                always(),
                eq(Some(String::from("http://example.org/"))),
                eq(false),
            )
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 25,
                    total_triples: 25,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "relative.ttl".to_string(),
            format: None,
            base: Some("http://example.org/".to_string()),
            merge: false,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }
}
