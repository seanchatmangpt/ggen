//! RDF data loading domain logic with real Oxigraph ingestion
//!
//! Chicago TDD: Uses REAL RDF file loading and graph state verification

use anyhow::{Context, Result};
use ggen_core::Graph;
use std::path::{Path, PathBuf};

/// Supported RDF formats
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RdfFormat {
    Turtle,
    NTriples,
    RdfXml,
    JsonLd,
    N3,
}

impl RdfFormat {
    /// Detect format from file extension
    pub fn from_extension(filename: &str) -> Self {
        let path = Path::new(filename);
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("ttl") | Some("turtle") => RdfFormat::Turtle,
            Some("nt") | Some("ntriples") => RdfFormat::NTriples,
            Some("rdf") | Some("xml") => RdfFormat::RdfXml,
            Some("jsonld") | Some("json") => RdfFormat::JsonLd,
            Some("n3") => RdfFormat::N3,
            _ => RdfFormat::Turtle, // Default
        }
    }

    /// Get format name as string
    pub fn as_str(&self) -> &'static str {
        match self {
            RdfFormat::Turtle => "Turtle",
            RdfFormat::NTriples => "N-Triples",
            RdfFormat::RdfXml => "RDF/XML",
            RdfFormat::JsonLd => "JSON-LD",
            RdfFormat::N3 => "N3",
        }
    }
}

/// Options for RDF loading
#[derive(Debug, Clone)]
pub struct LoadOptions {
    /// File path to load
    pub file_path: String,
    /// RDF format (auto-detected if None)
    pub format: Option<RdfFormat>,
    /// Base IRI for relative URIs
    pub base_iri: Option<String>,
    /// Merge with existing graph instead of replacing
    pub merge: bool,
}

/// Statistics from RDF loading operation
#[derive(Debug, Clone)]
pub struct LoadStats {
    /// Number of triples loaded from this file
    pub triples_loaded: usize,
    /// Total triples in graph after loading
    pub total_triples: usize,
    /// Detected or specified format
    pub format: RdfFormat,
    /// File path that was loaded
    pub file_path: String,
}

/// CLI Arguments for load command
#[derive(Debug, Clone, Default, serde::Deserialize)]
pub struct LoadInput {
    /// RDF file to load
    pub file: PathBuf,

    /// RDF format (auto-detected if not specified)
    pub format: Option<String>,

    /// Base IRI for relative URIs
    pub base_iri: Option<String>,

    /// Merge with existing graph
    #[serde(default)]
    pub merge: bool,
}

/// Load RDF data from file into graph
///
/// Chicago TDD: This performs REAL RDF file loading using Oxigraph
pub fn load_rdf(options: LoadOptions) -> Result<LoadStats> {
    // Verify file exists
    let file_path = Path::new(&options.file_path);
    if !file_path.exists() {
        anyhow::bail!("RDF file not found: {}", options.file_path);
    }

    // Detect format if not provided
    let format = options
        .format
        .unwrap_or_else(|| RdfFormat::from_extension(&options.file_path));

    // Load REAL RDF graph using Oxigraph
    let graph = Graph::load_from_file(&options.file_path)
        .context(format!("Failed to load RDF file: {}", options.file_path))?;

    // Get REAL triple count from Oxigraph
    let total_triples = graph.len();

    // For merge mode, we would load into existing graph
    // For now, we treat each load as new graph
    let triples_loaded = total_triples;

    Ok(LoadStats {
        triples_loaded,
        total_triples,
        format,
        file_path: options.file_path.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    /// Chicago TDD: Test loading REAL Turtle file
    #[test]
    fn test_load_turtle_file() -> Result<()> {
        // Create REAL Turtle file
        let turtle = r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .

            ex:alice a foaf:Person ;
                foaf:name "Alice" .

            ex:bob a foaf:Person ;
                foaf:name "Bob" .
        "#;

        let temp_file = tempfile::Builder::new()
            .suffix(".ttl")
            .tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        // Load REAL RDF file
        let options = LoadOptions {
            file_path: temp_path.clone(),
            format: Some(RdfFormat::Turtle),
            base_iri: None,
            merge: false,
        };

        let stats = load_rdf(options)?;

        // Verify REAL triple count
        assert_eq!(stats.format, RdfFormat::Turtle);
        assert_eq!(stats.file_path, temp_path);
        assert!(stats.total_triples > 0); // Should have loaded triples
        assert_eq!(stats.triples_loaded, stats.total_triples);

        Ok(())
    }

    /// Chicago TDD: Test format auto-detection from extension
    #[test]
    fn test_format_detection() {
        assert_eq!(RdfFormat::from_extension("data.ttl"), RdfFormat::Turtle);
        assert_eq!(RdfFormat::from_extension("data.nt"), RdfFormat::NTriples);
        assert_eq!(RdfFormat::from_extension("data.rdf"), RdfFormat::RdfXml);
        assert_eq!(RdfFormat::from_extension("data.jsonld"), RdfFormat::JsonLd);
        assert_eq!(RdfFormat::from_extension("data.n3"), RdfFormat::N3);
        assert_eq!(RdfFormat::from_extension("data.unknown"), RdfFormat::Turtle);
    }

    /// Chicago TDD: Test loading file with REAL graph state changes
    #[test]
    fn test_load_verifies_graph_state() -> Result<()> {
        let turtle = r#"
            @prefix ex: <http://example.org/> .
            ex:triple1 ex:predicate1 "value1" .
            ex:triple2 ex:predicate2 "value2" .
            ex:triple3 ex:predicate3 "value3" .
        "#;

        let temp_file = tempfile::Builder::new()
            .suffix(".ttl")
            .tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        let options = LoadOptions {
            file_path: temp_path,
            format: Some(RdfFormat::Turtle),
            base_iri: None,
            merge: false,
        };

        let stats = load_rdf(options)?;

        // Verify graph state changed with REAL triples
        assert!(stats.total_triples >= 3);

        Ok(())
    }

    /// Chicago TDD: Test error handling for non-existent file
    #[test]
    fn test_load_nonexistent_file_fails() {
        let options = LoadOptions {
            file_path: "/nonexistent/path/to/file.ttl".to_string(),
            format: Some(RdfFormat::Turtle),
            base_iri: None,
            merge: false,
        };

        let result = load_rdf(options);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));
    }

    /// Chicago TDD: Test loading complex RDF with multiple predicates
    #[test]
    fn test_load_complex_rdf() -> Result<()> {
        let turtle = r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            ex:person1 a foaf:Person ;
                foaf:name "John Doe" ;
                foaf:age "30"^^xsd:integer ;
                foaf:knows ex:person2 .

            ex:person2 a foaf:Person ;
                foaf:name "Jane Smith" ;
                foaf:age "28"^^xsd:integer .
        "#;

        // Create temp file with .ttl extension so format can be auto-detected
        let temp_file = tempfile::Builder::new()
            .suffix(".ttl")
            .tempfile()?;
        std::fs::write(temp_file.path(), turtle.as_bytes())?;
        let temp_path = temp_file.path().to_string_lossy().to_string();

        let options = LoadOptions {
            file_path: temp_path,
            format: Some(RdfFormat::Turtle),
            base_iri: Some("http://example.org/base/".to_string()),
            merge: false,
        };

        let stats = load_rdf(options)?;

        // Verify complex RDF loaded correctly
        assert!(stats.total_triples >= 6); // Multiple triples per subject

        Ok(())
    }
}

/// Load output for CLI
#[derive(Debug, Clone, serde::Serialize)]
pub struct LoadOutput {
    pub triples_loaded: usize,
    pub total_triples: usize,
    pub format: String,
    pub file_path: String,
}

/// Execute load operation - domain logic entry point
///
/// This is the main entry point for the load command from CLI
pub async fn execute_load(input: LoadInput) -> Result<LoadOutput> {
    // Parse format if provided
    let format = if let Some(ref format_str) = input.format {
        match format_str.to_lowercase().as_str() {
            "turtle" | "ttl" => Some(RdfFormat::Turtle),
            "ntriples" | "nt" => Some(RdfFormat::NTriples),
            "rdfxml" | "rdf" | "xml" => Some(RdfFormat::RdfXml),
            "jsonld" | "json" => Some(RdfFormat::JsonLd),
            "n3" => Some(RdfFormat::N3),
            _ => anyhow::bail!("Unsupported format: {}", format_str),
        }
    } else {
        None
    };

    // Create load options
    let options = LoadOptions {
        file_path: input.file.to_string_lossy().to_string(),
        format,
        base_iri: input.base_iri.clone(),
        merge: input.merge,
    };

    // Perform the actual load
    let stats = load_rdf(options)?;

    Ok(LoadOutput {
        triples_loaded: stats.triples_loaded,
        total_triples: stats.total_triples,
        format: stats.format.as_str().to_string(),
        file_path: stats.file_path,
    })
}

/// CLI run function - bridges sync CLI to async domain logic
pub fn run(args: &LoadInput) -> Result<()> {
    // Use tokio runtime to execute async function
    let rt = tokio::runtime::Runtime::new()
        .context("Failed to create tokio runtime")?;

    let output = rt.block_on(execute_load(args.clone()))?;

    println!("✅ Loaded {} triples from {} ({})",
        output.triples_loaded,
        output.file_path,
        output.format
    );
    if args.merge {
        println!("   Total triples in graph: {}", output.total_triples);
    }

    Ok(())
}
