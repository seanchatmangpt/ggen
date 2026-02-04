//! Error types for the ggen-craftplan pipeline
//!
//! All pipeline stages return `Result<T, CraftplanError>` for comprehensive error handling.

use std::path::PathBuf;
use thiserror::Error;

/// Result type alias for convenience
pub type Result<T> = std::result::Result<T, CraftplanError>;

/// Comprehensive error types for all pipeline stages
#[derive(Error, Debug)]
pub enum CraftplanError {
    /// μ₁: RDF validation failed
    #[error("RDF validation failed: {message}")]
    RdfValidation { message: String },

    /// μ₁: SHACL validation errors
    #[error("SHACL validation failed: {violations_count} violations")]
    ShaclValidation { violations_count: usize },

    /// μ₁: Dependency resolution failed
    #[error("Dependency resolution failed: {entity} depends on {missing_dependency}")]
    DependencyResolution {
        entity: String,
        missing_dependency: String,
    },

    /// μ₂: SPARQL query execution failed
    #[error("SPARQL query failed: {query}\nReason: {reason}")]
    SparqlQuery { query: String, reason: String },

    /// μ₂: OWL inference error
    #[error("OWL inference failed: {reason}")]
    OwlInference { reason: String },

    /// μ₃: Template rendering failed
    #[error("Template rendering failed for {template_name}: {reason}")]
    TemplateRendering {
        template_name: String,
        reason: String,
    },

    /// μ₃: Missing required template variable
    #[error("Missing required template variable: {variable_name} in {template_name}")]
    MissingVariable {
        variable_name: String,
        template_name: String,
    },

    /// μ₄: Canonicalization error
    #[error("Canonicalization failed: {reason}")]
    Canonicalization { reason: String },

    /// μ₅: Receipt generation failed
    #[error("Receipt generation failed: {reason}")]
    ReceiptGeneration { reason: String },

    /// I/O error with context
    #[error("I/O error for {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// File not found
    #[error("File not found: {path}")]
    FileNotFound { path: PathBuf },

    /// Parse error with context
    #[error("Failed to parse {file_type}: {reason}")]
    Parse { file_type: String, reason: String },
}

impl CraftplanError {
    /// Create an RDF validation error
    pub fn rdf_validation(message: impl Into<String>) -> Self {
        Self::RdfValidation {
            message: message.into(),
        }
    }

    /// Create a SPARQL query error
    pub fn sparql_query(query: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::SparqlQuery {
            query: query.into(),
            reason: reason.into(),
        }
    }

    /// Create a template rendering error
    pub fn template_rendering(template_name: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::TemplateRendering {
            template_name: template_name.into(),
            reason: reason.into(),
        }
    }
}

// Conversion from std::io::Error for convenience
impl From<std::io::Error> for CraftplanError {
    fn from(err: std::io::Error) -> Self {
        Self::Io {
            path: PathBuf::from("unknown"),
            source: err,
        }
    }
}
