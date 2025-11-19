//! Error types for the living documentation system

use thiserror::Error;

/// Result type alias for the living documentation system
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur in the living documentation system
#[derive(Error, Debug)]
pub enum Error {
    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// RDF/ontology error
    #[error("Ontology error: {0}")]
    Ontology(String),

    /// Code extraction error
    #[error("Code extraction error: {0}")]
    Extraction(String),

    /// Narrative generation error
    #[error("Narrative generation error: {0}")]
    Narrative(String),

    /// NLU processing error
    #[error("NLU processing error: {0}")]
    Nlu(String),

    /// Template error
    #[error("Template error: {0}")]
    Template(#[from] tera::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// Web server error
    #[error("Web server error: {0}")]
    WebServer(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),

    /// Graph error
    #[error("Graph error: {0}")]
    Graph(String),

    /// Parse error
    #[error("Parse error: {0}")]
    Parse(String),

    /// Validation error
    #[error("Validation error: {0}")]
    Validation(String),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Other(s)
    }
}

impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::Other(s.to_string())
    }
}

impl From<anyhow::Error> for Error {
    fn from(e: anyhow::Error) -> Self {
        Error::Other(e.to_string())
    }
}
