//! Error types for ggen-ai


/// Errors that can occur in ggen-ai operations
#[derive(Debug, thiserror::Error)]
pub enum GgenAiError {
    /// LLM provider errors
    #[error("LLM provider error: {0}")]
    LlmProvider(String),
    
    /// HTTP request errors
    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),
    
    /// JSON serialization/deserialization errors
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    
    /// Template generation errors
    #[error("Template generation error: {0}")]
    TemplateGeneration(String),
    
    /// SPARQL query generation errors
    #[error("SPARQL generation error: {0}")]
    SparqlGeneration(String),
    
    /// Ontology generation errors
    #[error("Ontology generation error: {0}")]
    OntologyGeneration(String),
    
    /// Configuration errors
    #[error("Configuration error: {0}")]
    Configuration(String),
    
    /// Validation errors
    #[error("Validation error: {0}")]
    Validation(String),
    
    /// IO errors
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    /// UTF-8 conversion errors
    #[error("UTF-8 conversion error: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
    
    /// Generic errors from ggen-core
    #[error("ggen-core error: {0}")]
    GgenCore(#[from] ggen_utils::error::Error),
    
    /// Anyhow errors
    #[error("Generic error: {0}")]
    Anyhow(#[from] anyhow::Error),
    
    /// Tera template errors
    #[error("Template error: {0}")]
    Tera(#[from] tera::Error),
}

/// Result type for ggen-ai operations
pub type Result<T> = std::result::Result<T, GgenAiError>;

impl GgenAiError {
    /// Create a new LLM provider error
    pub fn llm_provider(msg: impl Into<String>) -> Self {
        Self::LlmProvider(msg.into())
    }
    
    /// Create a new template generation error
    pub fn template_generation(msg: impl Into<String>) -> Self {
        Self::TemplateGeneration(msg.into())
    }
    
    /// Create a new SPARQL generation error
    pub fn sparql_generation(msg: impl Into<String>) -> Self {
        Self::SparqlGeneration(msg.into())
    }
    
    /// Create a new ontology generation error
    pub fn ontology_generation(msg: impl Into<String>) -> Self {
        Self::OntologyGeneration(msg.into())
    }
    
    /// Create a new configuration error
    pub fn configuration(msg: impl Into<String>) -> Self {
        Self::Configuration(msg.into())
    }
    
    /// Create a new validation error
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }
}
