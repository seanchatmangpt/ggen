//! Error types for ggen-ai
//!
//! Provides comprehensive error handling for LLM provider interactions,
//! configuration issues, validation failures, and generation operations.

/// Errors that can occur in ggen-ai operations
#[derive(Debug, thiserror::Error)]
pub enum GgenAiError {
    /// OpenAI API error with status code and message
    #[error("OpenAI API error: {message} (status: {status})")]
    OpenAI { message: String, status: u16 },

    /// Anthropic API error with status code and message
    #[error("Anthropic API error: {message} (status: {status})")]
    Anthropic { message: String, status: u16 },

    /// Ollama API error with message
    #[error("Ollama API error: {message}")]
    Ollama { message: String },

    /// Generic LLM provider error
    #[error("LLM provider '{provider}' error: {message}")]
    LlmProvider { provider: String, message: String },

    /// Configuration error
    #[error("Configuration error: {message}")]
    Config { message: String },

    /// Orchestration error
    #[error("Orchestration error: {message}")]
    Orchestration { message: String },

    /// Telemetry error
    #[error("Telemetry error: {message}")]
    Telemetry { message: String },

    /// Deployment error
    #[error("Deployment error: {message}")]
    Deployment { message: String },

    /// Missing required environment variable
    #[error("Missing required environment variable: {0}. Set it with: export {0}=your_key")]
    MissingEnvVar(String),

    /// Invalid API key format or content
    #[error("Invalid API key for {provider}: {reason}. Please check your API key configuration.")]
    InvalidApiKey { provider: String, reason: String },

    /// Invalid configuration field
    #[error("Invalid configuration: {field} - {reason}")]
    InvalidConfig { field: String, reason: String },

    /// Model not found or not supported
    #[error(
        "Model '{model}' not found or not supported by {provider}. Supported models: {supported:?}"
    )]
    ModelNotFound {
        provider: String,
        model: String,
        supported: Vec<String>,
    },

    /// Network or HTTP request failed
    #[error("Network request failed: {message}")]
    NetworkError { message: String },

    /// Rate limit exceeded
    #[error("Rate limit exceeded for {provider}. Retry after: {retry_after:?} seconds")]
    RateLimitExceeded {
        provider: String,
        retry_after: Option<u64>,
    },

    /// Request validation failed
    #[error("Request validation failed: {message}")]
    ValidationError { message: String },

    /// Response parsing failed
    #[error("Failed to parse response from {provider}: {message}")]
    ParseError { provider: String, message: String },

    /// Streaming operation failed
    #[error("Streaming error for {provider}: {message}")]
    StreamError { provider: String, message: String },

    /// Timeout during operation
    #[error("Operation timed out after {seconds} seconds for {provider}")]
    Timeout { provider: String, seconds: u64 },

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

    /// Ontology processing errors (OWL extraction, SPARQL queries)
    #[error("Ontology error: {message}")]
    OntologyError { message: String },

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

    /// Generic errors from ggen-core and ggen-utils
    #[error("Generic error: {0}")]
    Generic(#[from] ggen_utils::error::Error),

    /// Tera template errors
    #[error("Template error: {0}")]
    Tera(#[from] tera::Error),

    /// Generic error from `Box<dyn std::error::Error>`
    #[error("Generic error: {message}")]
    Other { message: String },
}

/// Result type for ggen-ai operations
pub type Result<T> = std::result::Result<T, GgenAiError>;

impl GgenAiError {
    /// Create a new OpenAI error
    pub fn openai(message: impl Into<String>, status: u16) -> Self {
        Self::OpenAI {
            message: message.into(),
            status,
        }
    }

    /// Create a new Anthropic error
    pub fn anthropic(message: impl Into<String>, status: u16) -> Self {
        Self::Anthropic {
            message: message.into(),
            status,
        }
    }

    /// Create a new Ollama error
    pub fn ollama(message: impl Into<String>) -> Self {
        Self::Ollama {
            message: message.into(),
        }
    }

    /// Create a new generic LLM provider error
    pub fn llm_provider(provider: impl Into<String>, message: impl Into<String>) -> Self {
        Self::LlmProvider {
            provider: provider.into(),
            message: message.into(),
        }
    }

    /// Create a new configuration error
    pub fn config_error(message: impl Into<String>) -> Self {
        Self::Configuration(message.into())
    }

    /// Create a new orchestration error
    pub fn orchestration(message: impl Into<String>) -> Self {
        Self::Orchestration {
            message: message.into(),
        }
    }

    /// Create a new telemetry error
    pub fn telemetry(message: impl Into<String>) -> Self {
        Self::Telemetry {
            message: message.into(),
        }
    }

    /// Create a new deployment error
    pub fn deployment(message: impl Into<String>) -> Self {
        Self::Deployment {
            message: message.into(),
        }
    }

    /// Create a missing environment variable error
    pub fn missing_env_var(var: impl Into<String>) -> Self {
        Self::MissingEnvVar(var.into())
    }

    /// Create an invalid API key error
    pub fn invalid_api_key(provider: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::InvalidApiKey {
            provider: provider.into(),
            reason: reason.into(),
        }
    }

    /// Create an invalid configuration error
    pub fn invalid_config(field: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::InvalidConfig {
            field: field.into(),
            reason: reason.into(),
        }
    }

    /// Create a model not found error
    pub fn model_not_found(
        provider: impl Into<String>, model: impl Into<String>, supported: Vec<String>,
    ) -> Self {
        Self::ModelNotFound {
            provider: provider.into(),
            model: model.into(),
            supported,
        }
    }

    /// Create a network error
    pub fn network_error(message: impl Into<String>) -> Self {
        Self::NetworkError {
            message: message.into(),
        }
    }

    /// Create a rate limit error
    pub fn rate_limit(provider: impl Into<String>, retry_after: Option<u64>) -> Self {
        Self::RateLimitExceeded {
            provider: provider.into(),
            retry_after,
        }
    }

    /// Create a validation error
    pub fn validation(message: impl Into<String>) -> Self {
        Self::ValidationError {
            message: message.into(),
        }
    }

    /// Create a parse error
    pub fn parse_error(provider: impl Into<String>, message: impl Into<String>) -> Self {
        Self::ParseError {
            provider: provider.into(),
            message: message.into(),
        }
    }

    /// Create a stream error
    pub fn stream_error(provider: impl Into<String>, message: impl Into<String>) -> Self {
        Self::StreamError {
            provider: provider.into(),
            message: message.into(),
        }
    }

    /// Create a timeout error
    pub fn timeout(provider: impl Into<String>, seconds: u64) -> Self {
        Self::Timeout {
            provider: provider.into(),
            seconds,
        }
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

    /// Create a new ontology error
    pub fn ontology_error(message: impl Into<String>) -> Self {
        Self::OntologyError {
            message: message.into(),
        }
    }
}

/// Validation helpers
impl GgenAiError {
    /// Validate API key is not empty and has reasonable length
    pub fn validate_api_key(api_key: &str, provider: &str) -> Result<()> {
        if api_key.is_empty() {
            return Err(Self::invalid_api_key(provider, "API key is empty"));
        }
        if api_key.len() < 10 {
            return Err(Self::invalid_api_key(
                provider,
                "API key is too short (minimum 10 characters)",
            ));
        }
        Ok(())
    }

    /// Validate timeout value
    pub fn validate_timeout(timeout: u64, field: &str) -> Result<()> {
        if timeout == 0 {
            return Err(Self::invalid_config(
                field,
                "Timeout must be greater than 0",
            ));
        }
        if timeout > 3600 {
            return Err(Self::invalid_config(
                field,
                "Timeout cannot exceed 3600 seconds (1 hour)",
            ));
        }
        Ok(())
    }

    /// Validate temperature value
    pub fn validate_temperature(temp: f32, field: &str) -> Result<()> {
        if !(0.0..=2.0).contains(&temp) {
            return Err(Self::invalid_config(
                field,
                format!("Temperature must be between 0.0 and 2.0, got {}", temp),
            ));
        }
        Ok(())
    }

    /// Validate top_p value
    pub fn validate_top_p(top_p: f32, field: &str) -> Result<()> {
        if !(0.0..=1.0).contains(&top_p) {
            return Err(Self::invalid_config(
                field,
                format!("top_p must be between 0.0 and 1.0, got {}", top_p),
            ));
        }
        Ok(())
    }
}

impl From<Box<dyn std::error::Error>> for GgenAiError {
    fn from(err: Box<dyn std::error::Error>) -> Self {
        GgenAiError::Other {
            message: err.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_openai_error() {
        let err = GgenAiError::openai("Rate limit exceeded", 429);
        assert!(err.to_string().contains("429"));
        assert!(err.to_string().contains("OpenAI"));
    }

    #[test]
    fn test_missing_env_var_error() {
        let err = GgenAiError::missing_env_var("OPENAI_API_KEY");
        let msg = err.to_string();
        assert!(msg.contains("OPENAI_API_KEY"));
        assert!(msg.contains("export"));
    }

    #[test]
    fn test_model_not_found_error() {
        let err = GgenAiError::model_not_found(
            "openai",
            "gpt-5",
            vec!["gpt-4".to_string(), "gpt-3.5-turbo".to_string()],
        );
        let msg = err.to_string();
        assert!(msg.contains("gpt-5"));
        assert!(msg.contains("gpt-4"));
    }

    #[test]
    fn test_validate_api_key() {
        assert!(GgenAiError::validate_api_key("", "openai").is_err());
        assert!(GgenAiError::validate_api_key("short", "openai").is_err());
        assert!(GgenAiError::validate_api_key("valid-api-key-123", "openai").is_ok());
    }

    #[test]
    fn test_validate_timeout() {
        assert!(GgenAiError::validate_timeout(0, "timeout").is_err());
        assert!(GgenAiError::validate_timeout(3601, "timeout").is_err());
        assert!(GgenAiError::validate_timeout(30, "timeout").is_ok());
    }

    #[test]
    fn test_validate_temperature() {
        assert!(GgenAiError::validate_temperature(-0.1, "temperature").is_err());
        assert!(GgenAiError::validate_temperature(2.1, "temperature").is_err());
        assert!(GgenAiError::validate_temperature(0.7, "temperature").is_ok());
    }

    #[test]
    fn test_validate_top_p() {
        assert!(GgenAiError::validate_top_p(-0.1, "top_p").is_err());
        assert!(GgenAiError::validate_top_p(1.1, "top_p").is_err());
        assert!(GgenAiError::validate_top_p(0.9, "top_p").is_ok());
    }
}
