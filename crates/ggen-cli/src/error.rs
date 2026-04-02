//! Semantic exit codes and error handling for ggen CLI
//!
//! Provides deterministic, agent-friendly error handling with semantic exit codes
//! that enable agents to understand why a command failed and respond appropriately.
//!
//! # Exit Codes
//! - 0: Success - operation completed successfully
//! - 1: ValidationError - RDF/SHACL/type validation failed
//! - 2: SparqlError - SPARQL query syntax/termination error
//! - 3: TemplateError - Template rendering failed
//! - 4: OutputInvalid - Generated code failed validation (not valid Rust)
//! - 5: Timeout - Operation exceeded time limit
//! - 6: FileError - File system operation failed
//! - 7: NetworkError - Network operation failed
//! - 8: ConfigError - Configuration validation failed
//! - 127: Unknown - Unexpected error

use thiserror::Error;

/// Semantic error types for ggen CLI operations
#[derive(Error, Debug)]
pub enum GgenError {
    /// RDF parsing, SHACL validation, or type consistency error
    #[error("Validation error: {0}")]
    ValidationError(String),

    /// SPARQL query syntax error, termination issue, or execution error
    #[error("SPARQL error: {0}")]
    SparqlError(String),

    /// Template rendering error (Tera error, context mismatch)
    #[error("Template error: {0}")]
    TemplateError(String),

    /// Generated code failed validation (syntax, compilation, safety checks)
    #[error("Output validation error: {0}")]
    OutputInvalid(String),

    /// Operation exceeded time limit (>5s for SPARQL, >10s for code gen)
    #[error("Operation timeout: {0}")]
    Timeout(String),

    /// File I/O error
    #[error("File error: {0}")]
    FileError(String),

    /// File I/O error with path context
    #[error("File error in {path}: {message}")]
    FileErrorWithPath { path: String, message: String },

    /// Network operation error (HTTP, API calls, etc.)
    #[error("Network error: {0}")]
    NetworkError(String),

    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    JsonError(String),

    /// Configuration file parsing or validation error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// clap noun-verb command execution error
    #[error("Command execution error: {0}")]
    CommandError(String),

    /// External tool/dependency error (LLM, marketplace, etc.)
    #[error("External service error: {0}")]
    ExternalServiceError(String),

    /// Internal error (should be avoided - use specific variants above)
    #[error("Internal error: {0}")]
    Internal(String),

    /// PaaS operation error (converted from PaasError)
    #[error("PaaS error: {0}")]
    PaasError(String),

    /// Pack receipt error (converted from PackReceiptError)
    #[error("Pack receipt error: {0}")]
    PackReceiptError(String),

    /// Validation error (converted from ValidationError)
    #[error("Validation error: {0}")]
    InvalidInput(String),
}

impl From<std::io::Error> for GgenError {
    fn from(err: std::io::Error) -> Self {
        GgenError::FileError(err.to_string())
    }
}

impl From<serde_json::error::Error> for GgenError {
    fn from(err: serde_json::error::Error) -> Self {
        GgenError::JsonError(err.to_string())
    }
}

impl GgenError {
    /// Convert from clap noun-verb error
    pub fn from_clap_error(err: clap_noun_verb::NounVerbError) -> Self {
        match err {
            clap_noun_verb::NounVerbError::argument_error(msg) => {
                GgenError::InvalidInput(msg)
            }
            clap_noun_verb::NounVerbError::execution_error(msg) => {
                GgenError::CommandError(msg)
            }
            _ => GgenError::CommandError(err.to_string()),
        }
    }

    /// Convert from PaasError
    pub fn from_paas_error(err: crate::commands::paas::errors::PaasError) -> Self {
        GgenError::PaasError(err.to_string())
    }

    /// Convert from PackReceiptError
    pub fn from_pack_receipt_error(err: crate::cmds::packs_receipt::PackReceiptError) -> Self {
        GgenError::PackReceiptError(err.to_string())
    }

    /// Convert from ValidationError
    pub fn from_validation_error(err: crate::validation::ValidationError) -> Self {
        GgenError::InvalidInput(err.to_string())
    }

    /// Create a file error with path context
    pub fn file_error(path: &str, message: &str) -> Self {
        GgenError::FileErrorWithPath {
            path: path.to_string(),
            message: message.to_string(),
        }
    }

    /// Create a network error
    pub fn network_error(message: &str) -> Self {
        GgenError::NetworkError(message.to_string())
    }

    /// Create an external service error
    pub fn external_service_error(message: &str) -> Self {
        GgenError::ExternalServiceError(message.to_string())
    }
}

impl GgenError {
    /// Get semantic exit code for this error
    pub fn exit_code(&self) -> i32 {
        match self {
            GgenError::ValidationError(_) => 1,
            GgenError::SparqlError(_) => 2,
            GgenError::TemplateError(_) => 3,
            GgenError::OutputInvalid(_) => 4,
            GgenError::Timeout(_) => 5,
            GgenError::FileError(_) => 127,
            GgenError::JsonError(_) => 127,
            GgenError::Internal(_) => 127,
        }
    }

    /// Human-readable error category for agent decisions
    pub fn category(&self) -> &'static str {
        match self {
            GgenError::ValidationError(_) => "validation",
            GgenError::SparqlError(_) => "sparql",
            GgenError::TemplateError(_) => "template",
            GgenError::OutputInvalid(_) => "output",
            GgenError::Timeout(_) => "timeout",
            GgenError::FileError(_) => "file",
            GgenError::JsonError(_) => "json",
            GgenError::Internal(_) => "internal",
        }
    }
}

/// Result type for ggen CLI operations
pub type Result<T> = std::result::Result<T, GgenError>;

/// Extension trait for easy error conversion
pub trait GgenResultExt<T> {
    /// Convert any result to GgenError using appropriate conversion
    fn to_ggen_result(self) -> Result<T>;
}

impl<T, E> GgenResultExt<T> for std::result::Result<T, E>
where
    E: ToString,
{
    fn to_ggen_result(self) -> Result<T> {
        self.map_err(|e| GgenError::Internal(e.to_string()))
    }
}

/// Macro for easy error conversion in CLI commands
#[macro_export]
macro_rules! map_err {
    ($expr:expr, $error_type:ident) => {
        $expr.map_err(|e| $crate::error::GgenError::$error_type(e.to_string()))?
    };
    ($expr:expr, $error_type:ident, $message:expr) => {
        $expr.map_err(|e| $crate::error::GgenError::$error_type(format!("{}: {}", $message, e)))?
    };
}

/// Audit trail for code generation (enables agent verification)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AuditTrail {
    /// SHA256 hash of input ontology
    pub input_ontology_hash: String,

    /// SPARQL query executed
    pub sparql_query: String,

    /// Template name used
    pub template_name: String,

    /// Generated code
    pub output_code: String,

    /// Validation passed (true = safe to commit, false = review needed)
    pub validation_passed: bool,

    /// Exit code from generation
    pub exit_code: i32,

    /// Generation time in milliseconds
    pub duration_ms: u64,

    /// Validation errors (if any)
    pub validation_errors: Vec<String>,
}

impl AuditTrail {
    /// Create new audit trail
    pub fn new(
        input_ontology_hash: String,
        sparql_query: String,
        template_name: String,
        output_code: String,
    ) -> Self {
        Self {
            input_ontology_hash,
            sparql_query,
            template_name,
            output_code,
            validation_passed: false,
            exit_code: 0,
            duration_ms: 0,
            validation_errors: Vec::new(),
        }
    }

    /// Mark validation as passed
    pub fn mark_valid(mut self) -> Self {
        self.validation_passed = true;
        self.exit_code = 0;
        self
    }

    /// Add validation error
    pub fn add_error(mut self, error: String) -> Self {
        self.validation_errors.push(error);
        self.validation_passed = false;
        self.exit_code = 4; // OutputInvalid
        self
    }

    /// Set generation duration
    pub fn with_duration(mut self, duration_ms: u64) -> Self {
        self.duration_ms = duration_ms;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exit_codes() {
        assert_eq!(GgenError::ValidationError("test".to_string()).exit_code(), 1);
        assert_eq!(GgenError::SparqlError("test".to_string()).exit_code(), 2);
        assert_eq!(GgenError::TemplateError("test".to_string()).exit_code(), 3);
        assert_eq!(GgenError::OutputInvalid("test".to_string()).exit_code(), 4);
        assert_eq!(GgenError::Timeout("test".to_string()).exit_code(), 5);
    }

    #[test]
    fn test_audit_trail() {
        let audit = AuditTrail::new(
            "abc123".to_string(),
            "SELECT ?x WHERE { ?x a rdfs:Class }".to_string(),
            "rust-service".to_string(),
            "struct User { id: Uuid }".to_string(),
        )
        .mark_valid()
        .with_duration(42);

        assert!(audit.validation_passed);
        assert_eq!(audit.exit_code, 0);
        assert_eq!(audit.duration_ms, 42);
    }
}
