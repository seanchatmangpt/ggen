//! Error types for the marketplace system
//!
//! Uses thiserror for ergonomic error handling with context preservation.

use std::fmt;
use thiserror::Error;

/// Result type for marketplace operations
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Comprehensive error type for marketplace operations
#[derive(Error, Debug)]
pub enum Error {
    /// Package not found in registry
    #[error("Package not found: {package_id}")]
    PackageNotFound {
        /// The ID of the package that was not found
        package_id: String,
    },

    /// Invalid package ID format
    #[error("Invalid package ID format: {reason}")]
    InvalidPackageId {
        /// The reason the package ID is invalid
        reason: String,
    },

    /// Invalid semantic version
    #[error("Invalid semantic version: {version}, reason: {reason}")]
    InvalidVersion {
        /// The invalid version string
        version: String,
        /// The reason the version is invalid
        reason: String,
    },

    /// Package already exists
    #[error("Package already exists: {package_id}")]
    PackageAlreadyExists {
        /// The ID of the package that already exists
        package_id: String,
    },

    /// Version already exists
    #[error("Version {version} already exists for package {package_id}")]
    VersionAlreadyExists {
        /// The ID of the package
        package_id: String,
        /// The version that already exists
        version: String,
    },

    /// Dependency resolution failed
    #[error("Dependency resolution failed for {package_id}: {reason}")]
    DependencyResolutionFailed {
        /// The package ID
        package_id: String,
        /// The reason resolution failed
        reason: String,
    },

    /// Installation failed
    #[error("Installation failed: {reason}")]
    InstallationFailed {
        /// The reason installation failed
        reason: String,
    },

    /// Validation failed
    #[error("Validation failed: {reason}")]
    ValidationFailed {
        /// The reason validation failed
        reason: String,
    },

    /// Security check failed
    #[error("Security check failed: {reason}")]
    SecurityCheckFailed {
        /// The reason the security check failed
        reason: String,
    },

    /// Signature verification failed
    #[error("Signature verification failed: {reason}")]
    SignatureVerificationFailed {
        /// The reason signature verification failed
        reason: String,
    },

    /// I/O error
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// TOML parsing error
    #[error("TOML parsing error: {0}")]
    TomlError(#[from] toml::de::Error),

    /// UTF-8 decoding error
    #[error("UTF-8 decoding error: {0}")]
    Utf8Error(#[from] std::string::FromUtf8Error),

    /// Cryptographic error
    #[error("Cryptographic error: {0}")]
    CryptoError(String),

    /// Search error
    #[error("Search error: {0}")]
    SearchError(String),

    /// Registry error
    #[error("Registry error: {0}")]
    RegistryError(String),

    /// Concurrency error (channel closed)
    #[error("Concurrency error: {0}")]
    ConcurrencyError(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Timeout error
    #[error("Operation timed out: {0}")]
    Timeout(String),

    /// RDF/SPARQL error
    #[error("SPARQL error in query: {query}, reason: {reason}")]
    SparqlError { query: String, reason: String },

    /// RDF store error
    #[error("RDF store error during {operation}: {reason}")]
    RdfStoreError { operation: String, reason: String },

    /// Invalid state transition
    #[error("Invalid state transition from {from} to {to}")]
    InvalidStateTransition { from: String, to: String },

    /// Unknown state
    #[error("Unknown state: {state}")]
    UnknownState { state: String },

    /// Configuration error (from Turtle config)
    #[error("Configuration error: {message}")]
    ConfigurationError { message: String },

    /// Feature not implemented
    #[error("Feature not implemented: {feature}")]
    NotImplemented { feature: String },

    /// Generic error with context
    #[error("{0}")]
    Other(String),
}

impl Error {
    /// Create a new "Package not found" error
    pub fn package_not_found(package_id: impl Into<String>) -> Self {
        Self::PackageNotFound {
            package_id: package_id.into(),
        }
    }

    /// Create a new "Invalid package ID" error
    pub fn invalid_package_id(reason: impl Into<String>) -> Self {
        Self::InvalidPackageId {
            reason: reason.into(),
        }
    }

    /// Create a new "Invalid version" error
    pub fn invalid_version(version: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::InvalidVersion {
            version: version.into(),
            reason: reason.into(),
        }
    }

    /// Create a new "Dependency resolution failed" error
    pub fn dependency_resolution_failed(
        package_id: impl Into<String>, reason: impl Into<String>,
    ) -> Self {
        Self::DependencyResolutionFailed {
            package_id: package_id.into(),
            reason: reason.into(),
        }
    }

    /// Create a new cryptographic error
    pub fn crypto_error(reason: impl Into<String>) -> Self {
        Self::CryptoError(reason.into())
    }

    /// Create a new search error
    pub fn search_error(reason: impl Into<String>) -> Self {
        Self::SearchError(reason.into())
    }

    /// Create a new registry error
    pub fn registry_error(reason: impl Into<String>) -> Self {
        Self::RegistryError(reason.into())
    }

    /// Create a new concurrency error
    pub fn concurrency_error(reason: impl Into<String>) -> Self {
        Self::ConcurrencyError(reason.into())
    }

    /// Create a new configuration error
    pub fn config_error(reason: impl Into<String>) -> Self {
        Self::ConfigError(reason.into())
    }

    /// Create a new timeout error
    pub fn timeout(reason: impl Into<String>) -> Self {
        Self::Timeout(reason.into())
    }
}

/// Additional context that can be attached to errors
#[derive(Debug, Clone)]
pub struct ErrorContext {
    /// Operation that was being performed
    pub operation: String,
    /// Context information
    pub context: String,
}

impl ErrorContext {
    /// Create new error context
    pub fn new(operation: impl Into<String>, context: impl Into<String>) -> Self {
        Self {
            operation: operation.into(),
            context: context.into(),
        }
    }
}

impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.operation, self.context)
    }
}

/// Error severity level for categorization
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorSeverity {
    /// Informational - operation completed with notes
    Info,
    /// Warning - operation completed but with issues
    Warning,
    /// Error - operation failed but recoverable
    Error,
    /// Critical - operation failed, requires intervention
    Critical,
}

impl fmt::Display for ErrorSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Info => write!(f, "INFO"),
            Self::Warning => write!(f, "WARN"),
            Self::Error => write!(f, "ERROR"),
            Self::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// Error category for grouping and metrics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCategory {
    /// Registry-related errors (not found, already exists)
    Registry,
    /// Installation errors (dependency failed, conflict)
    Installation,
    /// Validation errors (invalid package, failed checks)
    Validation,
    /// Security errors (signature invalid, auth failed)
    Security,
    /// Search errors (query invalid, index error)
    Search,
    /// RDF/SPARQL errors (store operation failed)
    Rdf,
    /// I/O errors (file, network)
    Io,
    /// Configuration errors
    Config,
    /// Timeout errors
    Timeout,
    /// Unknown/other errors
    Unknown,
}

impl fmt::Display for ErrorCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Registry => write!(f, "registry"),
            Self::Installation => write!(f, "installation"),
            Self::Validation => write!(f, "validation"),
            Self::Security => write!(f, "security"),
            Self::Search => write!(f, "search"),
            Self::Rdf => write!(f, "rdf"),
            Self::Io => write!(f, "io"),
            Self::Config => write!(f, "config"),
            Self::Timeout => write!(f, "timeout"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

impl Error {
    /// Get the error category for metrics and logging
    pub fn category(&self) -> ErrorCategory {
        match self {
            Self::PackageNotFound { .. }
            | Self::PackageAlreadyExists { .. }
            | Self::VersionAlreadyExists { .. }
            | Self::RegistryError(_) => ErrorCategory::Registry,

            Self::DependencyResolutionFailed { .. } | Self::InstallationFailed { .. } => {
                ErrorCategory::Installation
            }

            Self::InvalidPackageId { .. }
            | Self::InvalidVersion { .. }
            | Self::ValidationFailed { .. }
            | Self::InvalidStateTransition { .. }
            | Self::UnknownState { .. } => ErrorCategory::Validation,

            Self::SecurityCheckFailed { .. }
            | Self::SignatureVerificationFailed { .. }
            | Self::CryptoError(_) => ErrorCategory::Security,

            Self::SearchError(_) | Self::SparqlError { .. } => ErrorCategory::Search,

            Self::RdfStoreError { .. } => ErrorCategory::Rdf,

            Self::IoError(_) | Self::SerializationError(_) | Self::Utf8Error(_) => {
                ErrorCategory::Io
            }

            Self::ConfigError(_) | Self::ConfigurationError { .. } | Self::TomlError(_) => {
                ErrorCategory::Config
            }

            Self::Timeout(_) => ErrorCategory::Timeout,

            Self::ConcurrencyError(_)
            | Self::NotImplemented { .. }
            | Self::Other(_) => ErrorCategory::Unknown,
        }
    }

    /// Get the error severity
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            Self::PackageNotFound { .. }
            | Self::InvalidPackageId { .. }
            | Self::InvalidVersion { .. } => ErrorSeverity::Error,

            Self::PackageAlreadyExists { .. }
            | Self::VersionAlreadyExists { .. } => ErrorSeverity::Warning,

            Self::SecurityCheckFailed { .. }
            | Self::SignatureVerificationFailed { .. }
            | Self::CryptoError(_) => ErrorSeverity::Critical,

            Self::DependencyResolutionFailed { .. }
            | Self::InstallationFailed { .. }
            | Self::ValidationFailed { .. } => ErrorSeverity::Error,

            Self::RdfStoreError { .. }
            | Self::SparqlError { .. } => ErrorSeverity::Error,

            Self::Timeout(_) => ErrorSeverity::Warning,

            Self::NotImplemented { .. } => ErrorSeverity::Info,

            _ => ErrorSeverity::Error,
        }
    }

    /// Get a recovery suggestion for this error
    pub fn recovery_suggestion(&self) -> Option<String> {
        match self {
            Self::PackageNotFound { package_id } => Some(format!(
                "Use 'ggen marketplace search' to find available packages. \
                 Check spelling of '{}'.",
                package_id
            )),

            Self::InvalidPackageId { .. } => Some(
                "Package IDs must be lowercase, contain only alphanumeric characters, \
                 hyphens, and underscores. They cannot start or end with a hyphen."
                    .to_string(),
            ),

            Self::InvalidVersion { .. } => {
                Some("Use semantic versioning format (e.g., 1.0.0, 2.1.0-alpha).".to_string())
            }

            Self::PackageAlreadyExists { .. } => {
                Some("Use a different package ID or publish a new version.".to_string())
            }

            Self::VersionAlreadyExists { .. } => {
                Some("Increment the version number before publishing.".to_string())
            }

            Self::DependencyResolutionFailed { .. } => Some(
                "Check that all dependencies exist and version constraints are satisfiable."
                    .to_string(),
            ),

            Self::InstallationFailed { .. } => {
                Some("Check disk space, permissions, and network connectivity.".to_string())
            }

            Self::ValidationFailed { .. } => {
                Some("Fix the validation errors before publishing.".to_string())
            }

            Self::SecurityCheckFailed { .. } => {
                Some("Review the security requirements and fix any issues.".to_string())
            }

            Self::SignatureVerificationFailed { .. } => Some(
                "Ensure the package was signed correctly and the signing key is valid.".to_string(),
            ),

            Self::SearchError(_) => Some("Check your query syntax and try again.".to_string()),

            Self::Timeout(_) => Some("Try again or check network connectivity.".to_string()),

            Self::RdfStoreError { .. } => {
                Some("This may be a temporary issue. Try again or check the store status.".to_string())
            }

            Self::SparqlError { .. } => Some("Check the SPARQL query syntax.".to_string()),

            Self::ConfigError(_) | Self::ConfigurationError { .. } => {
                Some("Check your configuration file for errors.".to_string())
            }

            Self::IoError(_) => {
                Some("Check file permissions and that the file exists.".to_string())
            }

            _ => None,
        }
    }

    /// Create error with context chain
    pub fn with_context(self, context: ErrorContext) -> ErrorChain {
        ErrorChain {
            error: self,
            context: vec![context],
        }
    }

    /// Check if this error is retryable
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            Self::Timeout(_)
                | Self::RdfStoreError { .. }
                | Self::ConcurrencyError(_)
                | Self::IoError(_)
        )
    }
}

/// Error chain with context for detailed debugging
#[derive(Debug)]
pub struct ErrorChain {
    /// The root error
    pub error: Error,
    /// Context chain (most recent first)
    pub context: Vec<ErrorContext>,
}

impl ErrorChain {
    /// Add more context to the chain
    pub fn with_context(mut self, context: ErrorContext) -> Self {
        self.context.push(context);
        self
    }

    /// Get the full context path
    pub fn context_path(&self) -> String {
        self.context
            .iter()
            .map(|c| c.operation.as_str())
            .collect::<Vec<_>>()
            .join(" -> ")
    }
}

impl fmt::Display for ErrorChain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Error: {}", self.error)?;
        if !self.context.is_empty() {
            writeln!(f, "Context:")?;
            for ctx in &self.context {
                writeln!(f, "  - {}", ctx)?;
            }
        }
        if let Some(suggestion) = self.error.recovery_suggestion() {
            writeln!(f, "Suggestion: {}", suggestion)?;
        }
        Ok(())
    }
}

impl std::error::Error for ErrorChain {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = Error::package_not_found("test-pkg");
        assert!(err.to_string().contains("Package not found"));

        let err = Error::invalid_package_id("contains spaces");
        assert!(err.to_string().contains("Invalid package ID"));
    }

    #[test]
    fn test_error_context() {
        let ctx = ErrorContext::new("searching", "query too short");
        assert_eq!(ctx.operation, "searching");
        assert_eq!(ctx.context, "query too short");
    }

    #[test]
    fn test_error_category() {
        let err = Error::package_not_found("test");
        assert_eq!(err.category(), ErrorCategory::Registry);

        let err = Error::SignatureVerificationFailed {
            reason: "invalid".to_string(),
        };
        assert_eq!(err.category(), ErrorCategory::Security);
    }

    #[test]
    fn test_error_severity() {
        let err = Error::SecurityCheckFailed {
            reason: "test".to_string(),
        };
        assert_eq!(err.severity(), ErrorSeverity::Critical);

        let err = Error::package_not_found("test");
        assert_eq!(err.severity(), ErrorSeverity::Error);
    }

    #[test]
    fn test_error_recovery_suggestion() {
        let err = Error::package_not_found("test-pkg");
        assert!(err.recovery_suggestion().is_some());

        let err = Error::InvalidVersion {
            version: "bad".to_string(),
            reason: "test".to_string(),
        };
        assert!(err.recovery_suggestion().unwrap().contains("semantic"));
    }

    #[test]
    fn test_error_chain() {
        let err = Error::package_not_found("test-pkg");
        let chain = err
            .with_context(ErrorContext::new("install", "during dependency resolution"))
            .with_context(ErrorContext::new("cli", "user request"));

        assert_eq!(chain.context.len(), 2);
        assert!(chain.context_path().contains("install"));
    }

    #[test]
    fn test_is_retryable() {
        let err = Error::Timeout("test".to_string());
        assert!(err.is_retryable());

        let err = Error::package_not_found("test");
        assert!(!err.is_retryable());
    }
}
