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
}
