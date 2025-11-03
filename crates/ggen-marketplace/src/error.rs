use std::fmt;

/// Result type alias for marketplace operations
pub type Result<T> = std::result::Result<T, MarketplaceError>;

/// Core error types for the marketplace system
#[derive(Debug)]
pub enum MarketplaceError {
    /// Package not found in registry
    PackageNotFound { package_id: String, context: String },

    /// Invalid package data or format
    InvalidPackage { reason: String, context: String },

    /// Storage operation failed
    StorageError {
        operation: String,
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Search operation failed
    SearchError {
        query: String,
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Cryptographic verification failed
    VerificationError { reason: String, context: String },

    /// Network or IO error
    IoError {
        operation: String,
        source: std::io::Error,
    },

    /// Serialization/deserialization error
    SerializationError {
        operation: String,
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Registry operation failed
    RegistryError { operation: String, reason: String },

    /// Authentication or authorization failed
    AuthError { reason: String },

    /// Rate limit exceeded
    RateLimitExceeded { retry_after: Option<u64> },

    /// Configuration error
    ConfigError { field: String, reason: String },

    /// Persistence error
    PersistenceError { reason: String },
}

impl fmt::Display for MarketplaceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PackageNotFound {
                package_id,
                context,
            } => {
                write!(f, "Package '{}' not found: {}", package_id, context)
            }
            Self::InvalidPackage { reason, context } => {
                write!(f, "Invalid package ({}): {}", context, reason)
            }
            Self::StorageError { operation, source } => {
                write!(f, "Storage error during '{}': {}", operation, source)
            }
            Self::SearchError { query, source } => {
                write!(f, "Search failed for query '{}': {}", query, source)
            }
            Self::VerificationError { reason, context } => {
                write!(f, "Verification failed ({}): {}", context, reason)
            }
            Self::IoError { operation, source } => {
                write!(f, "IO error during '{}': {}", operation, source)
            }
            Self::SerializationError { operation, source } => {
                write!(f, "Serialization error during '{}': {}", operation, source)
            }
            Self::RegistryError { operation, reason } => {
                write!(f, "Registry error during '{}': {}", operation, reason)
            }
            Self::AuthError { reason } => {
                write!(f, "Authentication failed: {}", reason)
            }
            Self::RateLimitExceeded { retry_after } => {
                if let Some(seconds) = retry_after {
                    write!(f, "Rate limit exceeded, retry after {} seconds", seconds)
                } else {
                    write!(f, "Rate limit exceeded")
                }
            }
            Self::ConfigError { field, reason } => {
                write!(f, "Configuration error for '{}': {}", field, reason)
            }
            Self::PersistenceError { reason } => {
                write!(f, "Persistence error: {}", reason)
            }
        }
    }
}

impl std::error::Error for MarketplaceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::StorageError { source, .. } => Some(source.as_ref()),
            Self::SearchError { source, .. } => Some(source.as_ref()),
            Self::IoError { source, .. } => Some(source),
            Self::SerializationError { source, .. } => Some(source.as_ref()),
            _ => None,
        }
    }
}

// Convenience methods for creating errors with context
impl MarketplaceError {
    pub fn package_not_found(package_id: impl Into<String>, context: impl Into<String>) -> Self {
        Self::PackageNotFound {
            package_id: package_id.into(),
            context: context.into(),
        }
    }

    pub fn invalid_package(reason: impl Into<String>, context: impl Into<String>) -> Self {
        Self::InvalidPackage {
            reason: reason.into(),
            context: context.into(),
        }
    }

    pub fn storage_error(
        operation: impl Into<String>, source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self::StorageError {
            operation: operation.into(),
            source: Box::new(source),
        }
    }

    pub fn search_error(
        query: impl Into<String>, source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self::SearchError {
            query: query.into(),
            source: Box::new(source),
        }
    }

    pub fn verification_error(reason: impl Into<String>, context: impl Into<String>) -> Self {
        Self::VerificationError {
            reason: reason.into(),
            context: context.into(),
        }
    }

    pub fn io_error(operation: impl Into<String>, source: std::io::Error) -> Self {
        Self::IoError {
            operation: operation.into(),
            source,
        }
    }

    pub fn registry_error(operation: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::RegistryError {
            operation: operation.into(),
            reason: reason.into(),
        }
    }

    pub fn auth_error(reason: impl Into<String>) -> Self {
        Self::AuthError {
            reason: reason.into(),
        }
    }

    pub fn config_error(field: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::ConfigError {
            field: field.into(),
            reason: reason.into(),
        }
    }

    pub fn network_error(reason: impl Into<String>) -> Self {
        Self::RegistryError {
            operation: "network".into(),
            reason: reason.into(),
        }
    }

    pub fn serialization_error(source: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self::SerializationError {
            operation: "serialize".into(),
            source: Box::new(source),
        }
    }

    // Additional helper methods for common error patterns
    pub fn not_found(message: impl Into<String>) -> Self {
        Self::PackageNotFound {
            package_id: "unknown".into(),
            context: message.into(),
        }
    }

    pub fn parse_error(message: impl Into<String>) -> Self {
        Self::InvalidPackage {
            reason: "parse error".into(),
            context: message.into(),
        }
    }

    pub fn serialize_error(message: impl Into<String>) -> Self {
        Self::SerializationError {
            operation: "serialize".into(),
            source: Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                message.into(),
            )),
        }
    }

    pub fn already_exists(message: impl Into<String>) -> Self {
        Self::RegistryError {
            operation: "create".into(),
            reason: format!("already exists: {}", message.into()),
        }
    }

    pub fn persistence_error(reason: impl Into<String>) -> Self {
        Self::PersistenceError {
            reason: reason.into(),
        }
    }
}
