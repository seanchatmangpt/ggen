//! Error types for gpack operations (T006)
//!
//! This module defines the error hierarchy for all gpack-related operations,
//! following the Result<T, E> pattern required by the project constitution.
//!
//! ## Error Categories
//!
//! - `FormatError`: Package format parsing errors
//! - `ManifestError`: Manifest validation errors
//! - `CratesError`: crates.io registry errors
//! - `ResolverError`: Dependency resolution errors
//! - `ValidationError`: FMEA and quality validation errors
//! - `CacheError`: Cache operation errors

use std::fmt;
use thiserror::Error;

/// Result type alias for gpack operations
pub type GpackResult<T> = Result<T, GpackError>;

/// Comprehensive error type for gpack operations
#[derive(Error, Debug)]
pub enum GpackError {
    // =====================
    // Format Errors (T006 requirement)
    // =====================
    /// Package format errors
    #[error("Invalid package format: {0}")]
    InvalidFormat(String),

    /// Format parsing error with context
    #[error("Format parsing error in {format}: {reason}")]
    FormatParseError {
        /// The format being parsed (yaml, toml, json)
        format: String,
        /// The reason parsing failed
        reason: String,
    },

    // =====================
    // Manifest Errors (T006 requirement)
    // =====================
    /// Manifest parsing errors
    #[error("Manifest error: {0}")]
    ManifestError(String),

    /// Manifest field validation error
    #[error("Manifest error in field '{field}': {reason}")]
    ManifestFieldError {
        /// The field that failed validation
        field: String,
        /// The reason for the failure
        reason: String,
    },

    /// Required field missing from manifest
    #[error("Missing required field: {0}")]
    MissingField(String),

    // =====================
    // Crates.io Errors (T006 requirement)
    // =====================
    /// Crates.io client errors
    #[error("Registry error: {0}")]
    RegistryError(String),

    /// Crates.io operation error with context
    #[error("Crates.io error: {operation} failed: {reason}")]
    CratesOperationError {
        /// The operation being performed
        operation: String,
        /// The reason for failure
        reason: String,
    },

    /// Rate limited by crates.io
    #[error("Rate limited by crates.io, retry after {retry_after_secs}s")]
    RateLimited {
        /// Seconds until retry is allowed
        retry_after_secs: u64,
    },

    // =====================
    // Resolver Errors (T006 requirement)
    // =====================
    /// Dependency resolution errors
    #[error("Resolution error: {0}")]
    ResolutionError(String),

    /// Circular dependency detected
    #[error("Circular dependency detected: {cycle}")]
    CircularDependency {
        /// The dependency cycle (e.g., "A -> B -> C -> A")
        cycle: String,
    },

    /// Version conflict between dependencies
    #[error("Version conflict for {package}: {requirement1} vs {requirement2}")]
    VersionConflict {
        /// The package with conflicting requirements
        package: String,
        /// First version requirement
        requirement1: String,
        /// Second version requirement
        requirement2: String,
    },

    /// No version satisfies constraints
    #[error("No version of {package} satisfies constraint: {constraint}")]
    NoSatisfyingVersion {
        /// The package name
        package: String,
        /// The constraint that couldn't be satisfied
        constraint: String,
    },

    // =====================
    // Validation Errors (T006 requirement)
    // =====================
    /// Validation errors
    #[error("Validation error: {0}")]
    ValidationError(String),

    /// Validation failed with multiple violations
    #[error("Validation failed for {context}: {violations:?}")]
    ValidationFailedMultiple {
        /// The validation context
        context: String,
        /// List of violations
        violations: Vec<String>,
    },

    /// FMEA violation detected
    #[error("FMEA violation: {failure_mode} (RPN: {rpn})")]
    FmeaViolation { failure_mode: String, rpn: u32 },

    /// FMEA failure with control information
    #[error("FMEA failure: {failure_mode} (severity: {severity})")]
    FmeaFailure {
        /// The failure mode detected
        failure_mode: String,
        /// Severity level (1-10)
        severity: u8,
        /// The control that should prevent this
        control: String,
    },

    /// Poka-yoke validation failure
    #[error("Poka-yoke check failed: {0}")]
    PokaYokeFailure(String),

    /// Poka-yoke guard triggered with context
    #[error("Poka-yoke guard triggered: {guard}: {message}")]
    PokayokeGuardTriggered {
        /// The guard that was triggered
        guard: String,
        /// Error message
        message: String,
    },

    /// Quality tier not met
    #[error("Quality tier {required} required, but package is {actual}")]
    QualityTierNotMet {
        /// Required tier
        required: String,
        /// Actual tier
        actual: String,
    },

    // =====================
    // Cache Errors (T006 requirement)
    // =====================
    /// Cache errors
    #[error("Cache error: {0}")]
    CacheError(String),

    /// Cache operation failed with context
    #[error("Cache error during {operation}: {reason}")]
    CacheOperationError {
        /// The cache operation
        operation: String,
        /// The reason for failure
        reason: String,
    },

    /// Cache entry not found
    #[error("Cache miss for key: {0}")]
    CacheMiss(String),

    /// Cache entry expired
    #[error("Cache entry expired: {key} (expired at {expired_at})")]
    CacheExpired {
        /// The expired cache key
        key: String,
        /// When the entry expired
        expired_at: String,
    },

    // =====================
    // Network/HTTP errors
    // =====================
    /// Network/HTTP errors
    #[error("Network error: {0}")]
    NetworkError(String),

    // =====================
    // Lockfile errors
    // =====================
    /// Lockfile errors
    #[error("Lockfile error: {0}")]
    LockfileError(String),

    // =====================
    // Search errors
    // =====================
    /// Search errors
    #[error("Search error: {0}")]
    SearchError(String),

    // =====================
    // Quality scoring errors
    // =====================
    /// Quality scoring errors
    #[error("Quality error: {0}")]
    QualityError(String),

    // =====================
    // IO/Serialization errors
    // =====================
    /// IO errors
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Serialization errors
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// Checksum verification errors
    #[error("Checksum mismatch: expected {expected}, got {actual}")]
    ChecksumMismatch { expected: String, actual: String },

    /// Version constraint errors
    #[error("Version constraint error: {0}")]
    VersionConstraintError(String),

    /// Invalid version constraint syntax
    #[error("Invalid version constraint: {constraint}, reason: {reason}")]
    InvalidConstraint {
        /// The invalid constraint
        constraint: String,
        /// Why it's invalid
        reason: String,
    },

    /// Package not found
    #[error("Package not found: {0}")]
    PackageNotFound(String),

    /// Version not found
    #[error("Version {version} not found for package {package}")]
    VersionNotFound {
        /// The package name
        package: String,
        /// The requested version
        version: String,
    },

    // =====================
    // Internal errors
    // =====================
    /// Internal error (should not happen)
    #[error("Internal error: {0}")]
    Internal(String),

    /// Feature not implemented
    #[error("Feature not implemented: {0}")]
    NotImplemented(String),
}

impl From<serde_json::Error> for GpackError {
    fn from(err: serde_json::Error) -> Self {
        GpackError::SerializationError(err.to_string())
    }
}

impl From<toml::de::Error> for GpackError {
    fn from(err: toml::de::Error) -> Self {
        GpackError::SerializationError(err.to_string())
    }
}

impl From<reqwest::Error> for GpackError {
    fn from(err: reqwest::Error) -> Self {
        GpackError::NetworkError(err.to_string())
    }
}

// =====================
// Error helper constructors
// =====================
impl GpackError {
    /// Create a format parsing error
    pub fn format_error(format: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::FormatParseError {
            format: format.into(),
            reason: reason.into(),
        }
    }

    /// Create a manifest field error
    pub fn manifest_field(field: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::ManifestFieldError {
            field: field.into(),
            reason: reason.into(),
        }
    }

    /// Create a crates.io operation error
    pub fn crates_error(operation: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::CratesOperationError {
            operation: operation.into(),
            reason: reason.into(),
        }
    }

    /// Create a cache operation error
    pub fn cache_error(operation: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::CacheOperationError {
            operation: operation.into(),
            reason: reason.into(),
        }
    }

    /// Create an internal error
    pub fn internal(message: impl Into<String>) -> Self {
        Self::Internal(message.into())
    }

    /// Check if this error is retryable
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            Self::RateLimited { .. }
                | Self::NetworkError(_)
                | Self::CacheError(_)
                | Self::CacheOperationError { .. }
                | Self::IoError(_)
        )
    }

    /// Check if this is a user error (vs system error)
    pub fn is_user_error(&self) -> bool {
        matches!(
            self,
            Self::ManifestError(_)
                | Self::ManifestFieldError { .. }
                | Self::MissingField(_)
                | Self::InvalidConstraint { .. }
                | Self::ValidationError(_)
                | Self::ValidationFailedMultiple { .. }
                | Self::InvalidFormat(_)
                | Self::FormatParseError { .. }
        )
    }

    /// Get the error category for metrics
    pub fn category(&self) -> ErrorCategory {
        match self {
            Self::InvalidFormat(_) | Self::FormatParseError { .. } => ErrorCategory::Format,

            Self::ManifestError(_)
            | Self::ManifestFieldError { .. }
            | Self::MissingField(_) => ErrorCategory::Manifest,

            Self::RegistryError(_)
            | Self::CratesOperationError { .. }
            | Self::PackageNotFound(_)
            | Self::VersionNotFound { .. }
            | Self::RateLimited { .. } => ErrorCategory::Crates,

            Self::ResolutionError(_)
            | Self::CircularDependency { .. }
            | Self::VersionConflict { .. }
            | Self::NoSatisfyingVersion { .. }
            | Self::VersionConstraintError(_)
            | Self::InvalidConstraint { .. } => ErrorCategory::Resolver,

            Self::ValidationError(_)
            | Self::ValidationFailedMultiple { .. }
            | Self::FmeaViolation { .. }
            | Self::FmeaFailure { .. }
            | Self::PokaYokeFailure(_)
            | Self::PokayokeGuardTriggered { .. }
            | Self::QualityTierNotMet { .. }
            | Self::QualityError(_) => ErrorCategory::Validation,

            Self::CacheError(_)
            | Self::CacheOperationError { .. }
            | Self::CacheMiss(_)
            | Self::CacheExpired { .. } => ErrorCategory::Cache,

            Self::IoError(_)
            | Self::SerializationError(_)
            | Self::ChecksumMismatch { .. }
            | Self::NetworkError(_) => ErrorCategory::Io,

            Self::Internal(_)
            | Self::NotImplemented(_)
            | Self::LockfileError(_)
            | Self::SearchError(_) => ErrorCategory::Internal,
        }
    }
}

/// Error category for grouping and metrics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCategory {
    /// Format parsing errors
    Format,
    /// Manifest validation errors
    Manifest,
    /// Crates.io integration errors
    Crates,
    /// Dependency resolution errors
    Resolver,
    /// Validation errors
    Validation,
    /// Cache errors
    Cache,
    /// I/O errors
    Io,
    /// Internal/system errors
    Internal,
}

impl fmt::Display for ErrorCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Format => "format",
            Self::Manifest => "manifest",
            Self::Crates => "crates",
            Self::Resolver => "resolver",
            Self::Validation => "validation",
            Self::Cache => "cache",
            Self::Io => "io",
            Self::Internal => "internal",
        };
        write!(f, "{}", name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = GpackError::InvalidFormat("missing header".to_string());
        assert!(err.to_string().contains("Invalid package format"));
    }

    #[test]
    fn test_checksum_mismatch_display() {
        let err = GpackError::ChecksumMismatch {
            expected: "abc123".to_string(),
            actual: "def456".to_string(),
        };
        assert!(err.to_string().contains("abc123"));
        assert!(err.to_string().contains("def456"));
    }

    #[test]
    fn test_fmea_violation_display() {
        let err = GpackError::FmeaViolation {
            failure_mode: "invalid_checksum".to_string(),
            rpn: 504,
        };
        assert!(err.to_string().contains("invalid_checksum"));
        assert!(err.to_string().contains("504"));
    }

    #[test]
    fn test_error_category() {
        let err = GpackError::ManifestError("test".to_string());
        assert_eq!(err.category(), ErrorCategory::Manifest);

        let err = GpackError::CacheError("test".to_string());
        assert_eq!(err.category(), ErrorCategory::Cache);
    }

    #[test]
    fn test_is_retryable() {
        let err = GpackError::RateLimited { retry_after_secs: 60 };
        assert!(err.is_retryable());

        let err = GpackError::ManifestError("test".to_string());
        assert!(!err.is_retryable());
    }

    #[test]
    fn test_is_user_error() {
        let err = GpackError::ManifestError("test".to_string());
        assert!(err.is_user_error());

        let err = GpackError::Internal("system error".to_string());
        assert!(!err.is_user_error());
    }

    #[test]
    fn test_helper_constructors() {
        let err = GpackError::format_error("yaml", "invalid syntax");
        assert!(matches!(err, GpackError::FormatParseError { .. }));

        let err = GpackError::manifest_field("version", "invalid format");
        assert!(matches!(err, GpackError::ManifestFieldError { .. }));

        let err = GpackError::crates_error("fetch", "network timeout");
        assert!(matches!(err, GpackError::CratesOperationError { .. }));

        let err = GpackError::cache_error("get", "not found");
        assert!(matches!(err, GpackError::CacheOperationError { .. }));
    }

    #[test]
    fn test_version_conflict_display() {
        let err = GpackError::VersionConflict {
            package: "tokio".to_string(),
            requirement1: ">=1.0".to_string(),
            requirement2: "<0.5".to_string(),
        };
        let display = err.to_string();
        assert!(display.contains("tokio"));
        assert!(display.contains(">=1.0"));
        assert!(display.contains("<0.5"));
    }

    #[test]
    fn test_circular_dependency_display() {
        let err = GpackError::CircularDependency {
            cycle: "A -> B -> C -> A".to_string(),
        };
        assert!(err.to_string().contains("A -> B -> C -> A"));
    }

    #[test]
    fn test_cache_expired_display() {
        let err = GpackError::CacheExpired {
            key: "pkg@1.0.0".to_string(),
            expired_at: "2024-01-15T10:00:00Z".to_string(),
        };
        assert!(err.to_string().contains("pkg@1.0.0"));
    }
}
