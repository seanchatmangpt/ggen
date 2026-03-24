use std::fmt;

/// Result type for API operations
pub type ApiResult<T> = Result<T, ApiError>;

/// API-related errors
#[derive(Debug)]
pub enum ApiError {
    NotFound(String),
    Conflict(String),
    ValidationFailed(String),
    VersionMismatch(String),
    MigrationFailed(String),
    DeprecationError(String),
}

impl fmt::Display for ApiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ApiError::NotFound(msg) => write!(f, "Not found: {}", msg),
            ApiError::Conflict(msg) => write!(f, "Conflict: {}", msg),
            ApiError::ValidationFailed(msg) => write!(f, "Validation failed: {}", msg),
            ApiError::VersionMismatch(msg) => write!(f, "Version mismatch: {}", msg),
            ApiError::MigrationFailed(msg) => write!(f, "Migration failed: {}", msg),
            ApiError::DeprecationError(msg) => write!(f, "Deprecation error: {}", msg),
        }
    }
}

impl std::error::Error for ApiError {}
