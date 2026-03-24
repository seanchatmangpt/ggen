use std::fmt;

/// Result type for schema operations
pub type SchemaResult<T> = Result<T, SchemaError>;

/// Schema-related errors
#[derive(Debug)]
pub enum SchemaError {
    ValidationFailed(String),
    MigrationFailed(String),
    VersionConflict(String),
    NotFound(String),
    InvalidOperation(String),
    AuditError(String),
}

impl fmt::Display for SchemaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemaError::ValidationFailed(msg) => write!(f, "Validation failed: {}", msg),
            SchemaError::MigrationFailed(msg) => write!(f, "Migration failed: {}", msg),
            SchemaError::VersionConflict(msg) => write!(f, "Version conflict: {}", msg),
            SchemaError::NotFound(msg) => write!(f, "Not found: {}", msg),
            SchemaError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            SchemaError::AuditError(msg) => write!(f, "Audit error: {}", msg),
        }
    }
}

impl std::error::Error for SchemaError {}
