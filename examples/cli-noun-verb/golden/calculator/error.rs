//! Error Types - Type-safe error handling
//!
//! Two-level error hierarchy:
//! - DomainError: Business logic errors (returned by domain layer)
//! - CliError: CLI/presentation errors (returned by CLI layer)

use thiserror::Error;
use serde::Serialize;

// ============================================================================
// Domain Errors (from business logic)
// ============================================================================

/// Errors from the domain layer (pure business logic)
#[derive(Error, Debug, Clone, Serialize)]
pub enum DomainError {
    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Operation failed: {0}")]
    OperationFailed(String),

    #[error("{0}")]
    General(String),
}

impl DomainError {
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }

    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound(msg.into())
    }

    pub fn failed(msg: impl Into<String>) -> Self {
        Self::OperationFailed(msg.into())
    }
}

// ============================================================================
// CLI Errors (presentation layer)
// ============================================================================

/// Errors from the CLI layer (presentation/validation)
#[derive(Error, Debug)]
pub enum CliError {
    #[error("Domain error: {0}")]
    Domain(#[from] DomainError),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Serialization error: {0}")]
    Serialization(String),

    #[error("IO error: {0}")]
    Io(String),
}

impl CliError {
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::Domain(_) => 1,
            Self::InvalidArgument(_) => 2,
            Self::Serialization(_) => 3,
            Self::Io(_) => 4,
        }
    }
}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
        CliError::Io(err.to_string())
    }
}

// ============================================================================
// Result Type Aliases
// ============================================================================

pub type DomainResult<T> = Result<T, DomainError>;
pub type CliResult<T> = Result<T, CliError>;
