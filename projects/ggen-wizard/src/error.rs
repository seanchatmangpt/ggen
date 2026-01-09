
//! Error Types - Type-safe error handling for ggen Wizard
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

    #[error("Specification error: {0}")]
    Specification(String),

    #[error("Generation error: {0}")]
    Generation(String),

    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("IO error: {0}")]
    Io(String),

    #[error("{0}")]
    General(String),
}

impl DomainError {
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }

    pub fn specification(msg: impl Into<String>) -> Self {
        Self::Specification(msg.into())
    }

    pub fn generation(msg: impl Into<String>) -> Self {
        Self::Generation(msg.into())
    }

    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound(msg.into())
    }

    pub fn parse(msg: impl Into<String>) -> Self {
        Self::Parse(msg.into())
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
            Self::Domain(DomainError::Validation(_)) => 1,
            Self::Domain(DomainError::Specification(_)) => 2,
            Self::Domain(DomainError::Generation(_)) => 3,
            Self::Domain(DomainError::NotFound(_)) => 4,
            Self::Domain(DomainError::Parse(_)) => 5,
            Self::Domain(_) => 10,
            Self::InvalidArgument(_) => 20,
            Self::Serialization(_) => 30,
            Self::Io(_) => 40,
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
