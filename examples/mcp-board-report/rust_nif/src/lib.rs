//! MCP+ NIF Library
//!
//! Cryptographic operations for sealed operating contracts.
//!
//! Core axiom: A = μ(O), μ ∘ μ = μ, hash(A) = hash(μ(O)), O ⊨ Σ

pub mod crypto;
pub mod envelope;
pub mod receipt;
pub mod refusal;
pub mod bundle;

use std::fmt;

/// MCP+ Error type
#[derive(Debug, Clone)]
pub enum McpError {
    CryptoError(String),
    EnvelopeViolation(String),
    InvalidInput(String),
    ChainError(String),
    SerializationError(String),
}

impl fmt::Display for McpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            McpError::CryptoError(msg) => write!(f, "CryptoError: {}", msg),
            McpError::EnvelopeViolation(msg) => write!(f, "EnvelopeViolation: {}", msg),
            McpError::InvalidInput(msg) => write!(f, "InvalidInput: {}", msg),
            McpError::ChainError(msg) => write!(f, "ChainError: {}", msg),
            McpError::SerializationError(msg) => write!(f, "SerializationError: {}", msg),
        }
    }
}

impl std::error::Error for McpError {}

/// Result type for MCP+ operations
pub type McpResult<T> = Result<T, McpError>;

/// Panic-safe wrapper for NIF calls
pub fn catch_panic<F, T>(f: F) -> McpResult<T>
where
    F: FnOnce() -> McpResult<T> + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(f) {
        Ok(result) => result,
        Err(_) => Err(McpError::CryptoError("Internal panic caught".to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = McpError::CryptoError("test error".to_string());
        assert_eq!(format!("{}", err), "CryptoError: test error");
    }

    #[test]
    fn test_catch_panic_success() {
        let result = catch_panic(|| Ok(42));
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_catch_panic_error() {
        let result: McpResult<i32> = catch_panic(|| {
            Err(McpError::InvalidInput("bad input".to_string()))
        });
        assert!(result.is_err());
    }
}
