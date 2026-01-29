//! Error types for MCP+

use thiserror::Error;

/// MCP+ error type
#[derive(Debug, Error, Clone)]
pub enum McpError {
    #[error("Cryptographic error: {0}")]
    CryptoError(String),

    #[error("Invalid signature: {0}")]
    InvalidSignature(String),

    #[error("Signature verification failed: {0}")]
    SignatureVerificationFailed(String),

    #[error("Invalid public key: {0}")]
    InvalidPublicKey(String),

    #[error("Envelope violation: {0}")]
    EnvelopeViolation(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("Chain error: {0}")]
    ChainError(String),

    #[error("Merkle tree error: {0}")]
    MerkleError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("Contract error: {0}")]
    ContractError(String),

    #[error("Kill switch active: {0}")]
    KillSwitchActive(String),

    #[error("Epoch error: {0}")]
    EpochError(String),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Delegation error: {0}")]
    DelegationError(String),

    #[error("Capability violation: {0}")]
    CapabilityViolation(String),

    #[error("Delegation expired: {0}")]
    DelegationExpired(String),

    #[error("Constraint violation: {0}")]
    ConstraintViolation(String),
}

impl From<std::io::Error> for McpError {
    fn from(e: std::io::Error) -> Self {
        McpError::IoError(e.to_string())
    }
}

impl From<serde_json::Error> for McpError {
    fn from(e: serde_json::Error) -> Self {
        McpError::SerializationError(e.to_string())
    }
}

/// Result type for MCP+ operations
pub type McpResult<T> = Result<T, McpError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = McpError::CryptoError("test".to_string());
        assert!(err.to_string().contains("Cryptographic error"));
    }

    #[test]
    fn test_error_from_io() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let mcp_err: McpError = io_err.into();
        assert!(matches!(mcp_err, McpError::IoError(_)));
    }
}
