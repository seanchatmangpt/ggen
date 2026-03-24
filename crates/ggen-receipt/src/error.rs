//! Error types for the receipt system.

use thiserror::Error;

/// Result type alias for receipt operations.
pub type Result<T> = std::result::Result<T, ReceiptError>;

/// Errors that can occur during receipt operations.
#[derive(Debug, Error)]
pub enum ReceiptError {
    /// Signature verification failed.
    #[error("Signature verification failed")]
    InvalidSignature,

    /// Chain verification failed.
    #[error("Chain verification failed: {0}")]
    InvalidChain(String),

    /// Serialization error.
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// Invalid receipt structure.
    #[error("Invalid receipt: {0}")]
    InvalidReceipt(String),

    /// Cryptographic operation failed.
    #[error("Cryptographic error: {0}")]
    Crypto(String),

    /// Hash mismatch in chain.
    #[error("Hash mismatch: expected {expected}, got {actual}")]
    HashMismatch {
        /// Expected hash value.
        expected: String,
        /// Actual hash value.
        actual: String,
    },
}
