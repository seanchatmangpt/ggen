//! Error types for state management operations

use thiserror::Error;

/// Result type for state management operations
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur during state management operations
#[derive(Error, Debug)]
pub enum Error {
    /// Firestore operation failed
    #[error("Firestore error: {0}")]
    Firestore(String),

    /// State not found
    #[error("State not found: {0}")]
    StateNotFound(String),

    /// Version conflict detected (optimistic locking failure)
    #[error("Version conflict at document {doc_id}: expected version {expected}, found {actual}")]
    VersionConflict {
        doc_id: String,
        expected: u64,
        actual: u64,
    },

    /// Transaction failed due to concurrent modification
    #[error("Transaction failed: {0}")]
    TransactionFailed(String),

    /// Deadlock detected in transaction
    #[error("Deadlock detected: {0}")]
    DeadlockDetected(String),

    /// Operation timed out
    #[error("Operation timed out: {0}")]
    Timeout(String),

    /// Stale read detected (eventual consistency)
    #[error("Stale read detected: {context}")]
    StaleRead { context: String },

    /// Conflict resolution failed
    #[error("Conflict resolution failed: {0}")]
    ConflictResolutionFailed(String),

    /// Event sourcing error
    #[error("Event sourcing error: {0}")]
    EventSourcingError(String),

    /// Invalid state for operation
    #[error("Invalid state: {0}")]
    InvalidState(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),

    /// Lock acquisition failed
    #[error("Failed to acquire lock on {resource}: {reason}")]
    LockAcquisitionFailed { resource: String, reason: String },

    /// Lock already held
    #[error("Lock already held on {resource}")]
    LockAlreadyHeld { resource: String },

    /// Causality violation detected
    #[error("Causality violation: {0}")]
    CausalityViolation(String),

    /// Distributed consensus failed
    #[error("Consensus failed: {0}")]
    ConsensusFailed(String),
}

impl Error {
    /// Check if error is transient (can be retried)
    pub fn is_transient(&self) -> bool {
        matches!(
            self,
            Error::Firestore(_) | Error::Timeout(_) | Error::TransactionFailed(_)
        )
    }

    /// Check if error is permanent (should not be retried)
    pub fn is_permanent(&self) -> bool {
        matches!(
            self,
            Error::StateNotFound(_) | Error::InvalidState(_) | Error::SerializationError(_)
        )
    }

    /// Get a suitable backoff duration for retry
    pub fn backoff_duration(&self) -> std::time::Duration {
        use std::time::Duration;
        match self {
            Error::VersionConflict { .. } => Duration::from_millis(10),
            Error::TransactionFailed(_) => Duration::from_millis(50),
            Error::DeadlockDetected(_) => Duration::from_millis(100),
            Error::Timeout(_) => Duration::from_secs(1),
            Error::Firestore(_) => Duration::from_millis(500),
            _ => Duration::from_secs(5),
        }
    }
}
