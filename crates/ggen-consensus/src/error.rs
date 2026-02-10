//! Error types for Byzantine consensus

use thiserror::Error;

/// Result type for consensus operations
pub type Result<T> = std::result::Result<T, ConsensusError>;

/// Errors that can occur during consensus operations
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ConsensusError {
    /// Quorum not reached
    #[error("Quorum not reached: got {got} votes, need {needed}")]
    QuorumNotReached { got: usize, needed: usize },

    /// Invalid signature
    #[error("Invalid signature from replica {replica_id}")]
    InvalidSignature { replica_id: u64 },

    /// Duplicate vote from same replica
    #[error("Duplicate vote from replica {replica_id}")]
    DuplicateVote { replica_id: u64 },

    /// Byzantine fault detected
    #[error("Byzantine fault detected: {reason}")]
    ByzantineFault { reason: String },

    /// Invalid view number
    #[error("Invalid view number: expected {expected}, got {got}")]
    InvalidView { expected: u64, got: u64 },

    /// Invalid sequence number
    #[error("Invalid sequence number: expected {expected}, got {got}")]
    InvalidSequence { expected: u64, got: u64 },

    /// Message digest mismatch
    #[error("Message digest mismatch")]
    DigestMismatch,

    /// Insufficient replicas
    #[error("Insufficient replicas: got {got}, need at least {needed} (3f+1)")]
    InsufficientReplicas { got: usize, needed: usize },

    /// Invalid replica configuration
    #[error("Invalid replica configuration: {reason}")]
    InvalidConfiguration { reason: String },

    /// Timeout waiting for consensus
    #[error("Consensus timeout after {duration_ms}ms")]
    Timeout { duration_ms: u64 },

    /// Invalid phase transition
    #[error("Invalid phase transition from {from} to {to}")]
    InvalidPhaseTransition { from: String, to: String },

    /// Missing primary replica
    #[error("Primary replica {replica_id} not found")]
    MissingPrimary { replica_id: u64 },

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),
}

impl From<serde_json::Error> for ConsensusError {
    fn from(err: serde_json::Error) -> Self {
        ConsensusError::Serialization(err.to_string())
    }
}
