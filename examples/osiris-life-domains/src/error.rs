//! Error types for OSIRIS Life Domains system

use thiserror::Error;

/// Result type for operations
pub type Result<T> = std::result::Result<T, LifeDomainsError>;

/// Error types for the life domains system
#[derive(Error, Debug)]
pub enum LifeDomainsError {
    /// Domain not found
    #[error("Domain not found: {0}")]
    DomainNotFound(String),

    /// Agent not found
    #[error("Agent not found: {0}")]
    AgentNotFound(String),

    /// Goal not found
    #[error("Goal not found: {0}")]
    GoalNotFound(String),

    /// Invalid domain state
    #[error("Invalid domain state: {0}")]
    InvalidState(String),

    /// Consensus failed
    #[error("Consensus failed: {0}")]
    ConsensusFailed(String),

    /// Tool execution error
    #[error("Tool execution error: {0}")]
    ToolError(String),

    /// Persistence error
    #[error("Persistence error: {0}")]
    PersistenceError(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// Generic error
    #[error("{0}")]
    Other(String),
}
