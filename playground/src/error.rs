//! Error types for HTF

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("RDF error: {0}")]
    RdfError(String),

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Shard not found: {0}")]
    ShardNotFound(String),

    #[error("Ordering conflict: {0}")]
    OrderingConflict(String),

    #[error("Schema violation: {0}")]
    SchemaViolation(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Other error: {0}")]
    Other(String),
}
