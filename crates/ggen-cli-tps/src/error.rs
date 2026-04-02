use thiserror::Error;

/// Error result type for CLI operations.
pub type Result<T> = std::result::Result<T, CliError>;

/// Main error type for CLI operations, covering IO, validation, and domain-specific failures.
#[derive(Debug, Error)]
pub enum CliError {
    /// IO error from file system or network operations.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// JSON serialization or deserialization error.
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// Validation failed during input or state checks.
    #[error("Validation error: {0}")]
    Validation(String),

    /// Receipt verification or cryptographic validation failed.
    #[error("Receipt verification failed: {0}")]
    ReceiptVerification(String),

    /// Andon signal triggered indicating a critical issue requiring immediate attention.
    #[error("Andon signal: {0}")]
    AndonSignal(String),

    /// Error occurred during packet routing or message delivery.
    #[error("Packet routing error: {0}")]
    PacketRouting(String),

    /// Backpressure limit exceeded in queue or buffer processing.
    #[error("Backpressure limit exceeded: {0}")]
    BackpressureLimitExceeded(String),

    /// Supplier quality check or external dependency validation failed.
    #[error("Supplier quality check failed: {0}")]
    SupplierQualityFailed(String),

    /// Task encountered an invalid or unexpected state transition.
    #[error("Task state error: {0}")]
    TaskStateError(String),

    /// Operation blocked by firewall or access control policy.
    #[error("Firewall blocked: {0}")]
    FirewallBlocked(String),

    /// An unexpected error that doesn't fit into other categories.
    #[error("Unknown error: {0}")]
    Unknown(String),
}
