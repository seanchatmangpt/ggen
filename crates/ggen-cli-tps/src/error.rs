use thiserror::Error;

pub type Result<T> = std::result::Result<T, CliError>;

#[derive(Debug, Error)]
pub enum CliError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Receipt verification failed: {0}")]
    ReceiptVerification(String),

    #[error("Andon signal: {0}")]
    AndonSignal(String),

    #[error("Packet routing error: {0}")]
    PacketRouting(String),

    #[error("Backpressure limit exceeded: {0}")]
    BackpressureLimitExceeded(String),

    #[error("Supplier quality check failed: {0}")]
    SupplierQualityFailed(String),

    #[error("Task state error: {0}")]
    TaskStateError(String),

    #[error("Firewall blocked: {0}")]
    FirewallBlocked(String),

    #[error("Unknown error: {0}")]
    Unknown(String),
}
