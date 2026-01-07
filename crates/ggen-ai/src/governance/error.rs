//! Error types for governance system

/// Governance errors
#[derive(Debug, thiserror::Error)]
pub enum GovernanceError {
    #[error("Policy not found: {0}")]
    PolicyNotFound(String),

    #[error("Invalid policy pattern: {0}")]
    InvalidPattern(String),

    #[error("Audit error: {0}")]
    AuditError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("Safety error: {0}")]
    SafetyError(String),

    #[error("Snapshot not found: {0}")]
    SnapshotNotFound(String),

    #[error("Validation gate not found: {0}")]
    ValidationGateNotFound(String),

    #[error("Approval request not found: {0}")]
    ApprovalRequestNotFound(String),

    #[error("Approver not found: {0}")]
    ApproverNotFound(String),

    #[error("Approval error: {0}")]
    ApprovalError(String),

    #[error("Dashboard error: {0}")]
    DashboardError(String),

    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Database error: {0}")]
    DatabaseError(String),
}

/// Result type for governance operations
pub type Result<T> = std::result::Result<T, GovernanceError>;
