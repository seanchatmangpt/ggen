use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum AgentError {
    #[error("Agent failed: {0}")]
    ExecutionFailed(String),

    #[error("Circuit breaker open: {0}")]
    CircuitBreakerOpen(String),

    #[error("Timeout: {0}")]
    Timeout(String),

    #[error("Consensus failed: {0}")]
    ConsensusFailed(String),

    #[error("Tool execution failed: {0}")]
    ToolExecutionFailed(String),

    #[error("Invalid state transition: {0}")]
    InvalidStateTransition(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),
}

pub type Result<T> = std::result::Result<T, AgentError>;
