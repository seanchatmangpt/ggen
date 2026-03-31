//! Error types for the A2A registry.
//!
//! All errors use `thiserror` for ergonomic error handling.
//! Zero `unwrap`/`expect` in production code -- all fallible
//! operations return `Result<T, RegistryError>`.

use std::time::Duration;

/// Registry-specific errors.
#[derive(Debug, thiserror::Error)]
pub enum RegistryError {
    /// The requested agent was not found in the registry.
    #[error("agent not found: {0}")]
    AgentNotFound(String),

    /// An agent with the given ID is already registered.
    #[error("agent already registered: {0}")]
    AlreadyRegistered(String),

    /// A health check for the given agent failed.
    #[error("health check failed for {agent}: {reason}")]
    HealthCheckFailed {
        /// The agent identifier.
        agent: String,
        /// Human-readable reason for the failure.
        reason: String,
    },

    /// An underlying store operation failed.
    #[error("store error: {0}")]
    StoreError(String),

    /// The registry is shut down and cannot accept operations.
    #[error("registry is shut down")]
    Shutdown,

    /// A query produced no results.
    #[error("no agents matched query: {0}")]
    NoMatch(String),

    /// An HTTP request to an agent endpoint failed.
    #[error("http error for {url}: {reason}")]
    HttpError {
        /// The URL that was requested.
        url: String,
        /// Human-readable reason for the failure.
        reason: String,
    },

    /// A timeout was exceeded.
    #[error("timeout after {duration:?} for {operation}")]
    Timeout {
        /// The operation that timed out.
        operation: String,
        /// The duration that was exceeded.
        duration: Duration,
    },
}

/// Convenience `Result` alias for registry operations.
pub type RegistryResult<T> = Result<T, RegistryError>;
