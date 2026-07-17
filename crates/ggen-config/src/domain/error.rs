//! Domain-specific error types for ggen
//!
//! This module defines error types specific to the domain layer,
//! including A2A-RS integration errors and other domain-specific failures.
//!
//! Ported from `ggen-core/src/domain/error.rs` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/12-OPEN-QUESTIONS.md item 2). The `From<X> for Error` impls originally
//! targeted `ggen_core::utils::error::Error` (an ad-hoc string-message type, out of this port's
//! scope — still crate-root `ggen-core`, tracked separately). Remapped here onto
//! `crate::config_lib::ConfigError` — this crate's own richer `thiserror` enum — by call-site
//! meaning, not string-matching, same pattern used for the T019-T030 ports:
//!   * `file_not_found(PathBuf::from(msg))` → `ConfigError::FileNotFound(PathBuf::from(msg))`
//!     (a direct, exact-shape match — the one variant with a real 1:1 counterpart)
//!   * `invalid_input(msg)` → `ConfigError::Validation(msg)` (closest semantic match among the
//!     available variants; `InvalidValue { field, reason }` was rejected because these call
//!     sites never have a field name, only a free-form message)
//!   * `network_error`/`invalid_state`/`internal_error`/`feature_not_enabled`/the three
//!     `new(&format!(...))` call sites (Timeout/Protocol/MessageDelivery) → `ConfigError::
//!     Other(...)`, preserving the exact original wrapper text (e.g. `"Network error: {msg}"`,
//!     `"Invalid state: {msg}"`) so downstream `.to_string().contains(...)` checks against the
//!     original inner message are unaffected by the type change.

use crate::config_lib::ConfigError;
use thiserror::Error;

/// A2A-RS integration errors
#[derive(Error, Debug)]
pub enum A2aError {
    #[error("A2A connection failed: {0}")]
    Connection(String),

    #[error("A2A authentication failed: {0}")]
    Authentication(String),

    #[error("A2A agent not found: {0}")]
    AgentNotFound(String),

    #[error("A2A agent already running: {0}")]
    AgentAlreadyRunning(String),

    #[error("A2A invalid configuration: {0}")]
    InvalidConfiguration(String),

    #[error("A2A timeout: {0}")]
    Timeout(String),

    #[error("A2A protocol error: {0}")]
    Protocol(String),

    #[error("A2A transport error: {0}")]
    Transport(String),

    #[error("A2A message delivery failed: {0}")]
    MessageDelivery(String),

    #[error("A2A resource not found: {0}")]
    ResourceNotFound(String),

    #[error("A2A permission denied: {0}")]
    PermissionDenied(String),

    #[error("A2A internal error: {0}")]
    Internal(String),
}

/// Convert A2A errors to `ConfigError`
impl From<A2aError> for ConfigError {
    fn from(err: A2aError) -> Self {
        match err {
            A2aError::Connection(msg) => {
                ConfigError::Other(format!("Network error: {msg}"))
            }
            A2aError::Authentication(msg) => ConfigError::Validation(msg),
            A2aError::AgentNotFound(msg) => {
                ConfigError::FileNotFound(std::path::PathBuf::from(msg))
            }
            A2aError::AgentAlreadyRunning(msg) => {
                ConfigError::Other(format!("Invalid state: {msg}"))
            }
            A2aError::InvalidConfiguration(msg) => ConfigError::Validation(msg),
            A2aError::Timeout(msg) => ConfigError::Other(format!("Timeout: {msg}")),
            A2aError::Protocol(msg) => ConfigError::Other(format!("Protocol error: {msg}")),
            A2aError::Transport(msg) => ConfigError::Other(format!("Network error: {msg}")),
            A2aError::MessageDelivery(msg) => {
                ConfigError::Other(format!("Message delivery failed: {msg}"))
            }
            A2aError::ResourceNotFound(msg) => {
                ConfigError::FileNotFound(std::path::PathBuf::from(msg))
            }
            A2aError::PermissionDenied(msg) => ConfigError::Validation(msg),
            A2aError::Internal(msg) => ConfigError::Other(format!("Internal error: {msg}")),
        }
    }
}

/// MCP integration errors
#[derive(Error, Debug)]
pub enum McpError {
    #[error("MCP server connection failed: {0}")]
    ServerConnection(String),

    #[error("MCP server not found: {0}")]
    ServerNotFound(String),

    #[error("MCP tool not found: {0}")]
    ToolNotFound(String),

    #[error("MCP request failed: {0}")]
    RequestFailed(String),

    #[error("MCP response parsing failed: {0}")]
    ResponseParse(String),

    #[error("MCP authentication failed: {0}")]
    Authentication(String),

    #[error("MCP configuration error: {0}")]
    Configuration(String),

    #[error("MCP timeout: {0}")]
    Timeout(String),

    #[error("MCP internal error: {0}")]
    Internal(String),
}

/// Convert MCP errors to `ConfigError`
impl From<McpError> for ConfigError {
    fn from(err: McpError) -> Self {
        match err {
            McpError::ServerConnection(msg) => {
                ConfigError::Other(format!("Network error: {msg}"))
            }
            McpError::ServerNotFound(msg) => {
                ConfigError::FileNotFound(std::path::PathBuf::from(msg))
            }
            McpError::ToolNotFound(msg) => {
                ConfigError::FileNotFound(std::path::PathBuf::from(msg))
            }
            McpError::RequestFailed(msg) => ConfigError::Other(format!("Request failed: {msg}")),
            McpError::ResponseParse(msg) => {
                ConfigError::Other(format!("Response parsing failed: {msg}"))
            }
            McpError::Authentication(msg) => ConfigError::Validation(msg),
            McpError::Configuration(msg) => ConfigError::Validation(msg),
            McpError::Timeout(msg) => ConfigError::Other(format!("Timeout: {msg}")),
            McpError::Internal(msg) => ConfigError::Other(format!("Internal error: {msg}")),
        }
    }
}

/// Agent lifecycle errors
#[derive(Error, Debug)]
pub enum AgentError {
    #[error("Agent startup failed: {0}")]
    StartupFailed(String),

    #[error("Agent shutdown failed: {0}")]
    ShutdownFailed(String),

    #[error("Agent communication failed: {0}")]
    CommunicationFailed(String),

    #[error("Agent state transition failed: {0}")]
    StateTransitionFailed(String),

    #[error("Agent configuration invalid: {0}")]
    InvalidConfiguration(String),

    #[error("Agent capability not supported: {0}")]
    CapabilityNotSupported(String),

    #[error("Agent timeout: {0}")]
    Timeout(String),

    #[error("Agent internal error: {0}")]
    Internal(String),
}

/// Convert agent errors to `ConfigError`
impl From<AgentError> for ConfigError {
    fn from(err: AgentError) -> Self {
        match err {
            AgentError::StartupFailed(msg) => {
                ConfigError::Other(format!("Invalid state: {msg}"))
            }
            AgentError::ShutdownFailed(msg) => {
                ConfigError::Other(format!("Invalid state: {msg}"))
            }
            AgentError::CommunicationFailed(msg) => {
                ConfigError::Other(format!("Network error: {msg}"))
            }
            AgentError::StateTransitionFailed(msg) => {
                ConfigError::Other(format!("Invalid state: {msg}"))
            }
            AgentError::InvalidConfiguration(msg) => ConfigError::Validation(msg),
            AgentError::CapabilityNotSupported(msg) => {
                ConfigError::Other(format!("Feature '{msg}' not enabled. "))
            }
            AgentError::Timeout(msg) => ConfigError::Other(format!("Timeout: {msg}")),
            AgentError::Internal(msg) => ConfigError::Other(format!("Internal error: {msg}")),
        }
    }
}

/// Helper function for creating A2A connection errors
pub fn a2a_connection_error<S: Into<String>>(message: S) -> A2aError {
    A2aError::Connection(message.into())
}

/// Helper function for creating A2A authentication errors
pub fn a2a_auth_error<S: Into<String>>(message: S) -> A2aError {
    A2aError::Authentication(message.into())
}

/// Helper function for creating MCP server errors
pub fn mcp_server_error<S: Into<String>>(message: S) -> McpError {
    McpError::ServerConnection(message.into())
}

/// Helper function for creating agent startup errors
pub fn agent_startup_error<S: Into<String>>(message: S) -> AgentError {
    AgentError::StartupFailed(message.into())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_a2a_error_conversion() {
        let a2a_error = A2aError::Connection("Connection failed".to_string());
        let domain_error: ConfigError = a2a_error.into();
        assert!(domain_error.to_string().contains("Connection failed"));
    }

    #[test]
    fn test_mcp_error_conversion() {
        let mcp_error = McpError::ToolNotFound("tool not found".to_string());
        let domain_error: ConfigError = mcp_error.into();
        assert!(domain_error.to_string().contains("tool not found"));
    }

    #[test]
    fn test_agent_error_conversion() {
        let agent_error = AgentError::StartupFailed("startup failed".to_string());
        let domain_error: ConfigError = agent_error.into();
        assert!(domain_error.to_string().contains("startup failed"));
    }

    #[test]
    fn test_a2a_helper_functions() {
        let conn_error = a2a_connection_error("test connection error");
        assert!(matches!(conn_error, A2aError::Connection(_)));

        let auth_error = a2a_auth_error("test auth error");
        assert!(matches!(auth_error, A2aError::Authentication(_)));
    }

    #[test]
    fn test_mcp_helper_functions() {
        let server_error = mcp_server_error("test server error");
        assert!(matches!(server_error, McpError::ServerConnection(_)));
    }

    #[test]
    fn test_agent_helper_functions() {
        let startup_error = agent_startup_error("test startup error");
        assert!(matches!(startup_error, AgentError::StartupFailed(_)));
    }
}
