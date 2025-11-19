//! Error types for configuration parsing and validation

use std::path::PathBuf;

/// Result type alias for configuration operations
pub type Result<T> = std::result::Result<T, ConfigError>;

/// Errors that can occur during configuration operations
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    /// Configuration file not found
    #[error("Configuration file not found: {0}")]
    FileNotFound(PathBuf),

    /// I/O error reading configuration
    #[error("I/O error reading configuration: {0}")]
    Io(#[from] std::io::Error),

    /// TOML parsing error
    #[error("TOML parsing error: {0}")]
    TomlParse(#[from] toml::de::Error),

    /// TOML serialization error
    #[error("TOML serialization error: {0}")]
    TomlSerialize(#[from] toml::ser::Error),

    /// Configuration validation error
    #[error("Configuration validation error: {0}")]
    Validation(String),

    /// Missing required field
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Invalid value for field
    #[error("Invalid value for field '{field}': {reason}")]
    InvalidValue {
        /// Field name
        field: String,
        /// Reason for invalidity
        reason: String
    },

    /// Environment variable expansion error
    #[error("Environment variable error: {0}")]
    EnvVar(String),

    /// Workspace configuration error
    #[error("Workspace configuration error: {0}")]
    Workspace(String),
}
