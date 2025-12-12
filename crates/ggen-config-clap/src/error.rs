//! Error types for config-clap integration

#[derive(Debug, thiserror::Error)]
pub enum ConfigClapError {
    #[error("Failed to load ggen.toml: {0}")]
    LoadError(String),

    #[error("Failed to parse configuration: {0}")]
    ParseError(String),

    #[error("Type conversion error: {field} - {reason}")]
    TypeConversion { field: String, reason: String },

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Configuration error: {0}")]
    Config(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, ConfigClapError>;
