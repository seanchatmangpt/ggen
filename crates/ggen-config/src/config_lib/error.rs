//! Error types for configuration parsing and validation

#![allow(
    clippy::uninlined_format_args,
    clippy::doc_markdown,
    clippy::too_many_lines,
    clippy::match_same_arms
)]

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
        reason: String,
    },

    /// Environment variable expansion error
    #[error("Environment variable error: {0}")]
    EnvVar(String),

    /// Workspace configuration error
    #[error("Workspace configuration error: {0}")]
    Workspace(String),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

/// Format a single star_toml ValidationError to the legacy string format.
#[must_use]
pub fn format_single_star_toml_error(err: &star_toml::ValidationError) -> String {
    let loc_str = err.loc.to_string();
    let code = err.code();
    let input_val = err.input.as_deref().unwrap_or("");

    match (loc_str.as_str(), code) {
        ("project.name", "empty") => "Project name cannot be empty".to_string(),
        ("project.version", "invalid_semver") => format!(
            "Invalid version format: '{}'. Expected semver format (e.g., 1.0.0)",
            input_val
        ),
        ("ai.provider", "not_one_of") => format!(
            "Unknown AI provider: '{}'. Valid providers: [\"openai\", \"ollama\", \"anthropic\", \"cohere\", \"huggingface\"]",
            input_val
        ),
        ("ai.temperature", "out_of_range") => format!(
            "AI temperature must be between 0.0 and 1.0, got {}",
            input_val
        ),
        ("ai.max_tokens", "out_of_range") => "AI max_tokens must be greater than 0".to_string(),
        ("ai.timeout", "out_of_range") => "AI timeout must be greater than 0".to_string(),
        ("ai.validation.quality_threshold", "out_of_range") => format!(
            "AI validation quality_threshold must be between 0.0 and 1.0, got {}",
            input_val
        ),
        ("templates.directory", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("templates.output_directory", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("performance.max_workers", "out_of_range") => {
            "Performance max_workers must be greater than 0 when parallel_execution is enabled"
                .to_string()
        }
        ("performance.cache_size", "invalid_size_format") => format!(
            "Invalid cache_size format: '{}'. Expected format like '1GB', '512MB'",
            input_val
        ),
        ("logging.level", "not_one_of") => format!(
            "Invalid log level: '{}'. Valid levels: [\"trace\", \"debug\", \"info\", \"warn\", \"error\"]",
            input_val
        ),
        ("logging.format", "not_one_of") => format!(
            "Invalid log format: '{}'. Valid formats: [\"json\", \"text\", \"pretty\"]",
            input_val
        ),
        ("logging.file", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("mcp.transport.transport_type", "not_one_of") => format!(
            "Invalid MCP transport type: '{}'. Valid types: [\"stdio\", \"http\", \"websocket\"]",
            input_val
        ),
        ("mcp.transport.port", "out_of_range") => format!(
            "Invalid MCP port: {}. Must be between 1 and 65535",
            input_val
        ),
        ("mcp.transport.tls.cert_path", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("mcp.transport.tls.key_path", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("mcp.transport.tls.ca_path", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("mcp.tools.discovery_path", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("mcp.tool_timeout_ms", "out_of_range") => {
            "MCP tool_timeout_ms must be greater than 0".to_string()
        }
        ("mcp.max_concurrent_requests", "out_of_range") => {
            "MCP max_concurrent_requests must be greater than 0".to_string()
        }
        ("a2a.transport.transport_type", "not_one_of") => format!(
            "Invalid A2A transport type: '{}'. Valid types: [\"memory\", \"http\", \"websocket\", \"amqp\"]",
            input_val
        ),
        ("a2a.transport.port", "out_of_range") => format!(
            "Invalid A2A port: {}. Must be between 1 and 65535",
            input_val
        ),
        ("a2a.messaging.persistence_path", "invalid_path") => {
            if input_val.contains('\0') {
                format!(
                    "Invalid path '{}': path must not contain null bytes",
                    input_val
                )
            } else if input_val.contains("..") {
                format!(
                    "Invalid path '{}': path traversal ('..') is not allowed",
                    input_val
                )
            } else {
                format!("Invalid path '{}': path must not be empty", input_val)
            }
        }
        ("a2a.orchestration.mode", "not_one_of") => format!(
            "Invalid A2A orchestration mode: '{}'. Valid modes: [\"centralized\", \"decentralized\", \"hierarchical\"]",
            input_val
        ),
        ("a2a.orchestration.consensus_algorithm", "not_one_of") => format!(
            "Invalid A2A consensus algorithm: '{}'. Valid: [\"raft\", \"pbft\", \"naive\"]",
            input_val
        ),
        ("a2a.orchestration.consensus_algorithm", "missing") => {
            "A2A consensus algorithm must be specified when consensus is enabled".to_string()
        }
        _ => {
            if let Some(input) = &err.input {
                format!("{}: {} (got: `{}`)", loc_str, err.msg, input)
            } else {
                format!("{}: {}", loc_str, err.msg)
            }
        }
    }
}

/// Format star_toml ValidationErrors to the legacy string format.
#[must_use]
pub fn format_star_toml_errors(errs: &star_toml::ValidationErrors) -> String {
    let mut formatted = Vec::new();
    for err in errs.errors() {
        formatted.push(format_single_star_toml_error(err));
    }
    formatted.join("; ")
}

impl From<star_toml::Error> for ConfigError {
    fn from(err: star_toml::Error) -> Self {
        match err {
            star_toml::Error::FileNotFound(path) => ConfigError::FileNotFound(path),
            star_toml::Error::Io { source, .. } => ConfigError::Io(source),
            star_toml::Error::Parse { source, .. } => ConfigError::TomlParse(source),
            star_toml::Error::Serialize(source) => ConfigError::TomlSerialize(source),
            star_toml::Error::Validation { reason, .. } => ConfigError::Validation(reason),
            star_toml::Error::Invalid(errs) => {
                ConfigError::Validation(format_star_toml_errors(&errs))
            }
        }
    }
}
