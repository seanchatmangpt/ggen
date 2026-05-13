//! # ggen-config - Configuration Parser and Validator
//!
//! Type-safe configuration parsing and validation for ggen.toml files.
//!
//! ## Features
//!
//! - Type-safe TOML parsing with serde
//! - Comprehensive configuration schema
//! - Environment variable expansion
//! - Workspace support
//! - Rich error handling

pub mod config;
pub mod config_lib;

pub use config_lib::{
    A2AConfig, A2AMessagingConfig, A2AOrchestrationConfig, A2ARetryConfig, A2ATransportConfig,
    AiConfig, ConfigError, ConfigLoader, ConfigValidator, GgenConfig, McpConfig, McpDiscoveryConfig,
    McpTlsConfig, McpToolsConfig, McpTransportConfig, McpZaiConfig, ProjectConfig, Result,
    TemplatesConfig,
};
