//! Configuration module for ggen.toml
//!
//! This module provides configuration loading and validation for the main
//! ggen.toml configuration file with environment override support.

pub mod loader;
pub mod validation;

// Re-export commonly used types
pub use loader::{
    A2aAgentDef, A2aConfig, A2aServerDef, ConfigLoader, CrateDependency, DependenciesConfig,
    GenerationConfig, GenerationRule, GgenConfig, McpConfig, McpServerDef, ProjectMetadata,
    ZaiConfig,
};
pub use validation::{ConfigValidator, ValidationError, ValidationResult, ValidationWarning};
