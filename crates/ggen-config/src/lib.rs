//! # ggen-config
//!
//! Configuration parser and validator for ggen.toml files.
//!
//! This crate provides a type-safe interface for loading, parsing, and validating
//! ggen.toml configuration files that define project settings, AI providers,
//! templates, RDF/SPARQL settings, and more.
//!
//! ## Features
//!
//! - **Type-safe parsing**: Strongly-typed Rust structs with serde
//! - **Schema validation**: Validates configuration against expected schema
//! - **Environment overrides**: Support for environment-specific configs
//! - **Workspace support**: Mono-repo and workspace configuration
//! - **Error handling**: Comprehensive error types with context
//!
//! ## Example
//!
//! ```no_run
//! use ggen_config::{GgenConfig, ConfigLoader};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load configuration from file
//! let config = ConfigLoader::from_file("ggen.toml")?;
//!
//! // Access configuration
//! println!("Project: {}", config.project.name);
//! if let Some(ai) = &config.ai {
//!     println!("AI Provider: {}", ai.provider);
//! }
//! # Ok(())
//! # }
//! ```

#![warn(missing_docs)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::should_implement_trait)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

mod error;
mod parser;
mod schema;
mod validator;

pub use error::{ConfigError, Result};
pub use parser::ConfigLoader;
pub use schema::*;
pub use validator::ConfigValidator;

/// Re-export commonly used types
pub mod prelude {
    pub use super::{
        AiConfig, ConfigError, ConfigLoader, ConfigValidator, GgenConfig, ProjectConfig, Result,
        TemplatesConfig,
    };
}
