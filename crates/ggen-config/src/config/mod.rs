//! Configuration management for ggen
//!
//! This module provides configuration structures and utilities for various ggen
//! subsystems, including template generation options, marketplace settings, and
//! system-wide defaults.
//!
//! ## Features
//!
//! - **Template Configuration**: Search paths, default variables, cache settings
//! - **Generation Options**: Output directories, formatting, hooks, validation
//! - **Marketplace Settings**: Registry URLs, authentication, update policies
//!
//! ## Examples
//!
//! ### Loading Template Configuration
//!
//! ```rust,no_run
//! use ggen_config::config::TemplateConfig;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_config::config_lib::Result<()> {
//! let config = TemplateConfig {
//!     search_paths: vec![PathBuf::from("templates")],
//!     default_variables: std::collections::HashMap::new(),
//!     metadata_store: PathBuf::from(".ggen/metadata"),
//!     cache_dir: Some(PathBuf::from(".ggen/cache")),
//!     generation: ggen_config::config::GenerationOptions::default(),
//!     marketplace: ggen_config::config::MarketplaceSettings::default(),
//! };
//! # Ok(())
//! # }
//! ```

pub mod lock_manager;
pub mod ontology_config;
pub mod template_config;

#[cfg(test)]
mod ontology_integration_test;

pub use lock_manager::{CompositionMetadata, LockedPackage, LockfileManager, OntologyLockfile};
pub use ontology_config::{
    CompositionStrategy, ConflictResolution, GenerationHooks, LockConfig, OntologyConfig,
    OntologyPackRef, TargetConfig,
};
pub use template_config::{GenerationOptions, MarketplaceSettings, TemplateConfig};
