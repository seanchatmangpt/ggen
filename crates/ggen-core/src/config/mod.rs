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
//! use ggen_core::config::TemplateConfig;
//! use std::path::PathBuf;
//!
//! # fn main() -> anyhow::Result<()> {
//! let config = TemplateConfig {
//!     search_paths: vec![PathBuf::from("templates")],
//!     default_variables: std::collections::HashMap::new(),
//!     metadata_store: PathBuf::from(".ggen/metadata"),
//!     cache_dir: Some(PathBuf::from(".ggen/cache")),
//!     generation: ggen_core::config::GenerationOptions::default(),
//!     marketplace: ggen_core::config::MarketplaceSettings::default(),
//! };
//! # Ok(())
//! # }
//! ```

pub mod template_config;

pub use template_config::{GenerationOptions, MarketplaceSettings, TemplateConfig};
