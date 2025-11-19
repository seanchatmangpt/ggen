//! # ggen-config-clap
//!
//! Integration layer for loading ggen.toml configuration into clap applications.
//!
//! Provides traits and utilities for merging TOML configuration with CLI arguments.

pub mod error;
pub mod loader;

pub use error::ConfigClapError;
pub use loader::LoadConfigFromGgenToml;

/// Re-export ggen-config for convenience
pub use ggen_config;
