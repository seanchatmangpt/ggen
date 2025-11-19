//! # ggen-config-clap
//!
//! Integration layer for loading ggen.toml configuration into clap applications.
//!
//! Provides traits and utilities for merging TOML configuration with CLI arguments.

pub mod loader;
pub mod error;

pub use loader::LoadConfigFromGgenToml;
pub use error::ConfigClapError;

/// Re-export ggen-config for convenience
pub use ggen_config;
