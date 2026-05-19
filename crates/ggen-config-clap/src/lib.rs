//! # mcpp-config-clap
//!
//! Integration layer for loading mcpp.toml configuration into clap applications.
//!
//! Provides traits and utilities for merging TOML configuration with CLI arguments.

pub mod error;
pub mod loader;

pub use error::ConfigClapError;
pub use loader::LoadConfigFromGgenToml;

/// Re-export mcpp-config for convenience
pub use mcpp_config;
