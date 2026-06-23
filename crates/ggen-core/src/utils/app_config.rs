//! Application configuration management
//!
//! This module provides centralized configuration management for ggen applications.
//! It supports multiple configuration sources with a precedence order:
//! 1. Default configuration (embedded in binary)
//! 2. Configuration file (if specified)
//! 3. Environment variables (APP_* prefix)
//! 4. CLI arguments (highest precedence)
//!
//! ## Features
//!
//! - **Multi-source configuration**: Merge configs from files, env vars, and CLI args
//! - **Thread-safe access**: RwLock-based configuration builder
//! - **Type-safe access**: Deserialize configuration into typed structures
//! - **Runtime updates**: Update configuration at runtime
//!
//! ## Configuration Precedence
//!
//! When multiple sources provide the same configuration key, the following precedence
//! applies (highest to lowest):
//!
//! 1. CLI arguments
//! 2. Environment variables (APP_*)
//! 3. Configuration file
//! 4. Default configuration
//!
//! ## Examples
//!
//! ### Initializing Configuration
//!
//! ```rust,no_run
//! use ggen_core::utils::app_config::AppConfig;
//!
//! # fn main() -> ggen_core::utils::error::Result<()> {
//! // Initialize with default config
//! AppConfig::init(Some("debug = false\n"))?;
//!
//! // Load configuration
//! let config = AppConfig::fetch()?;
//! println!("Debug mode: {}", config.debug);
//! # Ok(())
//! # }
//! ```
//!
//! ### Accessing Configuration Values
//!
//! ```rust,no_run
//! use ggen_core::utils::app_config::AppConfig;
//!
//! # fn main() -> ggen_core::utils::error::Result<()> {
//! // Get a single value
//! let debug: bool = AppConfig::get("debug")?;
//!
//! // Get the full configuration
//! let config = AppConfig::fetch()?;
//! # Ok(())
//! # }
//! ```

#![allow(clippy::expect_used)] // RwLock poisoning is considered unrecoverable
use config::builder::DefaultState;
use config::{Config, ConfigBuilder, Environment};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::LazyLock;
use std::sync::RwLock;

use super::error::Result;
use crate::utils::types::LogLevel;

// CONFIG static variable. It's actually an AppConfig
// inside an RwLock.
static BUILDER: LazyLock<RwLock<ConfigBuilder<DefaultState>>> =
    LazyLock::new(|| RwLock::new(Config::builder()));

#[derive(Debug, Serialize, Deserialize)]
pub struct Database {
    pub url: String,
    pub variable: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AppConfig {
    pub debug: bool,
    pub log_level: LogLevel,
    pub database: Database,
}

impl AppConfig {
    /// Initialize `AppConfig`.
    ///
    /// # Errors
    ///
    /// Returns an error if configuration parsing fails.
    pub fn init(default_config: Option<&str>) -> Result<()> {
        let mut builder = Config::builder();

        // Embed file into executable
        // This macro will embed the configuration file into the
        // executable. Check include_str! for more info.
        if let Some(config_contents) = default_config {
            //let contents = include_str!(config_file_path);
            builder = builder.add_source(config::File::from_str(
                config_contents,
                config::FileFormat::Toml,
            ));
        }

        // Merge settings with env variables
        builder = builder.add_source(Environment::with_prefix("APP"));

        // Save Config to RwLoc
        {
            let mut w = BUILDER.write()?;
            *w = builder;
        }

        Ok(())
    }

    // CLI-argument merging (formerly `merge_args` / `init_with_args`, which took a
    // `clap::ArgMatches`) now lives in the ggen-cli command layer so ggen-core
    // stays CLI-agnostic and clap-free. Use `init` / `merge_config` for
    // programmatic configuration.

    /// Merge configuration from a file.
    ///
    /// # Errors
    ///
    /// Returns an error if the configuration file cannot be read or parsed.
    ///
    /// # Panics
    ///
    /// Panics if the `RwLock` is poisoned.
    pub fn merge_config(config_file: Option<&Path>) -> Result<()> {
        // Merge settings with config file if there is one
        if let Some(config_file_path) = config_file {
            {
                let mut w = BUILDER.write().expect("RwLock should not be poisoned");
                *w = w.clone().add_source(config::File::with_name(
                    config_file_path.to_str().unwrap_or(""),
                ));
            }
        }
        Ok(())
    }

    /// Set a configuration value.
    ///
    /// # Errors
    ///
    /// Returns an error if the configuration value cannot be set.
    ///
    /// # Panics
    ///
    /// Panics if the `RwLock` is poisoned.
    pub fn set(key: &str, value: &str) -> Result<()> {
        {
            let mut w = BUILDER.write().expect("RwLock should not be poisoned");
            *w = w.clone().set_override(key, value)?;
        }

        Ok(())
    }

    /// Get a single configuration value.
    ///
    /// # Errors
    ///
    /// Returns an error if the configuration value cannot be retrieved or deserialized.
    pub fn get<'de, T>(key: &'de str) -> Result<T>
    where
        T: serde::Deserialize<'de>,
    {
        Ok(BUILDER.read()?.clone().build()?.get::<T>(key)?)
    }

    /// Get the current configuration.
    ///
    /// This clones Config (from `RwLock<Config>`) into a new `AppConfig` object.
    /// This means you have to fetch this again if you changed the configuration.
    ///
    /// # Errors
    ///
    /// Returns an error if the configuration cannot be retrieved or deserialized.
    pub fn fetch() -> Result<Self> {
        // Clone the Config object and build it
        let config_clone = BUILDER.read()?.clone().build()?;

        // Coerce Config into AppConfig
        let app_config: Self = config_clone.try_into()?;
        Ok(app_config)
    }
}

// Coerce Config into AppConfig

impl TryFrom<Config> for AppConfig {
    type Error = crate::utils::error::Error;

    fn try_from(config: Config) -> Result<Self> {
        Ok(Self {
            debug: config.get_bool("debug")?,
            log_level: config.get::<LogLevel>("log_level")?,
            database: config.get::<Database>("database")?,
        })
    }
}
