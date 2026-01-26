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
//! use ggen_utils::app_config::AppConfig;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Initialize with default config
//! AppConfig::init(Some(include_str!("../resources/default_config.toml")))?;
//!
//! // Load configuration
//! let config = AppConfig::fetch()?;
//! println!("Debug mode: {}", config.debug);
//! # Ok(())
//! # }
//! ```
//!
//! ### Merging CLI Arguments
//!
//! ```rust,no_run
//! use ggen_utils::app_config::AppConfig;
//! use clap::Parser;
//!
//! # #[derive(Parser)]
//! # struct Args {
//! #     #[arg(long)]
//! #     debug: bool,
//! # }
//! # fn main() -> ggen_utils::error::Result<()> {
//! let args = Args::parse();
//! AppConfig::merge_args(&args)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Accessing Configuration Values
//!
//! ```rust,no_run
//! use ggen_utils::app_config::AppConfig;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Get a single value
//! let debug: bool = AppConfig::get("debug")?;
//!
//! // Get the full configuration
//! let config = AppConfig::fetch()?;
//! # Ok(())
//! # }
//! ```

use config::builder::DefaultState;
use config::{Config, ConfigBuilder, Environment};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::LazyLock;
use std::sync::RwLock;

use super::error::Result;
use crate::types::LogLevel;

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

    /// Merge CLI arguments with existing configuration.
    ///
    /// # Errors
    ///
    /// Returns an error if configuration merging fails.
    pub fn merge_args(args: &clap::ArgMatches) -> Result<()> {
        // Merge Clap arguments with existing configuration
        // This allows CLI arguments to override environment variables and config files

        if args.contains_id("debug") {
            let value: &bool = args.get_one("debug").unwrap_or(&false);
            Self::set("debug", &value.to_string())?;
        }

        if args.contains_id("log_level") {
            let value: &LogLevel = args.get_one("log_level").unwrap_or(&LogLevel::Info);
            Self::set("log_level", &value.to_string())?;
        }

        // Add support for more CLI arguments
        if args.contains_id("config") {
            if let Some(config_path) = args.get_one::<String>("config") {
                Self::merge_config(Some(Path::new(config_path)))?;
            }
        }

        Ok(())
    }

    /// Initialize configuration with proper precedence order:
    /// 1. Default configuration (embedded)
    /// 2. Configuration file (if specified)
    /// 3. Environment variables (APP_*)
    /// 4. CLI arguments (highest precedence)
    ///
    /// # Errors
    ///
    /// Returns an error if configuration initialization or merging fails.
    pub fn init_with_args(
        default_config: Option<&str>, args: Option<clap::ArgMatches>,
    ) -> Result<()> {
        // Initialize with defaults and environment variables
        Self::init(default_config)?;

        // Merge CLI arguments if provided
        if let Some(arg_matches) = args {
            Self::merge_args(&arg_matches)?;
        }

        Ok(())
    }

    /// Merge configuration from a file.
    ///
    /// # Errors
    ///
    /// Returns an error if the configuration file cannot be read or parsed,
    /// or if the `RwLock` is poisoned.
    pub fn merge_config(config_file: Option<&Path>) -> Result<()> {
        // Merge settings with config file if there is one
        if let Some(config_file_path) = config_file {
            {
                let mut w = BUILDER.write()?;
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
    /// Returns an error if the configuration value cannot be set,
    /// or if the `RwLock` is poisoned.
    pub fn set(key: &str, value: &str) -> Result<()> {
        {
            let mut w = BUILDER.write()?;
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
    type Error = crate::error::Error;

    fn try_from(config: Config) -> Result<Self> {
        Ok(Self {
            debug: config.get_bool("debug")?,
            log_level: config.get::<LogLevel>("log_level")?,
            database: config.get::<Database>("database")?,
        })
    }
}
