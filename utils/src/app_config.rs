use config::builder::DefaultState;
use config::{Config, ConfigBuilder, Environment};
use lazy_static::{__Deref, lazy_static};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::RwLock;

use super::error::Result;
use crate::types::LogLevel;

// CONFIG static variable. It's actually an AppConfig
// inside an RwLock.
lazy_static! {
    pub static ref BUILDER: RwLock<ConfigBuilder<DefaultState>> = RwLock::new(Config::builder());
}

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
    /// Initialize AppConfig.
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

    pub fn merge_args(args: clap::ArgMatches) -> Result<()> {
        // Merge Clap arguments with existing configuration
        // This allows CLI arguments to override environment variables and config files

        if args.contains_id("debug") {
            let value: &bool = args.get_one("debug").unwrap_or(&false);
            AppConfig::set("debug", &value.to_string())?;
        }

        if args.contains_id("log_level") {
            let value: &LogLevel = args.get_one("log_level").unwrap_or(&LogLevel::Info);
            AppConfig::set("log_level", &value.to_string())?;
        }

        // Add support for more CLI arguments
        if args.contains_id("config") {
            if let Some(config_path) = args.get_one::<String>("config") {
                AppConfig::merge_config(Some(Path::new(config_path)))?;
            }
        }

        Ok(())
    }

    /// Initialize configuration with proper precedence order:
    /// 1. Default configuration (embedded)
    /// 2. Configuration file (if specified)
    /// 3. Environment variables (APP_*)
    /// 4. CLI arguments (highest precedence)
    pub fn init_with_args(
        default_config: Option<&str>, args: Option<clap::ArgMatches>,
    ) -> Result<()> {
        // Initialize with defaults and environment variables
        AppConfig::init(default_config)?;

        // Merge CLI arguments if provided
        if let Some(arg_matches) = args {
            AppConfig::merge_args(arg_matches)?;
        }

        Ok(())
    }

    pub fn merge_config(config_file: Option<&Path>) -> Result<()> {
        // Merge settings with config file if there is one
        if let Some(config_file_path) = config_file {
            {
                let mut w = BUILDER.write().unwrap();
                *w = w.clone().add_source(config::File::with_name(
                    config_file_path.to_str().unwrap_or(""),
                ));
            }
        }
        Ok(())
    }

    // Set CONFIG
    pub fn set(key: &str, value: &str) -> Result<()> {
        {
            let mut w = BUILDER.write().unwrap();
            *w = w.clone().set_override(key, value)?;
        }

        Ok(())
    }

    // Get a single value
    pub fn get<'de, T>(key: &'de str) -> Result<T>
    where
        T: serde::Deserialize<'de>,
    {
        Ok(BUILDER.read()?.deref().clone().build()?.get::<T>(key)?)
    }

    // Get CONFIG
    // This clones Config (from RwLock<Config>) into a new AppConfig object.
    // This means you have to fetch this again if you changed the configuration.
    pub fn fetch() -> Result<AppConfig> {
        // Get a Read Lock from RwLock
        let r = BUILDER.read()?;

        // Clone the Config object
        let config_clone = r.deref().clone().build()?;

        // Coerce Config into AppConfig
        let app_config: AppConfig = config_clone.try_into()?;
        Ok(app_config)
    }
}

// Coerce Config into AppConfig

impl TryFrom<Config> for AppConfig {
    type Error = crate::error::Error;

    fn try_from(config: Config) -> Result<Self> {
        Ok(AppConfig {
            debug: config.get_bool("debug")?,
            log_level: config.get::<LogLevel>("log_level")?,
            database: config.get::<Database>("database")?,
        })
    }
}
