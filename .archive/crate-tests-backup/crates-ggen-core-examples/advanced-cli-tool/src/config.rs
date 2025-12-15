//! Configuration management for the CLI tool

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Buffer size for file I/O operations
    #[serde(default = "default_buffer_size")]
    pub buffer_size: usize,

    /// Maximum concurrent workers
    #[serde(default = "default_max_workers")]
    pub max_workers: usize,

    /// Enable compression by default
    #[serde(default)]
    pub compress: bool,

    /// Logging configuration
    #[serde(default)]
    pub logging: LoggingConfig,

    /// Performance tuning
    #[serde(default)]
    pub performance: PerformanceConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoggingConfig {
    #[serde(default = "default_log_level")]
    pub level: String,

    #[serde(default)]
    pub json_format: bool,

    pub log_file: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    #[serde(default = "default_chunk_size")]
    pub chunk_size: usize,

    #[serde(default = "default_timeout_secs")]
    pub timeout_secs: u64,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            buffer_size: default_buffer_size(),
            max_workers: default_max_workers(),
            compress: false,
            logging: LoggingConfig::default(),
            performance: PerformanceConfig::default(),
        }
    }
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            level: default_log_level(),
            json_format: false,
            log_file: None,
        }
    }
}

impl Default for PerformanceConfig {
    fn default() -> Self {
        Self {
            chunk_size: default_chunk_size(),
            timeout_secs: default_timeout_secs(),
        }
    }
}

impl Config {
    /// Load configuration from a TOML file
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();

        // Return default if file doesn't exist
        if !path.exists() {
            return Ok(Self::default());
        }

        let contents = fs::read_to_string(path)
            .with_context(|| format!("Failed to read config file: {}", path.display()))?;

        toml::from_str(&contents)
            .with_context(|| format!("Failed to parse config file: {}", path.display()))
    }

    /// Save configuration to a TOML file
    pub fn save<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let contents = toml::to_string_pretty(self)
            .context("Failed to serialize configuration")?;

        fs::write(path, contents)
            .with_context(|| format!("Failed to write config file: {}", path.display()))?;

        Ok(())
    }

    /// Validate configuration values
    pub fn validate(&self) -> Result<()> {
        anyhow::ensure!(
            self.buffer_size > 0,
            "buffer_size must be greater than 0"
        );
        anyhow::ensure!(
            self.max_workers > 0,
            "max_workers must be greater than 0"
        );
        anyhow::ensure!(
            self.performance.chunk_size > 0,
            "chunk_size must be greater than 0"
        );
        Ok(())
    }
}

fn default_buffer_size() -> usize {
    8192
}

fn default_max_workers() -> usize {
    num_cpus::get()
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_chunk_size() -> usize {
    1024 * 1024 // 1MB
}

fn default_timeout_secs() -> u64 {
    300 // 5 minutes
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert!(config.buffer_size > 0);
        assert!(config.max_workers > 0);
        assert!(!config.compress);
    }

    #[test]
    fn test_config_validation() {
        let config = Config::default();
        assert!(config.validate().is_ok());

        let mut invalid_config = Config::default();
        invalid_config.buffer_size = 0;
        assert!(invalid_config.validate().is_err());
    }

    #[test]
    fn test_config_save_load() {
        let temp_file = NamedTempFile::new().unwrap();
        let config = Config::default();

        config.save(temp_file.path()).unwrap();
        let loaded = Config::load(temp_file.path()).unwrap();

        assert_eq!(config.buffer_size, loaded.buffer_size);
        assert_eq!(config.max_workers, loaded.max_workers);
    }
}
