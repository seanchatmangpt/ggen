// Configuration struct with builder pattern
// This demonstrates common patterns for configuration management

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Application configuration with validation and defaults
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    /// Server host address
    pub host: String,

    /// Server port number
    pub port: u16,

    /// Database connection URL
    pub database_url: String,

    /// Log level (debug, info, warn, error)
    pub log_level: String,

    /// Maximum number of concurrent connections
    pub max_connections: usize,

    /// Request timeout in seconds
    pub timeout_secs: u64,

    /// Enable TLS/SSL
    pub use_tls: bool,

    /// Path to TLS certificate (required if use_tls is true)
    pub tls_cert_path: Option<PathBuf>,

    /// Path to TLS key (required if use_tls is true)
    pub tls_key_path: Option<PathBuf>,
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 8080,
            database_url: "sqlite::memory:".to_string(),
            log_level: "info".to_string(),
            max_connections: 100,
            timeout_secs: 30,
            use_tls: false,
            tls_cert_path: None,
            tls_key_path: None,
        }
    }
}

impl AppConfig {
    /// Create a new configuration builder
    pub fn builder() -> AppConfigBuilder {
        AppConfigBuilder::default()
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<(), String> {
        // Validate port range
        if self.port == 0 {
            return Err("Port cannot be 0".to_string());
        }

        // Validate log level
        let valid_levels = ["debug", "info", "warn", "error"];
        if !valid_levels.contains(&self.log_level.as_str()) {
            return Err(format!("Invalid log level: {}", self.log_level));
        }

        // Validate max connections
        if self.max_connections == 0 {
            return Err("max_connections must be greater than 0".to_string());
        }

        // Validate TLS configuration
        if self.use_tls {
            if self.tls_cert_path.is_none() {
                return Err("tls_cert_path required when use_tls is true".to_string());
            }
            if self.tls_key_path.is_none() {
                return Err("tls_key_path required when use_tls is true".to_string());
            }
        }

        Ok(())
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self, String> {
        let mut config = Self::default();

        if let Ok(host) = std::env::var("APP_HOST") {
            config.host = host;
        }

        if let Ok(port) = std::env::var("APP_PORT") {
            config.port = port.parse().map_err(|e| format!("Invalid port: {}", e))?;
        }

        if let Ok(db_url) = std::env::var("DATABASE_URL") {
            config.database_url = db_url;
        }

        if let Ok(log_level) = std::env::var("LOG_LEVEL") {
            config.log_level = log_level;
        }

        config.validate()?;
        Ok(config)
    }
}

/// Builder for AppConfig with fluent API
#[derive(Default)]
pub struct AppConfigBuilder {
    host: Option<String>,
    port: Option<u16>,
    database_url: Option<String>,
    log_level: Option<String>,
    max_connections: Option<usize>,
    timeout_secs: Option<u64>,
    use_tls: bool,
    tls_cert_path: Option<PathBuf>,
    tls_key_path: Option<PathBuf>,
}

impl AppConfigBuilder {
    pub fn host(mut self, host: impl Into<String>) -> Self {
        self.host = Some(host.into());
        self
    }

    pub fn port(mut self, port: u16) -> Self {
        self.port = Some(port);
        self
    }

    pub fn database_url(mut self, url: impl Into<String>) -> Self {
        self.database_url = Some(url.into());
        self
    }

    pub fn log_level(mut self, level: impl Into<String>) -> Self {
        self.log_level = Some(level.into());
        self
    }

    pub fn max_connections(mut self, max: usize) -> Self {
        self.max_connections = Some(max);
        self
    }

    pub fn timeout_secs(mut self, timeout: u64) -> Self {
        self.timeout_secs = Some(timeout);
        self
    }

    pub fn use_tls(mut self, use_tls: bool) -> Self {
        self.use_tls = use_tls;
        self
    }

    pub fn tls_cert_path(mut self, path: PathBuf) -> Self {
        self.tls_cert_path = Some(path);
        self
    }

    pub fn tls_key_path(mut self, path: PathBuf) -> Self {
        self.tls_key_path = Some(path);
        self
    }

    pub fn build(self) -> Result<AppConfig, String> {
        let default = AppConfig::default();

        let config = AppConfig {
            host: self.host.unwrap_or(default.host),
            port: self.port.unwrap_or(default.port),
            database_url: self.database_url.unwrap_or(default.database_url),
            log_level: self.log_level.unwrap_or(default.log_level),
            max_connections: self.max_connections.unwrap_or(default.max_connections),
            timeout_secs: self.timeout_secs.unwrap_or(default.timeout_secs),
            use_tls: self.use_tls,
            tls_cert_path: self.tls_cert_path,
            tls_key_path: self.tls_key_path,
        };

        config.validate()?;
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = AppConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_builder() {
        let config = AppConfig::builder()
            .host("0.0.0.0")
            .port(3000)
            .log_level("debug")
            .build()
            .unwrap();

        assert_eq!(config.host, "0.0.0.0");
        assert_eq!(config.port, 3000);
    }

    #[test]
    fn test_validation() {
        let mut config = AppConfig::default();
        config.port = 0;
        assert!(config.validate().is_err());
    }
}
