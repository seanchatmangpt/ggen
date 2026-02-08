//! Environment configuration for ggen-domain
//!
//! This module provides environment configuration and context management
//! for the domain layer.

use std::collections::HashMap;
use std::env;
use std::path::PathBuf;

/// Environment configuration for domain operations
#[derive(Debug, Clone)]
pub struct EnvironmentConfig {
    /// Environment name (development, staging, production)
    pub environment: String,
    /// Base data directory
    pub data_dir: PathBuf,
    /// Configuration directory
    pub config_dir: PathBuf,
    /// Cache directory
    pub cache_dir: PathBuf,
    /// Log directory
    pub log_dir: PathBuf,
    /// API keys and secrets
    pub secrets: HashMap<String, String>,
    /// Feature flags
    pub features: HashMap<String, bool>,
    /// Performance tuning settings
    pub performance: PerformanceConfig,
}

/// Performance configuration for domain operations
#[derive(Debug, Clone)]
pub struct PerformanceConfig {
    /// Maximum number of concurrent operations
    pub max_concurrency: usize,
    /// Cache size limits
    pub cache_size: usize,
    /// Timeout settings (in milliseconds)
    pub timeouts: TimeoutConfig,
    /// Batch processing settings
    pub batching: BatchConfig,
}

/// Timeout configuration
#[derive(Debug, Clone)]
pub struct TimeoutConfig {
    /// Operation timeout (default: 30s)
    pub operation_ms: u64,
    /// I/O timeout (default: 10s)
    pub io_ms: u64,
    /// Network timeout (default: 5s)
    pub network_ms: u64,
    /// Database timeout (default: 15s)
    pub database_ms: u64,
}

/// Batch processing configuration
#[derive(Debug, Clone)]
pub struct BatchConfig {
    /// Maximum batch size
    pub max_size: usize,
    /// Maximum batch wait time (milliseconds)
    pub max_wait_ms: u64,
    /// Enable batching
    pub enabled: bool,
}

impl Default for EnvironmentConfig {
    fn default() -> Self {
        Self::new("development").unwrap_or_else(|_| Self::development())
    }
}

impl EnvironmentConfig {
    /// Create a new environment configuration
    pub fn new(environment: &str) -> Result<Self, String> {
        // Get base directories
        let home_dir = dirs::home_dir()
            .ok_or_else(|| "Could not determine home directory")?;

        let data_dir = home_dir.join(".ggen").join("data");
        let config_dir = home_dir.join(".ggen").join("config");
        let cache_dir = home_dir.join(".ggen").join("cache");
        let log_dir = home_dir.join(".ggen").join("logs");

        // Create directories if they don't exist
        for dir in [&data_dir, &config_dir, &cache_dir, &log_dir] {
            std::fs::create_dir_all(dir)
                .map_err(|e| format!("Failed to create directory {:?}: {}", dir, e))?;
        }

        // Load secrets from environment variables
        let mut secrets = HashMap::new();

        // Common environment variable names for secrets
        let secret_names = [
            "API_KEY",
            "DATABASE_URL",
            "SECRET_KEY",
            "JWT_SECRET",
            "OPENAI_API_KEY",
            "ANTHROPIC_API_KEY",
            "GITHUB_TOKEN",
        ];

        for secret_name in &secret_names {
            if let Ok(value) = env::var(secret_name) {
                secrets.insert(secret_name.to_lowercase(), value);
            }
        }

        // Load feature flags
        let mut features = HashMap::new();
        features.insert("experimental".to_string(), false);
        features.insert("debug_logging".to_string(), environment == "development");
        features.insert("cache_enabled".to_string(), true);

        Ok(Self {
            environment: environment.to_string(),
            data_dir,
            config_dir,
            cache_dir,
            log_dir,
            secrets,
            features,
            performance: PerformanceConfig::default(),
        })
    }

    /// Development environment configuration
    pub fn development() -> Self {
        Self {
            environment: "development".to_string(),
            data_dir: PathBuf::from("./dev-data"),
            config_dir: PathBuf::from("./dev-config"),
            cache_dir: PathBuf::from("./dev-cache"),
            log_dir: PathBuf::from("./dev-logs"),
            secrets: HashMap::new(),
            features: {
                let mut features = HashMap::new();
                features.insert("experimental".to_string(), true);
                features.insert("debug_logging".to_string(), true);
                features.insert("cache_enabled".to_string(), true);
                features
            },
            performance: PerformanceConfig {
                max_concurrency: 4,
                cache_size: 1000,
                timeouts: TimeoutConfig {
                    operation_ms: 30000,
                    io_ms: 10000,
                    network_ms: 5000,
                    database_ms: 15000,
                },
                batching: BatchConfig {
                    max_size: 100,
                    max_wait_ms: 100,
                    enabled: true,
                },
            },
        }
    }

    /// Production environment configuration
    pub fn production() -> Self {
        Self {
            environment: "production".to_string(),
            data_dir: PathBuf::from("/var/lib/ggen"),
            config_dir: PathBuf::from("/etc/ggen"),
            cache_dir: PathBuf::from("/var/cache/ggen"),
            log_dir: PathBuf::from("/var/log/ggen"),
            secrets: HashMap::new(),
            features: {
                let mut features = HashMap::new();
                features.insert("experimental".to_string(), false);
                features.insert("debug_logging".to_string(), false);
                features.insert("cache_enabled".to_string(), true);
                features.insert("telemetry_enabled".to_string(), true);
                features
            },
            performance: PerformanceConfig {
                max_concurrency: 16,
                cache_size: 10000,
                timeouts: TimeoutConfig {
                    operation_ms: 5000,
                    io_ms: 2000,
                    network_ms: 3000,
                    database_ms: 10000,
                },
                batching: BatchConfig {
                    max_size: 1000,
                    max_wait_ms: 50,
                    enabled: true,
                },
            },
        }
    }

    /// Get a secret value
    pub fn get_secret(&self, key: &str) -> Option<&str> {
        self.secrets.get(key).map(|s| s.as_str())
    }

    /// Check if a feature is enabled
    pub fn is_feature_enabled(&self, feature: &str) -> bool {
        self.features.get(feature).copied().unwrap_or(false)
    }

    /// Get performance configuration
    pub fn performance(&self) -> &PerformanceConfig {
        &self.performance
    }

    /// Get data directory for a specific module
    pub fn module_data_dir(&self, module: &str) -> PathBuf {
        self.data_dir.join(module)
    }
}

impl Default for PerformanceConfig {
    fn default() -> Self {
        Self {
            max_concurrency: 8,
            cache_size: 5000,
            timeouts: TimeoutConfig::default(),
            batching: BatchConfig::default(),
        }
    }
}

impl Default for TimeoutConfig {
    fn default() -> Self {
        Self {
            operation_ms: 30000,
            io_ms: 10000,
            network_ms: 5000,
            database_ms: 15000,
        }
    }
}

impl Default for BatchConfig {
    fn default() -> Self {
        Self {
            max_size: 100,
            max_wait_ms: 100,
            enabled: true,
        }
    }
}

/// Environment context for domain operations
#[derive(Debug)]
pub struct EnvironmentContext {
    /// Current environment configuration
    pub config: EnvironmentConfig,
}

impl EnvironmentContext {
    /// Create a new environment context
    pub fn new(config: EnvironmentConfig) -> Self {
        Self { config }
    }

    /// Clone the environment configuration (thread-local data is not copied)
    pub fn clone_config(&self) -> EnvironmentConfig {
        self.config.clone()
    }

    /// Get current environment name
    pub fn environment(&self) -> &str {
        &self.config.environment
    }

    /// Check if running in development
    pub fn is_development(&self) -> bool {
        self.config.environment == "development"
    }

    /// Check if running in production
    pub fn is_production(&self) -> bool {
        self.config.environment == "production"
    }
}

impl Clone for EnvironmentContext {
    fn clone(&self) -> Self {
        Self::new(self.config.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_development_config() {
        let config = EnvironmentConfig::development();
        assert_eq!(config.environment, "development");
        assert!(config.is_feature_enabled("debug_logging"));
    }

    #[test]
    fn test_production_config() {
        let config = EnvironmentConfig::production();
        assert_eq!(config.environment, "production");
        assert!(!config.is_feature_enabled("debug_logging"));
        assert!(config.is_feature_enabled("telemetry_enabled"));
    }

    #[test]
    fn test_secrets() {
        // Set test environment variable
        env::set_var("TEST_SECRET", "test_value");

        let config = EnvironmentConfig::development();
        assert_eq!(config.get_secret("test_secret"), Some("test_value"));

        // Clean up
        env::remove_var("TEST_SECRET");
    }

    #[test]
    fn test_context() {
        let config = EnvironmentConfig::development();
        let context = EnvironmentContext::new(config);

        assert_eq!(context.environment(), "development");
        assert!(context.is_development());
        assert!(!context.is_production());
    }
}