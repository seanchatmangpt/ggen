//! Configuration management for MCP+
//!
//! Provides TOML configuration loading with validation, defaults, and
//! environment variable overrides.
//!
//! # Example
//!
//! ```ignore
//! use mcp_core::config::{Config, Environment};
//!
//! // Load from file
//! let config = Config::from_file("config/mcp-config.toml")?;
//!
//! // Load with environment detection
//! let config = Config::load_for_environment(Environment::Production)?;
//!
//! // Override with environment variables
//! let config = Config::from_file_with_env_overrides("config/mcp-config.toml")?;
//! ```

use crate::error::{McpError, McpResult};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// MCP+ environment type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum Environment {
    /// Production environment with strict security
    #[default]
    Production,
    /// Development environment with relaxed settings
    Development,
    /// Test environment with mock settings
    Test,
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Environment::Production => write!(f, "production"),
            Environment::Development => write!(f, "development"),
            Environment::Test => write!(f, "test"),
        }
    }
}

impl std::str::FromStr for Environment {
    type Err = McpError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "production" | "prod" => Ok(Environment::Production),
            "development" | "dev" => Ok(Environment::Development),
            "test" | "testing" => Ok(Environment::Test),
            _ => Err(McpError::InvalidInput(format!(
                "Unknown environment: {}. Valid values: production, development, test",
                s
            ))),
        }
    }
}

/// Root configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// MCP protocol settings
    pub mcp: McpConfig,
    /// Cryptographic settings
    pub crypto: CryptoConfig,
    /// Envelope constraints
    pub envelope: EnvelopeConfig,
    /// Kill switch settings
    pub kill_switch: KillSwitchConfig,
    /// Receipt chain settings
    pub receipts: ReceiptsConfig,
    /// Bundle settings
    pub bundles: BundlesConfig,
    /// Epoch settings
    pub epochs: EpochsConfig,
    /// Logging configuration
    pub logging: LoggingConfig,
    /// Metrics configuration
    pub metrics: MetricsConfig,
    /// Storage configuration
    #[serde(default)]
    pub storage: StorageConfig,
    /// Network configuration
    #[serde(default)]
    pub network: NetworkConfig,
}

/// MCP protocol configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpConfig {
    /// Protocol version
    #[serde(default = "default_version")]
    pub version: String,
    /// Current environment
    #[serde(default)]
    pub environment: Environment,
}

fn default_version() -> String {
    "0.2.0".to_string()
}

/// Cryptographic configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CryptoConfig {
    /// Signature algorithm (ed25519, rsa)
    #[serde(default = "default_algorithm")]
    pub algorithm: String,
    /// Hash function (sha256, sha3_256)
    #[serde(default = "default_hash_function")]
    pub hash_function: String,
    /// Key rotation interval in days
    #[serde(default = "default_key_rotation_days")]
    pub key_rotation_days: u32,
    /// Minimum RSA key bits (if RSA is used)
    #[serde(default = "default_min_key_bits")]
    pub min_key_bits: u32,
}

fn default_algorithm() -> String {
    "ed25519".to_string()
}

fn default_hash_function() -> String {
    "sha256".to_string()
}

fn default_key_rotation_days() -> u32 {
    90
}

fn default_min_key_bits() -> u32 {
    2048
}

/// Envelope constraints configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvelopeConfig {
    /// Maximum operations per envelope
    #[serde(default = "default_max_operations")]
    pub default_max_operations: u32,
    /// Maximum memory in MB
    #[serde(default = "default_max_memory_mb")]
    pub default_max_memory_mb: u32,
    /// Maximum duration in milliseconds
    #[serde(default = "default_max_duration_ms")]
    pub default_max_duration_ms: u64,
    /// Strict mode: reject operations exceeding envelope
    #[serde(default = "default_true")]
    pub strict_mode: bool,
    /// Track envelope violations in metrics
    #[serde(default = "default_true")]
    pub track_violations: bool,
}

fn default_max_operations() -> u32 {
    1000
}

fn default_max_memory_mb() -> u32 {
    100
}

fn default_max_duration_ms() -> u64 {
    30000
}

fn default_true() -> bool {
    true
}

/// Kill switch configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KillSwitchConfig {
    /// Global kill switch enabled
    #[serde(default = "default_true")]
    pub global_enabled: bool,
    /// Per-family kill switch enabled
    #[serde(default = "default_true")]
    pub family_enabled: bool,
    /// Per-capability kill switch enabled
    #[serde(default = "default_true")]
    pub capability_enabled: bool,
    /// Epoch-based kill switch enabled
    #[serde(default = "default_true")]
    pub epoch_enabled: bool,
    /// Cooldown period after activation (seconds)
    #[serde(default = "default_cooldown_seconds")]
    pub cooldown_seconds: u32,
}

fn default_cooldown_seconds() -> u32 {
    300
}

/// Receipt chain configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptsConfig {
    /// Enable chained receipt verification
    #[serde(default = "default_true")]
    pub chain_enabled: bool,
    /// Enable Merkle tree for batch verification
    #[serde(default = "default_true")]
    pub merkle_tree_enabled: bool,
    /// Require cryptographic signatures
    #[serde(default = "default_true")]
    pub signature_required: bool,
    /// Maximum chain depth before archival
    #[serde(default = "default_max_chain_depth")]
    pub max_chain_depth: u32,
    /// Archive receipts after N days
    #[serde(default = "default_archive_after_days")]
    pub archive_after_days: u32,
}

fn default_max_chain_depth() -> u32 {
    10000
}

fn default_archive_after_days() -> u32 {
    90
}

/// Bundle configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundlesConfig {
    /// Bundle format (mcpb)
    #[serde(default = "default_bundle_format")]
    pub format: String,
    /// Compression algorithm (none, gzip, zstd)
    #[serde(default = "default_compression")]
    pub compression: String,
    /// Retention period in days
    #[serde(default = "default_retention_days")]
    pub retention_days: u32,
    /// Maximum bundle size in MB
    #[serde(default = "default_max_size_mb")]
    pub max_size_mb: u32,
}

fn default_bundle_format() -> String {
    "mcpb".to_string()
}

fn default_compression() -> String {
    "none".to_string()
}

fn default_retention_days() -> u32 {
    365
}

fn default_max_size_mb() -> u32 {
    100
}

/// Epoch configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EpochsConfig {
    /// Default epoch duration in hours
    #[serde(default = "default_epoch_duration_hours")]
    pub default_duration_hours: u32,
    /// Automatically rotate epochs
    #[serde(default = "default_true")]
    pub auto_rotate: bool,
    /// Grace period after epoch expiration (minutes)
    #[serde(default = "default_grace_period_minutes")]
    pub grace_period_minutes: u32,
    /// Maximum active epochs
    #[serde(default = "default_max_active_epochs")]
    pub max_active_epochs: u32,
}

fn default_epoch_duration_hours() -> u32 {
    24
}

fn default_grace_period_minutes() -> u32 {
    60
}

fn default_max_active_epochs() -> u32 {
    3
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoggingConfig {
    /// Log level (trace, debug, info, warn, error)
    #[serde(default = "default_log_level")]
    pub level: String,
    /// Output format (json, text)
    #[serde(default = "default_log_format")]
    pub format: String,
    /// Log file path
    #[serde(default = "default_log_file")]
    pub file: String,
    /// Maximum log file size in MB
    #[serde(default = "default_max_log_file_size_mb")]
    pub max_file_size_mb: u32,
    /// Number of rotated log files to keep
    #[serde(default = "default_max_log_files")]
    pub max_files: u32,
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_log_format() -> String {
    "json".to_string()
}

fn default_log_file() -> String {
    "/var/log/mcp/mcp.log".to_string()
}

fn default_max_log_file_size_mb() -> u32 {
    100
}

fn default_max_log_files() -> u32 {
    10
}

/// Metrics configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsConfig {
    /// Enable metrics endpoint
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Metrics endpoint path
    #[serde(default = "default_metrics_endpoint")]
    pub endpoint: String,
    /// Metrics server port
    #[serde(default = "default_metrics_port")]
    pub port: u16,
    /// Include histogram buckets
    #[serde(default = "default_true")]
    pub include_histograms: bool,
    /// Default labels for all metrics
    #[serde(default)]
    pub default_labels: Vec<String>,
}

fn default_metrics_endpoint() -> String {
    "/metrics".to_string()
}

fn default_metrics_port() -> u16 {
    9090
}

/// Storage configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StorageConfig {
    /// Storage backend (file, memory, s3, gcs)
    #[serde(default = "default_storage_backend")]
    pub backend: String,
    /// Base path for file storage
    #[serde(default = "default_storage_base_path")]
    pub base_path: String,
    /// Enable encryption at rest
    #[serde(default)]
    pub encryption_enabled: bool,
    /// Compression for stored data
    #[serde(default = "default_compression")]
    pub compression: String,
}

fn default_storage_backend() -> String {
    "file".to_string()
}

fn default_storage_base_path() -> String {
    "/var/lib/mcp/data".to_string()
}

/// Network configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    /// Bind address
    #[serde(default = "default_bind_address")]
    pub bind_address: String,
    /// Server port
    #[serde(default = "default_network_port")]
    pub port: u16,
    /// TLS enabled
    #[serde(default)]
    pub tls_enabled: bool,
    /// TLS certificate path
    #[serde(default)]
    pub tls_cert_path: String,
    /// TLS key path
    #[serde(default)]
    pub tls_key_path: String,
    /// Request timeout in seconds
    #[serde(default = "default_request_timeout_seconds")]
    pub request_timeout_seconds: u32,
    /// Maximum concurrent connections
    #[serde(default = "default_max_connections")]
    pub max_connections: u32,
}

fn default_bind_address() -> String {
    "0.0.0.0".to_string()
}

fn default_network_port() -> u16 {
    8080
}

fn default_request_timeout_seconds() -> u32 {
    30
}

fn default_max_connections() -> u32 {
    1000
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            bind_address: default_bind_address(),
            port: default_network_port(),
            tls_enabled: false,
            tls_cert_path: String::new(),
            tls_key_path: String::new(),
            request_timeout_seconds: default_request_timeout_seconds(),
            max_connections: default_max_connections(),
        }
    }
}

impl Config {
    /// Load configuration from a TOML file
    pub fn from_file<P: AsRef<Path>>(path: P) -> McpResult<Self> {
        let content = std::fs::read_to_string(path.as_ref()).map_err(|e| {
            McpError::IoError(format!(
                "Failed to read config file '{}': {}",
                path.as_ref().display(),
                e
            ))
        })?;

        Self::from_str(&content)
    }

    /// Parse configuration from a TOML string
    pub fn from_str(content: &str) -> McpResult<Self> {
        let config: Config = toml::from_str(content).map_err(|e| {
            McpError::SerializationError(format!("Failed to parse TOML config: {}", e))
        })?;

        config.validate()?;
        Ok(config)
    }

    /// Load configuration with environment variable overrides
    ///
    /// Environment variables use the format: MCP_SECTION_KEY
    /// Examples:
    /// - MCP_CRYPTO_ALGORITHM=ed25519
    /// - MCP_ENVELOPE_MAX_OPERATIONS=2000
    /// - MCP_LOGGING_LEVEL=debug
    pub fn from_file_with_env_overrides<P: AsRef<Path>>(path: P) -> McpResult<Self> {
        let mut config = Self::from_file(path)?;
        config.apply_env_overrides();
        config.validate()?;
        Ok(config)
    }

    /// Load configuration for a specific environment
    ///
    /// Looks for config files in the following order:
    /// 1. config/mcp-{environment}.toml
    /// 2. config/mcp-config.toml (fallback)
    pub fn load_for_environment(env: Environment) -> McpResult<Self> {
        let env_config_path = format!("config/mcp-{}.toml", env);
        let default_config_path = "config/mcp-config.toml";

        if Path::new(&env_config_path).exists() {
            Self::from_file_with_env_overrides(&env_config_path)
        } else if Path::new(default_config_path).exists() {
            Self::from_file_with_env_overrides(default_config_path)
        } else {
            Err(McpError::IoError(format!(
                "No configuration file found for environment '{}'. Tried: {}, {}",
                env, env_config_path, default_config_path
            )))
        }
    }

    /// Detect environment from MCP_ENVIRONMENT variable and load config
    pub fn load_auto() -> McpResult<Self> {
        let env = std::env::var("MCP_ENVIRONMENT")
            .unwrap_or_else(|_| "production".to_string())
            .parse::<Environment>()?;

        Self::load_for_environment(env)
    }

    /// Apply environment variable overrides
    fn apply_env_overrides(&mut self) {
        // MCP settings
        if let Ok(v) = std::env::var("MCP_VERSION") {
            self.mcp.version = v;
        }
        if let Ok(v) = std::env::var("MCP_ENVIRONMENT") {
            if let Ok(env) = v.parse() {
                self.mcp.environment = env;
            }
        }

        // Crypto settings
        if let Ok(v) = std::env::var("MCP_CRYPTO_ALGORITHM") {
            self.crypto.algorithm = v;
        }
        if let Ok(v) = std::env::var("MCP_CRYPTO_HASH_FUNCTION") {
            self.crypto.hash_function = v;
        }
        if let Ok(v) = std::env::var("MCP_CRYPTO_KEY_ROTATION_DAYS") {
            if let Ok(days) = v.parse() {
                self.crypto.key_rotation_days = days;
            }
        }

        // Envelope settings
        if let Ok(v) = std::env::var("MCP_ENVELOPE_MAX_OPERATIONS") {
            if let Ok(ops) = v.parse() {
                self.envelope.default_max_operations = ops;
            }
        }
        if let Ok(v) = std::env::var("MCP_ENVELOPE_MAX_MEMORY_MB") {
            if let Ok(mem) = v.parse() {
                self.envelope.default_max_memory_mb = mem;
            }
        }
        if let Ok(v) = std::env::var("MCP_ENVELOPE_MAX_DURATION_MS") {
            if let Ok(dur) = v.parse() {
                self.envelope.default_max_duration_ms = dur;
            }
        }
        if let Ok(v) = std::env::var("MCP_ENVELOPE_STRICT_MODE") {
            self.envelope.strict_mode = v.to_lowercase() == "true" || v == "1";
        }

        // Kill switch settings
        if let Ok(v) = std::env::var("MCP_KILL_SWITCH_GLOBAL_ENABLED") {
            self.kill_switch.global_enabled = v.to_lowercase() == "true" || v == "1";
        }
        if let Ok(v) = std::env::var("MCP_KILL_SWITCH_COOLDOWN_SECONDS") {
            if let Ok(secs) = v.parse() {
                self.kill_switch.cooldown_seconds = secs;
            }
        }

        // Receipts settings
        if let Ok(v) = std::env::var("MCP_RECEIPTS_CHAIN_ENABLED") {
            self.receipts.chain_enabled = v.to_lowercase() == "true" || v == "1";
        }
        if let Ok(v) = std::env::var("MCP_RECEIPTS_SIGNATURE_REQUIRED") {
            self.receipts.signature_required = v.to_lowercase() == "true" || v == "1";
        }

        // Logging settings
        if let Ok(v) = std::env::var("MCP_LOGGING_LEVEL") {
            self.logging.level = v;
        }
        if let Ok(v) = std::env::var("MCP_LOGGING_FORMAT") {
            self.logging.format = v;
        }
        if let Ok(v) = std::env::var("MCP_LOGGING_FILE") {
            self.logging.file = v;
        }

        // Metrics settings
        if let Ok(v) = std::env::var("MCP_METRICS_ENABLED") {
            self.metrics.enabled = v.to_lowercase() == "true" || v == "1";
        }
        if let Ok(v) = std::env::var("MCP_METRICS_PORT") {
            if let Ok(port) = v.parse() {
                self.metrics.port = port;
            }
        }

        // Network settings
        if let Ok(v) = std::env::var("MCP_NETWORK_BIND_ADDRESS") {
            self.network.bind_address = v;
        }
        if let Ok(v) = std::env::var("MCP_NETWORK_PORT") {
            if let Ok(port) = v.parse() {
                self.network.port = port;
            }
        }
        if let Ok(v) = std::env::var("MCP_NETWORK_TLS_ENABLED") {
            self.network.tls_enabled = v.to_lowercase() == "true" || v == "1";
        }
    }

    /// Validate configuration values
    pub fn validate(&self) -> McpResult<()> {
        // Validate crypto algorithm
        let valid_algorithms = ["ed25519", "rsa"];
        if !valid_algorithms.contains(&self.crypto.algorithm.as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid crypto algorithm '{}'. Valid: {:?}",
                self.crypto.algorithm, valid_algorithms
            )));
        }

        // Validate hash function
        let valid_hash_functions = ["sha256", "sha3_256"];
        if !valid_hash_functions.contains(&self.crypto.hash_function.as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid hash function '{}'. Valid: {:?}",
                self.crypto.hash_function, valid_hash_functions
            )));
        }

        // Validate key rotation days
        if self.crypto.key_rotation_days == 0 {
            return Err(McpError::InvalidInput(
                "Key rotation days must be greater than 0".to_string(),
            ));
        }

        // Validate envelope constraints
        if self.envelope.default_max_operations == 0 {
            return Err(McpError::InvalidInput(
                "Max operations must be greater than 0".to_string(),
            ));
        }
        if self.envelope.default_max_memory_mb == 0 {
            return Err(McpError::InvalidInput(
                "Max memory must be greater than 0".to_string(),
            ));
        }
        if self.envelope.default_max_duration_ms == 0 {
            return Err(McpError::InvalidInput(
                "Max duration must be greater than 0".to_string(),
            ));
        }

        // Validate log level
        let valid_log_levels = ["trace", "debug", "info", "warn", "error"];
        if !valid_log_levels.contains(&self.logging.level.to_lowercase().as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid log level '{}'. Valid: {:?}",
                self.logging.level, valid_log_levels
            )));
        }

        // Validate log format
        let valid_log_formats = ["json", "text"];
        if !valid_log_formats.contains(&self.logging.format.to_lowercase().as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid log format '{}'. Valid: {:?}",
                self.logging.format, valid_log_formats
            )));
        }

        // Validate compression
        let valid_compressions = ["none", "gzip", "zstd"];
        if !valid_compressions.contains(&self.bundles.compression.as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid bundle compression '{}'. Valid: {:?}",
                self.bundles.compression, valid_compressions
            )));
        }

        // Validate storage backend
        let valid_backends = ["file", "memory", "s3", "gcs"];
        if !valid_backends.contains(&self.storage.backend.as_str()) {
            return Err(McpError::InvalidInput(format!(
                "Invalid storage backend '{}'. Valid: {:?}",
                self.storage.backend, valid_backends
            )));
        }

        // Validate TLS configuration
        if self.network.tls_enabled {
            if self.network.tls_cert_path.is_empty() {
                return Err(McpError::InvalidInput(
                    "TLS is enabled but tls_cert_path is empty".to_string(),
                ));
            }
            if self.network.tls_key_path.is_empty() {
                return Err(McpError::InvalidInput(
                    "TLS is enabled but tls_key_path is empty".to_string(),
                ));
            }
        }

        Ok(())
    }

    /// Get default configuration for production
    pub fn default_production() -> Self {
        Self {
            mcp: McpConfig {
                version: default_version(),
                environment: Environment::Production,
            },
            crypto: CryptoConfig {
                algorithm: default_algorithm(),
                hash_function: default_hash_function(),
                key_rotation_days: default_key_rotation_days(),
                min_key_bits: default_min_key_bits(),
            },
            envelope: EnvelopeConfig {
                default_max_operations: default_max_operations(),
                default_max_memory_mb: default_max_memory_mb(),
                default_max_duration_ms: default_max_duration_ms(),
                strict_mode: true,
                track_violations: true,
            },
            kill_switch: KillSwitchConfig {
                global_enabled: true,
                family_enabled: true,
                capability_enabled: true,
                epoch_enabled: true,
                cooldown_seconds: default_cooldown_seconds(),
            },
            receipts: ReceiptsConfig {
                chain_enabled: true,
                merkle_tree_enabled: true,
                signature_required: true,
                max_chain_depth: default_max_chain_depth(),
                archive_after_days: default_archive_after_days(),
            },
            bundles: BundlesConfig {
                format: default_bundle_format(),
                compression: default_compression(),
                retention_days: default_retention_days(),
                max_size_mb: default_max_size_mb(),
            },
            epochs: EpochsConfig {
                default_duration_hours: default_epoch_duration_hours(),
                auto_rotate: true,
                grace_period_minutes: default_grace_period_minutes(),
                max_active_epochs: default_max_active_epochs(),
            },
            logging: LoggingConfig {
                level: default_log_level(),
                format: default_log_format(),
                file: default_log_file(),
                max_file_size_mb: default_max_log_file_size_mb(),
                max_files: default_max_log_files(),
            },
            metrics: MetricsConfig {
                enabled: true,
                endpoint: default_metrics_endpoint(),
                port: default_metrics_port(),
                include_histograms: true,
                default_labels: vec!["environment".to_string(), "version".to_string()],
            },
            storage: StorageConfig {
                backend: default_storage_backend(),
                base_path: default_storage_base_path(),
                encryption_enabled: true,
                compression: "zstd".to_string(),
            },
            network: NetworkConfig::default(),
        }
    }

    /// Check if running in production mode
    pub fn is_production(&self) -> bool {
        self.mcp.environment == Environment::Production
    }

    /// Check if running in development mode
    pub fn is_development(&self) -> bool {
        self.mcp.environment == Environment::Development
    }

    /// Check if running in test mode
    pub fn is_test(&self) -> bool {
        self.mcp.environment == Environment::Test
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environment_parsing() {
        assert_eq!(
            "production".parse::<Environment>().unwrap(),
            Environment::Production
        );
        assert_eq!(
            "prod".parse::<Environment>().unwrap(),
            Environment::Production
        );
        assert_eq!(
            "development".parse::<Environment>().unwrap(),
            Environment::Development
        );
        assert_eq!(
            "dev".parse::<Environment>().unwrap(),
            Environment::Development
        );
        assert_eq!("test".parse::<Environment>().unwrap(), Environment::Test);
        assert_eq!(
            "testing".parse::<Environment>().unwrap(),
            Environment::Test
        );

        assert!("invalid".parse::<Environment>().is_err());
    }

    #[test]
    fn test_environment_display() {
        assert_eq!(Environment::Production.to_string(), "production");
        assert_eq!(Environment::Development.to_string(), "development");
        assert_eq!(Environment::Test.to_string(), "test");
    }

    #[test]
    fn test_config_from_toml_string() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "ed25519"
            hash_function = "sha256"
            key_rotation_days = 30
            min_key_bits = 2048

            [envelope]
            default_max_operations = 500
            default_max_memory_mb = 50
            default_max_duration_ms = 10000
            strict_mode = true
            track_violations = true

            [kill_switch]
            global_enabled = true
            family_enabled = true
            capability_enabled = true
            epoch_enabled = true
            cooldown_seconds = 60

            [receipts]
            chain_enabled = true
            merkle_tree_enabled = true
            signature_required = true
            max_chain_depth = 5000
            archive_after_days = 30

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30
            max_size_mb = 50

            [epochs]
            default_duration_hours = 12
            auto_rotate = true
            grace_period_minutes = 30
            max_active_epochs = 2

            [logging]
            level = "debug"
            format = "json"
            file = "/tmp/mcp.log"
            max_file_size_mb = 10
            max_files = 3

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9091
            include_histograms = true
            default_labels = ["env"]

            [storage]
            backend = "file"
            path = "/tmp/mcp-storage"
            max_size_mb = 100
            retention_days = 30
            compression = "none"
        "#;

        let config = Config::from_str(toml).unwrap();
        assert_eq!(config.mcp.version, "0.2.0");
        assert_eq!(config.mcp.environment, Environment::Test);
        assert_eq!(config.crypto.algorithm, "ed25519");
        assert_eq!(config.envelope.default_max_operations, 500);
        assert!(config.kill_switch.global_enabled);
        assert!(config.receipts.chain_enabled);
        assert_eq!(config.epochs.default_duration_hours, 12);
        assert_eq!(config.logging.level, "debug");
        assert!(config.metrics.enabled);
    }

    #[test]
    fn test_config_validation_invalid_algorithm() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "invalid"
            hash_function = "sha256"
            key_rotation_days = 30

            [envelope]
            default_max_operations = 100
            default_max_memory_mb = 10
            default_max_duration_ms = 5000

            [kill_switch]
            global_enabled = true

            [receipts]
            chain_enabled = true

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30

            [epochs]
            default_duration_hours = 24

            [logging]
            level = "info"
            format = "json"
            file = "/tmp/mcp.log"

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9090
        "#;

        let result = Config::from_str(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid crypto algorithm"));
    }

    #[test]
    fn test_config_validation_invalid_log_level() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "ed25519"
            hash_function = "sha256"
            key_rotation_days = 30

            [envelope]
            default_max_operations = 100
            default_max_memory_mb = 10
            default_max_duration_ms = 5000

            [kill_switch]
            global_enabled = true

            [receipts]
            chain_enabled = true

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30

            [epochs]
            default_duration_hours = 24

            [logging]
            level = "verbose"
            format = "json"
            file = "/tmp/mcp.log"

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9090
        "#;

        let result = Config::from_str(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid log level"));
    }

    #[test]
    fn test_config_validation_zero_operations() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "ed25519"
            hash_function = "sha256"
            key_rotation_days = 30

            [envelope]
            default_max_operations = 0
            default_max_memory_mb = 10
            default_max_duration_ms = 5000

            [kill_switch]
            global_enabled = true

            [receipts]
            chain_enabled = true

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30

            [epochs]
            default_duration_hours = 24

            [logging]
            level = "info"
            format = "json"
            file = "/tmp/mcp.log"

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9090
        "#;

        let result = Config::from_str(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Max operations must be greater than 0"));
    }

    #[test]
    fn test_default_production_config() {
        let config = Config::default_production();
        assert_eq!(config.mcp.environment, Environment::Production);
        assert_eq!(config.crypto.algorithm, "ed25519");
        assert!(config.envelope.strict_mode);
        assert!(config.receipts.signature_required);
        assert!(config.is_production());
        assert!(!config.is_development());
        assert!(!config.is_test());
    }

    #[test]
    fn test_config_with_defaults() {
        // Minimal config that relies on defaults
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]

            [envelope]

            [kill_switch]

            [receipts]

            [bundles]

            [epochs]

            [logging]

            [metrics]

            [storage]
            backend = "memory"
        "#;

        let config = Config::from_str(toml).unwrap();
        assert_eq!(config.crypto.algorithm, "ed25519");
        assert_eq!(config.crypto.hash_function, "sha256");
        assert_eq!(config.crypto.key_rotation_days, 90);
        assert_eq!(config.envelope.default_max_operations, 1000);
        assert_eq!(config.envelope.default_max_memory_mb, 100);
        assert_eq!(config.logging.level, "info");
    }

    #[test]
    fn test_tls_validation() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "ed25519"
            hash_function = "sha256"
            key_rotation_days = 30

            [envelope]
            default_max_operations = 100
            default_max_memory_mb = 10
            default_max_duration_ms = 5000

            [kill_switch]
            global_enabled = true

            [receipts]
            chain_enabled = true

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30

            [epochs]
            default_duration_hours = 24

            [logging]
            level = "info"
            format = "json"
            file = "/tmp/mcp.log"

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9090

            [network]
            tls_enabled = true
            tls_cert_path = ""
            tls_key_path = ""

            [storage]
            backend = "memory"
        "#;

        let result = Config::from_str(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("TLS is enabled but tls_cert_path is empty"));
    }

    #[test]
    fn test_storage_backend_validation() {
        let toml = r#"
            [mcp]
            version = "0.2.0"
            environment = "test"

            [crypto]
            algorithm = "ed25519"
            hash_function = "sha256"
            key_rotation_days = 30

            [envelope]
            default_max_operations = 100
            default_max_memory_mb = 10
            default_max_duration_ms = 5000

            [kill_switch]
            global_enabled = true

            [receipts]
            chain_enabled = true

            [bundles]
            format = "mcpb"
            compression = "none"
            retention_days = 30

            [epochs]
            default_duration_hours = 24

            [logging]
            level = "info"
            format = "json"
            file = "/tmp/mcp.log"

            [metrics]
            enabled = true
            endpoint = "/metrics"
            port = 9090

            [storage]
            backend = "invalid"
        "#;

        let result = Config::from_str(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid storage backend"));
    }
}
