//! Configuration for cleanroom testing framework

use crate::limits::ResourceLimits;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

/// Cleanroom configuration following best practices
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomConfig {
    /// Enable singleton container pattern
    pub enable_singleton_containers: bool,
    /// Container startup timeout
    pub container_startup_timeout: Duration,
    /// Test execution timeout
    pub test_execution_timeout: Duration,
    /// Enable deterministic execution
    pub enable_deterministic_execution: bool,
    /// Fixed seed for deterministic runs
    pub deterministic_seed: Option<u64>,
    /// Enable coverage tracking
    pub enable_coverage_tracking: bool,
    /// Enable snapshot testing
    pub enable_snapshot_testing: bool,
    /// Enable tracing and observability
    pub enable_tracing: bool,
    /// Resource limits configuration
    pub resource_limits: ResourceLimits,
    /// Security policy configuration
    pub security_policy: SecurityPolicy,
    /// Performance monitoring configuration
    pub performance_monitoring: PerformanceMonitoringConfig,
    /// Container customizers
    pub container_customizers: HashMap<String, ContainerCustomizer>,
}

/// Security policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityPolicy {
    /// Enable network isolation
    pub enable_network_isolation: bool,
    /// Enable filesystem isolation
    pub enable_filesystem_isolation: bool,
    /// Enable process isolation
    pub enable_process_isolation: bool,
    /// Allowed network ports
    pub allowed_ports: Vec<u16>,
    /// Blocked network addresses
    pub blocked_addresses: Vec<String>,
    /// Enable sensitive data redaction
    pub enable_data_redaction: bool,
    /// Redaction patterns
    pub redaction_patterns: Vec<String>,
}

/// Performance monitoring configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMonitoringConfig {
    /// Enable performance monitoring
    pub enable_monitoring: bool,
    /// Metrics collection interval
    pub metrics_interval: Duration,
    /// Performance thresholds
    pub thresholds: PerformanceThresholds,
    /// Enable detailed profiling
    pub enable_profiling: bool,
    /// Enable memory tracking
    pub enable_memory_tracking: bool,
}

/// Performance thresholds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceThresholds {
    /// Maximum CPU usage percentage
    pub max_cpu_usage_percent: f64,
    /// Maximum memory usage bytes
    pub max_memory_usage_bytes: u64,
    /// Maximum test execution time
    pub max_test_execution_time: Duration,
    /// Maximum container startup time
    pub max_container_startup_time: Duration,
}

/// Container customizer for flexible configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerCustomizer {
    /// Customizer name
    pub name: String,
    /// Environment variables
    pub env_vars: HashMap<String, String>,
    /// Volume mounts
    pub volume_mounts: Vec<VolumeMount>,
    /// Port mappings
    pub port_mappings: Vec<PortMapping>,
    /// Resource limits
    pub resource_limits: ContainerResourceLimits,
    /// Health check configuration
    pub health_check: HealthCheckConfig,
    /// Custom initialization commands
    pub init_commands: Vec<String>,
}

/// Volume mount configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VolumeMount {
    pub host_path: String,
    pub container_path: String,
    pub read_only: bool,
}

/// Port mapping configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortMapping {
    pub container_port: u16,
    pub host_port: Option<u16>,
    pub protocol: String,
}

/// Container resource limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerResourceLimits {
    pub cpu_limit: f64,
    pub memory_limit_bytes: u64,
    pub disk_limit_bytes: u64,
    pub network_bandwidth_limit: u64,
}

/// Health check configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckConfig {
    pub command: String,
    pub interval: Duration,
    pub timeout: Duration,
    pub retries: u32,
    pub start_period: Duration,
}

impl Default for CleanroomConfig {
    fn default() -> Self {
        Self {
            enable_singleton_containers: true,
            container_startup_timeout: Duration::from_secs(60),
            container_execution_timeout: Duration::from_secs(300),
            max_concurrent_containers: 10,
            enable_resource_monitoring: true,
            enable_performance_monitoring: true,
            enable_deterministic_execution: true,
            enable_security_policy: true,
            enable_coverage_tracking: true,
            enable_snapshots: true,
            enable_tracing: true,
            enable_redaction: true,
            enable_reports: true,
        }
    }
}

impl Default for PolicyConfig {
    fn default() -> Self {
        Self {
            network: "offline".to_string(),
            filesystem: "readonly".to_string(),
            process: "strict".to_string(),
        }
    }
}

impl Default for ResourceConfig {
    fn default() -> Self {
        Self {
            cpu_time_secs: Some(300), // 5 minutes
            memory_bytes: Some(512 * 1024 * 1024), // 512MB
            file_size_bytes: Some(100 * 1024 * 1024), // 100MB
            process_count: Some(10),
        }
    }
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            level: "info".to_string(),
            structured: true,
            file: None,
        }
    }
}

impl CleanroomConfig {
    /// Load configuration from file
    pub fn from_file(path: PathBuf) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let config: CleanroomConfig = toml::from_str(&content)?;
        Ok(config)
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self, Box<dyn std::error::Error>> {
        let mut config = Self::default();

        if let Ok(backend) = std::env::var("CLEANROOM_BACKEND") {
            config.backend = backend;
        }

        if let Ok(network) = std::env::var("CLEANROOM_NETWORK") {
            config.policy.network = network;
        }

        if let Ok(filesystem) = std::env::var("CLEANROOM_FILESYSTEM") {
            config.policy.filesystem = filesystem;
        }

        if let Ok(process) = std::env::var("CLEANROOM_PROCESS") {
            config.policy.process = process;
        }

        if let Ok(cpu_time) = std::env::var("CLEANROOM_CPU_TIME_SECS") {
            config.limits.cpu_time_secs = Some(cpu_time.parse()?);
        }

        if let Ok(memory) = std::env::var("CLEANROOM_MEMORY_BYTES") {
            config.limits.memory_bytes = Some(memory.parse()?);
        }

        if let Ok(file_size) = std::env::var("CLEANROOM_FILE_SIZE_BYTES") {
            config.limits.file_size_bytes = Some(file_size.parse()?);
        }

        if let Ok(process_count) = std::env::var("CLEANROOM_PROCESS_COUNT") {
            config.limits.process_count = Some(process_count.parse()?);
        }

        if let Ok(level) = std::env::var("CLEANROOM_LOG_LEVEL") {
            config.logging.level = level;
        }

        if let Ok(structured) = std::env::var("CLEANROOM_STRUCTURED_LOGGING") {
            config.logging.structured = structured.parse().unwrap_or(true);
        }

        if let Ok(file) = std::env::var("CLEANROOM_LOG_FILE") {
            config.logging.file = Some(file);
        }

        Ok(config)
    }

    /// Load configuration with precedence: TOML < ENV < CLI
    pub fn load_with_precedence(cli_overrides: Option<HashMap<String, String>>) -> Result<Self, Box<dyn std::error::Error>> {
        // Start with defaults
        let mut config = Self::default();

        // Apply TOML configuration if available
        if let Ok(toml_config) = Self::from_file(PathBuf::from("cleanroom.toml")) {
            config = toml_config;
        }

        // Apply environment variable overrides
        config = Self::from_env()?;

        // Apply CLI overrides (highest precedence)
        if let Some(overrides) = cli_overrides {
            config.apply_cli_overrides(overrides)?;
        }

        config.validate()?;
        Ok(config)
    }

    /// Apply CLI overrides to configuration
    pub fn apply_cli_overrides(&mut self, overrides: HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
        for (key, value) in overrides {
            match key.as_str() {
                "backend" => self.backend = value,
                "network" => self.policy.network = value,
                "filesystem" => self.policy.filesystem = value,
                "process" => self.policy.process = value,
                "cpu-time-secs" => self.limits.cpu_time_secs = Some(value.parse()?),
                "memory-bytes" => self.limits.memory_bytes = Some(value.parse()?),
                "file-size-bytes" => self.limits.file_size_bytes = Some(value.parse()?),
                "process-count" => self.limits.process_count = Some(value.parse()?),
                "log-level" => self.logging.level = value,
                "structured-logging" => self.logging.structured = value.parse().unwrap_or(true),
                "log-file" => self.logging.file = Some(value),
                _ => return Err(format!("Unknown configuration key: {}", key).into()),
            }
        }
        Ok(())
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Validate backend
        if !["auto", "local", "docker", "podman"].contains(&self.backend.as_str()) {
            return Err(format!("Invalid backend: {}", self.backend).into());
        }

        // Validate policy
        if !["offline", "limited", "open"].contains(&self.policy.network.as_str()) {
            return Err(format!("Invalid network policy: {}", self.policy.network).into());
        }

        if !["readonly", "writable"].contains(&self.policy.filesystem.as_str()) {
            return Err(format!("Invalid filesystem policy: {}", self.policy.filesystem).into());
        }

        if !["strict", "standard", "permissive"].contains(&self.policy.process.as_str()) {
            return Err(format!("Invalid process policy: {}", self.policy.process).into());
        }

        // Validate resource limits
        if let Some(cpu_time) = self.limits.cpu_time_secs {
            if cpu_time == 0 || cpu_time > 3600 {
                return Err(format!("Invalid CPU time limit: {}", cpu_time).into());
            }
        }

        if let Some(memory) = self.limits.memory_bytes {
            if memory == 0 || memory > 8 * 1024 * 1024 * 1024 {
                return Err(format!("Invalid memory limit: {}", memory).into());
            }
        }

        if let Some(file_size) = self.limits.file_size_bytes {
            if file_size == 0 || file_size > 1024 * 1024 * 1024 {
                return Err(format!("Invalid file size limit: {}", file_size).into());
            }
        }

        if let Some(process_count) = self.limits.process_count {
            if process_count == 0 || process_count > 100 {
                return Err(format!("Invalid process count limit: {}", process_count).into());
            }
        }

        // Validate logging
        if !["trace", "debug", "info", "warn", "error"].contains(&self.logging.level.as_str()) {
            return Err(format!("Invalid log level: {}", self.logging.level).into());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = CleanroomConfig::default();
        assert_eq!(config.backend, "auto");
        assert_eq!(config.policy.network, "offline");
        assert_eq!(config.policy.filesystem, "readonly");
        assert_eq!(config.policy.process, "strict");
    }

    #[test]
    fn test_config_validation() {
        let config = CleanroomConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_config_validation_invalid_backend() {
        let mut config = CleanroomConfig::default();
        config.backend = "invalid".to_string();
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_validation_invalid_network() {
        let mut config = CleanroomConfig::default();
        config.policy.network = "invalid".to_string();
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_cli_overrides() {
        let mut config = CleanroomConfig::default();
        let mut overrides = HashMap::new();
        overrides.insert("backend".to_string(), "docker".to_string());
        overrides.insert("network".to_string(), "open".to_string());
        
        config.apply_cli_overrides(overrides).unwrap();
        assert_eq!(config.backend, "docker");
        assert_eq!(config.policy.network, "open");
    }
}