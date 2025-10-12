//! Configuration for cleanroom testing framework
//!
//! This module provides configuration management following core team best practices:
//! - Type-safe configuration
//! - Environment variable integration
//! - File-based configuration
//! - Configuration validation

use crate::error::{Result, CleanroomError};
use crate::policy::SecurityPolicy;
use crate::limits::ResourceLimits;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Main cleanroom configuration
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
    /// Maximum concurrent containers
    pub max_concurrent_containers: u32,
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
    /// Host path to mount
    pub host_path: String,
    /// Container path where host path is mounted
    pub container_path: String,
    /// Whether the mount is read-only
    pub read_only: bool,
}

/// Port mapping configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortMapping {
    /// Container port
    pub container_port: u16,
    /// Host port (None for automatic assignment)
    pub host_port: Option<u16>,
    /// Protocol (tcp/udp)
    pub protocol: String,
}

/// Container resource limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerResourceLimits {
    /// CPU limit (percentage)
    pub cpu_limit: f64,
    /// Memory limit in bytes
    pub memory_limit_bytes: u64,
    /// Disk limit in bytes
    pub disk_limit_bytes: u64,
    /// Network bandwidth limit in bytes per second
    pub network_bandwidth_limit: u64,
}

/// Health check configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckConfig {
    /// Health check command
    pub command: String,
    /// Interval between health checks
    pub interval: Duration,
    /// Timeout for health check
    pub timeout: Duration,
    /// Number of retries before marking as unhealthy
    pub retries: u32,
    /// Start period before health checks begin
    pub start_period: Duration,
}

impl Default for CleanroomConfig {
    fn default() -> Self {
        Self {
            enable_singleton_containers: true,
            container_startup_timeout: Duration::from_secs(30),
            test_execution_timeout: Duration::from_secs(300),
            enable_deterministic_execution: true,
            deterministic_seed: Some(42),
            enable_coverage_tracking: true,
            enable_snapshot_testing: true,
            enable_tracing: true,
            resource_limits: ResourceLimits::default(),
            security_policy: SecurityPolicy::default(),
            performance_monitoring: PerformanceMonitoringConfig::default(),
            container_customizers: HashMap::new(),
            max_concurrent_containers: 10,
        }
    }
}

impl Default for PerformanceMonitoringConfig {
    fn default() -> Self {
        Self {
            enable_monitoring: true,
            metrics_interval: Duration::from_secs(5),
            thresholds: PerformanceThresholds::default(),
            enable_profiling: false,
            enable_memory_tracking: true,
        }
    }
}

impl Default for PerformanceThresholds {
    fn default() -> Self {
        Self {
            max_cpu_usage_percent: 80.0,
            max_memory_usage_bytes: 1024 * 1024 * 1024, // 1GB
            max_test_execution_time: Duration::from_secs(300),
            max_container_startup_time: Duration::from_secs(30),
        }
    }
}

impl CleanroomConfig {
    /// Create a new configuration with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self> {
        let mut config = Self::default();

        // Load singleton containers setting
        if let Ok(val) = std::env::var("CLEANROOM_ENABLE_SINGLETON_CONTAINERS") {
            config.enable_singleton_containers = val.parse().unwrap_or(true);
        }

        // Load timeouts
        if let Ok(val) = std::env::var("CLEANROOM_CONTAINER_STARTUP_TIMEOUT") {
            if let Ok(seconds) = val.parse::<u64>() {
                config.container_startup_timeout = Duration::from_secs(seconds);
            }
        }

        if let Ok(val) = std::env::var("CLEANROOM_TEST_EXECUTION_TIMEOUT") {
            if let Ok(seconds) = val.parse::<u64>() {
                config.test_execution_timeout = Duration::from_secs(seconds);
            }
        }

        // Load deterministic execution settings
        if let Ok(val) = std::env::var("CLEANROOM_ENABLE_DETERMINISTIC_EXECUTION") {
            config.enable_deterministic_execution = val.parse().unwrap_or(true);
        }

        if let Ok(val) = std::env::var("CLEANROOM_DETERMINISTIC_SEED") {
            if let Ok(seed) = val.parse::<u64>() {
                config.deterministic_seed = Some(seed);
            }
        }

        // Load feature flags
        if let Ok(val) = std::env::var("CLEANROOM_ENABLE_COVERAGE_TRACKING") {
            config.enable_coverage_tracking = val.parse().unwrap_or(true);
        }

        if let Ok(val) = std::env::var("CLEANROOM_ENABLE_SNAPSHOT_TESTING") {
            config.enable_snapshot_testing = val.parse().unwrap_or(true);
        }

        if let Ok(val) = std::env::var("CLEANROOM_ENABLE_TRACING") {
            config.enable_tracing = val.parse().unwrap_or(true);
        }

        Ok(config)
    }

    /// Load configuration from TOML file
    pub fn from_file(path: &str) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| CleanroomError::io_error(format!("Failed to read config file: {}", e)))?;

        let config: CleanroomConfig = toml::from_str(&content)
            .map_err(|e| CleanroomError::serialization_error(format!("Failed to parse config file: {}", e)))?;

        Ok(config)
    }

    /// Save configuration to TOML file
    pub fn to_file(&self, path: &str) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(format!("Failed to serialize config: {}", e)))?;

        std::fs::write(path, content)
            .map_err(|e| CleanroomError::io_error(format!("Failed to write config file: {}", e)))?;

        Ok(())
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        // Validate timeouts
        if self.container_startup_timeout.as_secs() == 0 {
            return Err(CleanroomError::validation_error("Container startup timeout must be greater than 0"));
        }

        if self.test_execution_timeout.as_secs() == 0 {
            return Err(CleanroomError::validation_error("Test execution timeout must be greater than 0"));
        }

        // Validate resource limits
        self.resource_limits.validate()?;

        // Validate security policy
        if self.security_policy.allowed_ports.is_empty() {
            return Err(CleanroomError::validation_error("At least one allowed port must be configured"));
        }

        // Validate performance monitoring
        if self.performance_monitoring.metrics_interval.as_secs() == 0 {
            return Err(CleanroomError::validation_error("Metrics interval must be greater than 0"));
        }

        Ok(())
    }

    /// Get configuration summary
    pub fn summary(&self) -> String {
        format!(
            "Cleanroom Configuration Summary:\n\
            Singleton Containers: {}\n\
            Container Startup Timeout: {:?}\n\
            Test Execution Timeout: {:?}\n\
            Deterministic Execution: {}\n\
            Coverage Tracking: {}\n\
            Snapshot Testing: {}\n\
            Tracing: {}\n\
            Resource Limits: {} MB memory, {}% CPU\n\
            Security Level: {:?}",
            self.enable_singleton_containers,
            self.container_startup_timeout,
            self.test_execution_timeout,
            self.enable_deterministic_execution,
            self.enable_coverage_tracking,
            self.enable_snapshot_testing,
            self.enable_tracing,
            self.resource_limits.memory.max_usage_bytes / (1024 * 1024),
            self.resource_limits.cpu.max_usage_percent,
            self.security_policy.security_level,
        )
    }

    /// Merge with another configuration (other takes precedence)
    pub fn merge(&mut self, other: &CleanroomConfig) {
        if other.enable_singleton_containers != self.enable_singleton_containers {
            self.enable_singleton_containers = other.enable_singleton_containers;
        }

        if other.container_startup_timeout != self.container_startup_timeout {
            self.container_startup_timeout = other.container_startup_timeout;
        }

        if other.test_execution_timeout != self.test_execution_timeout {
            self.test_execution_timeout = other.test_execution_timeout;
        }

        if other.enable_deterministic_execution != self.enable_deterministic_execution {
            self.enable_deterministic_execution = other.enable_deterministic_execution;
        }

        if other.deterministic_seed != self.deterministic_seed {
            self.deterministic_seed = other.deterministic_seed;
        }

        if other.enable_coverage_tracking != self.enable_coverage_tracking {
            self.enable_coverage_tracking = other.enable_coverage_tracking;
        }

        if other.enable_snapshot_testing != self.enable_snapshot_testing {
            self.enable_snapshot_testing = other.enable_snapshot_testing;
        }

        if other.enable_tracing != self.enable_tracing {
            self.enable_tracing = other.enable_tracing;
        }

        // Merge resource limits
        self.resource_limits = other.resource_limits.clone();

        // Merge security policy
        self.security_policy = other.security_policy.clone();

        // Merge performance monitoring
        self.performance_monitoring = other.performance_monitoring.clone();

        // Merge container customizers
        for (name, customizer) in &other.container_customizers {
            self.container_customizers.insert(name.clone(), customizer.clone());
        }
    }
}

impl Default for ContainerResourceLimits {
    fn default() -> Self {
        Self {
            cpu_limit: 1.0,
            memory_limit_bytes: 512 * 1024 * 1024,
            disk_limit_bytes: 1024 * 1024 * 1024,
            network_bandwidth_limit: 100 * 1024 * 1024,
        }
    }
}

impl Default for HealthCheckConfig {
    fn default() -> Self {
        Self {
            command: "echo 'health check'".to_string(),
            interval: Duration::from_secs(30),
            timeout: Duration::from_secs(10),
            retries: 3,
            start_period: Duration::from_secs(30),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_creation() {
        let config = CleanroomConfig::default();
        assert!(config.validate().is_ok());
        assert!(config.enable_singleton_containers);
        assert_eq!(config.container_startup_timeout.as_secs(), 30);
        assert_eq!(config.test_execution_timeout.as_secs(), 300);
        assert!(config.enable_deterministic_execution);
        assert!(config.enable_coverage_tracking);
        assert!(config.enable_snapshot_testing);
        assert!(config.enable_tracing);
    }

    #[test]
    fn test_config_from_env() {
        unsafe {
            std::env::set_var("CLEANROOM_ENABLE_DETERMINISTIC_EXECUTION", "false");
            std::env::set_var("CLEANROOM_DETERMINISTIC_SEED", "12345");
        }

        let config = CleanroomConfig::from_env().unwrap();
        assert_eq!(config.enable_deterministic_execution, false);
        assert_eq!(config.deterministic_seed, Some(12345));

        unsafe {
            std::env::remove_var("CLEANROOM_ENABLE_DETERMINISTIC_EXECUTION");
            std::env::remove_var("CLEANROOM_DETERMINISTIC_SEED");
        }
    }

    #[test]
    fn test_config_validation() {
        let mut config = CleanroomConfig::new();
        config.container_startup_timeout = Duration::from_secs(0);

        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_merge() {
        let mut config1 = CleanroomConfig::new();
        config1.enable_deterministic_execution = true;

        let mut config2 = CleanroomConfig::new();
        config2.enable_deterministic_execution = false;
        config2.deterministic_seed = Some(12345);

        config1.merge(&config2);

        assert_eq!(config1.enable_deterministic_execution, false);
        assert_eq!(config1.deterministic_seed, Some(12345));
    }

    #[test]
    fn test_config_summary() {
        let config = CleanroomConfig::new();
        let summary = config.summary();

        assert!(summary.contains("Cleanroom Configuration Summary"));
        assert!(summary.contains("Singleton Containers"));
        assert!(summary.contains("Deterministic Execution"));
    }
}