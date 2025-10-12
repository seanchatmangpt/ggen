//! Fixed resource limits for cleanroom testing framework
//!
//! This module provides resource limit management following core team best practices:
//! - Memory limits and monitoring
//! - CPU usage constraints
//! - Disk space management
//! - Network bandwidth controls
//! - Container resource allocation

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Resource limits configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// Maximum memory usage in MB
    pub max_memory_mb: usize,
    /// Maximum CPU usage percentage
    pub max_cpu_percent: f64,
    /// Maximum disk usage in MB
    pub max_disk_mb: usize,
    /// Maximum network usage in MB
    pub max_network_mb: usize,
    /// Maximum container count
    pub max_container_count: u32,
    /// Maximum test execution time
    pub max_test_execution_time: Duration,
    /// Enable resource monitoring
    pub enable_resource_monitoring: bool,
    /// Resource cleanup timeout
    pub resource_cleanup_timeout: Duration,
    /// Memory warning threshold percentage
    pub memory_warning_threshold: f64,
    /// CPU warning threshold percentage
    pub cpu_warning_threshold: f64,
    /// Disk warning threshold percentage
    pub disk_warning_threshold: f64,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            max_memory_mb: 1024,
            max_cpu_percent: 100.0,
            max_disk_mb: 2048,
            max_network_mb: 512,
            max_container_count: 10,
            max_test_execution_time: Duration::from_secs(300),
            enable_resource_monitoring: true,
            resource_cleanup_timeout: Duration::from_secs(30),
            memory_warning_threshold: 80.0,
            cpu_warning_threshold: 80.0,
            disk_warning_threshold: 80.0,
        }
    }
}

impl ResourceLimits {
    /// Create a new resource limits configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set maximum memory limit in MB
    pub fn with_max_memory_mb(mut self, memory_mb: usize) -> Self {
        self.max_memory_mb = memory_mb;
        self
    }

    /// Set maximum CPU usage percentage
    pub fn with_max_cpu_percent(mut self, cpu_percent: f64) -> Self {
        self.max_cpu_percent = cpu_percent;
        self
    }

    /// Set maximum disk usage in MB
    pub fn with_max_disk_mb(mut self, disk_mb: usize) -> Self {
        self.max_disk_mb = disk_mb;
        self
    }

    /// Set maximum network usage in MB
    pub fn with_max_network_mb(mut self, network_mb: usize) -> Self {
        self.max_network_mb = network_mb;
        self
    }

    /// Set maximum container count
    pub fn with_max_container_count(mut self, container_count: u32) -> Self {
        self.max_container_count = container_count;
        self
    }

    /// Set maximum test execution time
    pub fn with_max_test_execution_time(mut self, duration: Duration) -> Self {
        self.max_test_execution_time = duration;
        self
    }

    /// Enable or disable resource monitoring
    pub fn with_resource_monitoring(mut self, enable: bool) -> Self {
        self.enable_resource_monitoring = enable;
        self
    }

    /// Set resource cleanup timeout
    pub fn with_resource_cleanup_timeout(mut self, timeout: Duration) -> Self {
        self.resource_cleanup_timeout = timeout;
        self
    }

    /// Set memory warning threshold percentage
    pub fn with_memory_warning_threshold(mut self, threshold: f64) -> Self {
        self.memory_warning_threshold = threshold;
        self
    }

    /// Set CPU warning threshold percentage
    pub fn with_cpu_warning_threshold(mut self, threshold: f64) -> Self {
        self.cpu_warning_threshold = threshold;
        self
    }

    /// Set disk warning threshold percentage
    pub fn with_disk_warning_threshold(mut self, threshold: f64) -> Self {
        self.disk_warning_threshold = threshold;
        self
    }

    /// Validate the resource limits configuration
    pub fn validate(&self) -> Result<()> {
        if self.max_memory_mb == 0 {
            return Err(CleanroomError::resource_error(
                "Maximum memory limit must be greater than 0"
            ));
        }

        if self.max_cpu_percent <= 0.0 || self.max_cpu_percent > 100.0 {
            return Err(CleanroomError::resource_error(
                "CPU usage percentage must be between 0 and 100"
            ));
        }

        if self.max_disk_mb == 0 {
            return Err(CleanroomError::resource_error(
                "Maximum disk limit must be greater than 0"
            ));
        }

        if self.max_network_mb == 0 {
            return Err(CleanroomError::resource_error(
                "Maximum network limit must be greater than 0"
            ));
        }

        if self.max_container_count == 0 {
            return Err(CleanroomError::resource_error(
                "Maximum container count must be greater than 0"
            ));
        }

        if self.max_test_execution_time.as_secs() == 0 {
            return Err(CleanroomError::resource_error(
                "Maximum test execution time must be greater than 0"
            ));
        }

        if self.resource_cleanup_timeout.as_secs() == 0 {
            return Err(CleanroomError::resource_error(
                "Resource cleanup timeout must be greater than 0"
            ));
        }

        if self.memory_warning_threshold <= 0.0 || self.memory_warning_threshold > 100.0 {
            return Err(CleanroomError::resource_error(
                "Memory warning threshold must be between 0 and 100"
            ));
        }

        if self.cpu_warning_threshold <= 0.0 || self.cpu_warning_threshold > 100.0 {
            return Err(CleanroomError::resource_error(
                "CPU warning threshold must be between 0 and 100"
            ));
        }

        if self.disk_warning_threshold <= 0.0 || self.disk_warning_threshold > 100.0 {
            return Err(CleanroomError::resource_error(
                "Disk warning threshold must be between 0 and 100"
            ));
        }

        Ok(())
    }

    /// Check if memory usage is within limits
    pub fn is_memory_within_limits(&self, current_usage_mb: usize) -> bool {
        current_usage_mb <= self.max_memory_mb
    }

    /// Check if CPU usage is within limits
    pub fn is_cpu_within_limits(&self, current_usage_percent: f64) -> bool {
        current_usage_percent <= self.max_cpu_percent
    }

    /// Check if disk usage is within limits
    pub fn is_disk_within_limits(&self, current_usage_mb: usize) -> bool {
        current_usage_mb <= self.max_disk_mb
    }

    /// Check if network usage is within limits
    pub fn is_network_within_limits(&self, current_usage_mb: usize) -> bool {
        current_usage_mb <= self.max_network_mb
    }

    /// Check if container count is within limits
    pub fn is_container_count_within_limits(&self, current_count: u32) -> bool {
        current_count <= self.max_container_count
    }

    /// Check if test execution time is within limits
    pub fn is_execution_time_within_limits(&self, current_duration: Duration) -> bool {
        current_duration <= self.max_test_execution_time
    }

    /// Get memory usage percentage
    pub fn get_memory_usage_percentage(&self, current_usage_mb: usize) -> f64 {
        (current_usage_mb as f64 / self.max_memory_mb as f64) * 100.0
    }

    /// Get CPU usage percentage (already in percentage)
    pub fn get_cpu_usage_percentage(&self, current_usage_percent: f64) -> f64 {
        (current_usage_percent / self.max_cpu_percent) * 100.0
    }

    /// Get disk usage percentage
    pub fn get_disk_usage_percentage(&self, current_usage_mb: usize) -> f64 {
        (current_usage_mb as f64 / self.max_disk_mb as f64) * 100.0
    }

    /// Get network usage percentage
    pub fn get_network_usage_percentage(&self, current_usage_mb: usize) -> f64 {
        (current_usage_mb as f64 / self.max_network_mb as f64) * 100.0
    }

    /// Get container count percentage
    pub fn get_container_count_percentage(&self, current_count: u32) -> f64 {
        (current_count as f64 / self.max_container_count as f64) * 100.0
    }

    /// Check if memory usage is above warning threshold
    pub fn is_memory_warning(&self, current_usage_mb: usize) -> bool {
        self.get_memory_usage_percentage(current_usage_mb) >= self.memory_warning_threshold
    }

    /// Check if CPU usage is above warning threshold
    pub fn is_cpu_warning(&self, current_usage_percent: f64) -> bool {
        self.get_cpu_usage_percentage(current_usage_percent) >= self.cpu_warning_threshold
    }

    /// Check if disk usage is above warning threshold
    pub fn is_disk_warning(&self, current_usage_mb: usize) -> bool {
        self.get_disk_usage_percentage(current_usage_mb) >= self.disk_warning_threshold
    }

    /// Get a summary of the resource limits
    pub fn summary(&self) -> String {
        format!(
            "Resource Limits Summary:\n\
            Memory: {} MB ({}% warning threshold)\n\
            CPU: {}% ({}% warning threshold)\n\
            Disk: {} MB ({}% warning threshold)\n\
            Network: {} MB\n\
            Containers: {} max\n\
            Test Execution Time: {:?}\n\
            Resource Monitoring: {}\n\
            Cleanup Timeout: {:?}",
            self.max_memory_mb,
            self.memory_warning_threshold,
            self.max_cpu_percent,
            self.cpu_warning_threshold,
            self.max_disk_mb,
            self.disk_warning_threshold,
            self.max_network_mb,
            self.max_container_count,
            self.max_test_execution_time,
            self.enable_resource_monitoring,
            self.resource_cleanup_timeout
        )
    }

    /// Get resource usage statistics
    pub fn get_usage_stats(
        &self,
        current_memory_mb: usize,
        current_cpu_percent: f64,
        current_disk_mb: usize,
        current_network_mb: usize,
        current_container_count: u32,
    ) -> ResourceUsageStats {
        ResourceUsageStats {
            memory_usage_percentage: self.get_memory_usage_percentage(current_memory_mb),
            cpu_usage_percentage: self.get_cpu_usage_percentage(current_cpu_percent),
            disk_usage_percentage: self.get_disk_usage_percentage(current_disk_mb),
            network_usage_percentage: self.get_network_usage_percentage(current_network_mb),
            container_count_percentage: self.get_container_count_percentage(current_container_count),
            memory_warning: self.is_memory_warning(current_memory_mb),
            cpu_warning: self.is_cpu_warning(current_cpu_percent),
            disk_warning: self.is_disk_warning(current_disk_mb),
            memory_within_limits: self.is_memory_within_limits(current_memory_mb),
            cpu_within_limits: self.is_cpu_within_limits(current_cpu_percent),
            disk_within_limits: self.is_disk_within_limits(current_disk_mb),
            network_within_limits: self.is_network_within_limits(current_network_mb),
            container_count_within_limits: self.is_container_count_within_limits(current_container_count),
        }
    }
}

/// Resource usage statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsageStats {
    /// Memory usage percentage
    pub memory_usage_percentage: f64,
    /// CPU usage percentage
    pub cpu_usage_percentage: f64,
    /// Disk usage percentage
    pub disk_usage_percentage: f64,
    /// Network usage percentage
    pub network_usage_percentage: f64,
    /// Container count percentage
    pub container_count_percentage: f64,
    /// Memory warning flag
    pub memory_warning: bool,
    /// CPU warning flag
    pub cpu_warning: bool,
    /// Disk warning flag
    pub disk_warning: bool,
    /// Memory within limits flag
    pub memory_within_limits: bool,
    /// CPU within limits flag
    pub cpu_within_limits: bool,
    /// Disk within limits flag
    pub disk_within_limits: bool,
    /// Network within limits flag
    pub network_within_limits: bool,
    /// Container count within limits flag
    pub container_count_within_limits: bool,
}

impl ResourceUsageStats {
    /// Check if any resource is above warning threshold
    pub fn has_warnings(&self) -> bool {
        self.memory_warning || self.cpu_warning || self.disk_warning
    }

    /// Check if any resource is above limits
    pub fn has_limit_violations(&self) -> bool {
        !self.memory_within_limits || !self.cpu_within_limits || !self.disk_within_limits ||
        !self.network_within_limits || !self.container_count_within_limits
    }

    /// Get overall health status
    pub fn get_health_status(&self) -> ResourceHealthStatus {
        if self.has_limit_violations() {
            ResourceHealthStatus::Critical
        } else if self.has_warnings() {
            ResourceHealthStatus::Warning
        } else {
            ResourceHealthStatus::Healthy
        }
    }

    /// Get a summary of the usage statistics
    pub fn summary(&self) -> String {
        let health_status = self.get_health_status();
        format!(
            "Resource Usage Statistics:\n\
            Health Status: {:?}\n\
            Memory: {:.1}% (Warning: {})\n\
            CPU: {:.1}% (Warning: {})\n\
            Disk: {:.1}% (Warning: {})\n\
            Network: {:.1}%\n\
            Containers: {:.1}%\n\
            Within Limits: Memory={}, CPU={}, Disk={}, Network={}, Containers={}",
            health_status,
            self.memory_usage_percentage,
            self.memory_warning,
            self.cpu_usage_percentage,
            self.cpu_warning,
            self.disk_usage_percentage,
            self.disk_warning,
            self.network_usage_percentage,
            self.container_count_percentage,
            self.memory_within_limits,
            self.cpu_within_limits,
            self.disk_within_limits,
            self.network_within_limits,
            self.container_count_within_limits
        )
    }
}

/// Resource health status
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ResourceHealthStatus {
    /// All resources are healthy
    Healthy,
    /// Some resources are above warning threshold
    Warning,
    /// Some resources are above limits
    Critical,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resource_limits_creation() {
        let limits = ResourceLimits::new();
        assert_eq!(limits.max_memory_mb, 1024);
        assert_eq!(limits.max_cpu_percent, 100.0);
        assert_eq!(limits.max_disk_mb, 2048);
        assert_eq!(limits.max_network_mb, 512);
        assert_eq!(limits.max_container_count, 10);
    }

    #[test]
    fn test_resource_limits_builder_methods() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(512)
            .with_max_cpu_percent(50.0)
            .with_max_disk_mb(1024)
            .with_max_network_mb(256)
            .with_max_container_count(5);

        assert_eq!(limits.max_memory_mb, 512);
        assert_eq!(limits.max_cpu_percent, 50.0);
        assert_eq!(limits.max_disk_mb, 1024);
        assert_eq!(limits.max_network_mb, 256);
        assert_eq!(limits.max_container_count, 5);
    }

    #[test]
    fn test_resource_limits_validation() {
        let limits = ResourceLimits::new();
        assert!(limits.validate().is_ok());

        let invalid_limits = ResourceLimits::new()
            .with_max_memory_mb(0)
            .with_max_cpu_percent(-10.0);

        assert!(invalid_limits.validate().is_err());
    }

    #[test]
    fn test_resource_limits_within_limits() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(1024)
            .with_max_cpu_percent(80.0)
            .with_max_disk_mb(2048)
            .with_max_network_mb(512)
            .with_max_container_count(10);

        assert!(limits.is_memory_within_limits(512));
        assert!(!limits.is_memory_within_limits(2048));

        assert!(limits.is_cpu_within_limits(40.0));
        assert!(!limits.is_cpu_within_limits(90.0));

        assert!(limits.is_disk_within_limits(1024));
        assert!(!limits.is_disk_within_limits(4096));

        assert!(limits.is_network_within_limits(256));
        assert!(!limits.is_network_within_limits(1024));

        assert!(limits.is_container_count_within_limits(5));
        assert!(!limits.is_container_count_within_limits(15));
    }

    #[test]
    fn test_resource_limits_usage_percentages() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(1000)
            .with_max_cpu_percent(100.0)
            .with_max_disk_mb(2000)
            .with_max_network_mb(500)
            .with_max_container_count(10);

        assert_eq!(limits.get_memory_usage_percentage(500), 50.0);
        assert_eq!(limits.get_cpu_usage_percentage(50.0), 50.0);
        assert_eq!(limits.get_disk_usage_percentage(1000), 50.0);
        assert_eq!(limits.get_network_usage_percentage(250), 50.0);
        assert_eq!(limits.get_container_count_percentage(5), 50.0);
    }

    #[test]
    fn test_resource_limits_warnings() {
        let limits = ResourceLimits::new()
            .with_memory_warning_threshold(80.0)
            .with_cpu_warning_threshold(80.0)
            .with_disk_warning_threshold(80.0);

        // Test memory warnings
        assert!(!limits.is_memory_warning(400)); // 40% usage
        assert!(limits.is_memory_warning(900));  // 90% usage

        // Test CPU warnings
        assert!(!limits.is_cpu_warning(40.0)); // 40% usage
        assert!(limits.is_cpu_warning(90.0));  // 90% usage

        // Test disk warnings
        assert!(!limits.is_disk_warning(800)); // 40% usage
        assert!(limits.is_disk_warning(1800)); // 90% usage
    }

    #[test]
    fn test_resource_limits_summary() {
        let limits = ResourceLimits::new();
        let summary = limits.summary();

        assert!(summary.contains("Resource Limits Summary"));
        assert!(summary.contains("Memory: 1024 MB"));
        assert!(summary.contains("CPU: 100%"));
        assert!(summary.contains("Disk: 2048 MB"));
        assert!(summary.contains("Network: 512 MB"));
        assert!(summary.contains("Containers: 10 max"));
    }

    #[test]
    fn test_resource_usage_stats() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(1000)
            .with_max_cpu_percent(100.0)
            .with_max_disk_mb(2000)
            .with_max_network_mb(500)
            .with_max_container_count(10)
            .with_memory_warning_threshold(80.0)
            .with_cpu_warning_threshold(80.0)
            .with_disk_warning_threshold(80.0);

        let stats = limits.get_usage_stats(500, 50.0, 1000, 250, 5);

        assert_eq!(stats.memory_usage_percentage, 50.0);
        assert_eq!(stats.cpu_usage_percentage, 50.0);
        assert_eq!(stats.disk_usage_percentage, 50.0);
        assert_eq!(stats.network_usage_percentage, 50.0);
        assert_eq!(stats.container_count_percentage, 50.0);

        assert!(!stats.memory_warning);
        assert!(!stats.cpu_warning);
        assert!(!stats.disk_warning);

        assert!(stats.memory_within_limits);
        assert!(stats.cpu_within_limits);
        assert!(stats.disk_within_limits);
        assert!(stats.network_within_limits);
        assert!(stats.container_count_within_limits);

        assert_eq!(stats.get_health_status(), ResourceHealthStatus::Healthy);
    }

    #[test]
    fn test_resource_usage_stats_warnings() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(1000)
            .with_max_cpu_percent(100.0)
            .with_max_disk_mb(2000)
            .with_max_network_mb(500)
            .with_max_container_count(10)
            .with_memory_warning_threshold(80.0)
            .with_cpu_warning_threshold(80.0)
            .with_disk_warning_threshold(80.0);

        let stats = limits.get_usage_stats(900, 90.0, 1800, 250, 5);

        assert!(stats.memory_warning);
        assert!(stats.cpu_warning);
        assert!(stats.disk_warning);

        assert!(stats.has_warnings());
        assert!(!stats.has_limit_violations());

        assert_eq!(stats.get_health_status(), ResourceHealthStatus::Warning);
    }

    #[test]
    fn test_resource_usage_stats_critical() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(1000)
            .with_max_cpu_percent(100.0)
            .with_max_disk_mb(2000)
            .with_max_network_mb(500)
            .with_max_container_count(10);

        let stats = limits.get_usage_stats(1500, 150.0, 3000, 750, 15);

        assert!(!stats.memory_within_limits);
        assert!(!stats.cpu_within_limits);
        assert!(!stats.disk_within_limits);
        assert!(!stats.network_within_limits);
        assert!(!stats.container_count_within_limits);

        assert!(stats.has_limit_violations());

        assert_eq!(stats.get_health_status(), ResourceHealthStatus::Critical);
    }

    #[test]
    fn test_resource_usage_stats_summary() {
        let limits = ResourceLimits::new();
        let stats = limits.get_usage_stats(500, 50.0, 1000, 250, 5);
        let summary = stats.summary();

        assert!(summary.contains("Resource Usage Statistics"));
        assert!(summary.contains("Health Status: Healthy"));
        assert!(summary.contains("Memory: 50.0%"));
        assert!(summary.contains("CPU: 50.0%"));
        assert!(summary.contains("Disk: 50.0%"));
    }

    #[test]
    fn test_resource_limits_serialization() {
        let limits = ResourceLimits::new()
            .with_max_memory_mb(512)
            .with_max_cpu_percent(50.0)
            .with_max_disk_mb(1024);

        // Test JSON serialization
        let json = serde_json::to_string(&limits).unwrap();
        assert!(json.contains("max_memory_mb"));
        assert!(json.contains("max_cpu_percent"));
        assert!(json.contains("max_disk_mb"));

        // Test JSON deserialization
        let deserialized_limits: ResourceLimits = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized_limits.max_memory_mb, limits.max_memory_mb);
        assert_eq!(deserialized_limits.max_cpu_percent, limits.max_cpu_percent);
        assert_eq!(deserialized_limits.max_disk_mb, limits.max_disk_mb);
    }
}
