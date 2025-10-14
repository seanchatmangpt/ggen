//! Resource limits for cleanroom testing
//!
//! This module provides resource limits following core team best practices:
//! - CPU limits
//! - Memory limits
//! - Disk limits
//! - Network limits
//! - Process limits

use crate::error::{CleanroomError, Result};
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Resource limits configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// CPU limits
    pub cpu: CpuLimits,
    /// Memory limits
    pub memory: MemoryLimits,
    /// Disk limits
    pub disk: DiskLimits,
    /// Network limits
    pub network: NetworkLimits,
    /// Process limits
    pub process: ProcessLimits,
    /// Time limits
    pub time: TimeLimits,
}

/// CPU limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CpuLimits {
    /// Maximum CPU usage percentage
    pub max_usage_percent: f64,
    /// Maximum CPU cores
    pub max_cores: u32,
    /// CPU quota (microseconds per second)
    pub quota_us: u64,
    /// CPU period (microseconds)
    pub period_us: u64,
    /// Enable CPU throttling
    pub enable_throttling: bool,
}

/// Memory limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryLimits {
    /// Maximum memory usage bytes
    pub max_usage_bytes: u64,
    /// Memory soft limit bytes
    pub soft_limit_bytes: u64,
    /// Memory hard limit bytes
    pub hard_limit_bytes: u64,
    /// Memory swap limit bytes
    pub swap_limit_bytes: u64,
    /// Enable memory monitoring
    pub enable_monitoring: bool,
    /// Memory pressure threshold
    pub pressure_threshold_percent: f64,
}

/// Disk limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiskLimits {
    /// Maximum disk usage bytes
    pub max_usage_bytes: u64,
    /// Disk read limit bytes per second
    pub read_limit_bytes_per_sec: u64,
    /// Disk write limit bytes per second
    pub write_limit_bytes_per_sec: u64,
    /// Disk IOPS limit
    pub iops_limit: u64,
    /// Enable disk monitoring
    pub enable_monitoring: bool,
    /// Disk pressure threshold
    pub pressure_threshold_percent: f64,
}

/// Network limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkLimits {
    /// Maximum bandwidth bytes per second
    pub max_bandwidth_bytes_per_sec: u64,
    /// Maximum connections
    pub max_connections: u32,
    /// Maximum ports
    pub max_ports: u32,
    /// Enable network monitoring
    pub enable_monitoring: bool,
    /// Network pressure threshold
    pub pressure_threshold_percent: f64,
}

/// Process limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessLimits {
    /// Maximum processes
    pub max_processes: u32,
    /// Maximum threads
    pub max_threads: u32,
    /// Maximum file descriptors
    pub max_file_descriptors: u32,
    /// Maximum open files
    pub max_open_files: u32,
    /// Enable process monitoring
    pub enable_monitoring: bool,
}

/// Time limits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeLimits {
    /// Maximum execution time
    pub max_execution_time: Duration,
    /// Maximum idle time
    pub max_idle_time: Duration,
    /// Maximum wait time
    pub max_wait_time: Duration,
    /// Enable time monitoring
    pub enable_monitoring: bool,
}

impl Default for CpuLimits {
    fn default() -> Self {
        Self {
            max_usage_percent: 80.0,
            max_cores: 4,
            quota_us: 800000,   // 80% of 1 second
            period_us: 1000000, // 1 second
            enable_throttling: true,
        }
    }
}

impl Default for MemoryLimits {
    fn default() -> Self {
        Self {
            max_usage_bytes: 1024 * 1024 * 1024,  // 1GB
            soft_limit_bytes: 512 * 1024 * 1024,  // 512MB
            hard_limit_bytes: 1024 * 1024 * 1024, // 1GB
            swap_limit_bytes: 0,                  // No swap
            enable_monitoring: true,
            pressure_threshold_percent: 80.0,
        }
    }
}

impl Default for DiskLimits {
    fn default() -> Self {
        Self {
            max_usage_bytes: 10 * 1024 * 1024 * 1024,     // 10GB
            read_limit_bytes_per_sec: 100 * 1024 * 1024,  // 100MB/s
            write_limit_bytes_per_sec: 100 * 1024 * 1024, // 100MB/s
            iops_limit: 1000,
            enable_monitoring: true,
            pressure_threshold_percent: 80.0,
        }
    }
}

impl Default for NetworkLimits {
    fn default() -> Self {
        Self {
            max_bandwidth_bytes_per_sec: 100 * 1024 * 1024, // 100MB/s
            max_connections: 1000,
            max_ports: 100,
            enable_monitoring: true,
            pressure_threshold_percent: 80.0,
        }
    }
}

impl Default for ProcessLimits {
    fn default() -> Self {
        Self {
            max_processes: 100,
            max_threads: 1000,
            max_file_descriptors: 1024,
            max_open_files: 1024,
            enable_monitoring: true,
        }
    }
}

impl Default for TimeLimits {
    fn default() -> Self {
        Self {
            max_execution_time: Duration::from_secs(300), // 5 minutes
            max_idle_time: Duration::from_secs(60),       // 1 minute
            max_wait_time: Duration::from_secs(30),       // 30 seconds
            enable_monitoring: true,
        }
    }
}

impl ResourceLimits {
    /// Create new resource limits with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Create resource limits with custom CPU limits
    pub fn with_cpu_limits(max_usage_percent: f64, max_cores: u32) -> Self {
        let mut limits = Self::default();
        limits.cpu.max_usage_percent = max_usage_percent;
        limits.cpu.max_cores = max_cores;
        limits
    }

    /// Create resource limits with custom memory limits
    pub fn with_memory_limits(max_usage_bytes: u64) -> Self {
        let mut limits = Self::default();
        limits.memory.max_usage_bytes = max_usage_bytes;
        limits.memory.hard_limit_bytes = max_usage_bytes;
        limits
    }

    /// Create resource limits with custom disk limits
    pub fn with_disk_limits(max_usage_bytes: u64) -> Self {
        let mut limits = Self::default();
        limits.disk.max_usage_bytes = max_usage_bytes;
        limits
    }

    /// Create resource limits with custom network limits
    pub fn with_network_limits(max_bandwidth_bytes_per_sec: u64) -> Self {
        let mut limits = Self::default();
        limits.network.max_bandwidth_bytes_per_sec = max_bandwidth_bytes_per_sec;
        limits
    }

    /// Create resource limits with custom time limits
    pub fn with_time_limits(max_execution_time: Duration) -> Self {
        let mut limits = Self::default();
        limits.time.max_execution_time = max_execution_time;
        limits
    }

    /// Validate resource limits
    pub fn validate(&self) -> Result<()> {
        // Validate CPU limits
        if self.cpu.max_usage_percent <= 0.0 || self.cpu.max_usage_percent > 100.0 {
            return Err(CleanroomError::validation_error(
                "Invalid CPU usage percentage",
            ));
        }

        if self.cpu.max_cores == 0 {
            return Err(CleanroomError::validation_error("Invalid CPU core count"));
        }

        // Validate memory limits
        if self.memory.max_usage_bytes == 0 {
            return Err(CleanroomError::validation_error(
                "Invalid memory usage limit",
            ));
        }

        if self.memory.soft_limit_bytes > self.memory.hard_limit_bytes {
            return Err(CleanroomError::validation_error(
                "Soft limit cannot exceed hard limit",
            ));
        }

        // Validate disk limits
        if self.disk.max_usage_bytes == 0 {
            return Err(CleanroomError::validation_error("Invalid disk usage limit"));
        }

        // Validate network limits
        if self.network.max_bandwidth_bytes_per_sec == 0 {
            return Err(CleanroomError::validation_error(
                "Invalid network bandwidth limit",
            ));
        }

        if self.network.max_connections == 0 {
            return Err(CleanroomError::validation_error("Invalid connection limit"));
        }

        // Validate process limits
        if self.process.max_processes == 0 {
            return Err(CleanroomError::validation_error("Invalid process limit"));
        }

        if self.process.max_threads == 0 {
            return Err(CleanroomError::validation_error("Invalid thread limit"));
        }

        // Validate time limits
        if self.time.max_execution_time.as_secs() == 0 {
            return Err(CleanroomError::validation_error(
                "Invalid execution time limit",
            ));
        }

        Ok(())
    }

    /// Check if CPU usage is within limits
    pub fn check_cpu_usage(&self, usage_percent: f64) -> Result<()> {
        if usage_percent > self.cpu.max_usage_percent {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "CPU usage {}% exceeds limit {}%",
                usage_percent, self.cpu.max_usage_percent
            )))
        } else {
            Ok(())
        }
    }

    /// Check if memory usage is within limits
    pub fn check_memory_usage(&self, usage_bytes: u64) -> Result<()> {
        if usage_bytes > self.memory.max_usage_bytes {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "Memory usage {} bytes exceeds limit {} bytes",
                usage_bytes, self.memory.max_usage_bytes
            )))
        } else {
            Ok(())
        }
    }

    /// Check if disk usage is within limits
    pub fn check_disk_usage(&self, usage_bytes: u64) -> Result<()> {
        if usage_bytes > self.disk.max_usage_bytes {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "Disk usage {} bytes exceeds limit {} bytes",
                usage_bytes, self.disk.max_usage_bytes
            )))
        } else {
            Ok(())
        }
    }

    /// Check if network usage is within limits
    pub fn check_network_usage(&self, bandwidth_bytes_per_sec: u64) -> Result<()> {
        if bandwidth_bytes_per_sec > self.network.max_bandwidth_bytes_per_sec {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "Network bandwidth {} bytes/sec exceeds limit {} bytes/sec",
                bandwidth_bytes_per_sec, self.network.max_bandwidth_bytes_per_sec
            )))
        } else {
            Ok(())
        }
    }

    /// Check if process count is within limits
    pub fn check_process_count(&self, process_count: u32) -> Result<()> {
        if process_count > self.process.max_processes {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "Process count {} exceeds limit {}",
                process_count, self.process.max_processes
            )))
        } else {
            Ok(())
        }
    }

    /// Check if execution time is within limits
    pub fn check_execution_time(&self, execution_time: Duration) -> Result<()> {
        if execution_time > self.time.max_execution_time {
            Err(CleanroomError::resource_limit_exceeded(format!(
                "Execution time {:?} exceeds limit {:?}",
                execution_time, self.time.max_execution_time
            )))
        } else {
            Ok(())
        }
    }

    /// Get resource limits summary
    pub fn summary(&self) -> String {
        format!(
            "Resource Limits Summary:\n\
            CPU: {}% max usage, {} cores\n\
            Memory: {} bytes max usage\n\
            Disk: {} bytes max usage\n\
            Network: {} bytes/sec max bandwidth\n\
            Process: {} max processes, {} max threads\n\
            Time: {:?} max execution time",
            self.cpu.max_usage_percent,
            self.cpu.max_cores,
            self.memory.max_usage_bytes,
            self.disk.max_usage_bytes,
            self.network.max_bandwidth_bytes_per_sec,
            self.process.max_processes,
            self.process.max_threads,
            self.time.max_execution_time
        )
    }

    /// Get environment variables for resource limits
    pub fn to_env(&self) -> std::collections::HashMap<String, String> {
        let mut env = std::collections::HashMap::new();

        // CPU limits
        env.insert(
            "CLEANROOM_CPU_MAX_USAGE_PERCENT".to_string(),
            self.cpu.max_usage_percent.to_string(),
        );
        env.insert(
            "CLEANROOM_CPU_MAX_CORES".to_string(),
            self.cpu.max_cores.to_string(),
        );
        env.insert(
            "CLEANROOM_CPU_QUOTA_US".to_string(),
            self.cpu.quota_us.to_string(),
        );
        env.insert(
            "CLEANROOM_CPU_PERIOD_US".to_string(),
            self.cpu.period_us.to_string(),
        );
        env.insert(
            "CLEANROOM_CPU_ENABLE_THROTTLING".to_string(),
            self.cpu.enable_throttling.to_string(),
        );

        // Memory limits
        env.insert(
            "CLEANROOM_MEMORY_MAX_USAGE_BYTES".to_string(),
            self.memory.max_usage_bytes.to_string(),
        );
        env.insert(
            "CLEANROOM_MEMORY_SOFT_LIMIT_BYTES".to_string(),
            self.memory.soft_limit_bytes.to_string(),
        );
        env.insert(
            "CLEANROOM_MEMORY_HARD_LIMIT_BYTES".to_string(),
            self.memory.hard_limit_bytes.to_string(),
        );
        env.insert(
            "CLEANROOM_MEMORY_SWAP_LIMIT_BYTES".to_string(),
            self.memory.swap_limit_bytes.to_string(),
        );
        env.insert(
            "CLEANROOM_MEMORY_ENABLE_MONITORING".to_string(),
            self.memory.enable_monitoring.to_string(),
        );

        // Disk limits
        env.insert(
            "CLEANROOM_DISK_MAX_USAGE_BYTES".to_string(),
            self.disk.max_usage_bytes.to_string(),
        );
        env.insert(
            "CLEANROOM_DISK_READ_LIMIT_BYTES_PER_SEC".to_string(),
            self.disk.read_limit_bytes_per_sec.to_string(),
        );
        env.insert(
            "CLEANROOM_DISK_WRITE_LIMIT_BYTES_PER_SEC".to_string(),
            self.disk.write_limit_bytes_per_sec.to_string(),
        );
        env.insert(
            "CLEANROOM_DISK_IOPS_LIMIT".to_string(),
            self.disk.iops_limit.to_string(),
        );
        env.insert(
            "CLEANROOM_DISK_ENABLE_MONITORING".to_string(),
            self.disk.enable_monitoring.to_string(),
        );

        // Network limits
        env.insert(
            "CLEANROOM_NETWORK_MAX_BANDWIDTH_BYTES_PER_SEC".to_string(),
            self.network.max_bandwidth_bytes_per_sec.to_string(),
        );
        env.insert(
            "CLEANROOM_NETWORK_MAX_CONNECTIONS".to_string(),
            self.network.max_connections.to_string(),
        );
        env.insert(
            "CLEANROOM_NETWORK_MAX_PORTS".to_string(),
            self.network.max_ports.to_string(),
        );
        env.insert(
            "CLEANROOM_NETWORK_ENABLE_MONITORING".to_string(),
            self.network.enable_monitoring.to_string(),
        );

        // Process limits
        env.insert(
            "CLEANROOM_PROCESS_MAX_PROCESSES".to_string(),
            self.process.max_processes.to_string(),
        );
        env.insert(
            "CLEANROOM_PROCESS_MAX_THREADS".to_string(),
            self.process.max_threads.to_string(),
        );
        env.insert(
            "CLEANROOM_PROCESS_MAX_FILE_DESCRIPTORS".to_string(),
            self.process.max_file_descriptors.to_string(),
        );
        env.insert(
            "CLEANROOM_PROCESS_MAX_OPEN_FILES".to_string(),
            self.process.max_open_files.to_string(),
        );
        env.insert(
            "CLEANROOM_PROCESS_ENABLE_MONITORING".to_string(),
            self.process.enable_monitoring.to_string(),
        );

        // Time limits
        env.insert(
            "CLEANROOM_TIME_MAX_EXECUTION_TIME_SECS".to_string(),
            self.time.max_execution_time.as_secs().to_string(),
        );
        env.insert(
            "CLEANROOM_TIME_MAX_IDLE_TIME_SECS".to_string(),
            self.time.max_idle_time.as_secs().to_string(),
        );
        env.insert(
            "CLEANROOM_TIME_MAX_WAIT_TIME_SECS".to_string(),
            self.time.max_wait_time.as_secs().to_string(),
        );
        env.insert(
            "CLEANROOM_TIME_ENABLE_MONITORING".to_string(),
            self.time.enable_monitoring.to_string(),
        );

        env
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resource_limits_creation() {
        let limits = ResourceLimits::new();
        assert!(limits.validate().is_ok());
    }

    #[test]
    fn test_resource_limits_with_cpu() {
        let limits = ResourceLimits::with_cpu_limits(50.0, 2);
        assert_eq!(limits.cpu.max_usage_percent, 50.0);
        assert_eq!(limits.cpu.max_cores, 2);
    }

    #[test]
    fn test_resource_limits_with_memory() {
        let limits = ResourceLimits::with_memory_limits(512 * 1024 * 1024);
        assert_eq!(limits.memory.max_usage_bytes, 512 * 1024 * 1024);
        assert_eq!(limits.memory.hard_limit_bytes, 512 * 1024 * 1024);
    }

    #[test]
    fn test_resource_limits_with_disk() {
        let limits = ResourceLimits::with_disk_limits(5 * 1024 * 1024 * 1024);
        assert_eq!(limits.disk.max_usage_bytes, 5 * 1024 * 1024 * 1024);
    }

    #[test]
    fn test_resource_limits_with_network() {
        let limits = ResourceLimits::with_network_limits(50 * 1024 * 1024);
        assert_eq!(limits.network.max_bandwidth_bytes_per_sec, 50 * 1024 * 1024);
    }

    #[test]
    fn test_resource_limits_with_time() {
        let limits = ResourceLimits::with_time_limits(Duration::from_secs(120));
        assert_eq!(limits.time.max_execution_time, Duration::from_secs(120));
    }

    #[test]
    fn test_resource_limits_validation() {
        let limits = ResourceLimits::new();
        assert!(limits.validate().is_ok());
    }

    #[test]
    fn test_cpu_usage_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_cpu_usage(50.0).is_ok());
        assert!(limits.check_cpu_usage(90.0).is_err());
    }

    #[test]
    fn test_memory_usage_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_memory_usage(512 * 1024 * 1024).is_ok());
        assert!(limits.check_memory_usage(2 * 1024 * 1024 * 1024).is_err());
    }

    #[test]
    fn test_disk_usage_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_disk_usage(5 * 1024 * 1024 * 1024).is_ok());
        assert!(limits.check_disk_usage(20 * 1024 * 1024 * 1024).is_err());
    }

    #[test]
    fn test_network_usage_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_network_usage(50 * 1024 * 1024).is_ok());
        assert!(limits.check_network_usage(200 * 1024 * 1024).is_err());
    }

    #[test]
    fn test_process_count_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_process_count(50).is_ok());
        assert!(limits.check_process_count(200).is_err());
    }

    #[test]
    fn test_execution_time_check() {
        let limits = ResourceLimits::new();
        assert!(limits.check_execution_time(Duration::from_secs(60)).is_ok());
        assert!(limits
            .check_execution_time(Duration::from_secs(600))
            .is_err());
    }

    #[test]
    fn test_resource_limits_summary() {
        let limits = ResourceLimits::new();
        let summary = limits.summary();
        assert!(summary.contains("Resource Limits Summary"));
        assert!(summary.contains("CPU"));
        assert!(summary.contains("Memory"));
        assert!(summary.contains("Disk"));
        assert!(summary.contains("Network"));
        assert!(summary.contains("Process"));
        assert!(summary.contains("Time"));
    }

    #[test]
    fn test_resource_limits_env() {
        let limits = ResourceLimits::new();
        let env = limits.to_env();

        assert!(env.contains_key("CLEANROOM_CPU_MAX_USAGE_PERCENT"));
        assert!(env.contains_key("CLEANROOM_MEMORY_MAX_USAGE_BYTES"));
        assert!(env.contains_key("CLEANROOM_DISK_MAX_USAGE_BYTES"));
        assert!(env.contains_key("CLEANROOM_NETWORK_MAX_BANDWIDTH_BYTES_PER_SEC"));
        assert!(env.contains_key("CLEANROOM_PROCESS_MAX_PROCESSES"));
        assert!(env.contains_key("CLEANROOM_TIME_MAX_EXECUTION_TIME_SECS"));
    }
}
