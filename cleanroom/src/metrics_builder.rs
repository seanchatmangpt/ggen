//! ContainerMetrics builder for reducing repetition
//!
//! This module provides a builder pattern for ContainerMetrics to eliminate
//! repetitive construction patterns across container implementations.

use crate::cleanroom::ContainerMetrics;
use std::time::Instant;

/// Builder for ContainerMetrics
///
/// This builder eliminates the repetitive ContainerMetrics construction
/// patterns across different container types.
///
/// # Example
///
/// ```rust
/// let metrics = ContainerMetricsBuilder::new()
///     .cpu_usage(5.0)
///     .memory_mb(128)
///     .disk_mb(64)
///     .uptime_from_start(&start_time)
///     .build();
/// ```
#[derive(Debug, Default)]
pub struct ContainerMetricsBuilder {
    cpu_usage_percent: f64,
    memory_usage_bytes: u64,
    network_bytes_sent: u64,
    network_bytes_received: u64,
    disk_usage_bytes: u64,
    uptime_seconds: u64,
}

impl ContainerMetricsBuilder {
    /// Create a new ContainerMetricsBuilder
    pub fn new() -> Self {
        Self::default()
    }

    /// Set CPU usage percentage
    pub fn cpu_usage(mut self, cpu_usage: f64) -> Self {
        self.cpu_usage_percent = cpu_usage;
        self
    }

    /// Set memory usage in megabytes
    pub fn memory_mb(mut self, memory_mb: u64) -> Self {
        self.memory_usage_bytes = memory_mb * 1024 * 1024;
        self
    }

    /// Set memory usage in bytes
    pub fn memory_bytes(mut self, memory_bytes: u64) -> Self {
        self.memory_usage_bytes = memory_bytes;
        self
    }

    /// Set network bytes sent
    pub fn network_sent(mut self, bytes: u64) -> Self {
        self.network_bytes_sent = bytes;
        self
    }

    /// Set network bytes received
    pub fn network_received(mut self, bytes: u64) -> Self {
        self.network_bytes_received = bytes;
        self
    }

    /// Set disk usage in megabytes
    pub fn disk_mb(mut self, disk_mb: u64) -> Self {
        self.disk_usage_bytes = disk_mb * 1024 * 1024;
        self
    }

    /// Set disk usage in bytes
    pub fn disk_bytes(mut self, disk_bytes: u64) -> Self {
        self.disk_usage_bytes = disk_bytes;
        self
    }

    /// Set uptime from start time
    pub fn uptime_from_start(mut self, start_time: &Instant) -> Self {
        self.uptime_seconds = start_time.elapsed().as_secs();
        self
    }

    /// Set uptime in seconds
    pub fn uptime_seconds(mut self, seconds: u64) -> Self {
        self.uptime_seconds = seconds;
        self
    }

    /// Build the ContainerMetrics
    pub fn build(self) -> ContainerMetrics {
        ContainerMetrics {
            cpu_usage_percent: self.cpu_usage_percent,
            memory_usage_bytes: self.memory_usage_bytes,
            network_bytes_sent: self.network_bytes_sent,
            network_bytes_received: self.network_bytes_received,
            disk_usage_bytes: self.disk_usage_bytes,
            uptime_seconds: self.uptime_seconds,
        }
    }
}

/// Convenience methods for common container types
impl ContainerMetricsBuilder {
    /// Create metrics for PostgreSQL container
    pub fn postgres(start_time: &Instant) -> ContainerMetrics {
        Self::new()
            .cpu_usage(5.0)
            .memory_mb(128)
            .disk_mb(64)
            .uptime_from_start(start_time)
            .build()
    }

    /// Create metrics for Redis container
    pub fn redis(start_time: &Instant) -> ContainerMetrics {
        Self::new()
            .cpu_usage(2.0)
            .memory_mb(64)
            .disk_mb(32)
            .uptime_from_start(start_time)
            .build()
    }

    /// Create metrics for generic container
    pub fn generic(start_time: &Instant) -> ContainerMetrics {
        Self::new()
            .cpu_usage(10.0)
            .memory_mb(256)
            .disk_mb(128)
            .uptime_from_start(start_time)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_metrics_builder() {
        let start_time = Instant::now();
        std::thread::sleep(std::time::Duration::from_millis(10));

        let metrics = ContainerMetricsBuilder::new()
            .cpu_usage(5.0)
            .memory_mb(128)
            .disk_mb(64)
            .uptime_from_start(&start_time)
            .build();

        assert_eq!(metrics.cpu_usage_percent, 5.0);
        assert_eq!(metrics.memory_usage_bytes, 128 * 1024 * 1024);
        assert_eq!(metrics.disk_usage_bytes, 64 * 1024 * 1024);
        assert!(metrics.uptime_seconds > 0);
    }

    #[test]
    fn test_convenience_methods() {
        let start_time = Instant::now();

        let postgres_metrics = ContainerMetricsBuilder::postgres(&start_time);
        assert_eq!(postgres_metrics.cpu_usage_percent, 5.0);
        assert_eq!(postgres_metrics.memory_usage_bytes, 128 * 1024 * 1024);

        let redis_metrics = ContainerMetricsBuilder::redis(&start_time);
        assert_eq!(redis_metrics.cpu_usage_percent, 2.0);
        assert_eq!(redis_metrics.memory_usage_bytes, 64 * 1024 * 1024);

        let generic_metrics = ContainerMetricsBuilder::generic(&start_time);
        assert_eq!(generic_metrics.cpu_usage_percent, 10.0);
        assert_eq!(generic_metrics.memory_usage_bytes, 256 * 1024 * 1024);
    }
}
