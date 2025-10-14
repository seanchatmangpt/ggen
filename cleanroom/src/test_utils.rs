//! Test utilities for reducing repetition in tests
//!
//! This module provides utilities to eliminate repetitive patterns in test
//! implementations following core team best practices.

use crate::cleanroom::CleanroomEnvironment;
use crate::config::CleanroomConfig;
use crate::error::Result;
use std::time::Duration;

/// Test environment builder
///
/// This builder eliminates repetitive test environment setup patterns.
///
/// # Example
///
/// ```rust
/// let env = TestEnvironmentBuilder::new()
///     .with_singleton_containers(true)
///     .with_timeout(Duration::from_secs(30))
///     .build()
///     .await?;
/// ```
#[derive(Debug)]
pub struct TestEnvironmentBuilder {
    config: CleanroomConfig,
}

impl TestEnvironmentBuilder {
    /// Create a new test environment builder
    pub fn new() -> Self {
        Self {
            config: CleanroomConfig::default(),
        }
    }

    /// Enable singleton containers
    pub fn with_singleton_containers(mut self, enabled: bool) -> Self {
        self.config.enable_singleton_containers = enabled;
        self
    }

    /// Set container startup timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.config.container_startup_timeout = timeout;
        self
    }

    /// Set maximum concurrent containers
    pub fn with_max_containers(mut self, max: u32) -> Self {
        self.config.max_concurrent_containers = max;
        self
    }

    /// Set test execution timeout
    pub fn with_test_timeout(mut self, timeout: Duration) -> Self {
        self.config.test_execution_timeout = timeout;
        self
    }

    /// Build the test environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl Default for TestEnvironmentBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Test container helper
///
/// This helper provides common container operations for tests.
pub struct TestContainerHelper;

impl TestContainerHelper {
    /// Wait for container to be ready with timeout
    pub async fn wait_for_ready<C>(container: &C, timeout: Duration) -> Result<()>
    where
        C: crate::container_base::BaseContainer,
    {
        container.wait_for_ready(timeout).await
    }

    /// Assert container is running
    pub async fn assert_running<C>(container: &C) -> Result<()>
    where
        C: crate::container_base::BaseContainer,
    {
        if !container.is_running().await {
            return Err(crate::error_helpers::container_error(
                "Container is not running",
                Some("Expected container to be in running state"),
            ));
        }
        Ok(())
    }

    /// Assert container is ready
    pub async fn assert_ready<C>(container: &C) -> Result<()>
    where
        C: crate::container_base::BaseContainer,
    {
        if !container.is_ready().await {
            return Err(crate::error_helpers::container_error(
                "Container is not ready",
                Some("Expected container to be in ready state"),
            ));
        }
        Ok(())
    }

    /// Get container metrics with timeout
    pub async fn get_metrics<C>(
        container: &C, timeout: Duration,
    ) -> Result<crate::cleanroom::ContainerMetrics>
    where
        C: crate::container_base::BaseContainer,
    {
        tokio::time::timeout(timeout, container.get_metrics())
            .await
            .map_err(|_| {
                crate::error_helpers::timeout_error(
                    "Failed to get container metrics",
                    Some("Timeout waiting for metrics"),
                )
            })
    }
}

/// Test assertion helpers
///
/// These helpers provide common assertion patterns for tests.
pub mod assertions {
    use crate::cleanroom::ContainerMetrics;
    use crate::error::Result;

    /// Assert container metrics are within expected ranges
    pub fn assert_metrics_reasonable(metrics: &ContainerMetrics) -> Result<()> {
        if metrics.cpu_usage_percent < 0.0 || metrics.cpu_usage_percent > 100.0 {
            return Err(crate::error_helpers::container_error(
                "Invalid CPU usage percentage",
                Some("CPU usage should be between 0 and 100"),
            ));
        }

        if metrics.memory_usage_bytes == 0 {
            return Err(crate::error_helpers::container_error(
                "Memory usage is zero",
                Some("Container should have some memory usage"),
            ));
        }

        if metrics.uptime_seconds == 0 {
            return Err(crate::error_helpers::container_error(
                "Uptime is zero",
                Some("Container should have been running for some time"),
            ));
        }

        Ok(())
    }

    /// Assert container has been running for at least the minimum time
    pub fn assert_minimum_uptime(metrics: &ContainerMetrics, min_seconds: u64) -> Result<()> {
        if metrics.uptime_seconds < min_seconds {
            return Err(crate::error_helpers::container_error(
                "Container uptime too short",
                Some(&format!("Expected at least {} seconds uptime", min_seconds)),
            ));
        }
        Ok(())
    }

    /// Assert container memory usage is within expected range
    pub fn assert_memory_range(metrics: &ContainerMetrics, min_mb: u64, max_mb: u64) -> Result<()> {
        let memory_mb = metrics.memory_usage_bytes / (1024 * 1024);

        if memory_mb < min_mb || memory_mb > max_mb {
            return Err(crate::error_helpers::container_error(
                "Memory usage out of range",
                Some(&format!(
                    "Expected {}MB to {}MB, got {}MB",
                    min_mb, max_mb, memory_mb
                )),
            ));
        }
        Ok(())
    }
}

/// Test data generators
///
/// These generators provide common test data patterns.
pub mod generators {
    use crate::cleanroom::{ContainerMetrics, ContainerStatus};
    use crate::metrics_builder::ContainerMetricsBuilder;
    use std::time::Instant;

    /// Generate test container metrics
    pub fn test_metrics(container_type: &str, start_time: &Instant) -> ContainerMetrics {
        match container_type {
            "postgres" => ContainerMetricsBuilder::postgres(start_time),
            "redis" => ContainerMetricsBuilder::redis(start_time),
            "generic" => ContainerMetricsBuilder::generic(start_time),
            _ => ContainerMetricsBuilder::generic(start_time),
        }
    }

    /// Generate test container status
    pub fn test_status(status: &str) -> ContainerStatus {
        match status {
            "starting" => ContainerStatus::Starting,
            "ready" => ContainerStatus::Ready,
            "running" => ContainerStatus::Running,
            "stopped" => ContainerStatus::Stopped,
            "failed" => ContainerStatus::Failed,
            _ => ContainerStatus::Starting,
        }
    }

    /// Generate test container name
    pub fn test_container_name(prefix: &str, index: usize) -> String {
        format!("{}_{}", prefix, index)
    }

    /// Generate test connection string
    pub fn test_connection_string(host: &str, port: u16, database: &str) -> String {
        format!("postgresql://user:pass@{}:{}/{}", host, port, database)
    }
}

/// Test cleanup utilities
///
/// These utilities provide common cleanup patterns for tests.
pub mod cleanup {
    use crate::cleanroom::CleanroomEnvironment;
    use crate::error::Result;
    use std::time::Duration;

    /// Cleanup test environment with timeout
    pub async fn cleanup_environment(
        env: &mut CleanroomEnvironment, timeout: Duration,
    ) -> Result<()> {
        tokio::time::timeout(timeout, env.cleanup())
            .await
            .map_err(|_| {
                crate::error_helpers::timeout_error(
                    "Environment cleanup timeout",
                    Some("Timeout waiting for environment cleanup"),
                )
            })?
    }

    /// Cleanup test environment with default timeout
    pub async fn cleanup_environment_default(env: &mut CleanroomEnvironment) -> Result<()> {
        cleanup_environment(env, Duration::from_secs(30)).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_environment_builder() {
        let env = TestEnvironmentBuilder::new()
            .with_singleton_containers(true)
            .with_timeout(Duration::from_secs(30))
            .build()
            .await
            .unwrap();

        // Test that environment was created successfully
        assert!(env.get_container_count().await > 0);
    }

    #[test]
    fn test_generators() {
        use crate::cleanroom::ContainerStatus;
        use std::time::Instant;

        let start_time = Instant::now();
        let metrics = generators::test_metrics("postgres", &start_time);
        assert_eq!(metrics.cpu_usage_percent, 5.0);

        let status = generators::test_status("running");
        assert_eq!(status, ContainerStatus::Running);

        let name = generators::test_container_name("test", 1);
        assert_eq!(name, "test_1");

        let conn_str = generators::test_connection_string("localhost", 5432, "testdb");
        assert!(conn_str.contains("postgresql://"));
    }

    #[test]
    fn test_assertions() {
        use crate::metrics_builder::ContainerMetricsBuilder;
        use std::time::Instant;

        let start_time = Instant::now();
        let metrics = ContainerMetricsBuilder::postgres(&start_time);

        assertions::assert_metrics_reasonable(&metrics).unwrap();
        assertions::assert_minimum_uptime(&metrics, 0).unwrap();
        assertions::assert_memory_range(&metrics, 100, 200).unwrap();
    }
}
