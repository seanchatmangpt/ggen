//! Test suite for cleanroom testing framework
//!
//! This module contains test utilities and helpers for cleanroom tests

// Note: These module references are commented out as they reference files that don't exist yet
// Uncomment them when the test files are created
// pub mod bdd_tests;
// pub mod integration_tests;
// pub mod property_tests;
// pub mod unit_tests;

/// Test utilities and helpers
pub mod test_utils {
    use cleanroom::{
        CleanroomConfig, CleanroomEnvironment, Error as CleanroomError, ResourceLimits,
        SecurityLevel,
    };
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::time::timeout;

    /// Create a test cleanroom environment with default configuration
    pub async fn create_test_environment() -> Result<Arc<CleanroomEnvironment>, CleanroomError> {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await?;
        Ok(Arc::new(environment))
    }

    /// Create a test cleanroom environment with custom configuration
    pub async fn create_test_environment_with_config(
        config: CleanroomConfig,
    ) -> Result<Arc<CleanroomEnvironment>, CleanroomError> {
        let environment = CleanroomEnvironment::new(config).await?;
        Ok(Arc::new(environment))
    }

    /// Create test resource limits
    pub fn create_test_resource_limits() -> ResourceLimits {
        ResourceLimits::default()
    }

    /// Wait for a condition to be true with timeout
    pub async fn wait_for_condition<F, Fut>(
        condition: F, timeout_duration: Duration,
    ) -> Result<bool, CleanroomError>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = bool>,
    {
        let start_time = std::time::Instant::now();
        while start_time.elapsed() < timeout_duration {
            if condition().await {
                return Ok(true);
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        Ok(false)
    }

    /// Execute a test with timeout
    pub async fn execute_test_with_timeout<F, Fut, T>(
        test: F, timeout_duration: Duration,
    ) -> Result<T, CleanroomError>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = Result<T, CleanroomError>>,
    {
        timeout(timeout_duration, test())
            .await
            .map_err(|_| CleanroomError::validation_error("Test timeout"))?
    }

    /// Generate test data
    pub fn generate_test_data(size: usize) -> serde_json::Value {
        let mut data = serde_json::Map::new();
        for i in 0..size {
            data.insert(
                format!("key_{}", i),
                serde_json::Value::String(format!("value_{}", i)),
            );
        }
        serde_json::Value::Object(data)
    }

    /// Create test snapshot data
    pub fn create_test_snapshot_data() -> serde_json::Value {
        serde_json::json!({
            "test_data": "snapshot_value",
            "timestamp": "2024-01-01T00:00:00Z",
            "metadata": {
                "version": "1.0",
                "environment": "test"
            }
        })
    }

    /// Assert that two values are approximately equal (for floating point comparisons)
    pub fn assert_approx_eq(a: f64, b: f64, epsilon: f64) {
        assert!(
            (a - b).abs() < epsilon,
            "{} is not approximately equal to {} (epsilon: {})",
            a,
            b,
            epsilon
        );
    }

    /// Assert that a duration is within expected range
    pub fn assert_duration_in_range(duration: Duration, min: Duration, max: Duration) {
        assert!(
            duration >= min && duration <= max,
            "Duration {:?} is not in range [{:?}, {:?}]",
            duration,
            min,
            max
        );
    }

    /// Assert that a string contains expected content
    pub fn assert_string_contains(haystack: &str, needle: &str) {
        assert!(
            haystack.contains(needle),
            "String '{}' does not contain '{}'",
            haystack,
            needle
        );
    }

    /// Test configuration builder
    pub struct TestConfigBuilder {
        config: CleanroomConfig,
    }

    impl TestConfigBuilder {
        pub fn new() -> Self {
            Self {
                config: CleanroomConfig::default(),
            }
        }

        pub fn with_singleton_containers(mut self, enable: bool) -> Self {
            self.config.enable_singleton_containers = enable;
            self
        }

        pub fn with_startup_timeout(mut self, timeout: Duration) -> Self {
            self.config.container_startup_timeout = timeout;
            self
        }

        pub fn with_execution_timeout(mut self, timeout: Duration) -> Self {
            self.config.test_execution_timeout = timeout;
            self
        }

        pub fn with_max_containers(mut self, max: u32) -> Self {
            self.config.max_concurrent_containers = max;
            self
        }

        pub fn with_deterministic_execution(mut self, enable: bool) -> Self {
            self.config.enable_deterministic_execution = enable;
            self
        }

        pub fn with_coverage_tracking(mut self, enable: bool) -> Self {
            self.config.enable_coverage_tracking = enable;
            self
        }

        pub fn with_snapshot_testing(mut self, enable: bool) -> Self {
            self.config.enable_snapshot_testing = enable;
            self
        }

        pub fn with_tracing(mut self, enable: bool) -> Self {
            self.config.enable_tracing = enable;
            self
        }

        pub fn with_security_policy(mut self, security_level: SecurityLevel) -> Self {
            self.config.security_policy.security_level = security_level;
            self
        }

        pub fn build(self) -> CleanroomConfig {
            self.config
        }
    }

    impl Default for TestConfigBuilder {
        fn default() -> Self {
            Self::new()
        }
    }
}
