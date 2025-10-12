//! Test suite for cleanroom testing framework
//!
//! This module contains all test types for the cleanroom framework:
//! - Integration tests
//! - Unit tests
//! - Property tests
//! - BDD tests

pub mod integration_tests;
pub mod unit_tests;
pub mod property_tests;
pub mod bdd_tests;

/// Test utilities and helpers
pub mod test_utils {
    use std::time::Duration;
    use tokio::time::timeout;
    use cleanroom::{
        CleanroomEnvironment, CleanroomConfig, CleanroomError,
        PostgresContainer, RedisContainer, GenericContainer,
        Policy, SecurityLevel, ResourceLimits,
    };
    use std::sync::Arc;

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

    /// Create a test Postgres container
    pub fn create_test_postgres_container() -> PostgresContainer {
        PostgresContainer::new("postgres:15")
            .with_env("POSTGRES_PASSWORD", "test")
            .with_env("POSTGRES_DB", "testdb")
            .with_port(5432)
    }

    /// Create a test Redis container
    pub fn create_test_redis_container() -> RedisContainer {
        RedisContainer::new("redis:7")
            .with_port(6379)
    }

    /// Create a test generic container
    pub fn create_test_generic_container() -> GenericContainer {
        GenericContainer::new("nginx:latest")
            .with_port(8080)
            .with_env("NGINX_PORT", "8080")
    }

    /// Create a test policy
    pub fn create_test_policy() -> Policy {
        Policy::with_security_level(SecurityLevel::Standard)
            .with_network_isolation(false)
    }

    /// Create test resource limits
    pub fn create_test_resource_limits() -> ResourceLimits {
        ResourceLimits::new()
            .with_max_memory_mb(512)
            .with_max_cpu_percent(50.0)
            .with_max_disk_mb(1024)
    }

    /// Wait for a condition to be true with timeout
    pub async fn wait_for_condition<F, Fut>(
        condition: F,
        timeout_duration: Duration,
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
        test: F,
        timeout_duration: Duration,
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
        assert!((a - b).abs() < epsilon, "{} is not approximately equal to {} (epsilon: {})", a, b, epsilon);
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

    /// Assert that a vector contains expected elements
    pub fn assert_vec_contains<T: PartialEq + std::fmt::Debug>(vec: &[T], expected: &T) {
        assert!(
            vec.contains(expected),
            "Vector {:?} does not contain {:?}",
            vec,
            expected
        );
    }

    /// Assert that a result is ok and contains expected value
    pub fn assert_result_ok<T: PartialEq + std::fmt::Debug>(result: &Result<T, CleanroomError>, expected: &T) {
        match result {
            Ok(value) => assert_eq!(value, expected),
            Err(error) => panic!("Expected Ok({:?}), got Err({:?})", expected, error),
        }
    }

    /// Assert that a result is an error
    pub fn assert_result_err<T: std::fmt::Debug>(result: &Result<T, CleanroomError>) {
        match result {
            Ok(value) => panic!("Expected Err, got Ok({:?})", value),
            Err(_) => {} // Expected
        }
    }

    /// Assert that a result is an error with specific kind
    pub fn assert_result_err_kind<T: std::fmt::Debug>(
        result: &Result<T, CleanroomError>,
        expected_kind: cleanroom::ErrorKind,
    ) {
        match result {
            Ok(value) => panic!("Expected Err({:?}), got Ok({:?})", expected_kind, value),
            Err(error) => assert_eq!(error.kind(), expected_kind),
        }
    }

    /// Assert that a result is an error with specific message
    pub fn assert_result_err_message<T: std::fmt::Debug>(
        result: &Result<T, CleanroomError>,
        expected_message: &str,
    ) {
        match result {
            Ok(value) => panic!("Expected Err with message '{}', got Ok({:?})", expected_message, value),
            Err(error) => assert!(error.message().contains(expected_message)),
        }
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

        pub fn with_max_containers(mut self, max: usize) -> Self {
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

        pub fn with_security_policy(mut self, enable: bool) -> Self {
            self.config.enable_security_policy = enable;
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

    /// Test policy builder
    pub struct TestPolicyBuilder {
        policy: Policy,
    }

    impl TestPolicyBuilder {
        pub fn new() -> Self {
            Self {
                policy: Policy::default(),
            }
        }

        pub fn with_security_level(mut self, level: SecurityLevel) -> Self {
            self.policy.security_level = level;
            self
        }

        pub fn with_network_isolation(mut self, enable: bool) -> Self {
            self.policy.network.enable_network_isolation = enable;
            self
        }

        pub fn with_port_scanning(mut self, enable: bool) -> Self {
            self.policy.network.enable_port_scanning = enable;
            self
        }

        pub fn with_file_system_isolation(mut self, enable: bool) -> Self {
            self.policy.network.enable_file_system_isolation = enable;
            self
        }

        pub fn build(self) -> Policy {
            self.policy
        }
    }

    impl Default for TestPolicyBuilder {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Test resource limits builder
    pub struct TestResourceLimitsBuilder {
        limits: ResourceLimits,
    }

    impl TestResourceLimitsBuilder {
        pub fn new() -> Self {
            Self {
                limits: ResourceLimits::new(),
            }
        }

        pub fn with_max_memory_mb(mut self, memory: usize) -> Self {
            self.limits.max_memory_mb = memory;
            self
        }

        pub fn with_max_cpu_percent(mut self, cpu: f64) -> Self {
            self.limits.max_cpu_percent = cpu;
            self
        }

        pub fn with_max_disk_mb(mut self, disk: usize) -> Self {
            self.limits.max_disk_mb = disk;
            self
        }

        pub fn with_max_network_mb(mut self, network: usize) -> Self {
            self.limits.max_network_mb = network;
            self
        }

        pub fn build(self) -> ResourceLimits {
            self.limits
        }
    }

    impl Default for TestResourceLimitsBuilder {
        fn default() -> Self {
            Self::new()
        }
    }
}
