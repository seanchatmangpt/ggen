//! BDD (Behavior Driven Development) tests for cleanroom testing framework
//!
//! These tests use the cucumber framework to test cleanroom behavior
//! from a user perspective with Given-When-Then scenarios.

use cleanroom::{
    CleanroomConfig, CleanroomEnvironment, CleanroomError, CleanroomGuard, CoverageTracker,
    DeterministicManager, GenericContainer, Policy, PostgresContainer, RedisContainer,
    ResourceLimits, SecurityLevel, SnapshotManager, TestReport, TracingManager,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;

/// BDD test context for cleanroom scenarios
#[derive(Debug, Default)]
pub struct CleanroomTestContext {
    pub environment: Option<Arc<CleanroomEnvironment>>,
    pub config: Option<CleanroomConfig>,
    pub policy: Option<Policy>,
    pub resource_limits: Option<ResourceLimits>,
    pub test_results: Vec<String>,
    pub errors: Vec<CleanroomError>,
    pub containers: Vec<String>,
    pub services: Vec<String>,
    pub snapshots: Vec<String>,
    pub traces: Vec<String>,
}

impl CleanroomTestContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_environment(&mut self, environment: Arc<CleanroomEnvironment>) {
        self.environment = Some(environment);
    }

    pub fn get_environment(&self) -> Option<Arc<CleanroomEnvironment>> {
        self.environment.clone()
    }

    pub fn add_test_result(&mut self, result: String) {
        self.test_results.push(result);
    }

    pub fn add_error(&mut self, error: CleanroomError) {
        self.errors.push(error);
    }

    pub fn add_container(&mut self, container_id: String) {
        self.containers.push(container_id);
    }

    pub fn add_service(&mut self, service_id: String) {
        self.services.push(service_id);
    }

    pub fn add_snapshot(&mut self, snapshot_id: String) {
        self.snapshots.push(snapshot_id);
    }

    pub fn add_trace(&mut self, trace_id: String) {
        self.traces.push(trace_id);
    }

    pub fn clear(&mut self) {
        self.test_results.clear();
        self.errors.clear();
        self.containers.clear();
        self.services.clear();
        self.snapshots.clear();
        self.traces.clear();
    }
}

/// BDD test for cleanroom environment setup
#[tokio::test]
async fn test_cleanroom_environment_setup() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom configuration
    let config = CleanroomConfig::default();
    context.config = Some(config);

    // When: I create a cleanroom environment
    let environment = CleanroomEnvironment::new(context.config.unwrap()).await?;
    context.set_environment(Arc::new(environment));

    // Then: The environment should be initialized
    let env = context.get_environment().unwrap();
    assert!(env.is_initialized().await);

    // And: The environment should have default configuration
    let env_config = env.config();
    assert!(env_config.enable_singleton_containers);
    assert_eq!(
        env_config.container_startup_timeout,
        Duration::from_secs(30)
    );

    Ok(())
}

/// BDD test for container lifecycle management
#[tokio::test]
async fn test_container_lifecycle_management() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I start a Postgres container
    let postgres_container = PostgresContainer::new("postgres:15")
        .with_env("POSTGRES_PASSWORD", "test")
        .with_env("POSTGRES_DB", "testdb");

    let env = context.get_environment().unwrap();
    let container_id = env.start_container(postgres_container).await?;
    context.add_container(container_id.clone());

    // Then: The container should be running
    assert!(env.is_container_running(&container_id).await?);

    // And: I should be able to get container information
    let container_info = env.get_container_info(&container_id).await?;
    assert!(container_info.is_some());

    // When: I stop the container
    env.stop_container(&container_id).await?;

    // Then: The container should not be running
    assert!(!env.is_container_running(&container_id).await?);

    Ok(())
}

/// BDD test for service container integration
#[tokio::test]
async fn test_service_container_integration() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I create a Postgres service
    let env = context.get_environment().unwrap();
    let postgres_service = env.create_postgres_service("postgres:15").await?;
    context.add_service("postgres_service".to_string());

    // Then: The service should be ready
    assert!(postgres_service.is_ready().await?);

    // And: I should get a valid connection string
    let connection_string = postgres_service.connection_string();
    assert!(connection_string.contains("postgresql://"));

    // When: I create a Redis service
    let redis_service = env.create_redis_service("redis:7").await?;
    context.add_service("redis_service".to_string());

    // Then: The service should be ready
    assert!(redis_service.is_ready().await?);

    // And: I should get a valid Redis URL
    let redis_url = redis_service.redis_url();
    assert!(redis_url.starts_with("redis://"));

    Ok(())
}

/// BDD test for policy enforcement
#[tokio::test]
async fn test_policy_enforcement() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with security policy enabled
    let mut config = CleanroomConfig::default();
    config.enable_security_policy = true;
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I set a locked security policy
    let locked_policy = Policy::locked();
    context.policy = Some(locked_policy);

    let env = context.get_environment().unwrap();
    env.set_policy(context.policy.unwrap()).await?;

    // Then: The policy should be enforced
    let current_policy = env.get_policy().await?;
    assert_eq!(current_policy.security_level, SecurityLevel::Locked);
    assert!(!current_policy.allows_network());

    // When: I change to a permissive policy
    let permissive_policy =
        Policy::with_security_level(SecurityLevel::Permissive).with_network_isolation(false);
    context.policy = Some(permissive_policy);

    env.set_policy(context.policy.unwrap()).await?;

    // Then: The policy should be updated
    let updated_policy = env.get_policy().await?;
    assert_eq!(updated_policy.security_level, SecurityLevel::Permissive);
    assert!(updated_policy.allows_network());

    Ok(())
}

/// BDD test for resource limits and monitoring
#[tokio::test]
async fn test_resource_limits_and_monitoring() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I set resource limits
    let limits = ResourceLimits::new()
        .with_max_memory_mb(512)
        .with_max_cpu_percent(50.0)
        .with_max_disk_mb(1024);
    context.resource_limits = Some(limits);

    let env = context.get_environment().unwrap();
    env.set_resource_limits(context.resource_limits.unwrap())
        .await?;

    // Then: The limits should be set
    let current_limits = env.get_resource_limits().await?;
    assert_eq!(current_limits.max_memory_mb, 512);
    assert_eq!(current_limits.max_cpu_percent, 50.0);
    assert_eq!(current_limits.max_disk_mb, 1024);

    // When: I monitor resource usage
    let usage = env.get_resource_usage().await?;

    // Then: I should get valid usage metrics
    assert!(usage.memory_usage_mb >= 0.0);
    assert!(usage.cpu_usage_percent >= 0.0);
    assert!(usage.disk_usage_mb >= 0.0);

    Ok(())
}

/// BDD test for deterministic execution
#[tokio::test]
async fn test_deterministic_execution() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with deterministic execution enabled
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I set a fixed seed
    let seed = 12345u64;
    let env = context.get_environment().unwrap();
    env.set_fixed_seed(seed).await?;

    // Then: The seed should be set
    let current_seed = env.get_current_seed().await?;
    assert_eq!(current_seed, seed);

    // When: I execute a deterministic test
    let result1 = env
        .execute_deterministic_test("deterministic_test", || Ok("deterministic_result"))
        .await?;

    // And: I execute the same test again
    let result2 = env
        .execute_deterministic_test("deterministic_test", || Ok("deterministic_result"))
        .await?;

    // Then: Both results should be identical
    assert_eq!(result1, result2);
    context.add_test_result(result1);

    Ok(())
}

/// BDD test for coverage tracking
#[tokio::test]
async fn test_coverage_tracking() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with coverage tracking enabled
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I execute a test with coverage
    let env = context.get_environment().unwrap();
    let result = env
        .execute_test_with_coverage("coverage_test", || {
            println!("Test execution");
            Ok("coverage_result")
        })
        .await?;

    // Then: The test should complete successfully
    assert_eq!(result, "coverage_result");
    context.add_test_result(result);

    // And: I should get a coverage report
    let coverage_report = env.get_coverage_report().await?;
    assert!(coverage_report.total_tests >= 1);
    assert!(coverage_report.passed_tests >= 1);

    // When: I execute another test
    let result2 = env
        .execute_test_with_coverage("coverage_test_2", || {
            println!("Another test execution");
            Ok("coverage_result_2")
        })
        .await?;

    // Then: The coverage report should be updated
    let updated_report = env.get_coverage_report().await?;
    assert!(updated_report.total_tests >= 2);

    Ok(())
}

/// BDD test for snapshot management
#[tokio::test]
async fn test_snapshot_management() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with snapshot testing enabled
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I create a snapshot
    let snapshot_data = serde_json::json!({
        "test_data": "snapshot_value",
        "timestamp": "2024-01-01T00:00:00Z",
        "metadata": {
            "version": "1.0",
            "environment": "test"
        }
    });

    let env = context.get_environment().unwrap();
    let snapshot_id = env.create_snapshot("test_snapshot", &snapshot_data).await?;
    context.add_snapshot(snapshot_id.to_string());

    // Then: The snapshot should be created
    assert!(!snapshot_id.is_nil());

    // And: I should be able to verify the snapshot
    let is_valid = env.verify_snapshot("test_snapshot", &snapshot_data).await?;
    assert!(is_valid);

    // When: I retrieve the snapshot
    let retrieved_snapshot = env.get_snapshot("test_snapshot").await?;

    // Then: I should get the correct snapshot data
    assert!(retrieved_snapshot.is_some());

    // When: I try to verify with different data
    let different_data = serde_json::json!({
        "test_data": "different_value"
    });

    // Then: The verification should fail
    let is_invalid = env
        .verify_snapshot("test_snapshot", &different_data)
        .await?;
    assert!(!is_invalid);

    Ok(())
}

/// BDD test for tracing and logging
#[tokio::test]
async fn test_tracing_and_logging() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with tracing enabled
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I execute a test with tracing
    let env = context.get_environment().unwrap();
    let result = env
        .execute_test_with_tracing("traced_test", || {
            println!("Traced test execution");
            Ok("traced_result")
        })
        .await?;

    // Then: The test should complete successfully
    assert_eq!(result, "traced_result");
    context.add_test_result(result);

    // And: I should get trace logs
    let trace_logs = env.get_trace_logs().await?;
    assert!(!trace_logs.is_empty());

    // When: I execute another traced test
    let result2 = env
        .execute_test_with_tracing("traced_test_2", || {
            println!("Another traced test execution");
            Ok("traced_result_2")
        })
        .await?;

    // Then: The trace logs should be updated
    let updated_logs = env.get_trace_logs().await?;
    assert!(updated_logs.len() >= trace_logs.len());

    Ok(())
}

/// BDD test for comprehensive reporting
#[tokio::test]
async fn test_comprehensive_reporting() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I execute multiple tests
    let env = context.get_environment().unwrap();
    let result1 = env.execute_test("test1", || Ok("result1")).await?;
    let result2 = env.execute_test("test2", || Ok("result2")).await?;
    let result3 = env.execute_test("test3", || Ok("result3")).await?;

    context.add_test_result(result1);
    context.add_test_result(result2);
    context.add_test_result(result3);

    // Then: All tests should complete successfully
    assert_eq!(context.test_results.len(), 3);

    // When: I generate a comprehensive report
    let report = env.generate_comprehensive_report().await?;

    // Then: The report should contain all test results
    assert!(!report.session_id.is_nil());
    assert!(report.test_summary.total_tests >= 3);
    assert!(report.test_summary.passed_tests >= 3);
    assert_eq!(report.test_summary.failed_tests, 0);

    // And: The report should be serializable
    let json_report = report.to_json()?;
    assert!(json_report.contains("session_id"));
    assert!(json_report.contains("test_summary"));

    let toml_report = report.to_toml()?;
    assert!(toml_report.contains("session_id"));
    assert!(toml_report.contains("test_summary"));

    Ok(())
}

/// BDD test for error handling and recovery
#[tokio::test]
async fn test_error_handling_and_recovery() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I try to start an invalid container
    let env = context.get_environment().unwrap();
    let invalid_container = GenericContainer::new("nonexistent:latest");
    let result = env.start_container(invalid_container).await;

    // Then: The operation should fail
    assert!(result.is_err());
    if let Err(error) = result {
        context.add_error(error);
    }

    // When: I execute a failing test
    let test_result = env
        .execute_test("failing_test", || {
            Err(CleanroomError::validation_error("Test failure"))
        })
        .await;

    // Then: The test should fail
    assert!(test_result.is_err());
    if let Err(error) = test_result {
        context.add_error(error);
    }

    // When: I execute a test that times out
    let timeout_result = timeout(
        Duration::from_millis(100),
        env.execute_test("slow_test", || {
            std::thread::sleep(Duration::from_millis(200));
            Ok("slow_result")
        }),
    )
    .await;

    // Then: The test should timeout
    assert!(timeout_result.is_err());

    // And: I should have recorded errors
    assert!(!context.errors.is_empty());

    Ok(())
}

/// BDD test for concurrent test execution
#[tokio::test]
async fn test_concurrent_test_execution() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I execute multiple tests concurrently
    let env = context.get_environment().unwrap();
    let futures: Vec<_> = (0..5)
        .map(|i| {
            let env_clone = env.clone();
            async move {
                env_clone
                    .execute_test(&format!("concurrent_test_{}", i), || {
                        Ok(format!("result_{}", i))
                    })
                    .await
            }
        })
        .collect();

    let results = futures::future::join_all(futures).await;

    // Then: All tests should complete successfully
    for (i, result) in results.iter().enumerate() {
        assert!(result.is_ok());
        let test_result = result.as_ref().unwrap();
        assert_eq!(test_result, &format!("result_{}", i));
        context.add_test_result(test_result.clone());
    }

    // And: I should have all test results
    assert_eq!(context.test_results.len(), 5);

    Ok(())
}

/// BDD test for environment cleanup
#[tokio::test]
async fn test_environment_cleanup() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment with running containers
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    let env = context.get_environment().unwrap();

    // When: I start multiple containers
    let postgres_container =
        PostgresContainer::new("postgres:15").with_env("POSTGRES_PASSWORD", "test");
    let postgres_id = env.start_container(postgres_container).await?;
    context.add_container(postgres_id.clone());

    let redis_container = RedisContainer::new("redis:7");
    let redis_id = env.start_container(redis_container).await?;
    context.add_container(redis_id.clone());

    // Then: All containers should be running
    assert!(env.is_container_running(&postgres_id).await?);
    assert!(env.is_container_running(&redis_id).await?);

    // When: I cleanup the environment
    env.cleanup().await?;

    // Then: All containers should be stopped
    assert!(!env.is_container_running(&postgres_id).await?);
    assert!(!env.is_container_running(&redis_id).await?);

    Ok(())
}

/// BDD test for configuration validation
#[tokio::test]
async fn test_configuration_validation() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A valid cleanroom configuration
    let valid_config = CleanroomConfig::default();
    context.config = Some(valid_config);

    // When: I validate the configuration
    let validation_result = context.config.unwrap().validate();

    // Then: The validation should succeed
    assert!(validation_result.is_ok());

    // Given: An invalid cleanroom configuration
    let mut invalid_config = CleanroomConfig::default();
    invalid_config.max_concurrent_containers = 0; // Invalid value
    context.config = Some(invalid_config);

    // When: I validate the invalid configuration
    let validation_result = context.config.unwrap().validate();

    // Then: The validation should fail
    assert!(validation_result.is_err());

    Ok(())
}

/// BDD test for performance metrics collection
#[tokio::test]
async fn test_performance_metrics_collection() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = CleanroomTestContext::new();

    // Given: A cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    context.set_environment(Arc::new(environment));

    // When: I execute a test and measure performance
    let env = context.get_environment().unwrap();
    let start_time = std::time::Instant::now();
    let result = env
        .execute_test("performance_test", || {
            // Simulate some work
            std::thread::sleep(Duration::from_millis(10));
            Ok("performance_result")
        })
        .await?;

    let duration = start_time.elapsed();

    // Then: The test should complete successfully
    assert_eq!(result, "performance_result");
    assert!(duration >= Duration::from_millis(10));
    context.add_test_result(result);

    // And: I should get performance metrics
    let metrics = env.get_metrics().await;
    assert!(metrics.total_tests >= 1);
    assert!(metrics.average_execution_time >= Duration::from_millis(10));

    Ok(())
}
