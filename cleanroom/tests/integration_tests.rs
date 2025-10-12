//! Integration tests for cleanroom testing framework
//!
//! These tests verify the complete cleanroom environment functionality
//! including container lifecycle, service integration, and error handling.

use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, CleanroomGuard,
    PostgresContainer, RedisContainer, GenericContainer,
    ContainerWrapper, ServiceContainer,
    Policy, SecurityLevel,
    ResourceLimits, DeterministicManager,
    CoverageTracker, SnapshotManager, TracingManager,
    TestReport, CleanroomError,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;
use uuid::Uuid;

/// Test basic cleanroom environment creation and configuration
#[tokio::test]
async fn test_cleanroom_environment_creation() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Verify environment is properly initialized
    assert!(environment.is_initialized().await);
    
    // Verify configuration is set correctly
    let env_config = environment.config();
    assert!(env_config.enable_singleton_containers);
    assert_eq!(env_config.container_startup_timeout, Duration::from_secs(30));
    
    Ok(())
}

/// Test cleanroom guard for automatic cleanup
#[tokio::test]
async fn test_cleanroom_guard() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    let environment_arc = Arc::new(environment);
    
    // Create guard
    let _guard = CleanroomGuard::new(environment_arc.clone());
    
    // Verify environment is still accessible
    assert!(environment_arc.is_initialized().await);
    
    // Guard will automatically cleanup when dropped
    Ok(())
}

/// Test container lifecycle management
#[tokio::test]
async fn test_container_lifecycle() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test Postgres container
    let postgres_container = PostgresContainer::new("postgres:15")
        .with_env("POSTGRES_PASSWORD", "test")
        .with_env("POSTGRES_DB", "testdb");
    
    let container_id = environment.start_container(postgres_container).await?;
    assert!(environment.is_container_running(&container_id).await?);
    
    // Test container access
    let connection_info = environment.get_container_info(&container_id).await?;
    assert!(connection_info.is_some());
    
    // Test container stop
    environment.stop_container(&container_id).await?;
    assert!(!environment.is_container_running(&container_id).await?);
    
    Ok(())
}

/// Test service container integration
#[tokio::test]
async fn test_service_container_integration() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test Postgres service
    let postgres_service = environment.create_postgres_service("postgres:15").await?;
    assert!(postgres_service.is_ready().await?);
    
    let connection_string = postgres_service.connection_string();
    assert!(connection_string.contains("postgresql://"));
    
    // Test Redis service
    let redis_service = environment.create_redis_service("redis:7").await?;
    assert!(redis_service.is_ready().await?);
    
    let redis_url = redis_service.redis_url();
    assert!(redis_url.starts_with("redis://"));
    
    Ok(())
}

/// Test policy enforcement
#[tokio::test]
async fn test_policy_enforcement() -> Result<(), Box<dyn std::error::Error>> {
    let mut config = CleanroomConfig::default();
    config.enable_security_policy = true;
    
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test locked policy
    let locked_policy = Policy::locked();
    environment.set_policy(locked_policy).await?;
    
    // Verify policy is enforced
    let current_policy = environment.get_policy().await?;
    assert_eq!(current_policy.security_level, SecurityLevel::Locked);
    assert!(!current_policy.allows_network());
    
    // Test custom policy
    let custom_policy = Policy::with_security_level(SecurityLevel::Strict)
        .with_network_isolation(false);
    
    environment.set_policy(custom_policy).await?;
    
    let updated_policy = environment.get_policy().await?;
    assert_eq!(updated_policy.security_level, SecurityLevel::Strict);
    assert!(updated_policy.allows_network());
    
    Ok(())
}

/// Test resource limits and monitoring
#[tokio::test]
async fn test_resource_limits() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test resource limits
    let limits = ResourceLimits::new()
        .with_max_memory_mb(512)
        .with_max_cpu_percent(50.0)
        .with_max_disk_mb(1024);
    
    environment.set_resource_limits(limits).await?;
    
    // Verify limits are set
    let current_limits = environment.get_resource_limits().await?;
    assert_eq!(current_limits.max_memory_mb, 512);
    assert_eq!(current_limits.max_cpu_percent, 50.0);
    
    // Test resource monitoring
    let usage = environment.get_resource_usage().await?;
    assert!(usage.memory_usage_mb >= 0.0);
    assert!(usage.cpu_usage_percent >= 0.0);
    
    Ok(())
}

/// Test deterministic execution
#[tokio::test]
async fn test_deterministic_execution() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test deterministic manager
    let deterministic_manager = DeterministicManager::new();
    environment.set_deterministic_manager(deterministic_manager).await?;
    
    // Test fixed seed
    let seed = 12345u64;
    environment.set_fixed_seed(seed).await?;
    
    // Verify seed is set
    let current_seed = environment.get_current_seed().await?;
    assert_eq!(current_seed, seed);
    
    // Test deterministic test execution
    let result1 = environment.execute_deterministic_test("test1", || {
        Ok("deterministic_result")
    }).await?;
    
    let result2 = environment.execute_deterministic_test("test1", || {
        Ok("deterministic_result")
    }).await?;
    
    assert_eq!(result1, result2);
    
    Ok(())
}

/// Test coverage tracking
#[tokio::test]
async fn test_coverage_tracking() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test coverage tracker
    let coverage_tracker = CoverageTracker::new();
    environment.set_coverage_tracker(coverage_tracker).await?;
    
    // Execute test with coverage
    let result = environment.execute_test_with_coverage("coverage_test", || {
        println!("Test execution");
        Ok("coverage_result")
    }).await?;
    
    assert_eq!(result, "coverage_result");
    
    // Get coverage report
    let coverage_report = environment.get_coverage_report().await?;
    assert!(coverage_report.total_tests >= 1);
    
    Ok(())
}

/// Test snapshot management
#[tokio::test]
async fn test_snapshot_management() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test snapshot manager
    let snapshot_manager = SnapshotManager::new();
    environment.set_snapshot_manager(snapshot_manager).await?;
    
    // Create snapshot
    let snapshot_data = serde_json::json!({
        "test_data": "snapshot_value",
        "timestamp": "2024-01-01T00:00:00Z"
    });
    
    let snapshot_id = environment.create_snapshot("test_snapshot", &snapshot_data).await?;
    assert!(!snapshot_id.is_nil());
    
    // Verify snapshot
    let is_valid = environment.verify_snapshot("test_snapshot", &snapshot_data).await?;
    assert!(is_valid);
    
    // Get snapshot
    let retrieved_snapshot = environment.get_snapshot("test_snapshot").await?;
    assert!(retrieved_snapshot.is_some());
    
    Ok(())
}

/// Test tracing and logging
#[tokio::test]
async fn test_tracing_and_logging() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test tracing manager
    let tracing_manager = TracingManager::new();
    environment.set_tracing_manager(tracing_manager).await?;
    
    // Execute test with tracing
    let result = environment.execute_test_with_tracing("traced_test", || {
        println!("Traced test execution");
        Ok("traced_result")
    }).await?;
    
    assert_eq!(result, "traced_result");
    
    // Get trace logs
    let trace_logs = environment.get_trace_logs().await?;
    assert!(!trace_logs.is_empty());
    
    Ok(())
}

/// Test comprehensive reporting
#[tokio::test]
async fn test_comprehensive_reporting() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute multiple tests
    environment.execute_test("test1", || Ok("result1")).await?;
    environment.execute_test("test2", || Ok("result2")).await?;
    environment.execute_test("test3", || Ok("result3")).await?;
    
    // Generate comprehensive report
    let report = environment.generate_comprehensive_report().await?;
    
    // Verify report structure
    assert!(!report.session_id.is_nil());
    assert!(report.test_summary.total_tests >= 3);
    assert!(report.test_summary.passed_tests >= 3);
    assert_eq!(report.test_summary.failed_tests, 0);
    
    // Test report serialization
    let json_report = report.to_json()?;
    assert!(json_report.contains("session_id"));
    assert!(json_report.contains("test_summary"));
    
    Ok(())
}

/// Test error handling and recovery
#[tokio::test]
async fn test_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test container startup failure
    let invalid_container = GenericContainer::new("nonexistent:latest");
    let result = environment.start_container(invalid_container).await;
    assert!(result.is_err());
    
    // Test test execution failure
    let test_result = environment.execute_test("failing_test", || {
        Err(CleanroomError::validation_error("Test failure"))
    }).await;
    assert!(test_result.is_err());
    
    // Test timeout handling
    let timeout_result = timeout(
        Duration::from_millis(100),
        environment.execute_test("slow_test", || {
            std::thread::sleep(Duration::from_millis(200));
            Ok("slow_result")
        })
    ).await;
    assert!(timeout_result.is_err());
    
    Ok(())
}

/// Test concurrent test execution
#[tokio::test]
async fn test_concurrent_execution() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute multiple tests concurrently
    let futures: Vec<_> = (0..5).map(|i| {
        let env = environment.clone();
        async move {
            env.execute_test(&format!("concurrent_test_{}", i), || {
                Ok(format!("result_{}", i))
            }).await
        }
    }).collect();
    
    let results = futures::future::join_all(futures).await;
    
    // Verify all tests completed successfully
    for (i, result) in results.iter().enumerate() {
        assert!(result.is_ok());
        assert_eq!(result.as_ref().unwrap(), &format!("result_{}", i));
    }
    
    Ok(())
}

/// Test cleanroom environment cleanup
#[tokio::test]
async fn test_environment_cleanup() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Start some containers
    let postgres_container = PostgresContainer::new("postgres:15")
        .with_env("POSTGRES_PASSWORD", "test");
    let postgres_id = environment.start_container(postgres_container).await?;
    
    let redis_container = RedisContainer::new("redis:7");
    let redis_id = environment.start_container(redis_container).await?;
    
    // Verify containers are running
    assert!(environment.is_container_running(&postgres_id).await?);
    assert!(environment.is_container_running(&redis_id).await?);
    
    // Cleanup environment
    environment.cleanup().await?;
    
    // Verify containers are stopped
    assert!(!environment.is_container_running(&postgres_id).await?);
    assert!(!environment.is_container_running(&redis_id).await?);
    
    Ok(())
}

/// Test configuration validation
#[tokio::test]
async fn test_configuration_validation() -> Result<(), Box<dyn std::error::Error>> {
    // Test valid configuration
    let valid_config = CleanroomConfig::default();
    assert!(valid_config.validate().is_ok());
    
    // Test invalid configuration
    let mut invalid_config = CleanroomConfig::default();
    invalid_config.max_concurrent_containers = 0; // Invalid value
    
    let validation_result = invalid_config.validate();
    assert!(validation_result.is_err());
    
    Ok(())
}

/// Test performance metrics collection
#[tokio::test]
async fn test_performance_metrics() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test and measure performance
    let start_time = std::time::Instant::now();
    let result = environment.execute_test("performance_test", || {
        // Simulate some work
        std::thread::sleep(Duration::from_millis(10));
        Ok("performance_result")
    }).await?;
    
    let duration = start_time.elapsed();
    
    assert_eq!(result, "performance_result");
    assert!(duration >= Duration::from_millis(10));
    
    // Get performance metrics
    let metrics = environment.get_metrics().await;
    assert!(metrics.total_tests >= 1);
    assert!(metrics.average_execution_time >= Duration::from_millis(10));
    
    Ok(())
}
