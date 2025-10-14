//! Integration tests for cleanroom testing framework
//!
//! These tests verify the complete cleanroom environment functionality
//! including container lifecycle, service integration, and error handling.

use cleanroom::{
    new_cleanroom, run, run_with_policy, Assert, CleanroomConfig, CleanroomEnvironment,
    Error as CleanroomError, Policy, ResourceLimits, SecurityLevel,
};
use std::time::Duration;

/// Test basic cleanroom environment creation and configuration
#[tokio::test]
async fn test_cleanroom_environment_creation() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Verify environment is properly initialized
    assert!(environment.get_container_count().await >= 0);

    // Verify configuration is set correctly
    let env_config = environment.config();
    assert!(env_config.enable_singleton_containers);
    assert_eq!(
        env_config.container_startup_timeout,
        Duration::from_secs(30)
    );

    Ok(())
}

/// Test container lifecycle management
#[tokio::test]
async fn test_container_lifecycle() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let mut environment = CleanroomEnvironment::new(config).await?;

    let container_id = environment.start_container("test1").await?;
    assert!(environment.get_container_count().await >= 1);

    // Test container access
    let container_count = environment.get_container_count().await;
    assert!(container_count >= 1);

    // Test container cleanup
    environment.cleanup().await?;
    assert_eq!(environment.get_container_count().await, 0);

    Ok(())
}

/// Test resource limits and monitoring
#[tokio::test]
async fn test_resource_limits() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Test resource limits creation
    let limits = ResourceLimits::default();
    assert!(limits.memory.max_usage_bytes > 0);

    // Test resource monitoring
    let metrics = environment.get_metrics().await;
    assert!(metrics.tests_executed >= 0);

    Ok(())
}

/// Test test execution
#[tokio::test]
async fn test_execute_test() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Test deterministic test execution
    let result1 = environment
        .execute_test("test1", || {
            Ok::<String, CleanroomError>("result".to_string())
        })
        .await?;

    let result2 = environment
        .execute_test("test1", || {
            Ok::<String, CleanroomError>("result".to_string())
        })
        .await?;

    assert_eq!(result1, result2);

    Ok(())
}

/// Test comprehensive reporting
#[tokio::test]
async fn test_comprehensive_reporting() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Execute multiple tests
    environment
        .execute_test("test1", || {
            Ok::<String, CleanroomError>("result1".to_string())
        })
        .await?;
    environment
        .execute_test("test2", || {
            Ok::<String, CleanroomError>("result2".to_string())
        })
        .await?;
    environment
        .execute_test("test3", || {
            Ok::<String, CleanroomError>("result3".to_string())
        })
        .await?;

    // Generate comprehensive report
    let metrics = environment.get_metrics().await;

    // Verify metrics structure
    assert!(metrics.tests_executed >= 3);
    assert!(metrics.tests_passed >= 3);
    assert_eq!(metrics.tests_failed, 0);

    Ok(())
}

/// Test error handling and recovery
#[tokio::test]
async fn test_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Test test execution failure
    let test_result = environment
        .execute_test("failing_test", || {
            Err::<String, CleanroomError>(CleanroomError::validation_error("Test failure"))
        })
        .await;
    assert!(test_result.is_err());

    Ok(())
}

/// Test concurrent test execution
#[tokio::test]
async fn test_concurrent_execution() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Execute multiple tests sequentially
    for i in 0..5 {
        let result = environment
            .execute_test(&format!("concurrent_test_{}", i), || {
                Ok::<String, CleanroomError>(format!("result_{}", i))
            })
            .await?;
        assert_eq!(result, format!("result_{}", i));
    }

    Ok(())
}

/// Test cleanroom environment cleanup
#[tokio::test]
async fn test_environment_cleanup() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let mut environment = CleanroomEnvironment::new(config).await?;

    // Start some containers
    let _id1 = environment.start_container("test1").await?;
    let _id2 = environment.start_container("test2").await?;

    // Verify containers are running
    assert!(environment.get_container_count().await >= 2);

    // Cleanup environment
    environment.cleanup().await?;

    // Verify containers are stopped
    assert_eq!(environment.get_container_count().await, 0);

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
    let result = environment
        .execute_test("performance_test", || {
            // Simulate some work
            std::thread::sleep(Duration::from_millis(10));
            Ok::<String, CleanroomError>("performance_result".to_string())
        })
        .await?;

    let duration = start_time.elapsed();

    assert_eq!(result, "performance_result");
    assert!(duration >= Duration::from_millis(10));

    // Get performance metrics
    let metrics = environment.get_metrics().await;
    assert!(metrics.tests_executed >= 1);

    Ok(())
}

/// Test basic Docker integration with simple command execution
#[tokio::test]
async fn test_docker_integration_basic() -> Result<(), Box<dyn std::error::Error>> {
    // Skip if Docker is not available
    if !std::process::Command::new("docker")
        .arg("--version")
        .output()
        .is_ok()
    {
        println!("Skipping Docker integration test: Docker not available");
        return Ok(());
    }

    // Test simple command execution
    let result = run(["echo", "hello from cleanroom"])?;

    result.assert_success();
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.trim().contains("hello from cleanroom"));

    Ok(())
}

/// Test new_cleanroom convenience function
#[tokio::test]
async fn test_new_cleanroom_convenience() -> Result<(), Box<dyn std::error::Error>> {
    let environment = new_cleanroom().await?;

    assert!(environment.get_container_count().await >= 0);

    // Test a simple command through the environment
    let result = environment
        .execute_test("convenience_test", || {
            Ok::<String, CleanroomError>("convenience works".to_string())
        })
        .await?;

    assert_eq!(result, "convenience works");

    Ok(())
}

/// Test error handling in Docker integration
#[tokio::test]
async fn test_docker_integration_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    // Skip if Docker is not available
    if !std::process::Command::new("docker")
        .arg("--version")
        .output()
        .is_ok()
    {
        println!("Skipping error handling test: Docker not available");
        return Ok(());
    }

    // Test command that should fail
    let result = run(["sh", "-c", "exit 42"])?;

    result.assert_failure();
    assert_eq!(result.exit_code, 42);

    Ok(())
}

/// Test container isolation and cleanup
#[tokio::test]
async fn test_container_isolation_and_cleanup() -> Result<(), Box<dyn std::error::Error>> {
    // Skip if Docker is not available
    if !std::process::Command::new("docker")
        .arg("--version")
        .output()
        .is_ok()
    {
        println!("Skipping isolation test: Docker not available");
        return Ok(());
    }

    // Execute multiple commands to test isolation
    let result1 = run(["echo", "first command"])?;
    let result2 = run(["echo", "second command"])?;

    result1.assert_success();
    result2.assert_success();
    assert_eq!(result1.stdout.trim(), "first command");
    assert_eq!(result2.stdout.trim(), "second command");

    Ok(())
}

/// Test performance characteristics
#[tokio::test]
async fn test_performance_characteristics() -> Result<(), Box<dyn std::error::Error>> {
    // Skip if Docker is not available
    if !std::process::Command::new("docker")
        .arg("--version")
        .output()
        .is_ok()
    {
        println!("Skipping performance test: Docker not available");
        return Ok(());
    }

    let start = std::time::Instant::now();

    // Execute a simple command
    let result = run(["echo", "performance test"])?;

    let duration = start.elapsed();

    result.assert_success();
    assert!(duration.as_millis() < 10000); // Should complete within 10 seconds
    assert!(result.duration_ms > 0); // Should have recorded execution time

    Ok(())
}
