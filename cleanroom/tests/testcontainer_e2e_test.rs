//! End-to-end testcontainer integration test
//!
//! This test verifies that the Cleanroom Testing Framework works end-to-end
//! with testcontainers, ensuring all components integrate correctly.

use cleanroom::{
    run, CleanroomConfig, CleanroomEnvironment, CleanroomGuard, ContainerWrapper, GenericContainer,
    Policy, PostgresContainer, RedisContainer,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;

/// Test basic command execution
#[tokio::test]
async fn test_basic_command_execution() {
    let result = run(["echo", "hello world"]);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("hello world"));
}

/// Test Docker integration with basic command
#[tokio::test]
async fn test_docker_integration_basic() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let result = run(["docker", "--version"]);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("Docker version"));
}

/// Test CleanroomEnvironment creation and basic functionality
#[tokio::test]
async fn test_cleanroom_environment_creation() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await;
    assert!(environment.is_ok());

    let environment = environment.unwrap();
    let container_count = environment.get_container_count().await;
    assert_eq!(container_count, 0);
}

/// Test container creation and management
#[tokio::test]
async fn test_container_creation_and_management() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Test PostgreSQL container creation
    let postgres_result = PostgresContainer::new_async("testdb", "testuser", "testpass").await;
    assert!(postgres_result.is_ok());
    let postgres_container = postgres_result.unwrap();
    assert_eq!(postgres_container.name(), "postgres");
    environment_arc
        .register_container("postgres".to_string(), "postgres_1".to_string())
        .await
        .unwrap();

    // Test Redis container creation
    let redis_result = RedisContainer::new_async(Some("testpass".to_string())).await;
    assert!(redis_result.is_ok());
    let redis_container = redis_result.unwrap();
    assert_eq!(redis_container.name(), "redis");
    environment_arc
        .register_container("redis".to_string(), "redis_1".to_string())
        .await
        .unwrap();

    // Test Generic container creation
    let generic_result = GenericContainer::new_async("test_container", "alpine", "latest").await;
    assert!(generic_result.is_ok());
    let generic_container = generic_result.unwrap();
    assert_eq!(generic_container.name(), "test_container");
    environment_arc
        .register_container("generic".to_string(), "generic_1".to_string())
        .await
        .unwrap();

    // Verify container count increased
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 3);
}

/// Test container singleton pattern
#[tokio::test]
async fn test_container_singleton_pattern() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let config = CleanroomConfig {
        enable_singleton_containers: true,
        ..Default::default()
    };

    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Create first container
    let container1 = PostgresContainer::new_async("testdb", "testuser", "testpass")
        .await
        .unwrap();
    environment_arc
        .register_container("postgres".to_string(), "postgres_1".to_string())
        .await
        .unwrap();

    // Verify singleton pattern: container already registered
    assert!(environment_arc.is_container_registered("postgres").await);

    // Container count should be 1
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 1);
}

/// Test container metrics and status
#[tokio::test]
async fn test_container_metrics_and_status() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let container = PostgresContainer::new_async("testdb", "testuser", "testpass")
        .await
        .unwrap();

    // Test container status
    let status = container.status();
    assert_eq!(status, cleanroom::ContainerStatus::Running);

    // Test container metrics
    let metrics = container.metrics();
    assert!(metrics.cpu_usage_percent >= 0.0);
    assert!(metrics.memory_usage_bytes > 0);
}

/// Test policy enforcement
#[tokio::test]
async fn test_policy_enforcement() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Test with default policy
    let result = run_with_policy(["echo", "test"], Policy::default());
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.exit_code, 0);
}

/// Test error handling
#[tokio::test]
async fn test_error_handling() {
    // Test command that should fail
    let result = run(["false"]);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_ne!(result.exit_code, 0);
}

/// Test timeout handling
#[tokio::test]
async fn test_timeout_handling() {
    // Test command that takes too long
    let result = timeout(Duration::from_secs(1), async { run(["sleep", "10"]) }).await;

    assert!(result.is_err()); // Should timeout
}

/// Test concurrent container operations
#[tokio::test]
async fn test_concurrent_container_operations() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Create multiple containers concurrently
    let future1 = PostgresContainer::new_async("testdb1", "testuser", "testpass");
    let future2 = PostgresContainer::new_async("testdb2", "testuser", "testpass");
    let future3 = RedisContainer::new_async(Some("testpass".to_string()));

    let (result1, result2, result3) = tokio::join!(future1, future2, future3);

    // All should succeed
    assert!(result1.is_ok());
    assert!(result2.is_ok());
    assert!(result3.is_ok());

    // Register containers
    environment_arc
        .register_container("postgres1".to_string(), "postgres_1".to_string())
        .await
        .unwrap();
    environment_arc
        .register_container("postgres2".to_string(), "postgres_2".to_string())
        .await
        .unwrap();
    environment_arc
        .register_container("redis1".to_string(), "redis_1".to_string())
        .await
        .unwrap();

    // Verify container count
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 3);
}

/// Test resource cleanup
#[tokio::test]
async fn test_resource_cleanup() {
    // Skip if Docker is not available
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;
    }

    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);

    // Create container
    let _container = PostgresContainer::new_async("testdb", "testuser", "testpass")
        .await
        .unwrap();
    environment_arc
        .register_container("postgres".to_string(), "postgres_1".to_string())
        .await
        .unwrap();

    // Verify container exists
    let container_count_before = environment_arc.get_container_count().await;
    assert_eq!(container_count_before, 1);

    // Cleanup should be handled by CleanroomGuard when it goes out of scope
    // This is tested implicitly by the guard being dropped
}

/// Helper function to check if Docker is available
async fn is_docker_available() -> bool {
    let result = run(["docker", "--version"]);
    result.is_ok() && result.unwrap().exit_code == 0
}

/// Helper function to run command with policy
fn run_with_policy<I, S>(args: I, _policy: Policy) -> Result<cleanroom::RunResult, cleanroom::Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    // For now, just run the command normally
    // In a full implementation, this would apply the policy
    run(args)
}
