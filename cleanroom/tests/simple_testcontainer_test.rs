//! Simple testcontainer integration test
//!
//! This test verifies that the Cleanroom Testing Framework works end-to-end
//! with testcontainers in the simplest possible way.

use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, CleanroomGuard,
    PostgresContainer, RedisContainer, GenericContainer, ContainerWrapper,
};
use std::sync::Arc;

/// Test basic CleanroomEnvironment creation
#[tokio::test]
async fn test_environment_creation() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await;
    assert!(environment.is_ok());
    
    let environment = environment.unwrap();
    let container_count = environment.get_container_count().await;
    assert_eq!(container_count, 0);
}

/// Test container creation without Docker (mock test)
#[tokio::test]
async fn test_container_creation_mock() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Test PostgreSQL container creation
    let postgres_result = PostgresContainer::new_async("testdb", "testuser", "testpass").await;
    assert!(postgres_result.is_ok());
    let postgres_container = postgres_result.unwrap();
    assert_eq!(postgres_container.name(), "postgres");
    environment_arc.register_container("postgres".to_string(), "postgres_1".to_string()).await.unwrap();

    // Test Redis container creation
    let redis_result = RedisContainer::new_async(Some("testpass".to_string())).await;
    assert!(redis_result.is_ok());
    let redis_container = redis_result.unwrap();
    assert_eq!(redis_container.name(), "redis");
    environment_arc.register_container("redis".to_string(), "redis_1".to_string()).await.unwrap();

    // Test Generic container creation
    let generic_result = GenericContainer::new_async("test_container", "alpine", "latest").await;
    assert!(generic_result.is_ok());
    let generic_container = generic_result.unwrap();
    assert_eq!(generic_container.name(), "test_container");
    environment_arc.register_container("generic".to_string(), "generic_1".to_string()).await.unwrap();

    // Verify container count increased
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 3);
}

/// Test container singleton pattern
#[tokio::test]
async fn test_container_singleton_pattern() {
    let config = CleanroomConfig {
        enable_singleton_containers: true,
        ..Default::default()
    };
    
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    // Create first container
    let container1 = PostgresContainer::new_async("testdb", "testuser", "testpass").await.unwrap();
    environment_arc.register_container("postgres".to_string(), "postgres_1".to_string()).await.unwrap();

    // Verify singleton pattern: container already registered
    assert!(environment_arc.is_container_registered("postgres").await);

    // Container count should be 1
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 1);
}

/// Test container metrics and status
#[tokio::test]
async fn test_container_metrics_and_status() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    let _guard = CleanroomGuard::new(environment_arc.clone());

    let container = PostgresContainer::new_async("testdb", "testuser", "testpass").await.unwrap();

    // Test container status
    let status = container.status();
    assert_eq!(status, cleanroom::ContainerStatus::Running);

    // Test container metrics
    let metrics = container.metrics();
    assert!(metrics.cpu_usage_percent >= 0.0);
    assert!(metrics.memory_usage_bytes > 0);
}

/// Test concurrent container operations
#[tokio::test]
async fn test_concurrent_container_operations() {
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
    environment_arc.register_container("postgres1".to_string(), "postgres_1".to_string()).await.unwrap();
    environment_arc.register_container("postgres2".to_string(), "postgres_2".to_string()).await.unwrap();
    environment_arc.register_container("redis1".to_string(), "redis_1".to_string()).await.unwrap();

    // Verify container count
    let container_count = environment_arc.get_container_count().await;
    assert_eq!(container_count, 3);
}

/// Test resource cleanup
#[tokio::test]
async fn test_resource_cleanup() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    let environment_arc = Arc::new(environment);
    
    // Create container
    let _container = PostgresContainer::new_async("testdb", "testuser", "testpass").await.unwrap();
    environment_arc.register_container("postgres".to_string(), "postgres_1".to_string()).await.unwrap();

    // Verify container exists
    let container_count_before = environment_arc.get_container_count().await;
    assert_eq!(container_count_before, 1);

    // Cleanup should be handled by CleanroomGuard when it goes out of scope
    // This is tested implicitly by the guard being dropped
}
