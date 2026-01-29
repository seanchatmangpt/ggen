//! Integration tests for testcontainers functionality
//!
//! Tests Docker container management, health checks, and cleanup using Chicago TDD pattern.
//! Tests are marked with #[ignore] by default as they require Docker to be running.

use ggen_core::testing::{ContainerConfig, ContainerManager, DockerClient, HealthCheck};
use ggen_utils::error::Result;
use std::time::Duration;

// Note: These tests require Docker to be running and are ignored by default
// Run with: cargo test --test testcontainers_tests -- --ignored --test-threads=1

#[test]
#[ignore] // Requires Docker
fn test_redis_container_lifecycle() -> Result<()> {
    // Arrange
    let config = ContainerConfig::redis()
        .with_health_timeout(Duration::from_secs(30));

    // Act - Container starts and health check runs
    let manager = ContainerManager::new(config)?;

    // Assert - Container is running and healthy
    assert!(!manager.container_id().is_empty());

    // Verify we can get the port
    let port = manager.first_port()?;
    assert!(port > 0);
    assert!(port < 65536);

    // Container automatically stopped and removed on drop
    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_postgres_container_lifecycle() -> Result<()> {
    // Arrange
    let config = ContainerConfig::postgres()
        .with_health_timeout(Duration::from_secs(30));

    // Act - Container starts and health check runs
    let manager = ContainerManager::new(config)?;

    // Assert - Container is running and healthy
    assert!(!manager.container_id().is_empty());

    // Verify we can get the port
    let port = manager.first_port()?;
    assert!(port > 0);

    // Container automatically stopped and removed on drop
    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_custom_container_configuration() -> Result<()> {
    // Arrange
    let config = ContainerConfig::new("alpine:latest")
        .with_name("test-alpine")
        .with_env("TEST_VAR", "test_value")
        .with_health_check(HealthCheck::none());

    // Act
    let manager = ContainerManager::new(config)?;

    // Assert
    assert!(!manager.container_id().is_empty());

    // Verify we can execute commands
    let output = manager.exec(&vec!["echo".to_string(), "hello".to_string()])?;
    assert_eq!(output.trim(), "hello");

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_exec_command() -> Result<()> {
    // Arrange
    let config = ContainerConfig::new("alpine:latest")
        .with_health_check(HealthCheck::none());
    let manager = ContainerManager::new(config)?;

    // Act
    let output = manager.exec(&vec![
        "sh".to_string(),
        "-c".to_string(),
        "echo 'test output'".to_string(),
    ])?;

    // Assert
    assert!(output.contains("test output"));

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_port_mapping() -> Result<()> {
    // Arrange
    let config = ContainerConfig::redis()
        .with_health_timeout(Duration::from_secs(30));

    // Act
    let manager = ContainerManager::new(config)?;
    let port = manager.port("6379")?;

    // Assert
    assert!(port > 0);
    assert!(port < 65536);

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_logs() -> Result<()> {
    // Arrange
    let config = ContainerConfig::redis()
        .with_health_timeout(Duration::from_secs(30));
    let manager = ContainerManager::new(config)?;

    // Act - Wait a moment for logs to be generated
    std::thread::sleep(Duration::from_secs(1));
    let logs = manager.logs(Some(10))?;

    // Assert - Redis should output startup logs
    assert!(!logs.is_empty());

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_pause_unpause() -> Result<()> {
    // Arrange
    let config = ContainerConfig::redis()
        .with_health_timeout(Duration::from_secs(30));
    let mut manager = ContainerManager::new(config)?;

    // Act - Pause the container
    manager.pause()?;

    // Assert - Container should be paused (exec should fail)
    let exec_result = manager.exec(&vec!["redis-cli".to_string(), "ping".to_string()]);
    assert!(exec_result.is_err()); // Should fail while paused

    // Act - Unpause the container
    manager.unpause()?;

    // Give container a moment to become ready again
    std::thread::sleep(Duration::from_millis(500));

    // Assert - Container should work again
    let exec_result = manager.exec(&vec!["redis-cli".to_string(), "ping".to_string()]);
    assert!(exec_result.is_ok());

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_stop() -> Result<()> {
    // Arrange
    let config = ContainerConfig::redis()
        .with_health_timeout(Duration::from_secs(30));
    let mut manager = ContainerManager::new(config)?;

    // Act - Stop the container
    manager.stop()?;

    // Assert - Exec should fail on stopped container
    let exec_result = manager.exec(&vec!["redis-cli".to_string(), "ping".to_string()]);
    assert!(exec_result.is_err());

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_health_check_timeout() {
    // Arrange - Create a container with health check that will fail
    let config = ContainerConfig::new("alpine:latest")
        .with_health_check(
            HealthCheck::command(vec!["false".to_string()])
                .with_timeout(Duration::from_secs(2))
                .with_interval(Duration::from_millis(100))
        );

    // Act & Assert - Container creation should fail due to health check timeout
    let result = ContainerManager::new(config);
    assert!(result.is_err());
}

#[test]
#[ignore] // Requires Docker
fn test_docker_client_basic() -> Result<()> {
    // Arrange
    let client = DockerClient::new();

    // Act - Start a simple container
    let container_id = client.run("alpine:latest", &[
        "--rm".to_string(),
        "sleep".to_string(),
        "5".to_string(),
    ])?;

    // Assert
    assert!(!container_id.is_empty());

    // Cleanup
    let _ = client.stop(&container_id);
    let _ = client.rm(&container_id, true);

    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_multiple_containers_concurrently() -> Result<()> {
    // Arrange
    let config1 = ContainerConfig::redis()
        .with_name("test-redis-1")
        .with_health_timeout(Duration::from_secs(30));

    let config2 = ContainerConfig::redis()
        .with_name("test-redis-2")
        .with_health_timeout(Duration::from_secs(30));

    // Act - Start multiple containers
    let manager1 = ContainerManager::new(config1)?;
    let manager2 = ContainerManager::new(config2)?;

    // Assert - Both containers should be running
    assert!(!manager1.container_id().is_empty());
    assert!(!manager2.container_id().is_empty());
    assert_ne!(manager1.container_id(), manager2.container_id());

    let port1 = manager1.first_port()?;
    let port2 = manager2.first_port()?;
    assert_ne!(port1, port2); // Different ports

    // Both containers automatically cleaned up on drop
    Ok(())
}

#[test]
#[ignore] // Requires Docker
fn test_container_with_volume_mount() -> Result<()> {
    // Arrange
    let temp_dir = tempfile::tempdir()?;
    let temp_path = temp_dir.path().to_string_lossy();

    let config = ContainerConfig::new("alpine:latest")
        .with_volume(&format!("{}:/data", temp_path))
        .with_health_check(HealthCheck::none());

    // Act
    let manager = ContainerManager::new(config)?;

    // Write a file in the mounted volume
    manager.exec(&vec![
        "sh".to_string(),
        "-c".to_string(),
        "echo 'test content' > /data/test.txt".to_string(),
    ])?;

    // Assert - File should exist in host directory
    let file_path = temp_dir.path().join("test.txt");
    assert!(file_path.exists());

    let content = std::fs::read_to_string(file_path)?;
    assert!(content.contains("test content"));

    Ok(())
}

// Test that verifies automatic cleanup on drop
#[test]
#[ignore] // Requires Docker
fn test_container_automatic_cleanup() -> Result<()> {
    // Arrange
    let client = DockerClient::new();
    let config = ContainerConfig::redis()
        .with_name("test-redis-cleanup")
        .with_health_timeout(Duration::from_secs(30));

    let container_id: String;

    // Act - Create container in inner scope
    {
        let manager = ContainerManager::new(config)?;
        container_id = manager.container_id().to_string();

        // Container should be running
        let inspect_result = client.inspect(&container_id);
        assert!(inspect_result.is_ok());
    } // Manager dropped here - container should be cleaned up

    // Assert - Container should be removed
    std::thread::sleep(Duration::from_millis(500)); // Give cleanup time
    let inspect_result = client.inspect(&container_id);
    assert!(inspect_result.is_err()); // Container should not exist

    Ok(())
}
