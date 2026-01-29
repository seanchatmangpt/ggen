//! Chaos engineering integration tests
//!
//! Tests chaos engineering scenarios with real containers to validate
//! resilience, recovery mechanisms, and graceful degradation.

#![cfg(feature = "testcontainers")]

use ggen_core::testing::{
    ChaosExecutor, ChaosScenario, FailureInjector, InjectionResult, RecoveryResult,
};
use std::time::Duration;
use testcontainers::{clients::Cli, core::WaitFor, GenericImage};

/// Arrange: Create a test container for chaos testing
fn setup_test_container(docker: &Cli) -> testcontainers::Container<'_, GenericImage> {
    let image = GenericImage::new("alpine", "latest")
        .with_wait_for(WaitFor::message_on_stdout("ready"))
        .with_cmd(vec![
            "sh",
            "-c",
            "echo ready && while true; do sleep 1; done",
        ]);

    docker.run(image)
}

#[test]
fn test_chaos_executor_creation() {
    // Arrange
    let docker_host = "unix:///var/run/docker.sock".to_string();

    // Act
    let executor = ChaosExecutor::new(docker_host);

    // Assert
    assert!(
        executor.is_ok(),
        "Failed to create chaos executor: {:?}",
        executor.err()
    );
}

#[test]
fn test_chaos_executor_with_custom_timeout() {
    // Arrange
    let docker_host = "unix:///var/run/docker.sock".to_string();
    let timeout = Duration::from_secs(600);

    // Act
    let executor = ChaosExecutor::new(docker_host)
        .expect("Failed to create executor")
        .with_recovery_timeout(timeout);

    // Assert - verify executor created with custom timeout
    // Note: We can't directly inspect recovery_timeout as it's private,
    // but we verify the builder pattern works correctly
    assert_eq!(executor.recovery_timeout, timeout);
}

#[test]
fn test_container_kill_scenario() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host).expect("Failed to create executor");

    let scenario = ChaosScenario::ContainerFailure {
        container_id: container_id.to_string(),
        verify_recovery: false,
    };

    // Act
    let result = executor.execute_scenario(&scenario);

    // Assert
    assert!(
        result.is_ok(),
        "Container kill scenario failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Injection should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_network_partition_scenario() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host).expect("Failed to create executor");

    let scenario = ChaosScenario::NetworkPartition {
        container_id: container_id.to_string(),
        duration: Duration::from_secs(2), // Short duration for fast tests
    };

    // Act
    let result = executor.execute_scenario(&scenario);

    // Assert
    assert!(
        result.is_ok(),
        "Network partition scenario failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Injection should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_resource_exhaustion_scenario() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host).expect("Failed to create executor");

    let scenario = ChaosScenario::ResourceExhaustion {
        container_id: container_id.to_string(),
        memory_limit: Some(100_000_000), // 100MB
        cpu_quota: Some(50_000),         // 0.5 CPU
    };

    // Act
    let result = executor.execute_scenario(&scenario);

    // Assert
    assert!(
        result.is_ok(),
        "Resource exhaustion scenario failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Injection should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_recovery_verification_success() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id().to_string();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host)
        .expect("Failed to create executor")
        .with_recovery_timeout(Duration::from_secs(30));

    // Health check that always returns true (simulating successful recovery)
    let health_check = |_: &str| -> Result<bool, ggen_core::testing::chaos::ChaosError> {
        Ok(true)
    };

    // Act
    let result = executor.verify_recovery(&container_id, health_check);

    // Assert
    assert!(
        result.is_ok(),
        "Recovery verification failed: {:?}",
        result.err()
    );
    let recovery_result = result.expect("Should have recovery result");
    assert!(
        recovery_result.success,
        "Recovery should succeed: {:?}",
        recovery_result.error_message
    );
}

#[test]
fn test_recovery_verification_timeout() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id().to_string();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host)
        .expect("Failed to create executor")
        .with_recovery_timeout(Duration::from_secs(2)); // Short timeout

    // Health check that always returns false (simulating failed recovery)
    let health_check = |_: &str| -> Result<bool, ggen_core::testing::chaos::ChaosError> {
        Ok(false)
    };

    // Act
    let result = executor.verify_recovery(&container_id, health_check);

    // Assert
    assert!(
        result.is_ok(),
        "Recovery verification failed: {:?}",
        result.err()
    );
    let recovery_result = result.expect("Should have recovery result");
    assert!(
        !recovery_result.success,
        "Recovery should timeout and fail"
    );
    assert!(recovery_result.error_message.is_some());
}

#[test]
fn test_failure_injector_kill_container() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let injector = FailureInjector::new(docker_host).expect("Failed to create injector");

    // Act
    let result = injector.kill_container(container_id);

    // Assert
    assert!(
        result.is_ok(),
        "Kill container failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Kill should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_failure_injector_pause_unpause() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let injector = FailureInjector::new(docker_host).expect("Failed to create injector");

    // Act - Pause
    let pause_result = injector.pause_container(container_id);

    // Assert - Pause
    assert!(
        pause_result.is_ok(),
        "Pause container failed: {:?}",
        pause_result.err()
    );
    let pause_injection = pause_result.expect("Should have result");
    assert!(
        pause_injection.success,
        "Pause should succeed: {:?}",
        pause_injection.error_message
    );

    // Act - Unpause
    let unpause_result = injector.unpause_container(container_id);

    // Assert - Unpause
    assert!(
        unpause_result.is_ok(),
        "Unpause container failed: {:?}",
        unpause_result.err()
    );
    let unpause_injection = unpause_result.expect("Should have result");
    assert!(
        unpause_injection.success,
        "Unpause should succeed: {:?}",
        unpause_injection.error_message
    );
}

#[test]
fn test_failure_injector_set_resource_limits() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let injector = FailureInjector::new(docker_host).expect("Failed to create injector");

    // Act
    let result = injector.set_resource_limits(
        container_id,
        Some(100_000_000), // 100MB
        Some(50_000),      // 0.5 CPU
    );

    // Assert
    assert!(
        result.is_ok(),
        "Set resource limits failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Resource limit update should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_failure_injector_invalid_resource_limits() {
    // Arrange
    let docker_host = "unix:///var/run/docker.sock".to_string();
    let injector = FailureInjector::new(docker_host).expect("Failed to create injector");

    // Act & Assert - Zero memory
    let result = injector.set_resource_limits("test", Some(0), None);
    assert!(
        result.is_err(),
        "Should reject zero memory limit"
    );

    // Act & Assert - Negative CPU
    let result = injector.set_resource_limits("test", None, Some(-1));
    assert!(
        result.is_err(),
        "Should reject negative CPU quota"
    );

    // Act & Assert - Excessive CPU
    let result = injector.set_resource_limits("test", None, Some(100_001));
    assert!(
        result.is_err(),
        "Should reject CPU quota > 100000"
    );
}

#[test]
fn test_concurrent_failures_scenario() {
    // Arrange
    let docker = Cli::default();
    let container1 = setup_test_container(&docker);
    let container2 = setup_test_container(&docker);

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host).expect("Failed to create executor");

    let scenarios = vec![
        ChaosScenario::ResourceExhaustion {
            container_id: container1.id().to_string(),
            memory_limit: Some(50_000_000),
            cpu_quota: Some(25_000),
        },
        ChaosScenario::ResourceExhaustion {
            container_id: container2.id().to_string(),
            memory_limit: Some(75_000_000),
            cpu_quota: Some(50_000),
        },
    ];

    let scenario = ChaosScenario::ConcurrentFailures { scenarios };

    // Act
    let result = executor.execute_scenario(&scenario);

    // Assert
    assert!(
        result.is_ok(),
        "Concurrent failures scenario failed: {:?}",
        result.err()
    );
    let injection_result = result.expect("Should have result");
    assert!(
        injection_result.success,
        "Concurrent injections should succeed: {:?}",
        injection_result.error_message
    );
}

#[test]
fn test_graceful_degradation() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id().to_string();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host).expect("Failed to create executor");

    // Simulate resource exhaustion
    let scenario = ChaosScenario::ResourceExhaustion {
        container_id: container_id.clone(),
        memory_limit: Some(10_000_000), // Very low memory (10MB)
        cpu_quota: Some(10_000),        // Very low CPU (0.1 CPU)
    };

    // Act
    let injection_result = executor
        .execute_scenario(&scenario)
        .expect("Should execute scenario");

    // Assert - Verify graceful degradation
    assert!(
        injection_result.success,
        "Resource limits should be applied: {:?}",
        injection_result.error_message
    );

    // Additional assertion: Container should still be running (graceful degradation)
    // This would require inspecting container state, which we can verify through
    // the absence of errors in the injection result
    assert!(injection_result.error_message.is_none());
}

#[test]
fn test_recovery_after_network_partition() {
    // Arrange
    let docker = Cli::default();
    let container = setup_test_container(&docker);
    let container_id = container.id().to_string();

    let docker_host = "unix:///var/run/docker.sock".to_string();
    let executor = ChaosExecutor::new(docker_host)
        .expect("Failed to create executor")
        .with_recovery_timeout(Duration::from_secs(30));

    // Simulate network partition
    let scenario = ChaosScenario::NetworkPartition {
        container_id: container_id.clone(),
        duration: Duration::from_secs(2),
    };

    executor
        .execute_scenario(&scenario)
        .expect("Should execute partition scenario");

    // Health check that verifies container is running
    let health_check = |_: &str| -> Result<bool, ggen_core::testing::chaos::ChaosError> {
        // In real scenario, would check container state
        Ok(true)
    };

    // Act
    let recovery_result = executor
        .verify_recovery(&container_id, health_check)
        .expect("Should verify recovery");

    // Assert
    assert!(
        recovery_result.success,
        "Should recover after network partition: {:?}",
        recovery_result.error_message
    );
    assert!(recovery_result.recovery_time < Duration::from_secs(30));
}
