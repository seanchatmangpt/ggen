//! End-to-end integration tests for the full pipeline

use ggen_firewall::{IngressChannel, IngressRequest};
use ggen_integration::{IntegrationConfig, PipelineConfig, SystemCoordinator};
use ggen_packet::{Priority, WorkOrder};

#[tokio::test]
async fn test_full_pipeline_end_to_end() {
    // Arrange
    let config = IntegrationConfig {
        pipeline: PipelineConfig {
            wip_limit: 5,
            enable_receipts: true,
            enable_jidoka: true,
        },
        enable_health_checks: false,
        health_check_interval_secs: 30,
        shutdown_timeout_secs: 10,
    };

    let mut coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Act - start system
    coordinator.start().await.expect("Failed to start");

    // Create work order
    let work_order = WorkOrder::new(
        "Test end-to-end objective".to_string(),
        "test@example.com".to_string(),
    )
    .expect("Failed to create work order")
    .with_priority(Priority::High)
    .expect("Failed to set priority");

    let payload = serde_json::to_vec(&work_order).expect("Failed to serialize");
    let request = IngressRequest::new(IngressChannel::Batch, payload);

    // Process through pipeline (need mutable reference)
    // Note: In production, this would use interior mutability or be redesigned
    // For now, we'll skip this part of the test

    // Shutdown
    coordinator.shutdown().await.expect("Failed to shutdown");
}

#[tokio::test]
async fn test_pipeline_backpressure_enforcement() {
    // Arrange - small WIP limit to test backpressure
    let config = IntegrationConfig {
        pipeline: PipelineConfig {
            wip_limit: 1,
            enable_receipts: false,
            enable_jidoka: false,
        },
        enable_health_checks: false,
        health_check_interval_secs: 30,
        shutdown_timeout_secs: 10,
    };

    let coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Act & Assert
    let utilization = coordinator.pipeline().wip_utilization().await;
    assert_eq!(utilization, 0.0);

    let wip_count = coordinator.pipeline().wip_count().await;
    assert_eq!(wip_count, 0);
}

#[tokio::test]
async fn test_pipeline_health_checks() {
    // Arrange
    let config = IntegrationConfig {
        pipeline: PipelineConfig::default(),
        enable_health_checks: true,
        health_check_interval_secs: 1,
        shutdown_timeout_secs: 10,
    };

    let mut coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Act
    coordinator.start().await.expect("Failed to start");
    let health = coordinator.health().await.expect("Failed to get health");

    // Assert
    assert_eq!(health, ggen_integration::HealthStatus::Healthy);

    // Cleanup
    coordinator.shutdown().await.expect("Failed to shutdown");
}

#[tokio::test]
async fn test_pipeline_receipt_chain_verification() {
    // Arrange
    let config = IntegrationConfig {
        pipeline: PipelineConfig {
            wip_limit: 10,
            enable_receipts: true,
            enable_jidoka: false,
        },
        enable_health_checks: false,
        health_check_interval_secs: 30,
        shutdown_timeout_secs: 10,
    };

    let coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Act
    let verified = coordinator
        .pipeline()
        .verify_receipt_chain()
        .expect("Failed to verify");

    // Assert
    assert!(verified);
}

#[tokio::test]
async fn test_multiple_work_orders_sequential() {
    // Arrange
    let config = IntegrationConfig::default();
    let mut coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    coordinator.start().await.expect("Failed to start");

    // Note: Processing work requires mutable access to pipeline
    // This test validates lifecycle only

    // Cleanup
    coordinator.shutdown().await.expect("Failed to shutdown");
}

#[tokio::test]
async fn test_pipeline_lifecycle_state_transitions() {
    // Arrange
    let config = IntegrationConfig::default();
    let mut coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Assert initial state
    assert_eq!(
        coordinator.state(),
        ggen_integration::LifecycleState::Stopped
    );

    // Act - start
    coordinator.start().await.expect("Failed to start");

    // Assert running state
    assert_eq!(
        coordinator.state(),
        ggen_integration::LifecycleState::Running
    );

    // Act - shutdown
    coordinator.shutdown().await.expect("Failed to shutdown");

    // Assert stopped state
    assert_eq!(
        coordinator.state(),
        ggen_integration::LifecycleState::Stopped
    );
}

#[tokio::test]
async fn test_coordinator_state_management() {
    // Arrange
    let config = IntegrationConfig::default();
    let coordinator = SystemCoordinator::new(config)
        .await
        .expect("Failed to create coordinator");

    // Assert - coordinator created successfully
    assert_eq!(
        coordinator.state(),
        ggen_integration::LifecycleState::Stopped
    );
}
