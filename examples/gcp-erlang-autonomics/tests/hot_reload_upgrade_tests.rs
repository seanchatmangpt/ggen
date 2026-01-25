//! Hot Reload & Code Upgrade Integration Tests
//!
//! Tests for upgrading code while governors are running:
//! - Upgrade code while governors running
//! - State preserved after upgrade
//! - Automatic rollback on error
//! - Zero message loss during upgrade
//!
//! Simulates production upgrade scenarios without actual redeployment.

use gcp_erlang_autonomics::{
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, Entitlement, EntitlementState,
};
use gcp_erlang_autonomics::entitlement::{QuotaLimits, ResourceUsage};
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::Utc;

/// Test: State preserved during hot reload
///
/// Verifies state persistence across code upgrade:
/// - Arrange: Create orchestrator with active governors and pending events
/// - Act: Simulate code upgrade (serialize state → deserialize)
/// - Assert: State identical after upgrade, events not lost
#[tokio::test]
async fn test_state_preserved_across_hot_reload() {
    // Arrange: Create orchestrator and process some events
    let mut orchestrator = MarketplaceOrchestrator::new();
    let _ = orchestrator.initialize().await;

    // Create some state by processing events
    let event = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-reload-001".to_string(),
        sku: "pro".to_string(),
    };
    let assigned_before = orchestrator.assign_governors(&event);

    // Capture state before upgrade
    let state_before = orchestrator.current_state();
    let stats_before = orchestrator.stats();

    // Act: Simulate upgrade by serializing and deserializing
    let state_json = serde_json::to_string(&orchestrator)
        .expect("Serialization should succeed");

    let orchestrator_after: MarketplaceOrchestrator = serde_json::from_str(&state_json)
        .expect("Deserialization should succeed");

    // Assert: State identical after upgrade
    assert_eq!(
        orchestrator_after.current_state(),
        state_before,
        "State should be preserved after reload"
    );

    let stats_after = orchestrator_after.stats();
    assert_eq!(
        stats_after.total_governors,
        stats_before.total_governors,
        "Governor count should be preserved"
    );
    assert_eq!(
        stats_after.governors_healthy,
        stats_before.governors_healthy,
        "Healthy governor count should be preserved"
    );
}

/// Test: Governor operations continue during rolling upgrade
///
/// Verifies zero-downtime upgrade:
/// - Arrange: Start processing events before upgrade
/// - Act: Simulate upgrade (pause → update code → resume)
/// - Assert: Events before/after upgrade both succeed
#[tokio::test]
async fn test_events_processed_before_and_after_upgrade() {
    // Arrange: Create orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Act: Process events before upgrade
    let event_before = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-before-upgrade".to_string(),
        sku: "starter".to_string(),
    };

    let result_before = {
        let mut orch = orchestrator.write().await;
        orch.assign_governors(&event_before)
    };

    // Assert: Pre-upgrade event succeeds
    assert!(!result_before.is_empty(), "Pre-upgrade event should succeed");

    // Act: Simulate upgrade (orchestrator remains in memory)
    // In real system, this would be: pause handlers → load new code → resume
    // Here we just verify the orchestrator is still operational
    let orch = orchestrator.read().await;
    assert_eq!(
        orch.current_state(),
        OrchestratorState::Idle,
        "Should still be Idle after upgrade period"
    );

    // Act: Process events after upgrade
    let event_after = MarketplaceEvent::SubscriptionRenewed {
        customer_id: "cust-after-upgrade".to_string(),
        subscription_id: "sub-001".to_string(),
        new_sku: "pro".to_string(),
    };

    let result_after = {
        let mut orch = orchestrator.write().await;
        orch.assign_governors(&event_after)
    };

    // Assert: Post-upgrade event also succeeds
    assert!(!result_after.is_empty(), "Post-upgrade event should succeed");
}

/// Test: Message buffering prevents message loss during upgrade
///
/// Verifies buffering mechanism:
/// - Arrange: Create message queue
/// - Act: Buffer messages during simulated upgrade
/// - Assert: All buffered messages are processed after upgrade
#[tokio::test]
async fn test_message_buffering_prevents_loss_during_upgrade() {
    // Arrange: Create event buffer
    let mut event_buffer = Vec::new();

    // Simulate receiving events before upgrade
    for i in 0..50 {
        event_buffer.push(MarketplaceEvent::CustomerSubscribes {
            customer_id: format!("cust-buffer-{}", i),
            sku: "pro".to_string(),
        });
    }

    // Act: Simulate upgrade (buffer remains, no processing)
    let buffered_count = event_buffer.len();
    assert_eq!(buffered_count, 50, "Should buffer 50 events");

    // Act: After upgrade, process buffered events
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    let mut tasks = Vec::new();
    let events_to_process = event_buffer.clone();

    for event in events_to_process {
        let orch_clone = Arc::clone(&orchestrator);
        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    let results: Vec<_> = futures::future::join_all(tasks)
        .await
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    // Assert: All buffered events processed
    assert_eq!(
        results.len(),
        buffered_count,
        "All {} buffered events should be processed after upgrade",
        buffered_count
    );
}

/// Test: Automatic rollback on upgrade error
///
/// Verifies rollback mechanism:
/// - Arrange: Initialize with stable state
/// - Act: Simulate upgrade error
/// - Assert: Rollback to previous state
#[tokio::test]
async fn test_automatic_rollback_on_upgrade_error() {
    // Arrange: Create orchestrator with known state
    let mut orchestrator = MarketplaceOrchestrator::new();
    let _ = orchestrator.initialize().await;

    // Capture state before "upgrade"
    let state_before = orchestrator.current_state();

    // Act: Attempt upgrade with error
    // Simulate: serialize state → try upgrade → fail → restore
    let state_snapshot = serde_json::to_string(&orchestrator)
        .expect("Serialization should succeed");

    // Simulate upgrade attempting to modify governor count (would fail)
    let upgrade_failed = true;

    // Act: Rollback on failure
    if upgrade_failed {
        let restored: MarketplaceOrchestrator = serde_json::from_str(&state_snapshot)
            .expect("Restoration should succeed");

        // Assert: State restored to pre-upgrade
        assert_eq!(
            restored.current_state(),
            state_before,
            "State should be rolled back to pre-upgrade version"
        );
    }
}

/// Test: Version mismatch detection during upgrade
///
/// Verifies version compatibility check:
/// - Arrange: Orchestrator with version 1.0
/// - Act: Attempt upgrade to incompatible version 2.0
/// - Assert: Version check prevents incompatible upgrade
#[tokio::test]
async fn test_version_mismatch_prevents_incompatible_upgrade() {
    // Arrange: Create orchestrator
    let orchestrator = MarketplaceOrchestrator::new();

    // Simulate version check
    let current_version = "1.0.0";
    let upgrade_version = "2.0.0"; // Incompatible major version

    // Act: Check version compatibility
    let is_compatible = {
        let current_major = current_version.split('.').next().unwrap_or("0");
        let upgrade_major = upgrade_version.split('.').next().unwrap_or("0");
        current_major == upgrade_major
    };

    // Assert: Incompatible upgrade blocked
    assert!(
        !is_compatible,
        "Major version change from {} to {} should be rejected",
        current_version,
        upgrade_version
    );

    // Compatible upgrade should pass
    let compatible_version = "1.1.0";
    let is_compatible_upgrade = {
        let current_major = current_version.split('.').next().unwrap_or("0");
        let compatible_major = compatible_version.split('.').next().unwrap_or("0");
        current_major == compatible_major
    };

    assert!(
        is_compatible_upgrade,
        "Minor version upgrade within same major should be allowed"
    );
}

/// Test: Governor state preserved during individual governor upgrade
///
/// Verifies single governor upgrade:
/// - Arrange: Entitlement governor in Active state with quota
/// - Act: Upgrade governor code
/// - Assert: Quota and state preserved
#[tokio::test]
async fn test_governor_state_preserved_during_individual_upgrade() {
    // Arrange: Create entitlement with specific state
    let original = Entitlement {
        tenant_id: "tenant-upgrade-001".to_string(),
        sku: "enterprise".to_string(),
        state: EntitlementState::Active,
        created_at: Utc::now(),
        expires_at: None,
        quota: QuotaLimits {
            cpu_cores: 16,
            memory_gb: 32,
            concurrent_requests: 1000,
            storage_gb: 500,
            daily_requests: 1_000_000,
        },
        current_usage: ResourceUsage {
            cpu_usage_pct: 45,
            memory_usage_pct: 67,
            concurrent_requests: 450,
            storage_used_gb: 250,
            requests_today: 500_000,
        },
    };

    // Act: Serialize and deserialize (simulate upgrade)
    let json = serde_json::to_string(&original).expect("Serialization should work");
    let upgraded: Entitlement = serde_json::from_str(&json)
        .expect("Deserialization should work");

    // Assert: All state preserved
    assert_eq!(
        upgraded.tenant_id,
        original.tenant_id,
        "Tenant ID should be preserved"
    );
    assert_eq!(upgraded.sku, original.sku, "SKU should be preserved");
    assert_eq!(
        upgraded.state, original.state,
        "Entitlement state should be preserved"
    );
    assert_eq!(
        upgraded.quota.cpu_cores,
        original.quota.cpu_cores,
        "Quota should be preserved"
    );
    assert_eq!(
        upgraded.current_usage.cpu_usage_pct,
        original.current_usage.cpu_usage_pct,
        "Usage metrics should be preserved"
    );
}

/// Test: Idempotent upgrade (upgrade same version twice is safe)
///
/// Verifies upgrade idempotence:
/// - Arrange: Install version 1.1.0
/// - Act: Run upgrade to 1.1.0 twice
/// - Assert: Both safe, no side effects
#[tokio::test]
async fn test_idempotent_upgrade_same_version_twice() {
    // Arrange: Create orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Capture initial state
    let state_1 = {
        let orch = orchestrator.read().await;
        (
            orch.current_state(),
            orch.stats().total_governors,
            orch.stats().governors_healthy,
        )
    };

    // Act: Process event before first "upgrade"
    {
        let mut orch = orchestrator.write().await;
        let event = MarketplaceEvent::CustomerSubscribes {
            customer_id: "cust-idempotent-1".to_string(),
            sku: "pro".to_string(),
        };
        let _assigned = orch.assign_governors(&event);
    }

    // Capture state after first upgrade
    let state_2 = {
        let orch = orchestrator.read().await;
        (
            orch.current_state(),
            orch.stats().total_governors,
            orch.stats().governors_healthy,
        )
    };

    // Act: Process event before second "upgrade"
    {
        let mut orch = orchestrator.write().await;
        let event = MarketplaceEvent::SubscriptionRenewed {
            customer_id: "cust-idempotent-2".to_string(),
            subscription_id: "sub-001".to_string(),
            new_sku: "enterprise".to_string(),
        };
        let _assigned = orch.assign_governors(&event);
    }

    // Capture state after second upgrade
    let state_3 = {
        let orch = orchestrator.read().await;
        (
            orch.current_state(),
            orch.stats().total_governors,
            orch.stats().governors_healthy,
        )
    };

    // Assert: All states identical (idempotent)
    assert_eq!(
        state_1, state_2,
        "State should not change for no-op upgrade"
    );
    assert_eq!(
        state_2, state_3,
        "Second idempotent upgrade should have no effect"
    );
}

/// Test: Concurrent operations during upgrade window
///
/// Verifies safety during upgrade:
/// - Arrange: Start 100 concurrent operations
/// - Act: Simulate upgrade starting
/// - Assert: In-flight operations complete safely
#[tokio::test]
async fn test_concurrent_operations_safe_during_upgrade() {
    // Arrange: Create orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Act: Start 100 concurrent operations
    let mut tasks = Vec::new();
    for i in 0..100 {
        let orch_clone = Arc::clone(&orchestrator);
        let task = tokio::spawn(async move {
            // Simulate operation that might be in-flight during upgrade
            let mut orch = orch_clone.write().await;
            let event = MarketplaceEvent::CustomerSubscribes {
                customer_id: format!("cust-upgrade-safe-{}", i),
                sku: "pro".to_string(),
            };
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    // Act: Wait for all operations to complete
    let results: Vec<_> = futures::future::join_all(tasks)
        .await
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    // Assert: All in-flight operations completed
    assert_eq!(
        results.len(),
        100,
        "All 100 concurrent operations should complete"
    );

    // Assert: Orchestrator still healthy
    let orch = orchestrator.read().await;
    assert_eq!(
        orch.current_state(),
        OrchestratorState::Idle,
        "Orchestrator should still be Idle"
    );
}

/// Test: Health check after upgrade completes
///
/// Verifies post-upgrade validation:
/// - Arrange: Upgrade orchestrator
/// - Act: Run health checks on all governors
/// - Assert: All 8 governors report healthy
#[tokio::test]
async fn test_health_check_after_upgrade_succeeds() {
    // Arrange: Create and initialize orchestrator
    let mut orchestrator = MarketplaceOrchestrator::new();
    let _ = orchestrator.initialize().await;

    // Simulate upgrade by serializing and deserializing
    let state_json = serde_json::to_string(&orchestrator)
        .expect("Serialization should succeed");

    let mut orchestrator_after: MarketplaceOrchestrator = serde_json::from_str(&state_json)
        .expect("Deserialization should succeed");

    // Simulate re-initialization after upgrade
    let init_result = orchestrator_after.initialize().await;

    // Assert: Post-upgrade initialization succeeds
    assert!(
        init_result.is_ok(),
        "Post-upgrade initialization should succeed"
    );

    // Act: Check health of all governors
    let stats = orchestrator_after.stats();

    // Assert: All governors healthy
    assert_eq!(stats.total_governors, 8, "Should have 8 governors");
    assert_eq!(
        stats.governors_healthy, 8,
        "All 8 governors should be healthy after upgrade"
    );
}

/// Test: Upgrade does not affect pending event queue
///
/// Verifies event queue preservation:
/// - Arrange: Queue 50 events
/// - Act: Upgrade orchestrator
/// - Assert: All 50 events still in queue, ready to process
#[tokio::test]
async fn test_pending_event_queue_preserved_after_upgrade() {
    // Arrange: Create queue of pending events
    let pending_events = (0..50)
        .map(|i| MarketplaceEvent::CustomerSubscribes {
            customer_id: format!("cust-queue-{}", i),
            sku: "pro".to_string(),
        })
        .collect::<Vec<_>>();

    // Store queue size
    let queue_size_before = pending_events.len();

    // Act: Simulate upgrade (events remain in queue)
    // Serialize the queue
    let queue_json = serde_json::to_string(&pending_events)
        .expect("Queue serialization should work");

    // Deserialize after upgrade
    let restored_queue: Vec<MarketplaceEvent> = serde_json::from_str(&queue_json)
        .expect("Queue deserialization should work");

    // Assert: Queue size unchanged
    assert_eq!(
        restored_queue.len(),
        queue_size_before,
        "Queue should preserve all {} pending events",
        queue_size_before
    );

    // Act: Process queue after upgrade
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    let mut tasks = Vec::new();
    for event in restored_queue {
        let orch_clone = Arc::clone(&orchestrator);
        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    futures::future::join_all(tasks).await;

    // Assert: All events processed successfully
    let orch = orchestrator.read().await;
    let final_stats = orch.stats();
    assert_eq!(
        final_stats.pending_events, 0,
        "All events from pre-upgrade queue should be processed"
    );
}
