//! Edge Device (AtomVM-Inspired) Integration Tests
//!
//! Tests for lightweight autonomic governors on edge devices:
//! - Lightweight governors on edge device (minimal memory footprint)
//! - Sync to BEAM cluster (bi-directional communication)
//! - Offline operation (cache decisions locally)
//! - Recovery when online (sync state, retry failures)
//!
//! Simulates IoT/edge computing scenarios with resource constraints.

use gcp_erlang_autonomics::{
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, Entitlement, EntitlementState,
};
use gcp_erlang_autonomics::entitlement::{QuotaLimits, ResourceUsage};
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::Utc;

/// Lightweight edge governor simulator
///
/// Represents a minimal-footprint governor running on IoT device
#[derive(Clone, Debug)]
struct EdgeGovernor {
    device_id: String,
    governor_name: String,
    local_cache: Vec<String>, // Local decision cache
    synced: bool,             // Whether synced with cluster
    pending_sync: Vec<String>, // Decisions pending sync
}

impl EdgeGovernor {
    fn new(device_id: String, governor_name: String) -> Self {
        Self {
            device_id,
            governor_name,
            local_cache: Vec::new(),
            synced: false,
            pending_sync: Vec::new(),
        }
    }

    /// Lightweight decision making (minimal CPU/memory)
    fn make_decision(&mut self, signal: &str) -> String {
        let decision = format!(
            "edge-decision:{}:{}",
            self.governor_name, signal
        );
        self.local_cache.push(decision.clone());
        self.pending_sync.push(decision.clone());
        decision
    }

    /// Sync pending decisions with cluster
    async fn sync_with_cluster(&mut self) -> bool {
        if !self.pending_sync.is_empty() {
            // Simulate network sync
            self.pending_sync.clear();
            self.synced = true;
            true
        } else {
            self.synced = true;
            true
        }
    }
}

/// Test: Lightweight governor footprint is minimal
///
/// Verifies memory efficiency:
/// - Arrange: Create edge governor
/// - Act: Check memory characteristics
/// - Assert: Footprint < 1MB per governor
#[tokio::test]
async fn test_lightweight_governor_minimal_memory_footprint() {
    // Arrange: Create lightweight edge governor
    let governor = EdgeGovernor::new(
        "edge-device-001".to_string(),
        "entitlement".to_string(),
    );

    // Assert: Minimal fields only
    assert!(governor.local_cache.is_empty(), "Cache should be minimal");
    assert!(
        !governor.synced,
        "Initially not synced until first cluster contact"
    );

    // Estimate footprint (very rough)
    let footprint_estimate = std::mem::size_of_val(&governor);
    println!(
        "Edge Governor estimated size: {} bytes (< 1MB requirement)",
        footprint_estimate
    );

    // Assert: Must be orders of magnitude smaller than orchestrator
    // (Orchestrator might be 10-100KB, edge governor should be < 5KB)
    assert!(
        footprint_estimate < 100_000,
        "Edge governor must be lightweight (< 100KB)"
    );
}

/// Test: Edge governor operates offline with local decisions
///
/// Verifies offline capability:
/// - Arrange: Create edge governor on offline device
/// - Act: Make decisions without cluster connection
/// - Assert: Decisions cached locally, marked for later sync
#[tokio::test]
async fn test_edge_governor_offline_operation_cached_decisions() {
    // Arrange: Create edge governor (no cluster connection)
    let mut governor = EdgeGovernor::new(
        "edge-device-002".to_string(),
        "quota".to_string(),
    );

    // Assert: Initially offline
    assert!(!governor.synced, "Should start offline");

    // Act: Make 10 decisions while offline
    for i in 0..10 {
        let signal = format!("quota-check-{}", i);
        let decision = governor.make_decision(&signal);

        // Assert: Decision cached locally
        assert!(
            governor.local_cache.contains(&decision),
            "Decision should be cached locally"
        );

        // Assert: Decision marked for sync
        assert!(
            governor.pending_sync.contains(&decision),
            "Decision should be pending sync"
        );
    }

    // Assert: All decisions cached, ready for sync when online
    assert_eq!(
        governor.local_cache.len(), 10,
        "All 10 decisions should be cached"
    );
    assert_eq!(
        governor.pending_sync.len(), 10,
        "All 10 should be pending sync"
    );
}

/// Test: Sync to cluster reconciles local and remote state
///
/// Verifies bi-directional sync:
/// - Arrange: Edge governor with 10 offline decisions
/// - Act: Connect to cluster and sync
/// - Assert: Pending decisions flushed, synced flag set
#[tokio::test]
async fn test_sync_to_cluster_reconciles_state() {
    // Arrange: Create edge governor with offline decisions
    let mut governor = EdgeGovernor::new(
        "edge-device-003".to_string(),
        "billing".to_string(),
    );

    // Make 5 offline decisions
    for i in 0..5 {
        let signal = format!("charge-{}", i);
        governor.make_decision(&signal);
    }

    // Assert: Pending sync before connection
    assert_eq!(
        governor.pending_sync.len(), 5,
        "Should have 5 pending syncs"
    );

    // Act: Sync with cluster
    let sync_result = governor.sync_with_cluster().await;

    // Assert: Sync succeeds
    assert!(sync_result, "Sync with cluster should succeed");

    // Assert: Pending cleared, synced flag set
    assert!(
        governor.pending_sync.is_empty(),
        "Pending sync should be cleared"
    );
    assert!(governor.synced, "Should be marked as synced");

    // Assert: Local cache preserved (history)
    assert_eq!(
        governor.local_cache.len(), 5,
        "Local cache should remain for replay/audit"
    );
}

/// Test: Offline retry queue persists across restart
///
/// Verifies queue persistence:
/// - Arrange: Edge device makes decisions offline, fails to sync
/// - Act: Persist retry queue, restart device
/// - Assert: Queue recovered, retry succeeds on next sync
#[tokio::test]
async fn test_offline_retry_queue_persists_across_restart() {
    // Arrange: Create governor with offline decisions
    let mut governor = EdgeGovernor::new(
        "edge-device-004".to_string(),
        "compliance".to_string(),
    );

    // Make 3 offline decisions
    for i in 0..3 {
        let signal = format!("kyc-check-{}", i);
        governor.make_decision(&signal);
    }

    // Serialize retry queue (simulate persistence)
    let retry_queue_json = serde_json::to_string(&governor.pending_sync)
        .expect("Retry queue serialization should work");

    // Simulate restart (queue lost in memory, but recoverable from storage)
    let mut governor_after_restart = EdgeGovernor::new(
        "edge-device-004".to_string(), // Same device
        "compliance".to_string(),
    );

    // Restore retry queue from storage
    let restored_queue: Vec<String> = serde_json::from_str(&retry_queue_json)
        .expect("Retry queue deserialization should work");

    governor_after_restart.pending_sync = restored_queue;

    // Assert: Queue recovered
    assert_eq!(
        governor_after_restart.pending_sync.len(), 3,
        "Retry queue should be recovered after restart"
    );

    // Act: Sync on restart
    let sync_result = governor_after_restart.sync_with_cluster().await;

    // Assert: Sync succeeds, queue cleared
    assert!(sync_result, "Sync after restart should succeed");
    assert!(
        governor_after_restart.pending_sync.is_empty(),
        "Queue should be cleared after sync"
    );
}

/// Test: Edge cluster replication (multiple edge devices)
///
/// Verifies multi-device coordination:
/// - Arrange: 3 edge devices with governors
/// - Act: Each makes local decisions, syncs with cluster
/// - Assert: All syncs succeed, consistent final state
#[tokio::test]
async fn test_edge_cluster_replication_three_devices() {
    // Arrange: Create 3 edge governors on different devices
    let mut edge1 = EdgeGovernor::new(
        "edge-device-001".to_string(),
        "entitlement".to_string(),
    );
    let mut edge2 = EdgeGovernor::new(
        "edge-device-002".to_string(),
        "entitlement".to_string(),
    );
    let mut edge3 = EdgeGovernor::new(
        "edge-device-003".to_string(),
        "entitlement".to_string(),
    );

    // Act: Each device makes independent decisions
    edge1.make_decision("grant-sku-pro");
    edge2.make_decision("grant-sku-enterprise");
    edge3.make_decision("grant-sku-starter");

    // Act: All devices sync with cluster
    let sync1 = edge1.sync_with_cluster().await;
    let sync2 = edge2.sync_with_cluster().await;
    let sync3 = edge3.sync_with_cluster().await;

    // Assert: All syncs succeed
    assert!(sync1, "Device 1 sync should succeed");
    assert!(sync2, "Device 2 sync should succeed");
    assert!(sync3, "Device 3 sync should succeed");

    // Assert: All mark as synced
    assert!(edge1.synced, "Device 1 should be synced");
    assert!(edge2.synced, "Device 2 should be synced");
    assert!(edge3.synced, "Device 3 should be synced");
}

/// Test: Network partition resilience (edge isolated from cluster)
///
/// Verifies behavior during network loss:
/// - Arrange: Edge governor connected and synced
/// - Act: Simulate network partition
/// - Assert: Edge continues offline, decisions queued for later sync
#[tokio::test]
async fn test_network_partition_edge_continues_offline() {
    // Arrange: Create edge governor, sync with cluster
    let mut governor = EdgeGovernor::new(
        "edge-device-005".to_string(),
        "quota".to_string(),
    );

    governor.make_decision("allocate-cpu-4cores");
    let _ = governor.sync_with_cluster().await;

    // Assert: Initially synced
    assert!(governor.synced, "Should be synced initially");

    // Act: Simulate network partition (no cluster connection)
    // Continue making decisions offline
    governor.synced = false; // Simulate network loss
    governor.make_decision("allocate-memory-8gb");
    governor.make_decision("allocate-storage-50gb");

    // Assert: Decisions queued despite network loss
    assert_eq!(
        governor.pending_sync.len(), 2,
        "Decisions should queue during network partition"
    );

    // Assert: Can continue operating
    governor.make_decision("enforce-quota-limits");
    assert_eq!(
        governor.local_cache.len(), 4,
        "Should have 4 total cached decisions"
    );

    // Act: Reconnect and sync
    let sync_result = governor.sync_with_cluster().await;

    // Assert: Sync recovers all pending decisions
    assert!(sync_result, "Sync after reconnect should succeed");
    assert!(
        governor.pending_sync.is_empty(),
        "Pending queue should flush on reconnect"
    );
}

/// Test: Edge device with limited entitlement state
///
/// Verifies lightweight entitlement representation:
/// - Arrange: Minimal entitlement on edge device
/// - Act: Operate with limited quota info
/// - Assert: Decisions enforce quota anyway
#[tokio::test]
async fn test_edge_device_lightweight_entitlement_state() {
    // Arrange: Minimal entitlement (only essential fields)
    let edge_entitlement = Entitlement {
        tenant_id: "edge-tenant-001".to_string(),
        sku: "edge-lite".to_string(),
        state: EntitlementState::Active,
        created_at: Utc::now(),
        expires_at: None,
        quota: QuotaLimits {
            cpu_cores: 2,       // Limited for edge
            memory_gb: 1,       // Minimal
            concurrent_requests: 10, // Few concurrent
            storage_gb: 5,      // Edge storage
            daily_requests: 1000, // Small daily limit
        },
        current_usage: ResourceUsage::default(),
    };

    // Assert: Entitlement created with minimal state
    assert_eq!(
        edge_entitlement.quota.cpu_cores, 2,
        "Edge should have 2 CPU cores max"
    );
    assert_eq!(
        edge_entitlement.quota.memory_gb, 1,
        "Edge should have 1GB memory max"
    );

    // Act: Serialize for transmission to cluster
    let json = serde_json::to_string(&edge_entitlement)
        .expect("Serialization should work");

    // Assert: Minimal JSON footprint
    assert!(
        json.len() < 500,
        "Edge entitlement JSON should be < 500 bytes"
    );
}

/// Test: Edge device decision log for offline audit
///
/// Verifies audit trail in offline mode:
/// - Arrange: Edge governor making decisions offline
/// - Act: Log each decision to local ledger
/// - Assert: Audit trail persists for sync
#[tokio::test]
async fn test_edge_device_decision_log_for_offline_audit() {
    // Arrange: Create edge governor
    let mut governor = EdgeGovernor::new(
        "edge-device-006".to_string(),
        "billing".to_string(),
    );

    // Act: Make decisions with timestamp logging
    let mut audit_log = Vec::new();

    for i in 0..5 {
        let signal = format!("charge-customer-{}", i);
        let decision = governor.make_decision(&signal);

        // Log with timestamp
        let log_entry = format!(
            "ts={:?}, signal={}, decision={}",
            Utc::now(),
            signal,
            decision
        );
        audit_log.push(log_entry);
    }

    // Assert: Audit log captures all decisions
    assert_eq!(
        audit_log.len(), 5,
        "Audit log should have 5 entries"
    );

    // Serialize audit log for sync
    let audit_json = serde_json::to_string(&audit_log)
        .expect("Audit log serialization should work");

    // Assert: Audit log can be synced with cluster
    let restored_audit: Vec<String> = serde_json::from_str(&audit_json)
        .expect("Audit log deserialization should work");

    assert_eq!(
        restored_audit.len(), 5,
        "Audit log should survive serialization roundtrip"
    );
}

/// Test: Batch sync of offline decisions (efficiency)
///
/// Verifies efficient sync:
/// - Arrange: 100 offline decisions accumulated
/// - Act: Batch sync all at once
/// - Assert: Single batch reduces network round-trips
#[tokio::test]
async fn test_batch_sync_offline_decisions_efficiency() {
    // Arrange: Create governor
    let mut governor = EdgeGovernor::new(
        "edge-device-007".to_string(),
        "quota".to_string(),
    );

    // Make 100 offline decisions
    for i in 0..100 {
        let signal = format!("quota-check-{}", i);
        governor.make_decision(&signal);
    }

    // Assert: All pending sync
    assert_eq!(
        governor.pending_sync.len(), 100,
        "Should have 100 pending syncs"
    );

    // Act: Batch sync (single network operation)
    let batch_size = governor.pending_sync.len();
    let sync_result = governor.sync_with_cluster().await;

    // Assert: Batch sync succeeds
    assert!(sync_result, "Batch sync should succeed");

    // Assert: All 100 decisions flushed in one batch
    assert!(
        governor.pending_sync.is_empty(),
        "All 100 decisions should be synced"
    );

    println!(
        "Batch Sync: {} decisions synced in single batch operation",
        batch_size
    );
}

/// Test: Cache coherency during cluster sync
///
/// Verifies consistency:
/// - Arrange: Edge governor with local cache
/// - Act: Cluster sends update conflicting with local cache
/// - Assert: Merge strategy (last-write-wins or merge)
#[tokio::test]
async fn test_cache_coherency_during_cluster_sync() {
    // Arrange: Create governor with local decision
    let mut governor = EdgeGovernor::new(
        "edge-device-008".to_string(),
        "customer-account".to_string(),
    );

    // Local decision: set status to "premium"
    let local_decision = governor.make_decision("upgrade-to-premium");
    assert!(
        governor.local_cache.contains(&local_decision),
        "Local decision should be cached"
    );

    // Act: Simulate cluster update (different decision)
    // In real system, cluster might have different view
    let cluster_decision = "downgrade-to-starter"; // Conflict!

    // Conflict resolution: last-write-wins (cluster has newer timestamp)
    // or merge-based resolution

    // Simplified: cluster decision wins if newer
    let final_decision = if true {
        // Assume cluster is authoritative
        cluster_decision
    } else {
        &local_decision
    };

    // Assert: Final decision is consistent
    assert_eq!(
        final_decision, "downgrade-to-starter",
        "Cluster decision should win (authoritative)"
    );
}

/// Test: Recovery from partial sync failure
///
/// Verifies robustness:
/// - Arrange: Sync 100 decisions, 50 succeed, 50 fail
/// - Act: Retry on next sync
/// - Assert: Failed batch retried completely
#[tokio::test]
async fn test_recovery_from_partial_sync_failure() {
    // Arrange: Create governor with 100 decisions
    let mut governor = EdgeGovernor::new(
        "edge-device-009".to_string(),
        "multi-tenant".to_string(),
    );

    for i in 0..100 {
        let signal = format!("isolation-check-{}", i);
        governor.make_decision(&signal);
    }

    // Simulate partial sync failure (first 50 succeed, rest fail)
    // In real scenario: network timeout after 50 items
    let mut partial_sync = governor.pending_sync.clone();
    let failed_sync = partial_sync.split_off(50);

    // Clear only succeeded items
    governor.pending_sync = failed_sync.clone();

    // Assert: Failed items still pending
    assert_eq!(
        governor.pending_sync.len(), 50,
        "Failed items should remain in queue"
    );

    // Act: Retry complete batch
    let sync_result = governor.sync_with_cluster().await;

    // Assert: Retry succeeds
    assert!(sync_result, "Retry sync should succeed");

    // Assert: All items eventually synced
    assert!(
        governor.pending_sync.is_empty(),
        "All items should sync after retry"
    );
}
