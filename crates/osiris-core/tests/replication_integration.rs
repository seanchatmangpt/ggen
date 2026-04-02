//! Multi-Region Integration Tests
//!
//! Comprehensive scenario tests for multi-region replication covering:
//! - Partition recovery scenarios
//! - Leader failover tests
//! - Conflict resolution scenarios
//! - Merge storm tests
//! - Byzantine fault injection scenarios
//!
//! These tests validate the complete replication pipeline across multiple regions
//! with realistic failure modes and recovery procedures.

use osiris_core::{
    CausalityResult, FailoverConfig, FailoverCoordinator, FailoverDecision, FailoverState,
    HealthCheckResult, MultiRegionConfig, MultiRegionManager, RegionHealth, RegionNode,
    ReplicationLag, VectorClock,
};
use std::time::Duration;

// Helper function to create health check result
fn create_health_result(
    region_id: &str,
    health: RegionHealth,
    lag_ms: u64,
    reachable: bool,
) -> HealthCheckResult {
    HealthCheckResult {
        region_id: region_id.to_string(),
        health,
        replication_lag: ReplicationLag::new(lag_ms, 0),
        is_reachable: reachable,
        timestamp: 0,
    }
}

// ============================================================================
// PARTITION RECOVERY SCENARIOS
// ============================================================================

#[tokio::test]
async fn test_partition_recovery_two_regions_diverge_then_reconnect() {
    // Scenario: Two regions diverge during network partition, then reconnect and merge
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Initial state: both regions at VC [us-east=0, us-west=0]
    let initial_vc = manager.get_vector_clock().await;
    assert_eq!(initial_vc.get("us-east"), 0);
    assert_eq!(initial_vc.get("us-west"), 0);

    // Partition begins: each region writes independently
    let mut vc_east = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_east.increment("us-east");
    vc_east.increment("us-east"); // East writes twice

    let mut vc_west = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_west.increment("us-west");
    vc_west.increment("us-west");
    vc_west.increment("us-west"); // West writes three times

    // Verify clocks are concurrent (partitioned)
    assert!(vc_east.concurrent(&vc_west));
    assert!(vc_west.concurrent(&vc_east));

    // Reconnection: merge clocks
    let merge_result = vc_east.merge_with_conflict_detection(&vc_west, None);
    assert!(merge_result.had_conflicts);
    assert_eq!(merge_result.merged_clock.get("us-east"), 2);
    assert_eq!(merge_result.merged_clock.get("us-west"), 3);

    // Manager reflects merged state
    manager.update_vector_clock(&merge_result.merged_clock).await;
    let final_vc = manager.get_vector_clock().await;
    assert_eq!(final_vc.get("us-east"), 2);
    assert_eq!(final_vc.get("us-west"), 3);
}

#[tokio::test]
async fn test_partition_recovery_three_region_causal_chain_preserved() {
    // Scenario: Three-region partition where causal chain must be preserved
    let manager = MultiRegionManager::default_with_regions(&["us-east", "eu-west", "ap-south"])
        .await
        .expect("Failed to create manager");

    // Build causal chain: us-east → eu-west → ap-south
    let mut vc1 = manager.get_vector_clock().await;
    vc1.increment("us-east");

    manager.update_vector_clock(&vc1).await;
    let mut vc2 = manager.get_vector_clock().await;
    vc2.increment("eu-west");

    manager.update_vector_clock(&vc2).await;
    let mut vc3 = manager.get_vector_clock().await;
    vc3.increment("ap-south");

    // Verify causal chain
    assert!(vc1.happens_before(&vc2), "vc1 should happen before vc2");
    assert!(vc2.happens_before(&vc3), "vc2 should happen before vc3");
    assert!(vc1.happens_before(&vc3), "vc1 should happen before vc3");

    // Simulate partition recovery by merging all clocks
    let merged = vc1.merge_with_conflict_detection(&vc2, None);
    let final_merged = merged.merged_clock.merge_with_conflict_detection(&vc3, None);

    assert!(!final_merged.had_conflicts, "Causal chain should not have conflicts");
    assert_eq!(final_merged.merged_clock.get("us-east"), 1);
    assert_eq!(final_merged.merged_clock.get("eu-west"), 1);
    assert_eq!(final_merged.merged_clock.get("ap-south"), 1);
}

#[tokio::test]
async fn test_partition_recovery_with_partial_vector_clocks() {
    // Scenario: Regions have different sets of tracked regions (sparse VCs)
    let mut vc_east = VectorClock::new();
    vc_east
        .as_map()
        .clone()
        .insert("us-east".to_string(), 5);
    vc_east
        .as_map()
        .clone()
        .insert("eu-west".to_string(), 0);

    let mut vc_west = VectorClock::new();
    vc_west
        .as_map()
        .clone()
        .insert("us-east".to_string(), 3);
    vc_west
        .as_map()
        .clone()
        .insert("eu-west".to_string(), 7);

    // Merge should take max for each region
    vc_east.merge(&vc_west);
    assert_eq!(vc_east.get("us-east"), 5); // Max(5, 3)
    assert_eq!(vc_east.get("eu-west"), 7); // Max(0, 7)
}

#[tokio::test]
async fn test_partition_recovery_after_multiple_divergences() {
    // Scenario: Regions partition and reconnect multiple times
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // First partition cycle
    let mut vc_east = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_east.increment("us-east");

    let mut vc_west = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_west.increment("us-west");

    // Merge after first partition
    vc_east.merge(&vc_west);
    assert_eq!(vc_east.get("us-east"), 1);
    assert_eq!(vc_east.get("us-west"), 1);

    // Second partition cycle
    vc_east.increment("us-east");
    vc_east.increment("us-east");

    vc_west.increment("us-west");

    // Merge after second partition
    vc_east.merge(&vc_west);
    assert_eq!(vc_east.get("us-east"), 3);
    assert_eq!(vc_east.get("us-west"), 2);
}

// ============================================================================
// LEADER FAILOVER SCENARIOS
// ============================================================================

#[tokio::test]
async fn test_leader_failover_healthy_primary_no_action() {
    // Scenario: Primary is healthy, no failover should occur
    let coordinator = FailoverCoordinator::new(FailoverConfig::default(), "us-east".to_string());

    // All health checks pass
    let result = create_health_result("us-east", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    assert_eq!(decision, FailoverDecision::None);
    assert_eq!(coordinator.get_state().await, FailoverState::Healthy);
}

#[tokio::test]
async fn test_leader_failover_degraded_primary_triggers_failover() {
    // Scenario: Primary degrades, failover to healthy standby
    let config = FailoverConfig {
        max_consecutive_failures: 3,
        min_quorum_size: 2,
        ..Default::default()
    };
    let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

    // Primary degrades 3 times
    for _ in 0..3 {
        let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
        coordinator.update_health(result).await.unwrap();
    }

    // Standby is healthy
    let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    match decision {
        FailoverDecision::Failover { target_region, .. } => {
            assert_eq!(target_region, "us-west");
        }
        _ => panic!("Expected failover decision"),
    }
}

#[tokio::test]
async fn test_leader_failover_unreachable_primary_immediate_failover() {
    // Scenario: Primary becomes unreachable, immediate failover
    let coordinator = FailoverCoordinator::new(FailoverConfig::default(), "us-east".to_string());

    // Primary unreachable
    let result = create_health_result("us-east", RegionHealth::Healthy, 100, false);
    coordinator.update_health(result).await.unwrap();

    // Standby healthy
    let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    match decision {
        FailoverDecision::Failover { target_region, .. } => {
            assert_eq!(target_region, "us-west");
        }
        _ => panic!("Expected failover decision"),
    }
}

#[tokio::test]
async fn test_leader_failover_insufficient_quorum_blocks_failover() {
    // Scenario: Primary degraded but insufficient healthy regions for quorum
    let config = FailoverConfig {
        min_quorum_size: 2,
        ..Default::default()
    };
    let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

    // Primary degraded
    for _ in 0..3 {
        let result = create_health_result("us-east", RegionHealth::Unhealthy, 100, false);
        coordinator.update_health(result).await.unwrap();
    }

    // Only one healthy standby (insufficient for quorum of 2)
    let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await;
    assert!(decision.is_err(), "Should fail with insufficient quorum");
}

#[tokio::test]
async fn test_leader_failover_selects_lowest_lag_candidate() {
    // Scenario: Multiple healthy standbys, select one with lowest replication lag
    let coordinator = FailoverCoordinator::new(FailoverConfig::default(), "us-east".to_string());

    // Primary degraded
    for _ in 0..3 {
        let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
        coordinator.update_health(result).await.unwrap();
    }

    // Multiple standbys with different lags
    let result1 = create_health_result("us-west", RegionHealth::Healthy, 5000, true);
    let result2 = create_health_result("eu-west", RegionHealth::Healthy, 100, true);
    let result3 = create_health_result("ap-south", RegionHealth::Healthy, 1000, true);
    coordinator.update_health(result1).await.unwrap();
    coordinator.update_health(result2).await.unwrap();
    coordinator.update_health(result3).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    match decision {
        FailoverDecision::Failover { target_region, .. } => {
            // Should select eu-west (lowest lag)
            assert_eq!(target_region, "eu-west");
        }
        _ => panic!("Expected failover decision"),
    }
}

// ============================================================================
// CONFLICT RESOLUTION SCENARIOS
// ============================================================================

#[tokio::test]
async fn test_conflict_resolution_concurrent_writes_detected() {
    // Scenario: Two regions write concurrently, conflict detected
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Concurrent writes
    let mut vc_east = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_east.increment("us-east");

    let mut vc_west = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_west.increment("us-west");

    // Detect concurrency
    let causality = manager.detect_causality(&vc_east, &vc_west).await;
    assert_eq!(causality, CausalityResult::Concurrent);
}

#[tokio::test]
async fn test_conflict_resolution_last_writer_wins_by_timestamp() {
    // Scenario: Concurrent writes resolved by LWW (higher timestamp wins)
    let (winner, ts) = VectorClock::resolve_lww("us-east", 100, "us-west", 101);
    assert_eq!(winner, "us-west");
    assert_eq!(ts, 101);
}

#[tokio::test]
async fn test_conflict_resolution_last_writer_wins_by_region_id() {
    // Scenario: Timestamps equal, region ID acts as tiebreaker
    let (winner, ts) = VectorClock::resolve_lww("us-east", 100, "eu-west", 100);
    assert_eq!(winner, "us-east"); // "us-east" > "eu-west" lexicographically
    assert_eq!(ts, 100);
}

#[tokio::test]
async fn test_conflict_resolution_causal_writes_no_conflict() {
    // Scenario: Causally ordered writes should not conflict
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Causal writes: us-east writes, then us-west writes after receiving
    let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
    vc1.increment("us-east");

    let mut vc2 = vc1.clone();
    vc2.increment("us-west");

    // Should be causal, not concurrent
    let causality = manager.detect_causality(&vc1, &vc2).await;
    assert_eq!(
        causality,
        CausalityResult::Causality {
            happens_before: true
        }
    );
}

#[tokio::test]
async fn test_conflict_resolution_merge_idempotence() {
    // Scenario: Merging same clocks twice should be idempotent
    let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
    vc1.increment("us-east");

    let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
    vc2.increment("us-west");

    // Merge once
    vc1.merge(&vc2);
    let merged_once = vc1.clone();

    // Merge again
    vc1.merge(&vc2);

    assert_eq!(vc1, merged_once);
}

#[tokio::test]
async fn test_conflict_resolution_merge_commutativity() {
    // Scenario: Merge order shouldn't matter (commutative)
    let mut vc1_a = VectorClock::with_regions(&["us-east", "us-west"]);
    vc1_a.increment("us-east");

    let mut vc2_a = VectorClock::with_regions(&["us-east", "us-west"]);
    vc2_a.increment("us-west");

    let mut vc1_b = vc1_a.clone();
    let mut vc2_b = vc2_a.clone();

    // vc1.merge(vc2)
    vc1_a.merge(&vc2_a);

    // vc2.merge(vc1)
    vc2_b.merge(&vc1_b);

    assert_eq!(vc1_a, vc2_b);
}

// ============================================================================
// MERGE STORM SCENARIOS
// ============================================================================

#[tokio::test]
async fn test_merge_storm_three_regions_concurrent_writes() {
    // Scenario: All three regions write concurrently (merge storm)
    let mut vc_east = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
    vc_east.increment("us-east");

    let mut vc_west = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
    vc_west.increment("eu-west");

    let mut vc_south = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
    vc_south.increment("ap-south");

    // All pairs concurrent
    assert!(vc_east.concurrent(&vc_west));
    assert!(vc_east.concurrent(&vc_south));
    assert!(vc_west.concurrent(&vc_south));

    // Merge all three
    let result1 = vc_east.merge_with_conflict_detection(&vc_west, None);
    assert!(result1.had_conflicts);

    let result2 = result1.merged_clock.merge_with_conflict_detection(&vc_south, None);
    assert!(result2.had_conflicts);

    assert_eq!(result2.merged_clock.get("us-east"), 1);
    assert_eq!(result2.merged_clock.get("eu-west"), 1);
    assert_eq!(result2.merged_clock.get("ap-south"), 1);
}

#[tokio::test]
async fn test_merge_storm_cascading_maintains_causality() {
    // Scenario: Cascading merges maintain causal relationships
    let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
    vc1.increment("us-east");

    let mut vc2 = vc1.clone();
    vc2.increment("eu-west");

    let mut vc3 = vc2.clone();
    vc3.increment("ap-south");

    // Cascading merge: (vc1.merge(vc2)).merge(vc3) = vc1.merge(vc2.merge(vc3))
    let mut left_a = vc1.clone();
    let mut left_b = vc2.clone();
    let mut left_c = vc3.clone();

    left_a.merge(&left_b);
    left_a.merge(&left_c);

    let mut right_b = vc2.clone();
    let mut right_c = vc3.clone();
    right_b.merge(&right_c);
    vc1.merge(&right_b);

    assert_eq!(left_a, vc1);
}

#[tokio::test]
async fn test_merge_storm_high_write_volume_convergence() {
    // Scenario: High write volume during merge storm, all converge
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Simulate high write volume
    for i in 0..100 {
        if i % 2 == 0 {
            manager.increment_vector_clock("us-east").await.unwrap();
        } else {
            manager.increment_vector_clock("us-west").await.unwrap();
        }
    }

    let final_vc = manager.get_vector_clock().await;
    assert_eq!(final_vc.get("us-east"), 50);
    assert_eq!(final_vc.get("us-west"), 50);
}

// ============================================================================
// BYZANTINE FAULT INJECTION SCENARIOS
// ============================================================================

#[tokio::test]
async fn test_byzantine_region_sends_conflicting_data() {
    // Scenario: Region sends conflicting hashes (Byzantine behavior)
    let manager = MultiRegionManager::with_evidence_tracking_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Record data conflict from us-west
    manager
        .record_replication_event(
            "us-west",
            osiris_core::replication::evidence_tracker::ReplicationEvent::DataConflict {
                expected_hash: "abc123".to_string(),
                received_hash: "def456".to_string(),
            },
        )
        .await
        .unwrap();

    // Check evidence tracker
    let tracker = manager.get_evidence_tracker().expect("Evidence tracker not found");
    let conflicts = tracker.get_unresolved_conflicts().await;
    assert_eq!(conflicts.len(), 1);
    assert_eq!(conflicts[0].region_id, "us-west");
}

#[tokio::test]
async fn test_byzantine_region_multiple_timeouts_triggers_isolation() {
    // Scenario: Region times out repeatedly, should be isolated
    let manager = MultiRegionManager::with_evidence_tracking_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Record multiple timeouts from us-west
    for i in 0..8 {
        manager
            .record_replication_event(
                "us-west",
                osiris_core::replication::evidence_tracker::ReplicationEvent::AckTimeout {
                    operation_id: format!("op-{}", i),
                    attempt: 1,
                },
            )
            .await
            .unwrap();
    }

    // Region should be marked for isolation
    assert!(manager.should_isolate_region("us-west").await);

    let to_isolate = manager.get_regions_to_isolate().await;
    assert_eq!(to_isolate.len(), 1);
    assert!(to_isolate.contains(&"us-west".to_string()));
}

#[tokio::test]
async fn test_byzantine_region_health_degraded_after_evidence() {
    // Scenario: Byzantine evidence degrades region health
    let manager = MultiRegionManager::with_evidence_tracking_regions(&["us-east"])
        .await
        .expect("Failed to create manager");

    // Record successful operations
    for _ in 0..5 {
        manager
            .record_replication_event(
                "us-east",
                osiris_core::replication::evidence_tracker::ReplicationEvent::WriteReplicated {
                    operation_id: "op-1".to_string(),
                    vector_clock_version: 1,
                },
            )
            .await
            .unwrap();
    }

    // Health should be healthy
    manager
        .update_region_health_from_evidence("us-east")
        .await
        .unwrap();
    let region = manager.get_region("us-east").await.unwrap();
    assert_eq!(region.health, RegionHealth::Healthy);

    // Record multiple failures
    for i in 0..5 {
        manager
            .record_replication_event(
                "us-east",
                osiris_core::replication::evidence_tracker::ReplicationEvent::AckTimeout {
                    operation_id: format!("op-{}", i),
                    attempt: 1,
                },
            )
            .await
            .unwrap();
    }

    // Health should degrade
    manager
        .update_region_health_from_evidence("us-east")
        .await
        .unwrap();
    let region = manager.get_region("us-east").await.unwrap();
    assert_eq!(region.health, RegionHealth::Degraded);
}

#[tokio::test]
async fn test_byzantine_failover_with_evidence_awareness() {
    // Scenario: Failover coordinator avoids Byzantine regions
    let config = FailoverConfig::default();
    let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

    // Primary degraded
    for _ in 0..3 {
        let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
        coordinator.update_health(result).await.unwrap();
    }

    // us-west is unhealthy (Byzantine)
    let result = create_health_result("us-west", RegionHealth::Unhealthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    // eu-west is healthy
    let result = create_health_result("eu-west", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    match decision {
        FailoverDecision::Failover { target_region, .. } => {
            // Should select eu-west (not unhealthy us-west)
            assert_eq!(target_region, "eu-west");
        }
        _ => panic!("Expected failover decision"),
    }
}

#[tokio::test]
async fn test_byzantine_region_recovery_after_successful_operations() {
    // Scenario: Byzantine region recovers after successful operations
    let manager = MultiRegionManager::with_evidence_tracking_regions(&["us-east"])
        .await
        .expect("Failed to create manager");

    // Apply penalties
    for i in 0..5 {
        manager
            .record_replication_event(
                "us-east",
                osiris_core::replication::evidence_tracker::ReplicationEvent::AckTimeout {
                    operation_id: format!("op-{}", i),
                    attempt: 1,
                },
            )
            .await
            .unwrap();
    }

    let score_before = manager
        .get_evidence_tracker()
        .unwrap()
        .get_health_score("us-east")
        .await;
    assert_eq!(score_before, Some(osiris_core::replication::evidence_tracker::HealthScore::new(50)));

    // Successful operations should recover health
    for _ in 0..10 {
        manager
            .record_replication_event(
                "us-east",
                osiris_core::replication::evidence_tracker::ReplicationEvent::WriteReplicated {
                    operation_id: "op-recovery".to_string(),
                    vector_clock_version: 1,
                },
            )
            .await
            .unwrap();
    }

    let score_after = manager
        .get_evidence_tracker()
        .unwrap()
        .get_health_score("us-east")
        .await;
    assert!(score_after.unwrap().0 > 50, "Health should recover");
}

// ============================================================================
// EDGE CASES AND STRESS TESTS
// ============================================================================

#[tokio::test]
async fn test_edge_case_empty_vector_clock_merge() {
    // Scenario: Merging empty vector clock
    let mut vc1 = VectorClock::new();
    let vc2 = VectorClock::new();

    vc1.merge(&vc2);
    assert_eq!(vc1.region_count(), 0);
}

#[tokio::test]
async fn test_edge_case_single_region_operations() {
    // Scenario: Single region cluster
    let manager = MultiRegionManager::default_with_regions(&["us-east"])
        .await
        .expect("Failed to create manager");

    manager.increment_vector_clock("us-east").await.unwrap();
    let vc = manager.get_vector_clock().await;
    assert_eq!(vc.get("us-east"), 1);
}

#[tokio::test]
async fn test_edge_case_all_regions_unhealthy() {
    // Scenario: All regions unhealthy, no failover possible
    let coordinator = FailoverCoordinator::new(FailoverConfig::default(), "us-east".to_string());

    // All regions unhealthy
    let result = create_health_result("us-east", RegionHealth::Unhealthy, 100, false);
    coordinator.update_health(result).await.unwrap();

    let result = create_health_result("us-west", RegionHealth::Unhealthy, 100, false);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await;
    assert!(decision.is_err(), "Should fail with no healthy candidates");
}

#[tokio::test]
async fn test_stress_rapid_concurrent_writes() {
    // Scenario: Rapid concurrent writes from multiple regions
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west", "eu"])
        .await
        .expect("Failed to create manager");

    // Spawn concurrent writes
    let manager_clone = manager.clone();
    let handle1 = tokio::spawn(async move {
        for _ in 0..10 {
            manager_clone.increment_vector_clock("us-east").await.unwrap();
        }
    });

    let manager_clone2 = manager.clone();
    let handle2 = tokio::spawn(async move {
        for _ in 0..10 {
            manager_clone2.increment_vector_clock("us-west").await.unwrap();
        }
    });

    let manager_clone3 = manager.clone();
    let handle3 = tokio::spawn(async move {
        for _ in 0..10 {
            manager_clone3.increment_vector_clock("eu").await.unwrap();
        }
    });

    // Wait for all writes
    let results = tokio::join!(handle1, handle2, handle3);
    assert!(results.0.is_ok());
    assert!(results.1.is_ok());
    assert!(results.2.is_ok());

    // Verify final state
    let vc = manager.get_vector_clock().await;
    assert!(vc.get("us-east") >= 10);
    assert!(vc.get("us-west") >= 10);
    assert!(vc.get("eu") >= 10);
}

#[tokio::test]
async fn test_stress_failover_cooldown_prevents_flapping() {
    // Scenario: Failover cooldown prevents rapid failover cycles
    let config = FailoverConfig {
        failover_cooldown_ms: 100, // 100ms cooldown
        ..Default::default()
    };
    let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

    // First failover
    let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();
    coordinator.execute_failover("us-west".to_string()).await.unwrap();

    // Immediately degrade new primary
    for _ in 0..3 {
        let result = create_health_result("us-west", RegionHealth::Degraded, 100, true);
        coordinator.update_health(result).await.unwrap();
    }

    // Should be in cooldown, no failover
    let decision = coordinator.evaluate_failover().await.unwrap();
    assert_eq!(decision, FailoverDecision::None);

    // Wait for cooldown
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Now failover should be possible
    let result = create_health_result("eu", RegionHealth::Healthy, 100, true);
    coordinator.update_health(result).await.unwrap();

    let decision = coordinator.evaluate_failover().await.unwrap();
    match decision {
        FailoverDecision::Failover { .. } => {
            // Expected
        }
        _ => panic!("Expected failover after cooldown"),
    }
}
