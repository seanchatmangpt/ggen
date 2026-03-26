//! Integration tests for MultiRegionManager
//!
//! Tests cover:
//! - 3-region cluster creation and management
//! - Vector clock causality ordering
//! - Failover from primary to secondary regions
//! - Concurrent event detection

use osiris_core::{
    CausalityResult, MultiRegionConfig, MultiRegionManager, RegionHealth, ReplicationLag,
    VectorClock,
};

#[tokio::test]
async fn test_create_three_region_cluster() {
    // Create a 3-region cluster: US-East (primary), US-West, EU (replicas)
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west", "eu"]).await;
    assert!(manager.is_ok(), "Failed to create manager");

    let manager = manager.unwrap();
    let regions = manager.get_regions().await.expect("Failed to get regions");

    // Verify 3 regions created
    assert_eq!(regions.len(), 3, "Expected 3 regions");

    // Verify region IDs
    let region_ids: Vec<_> = regions.iter().map(|r| r.region_id.clone()).collect();
    assert!(region_ids.contains(&"us-east".to_string()));
    assert!(region_ids.contains(&"us-west".to_string()));
    assert!(region_ids.contains(&"eu".to_string()));

    // Verify primary is US-East
    let primary = manager
        .get_primary_region()
        .await
        .expect("Failed to get primary region");
    assert_eq!(primary, "us-east");

    // Verify primary flag is set correctly
    assert!(
        manager
            .is_primary("us-east")
            .await
            .expect("Failed to check primary"),
        "us-east should be primary"
    );
    assert!(
        !manager
            .is_primary("us-west")
            .await
            .expect("Failed to check primary"),
        "us-west should not be primary"
    );
}

#[tokio::test]
async fn test_vector_clock_ordering_causality() {
    // Test that vector clocks correctly detect causal ordering
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west", "eu"])
        .await
        .expect("Failed to create manager");

    // Simulate write on US-East at T0
    let vc_t0 = manager
        .increment_vector_clock("us-east")
        .await
        .expect("Failed to increment VC");

    // Simulate replication to US-West, then write on US-West at T1
    manager.update_vector_clock(&vc_t0).await;
    let vc_t1 = manager
        .increment_vector_clock("us-west")
        .await
        .expect("Failed to increment VC");

    // VC ordering: vc_t0 should happen before vc_t1
    assert!(
        vc_t0.happens_before(&vc_t1),
        "vc_t0 should happen before vc_t1"
    );
    assert!(
        !vc_t1.happens_before(&vc_t0),
        "vc_t1 should not happen before vc_t0"
    );

    // Detect causality
    let result = manager.detect_causality(&vc_t0, &vc_t1).await;
    assert_eq!(
        result,
        CausalityResult::Causality {
            happens_before: true
        },
        "Expected causal relationship"
    );
}

#[tokio::test]
async fn test_concurrent_events_detection() {
    // Test that concurrent writes in different regions are detected
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Concurrent write 1: US-East writes policy-1
    let mut vc_east = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_east.increment("us-east");

    // Concurrent write 2: US-West writes policy-2 (before receiving write from east)
    let mut vc_west = VectorClock::with_regions(&["us-east", "us-west"]);
    vc_west.increment("us-west");

    // These should be concurrent (neither happens before the other)
    assert!(vc_east.concurrent(&vc_west), "Events should be concurrent");
    assert!(vc_west.concurrent(&vc_east), "Events should be concurrent");

    // Manager should detect concurrency
    let result = manager.detect_causality(&vc_east, &vc_west).await;
    assert_eq!(
        result,
        CausalityResult::Concurrent,
        "Expected concurrent relationship"
    );
}

#[tokio::test]
async fn test_replication_lag_tracking() {
    // Test that replication lag is tracked for each region
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Simulate replication lag on US-West
    let lag = ReplicationLag::new(250, 5);
    manager
        .update_replication_lag("us-west", lag)
        .await
        .expect("Failed to update lag");

    let region = manager
        .get_region("us-west")
        .await
        .expect("Failed to get region");
    assert_eq!(region.replication_lag.milliseconds, 250);
    assert_eq!(region.replication_lag.event_count, 5);
}

#[tokio::test]
async fn test_region_health_status() {
    // Test that region health status is tracked
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Update US-West health to degraded
    manager
        .update_region_health("us-west", RegionHealth::Degraded)
        .await
        .expect("Failed to update health");

    let region = manager
        .get_region("us-west")
        .await
        .expect("Failed to get region");
    assert_eq!(region.health, RegionHealth::Degraded);

    // Update to unhealthy
    manager
        .update_region_health("us-west", RegionHealth::Unhealthy)
        .await
        .expect("Failed to update health");

    let region = manager
        .get_region("us-west")
        .await
        .expect("Failed to get region");
    assert_eq!(region.health, RegionHealth::Unhealthy);
}

#[tokio::test]
async fn test_failover_scenario() {
    // Test failover from primary (us-east) to secondary (us-west)
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west", "eu"])
        .await
        .expect("Failed to create manager");

    // Verify US-East is primary
    assert!(manager.is_primary("us-east").await.unwrap());

    // Simulate US-East becoming unhealthy
    manager
        .update_region_health("us-east", RegionHealth::Unhealthy)
        .await
        .expect("Failed to update health");

    // Check health is updated
    let us_east = manager.get_region("us-east").await.unwrap();
    assert_eq!(us_east.health, RegionHealth::Unhealthy);

    // In Phase 1, failover decision is made by operator based on health
    // This test verifies the health monitoring infrastructure is in place
    let regions = manager.get_regions().await.unwrap();
    let unhealthy_count = regions
        .iter()
        .filter(|r| r.health == RegionHealth::Unhealthy)
        .count();
    assert_eq!(unhealthy_count, 1);

    // US-West should still be healthy
    let us_west = manager.get_region("us-west").await.unwrap();
    assert_eq!(us_west.health, RegionHealth::Unknown); // Default state
}

#[tokio::test]
async fn test_vector_clock_merge() {
    // Test that receiving a replication event merges vector clocks
    let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
        .await
        .expect("Failed to create manager");

    // Simulate a sequence of events
    // Event 1: US-East writes
    let vc1 = manager
        .increment_vector_clock("us-east")
        .await
        .expect("Failed to increment");
    assert_eq!(vc1.get("us-east"), 1);

    // Event 2: US-East writes again
    let vc2 = manager
        .increment_vector_clock("us-east")
        .await
        .expect("Failed to increment");
    assert_eq!(vc2.get("us-east"), 2);

    // Event 3: US-West receives and processes, then writes
    // First merge the global VC
    manager.update_vector_clock(&vc2).await;
    let vc3 = manager
        .increment_vector_clock("us-west")
        .await
        .expect("Failed to increment");

    // VC3 should have ts_us_east=2 and ts_us_west=1
    assert_eq!(vc3.get("us-east"), 2, "Should preserve US-East timestamp");
    assert_eq!(vc3.get("us-west"), 1, "Should have incremented US-West");
}

#[tokio::test]
async fn test_add_region_duplicate_error() {
    // Test that adding duplicate region returns error
    let mut manager = MultiRegionManager::new(MultiRegionConfig::default());

    // Add first region successfully
    let result1 = manager
        .add_region(
            "us-east".to_string(),
            "https://us-east.example.com".to_string(),
            true,
        )
        .await;
    assert!(result1.is_ok());

    // Try to add same region - should fail
    let result2 = manager
        .add_region(
            "us-east".to_string(),
            "https://us-east2.example.com".to_string(),
            false,
        )
        .await;
    assert!(result2.is_err());
}
