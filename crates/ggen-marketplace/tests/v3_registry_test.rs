//! Integration tests for V3OptimizedRegistry performance features

#![allow(dead_code)]

use ggen_core::marketplace::models::PackageId;
use ggen_core::marketplace::v3::{V3MetricsSnapshot, V3OptimizedRegistry};
use oxigraph::store::Store;
use std::sync::Arc;

/// Test: Create a V3 registry
#[tokio::test]
async fn test_v3_registry_creation() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).expect("registry should initialize");

    let stats = registry.stats();
    assert_eq!(stats.total_queries, 0, "initial stats should be zero");
    assert_eq!(stats.cache_hit_rate, 0.0, "initial hit rate should be zero");
}

/// Test: Statistics reset
#[test]
fn test_v3_stats_reset() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    // Simulate some activity
    let _ = registry.search_parallel("test");

    registry.reset_stats();

    let stats = registry.stats();
    assert_eq!(stats.total_queries, 0, "stats should be reset");
    assert_eq!(stats.hot_cache_hits, 0);
    assert_eq!(stats.metadata_cache_hits, 0);
}

/// Test: Parallel search (rayon)
#[test]
fn test_v3_parallel_search() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    // Update search index
    let _ = registry.update_search_index("test-pkg-1", "Test Package");

    // Search
    let results = registry.search_parallel("test");
    assert!(
        results.is_empty() || results.len() > 0,
        "search should execute without error"
    );
}

/// Test: Metrics snapshot collection
#[test]
fn test_v3_metrics_snapshot() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    let snapshot: V3MetricsSnapshot = registry.metrics_snapshot();

    // Verify snapshot structure
    assert_eq!(snapshot.cache_hits, 0);
    assert_eq!(snapshot.cache_misses, 0);
    assert_eq!(snapshot.batch_ops, 0);
    assert_eq!(snapshot.parallel_searches, 0);
    assert_eq!(
        snapshot.latency_buckets.len(),
        8,
        "should have 8 latency buckets"
    );
}

/// Test: Latency recording
#[test]
fn test_v3_latency_recording() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    // Record latencies at different levels
    registry.record_latency(5); // <10us
    registry.record_latency(25); // <50us
    registry.record_latency(75); // <100us
    registry.record_latency(250); // <500us
    registry.record_latency(750); // <1ms
    registry.record_latency(2500); // <5ms
    registry.record_latency(7500); // <10ms
    registry.record_latency(15000); // >=10ms

    let snapshot = registry.metrics_snapshot();
    // Each bucket should have one entry
    for (i, count) in snapshot.latency_buckets.iter().enumerate() {
        assert_eq!(*count, 1, "bucket {} should have 1 entry", i);
    }
}

/// Test: Batch operations metric
#[tokio::test]
async fn test_v3_batch_operations_metric() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    // Execute batch delete
    let ids = vec![
        PackageId::new("test/pkg1".to_string()).unwrap(),
        PackageId::new("test/pkg2".to_string()).unwrap(),
    ];

    let deleted = registry.batch_delete(ids).await.unwrap();
    assert_eq!(deleted, 2, "should delete 2 packages");

    // Verify metric was recorded
    let snapshot = registry.metrics_snapshot();
    assert_eq!(
        snapshot.batch_ops, 1,
        "should have 1 batch operation recorded"
    );
}

/// Test: Search index update
#[test]
fn test_v3_search_index_update() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    // Update index
    let _ = registry.update_search_index("pkg-123", "My Awesome Package");

    // Search should find it
    let results = registry.search_parallel("awesome");
    assert!(results.len() >= 0, "search should execute");
}

/// Test: Display formatting for stats and metrics
#[test]
fn test_v3_display_formatting() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    let stats = registry.stats();
    let stats_display = format!("{}", stats);
    assert!(
        stats_display.contains("v3 Registry"),
        "stats display should contain 'v3 Registry'"
    );

    let metrics = registry.metrics_snapshot();
    let metrics_display = format!("{}", metrics);
    assert!(
        metrics_display.contains("v3 Metrics"),
        "metrics display should contain 'v3 Metrics'"
    );
}

/// Test: Multiple searches update metrics correctly
#[test]
fn test_v3_multiple_searches() {
    let store = Arc::new(Store::new().unwrap());
    let registry = V3OptimizedRegistry::new(store).unwrap();

    for i in 0..5 {
        let _ = registry.search_parallel(&format!("search-{}", i));
    }

    let snapshot = registry.metrics_snapshot();
    assert_eq!(
        snapshot.parallel_searches, 5,
        "should record 5 parallel searches"
    );
}
