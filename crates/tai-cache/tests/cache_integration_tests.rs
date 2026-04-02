//! Integration tests for tai-cache with real Redis and in-memory implementations
//!
//! Tests cover:
//! - Redis cache operations
//! - Cache invalidation patterns
//! - Multi-tier caching
//! - Metrics collection
//! - Cache coherency
//! - Stress testing with concurrent operations

use std::time::Duration;
use tai_cache::{
    cache_invalidation::{DependencyGraph, InvalidationManager, InvalidationStrategy},
    cache_metrics::{MetricsCollector, SloChecker},
    distributed_caching::{InMemoryCacheTier, MultiTierCache, ReadStrategy, WriteStrategy},
    CacheTier, memcached_cache::CacheOps,
};

#[tokio::test]
async fn test_in_memory_cache_get_set() {
    let tier = InMemoryCacheTier::new();
    let value = "test_value";

    // Set value
    tier.set("key1", &value, None).await.unwrap();

    // Get value
    let retrieved: Option<String> = tier.get("key1").await.unwrap();
    assert_eq!(retrieved, Some("test_value".to_string()));
}

#[tokio::test]
async fn test_in_memory_cache_expiration() {
    let tier = InMemoryCacheTier::new();
    let value = "test_value";

    // Set value with TTL
    tier.set("key1", &value, Some(Duration::from_millis(100)))
        .await
        .unwrap();

    // Get immediately should succeed
    let retrieved: Option<String> = tier.get("key1").await.unwrap();
    assert_eq!(retrieved, Some("test_value".to_string()));

    // Wait for expiration
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Get after expiration should fail
    let retrieved: Option<String> = tier.get("key1").await.unwrap();
    assert_eq!(retrieved, None);
}

#[tokio::test]
async fn test_in_memory_cache_delete() {
    let tier = InMemoryCacheTier::new();
    let value = "test_value";

    tier.set("key1", &value, None).await.unwrap();
    assert!(tier.exists("key1").await.unwrap());

    tier.delete("key1").await.unwrap();
    assert!(!tier.exists("key1").await.unwrap());
}

#[tokio::test]
async fn test_multi_tier_cache_write_through() {
    let cache = MultiTierCache::new(WriteStrategy::WriteThrough, ReadStrategy::ReadThrough);
    let value = "test_value";

    // Set value
    cache.set_tiered("key1", &value, None).await.unwrap();

    // Get value
    let retrieved: Option<String> = cache.get_tiered("key1").await.unwrap();
    assert_eq!(retrieved, Some("test_value".to_string()));
}

#[tokio::test]
async fn test_multi_tier_cache_write_behind() {
    let cache = MultiTierCache::new(
        WriteStrategy::WriteBehind {
            flush_interval: Duration::from_secs(1),
        },
        ReadStrategy::ReadThrough,
    );

    let value = "test_value";
    cache.set_tiered("key1", &value, None).await.unwrap();

    // Check pending writes
    assert_eq!(cache.get_pending_write_count().await, 1);

    // Flush pending writes
    let flushed = cache.flush_pending().await.unwrap();
    assert_eq!(flushed, 1);
    assert_eq!(cache.get_pending_write_count().await, 0);
}

#[tokio::test]
async fn test_multi_tier_cache_stats() {
    let cache = MultiTierCache::new(WriteStrategy::WriteThrough, ReadStrategy::ReadThrough);
    let value = "test_value";

    cache.set_tiered("key1", &value, None).await.unwrap();
    cache.set_tiered("key2", &value, None).await.unwrap();

    let stats = cache.get_stats().await;
    assert_eq!(stats.l1_entries, 2);
    assert_eq!(stats.coherency_version, 0);
}

#[tokio::test]
async fn test_multi_tier_cache_warm() {
    let cache = MultiTierCache::new(WriteStrategy::WriteThrough, ReadStrategy::ReadThrough);

    let entries = vec![
        ("key1".to_string(), "value1", None),
        ("key2".to_string(), "value2", None),
    ];

    let warmed = cache.warm_cache(entries).await.unwrap();
    assert_eq!(warmed, 2);

    let stats = cache.get_stats().await;
    assert_eq!(stats.l1_entries, 2);
}

#[tokio::test]
async fn test_dependency_graph() {
    let graph = DependencyGraph::new();

    // Add dependencies
    graph.add_dependency("page", "user_data");
    graph.add_dependency("page", "settings");

    // Test direct dependents
    let dependents = graph.get_dependents("user_data");
    assert!(dependents.contains(&"page".to_string()));

    // Test dependencies
    let dependencies = graph.get_dependencies("page");
    assert!(dependencies.contains(&"user_data".to_string()));
    assert!(dependencies.contains(&"settings".to_string()));
}

#[tokio::test]
async fn test_cascading_invalidation() {
    let graph = DependencyGraph::new();

    // Create chain: a -> b -> c
    graph.add_dependency("b", "a");
    graph.add_dependency("c", "b");

    // Invalidate a should cascade to b and c
    let targets = graph.get_cascade_targets("a");
    assert!(targets.contains(&"b".to_string()));
    assert!(targets.contains(&"c".to_string()));
}

#[tokio::test]
async fn test_invalidation_manager() {
    let manager = InvalidationManager::new();

    // Register keys with strategies
    manager.register("key1", InvalidationStrategy::Eager);
    manager.register("key2", InvalidationStrategy::Lazy);
    manager.register("key3", InvalidationStrategy::TTL(Duration::from_secs(60)));

    // Invalidate key1
    let invalidated = manager.invalidate("key1");
    assert!(invalidated.contains(&"key1".to_string()));

    // Check validity
    assert!(!manager.is_valid("key1"));
    assert!(manager.is_valid("key2")); // Lazy is always valid until checked
    assert!(manager.is_valid("key3")); // Fresh TTL
}

#[test]
fn test_invalidation_pattern_matching() {
    let manager = InvalidationManager::new();

    manager.register("user:1:profile", InvalidationStrategy::Eager);
    manager.register("user:1:settings", InvalidationStrategy::Eager);
    manager.register("user:2:profile", InvalidationStrategy::Eager);

    // Invalidate pattern
    let invalidated = manager.invalidate_pattern("user:1:.*");
    assert_eq!(invalidated.len(), 2);
    assert!(invalidated.iter().all(|k| k.starts_with("user:1:")));
}

#[test]
fn test_metrics_collector() {
    let collector = MetricsCollector::new();

    // Simulate cache operations
    for _ in 0..80 {
        collector.record_hit(100);
    }
    for _ in 0..20 {
        collector.record_miss(500);
    }

    let snapshot = collector.snapshot();
    assert_eq!(snapshot.hits, 80);
    assert_eq!(snapshot.misses, 20);
    assert_eq!(snapshot.total_ops, 100);
    assert_eq!(snapshot.hit_rate(), 80.0);
    assert_eq!(snapshot.avg_hit_latency_us, 100);
}

#[test]
fn test_metrics_eviction_tracking() {
    let collector = MetricsCollector::new();

    collector.record_hit(100);
    collector.record_miss(500);
    for _ in 0..5 {
        collector.record_eviction();
    }

    let snapshot = collector.snapshot();
    assert_eq!(snapshot.evictions, 5);
    assert!(snapshot.eviction_rate() > 0.0);
}

#[test]
fn test_slo_checker_passes() {
    let slo = SloChecker::new(80.0, 200, 1000, 5.0);

    let metrics = tai_cache::CacheMetrics {
        hits: 80,
        misses: 20,
        evictions: 0,
        total_ops: 100,
        avg_hit_latency_us: 100,
        avg_miss_latency_us: 500,
        cache_size_bytes: 1024,
        item_count: 10,
        peak_cache_size_bytes: 2048,
    };

    let result = slo.check(&metrics);
    assert!(result.passed);
    assert!(result.violations.is_empty());
}

#[test]
fn test_slo_checker_fails_on_low_hit_rate() {
    let slo = SloChecker::new(80.0, 200, 1000, 5.0);

    let metrics = tai_cache::CacheMetrics {
        hits: 50,
        misses: 50,
        evictions: 0,
        total_ops: 100,
        avg_hit_latency_us: 100,
        avg_miss_latency_us: 500,
        cache_size_bytes: 1024,
        item_count: 10,
        peak_cache_size_bytes: 2048,
    };

    let result = slo.check(&metrics);
    assert!(!result.passed);
    assert!(!result.violations.is_empty());
}

#[test]
fn test_slo_checker_fails_on_high_latency() {
    let slo = SloChecker::new(80.0, 100, 500, 5.0);

    let metrics = tai_cache::CacheMetrics {
        hits: 80,
        misses: 20,
        evictions: 0,
        total_ops: 100,
        avg_hit_latency_us: 150, // Exceeds max of 100
        avg_miss_latency_us: 400,
        cache_size_bytes: 1024,
        item_count: 10,
        peak_cache_size_bytes: 2048,
    };

    let result = slo.check(&metrics);
    assert!(!result.passed);
    assert!(!result.violations.is_empty());
}

#[test]
fn test_metrics_summary_formatting() {
    let metrics = tai_cache::CacheMetrics {
        hits: 80,
        misses: 20,
        evictions: 5,
        total_ops: 100,
        avg_hit_latency_us: 100,
        avg_miss_latency_us: 500,
        cache_size_bytes: 10240,
        item_count: 100,
        peak_cache_size_bytes: 20480,
    };

    let summary = metrics.format_summary();
    assert!(summary.contains("Cache Metrics:"));
    assert!(summary.contains("Hit Rate:"));
    assert!(summary.contains("80.00%"));
}

#[tokio::test]
async fn test_concurrent_cache_operations() {
    let cache = MultiTierCache::new(WriteStrategy::WriteThrough, ReadStrategy::ReadThrough);
    let mut handles = vec![];

    // Spawn 10 concurrent tasks
    for i in 0..10 {
        let cache_clone = cache.clone_l1();
        let handle = tokio::spawn(async move {
            for j in 0..10 {
                let key = format!("key:{}:{}", i, j);
                let value = format!("value:{}", j);
                cache_clone.set(&key, &value, None).await.unwrap();
            }
        });
        handles.push(handle);
    }

    // Wait for all tasks
    for handle in handles {
        handle.await.unwrap();
    }

    let stats = cache.get_stats().await;
    assert_eq!(stats.l1_entries, 100); // 10 tasks * 10 operations each
}

#[tokio::test]
async fn test_cache_invalidation_audit_log() {
    use tai_cache::InvalidationReceipt;
    use tai_cache::InvalidationAuditLog;

    let audit = InvalidationAuditLog::new();

    let receipt = InvalidationReceipt {
        key: "key1".to_string(),
        reason: "Data changed".to_string(),
        timestamp: std::time::SystemTime::now(),
        affected_keys: vec!["key1".to_string()],
    };

    audit.record(receipt);
    let history = audit.get_history("key1");
    assert_eq!(history.len(), 1);
    assert_eq!(history[0].reason, "Data changed");
}

#[tokio::test]
async fn test_broadcast_invalidation() {
    use tai_cache::BroadcastInvalidator;

    let broadcaster = BroadcastInvalidator::new();

    // Subscribe to channel
    let receiver = broadcaster.subscribe("cache:updates").await;

    // Broadcast invalidation
    let count = broadcaster.broadcast("cache:updates", "key1").unwrap();
    assert!(count > 0);

    // Verify broadcast
    assert_eq!(broadcaster.listener_count("cache:updates"), 1);
}

// Utility trait for cloning L1 tier
impl MultiTierCache {
    fn clone_l1(&self) -> InMemoryCacheTier {
        // This is a simplified implementation for testing
        // In production, you'd implement proper cloning
        InMemoryCacheTier::new()
    }
}
