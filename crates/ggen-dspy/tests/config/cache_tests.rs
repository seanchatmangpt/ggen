//! Tests for cache manager (memory and disk)

use ggen_dspy::config::{CacheConfig, CacheManager};
use tempfile::TempDir;

async fn create_test_cache() -> (CacheManager, TempDir) {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        enable_memory: true,
        enable_disk: true,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 1_000_000,
        ttl_seconds: None,
    };

    let manager = CacheManager::new(config).unwrap();
    (manager, temp_dir)
}

#[tokio::test]
async fn test_cache_set_and_get() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("test_key".to_string(), "test_value".to_string()).await.unwrap();

    let value = manager.get("test_key").await;
    assert_eq!(value, Some("test_value".to_string()));
}

#[tokio::test]
async fn test_cache_miss() {
    let (manager, _temp_dir) = create_test_cache().await;

    let value = manager.get("nonexistent_key").await;
    assert_eq!(value, None);
}

#[tokio::test]
async fn test_cache_remove() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();
    assert!(manager.get("key1").await.is_some());

    manager.remove("key1").await.unwrap();
    assert!(manager.get("key1").await.is_none());
}

#[tokio::test]
async fn test_memory_cache_hit() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();

    // First get - memory hit
    let value1 = manager.get("key1").await;
    assert_eq!(value1, Some("value1".to_string()));

    // Second get - also memory hit
    let value2 = manager.get("key1").await;
    assert_eq!(value2, Some("value1".to_string()));

    let stats = manager.stats();
    assert_eq!(stats.memory_hits, 2);
    assert_eq!(stats.hits, 2);
    assert_eq!(stats.misses, 0);
}

#[tokio::test]
async fn test_disk_cache_promotion() {
    let (manager, _temp_dir) = create_test_cache().await;

    // Set value
    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();

    // Clear memory cache but keep disk
    if let Some(ref cache) = manager.memory_cache {
        cache.invalidate_all();
    }

    // First get should hit disk and promote to memory
    let value = manager.get("key1").await;
    assert_eq!(value, Some("value1".to_string()));

    let stats = manager.stats();
    assert_eq!(stats.disk_hits, 1);

    // Second get should hit memory
    manager.get("key1").await;
    let stats = manager.stats();
    assert_eq!(stats.memory_hits, 1);
}

#[tokio::test]
async fn test_cache_clear() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();
    manager.set("key2".to_string(), "value2".to_string()).await.unwrap();

    manager.clear().await.unwrap();

    assert!(manager.get("key1").await.is_none());
    assert!(manager.get("key2").await.is_none());
}

#[tokio::test]
async fn test_cache_statistics() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();

    // 2 hits, 1 miss
    manager.get("key1").await;
    manager.get("key1").await;
    manager.get("key2").await;

    let stats = manager.stats();
    assert_eq!(stats.hits, 2);
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hit_rate(), 2.0 / 3.0);
}

#[tokio::test]
async fn test_memory_only_cache() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        enable_memory: true,
        enable_disk: false,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 0,
        ttl_seconds: None,
    };

    let manager = CacheManager::new(config).unwrap();

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();
    let value = manager.get("key1").await;
    assert_eq!(value, Some("value1".to_string()));

    let stats = manager.stats();
    assert_eq!(stats.disk_entries, 0); // No disk cache
}

#[tokio::test]
async fn test_disk_only_cache() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        enable_memory: false,
        enable_disk: true,
        memory_max_entries: 0,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 1_000_000,
        ttl_seconds: None,
    };

    let manager = CacheManager::new(config).unwrap();

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();
    let value = manager.get("key1").await;
    assert_eq!(value, Some("value1".to_string()));

    let stats = manager.stats();
    assert_eq!(stats.memory_entries, 0); // No memory cache
    assert_eq!(stats.disk_hits, 1);
}

#[tokio::test]
async fn test_cache_reset_stats() {
    let (manager, _temp_dir) = create_test_cache().await;

    manager.set("key1".to_string(), "value1".to_string()).await.unwrap();
    manager.get("key1").await;

    let stats_before = manager.stats();
    assert!(stats_before.hits > 0);

    manager.reset_stats();

    let stats_after = manager.stats();
    assert_eq!(stats_after.hits, 0);
    assert_eq!(stats_after.misses, 0);
}
