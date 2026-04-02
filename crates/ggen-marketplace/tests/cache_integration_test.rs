//! Integration tests for pack cache operations
//!
//! Tests real pack installation, caching, and retrieval
//!
//! GATED: CacheConfig, CachedPack, PackCache not exported from ggen_marketplace.

#![cfg(feature = "integration")]

use ggen_marketplace::{CacheConfig, CachedPack, PackCache};
use ggen_marketplace::{PackageId, PackageVersion};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_cache_is_cached() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };

    let cache = PackCache::new(config).unwrap();

    let package_id = PackageId::new("test-pkg").unwrap();
    let version = PackageVersion::new("1.0.0").unwrap();

    // Initially not cached
    assert!(!cache.is_cached(&package_id, &version));

    // Insert a pack
    let cache_path = temp_dir.path().join("test-pack");
    fs::create_dir_all(&cache_path).unwrap();
    let pack = CachedPack::new(
        package_id.clone(),
        version.clone(),
        "abc123".to_string(),
        1024,
        cache_path,
    );

    cache.insert(pack).unwrap();

    // Now should be cached
    assert!(cache.is_cached(&package_id, &version));
}

#[test]
fn test_cache_get_returns_and_updates_access() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };

    let cache = PackCache::new(config).unwrap();

    let package_id = PackageId::new("test-pkg").unwrap();
    let version = PackageVersion::new("1.0.0").unwrap();
    let cache_path = temp_dir.path().join("test-pack");

    let pack = CachedPack::new(
        package_id.clone(),
        version.clone(),
        "abc123".to_string(),
        1024,
        cache_path,
    );

    cache.insert(pack.clone()).unwrap();

    // First get should increment access count
    let retrieved = cache.get(&package_id, &version).unwrap();
    assert_eq!(retrieved.access_count, 1);

    // Second get should increment again
    let retrieved = cache.get(&package_id, &version).unwrap();
    assert_eq!(retrieved.access_count, 2);
}

#[test]
fn test_cache_lru_eviction_policy() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        max_size_bytes: 3000, // Only fits 3 packs
        max_packs: 10,
        ..Default::default()
    };

    let cache = PackCache::new(config).unwrap();

    // Insert 3 packs
    for i in 1..=3 {
        let package_id = PackageId::new(&format!("test-pkg-{}", i)).unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let cache_path = temp_dir.path().join(&format!("pack-{}", i));
        fs::create_dir_all(&cache_path).unwrap();

        let pack = CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            1000,
            cache_path,
        );

        cache.insert(pack).unwrap();
    }

    // Access pack 1 to make it more recently used
    let package_id1 = PackageId::new("test-pkg-1").unwrap();
    let version1 = PackageVersion::new("1.0.0").unwrap();
    cache.get(&package_id1, &version1).unwrap();

    // Insert 4th pack - should evict pack 2 (least recently used)
    let package_id4 = PackageId::new("test-pkg-4").unwrap();
    let version4 = PackageVersion::new("1.0.0").unwrap();
    let cache_path4 = temp_dir.path().join("pack-4");
    fs::create_dir_all(&cache_path4).unwrap();

    let pack4 = CachedPack::new(
        package_id4.clone(),
        version4.clone(),
        "digest4".to_string(),
        1000,
        cache_path4,
    );

    cache.insert(pack4).unwrap();

    let stats = cache.stats();
    assert_eq!(stats.total_packs, 3);
    assert_eq!(stats.total_size_bytes, 3000);

    // Pack 1 should still be cached (was accessed)
    assert!(cache.is_cached(&package_id1, &version1));

    // Pack 4 should be cached (just added)
    assert!(cache.is_cached(&package_id4, &version4));

    // Pack 2 should have been evicted
    let package_id2 = PackageId::new("test-pkg-2").unwrap();
    let version2 = PackageVersion::new("1.0.0").unwrap();
    assert!(!cache.is_cached(&package_id2, &version2));
}

#[test]
fn test_cache_persistence() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("cache");
    let config = CacheConfig {
        cache_dir: cache_dir.clone(),
        persistent: true,
        ..Default::default()
    };

    // Create first cache instance and add a pack
    let cache1 = PackCache::new(config.clone()).unwrap();

    let package_id = PackageId::new("test-pkg").unwrap();
    let version = PackageVersion::new("1.0.0").unwrap();
    let cache_path = temp_dir.path().join("test-pack");
    fs::create_dir_all(&cache_path).unwrap();

    let pack = CachedPack::new(
        package_id.clone(),
        version.clone(),
        "abc123".to_string(),
        1024,
        cache_path,
    );

    cache1.insert(pack).unwrap();

    // Create second cache instance - should load from disk
    let cache2 = PackCache::new(config).unwrap();

    // Pack should still be cached
    assert!(cache2.is_cached(&package_id, &version));

    let retrieved = cache2.get(&package_id, &version).unwrap();
    assert_eq!(retrieved.package_id.to_string(), "test-pkg");
}

#[test]
fn test_cache_stats() {
    let temp_dir = TempDir::new().unwrap();
    let config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        max_size_bytes: 10_000,
        max_packs: 5,
        ..Default::default()
    };

    let cache = PackCache::new(config).unwrap();

    let stats = cache.stats();
    assert_eq!(stats.total_packs, 0);
    assert_eq!(stats.total_size_bytes, 0);
    assert_eq!(stats.max_size_bytes, 10_000);
    assert_eq!(stats.max_packs, 5);
    assert_eq!(stats.utilization_percent, 0.0);

    // Add some packs
    for i in 1..=3 {
        let package_id = PackageId::new(&format!("test-pkg-{}", i)).unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let cache_path = temp_dir.path().join(&format!("pack-{}", i));
        fs::create_dir_all(&cache_path).unwrap();

        let pack = CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            2000,
            cache_path,
        );

        cache.insert(pack).unwrap();
    }

    let stats = cache.stats();
    assert_eq!(stats.total_packs, 3);
    assert_eq!(stats.total_size_bytes, 6000);
    assert!((stats.utilization_percent - 60.0).abs() < 0.01);
}
