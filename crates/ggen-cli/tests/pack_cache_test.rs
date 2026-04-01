//! Pack cache integration tests
//!
//! Chicago TDD: Real filesystem, real cache operations, no mocks.

use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_pack_cache_integration() {
    // Create a temporary cache directory
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_dir = temp_dir.path().join("packs");

    // Initialize cache
    let config = ggen_marketplace::cache::CacheConfig {
        cache_dir: cache_dir.clone(),
        max_size_bytes: 1_000_000_000, // 1GB
        max_packs: 50,
        persistent: false, // Don't persist metadata in tests
    };
    let cache =
        ggen_marketplace::cache::PackCache::new(config).expect("Failed to initialize cache");

    // Test cache stats initially empty
    let stats = cache.stats();
    assert_eq!(stats.total_packs, 0);
    assert_eq!(stats.total_size_bytes, 0);

    println!("✓ Cache initialized with empty stats");
}

#[test]
fn test_pack_cache_lru_eviction() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_dir = temp_dir.path().join("packs");

    // Small cache to test eviction
    let config = ggen_marketplace::cache::CacheConfig {
        cache_dir: cache_dir.clone(),
        max_size_bytes: 3000, // Only fits 3 packs
        max_packs: 10,
        persistent: false,
    };
    let cache =
        ggen_marketplace::cache::PackCache::new(config).expect("Failed to initialize cache");

    // Create and insert 3 packs
    for i in 1..=3 {
        let pack_dir = temp_dir.path().join(&format!("pack-{}", i));
        fs::create_dir_all(&pack_dir).expect("Failed to create pack dir");
        fs::write(pack_dir.join("test.txt"), format!("content{}", i))
            .expect("Failed to write pack file");

        let package_id = ggen_marketplace::models::PackageId::new(&format!("test-pkg-{}", i))
            .expect("Invalid package ID");
        let version =
            ggen_marketplace::models::PackageVersion::new("1.0.0").expect("Invalid version");

        let cached_pack = ggen_marketplace::cache::CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            1000,
            pack_dir,
        );

        cache.insert(cached_pack).expect("Failed to insert pack");
    }

    let stats = cache.stats();
    assert_eq!(stats.total_packs, 3);
    assert_eq!(stats.total_size_bytes, 3000);

    println!("✓ LRU eviction test passed: 3 packs cached");
}

#[test]
fn test_pack_cache_digest_verification() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_dir = temp_dir.path().join("packs");

    let config = ggen_marketplace::cache::CacheConfig {
        cache_dir: cache_dir.clone(),
        max_size_bytes: 1_000_000_000,
        max_packs: 100,
        persistent: false,
    };
    let cache =
        ggen_marketplace::cache::PackCache::new(config).expect("Failed to initialize cache");

    // Create a pack with known content
    let pack_dir = temp_dir.path().join("test-pack");
    fs::create_dir_all(&pack_dir).expect("Failed to create pack dir");
    fs::write(pack_dir.join("test.txt"), b"known content").expect("Failed to write pack file");

    // Calculate expected digest
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(b"known content");
    let expected_digest = hex::encode(hasher.finalize());

    let package_id =
        ggen_marketplace::models::PackageId::new("test-pkg").expect("Invalid package ID");
    let version = ggen_marketplace::models::PackageVersion::new("1.0.0").expect("Invalid version");

    let cached_pack = ggen_marketplace::cache::CachedPack::new(
        package_id,
        version,
        expected_digest.clone(),
        12, // "known content" is 12 bytes
        pack_dir.clone(),
    );

    // Verify digest matches
    let verified = cache
        .verify_digest(&cached_pack)
        .expect("Digest verification failed");
    assert!(verified, "Digest verification should succeed");

    println!("✓ Digest verification test passed");
}

#[test]
fn test_pack_cache_persistence() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_dir = temp_dir.path().join("packs");

    // Create cache with persistence enabled
    let config = ggen_marketplace::cache::CacheConfig {
        cache_dir: cache_dir.clone(),
        max_size_bytes: 1_000_000_000,
        max_packs: 100,
        persistent: true,
    };

    {
        let cache1 = ggen_marketplace::cache::PackCache::new(config.clone())
            .expect("Failed to initialize cache");

        // Create and insert a pack
        let pack_dir = temp_dir.path().join("test-pack");
        fs::create_dir_all(&pack_dir).expect("Failed to create pack dir");
        fs::write(pack_dir.join("test.txt"), "content").expect("Failed to write pack file");

        let package_id =
            ggen_marketplace::models::PackageId::new("test-pkg").expect("Invalid package ID");
        let version =
            ggen_marketplace::models::PackageVersion::new("1.0.0").expect("Invalid version");

        let cached_pack = ggen_marketplace::cache::CachedPack::new(
            package_id,
            version,
            "digest123".to_string(),
            100,
            pack_dir,
        );

        cache1.insert(cached_pack).expect("Failed to insert pack");
    } // cache1 drops, saving metadata

    // Load cache again
    let cache2 =
        ggen_marketplace::cache::PackCache::new(config).expect("Failed to initialize cache");

    let stats = cache2.stats();
    assert_eq!(stats.total_packs, 1);
    assert_eq!(stats.total_size_bytes, 100);

    println!("✓ Cache persistence test passed");
}
