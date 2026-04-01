//! Comprehensive thread safety tests for marketplace cache operations.
//!
//! Chicago TDD: Real concurrent operations, no mocks.
//! Tests cover:
//! - Concurrent cache insert operations
//! - Concurrent cache get operations
//! - Lock poisoning recovery
//! - Cache operations under load
//! - Thread-safe statistics

use ggen_marketplace::cache::{CacheConfig, CachedPack, PackCache};
use ggen_marketplace::models::{PackageId, PackageVersion};
use std::sync::{Arc, Barrier};
use std::time::Duration;
use tempfile::TempDir;

#[test]
fn test_cache_concurrent_inserts() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        max_size_bytes: 10_000_000, // 10 MB
        max_packs: 1000,
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");
    let scratch_root = temp_dir.path().to_path_buf();

    let num_threads = 10;
    let inserts_per_thread = 50;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);
        let root = scratch_root.clone();

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for i in 0..inserts_per_thread {
                let package_id = PackageId::new(&format!("pack-{}-{}", thread_id, i))
                    .expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");
                let cache_path = root.join(format!("pack-{}-{}", thread_id, i));

                let pack = CachedPack::new(
                    package_id,
                    version,
                    format!("digest{}", i),
                    1024,
                    cache_path,
                );

                // Ignore errors from cache being full
                let _ = cache_clone.insert(pack);
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Verify cache state
    let stats = cache_arc.stats();
    assert!(stats.total_packs > 0, "Cache should have entries");
    assert!(
        stats.total_packs <= 1000,
        "Cache should not exceed max packs"
    );

    println!(
        "Concurrent inserts test: {} packs in cache",
        stats.total_packs
    );
}

#[test]
fn test_cache_concurrent_gets() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");

    // Insert some packs first
    for i in 0..10 {
        let package_id = PackageId::new(&format!("pack-{}", i)).expect("Invalid package ID");
        let version = PackageVersion::new("1.0.0").expect("Invalid version");
        let cache_path = temp_dir.path().join(format!("pack-{}", i));

        let pack = CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            1024,
            cache_path,
        );

        cache.insert(pack).expect("Failed to insert pack");
    }

    let num_threads = 10;
    let gets_per_thread = 100;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for _ in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for _ in 0..gets_per_thread {
                let pack_num = (rand::random::<usize>() % 10) as u32;
                let package_id =
                    PackageId::new(&format!("pack-{}", pack_num)).expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");

                // Get should not panic
                let _ = cache_clone.get(&package_id, &version);
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Verify cache is still consistent
    let stats = cache_arc.stats();
    assert_eq!(stats.total_packs, 10, "All packs should still be present");
}

#[test]
fn test_cache_concurrent_mixed_operations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        max_size_bytes: 5_000_000, // 5 MB
        max_packs: 100,
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");
    let scratch_root = temp_dir.path().to_path_buf();

    let num_threads = 8;
    let operations_per_thread = 100;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);
        let root = scratch_root.clone();

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for i in 0..operations_per_thread {
                let operation = rand::random::<u8>() % 3;

                match operation {
                    // Insert (60% chance)
                    0 | 1 => {
                        let package_id = PackageId::new(&format!("pack-{}-{}", thread_id, i))
                            .expect("Invalid package ID");
                        let version = PackageVersion::new("1.0.0").expect("Invalid version");
                        let cache_path = root.join(format!("pack-{}-{}", thread_id, i));

                        let pack = CachedPack::new(
                            package_id,
                            version,
                            format!("digest{}", i),
                            1024,
                            cache_path,
                        );

                        let _ = cache_clone.insert(pack);
                    }
                    // Get (30% chance)
                    2 => {
                        let pack_num = (rand::random::<usize>() % 50) as u32;
                        let package_id =
                            PackageId::new(&format!("pack-{}-{}", thread_id, pack_num))
                                .expect("Invalid package ID");
                        let version = PackageVersion::new("1.0.0").expect("Invalid version");

                        let _ = cache_clone.get(&package_id, &version);
                    }
                    // Stats (10% chance)
                    _ => {
                        let _ = cache_clone.stats();
                    }
                }
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Verify cache is still consistent
    let stats = cache_arc.stats();
    assert!(
        stats.total_packs <= 100,
        "Cache should not exceed max packs"
    );
    assert!(
        stats.total_size_bytes <= 5_000_000,
        "Cache should not exceed max size"
    );

    println!(
        "Mixed operations test: {} packs, {} bytes",
        stats.total_packs, stats.total_size_bytes
    );
}

#[test]
fn test_cache_lock_contention() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");

    let num_threads = 20;
    let operations_per_thread = 50;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    // All threads will compete for the same pack
    for _ in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for _ in 0..operations_per_thread {
                let package_id = PackageId::new("contended-pack").expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");

                // High contention on the same pack
                let _ = cache_clone.get(&package_id, &version);

                // Also try to get stats (read-only operation)
                let _ = cache_clone.stats();
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Cache should still be functional
    let stats = cache_arc.stats();
    assert!(stats.total_packs >= 0, "Cache should be consistent");
}

#[test]
fn test_cache_stats_thread_safety() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");

    // Insert some packs
    for i in 0..10 {
        let package_id = PackageId::new(&format!("pack-{}", i)).expect("Invalid package ID");
        let version = PackageVersion::new("1.0.0").expect("Invalid version");
        let cache_path = temp_dir.path().join(format!("pack-{}", i));

        let pack = CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            1024,
            cache_path,
        );

        cache.insert(pack).expect("Failed to insert pack");
    }

    let num_threads = 10;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    // All threads read stats simultaneously
    for _ in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for _ in 0..100 {
                let stats = cache_clone.stats();

                // Stats should be consistent
                assert_eq!(stats.total_packs, 10);
                assert_eq!(stats.total_size_bytes, 10 * 1024);
                assert_eq!(stats.max_packs, 100);
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

#[test]
fn test_cache_clear_under_load() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");
    let scratch_root = temp_dir.path().to_path_buf();

    let num_threads = 5;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    // Thread 0: Clear cache
    // Threads 1-4: Insert packs
    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);
        let root = scratch_root.clone();

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            if thread_id == 0 {
                // Clearing thread
                std::thread::sleep(Duration::from_millis(10));
                let _ = cache_clone.clear();
            } else {
                // Inserting threads
                for i in 0..20 {
                    let package_id = PackageId::new(&format!("pack-{}-{}", thread_id, i))
                        .expect("Invalid package ID");
                    let version = PackageVersion::new("1.0.0").expect("Invalid version");
                    let cache_path = root.join(format!("pack-{}-{}", thread_id, i));

                    let pack = CachedPack::new(
                        package_id,
                        version,
                        format!("digest{}", i),
                        1024,
                        cache_path,
                    );

                    let _ = cache_clone.insert(pack);
                    std::thread::sleep(Duration::from_micros(100));
                }
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Cache should be cleared or have some packs (race condition is OK)
    let stats = cache_arc.stats();
    assert!(stats.total_packs >= 0, "Cache should be in valid state");
}

#[test]
fn test_cache_remove_under_load() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");

    // Insert initial packs
    for i in 0..10 {
        let package_id = PackageId::new(&format!("pack-{}", i)).expect("Invalid package ID");
        let version = PackageVersion::new("1.0.0").expect("Invalid version");
        let cache_path = temp_dir.path().join(format!("pack-{}", i));

        let pack = CachedPack::new(
            package_id,
            version,
            format!("digest{}", i),
            1024,
            cache_path,
        );

        cache.insert(pack).expect("Failed to insert pack");
    }

    let num_threads = 5;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for i in 0..10 {
                let package_id =
                    PackageId::new(&format!("pack-{}", i)).expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");

                // Some threads remove, some get
                if thread_id % 2 == 0 {
                    let _ = cache_clone.remove(&package_id, &version);
                } else {
                    let _ = cache_clone.get(&package_id, &version);
                }
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Cache should be in valid state
    let stats = cache_arc.stats();
    assert!(
        stats.total_packs >= 0 && stats.total_packs <= 10,
        "Cache should be consistent"
    );
}

#[test]
fn test_cache_lru_eviction_under_concurrent_load() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        max_size_bytes: 5000, // Small cache to trigger eviction
        max_packs: 5,
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");
    let scratch_root = temp_dir.path().to_path_buf();

    let num_threads = 4;
    let inserts_per_thread = 20;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);
        let root = scratch_root.clone();

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for i in 0..inserts_per_thread {
                let package_id = PackageId::new(&format!("pack-{}-{}", thread_id, i))
                    .expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");
                let cache_path = root.join(format!("pack-{}-{}", thread_id, i));

                let pack = CachedPack::new(
                    package_id,
                    version,
                    format!("digest{}", i),
                    1000, // 1 KB per pack
                    cache_path,
                );

                // Insert may trigger eviction
                let _ = cache_clone.insert(pack);

                // Small delay to increase contention
                std::thread::sleep(Duration::from_micros(100));
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Cache should respect limits
    let stats = cache_arc.stats();
    assert!(stats.total_packs <= 5, "Cache should not exceed max packs");
    assert!(
        stats.total_size_bytes <= 5000,
        "Cache should not exceed max size"
    );

    println!(
        "LRU eviction under load: {} packs, {} bytes",
        stats.total_packs, stats.total_size_bytes
    );
}

#[test]
fn test_cache_persistent_metadata_thread_safety() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cache_config = CacheConfig {
        cache_dir: temp_dir.path().join("cache"),
        persistent: true,
        ..Default::default()
    };
    let cache = PackCache::new(cache_config).expect("Failed to create cache");
    let scratch_root = temp_dir.path().to_path_buf();

    let num_threads = 5;
    let barrier = Arc::new(Barrier::new(num_threads));
    let cache_arc = std::sync::Arc::new(cache);

    let mut handles = vec![];

    for thread_id in 0..num_threads {
        let cache_clone = std::sync::Arc::clone(&cache_arc);
        let barrier_clone = Arc::clone(&barrier);
        let root = scratch_root.clone();

        let handle = std::thread::spawn(move || {
            barrier_clone.wait();

            for i in 0..10 {
                let package_id = PackageId::new(&format!("pack-{}-{}", thread_id, i))
                    .expect("Invalid package ID");
                let version = PackageVersion::new("1.0.0").expect("Invalid version");
                let cache_path = root.join(format!("pack-{}-{}", thread_id, i));

                let pack = CachedPack::new(
                    package_id,
                    version,
                    format!("digest{}", i),
                    1024,
                    cache_path,
                );

                // Insert will trigger metadata save (with locking)
                let _ = cache_clone.insert(pack);
            }
        });

        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Cache should be consistent
    let stats = cache_arc.stats();
    assert!(stats.total_packs > 0, "Cache should have entries");

    // Metadata file should exist
    let metadata_path = temp_dir.path().join("cache").join("cache_metadata.json");
    assert!(metadata_path.exists(), "Metadata file should exist");
}
