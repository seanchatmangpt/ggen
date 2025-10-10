use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::sync::Arc;
use lru::LruCache;
use parking_lot::Mutex;
use dashmap::DashMap;

/// Benchmark LRU cache performance for template lookups
fn benchmark_lru_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("lru_cache");

    // Single-threaded cache access
    group.bench_function("get_hit", |b| {
        let mut cache = LruCache::new(std::num::NonZeroUsize::new(1000).unwrap());
        for i in 0..100 {
            cache.put(format!("key_{}", i), format!("value_{}", i));
        }

        b.iter(|| {
            black_box(cache.get(&"key_50".to_string()))
        });
    });

    group.bench_function("get_miss", |b| {
        let mut cache = LruCache::new(std::num::NonZeroUsize::new(1000).unwrap());
        for i in 0..100 {
            cache.put(format!("key_{}", i), format!("value_{}", i));
        }

        b.iter(|| {
            black_box(cache.get(&"missing_key".to_string()))
        });
    });

    group.bench_function("put_new", |b| {
        let mut cache = LruCache::new(std::num::NonZeroUsize::new(1000).unwrap());
        let mut counter = 0;

        b.iter(|| {
            cache.put(format!("key_{}", counter), format!("value_{}", counter));
            counter += 1;
        });
    });

    // Test eviction performance
    group.bench_function("put_with_eviction", |b| {
        let mut cache = LruCache::new(std::num::NonZeroUsize::new(100).unwrap());
        let mut counter = 0;

        b.iter(|| {
            cache.put(format!("key_{}", counter), format!("value_{}", counter));
            counter += 1;
        });
    });

    // Varying cache sizes
    for size in [10, 100, 1000, 10000].iter() {
        group.bench_with_input(BenchmarkId::new("cache_size", size), size, |b, &size| {
            let mut cache = LruCache::new(std::num::NonZeroUsize::new(size).unwrap());
            for i in 0..size {
                cache.put(format!("key_{}", i), format!("value_{}", i));
            }

            b.iter(|| {
                black_box(cache.get(&format!("key_{}", size / 2)))
            });
        });
    }

    group.finish();
}

/// Benchmark thread-safe cache with parking_lot
fn benchmark_concurrent_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_cache");

    // Arc<Mutex<LruCache>> benchmark
    group.bench_function("mutex_lru_get", |b| {
        let cache = Arc::new(Mutex::new(LruCache::new(std::num::NonZeroUsize::new(1000).unwrap())));
        {
            let mut cache_lock = cache.lock();
            for i in 0..100 {
                cache_lock.put(format!("key_{}", i), format!("value_{}", i));
            }
        }

        b.iter(|| {
            let mut cache_lock = cache.lock();
            black_box(cache_lock.get(&"key_50".to_string()))
        });
    });

    group.bench_function("mutex_lru_put", |b| {
        let cache = Arc::new(Mutex::new(LruCache::new(std::num::NonZeroUsize::new(1000).unwrap())));
        let mut counter = 0;

        b.iter(|| {
            let mut cache_lock = cache.lock();
            cache_lock.put(format!("key_{}", counter), format!("value_{}", counter));
            counter += 1;
        });
    });

    // DashMap benchmark (lockless concurrent map)
    group.bench_function("dashmap_get", |b| {
        let cache = DashMap::new();
        for i in 0..100 {
            cache.insert(format!("key_{}", i), format!("value_{}", i));
        }

        b.iter(|| {
            black_box(cache.get(&"key_50".to_string()))
        });
    });

    group.bench_function("dashmap_insert", |b| {
        let cache = DashMap::new();
        let mut counter = 0;

        b.iter(|| {
            cache.insert(format!("key_{}", counter), format!("value_{}", counter));
            counter += 1;
        });
    });

    // Compare under concurrent load
    group.bench_function("concurrent_reads_dashmap", |b| {
        let cache = Arc::new(DashMap::new());
        for i in 0..1000 {
            cache.insert(format!("key_{}", i), format!("value_{}", i));
        }

        let rt = tokio::runtime::Runtime::new().unwrap();
        b.to_async(&rt).iter(|| {
            let cache = cache.clone();
            async move {
                let tasks: Vec<_> = (0..100).map(|i| {
                    let cache = cache.clone();
                    tokio::spawn(async move {
                        cache.get(&format!("key_{}", i % 1000))
                    })
                }).collect();

                for task in tasks {
                    let _ = task.await;
                }
            }
        });
    });

    group.finish();
}

/// Benchmark cache strategies for marketplace data
fn benchmark_marketplace_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_cache");

    // Simulate package metadata caching
    #[derive(Clone, Debug)]
    struct PackageMetadata {
        id: String,
        name: String,
        description: String,
        downloads: u32,
        stars: u32,
    }

    group.bench_function("cache_package_metadata", |b| {
        let cache = Arc::new(DashMap::new());

        b.iter(|| {
            let pkg = PackageMetadata {
                id: "test-id".to_string(),
                name: "test-package".to_string(),
                description: "A test package".to_string(),
                downloads: 1000,
                stars: 50,
            };
            cache.insert("test-id".to_string(), pkg);
        });
    });

    group.bench_function("retrieve_cached_package", |b| {
        let cache = Arc::new(DashMap::new());
        let pkg = PackageMetadata {
            id: "test-id".to_string(),
            name: "test-package".to_string(),
            description: "A test package".to_string(),
            downloads: 1000,
            stars: 50,
        };
        cache.insert("test-id".to_string(), pkg);

        b.iter(|| {
            black_box(cache.get(&"test-id".to_string()))
        });
    });

    // Benchmark search result caching
    group.bench_function("cache_search_results", |b| {
        let cache = Arc::new(DashMap::new());

        b.iter(|| {
            let results: Vec<String> = (0..20).map(|i| format!("result_{}", i)).collect();
            cache.insert("query:auth".to_string(), results);
        });
    });

    group.bench_function("retrieve_cached_search", |b| {
        let cache = Arc::new(DashMap::new());
        let results: Vec<String> = (0..20).map(|i| format!("result_{}", i)).collect();
        cache.insert("query:auth".to_string(), results);

        b.iter(|| {
            black_box(cache.get(&"query:auth".to_string()))
        });
    });

    group.finish();
}

/// Benchmark cache warming strategies
fn benchmark_cache_warming(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_warming");

    group.bench_function("warm_sequential", |b| {
        b.iter(|| {
            let cache = LruCache::new(std::num::NonZeroUsize::new(1000).unwrap());
            let mut cache_mut = cache;
            for i in 0..100 {
                cache_mut.put(format!("key_{}", i), format!("value_{}", i));
            }
            black_box(cache_mut)
        });
    });

    group.bench_function("warm_concurrent", |b| {
        let rt = tokio::runtime::Runtime::new().unwrap();
        b.to_async(&rt).iter(|| async {
            let cache = Arc::new(DashMap::new());

            let tasks: Vec<_> = (0..100).map(|i| {
                let cache = cache.clone();
                tokio::spawn(async move {
                    cache.insert(format!("key_{}", i), format!("value_{}", i));
                })
            }).collect();

            for task in tasks {
                let _ = task.await;
            }

            black_box(cache)
        });
    });

    group.finish();
}

/// Benchmark TTL (Time-To-Live) cache implementation
fn benchmark_ttl_cache(c: &mut Criterion) {
    use std::time::{Duration, Instant};

    #[derive(Clone)]
    struct TtlEntry<T> {
        value: T,
        expires_at: Instant,
    }

    let mut group = c.benchmark_group("ttl_cache");

    group.bench_function("insert_with_ttl", |b| {
        let cache: DashMap<String, TtlEntry<String>> = DashMap::new();
        let mut counter = 0;

        b.iter(|| {
            let entry = TtlEntry {
                value: format!("value_{}", counter),
                expires_at: Instant::now() + Duration::from_secs(3600),
            };
            cache.insert(format!("key_{}", counter), entry);
            counter += 1;
        });
    });

    group.bench_function("get_with_ttl_check", |b| {
        let cache: DashMap<String, TtlEntry<String>> = DashMap::new();
        for i in 0..100 {
            let entry = TtlEntry {
                value: format!("value_{}", i),
                expires_at: Instant::now() + Duration::from_secs(3600),
            };
            cache.insert(format!("key_{}", i), entry);
        }

        b.iter(|| {
            if let Some(entry) = cache.get(&"key_50".to_string()) {
                let now = Instant::now();
                if now < entry.expires_at {
                    black_box(&entry.value);
                }
            }
        });
    });

    group.bench_function("cleanup_expired", |b| {
        let cache: Arc<DashMap<String, TtlEntry<String>>> = Arc::new(DashMap::new());
        for i in 0..1000 {
            let entry = TtlEntry {
                value: format!("value_{}", i),
                expires_at: if i % 2 == 0 {
                    Instant::now() - Duration::from_secs(1) // Expired
                } else {
                    Instant::now() + Duration::from_secs(3600) // Valid
                },
            };
            cache.insert(format!("key_{}", i), entry);
        }

        b.iter(|| {
            let now = Instant::now();
            let keys_to_remove: Vec<String> = cache
                .iter()
                .filter(|entry| entry.value().expires_at < now)
                .map(|entry| entry.key().clone())
                .collect();

            for key in keys_to_remove {
                cache.remove(&key);
            }
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_lru_cache,
    benchmark_concurrent_cache,
    benchmark_marketplace_cache,
    benchmark_cache_warming,
    benchmark_ttl_cache
);
criterion_main!(benches);
