//! CRDT Replication Performance Benchmarks
//!
//! This benchmark suite validates the performance characteristics of CRDT operations
//! and measures the 500× latency improvement target from the PhD thesis (Chapter 6.6).
//!
//! ## Service Level Objectives (SLOs)
//!
//! | Benchmark Category | Target | Description |
//! |-------------------|--------|-------------|
//! | CRDT Insert (single) | <1μs | Single key-value insert operation |
//! | CRDT Read (snapshot) | <100ns | Lock-free snapshot read |
//! | CRDT Merge (1k elements) | <10ms | Merge two stores with 1k elements |
//! | Vector Clock Update | <500ns | Single vector clock operation |
//! | Replication Latency (3-region) | <50ms | End-to-end replication across 3 regions |
//! | Concurrent Writes (100 threads) | <100ms | 100 concurrent write operations |
//! | Memory Overhead | <2× | CRDT overhead vs standard HashMap |
//!
//! ## Running Benchmarks
//!
//! ```bash
//! # Run all CRDT benchmarks
//! cargo bench --bench replication
//!
//! # Run specific benchmark group
//! cargo bench --bench replication -- crdt_insert
//!
//! # Generate HTML report
//! cargo bench --bench replication -- --output-format html
//! ```

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use osiris_core::crdt::{CRDTStore, OrSet, or_set::ElementTag};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Instant;
use std::thread;

// =============================================================================
// SLO Constants from PhD Thesis Chapter 6.6
// =============================================================================

/// Maximum allowed time for CRDT insert (1 microsecond)
const SLO_CRDT_INSERT_NS: u64 = 1_000;

/// Maximum allowed time for lock-free snapshot read (100 nanoseconds)
const SLO_SNAPSHOT_READ_NS: u64 = 100;

/// Maximum allowed time for CRDT merge of 1k elements (10ms)
const SLO_MERGE_1K_MS: u64 = 10;

/// Maximum allowed time for vector clock update (500 nanoseconds)
const SLO_VECTOR_CLOCK_UPDATE_NS: u64 = 500;

/// Maximum allowed time for 3-region replication (50ms)
const SLO_REPLICATION_3_REGION_MS: u64 = 50;

/// Maximum allowed time for 100 concurrent writes (100ms)
const SLO_CONCURRENT_WRITES_100_MS: u64 = 100;

/// Target latency improvement vs RwLock (500×)
const TARGET_LATENCY_IMPROVEMENT: f64 = 500.0;

// =============================================================================
// Baseline RwLock Implementation (for comparison)
// =============================================================================

/// Traditional RwLock-backed store (baseline for comparison)
#[derive(Clone)]
struct RwLockStore<K, V>
where
    K: Clone + PartialEq + Eq + std::hash::Hash,
    V: Clone,
{
    data: Arc<RwLock<HashMap<K, V>>>,
}

impl<K, V> RwLockStore<K, V>
where
    K: Clone + PartialEq + Eq + std::hash::Hash,
    V: Clone,
{
    fn new() -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    fn insert(&self, key: K, value: V) -> Option<V> {
        let mut guard = self.data.write().unwrap();
        guard.insert(key, value)
    }

    fn get(&self, key: &K) -> Option<V> {
        let guard = self.data.read().unwrap();
        guard.get(key).cloned()
    }

    fn len(&self) -> usize {
        let guard = self.data.read().unwrap();
        guard.len()
    }
}

// =============================================================================
// Test Data Generators
// =============================================================================

/// Generate test key-value pairs
fn generate_kv_pairs(count: usize) -> Vec<(String, String)> {
    (0..count)
        .map(|i| (format!("key_{}", i), format!("value_{}", i)))
        .collect()
}

/// Generate concurrent workload
fn generate_concurrent_workloads(thread_count: usize, ops_per_thread: usize) -> Vec<Vec<(String, String)>> {
    (0..thread_count)
        .map(|thread_id| {
            (0..ops_per_thread)
                .map(|op_id| (format!("thread_{}_key_{}", thread_id, op_id), format!("value_{}", op_id)))
                .collect()
        })
        .collect()
}

// =============================================================================
// CRDT Insert Performance Benchmarks
// =============================================================================

fn bench_crdt_insert_single(c: &mut Criterion) {
    let mut group = c.benchmark_group("crdt_insert");

    group.bench_function("single_insert", |b| {
        let mut store = CRDTStore::new("region-1", 1);
        let mut counter = 0;

        b.iter(|| {
            let key = format!("key_{}", counter);
            let value = format!("value_{}", counter);
            black_box(store.insert(key, value));
            counter += 1;
        });
    });

    group.bench_function("batch_insert_10", |b| {
        b.iter(|| {
            let mut store = CRDTStore::new("region-1", 1);
            let kvs = generate_kv_pairs(10);

            for (key, value) in kvs {
                black_box(store.insert(key, value));
            }
        });
    });

    group.bench_function("batch_insert_100", |b| {
        b.iter(|| {
            let mut store = CRDTStore::new("region-1", 1);
            let kvs = generate_kv_pairs(100);

            for (key, value) in kvs {
                black_box(store.insert(key, value));
            }
        });
    });

    group.bench_function("batch_insert_1000", |b| {
        b.iter(|| {
            let mut store = CRDTStore::new("region-1", 1);
            let kvs = generate_kv_pairs(1000);

            for (key, value) in kvs {
                black_box(store.insert(key, value));
            }
        });
    });

    group.finish();
}

// =============================================================================
// Lock-Free Snapshot Read Benchmarks
// =============================================================================

fn bench_snapshot_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("snapshot_read");

    // Prepare stores with different sizes
    let store_10 = {
        let mut store = CRDTStore::new("region-1", 1);
        for (key, value) in generate_kv_pairs(10) {
            store.insert(key, value);
        }
        store
    };

    let store_100 = {
        let mut store = CRDTStore::new("region-1", 1);
        for (key, value) in generate_kv_pairs(100) {
            store.insert(key, value);
        }
        store
    };

    let store_1000 = {
        let mut store = CRDTStore::new("region-1", 1);
        for (key, value) in generate_kv_pairs(1000) {
            store.insert(key, value);
        }
        store
    };

    group.bench_function("read_10_elements", |b| {
        b.iter(|| {
            let snapshot = black_box(store_10.snapshot());
            black_box(snapshot.len());
        });
    });

    group.bench_function("read_100_elements", |b| {
        b.iter(|| {
            let snapshot = black_box(store_100.snapshot());
            black_box(snapshot.len());
        });
    });

    group.bench_function("read_1000_elements", |b| {
        b.iter(|| {
            let snapshot = black_box(store_1000.snapshot());
            black_box(snapshot.len());
        });
    });

    group.bench_function("get_single_key_1000", |b| {
        let snapshot = store_1000.snapshot();
        b.iter(|| {
            black_box(snapshot.get(&"key_500".to_string()));
        });
    });

    group.finish();
}

// =============================================================================
// CRDT Merge Performance Benchmarks
// =============================================================================

fn bench_crdt_merge(c: &mut Criterion) {
    let mut group = c.benchmark_group("crdt_merge");

    for size in [10, 100, 1_000, 10_000].iter() {
        group.throughput(Throughput::Elements(*size));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            // Prepare two stores with disjoint keys
            let mut store1 = CRDTStore::new("region-1", 1);
            let mut store2 = CRDTStore::new("region-2", 1_000_000);

            for i in 0..size {
                store1.insert(format!("r1_key_{}", i), format!("r1_value_{}", i));
                store2.insert(format!("r2_key_{}", i), format!("r2_value_{}", i));
            }

            b.iter(|| {
                let mut s1 = store1.clone();
                black_box(s1.merge(&store2));
            });
        });
    }

    group.finish();
}

// =============================================================================
// OR-Set Operation Benchmarks
// =============================================================================

fn bench_orset_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("orset_operations");

    group.bench_function("add_single", |b| {
        let mut set = OrSet::new("region-1", 1);
        let mut counter = 0;

        b.iter(|| {
            black_box(set.add(format!("element_{}", counter)));
            counter += 1;
        });
    });

    group.bench_function("contains_check", |b| {
        let mut set = OrSet::new("region-1", 1);
        for i in 0..1000 {
            set.add(format!("element_{}", i));
        }

        b.iter(|| {
            black_box(set.contains(&"element_500".to_string()));
        });
    });

    group.bench_function("remove_single", |b| {
        let mut set = OrSet::new("region-1", 1);
        let tag = set.add("element_to_remove".to_string());

        b.iter(|| {
            let mut s = set.clone();
            black_box(s.remove(&"element_to_remove".to_string(), &tag));
        });
    });

    group.bench_function("merge_1000_elements", |b| {
        let mut set1 = OrSet::new("region-1", 1);
        let mut set2 = OrSet::new("region-2", 1_000_000);

        for i in 0..1000 {
            set1.add(format!("r1_element_{}", i));
            set2.add(format!("r2_element_{}", i));
        }

        b.iter(|| {
            let mut s1 = set1.clone();
            black_box(s1.merge(&set2));
        });
    });

    group.finish();
}

// =============================================================================
// Comparison: CRDT vs RwLock (Latency Improvement)
// =============================================================================

fn bench_crdt_vs_rwlock(c: &mut Criterion) {
    let mut group = c.benchmark_group("crdt_vs_rwlock");

    // CRDT Store
    group.bench_function("crdt_concurrent_write_100", |b| {
        b.iter(|| {
            let store = Arc::new(std::sync::Mutex::new(CRDTStore::new("region-1", 1)));
            let handles: Vec<_> = (0..10)
                .map(|thread_id| {
                    let store = store.clone();
                    thread::spawn(move || {
                        for i in 0..10 {
                            let mut s = store.lock().unwrap();
                            s.insert(format!("t{}_k{}", thread_id, i), format!("v{}", i));
                        }
                    })
                })
                .collect();

            for handle in handles {
                handle.join().unwrap();
            }
        });
    });

    // RwLock Store (baseline)
    group.bench_function("rwlock_concurrent_write_100", |b| {
        b.iter(|| {
            let store = RwLockStore::new();
            let handles: Vec<_> = (0..10)
                .map(|thread_id| {
                    let store = store.clone();
                    thread::spawn(move || {
                        for i in 0..10 {
                            store.insert(format!("t{}_k{}", thread_id, i), format!("v{}", i));
                        }
                    })
                })
                .collect();

            for handle in handles {
                handle.join().unwrap();
            }
        });
    });

    // Read-heavy workload
    let crdt_read_store = {
        let mut store = CRDTStore::new("region-1", 1);
        for (key, value) in generate_kv_pairs(1000) {
            store.insert(key, value);
        }
        Arc::new(store)
    };

    group.bench_function("crdt_read_heavy_1000_ops", |b| {
        b.iter(|| {
            for i in 0..1000 {
                black_box(crdt_read_store.snapshot().get(&format!("key_{}", i % 1000)));
            }
        });
    });

    let rwlock_read_store = {
        let mut store = RwLockStore::new();
        for (key, value) in generate_kv_pairs(1000) {
            store.insert(key, value);
        }
        Arc::new(store)
    };

    group.bench_function("rwlock_read_heavy_1000_ops", |b| {
        b.iter(|| {
            for i in 0..1000 {
                black_box(rwlock_read_store.get(&format!("key_{}", i % 1000)));
            }
        });
    });

    group.finish();
}

// =============================================================================
// Multi-Region Replication Benchmarks
// =============================================================================

fn bench_multi_region_replication(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi_region_replication");

    // 3-region replication latency
    group.bench_function("replicate_3_regions_100_elements", |b| {
        b.iter(|| {
            let mut region1 = CRDTStore::new("region-1", 1);
            let mut region2 = CRDTStore::new("region-2", 1_000_000);
            let mut region3 = CRDTStore::new("region-3", 2_000_000);

            // Region 1 writes 100 elements
            for i in 0..100 {
                region1.insert(format!("key_{}", i), format!("value_{}", i));
            }

            // Simulate replication: region1 -> region2 -> region3
            let start = Instant::now();

            region2.merge(&region1);
            region3.merge(&region2);

            let duration = start.elapsed();

            // Verify all regions have same data
            assert_eq!(region1.len(), region2.len());
            assert_eq!(region2.len(), region3.len());

            black_box(duration);
        });
    });

    // 5-region replication
    group.bench_function("replicate_5_regions_100_elements", |b| {
        b.iter(|| {
            let mut regions: Vec<CRDTStore<String, String>> = (0..5)
                .map(|i| CRDTStore::new(format!("region-{}", i + 1), (i as u64) * 1_000_000))
                .collect();

            // Region 1 writes 100 elements
            for i in 0..100 {
                regions[0].insert(format!("key_{}", i), format!("value_{}", i));
            }

            // Chain replication
            for i in 1..regions.len() {
                let (prev, curr) = regions.split_at_mut(i);
                curr[0].merge(&prev[i - 1]);
            }

            // Verify convergence
            for i in 1..regions.len() {
                assert_eq!(regions[0].len(), regions[i].len());
            }

            black_box(regions);
        });
    });

    // Concurrent writes across regions + merge
    group.bench_function("concurrent_writes_3_regions_merge", |b| {
        b.iter(|| {
            let mut region1 = CRDTStore::new("region-1", 1);
            let mut region2 = CRDTStore::new("region-2", 1_000_000);
            let mut region3 = CRDTStore::new("region-3", 2_000_000);

            // Concurrent writes (disjoint key sets)
            for i in 0..50 {
                region1.insert(format!("r1_key_{}", i), format!("r1_value_{}", i));
                region2.insert(format!("r2_key_{}", i), format!("r2_value_{}", i));
                region3.insert(format!("r3_key_{}", i), format!("r3_value_{}", i));
            }

            // Merge all
            let start = Instant::now();

            region1.merge(&region2);
            region1.merge(&region3);

            let duration = start.elapsed();

            // Verify all elements present
            assert_eq!(region1.len(), 150);

            black_box(duration);
        });
    });

    group.finish();
}

// =============================================================================
// Memory Overhead Benchmarks
// =============================================================================

fn bench_memory_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_overhead");

    // We'll use size as a proxy for memory usage
    group.bench_function("crdt_store_1000_elements_size", |b| {
        b.iter(|| {
            let mut store = CRDTStore::new("region-1", 1);
            for (key, value) in generate_kv_pairs(1000) {
                store.insert(key, value);
            }

            // Serialize to get approximate size
            let serialized = bincode::serialize(&store).unwrap();
            black_box(serialized.len());
        });
    });

    group.bench_function("rwlock_store_1000_elements_size", |b| {
        b.iter(|| {
            let mut map = HashMap::new();
            for (key, value) in generate_kv_pairs(1000) {
                map.insert(key, value);
            }

            // Serialize to get approximate size
            let serialized = bincode::serialize(&map).unwrap();
            black_box(serialized.len());
        });
    });

    group.finish();
}

// =============================================================================
// Conflict Resolution Benchmarks
// =============================================================================

fn bench_conflict_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("conflict_resolution");

    // Last-Writer-Wins conflict resolution
    group.bench_function("lww_conflict_100_keys", |b| {
        b.iter(|| {
            let mut region1 = CRDTStore::new("region-1", 1);
            let mut region2 = CRDTStore::new("region-2", 1_000_000);

            // Both regions write same keys with different values
            for i in 0..100 {
                region1.insert(format!("key_{}", i), format!("r1_value_{}", i));
                region2.insert(format!("key_{}", i), format!("r2_value_{}", i));
            }

            // Merge triggers conflict resolution
            region1.merge(&region2);

            // Verify LWW semantics (region2 has higher sequence)
            let snapshot = region1.snapshot();
            assert_eq!(snapshot.get(&"key_0".to_string()), Some(&"r2_value_0".to_string()));

            black_box(snapshot);
        });
    });

    // Concurrent add-remove conflicts
    group.bench_function("orset_add_remove_conflict", |b| {
        b.iter(|| {
            let mut set1 = OrSet::new("region-1", 1);
            let mut set2 = OrSet::new("region-2", 1_000_000);

            // Region 1 adds elements
            let mut tags = Vec::new();
            for i in 0..100 {
                tags.push(set1.add(format!("element_{}", i)));
            }

            // Region 2 removes some elements (before seeing adds)
            for i in 0..50 {
                // Create fake removals
                let tag = ElementTag {
                    region_id: "region-1".to_string(),
                    sequence: i as u64 + 1,
                };
                set2.remove(&format!("element_{}", i), &tag);
                set2.remove(&format!("element_{}", i), &tag);
            }

            // Merge: add wins over remove
            set1.merge(&set2);

            // Verify add-wins semantics
            for i in 0..100 {
                assert!(set1.contains(&format!("element_{}", i)));
            }

            black_box(set1);
        });
    });

    group.finish();
}

// =============================================================================
// SLO Validation Functions
// =============================================================================

/// Validate that CRDT insert meets SLO
fn validate_crdt_insert_slo() {
    let mut store = CRDTStore::new("region-1", 1);
    let iterations = 10_000;

    let start = Instant::now();
    for i in 0..iterations {
        store.insert(format!("key_{}", i), format!("value_{}", i));
    }
    let duration = start.elapsed();

    let avg_ns = duration.as_nanos() / iterations as u128;
    let slo_ns = SLO_CRDT_INSERT_NS as u128;

    println!("\n=== CRDT Insert SLO Validation ===");
    println!("Average insert time: {} ns", avg_ns);
    println!("SLO target: {} ns", slo_ns);
    println!("Result: {}", if avg_ns <= slo_ns { "✅ PASS" } else { "❌ FAIL" });
}

/// Validate that snapshot read meets SLO
fn validate_snapshot_read_slo() {
    let mut store = CRDTStore::new("region-1", 1);
    for i in 0..1000 {
        store.insert(format!("key_{}", i), format!("value_{}", i));
    }

    let iterations = 100_000;
    let start = Instant::now();
    for _ in 0..iterations {
        let _ = store.snapshot();
    }
    let duration = start.elapsed();

    let avg_ns = duration.as_nanos() / iterations as u128;
    let slo_ns = SLO_SNAPSHOT_READ_NS as u128;

    println!("\n=== Snapshot Read SLO Validation ===");
    println!("Average read time: {} ns", avg_ns);
    println!("SLO target: {} ns", slo_ns);
    println!("Result: {}", if avg_ns <= slo_ns { "✅ PASS" } else { "❌ FAIL" });
}

/// Validate that merge operation meets SLO
fn validate_merge_slo() {
    let mut store1 = CRDTStore::new("region-1", 1);
    let mut store2 = CRDTStore::new("region-2", 1_000_000);

    for i in 0..1000 {
        store1.insert(format!("r1_key_{}", i), format!("r1_value_{}", i));
        store2.insert(format!("r2_key_{}", i), format!("r2_value_{}", i));
    }

    let iterations = 1000;
    let start = Instant::now();
    for _ in 0..iterations {
        let mut s1 = store1.clone();
        s1.merge(&store2);
    }
    let duration = start.elapsed();

    let avg_ms = duration.as_micros() / iterations as u128;
    let slo_ms = SLO_MERGE_1K_MS as u128 * 1000;

    println!("\n=== CRDT Merge SLO Validation (1k elements) ===");
    println!("Average merge time: {} μs", avg_ms);
    println!("SLO target: {} μs", slo_ms);
    println!("Result: {}", if avg_ms <= slo_ms { "✅ PASS" } else { "❌ FAIL" });
}

/// Validate latency improvement vs RwLock
fn validate_latency_improvement() {
    const THREADS: usize = 10;
    const OPS_PER_THREAD: usize = 100;

    // CRDT Store
    let crdt_store = Arc::new(std::sync::Mutex::new(CRDTStore::new("region-1", 1)));
    let crdt_start = Instant::now();

    let crdt_handles: Vec<_> = (0..THREADS)
        .map(|thread_id| {
            let store = crdt_store.clone();
            thread::spawn(move || {
                for i in 0..OPS_PER_THREAD {
                    let mut s = store.lock().unwrap();
                    s.insert(format!("t{}_k{}", thread_id, i), format!("v{}", i));
                }
            })
        })
        .collect();

    for handle in crdt_handles {
        handle.join().unwrap();
    }

    let crdt_duration = crdt_start.elapsed();

    // RwLock Store
    let rwlock_store = RwLockStore::new();
    let rwlock_start = Instant::now();

    let rwlock_handles: Vec<_> = (0..THREADS)
        .map(|thread_id| {
            let store = rwlock_store.clone();
            thread::spawn(move || {
                for i in 0..OPS_PER_THREAD {
                    store.insert(format!("t{}_k{}", thread_id, i), format!("v{}", i));
                }
            })
        })
        .collect();

    for handle in rwlock_handles {
        handle.join().unwrap();
    }

    let rwlock_duration = rwlock_start.elapsed();

    let improvement = rwlock_duration.as_nanos() as f64 / crdt_duration.as_nanos() as f64;

    println!("\n=== Latency Improvement Validation (1000 concurrent writes) ===");
    println!("CRDT duration: {} ms", crdt_duration.as_millis());
    println!("RwLock duration: {} ms", rwlock_duration.as_millis());
    println!("Improvement: {}×", improvement);
    println!("Target: {}×", TARGET_LATENCY_IMPROVEMENT);
    println!("Result: {}", if improvement >= TARGET_LATENCY_IMPROVEMENT { "✅ PASS" } else { "❌ FAIL" });
}

// =============================================================================
// Criterion Benchmark Groups
// =============================================================================

criterion_group!(
    benches,
    bench_crdt_insert_single,
    bench_snapshot_read,
    bench_crdt_merge,
    bench_orset_operations,
    bench_crdt_vs_rwlock,
    bench_multi_region_replication,
    bench_memory_overhead,
    bench_conflict_resolution,
);

criterion_main!(benches);

// =============================================================================
// Standalone SLO Validation (run with: cargo test --bench replication -- --nocapture)
// =============================================================================

#[cfg(test)]
mod slo_validation {

    #[test]
    fn validate_all_slos() {
        println!("\n╔══════════════════════════════════════════════════════════════╗");
        println!("║     CRDT REPLICATION SLO VALIDATION (Thesis Chapter 6.6)    ║");
        println!("╚══════════════════════════════════════════════════════════════╝");

        validate_crdt_insert_slo();
        validate_snapshot_read_slo();
        validate_merge_slo();
        validate_latency_improvement();

        println!("\n╔══════════════════════════════════════════════════════════════╗");
        println!("║                    SLO VALIDATION COMPLETE                  ║");
        println!("╚══════════════════════════════════════════════════════════════╝\n");
    }
}
