// Memory Profiling Benchmarks
// Tracks memory allocations and verifies zero-copy patterns

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// ============================================================================
// MEMORY TRACKING ALLOCATOR
// ============================================================================

struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATED: AtomicUsize = AtomicUsize::new(0);
static ALLOCATION_COUNT: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = System.alloc(layout);
        if !ret.is_null() {
            ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
            ALLOCATION_COUNT.fetch_add(1, Ordering::SeqCst);
        }
        ret
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        DEALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
    }
}

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;

/// Reset memory counters
fn reset_memory_counters() {
    ALLOCATED.store(0, Ordering::SeqCst);
    DEALLOCATED.store(0, Ordering::SeqCst);
    ALLOCATION_COUNT.store(0, Ordering::SeqCst);
}

/// Get current memory stats
fn get_memory_stats() -> (usize, usize, usize) {
    (
        ALLOCATED.load(Ordering::SeqCst),
        DEALLOCATED.load(Ordering::SeqCst),
        ALLOCATION_COUNT.load(Ordering::SeqCst),
    )
}

// ============================================================================
// ZERO-COPY PATTERN VERIFICATION
// ============================================================================

mod zero_copy_verification {
    use super::*;

    /// Zero-copy slice operation
    /// Should not allocate
    fn zero_copy_slice(data: &[u8]) -> &[u8] {
        &data[0..data.len().min(10)]
    }

    /// Allocation slice operation (for comparison)
    /// Will allocate
    fn allocating_slice(data: &[u8]) -> Vec<u8> {
        data[0..data.len().min(10)].to_vec()
    }

    pub fn benchmark_zero_copy(c: &mut Criterion) {
        let mut group = c.benchmark_group("zero_copy_verification");

        let data = vec![0u8; 1024];

        group.bench_function("zero_copy_slice_no_allocation", |b| {
            reset_memory_counters();
            b.iter(|| {
                let _slice = zero_copy_slice(black_box(&data));
            });
            let (allocated, _, count) = get_memory_stats();
            println!(
                "Zero-copy: {} bytes allocated, {} allocations",
                allocated, count
            );
        });

        group.bench_function("allocating_slice_baseline", |b| {
            reset_memory_counters();
            b.iter(|| {
                let _vec = allocating_slice(black_box(&data));
            });
            let (allocated, _, count) = get_memory_stats();
            println!(
                "Allocating: {} bytes allocated, {} allocations",
                allocated, count
            );
        });

        group.finish();
    }
}

// ============================================================================
// BUILDER PATTERN ALLOCATION TRACKING
// ============================================================================

mod builder_allocations {
    use super::*;

    struct LeanBuilder {
        field1: Option<i32>,
        field2: Option<String>,
    }

    impl LeanBuilder {
        fn new() -> Self {
            Self {
                field1: None,
                field2: None,
            }
        }

        fn field1(mut self, value: i32) -> Self {
            self.field1 = Some(value);
            self
        }

        fn field2(mut self, value: String) -> Self {
            self.field2 = Some(value);
            self
        }

        fn build(self) -> (i32, String) {
            (
                self.field1.unwrap_or(0),
                self.field2.unwrap_or_default(),
            )
        }
    }

    pub fn benchmark_builder_allocations(c: &mut Criterion) {
        let mut group = c.benchmark_group("builder_allocations");

        group.bench_function("builder_pattern_overhead", |b| {
            reset_memory_counters();
            b.iter(|| {
                let _result = LeanBuilder::new()
                    .field1(black_box(42))
                    .field2(black_box("test".to_string()))
                    .build();
            });
            let (allocated, deallocated, count) = get_memory_stats();
            println!(
                "Builder: {} bytes allocated, {} deallocated, {} allocations",
                allocated, deallocated, count
            );
        });

        group.finish();
    }
}

// ============================================================================
// TEST FIXTURE MEMORY USAGE
// ============================================================================

mod fixture_memory {
    use super::*;

    struct TestFixture {
        data: Vec<i32>,
        large_buffer: Vec<u8>,
    }

    impl TestFixture {
        fn new(size: usize) -> Self {
            Self {
                data: vec![0; size],
                large_buffer: vec![0u8; size * 100],
            }
        }

        fn cleanup(self) {
            // Explicit drop
            drop(self);
        }
    }

    pub fn benchmark_fixture_memory(c: &mut Criterion) {
        let mut group = c.benchmark_group("fixture_memory");

        for size in [10, 100, 1000].iter() {
            group.bench_with_input(
                BenchmarkId::new("fixture_creation_cleanup", size),
                size,
                |b, &size| {
                    reset_memory_counters();
                    b.iter(|| {
                        let fixture = TestFixture::new(black_box(size));
                        fixture.cleanup();
                    });
                    let (allocated, deallocated, count) = get_memory_stats();
                    println!(
                        "Fixture size {}: {} bytes allocated, {} deallocated, {} allocations",
                        size, allocated, deallocated, count
                    );
                },
            );
        }

        group.finish();
    }
}

// ============================================================================
// MEMORY LEAK DETECTION
// ============================================================================

mod leak_detection {
    use super::*;

    /// Proper cleanup pattern
    fn proper_cleanup() {
        let data = vec![0u8; 1024];
        let _slice = &data[0..10];
        drop(data);
    }

    /// Verify no leaks after repeated operations
    fn verify_no_leaks(iterations: usize) -> bool {
        reset_memory_counters();

        for _ in 0..iterations {
            proper_cleanup();
        }

        let (allocated, deallocated, _) = get_memory_stats();

        // Allow small variance for runtime overhead
        let leaked = allocated.saturating_sub(deallocated);
        leaked < 100 // Less than 100 bytes leaked is acceptable
    }

    pub fn benchmark_leak_detection(c: &mut Criterion) {
        let mut group = c.benchmark_group("leak_detection");

        group.bench_function("verify_no_memory_leaks_1000_iterations", |b| {
            b.iter(|| {
                let no_leaks = verify_no_leaks(black_box(1000));
                assert!(no_leaks, "Memory leak detected!");
            });
        });

        group.finish();
    }
}

// ============================================================================
// ALLOCATION OVERHEAD MEASUREMENT
// ============================================================================

mod allocation_overhead {
    use super::*;

    /// Measure allocation overhead for different patterns
    fn measure_overhead<F: Fn()>(name: &str, f: F) -> f64 {
        reset_memory_counters();

        // Run operation
        f();

        let (allocated, _, count) = get_memory_stats();

        println!(
            "{}: {} bytes allocated across {} allocations",
            name, allocated, count
        );

        // Return overhead as percentage increase
        if count > 0 {
            (allocated as f64 / count as f64) / 1024.0 // Average KB per allocation
        } else {
            0.0
        }
    }

    pub fn benchmark_allocation_overhead(c: &mut Criterion) {
        let mut group = c.benchmark_group("allocation_overhead");

        group.bench_function("small_allocation_overhead", |b| {
            b.iter(|| {
                measure_overhead("small", || {
                    let _v = vec![0u8; 64];
                })
            })
        });

        group.bench_function("medium_allocation_overhead", |b| {
            b.iter(|| {
                measure_overhead("medium", || {
                    let _v = vec![0u8; 1024];
                })
            })
        });

        group.bench_function("large_allocation_overhead", |b| {
            b.iter(|| {
                measure_overhead("large", || {
                    let _v = vec![0u8; 1024 * 1024];
                })
            })
        });

        group.finish();
    }
}

// ============================================================================
// BENCHMARK GROUPS
// ============================================================================

criterion_group!(
    benches,
    zero_copy_verification::benchmark_zero_copy,
    builder_allocations::benchmark_builder_allocations,
    fixture_memory::benchmark_fixture_memory,
    leak_detection::benchmark_leak_detection,
    allocation_overhead::benchmark_allocation_overhead,
);

criterion_main!(benches);
