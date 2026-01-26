use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput, Duration};
use std::process::Command;
use std::time::Instant;
use std::collections::HashMap;

/// Comprehensive performance benchmark suite for ggen
/// Covers all critical performance paths and SLO metrics

pub struct PerformanceSuite {
    pub build_metrics: HashMap<String, Duration>,
    pub test_metrics: HashMap<String, Duration>,
    pub memory_metrics: HashMap<String, usize>,
}

impl PerformanceSuite {
    pub fn new() -> Self {
        PerformanceSuite {
            build_metrics: HashMap::new(),
            test_metrics: HashMap::new(),
            memory_metrics: HashMap::new(),
        }
    }

    /// Run comprehensive performance analysis
    pub fn run_full_suite(&mut self) {
        self.measure_build_pipeline();
        self.measure_test_suite();
        self.measure_workspace_operations();
    }

    fn measure_build_pipeline(&mut self) {
        // Simulate various build scenarios
        let scenarios = vec![
            ("check_debug", vec!["check"]),
            ("check_all_features", vec!["check", "--all-features"]),
            ("build_debug", vec!["build"]),
        ];

        for (name, args) in scenarios {
            let start = Instant::now();
            let _ = Command::new("cargo")
                .args(&args)
                .output();
            let elapsed = start.elapsed();
            self.build_metrics.insert(name.to_string(), elapsed);
        }
    }

    fn measure_test_suite(&mut self) {
        let test_scenarios = vec![
            ("test_lib", vec!["test", "--lib"]),
            ("test_all", vec!["test", "--all"]),
        ];

        for (name, args) in test_scenarios {
            let start = Instant::now();
            let _ = Command::new("cargo")
                .args(&args)
                .output();
            let elapsed = start.elapsed();
            self.test_metrics.insert(name.to_string(), elapsed);
        }
    }

    fn measure_workspace_operations(&mut self) {
        // Measure various workspace-wide operations
        let start = Instant::now();
        let _ = Command::new("cargo")
            .args(&["metadata", "--format-version=1"])
            .output();
        let elapsed = start.elapsed();
        self.build_metrics.insert("workspace_metadata".to_string(), elapsed);
    }
}

// ============================================================================
// Benchmark Groups
// ============================================================================

fn build_pipeline_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_pipeline");
    group.measurement_time(Duration::from_secs(10));

    group.bench_function("cargo_check", |b| {
        b.iter(|| {
            Command::new("cargo")
                .args(&["check"])
                .output()
                .ok()
        })
    });

    group.finish();
}

fn test_suite_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("test_suite");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    group.bench_function("unit_tests", |b| {
        b.iter(|| {
            Command::new("cargo")
                .args(&["test", "--lib", "--no-fail-fast"])
                .output()
                .ok()
        })
    });

    group.finish();
}

fn memory_efficiency_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_efficiency");
    group.measurement_time(Duration::from_secs(5));

    // Benchmark for large allocation patterns
    group.bench_with_input(
        BenchmarkId::from_parameter("allocation_1mb"),
        &1_000_000,
        |b, &size| {
            b.iter(|| {
                let _vec: Vec<u8> = black_box(vec![0; size]);
                _vec.len()
            })
        },
    );

    group.bench_with_input(
        BenchmarkId::from_parameter("allocation_10mb"),
        &10_000_000,
        |b, &size| {
            b.iter(|| {
                let _vec: Vec<u8> = black_box(vec![0; size]);
                _vec.len()
            })
        },
    );

    group.finish();
}

fn throughput_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput");

    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("process_{}_items", size)),
            size,
            |b, &size| {
                b.iter(|| {
                    black_box(vec![0; size].iter().sum::<u8>())
                })
            },
        );
    }

    group.finish();
}

fn concurrent_operation_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_operations");
    group.measurement_time(Duration::from_secs(5));

    // Simulate concurrent tasks
    group.bench_function("concurrent_hash_map_insert", |b| {
        b.iter(|| {
            let mut map = HashMap::new();
            for i in 0..1000 {
                map.insert(i, format!("value_{}", i));
            }
            map.len()
        })
    });

    group.bench_function("concurrent_vector_operations", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1000 {
                vec.push(i);
            }
            vec.iter().sum::<usize>()
        })
    });

    group.finish();
}

fn cache_locality_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_locality");
    group.measurement_time(Duration::from_secs(10));

    // Sequential access pattern (good cache locality)
    group.bench_function("sequential_access", |b| {
        let vec = (0..10000).collect::<Vec<_>>();
        b.iter(|| {
            let mut sum = 0;
            for &val in &vec {
                sum += val;
            }
            black_box(sum)
        })
    });

    // Random access pattern (poor cache locality)
    group.bench_function("random_access", |b| {
        let vec = (0..10000).collect::<Vec<_>>();
        b.iter(|| {
            let mut sum = 0;
            for i in (0..10000).step_by(17) {
                sum += vec[i % vec.len()];
            }
            black_box(sum)
        })
    });

    group.finish();
}

fn compiler_performance_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("compiler_performance");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    group.bench_function("incremental_check", |b| {
        b.iter(|| {
            Command::new("cargo")
                .args(&["check", "--workspace"])
                .output()
                .ok()
                .map(|output| output.status.success())
        })
    });

    group.finish();
}

// ============================================================================
// Criterion Main
// ============================================================================

criterion_group!(
    benches,
    build_pipeline_benchmarks,
    test_suite_benchmarks,
    memory_efficiency_benchmarks,
    throughput_benchmarks,
    concurrent_operation_benchmarks,
    cache_locality_benchmarks,
    compiler_performance_benchmarks
);

criterion_main!(benches);
