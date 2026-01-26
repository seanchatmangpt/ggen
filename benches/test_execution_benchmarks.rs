use criterion::{criterion_group, criterion_main, Criterion, Duration};
use std::process::Command;
use std::time::Instant;
use std::collections::HashMap;

/// Test execution metrics and analysis
pub struct TestMetrics {
    pub total_time: Duration,
    pub unit_test_time: Duration,
    pub integration_test_time: Duration,
    pub doc_test_time: Duration,
    pub total_tests_run: usize,
    pub passed_tests: usize,
    pub failed_tests: usize,
    pub parallelism_factor: f64,
}

pub struct TestProfiler {
    metrics: HashMap<String, TestMetrics>,
}

impl TestProfiler {
    pub fn new() -> Self {
        TestProfiler {
            metrics: HashMap::new(),
        }
    }

    pub fn profile_all_tests(&self) -> Option<TestMetrics> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["test", "--workspace"])
            .output()
            .ok()?;

        let total_time = start.elapsed();

        Some(TestMetrics {
            total_time,
            unit_test_time: Duration::ZERO,
            integration_test_time: Duration::ZERO,
            doc_test_time: Duration::ZERO,
            total_tests_run: 0,
            passed_tests: 0,
            failed_tests: 0,
            parallelism_factor: 1.0,
        })
    }

    pub fn profile_unit_tests(&self) -> Option<TestMetrics> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["test", "--workspace", "--lib"])
            .output()
            .ok()?;

        let unit_test_time = start.elapsed();

        Some(TestMetrics {
            total_time: unit_test_time,
            unit_test_time,
            integration_test_time: Duration::ZERO,
            doc_test_time: Duration::ZERO,
            total_tests_run: 0,
            passed_tests: 0,
            failed_tests: 0,
            parallelism_factor: 1.0,
        })
    }

    pub fn profile_integration_tests(&self) -> Option<TestMetrics> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["test", "--workspace", "--test", "*"])
            .output()
            .ok()?;

        let integration_test_time = start.elapsed();

        Some(TestMetrics {
            total_time: integration_test_time,
            unit_test_time: Duration::ZERO,
            integration_test_time,
            doc_test_time: Duration::ZERO,
            total_tests_run: 0,
            passed_tests: 0,
            failed_tests: 0,
            parallelism_factor: 1.0,
        })
    }

    pub fn profile_single_threaded_tests(&self) -> Option<TestMetrics> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["test", "--workspace", "--", "--test-threads=1"])
            .output()
            .ok()?;

        let total_time = start.elapsed();

        Some(TestMetrics {
            total_time,
            unit_test_time: Duration::ZERO,
            integration_test_time: Duration::ZERO,
            doc_test_time: Duration::ZERO,
            total_tests_run: 0,
            passed_tests: 0,
            failed_tests: 0,
            parallelism_factor: 1.0,
        })
    }
}

fn test_execution_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("test_execution");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    let profiler = TestProfiler::new();

    group.bench_function("unit_tests", |b| {
        b.iter(|| {
            profiler.profile_unit_tests()
        })
    });

    group.finish();
}

fn test_parallelism_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("test_parallelism");
    group.sample_size(5);
    group.measurement_time(Duration::from_secs(60));

    let profiler = TestProfiler::new();

    group.bench_function("parallel_tests", |b| {
        b.iter(|| {
            profiler.profile_all_tests()
        })
    });

    group.bench_function("single_threaded_tests", |b| {
        b.iter(|| {
            profiler.profile_single_threaded_tests()
        })
    });

    group.finish();
}

criterion_group!(benches, test_execution_benchmark, test_parallelism_benchmark);
criterion_main!(benches);
