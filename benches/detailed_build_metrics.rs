use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Duration};
use std::process::{Command, Stdio};
use std::time::Instant;
use std::collections::HashMap;

/// Detailed build metrics tracking
pub struct BuildMetrics {
    /// Time to compile a single file
    pub single_file_time: Duration,
    /// Time for complete clean build
    pub clean_build_time: Duration,
    /// Time for incremental build (single file change)
    pub incremental_build_time: Duration,
    /// Memory used during build
    pub peak_memory_usage: usize,
    /// Number of compilation units
    pub compilation_units: usize,
    /// Average time per compilation unit
    pub time_per_unit: Duration,
}

pub struct BuildProfiler {
    metrics: HashMap<String, BuildMetrics>,
}

impl BuildProfiler {
    pub fn new() -> Self {
        BuildProfiler {
            metrics: HashMap::new(),
        }
    }

    pub fn profile_clean_build(&self) -> Option<BuildMetrics> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["clean"])
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let build_start = Instant::now();
        let output = Command::new("cargo")
            .args(&["build", "--release"])
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let clean_build_time = build_start.elapsed();

        Some(BuildMetrics {
            single_file_time: Duration::ZERO,
            clean_build_time,
            incremental_build_time: Duration::ZERO,
            peak_memory_usage: 0,
            compilation_units: 0,
            time_per_unit: Duration::ZERO,
        })
    }

    pub fn profile_check(&self) -> Option<Duration> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["check", "--workspace"])
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        Some(start.elapsed())
    }

    pub fn profile_test_build(&self) -> Option<Duration> {
        let start = Instant::now();

        let output = Command::new("cargo")
            .args(&["test", "--no-run", "--workspace"])
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        Some(start.elapsed())
    }

    pub fn estimate_incremental_build(&self) -> Option<Duration> {
        // Estimate based on check time (incremental compilation is typically similar to check)
        self.profile_check()
    }
}

fn build_metrics_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_metrics");
    group.measurement_time(Duration::from_secs(10));

    let profiler = BuildProfiler::new();

    // Benchmark cargo check (lightweight)
    group.bench_function("cargo_check", |b| {
        b.iter(|| {
            Command::new("cargo")
                .args(&["check"])
                .output()
                .ok()
        })
    });

    // Benchmark cargo build debug
    group.bench_function("cargo_build_debug", |b| {
        b.iter(|| {
            Command::new("cargo")
                .args(&["build"])
                .output()
                .ok()
        })
    });

    group.finish();
}

fn build_profiling_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_profiling");
    group.sample_size(10); // Small sample size for long operations
    group.measurement_time(Duration::from_secs(30));

    let profiler = BuildProfiler::new();

    group.bench_function("check_workspace", |b| {
        b.iter(|| {
            profiler.profile_check()
        })
    });

    group.bench_function("test_build", |b| {
        b.iter(|| {
            profiler.profile_test_build()
        })
    });

    group.finish();
}

criterion_group!(benches, build_metrics_benchmark, build_profiling_benchmark);
criterion_main!(benches);
