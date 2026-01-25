/// Build Time Benchmarking Suite
///
/// Comprehensive measurement of compilation performance across different scenarios:
/// - Clean builds (full recompilation)
/// - Incremental builds (single file changes)
/// - Parallel compilation scaling
/// - Feature-gated compilation times
/// - Full workspace rebuilds
///
/// SLO Targets:
/// - First build ≤ 15s
/// - Incremental ≤ 2s
/// - Feature-gated variations ≤ 10s
/// - Full workspace ≤ 30s
///
/// Storage: swarm/benchmarks/build_times/{timestamp}/

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

// ============================================================================
// Build Time Measurement Utilities
// ============================================================================

/// Record a single build time measurement
#[derive(Clone, Debug)]
struct BuildTimeMeasurement {
    name: String,
    build_type: String,
    duration_ms: f64,
    artifacts_bytes: u64,
    timestamp: String,
}

impl BuildTimeMeasurement {
    /// Create measurement record
    fn new(name: &str, build_type: &str, duration: Duration, size: u64) -> Self {
        Self {
            name: name.to_string(),
            build_type: build_type.to_string(),
            duration_ms: duration.as_secs_f64() * 1000.0,
            artifacts_bytes: size,
            timestamp: chrono::Local::now().to_rfc3339(),
        }
    }

    /// Serialize to JSON for storage
    fn to_json(&self) -> String {
        format!(
            r#"{{
  "name": "{}",
  "build_type": "{}",
  "duration_ms": {:.2},
  "artifacts_bytes": {},
  "artifacts_mb": {:.2},
  "timestamp": "{}"
}}"#,
            self.name,
            self.build_type,
            self.duration_ms,
            self.artifacts_bytes,
            self.artifacts_bytes as f64 / (1024.0 * 1024.0),
            self.timestamp
        )
    }
}

/// Execute build command and measure time
fn measure_build(args: &[&str]) -> Result<(Duration, u64), String> {
    // Clean build artifacts first
    let _ = Command::new("cargo")
        .args(&["clean"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    let start = Instant::now();

    let output = Command::new("cargo")
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .status()
        .map_err(|e| format!("Failed to execute cargo: {}", e))?;

    let duration = start.elapsed();

    if !output.success() {
        return Err(format!("Build failed with status: {}", output));
    }

    // Calculate artifact size
    let artifact_size = measure_artifact_size("target")?;

    Ok((duration, artifact_size))
}

/// Measure total size of build artifacts
fn measure_artifact_size(path: &str) -> Result<u64, String> {
    let output = Command::new("du")
        .args(&["-sb", path])
        .output()
        .map_err(|e| format!("Failed to measure artifact size: {}", e))?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let size_str = stdout
        .split_whitespace()
        .next()
        .ok_or("Failed to parse du output")?;

    size_str
        .parse::<u64>()
        .map_err(|e| format!("Failed to parse size: {}", e))
}

// ============================================================================
// Benchmark 1: Clean Build Performance
// ============================================================================

fn bench_clean_builds(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_clean");
    group.sample_size(3); // Only 3 samples for builds (expensive)
    group.measurement_time(Duration::from_secs(10));

    // Clean debug build
    group.bench_function("debug_clean", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&["build", "-p", "ggen-cli", "--bin", "ggen"])
                .expect("Clean debug build failed");
            duration
        });
    });

    // Clean release build
    group.bench_function("release_clean", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&[
                "build",
                "--release",
                "-p",
                "ggen-cli",
                "--bin",
                "ggen",
            ])
            .expect("Clean release build failed");
            duration
        });
    });

    // Workspace clean build
    group.bench_function("workspace_clean", |b| {
        b.iter_custom(|_| {
            let (duration, _size) =
                measure_build(&["build", "--workspace"]).expect("Workspace clean build failed");
            duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 2: Incremental Build Performance
// ============================================================================

fn bench_incremental_builds(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_incremental");
    group.sample_size(5);
    group.measurement_time(Duration::from_secs(15));

    // Incremental after single file change
    group.bench_function("single_file_change", |b| {
        b.iter_custom(|_| {
            // Touch a file
            let path = "crates/ggen-utils/src/lib.rs";
            let _ = Command::new("touch")
                .arg(path)
                .status()
                .expect("Failed to touch file");

            // Measure rebuild time
            let (duration, _size) =
                measure_build(&["build", "-p", "ggen-utils"]).expect("Incremental build failed");
            duration
        });
    });

    // Incremental after feature change
    group.bench_function("with_feature_gate", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&[
                "build",
                "-p",
                "ggen-core",
                "--features",
                "rdf-processing",
            ])
            .expect("Feature-gated build failed");
            duration
        });
    });

    // Incremental build with docs
    group.bench_function("with_documentation", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&["build", "--doc"]).expect("Doc build failed");
            duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 3: Feature-Gated Compilation
// ============================================================================

fn bench_feature_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_features");
    group.sample_size(3);
    group.measurement_time(Duration::from_secs(10));

    let feature_sets = vec![
        ("no_features", vec!["--no-default-features"]),
        ("minimal_features", vec!["--features", "core"]),
        ("prod_features", vec!["--no-default-features", "--features", "prod"]),
        ("dev_features", vec!["--features", "dev"]),
        ("all_features", vec!["--all-features"]),
    ];

    for (name, args) in feature_sets {
        let mut build_args = vec!["build", "-p", "ggen-cli", "--bin", "ggen"];
        build_args.extend(args);

        group.bench_function(name, |b| {
            b.iter_custom(|_| {
                let (duration, _size) = measure_build(&build_args)
                    .expect(&format!("Feature build '{}' failed", name));
                duration
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark 4: Parallel Compilation Scaling
// ============================================================================

fn bench_parallel_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_parallel");
    group.sample_size(3);
    group.measurement_time(Duration::from_secs(10));

    let job_counts = vec![1, 2, 4, 8];

    for jobs in job_counts {
        let bench_name = format!("jobs_{}", jobs);

        group.bench_function(&bench_name, |b| {
            b.iter_custom(|_| {
                let job_arg = format!("{}", jobs);
                let (duration, _size) = measure_build(&[
                    "build",
                    "-j",
                    &job_arg,
                    "-p",
                    "ggen-cli",
                    "--bin",
                    "ggen",
                ])
                .expect(&format!("Parallel build with {} jobs failed", jobs));
                duration
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark 5: Workspace Compilation
// ============================================================================

fn bench_workspace_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_workspace");
    group.sample_size(3);
    group.measurement_time(Duration::from_secs(15));

    // Core crates only
    group.bench_function("core_crates", |b| {
        b.iter_custom(|_| {
            let (duration, _size) =
                measure_build(&["build", "--workspace", "--no-default-features", "--features", "core"])
                    .expect("Core crates build failed");
            duration
        });
    });

    // With optional features
    group.bench_function("with_optional", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&["build", "--workspace", "--features", "dev"])
                .expect("Optional features build failed");
            duration
        });
    });

    // Full workspace
    group.bench_function("full_workspace", |b| {
        b.iter_custom(|_| {
            let (duration, _size) =
                measure_build(&["build", "--workspace"]).expect("Full workspace build failed");
            duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 6: Check Performance
// ============================================================================

fn bench_check_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_check");
    group.sample_size(5);
    group.measurement_time(Duration::from_secs(10));

    // Quick check
    group.bench_function("quick_check", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let output = Command::new("cargo")
                .args(&["check", "--workspace", "--no-default-features", "--features", "core"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("Check command failed");

            if !output.success() {
                panic!("Check failed");
            }

            start.elapsed()
        });
    });

    // Full workspace check
    group.bench_function("workspace_check", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let output = Command::new("cargo")
                .args(&["check", "--workspace"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("Check command failed");

            if !output.success() {
                panic!("Check failed");
            }

            start.elapsed()
        });
    });

    // Check with all features
    group.bench_function("check_all_features", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let output = Command::new("cargo")
                .args(&["check", "--workspace", "--all-features"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("Check command failed");

            if !output.success() {
                panic!("Check failed");
            }

            start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 7: Test Compilation
// ============================================================================

fn bench_test_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_tests");
    group.sample_size(3);
    group.measurement_time(Duration::from_secs(15));

    // Compile unit tests
    group.bench_function("compile_unit_tests", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let output = Command::new("cargo")
                .args(&["test", "--lib", "--no-run"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("Test compilation failed");

            if !output.success() {
                panic!("Test compilation failed");
            }

            start.elapsed()
        });
    });

    // Compile integration tests
    group.bench_function("compile_integration_tests", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let output = Command::new("cargo")
                .args(&["test", "--test", "*", "--no-run"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("Integration test compilation failed");

            if !output.success() {
                panic!("Integration test compilation failed");
            }

            start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Setup
// ============================================================================

criterion_group!(
    benches,
    bench_clean_builds,
    bench_incremental_builds,
    bench_feature_compilation,
    bench_parallel_compilation,
    bench_workspace_compilation,
    bench_check_performance,
    bench_test_compilation
);

criterion_main!(benches);
