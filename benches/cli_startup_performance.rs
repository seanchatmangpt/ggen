use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

// ============================================================================
// BENCHMARK 1: CLI Command Startup Time
// ============================================================================

fn bench_cli_startup(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_startup");

    // Ensure binary is built
    let build_status = Command::new("cargo")
        .args(&["build", "--release", "--bin", "ggen"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if build_status.is_err() || !build_status.unwrap().success() {
        eprintln!("Warning: Failed to build ggen binary. Skipping CLI startup benchmarks.");
        return;
    }

    let binary_path = std::env::current_dir().unwrap().join("target/release/ggen");

    if !binary_path.exists() {
        eprintln!(
            "Warning: ggen binary not found at {:?}. Skipping CLI startup benchmarks.",
            binary_path
        );
        return;
    }

    // Benchmark: Help command (minimal startup)
    group.bench_function("help_command", |b| {
        b.iter(|| {
            let output = Command::new(black_box(&binary_path))
                .arg("--help")
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output();
            black_box(output)
        });
    });

    // Benchmark: Version command
    group.bench_function("version_command", |b| {
        b.iter(|| {
            let output = Command::new(black_box(&binary_path))
                .arg("--version")
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output();
            black_box(output)
        });
    });

    // Benchmark: List templates command (if available)
    group.bench_function("list_command", |b| {
        b.iter(|| {
            let output = Command::new(black_box(&binary_path))
                .args(&["list"])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output();
            black_box(output)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 2: CLI Command Execution Time
// ============================================================================

fn bench_cli_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_execution");

    let binary_path = std::env::current_dir().unwrap().join("target/release/ggen");

    if !binary_path.exists() {
        eprintln!("Warning: ggen binary not found. Skipping CLI execution benchmarks.");
        return;
    }

    // Create a simple test template
    let test_template = r#"---
to: "output/test.rs"
---
fn main() {
    println!("Hello, world!");
}
"#;

    use tempfile::TempDir;
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");
    std::fs::write(&template_path, test_template).unwrap();

    // Benchmark: Generate from simple template
    group.bench_function("simple_generation", |b| {
        b.iter(|| {
            let output = Command::new(black_box(&binary_path))
                .args(&["generate", template_path.to_str().unwrap()])
                .current_dir(temp_dir.path())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output();
            black_box(output)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 3: Cold vs Warm Start
// ============================================================================

fn bench_cold_vs_warm_start(c: &mut Criterion) {
    let mut group = c.benchmark_group("cold_warm_start");

    let binary_path = std::env::current_dir().unwrap().join("target/release/ggen");

    if !binary_path.exists() {
        eprintln!("Warning: ggen binary not found. Skipping cold/warm start benchmarks.");
        return;
    }

    // Cold start: First execution
    group.bench_function("cold_start", |b| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::new(0, 0);

            for _ in 0..iters {
                // Clear system caches (best effort)
                #[cfg(target_os = "linux")]
                {
                    let _ = Command::new("sync").status();
                }

                let start = Instant::now();
                let _ = Command::new(black_box(&binary_path))
                    .arg("--version")
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status();
                total_duration += start.elapsed();

                // Sleep to ensure cold start
                std::thread::sleep(Duration::from_millis(10));
            }

            total_duration
        });
    });

    // Warm start: Repeated execution
    group.bench_function("warm_start", |b| {
        // Prime the cache
        for _ in 0..3 {
            let _ = Command::new(&binary_path)
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
        }

        b.iter(|| {
            let output = Command::new(black_box(&binary_path))
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            black_box(output)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 4: Startup Time Components
// ============================================================================

fn bench_startup_components(c: &mut Criterion) {
    let mut group = c.benchmark_group("startup_components");

    // Benchmark: Binary loading time (minimal execution)
    group.bench_function("binary_load", |b| {
        let binary_path = std::env::current_dir().unwrap().join("target/release/ggen");

        if !binary_path.exists() {
            return;
        }

        b.iter(|| {
            let start = Instant::now();
            let _ = Command::new(black_box(&binary_path))
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            let duration = start.elapsed();
            black_box(duration)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    cli_benches,
    bench_cli_startup,
    bench_cli_execution,
    bench_cold_vs_warm_start,
    bench_startup_components
);

criterion_main!(cli_benches);
