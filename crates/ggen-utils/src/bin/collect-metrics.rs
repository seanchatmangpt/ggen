// Metrics Collection System
// Collects and stores daily metrics for tracking project health

use chrono::Local;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

#[derive(Debug, Serialize, Deserialize)]
struct Metrics {
    date: String,
    build_time_seconds: f64,
    test_pass_rate: f64,
    total_tests: usize,
    passed_tests: usize,
    compiler_errors: usize,
    compiler_warnings: usize,
    clippy_warnings: usize,
    code_lines: usize,
    test_lines: usize,
    code_coverage: Option<f64>,
}

fn main() {
    println!("📊 Collecting metrics...");

    let metrics = Metrics {
        date: Local::now().format("%Y-%m-%d").to_string(),
        build_time_seconds: measure_build_time(),
        test_pass_rate: measure_test_pass_rate().0,
        total_tests: measure_test_pass_rate().1,
        passed_tests: measure_test_pass_rate().2,
        compiler_errors: count_compiler_errors(),
        compiler_warnings: count_compiler_warnings(),
        clippy_warnings: count_clippy_warnings(),
        code_lines: count_lines("crates/*/src/**/*.rs"),
        test_lines: count_lines("crates/*/tests/**/*.rs"),
        code_coverage: measure_coverage(),
    };

    // Create metrics directory
    let metrics_dir = Path::new(".metrics");
    fs::create_dir_all(metrics_dir).expect("Failed to create metrics directory");

    // Write metrics to file
    let filename = format!("{}.json", metrics.date);
    let path = metrics_dir.join(&filename);

    let json = serde_json::to_string_pretty(&metrics).expect("Failed to serialize metrics");
    fs::write(&path, json).expect("Failed to write metrics");

    println!("✅ Metrics saved to {}", path.display());
    println!();
    println!("Summary:");
    println!("  Build time: {:.2}s", metrics.build_time_seconds);
    println!("  Test pass rate: {:.1}%", metrics.test_pass_rate * 100.0);
    println!("  Tests: {}/{}", metrics.passed_tests, metrics.total_tests);
    println!("  Compiler errors: {}", metrics.compiler_errors);
    println!("  Compiler warnings: {}", metrics.compiler_warnings);
    println!("  Clippy warnings: {}", metrics.clippy_warnings);
    println!("  Code lines: {}", metrics.code_lines);
    println!("  Test lines: {}", metrics.test_lines);
}

fn measure_build_time() -> f64 {
    println!("  Measuring build time...");

    // Clean build to get accurate timing
    let _ = Command::new("cargo").args(&["clean"]).output();

    let start = Instant::now();
    let output = Command::new("cargo")
        .args(&["build", "--all-targets"])
        .output()
        .expect("Failed to run cargo build");

    let duration = start.elapsed().as_secs_f64();

    if !output.status.success() {
        eprintln!("Warning: Build failed, timing may be inaccurate");
    }

    duration
}

fn measure_test_pass_rate() -> (f64, usize, usize) {
    println!("  Measuring test pass rate...");

    let output = Command::new("cargo")
        .args(&["test", "--", "--test-threads=1", "--format=json"])
        .output()
        .expect("Failed to run cargo test");

    let stdout = String::from_utf8_lossy(&output.stdout);

    let mut total = 0;
    let mut passed = 0;

    for line in stdout.lines() {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(line) {
            if json["type"] == "test" {
                total += 1;
                if json["event"] == "ok" {
                    passed += 1;
                }
            }
        }
    }

    let rate = if total > 0 {
        passed as f64 / total as f64
    } else {
        0.0
    };

    (rate, total, passed)
}

fn count_compiler_errors() -> usize {
    println!("  Counting compiler errors...");

    let output = Command::new("cargo")
        .args(&["check", "--message-format=json"])
        .output()
        .expect("Failed to run cargo check");

    let stdout = String::from_utf8_lossy(&output.stdout);

    stdout
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|json| json["message"]["level"] == "error")
        .count()
}

fn count_compiler_warnings() -> usize {
    println!("  Counting compiler warnings...");

    let output = Command::new("cargo")
        .args(&["check", "--message-format=json"])
        .output()
        .expect("Failed to run cargo check");

    let stdout = String::from_utf8_lossy(&output.stdout);

    stdout
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|json| json["message"]["level"] == "warning")
        .count()
}

fn count_clippy_warnings() -> usize {
    println!("  Counting clippy warnings...");

    let output = Command::new("cargo")
        .args(&["clippy", "--message-format=json"])
        .output()
        .expect("Failed to run cargo clippy");

    let stdout = String::from_utf8_lossy(&output.stdout);

    stdout
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|json| json["message"]["level"] == "warning")
        .count()
}

fn count_lines(pattern: &str) -> usize {
    println!("  Counting lines for pattern: {}", pattern);

    let output = Command::new("find")
        .args(&[
            "crates", "-name", "*.rs", "-type", "f", "-exec", "wc", "-l", "{}", "+",
        ])
        .output()
        .expect("Failed to count lines");

    let stdout = String::from_utf8_lossy(&output.stdout);

    stdout
        .lines()
        .filter_map(|line| {
            line.trim()
                .split_whitespace()
                .next()
                .and_then(|s| s.parse::<usize>().ok())
        })
        .sum()
}

fn measure_coverage() -> Option<f64> {
    println!("  Measuring code coverage (skipped - requires tarpaulin)...");
    // Would require: cargo tarpaulin --out Json
    // For now, return None
    None
}
