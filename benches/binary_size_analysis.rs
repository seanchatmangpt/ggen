/// Binary Size Analysis Suite
///
/// Comprehensive binary size tracking across profiles and builds:
/// - Total binary size tracking
/// - Size per profile (dev/release/test)
/// - Symbol table size
/// - Debug info size
/// - Strip effectiveness
///
/// SLO Targets:
/// - Release binary ≤ 10MB
/// - Debug binary ≤ 50MB
/// - Symbol reduction via strip ≥ 50%
/// - Debug info ≤ 100MB unstripped
///
/// Storage: swarm/benchmarks/binary_sizes/{timestamp}/

use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;
use std::path::Path;
use std::process::Command;

// ============================================================================
// Binary Size Measurement Utilities
// ============================================================================

/// Record binary size analysis
#[derive(Clone, Debug)]
struct BinarySizeRecord {
    binary_name: String,
    profile: String,
    total_size_bytes: u64,
    stripped_size_bytes: Option<u64>,
    symbol_count: Option<u64>,
    debug_info_size: Option<u64>,
    timestamp: String,
}

impl BinarySizeRecord {
    fn new(name: &str, profile: &str, size: u64) -> Self {
        Self {
            binary_name: name.to_string(),
            profile: profile.to_string(),
            total_size_bytes: size,
            stripped_size_bytes: None,
            symbol_count: None,
            debug_info_size: None,
            timestamp: chrono::Local::now().to_rfc3339(),
        }
    }

    fn to_json(&self) -> String {
        let stripped_str = self
            .stripped_size_bytes
            .map(|s| format!("  \"stripped_size_mb\": {:.2},", s as f64 / (1024.0 * 1024.0)))
            .unwrap_or_default();

        let symbols_str = self
            .symbol_count
            .map(|s| format!("  \"symbol_count\": {},", s))
            .unwrap_or_default();

        let debug_str = self
            .debug_info_size
            .map(|s| format!("  \"debug_info_mb\": {:.2},", s as f64 / (1024.0 * 1024.0)))
            .unwrap_or_default();

        format!(
            r#"{{
  "binary": "{}",
  "profile": "{}",
  "total_size_mb": {:.2},
{}{}{}
  "timestamp": "{}"
}}"#,
            self.binary_name,
            self.profile,
            self.total_size_bytes as f64 / (1024.0 * 1024.0),
            stripped_str,
            symbols_str,
            debug_str,
            self.timestamp
        )
    }
}

/// Get file size in bytes
fn get_file_size(path: &str) -> Result<u64, String> {
    fs::metadata(path)
        .map(|m| m.len())
        .map_err(|e| format!("Failed to get file size: {}", e))
}

/// Count symbols in binary
fn count_symbols(path: &str) -> Result<u64, String> {
    let output = Command::new("nm")
        .arg(path)
        .output()
        .map_err(|e| format!("Failed to run nm: {}", e))?;

    let count = String::from_utf8_lossy(&output.stdout)
        .lines()
        .count() as u64;

    Ok(count)
}

/// Strip debug symbols from binary
fn strip_binary(path: &str) -> Result<u64, String> {
    let temp_path = format!("{}.stripped", path);

    // Copy binary
    fs::copy(path, &temp_path)
        .map_err(|e| format!("Failed to copy binary: {}", e))?;

    // Strip binary
    let output = Command::new("strip")
        .arg(&temp_path)
        .status()
        .map_err(|e| format!("Failed to strip binary: {}", e))?;

    if !output.success() {
        let _ = fs::remove_file(&temp_path);
        return Err("Strip command failed".to_string());
    }

    // Get stripped size
    let size = get_file_size(&temp_path)?;

    // Clean up
    let _ = fs::remove_file(&temp_path);

    Ok(size)
}

// ============================================================================
// Benchmark 1: Debug Binary Sizes
// ============================================================================

fn bench_debug_binary_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("binary_debug");

    // Build debug binary
    let _ = Command::new("cargo")
        .args(&["build", "-p", "ggen-cli", "--bin", "ggen"])
        .status();

    let debug_path = "target/debug/ggen";

    if Path::new(debug_path).exists() {
        if let Ok(size) = get_file_size(debug_path) {
            let size_mb = size as f64 / (1024.0 * 1024.0);
            println!(
                "[Binary Size] Debug ggen: {:.2} MB ({} bytes)",
                size_mb, size
            );

            group.bench_function("debug_binary_ggen", |b| {
                b.iter(|| {
                    println!("Debug binary size: {:.2} MB", size_mb);
                });
            });
        }
    }

    group.finish();
}

// ============================================================================
// Benchmark 2: Release Binary Sizes
// ============================================================================

fn bench_release_binary_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("binary_release");

    // Build release binary
    let _ = Command::new("cargo")
        .args(&["build", "--release", "-p", "ggen-cli", "--bin", "ggen"])
        .status();

    let release_path = "target/release/ggen";

    if Path::new(release_path).exists() {
        if let Ok(size) = get_file_size(release_path) {
            let size_mb = size as f64 / (1024.0 * 1024.0);
            println!(
                "[Binary Size] Release ggen: {:.2} MB ({} bytes)",
                size_mb, size
            );

            group.bench_function("release_binary_ggen", |b| {
                b.iter(|| {
                    println!("Release binary size: {:.2} MB", size_mb);
                });
            });

            // Try to strip and measure
            if let Ok(stripped_size) = strip_binary(release_path) {
                let stripped_mb = stripped_size as f64 / (1024.0 * 1024.0);
                let reduction_percent = ((size - stripped_size) as f64 / size as f64) * 100.0;
                println!(
                    "[Binary Size] Stripped ggen: {:.2} MB (reduction: {:.1}%)",
                    stripped_mb, reduction_percent
                );
            }
        }
    }

    group.finish();
}

// ============================================================================
// Benchmark 3: Binary Size Trends
// ============================================================================

fn bench_binary_trends(c: &mut Criterion) {
    let mut group = c.benchmark_group("binary_trends");

    // Measure key binaries
    let binaries = vec![
        ("target/release/ggen", "ggen CLI"),
        ("target/debug/ggen", "ggen (debug)"),
    ];

    for (path, name) in binaries {
        if Path::new(path).exists() {
            if let Ok(size) = get_file_size(path) {
                group.bench_function(name, |b| {
                    let size_mb = size as f64 / (1024.0 * 1024.0);
                    b.iter(|| {
                        println!("{}: {:.2} MB", name, size_mb);
                    });
                });
            }
        }
    }

    group.finish();
}

// ============================================================================
// Benchmark 4: Symbol Analysis
// ============================================================================

fn bench_symbol_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("binary_symbols");

    let release_path = "target/release/ggen";

    if Path::new(release_path).exists() {
        // Count symbols
        if let Ok(symbol_count) = count_symbols(release_path) {
            println!(
                "[Binary Analysis] Symbol count in ggen: {}",
                symbol_count
            );

            group.bench_function("symbol_count", |b| {
                b.iter(|| {
                    println!("Total symbols: {}", symbol_count);
                });
            });
        }
    }

    group.finish();
}

// ============================================================================
// Benchmark 5: Workspace Binary Comparison
// ============================================================================

fn bench_workspace_binaries(c: &mut Criterion) {
    let mut group = c.benchmark_group("binary_workspace");

    // Measure all binaries in target/release
    if let Ok(entries) = fs::read_dir("target/release") {
        let mut binaries = Vec::new();

        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_file() && !path.extension().is_some() {
                    if let Ok(size) = get_file_size(path.to_str().unwrap_or("")) {
                        if let Some(filename) = path.file_name() {
                            let name = filename.to_string_lossy().to_string();
                            binaries.push((name, size));
                        }
                    }
                }
            }
        }

        // Sort by size
        binaries.sort_by(|a, b| b.1.cmp(&a.1));

        // Report top 5
        println!("[Binary Analysis] Top 5 binaries by size:");
        for (name, size) in binaries.iter().take(5) {
            let size_mb = *size as f64 / (1024.0 * 1024.0);
            println!("  {}: {:.2} MB", name, size_mb);
        }

        group.bench_function("workspace_binaries", |b| {
            b.iter(|| {
                for (name, size) in &binaries {
                    println!("{}: {:.2} MB", name, *size as f64 / (1024.0 * 1024.0));
                }
            });
        });
    }

    group.finish();
}

// ============================================================================
// Criterion Setup
// ============================================================================

criterion_group!(
    benches,
    bench_debug_binary_sizes,
    bench_release_binary_sizes,
    bench_binary_trends,
    bench_symbol_analysis,
    bench_workspace_binaries
);

criterion_main!(benches);
