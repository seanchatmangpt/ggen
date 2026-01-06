//! Disk I/O performance benchmarks
//!
//! Measures real filesystem operations: file read/write, directory creation, metadata checks.
//! These benchmarks measure actual OS-level I/O performance on the test system.
//!
//! Methodology:
//! - Uses tempfile crate for isolated test directories
//! - Measures actual fs::read_to_string, fs::write, fs::create_dir_all
//! - Varies file sizes and operation counts to show scaling behavior
//! - Reports throughput (bytes/sec) and latency with variance
//!
//! Note: I/O performance is system-dependent. These benchmarks show relative performance.
//!
//! Run with: cargo bench --bench disk_io_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tempfile::TempDir;

fn create_file_structure(dir: &TempDir, num_files: usize) -> Vec<PathBuf> {
    let mut paths = Vec::new();

    for i in 0..num_files {
        let file_path = dir.path().join(format!("file_{:04}.rs", i));
        let content = format!(
            "// Generated file {}\npub fn function_{}() {{\n    println!(\"File {}\");\n}}\n",
            i, i, i
        );
        fs::write(&file_path, content).expect("Failed to write file");
        paths.push(file_path);
    }

    paths
}

fn bench_single_file_write(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_file_write");

    let sizes = vec![
        ("small", 100),      // 100 bytes
        ("medium", 1_000),   // 1 KB
        ("large", 10_000),   // 10 KB
        ("xlarge", 100_000), // 100 KB
    ];

    for (name, size) in sizes {
        let content = "x".repeat(size);

        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &content, |b, content| {
            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();
                let file_path = temp_dir.path().join("test.txt");
                fs::write(black_box(&file_path), black_box(content))
                    .expect("Failed to write file");
            });
        });
    }

    group.finish();
}

fn bench_single_file_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_file_read");

    let sizes = vec![
        ("small", 100),
        ("medium", 1_000),
        ("large", 10_000),
        ("xlarge", 100_000),
    ];

    for (name, size) in sizes {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        let content = "x".repeat(size);
        fs::write(&file_path, &content).expect("Failed to write file");

        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &file_path, |b, path| {
            b.iter(|| {
                let _content = fs::read_to_string(black_box(path))
                    .expect("Failed to read file");
            });
        });
    }

    group.finish();
}

fn bench_multiple_file_writes(c: &mut Criterion) {
    let mut group = c.benchmark_group("multiple_file_writes");

    let file_counts = vec![
        ("10_files", 10),
        ("50_files", 50),
        ("100_files", 100),
        ("500_files", 500),
    ];

    for (name, count) in file_counts {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &count, |b, &count| {
            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();
                for i in 0..count {
                    let file_path = temp_dir.path().join(format!("file_{:04}.rs", i));
                    let content = format!("// File {}\npub fn func_{}() {{}}\n", i, i);
                    fs::write(&file_path, &content).expect("Failed to write file");
                }
            });
        });
    }

    group.finish();
}

fn bench_multiple_file_reads(c: &mut Criterion) {
    let mut group = c.benchmark_group("multiple_file_reads");

    let file_counts = vec![
        ("10_files", 10),
        ("50_files", 50),
        ("100_files", 100),
        ("500_files", 500),
    ];

    for (name, count) in file_counts {
        let temp_dir = TempDir::new().unwrap();
        let paths = create_file_structure(&temp_dir, count);

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &paths,
            |b, paths| {
                b.iter(|| {
                    for path in paths {
                        let _content = fs::read_to_string(black_box(path))
                            .expect("Failed to read file");
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_directory_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("directory_creation");

    let dir_depths = vec![
        ("1_level", 1),
        ("5_levels", 5),
        ("10_levels", 10),
    ];

    for (name, depth) in dir_depths {
        group.bench_with_input(BenchmarkId::from_parameter(name), &depth, |b, &depth| {
            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();
                let mut current_path = temp_dir.path().to_path_buf();

                for i in 0..depth {
                    current_path.push(format!("dir_{}", i));
                    fs::create_dir_all(black_box(&current_path))
                        .expect("Failed to create directory");
                }
            });
        });
    }

    group.finish();
}

fn bench_file_operations_sequence(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_operations_sequence");
    group.sample_size(20);

    group.bench_function("write_then_read_single_file", |b| {
        b.iter(|| {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.rs");
            let content = "pub fn main() { println!(\"Hello\"); }";

            // Write
            fs::write(&file_path, content).expect("Failed to write");
            // Read
            let _read_content = fs::read_to_string(&file_path)
                .expect("Failed to read");
        });
    });

    group.bench_function("write_read_modify_5_files", |b| {
        b.iter(|| {
            let temp_dir = TempDir::new().unwrap();

            for i in 0..5 {
                let file_path = temp_dir.path().join(format!("file_{}.rs", i));
                let content = format!("pub fn func_{}() {{}}\n", i);

                // Write
                fs::write(&file_path, &content).expect("Failed to write");
                // Read
                let mut content = fs::read_to_string(&file_path)
                    .expect("Failed to read");
                // Modify
                content.push_str("// Modified\n");
                // Write again
                fs::write(&file_path, &content).expect("Failed to write");
            }
        });
    });

    group.finish();
}

fn bench_permission_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("permission_checks");

    group.bench_function("check_file_exists", |b| {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        fs::write(&file_path, "test").expect("Failed to write file");

        b.iter(|| {
            let _exists = black_box(&file_path).exists();
        });
    });

    group.bench_function("check_file_readability", |b| {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        fs::write(&file_path, "test").expect("Failed to write file");

        b.iter(|| {
            let metadata = fs::metadata(black_box(&file_path))
                .expect("Failed to get metadata");
            let _readable = !metadata.permissions().readonly();
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_single_file_write,
    bench_single_file_read,
    bench_multiple_file_writes,
    bench_multiple_file_reads,
    bench_directory_creation,
    bench_file_operations_sequence,
    bench_permission_checks,
);
criterion_main!(benches);
