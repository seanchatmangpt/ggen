//! Error handling and resilience performance benchmarks
//!
//! Benchmarks for error creation, propagation, and recovery scenarios
//!
//! Run with: cargo bench --bench error_handling_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::fs;
use std::io;
use std::path::PathBuf;
use tempfile::TempDir;

// Simulated error types
#[derive(Debug)]
enum TestError {
    Io(String),
    Parse(String),
    Validation(String),
    NotFound(String),
    PermissionDenied(String),
}

impl From<io::Error> for TestError {
    fn from(e: io::Error) -> Self {
        TestError::Io(e.to_string())
    }
}

fn bench_error_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_creation");

    group.throughput(Throughput::Elements(1));

    group.bench_function("create_io_error", |b| {
        b.iter(|| {
            let _err = TestError::Io(black_box("File not found".to_string()));
        });
    });

    group.bench_function("create_parse_error", |b| {
        b.iter(|| {
            let _err = TestError::Parse(black_box("Invalid JSON".to_string()));
        });
    });

    group.bench_function("create_validation_error", |b| {
        b.iter(|| {
            let _err = TestError::Validation(black_box("Invalid field value".to_string()));
        });
    });

    group.finish();
}

fn bench_error_propagation(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_propagation");

    // Simulate error propagation through call stack
    fn level_1(depth: usize) -> Result<(), TestError> {
        level_2(depth)
    }

    fn level_2(depth: usize) -> Result<(), TestError> {
        if depth == 0 {
            Err(TestError::Validation("Error at level 2".to_string()))
        } else {
            level_3(depth - 1)
        }
    }

    fn level_3(depth: usize) -> Result<(), TestError> {
        if depth == 0 {
            Err(TestError::Validation("Error at level 3".to_string()))
        } else {
            level_4(depth - 1)
        }
    }

    fn level_4(depth: usize) -> Result<(), TestError> {
        if depth == 0 {
            Err(TestError::Validation("Error at level 4".to_string()))
        } else {
            Ok(())
        }
    }

    let depths = vec![
        ("shallow_1_level", 0),
        ("medium_3_levels", 2),
        ("deep_5_levels", 4),
    ];

    for (name, depth) in depths {
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &depth,
            |b, &depth| {
                b.iter(|| {
                    let result = level_1(black_box(depth));
                    let _ = black_box(result);
                });
            },
        );
    }

    group.finish();
}

fn bench_error_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_handling");

    group.bench_function("match_error_arms", |b| {
        b.iter(|| {
            let err = TestError::Validation("Test error".to_string());
            match black_box(err) {
                TestError::Io(_) => "IO Error",
                TestError::Parse(_) => "Parse Error",
                TestError::Validation(_) => "Validation Error",
                TestError::NotFound(_) => "Not Found",
                TestError::PermissionDenied(_) => "Permission Denied",
            };
        });
    });

    group.bench_function("convert_io_error", |b| {
        b.iter(|| {
            let io_err = io::Error::new(io::ErrorKind::NotFound, "File not found");
            let _converted: TestError = io_err.into();
        });
    });

    group.finish();
}

fn bench_recovery_scenarios(c: &mut Criterion) {
    let mut group = c.benchmark_group("recovery_scenarios");
    group.sample_size(20);

    group.bench_function("retry_failed_operation_1_attempt", |b| {
        b.iter(|| {
            let mut attempts = 0;
            loop {
                attempts += 1;
                let _result: Result<String, TestError> = if attempts < 1 {
                    Err(TestError::Io("Temporary error".to_string()))
                } else {
                    Ok("Success".to_string())
                };
                if _result.is_ok() {
                    break;
                }
            }
        });
    });

    group.bench_function("retry_with_backoff_3_attempts", |b| {
        b.iter(|| {
            let mut attempts = 0;
            loop {
                attempts += 1;
                let _result: Result<String, TestError> = if attempts < 3 {
                    Err(TestError::Io("Temporary error".to_string()))
                } else {
                    Ok("Success".to_string())
                };
                if _result.is_ok() {
                    break;
                }
                if attempts > 1 {
                    // Simulate backoff delay (but don't actually sleep in benchmark)
                    let _ = black_box(2u64.pow((attempts - 1) as u32));
                }
            }
        });
    });

    group.finish();
}

fn bench_file_error_scenarios(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_error_scenarios");

    group.bench_function("handle_missing_file_error", |b| {
        let temp_dir = TempDir::new().unwrap();
        let missing_path = temp_dir.path().join("nonexistent.txt");

        b.iter(|| {
            match fs::read_to_string(black_box(&missing_path)) {
                Ok(_) => "Success",
                Err(_) => "File not found error handled",
            };
        });
    });

    group.bench_function("handle_permission_denied_error", |b| {
        b.iter(|| {
            let path = PathBuf::from("/root/protected_file.txt");
            match fs::read_to_string(black_box(&path)) {
                Ok(_) => "Success",
                Err(e) if e.kind() == io::ErrorKind::PermissionDenied => "Permission denied",
                Err(_) => "Other error",
            };
        });
    });

    group.finish();
}

fn bench_error_context_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_context_overhead");

    group.bench_function("error_without_context", |b| {
        b.iter(|| {
            let _err = TestError::Validation("Simple error".to_string());
        });
    });

    group.bench_function("error_with_file_info", |b| {
        b.iter(|| {
            let _err = TestError::Validation(format!(
                "Error in file {} at line {} column {}",
                "example.rs", 42, 10
            ));
        });
    });

    group.bench_function("error_with_full_context", |b| {
        b.iter(|| {
            let context = format!(
                "Error: Invalid configuration\nFile: {}\nLine: {}\nColumn: {}\nExpected: {}\nFound: {}",
                "ggen.toml", 15, 8, "string", "number"
            );
            let _err = TestError::Validation(context);
        });
    });

    group.finish();
}

fn bench_result_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("result_operations");

    group.bench_function("ok_unwrap", |b| {
        b.iter(|| {
            let result: Result<i32, TestError> = Ok(42);
            let _value = result.unwrap();
        });
    });

    group.bench_function("ok_map", |b| {
        b.iter(|| {
            let result: Result<i32, TestError> = Ok(42);
            let _mapped = result.map(|v| v * 2);
        });
    });

    group.bench_function("err_unwrap_or_else", |b| {
        b.iter(|| {
            let result: Result<i32, TestError> = Err(TestError::Validation("error".to_string()));
            let _value = result.unwrap_or_else(|_| 0);
        });
    });

    group.bench_function("ok_and_then", |b| {
        b.iter(|| {
            let result: Result<i32, TestError> = Ok(42);
            let _chained = result.and_then(|v| Ok(v * 2));
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_error_creation,
    bench_error_propagation,
    bench_error_handling,
    bench_recovery_scenarios,
    bench_file_error_scenarios,
    bench_error_context_overhead,
    bench_result_operations,
);
criterion_main!(benches);
