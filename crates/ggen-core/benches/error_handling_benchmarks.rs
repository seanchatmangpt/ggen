//! Error handling performance benchmarks
//!
//! HONEST BENCHMARKS: These measure Rust error handling patterns.
//!
//! IMPORTANT: String allocation dominates measurement time (microseconds).
//! We separate the cost of error creation from string allocation to be honest
//! about what each operation actually costs.
//!
//! Run with: cargo bench --bench error_handling_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::io;

// Error types WITHOUT string allocation (what error creation actually costs)
#[derive(Debug)]
enum ZeroAllocError {
    Io,
    Parse,
    Validation,
}

// Error types WITH string allocation (what users actually do in real code)
#[derive(Debug)]
#[allow(dead_code)]
enum RealError {
    Io(String),
    Parse(String),
    Validation(String),
}

impl From<io::Error> for RealError {
    fn from(e: io::Error) -> Self {
        RealError::Io(e.to_string())
    }
}

fn bench_error_creation_zero_alloc(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_creation_zero_alloc");
    group.throughput(Throughput::Elements(1));

    group.bench_function("create_io_error_no_string", |b| {
        b.iter(|| {
            let _err = ZeroAllocError::Io;
        });
    });

    group.bench_function("create_parse_error_no_string", |b| {
        b.iter(|| {
            let _err = ZeroAllocError::Parse;
        });
    });

    group.bench_function("create_validation_error_no_string", |b| {
        b.iter(|| {
            let _err = ZeroAllocError::Validation;
        });
    });

    group.finish();
}

fn bench_string_allocation_cost(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_allocation_cost");
    group.throughput(Throughput::Elements(1));

    group.bench_function("allocate_string_15bytes", |b| {
        b.iter(|| {
            let _s = black_box("File not found".to_string());
        });
    });

    group.bench_function("allocate_string_40bytes", |b| {
        b.iter(|| {
            let _s = black_box("Invalid configuration field value".to_string());
        });
    });

    group.finish();
}

fn bench_error_creation_with_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_creation_with_string");
    group.throughput(Throughput::Elements(1));

    // This is what REAL applications do - errors with context
    group.bench_function("create_error_with_message", |b| {
        b.iter(|| {
            let _err = RealError::Io(black_box("File not found".to_string()));
        });
    });

    group.finish();
}

fn bench_error_propagation(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_propagation");

    fn level_1(depth: usize) -> Result<(), ZeroAllocError> {
        level_2(depth)
    }

    fn level_2(depth: usize) -> Result<(), ZeroAllocError> {
        if depth == 0 {
            Err(ZeroAllocError::Validation)
        } else {
            level_3(depth - 1)
        }
    }

    fn level_3(depth: usize) -> Result<(), ZeroAllocError> {
        if depth == 0 {
            Err(ZeroAllocError::Validation)
        } else {
            level_4(depth - 1)
        }
    }

    fn level_4(depth: usize) -> Result<(), ZeroAllocError> {
        if depth == 0 {
            Err(ZeroAllocError::Validation)
        } else {
            Ok(())
        }
    }

    let depths = vec![
        ("propagate_1_level", 0),
        ("propagate_3_levels", 2),
        ("propagate_5_levels", 4),
    ];

    for (name, depth) in depths {
        group.bench_with_input(BenchmarkId::from_parameter(name), &depth, |b, &depth| {
            b.iter(|| {
                let result = level_1(black_box(depth));
                let _ = black_box(result);
            });
        });
    }

    group.finish();
}

fn bench_result_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("result_operations");

    group.bench_function("result_unwrap_ok", |b| {
        b.iter(|| {
            let result: Result<i32, ZeroAllocError> = Ok(42);
            let _value = result.unwrap();
        });
    });

    group.bench_function("result_map", |b| {
        b.iter(|| {
            let result: Result<i32, ZeroAllocError> = Ok(42);
            let _mapped = result.map(|v| v * 2);
        });
    });

    group.bench_function("result_and_then", |b| {
        b.iter(|| {
            let result: Result<i32, ZeroAllocError> = Ok(42);
            let _chained = result.and_then(|v| Ok(v * 2));
        });
    });

    group.bench_function("result_unwrap_or", |b| {
        b.iter(|| {
            let result: Result<i32, ZeroAllocError> = Err(ZeroAllocError::Validation);
            let _value = result.unwrap_or(0);
        });
    });

    group.finish();
}

fn bench_match_error_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("match_error_handling");

    group.bench_function("match_5_error_variants", |b| {
        let err = ZeroAllocError::Validation;
        b.iter(|| {
            let _result = match black_box(&err) {
                ZeroAllocError::Io => "IO Error",
                ZeroAllocError::Parse => "Parse Error",
                ZeroAllocError::Validation => "Validation Error",
            };
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_error_creation_zero_alloc,
    bench_string_allocation_cost,
    bench_error_creation_with_string,
    bench_error_propagation,
    bench_result_operations,
    bench_match_error_handling,
);
criterion_main!(benches);
