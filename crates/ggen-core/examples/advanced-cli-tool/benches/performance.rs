//! Performance benchmarks using Criterion

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use sha2::{Digest, Sha256};
use std::fs;
use tempfile::TempDir;

fn benchmark_hashing(c: &mut Criterion) {
    let mut group = c.benchmark_group("hashing");

    for size in [1024, 4096, 16384, 65536].iter() {
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            let data = vec![0u8; size];
            b.iter(|| {
                let mut hasher = Sha256::new();
                hasher.update(black_box(&data));
                hasher.finalize()
            });
        });
    }

    group.finish();
}

fn benchmark_file_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_operations");

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("benchmark.txt");
    let data = vec![0u8; 1024 * 1024]; // 1MB

    group.bench_function("write_1mb", |b| {
        b.iter(|| {
            fs::write(&file_path, black_box(&data)).unwrap();
        });
    });

    // Setup for read benchmark
    fs::write(&file_path, &data).unwrap();

    group.bench_function("read_1mb", |b| {
        b.iter(|| {
            fs::read(black_box(&file_path)).unwrap()
        });
    });

    group.finish();
}

fn benchmark_buffer_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("buffer_sizes");

    let data = vec![0u8; 1024 * 1024]; // 1MB

    for buffer_size in [1024, 4096, 8192, 16384, 65536].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(buffer_size),
            buffer_size,
            |b, &buffer_size| {
                b.iter(|| {
                    let mut processed = 0;
                    let mut buffer = vec![0u8; buffer_size];
                    let mut offset = 0;

                    while offset < data.len() {
                        let chunk_size = (data.len() - offset).min(buffer_size);
                        buffer[..chunk_size].copy_from_slice(&data[offset..offset + chunk_size]);
                        processed += chunk_size;
                        offset += chunk_size;

                        black_box(&buffer);
                    }

                    processed
                });
            },
        );
    }

    group.finish();
}

fn benchmark_parallel_processing(c: &mut Criterion) {
    use rayon::prelude::*;

    let mut group = c.benchmark_group("parallel");

    let data: Vec<Vec<u8>> = (0..100)
        .map(|_| vec![0u8; 1024])
        .collect();

    group.bench_function("sequential", |b| {
        b.iter(|| {
            data.iter()
                .map(|chunk| {
                    let mut hasher = Sha256::new();
                    hasher.update(chunk);
                    hasher.finalize()
                })
                .collect::<Vec<_>>()
        });
    });

    group.bench_function("parallel", |b| {
        b.iter(|| {
            data.par_iter()
                .map(|chunk| {
                    let mut hasher = Sha256::new();
                    hasher.update(chunk);
                    hasher.finalize()
                })
                .collect::<Vec<_>>()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_hashing,
    benchmark_file_operations,
    benchmark_buffer_sizes,
    benchmark_parallel_processing
);
criterion_main!(benches);
