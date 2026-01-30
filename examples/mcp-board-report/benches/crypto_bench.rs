//! Cryptographic operation benchmarks for MCP+
//!
//! Benchmarks:
//! - SHA-256 hashing (small and large data)
//! - SHA3-256 hashing (small and large data)
//! - Ed25519 signing and verification
//! - Key pair generation
//! - Hash combining (Merkle node construction)
//!
//! Target SLOs:
//! - Ed25519 sign: < 100us
//! - Ed25519 verify: < 200us
//! - SHA-256 (1KB): < 10us

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use mcp_core::crypto::{combine_hashes, hash_sha256, hash_sha256_bytes, hash_sha3_256, KeyPair};

/// Small data size (64 bytes - typical hash input)
const SMALL_SIZE: usize = 64;
/// Large data size (1 MB - large document/image)
const LARGE_SIZE: usize = 1024 * 1024;

fn create_test_data(size: usize) -> Vec<u8> {
    (0..size).map(|i| (i % 256) as u8).collect()
}

/// Benchmark SHA-256 hashing on small data (64 bytes)
fn bench_sha256_small(c: &mut Criterion) {
    let data = create_test_data(SMALL_SIZE);

    let mut group = c.benchmark_group("sha256");
    group.throughput(Throughput::Bytes(SMALL_SIZE as u64));

    group.bench_function(BenchmarkId::new("small", SMALL_SIZE), |b| {
        b.iter(|| hash_sha256(black_box(&data)))
    });

    group.finish();
}

/// Benchmark SHA-256 hashing on large data (1 MB)
fn bench_sha256_large(c: &mut Criterion) {
    let data = create_test_data(LARGE_SIZE);

    let mut group = c.benchmark_group("sha256");
    group.throughput(Throughput::Bytes(LARGE_SIZE as u64));

    group.bench_function(BenchmarkId::new("large", LARGE_SIZE), |b| {
        b.iter(|| hash_sha256(black_box(&data)))
    });

    group.finish();
}

/// Benchmark SHA3-256 hashing on small data (64 bytes)
fn bench_sha3_256_small(c: &mut Criterion) {
    let data = create_test_data(SMALL_SIZE);

    let mut group = c.benchmark_group("sha3_256");
    group.throughput(Throughput::Bytes(SMALL_SIZE as u64));

    group.bench_function(BenchmarkId::new("small", SMALL_SIZE), |b| {
        b.iter(|| hash_sha3_256(black_box(&data)))
    });

    group.finish();
}

/// Benchmark SHA3-256 hashing on large data (1 MB)
fn bench_sha3_256_large(c: &mut Criterion) {
    let data = create_test_data(LARGE_SIZE);

    let mut group = c.benchmark_group("sha3_256");
    group.throughput(Throughput::Bytes(LARGE_SIZE as u64));

    group.bench_function(BenchmarkId::new("large", LARGE_SIZE), |b| {
        b.iter(|| hash_sha3_256(black_box(&data)))
    });

    group.finish();
}

/// Benchmark Ed25519 signing
/// Target SLO: < 100us
fn bench_ed25519_sign(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let data = create_test_data(256); // Typical message size

    c.bench_function("ed25519_sign", |b| {
        b.iter(|| keypair.sign(black_box(&data)))
    });
}

/// Benchmark Ed25519 signature verification
/// Target SLO: < 200us
fn bench_ed25519_verify(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let data = create_test_data(256);
    let signature = keypair.sign(&data);

    c.bench_function("ed25519_verify", |b| {
        b.iter(|| keypair.verify(black_box(&data), black_box(&signature)))
    });
}

/// Benchmark key pair generation
fn bench_keypair_generation(c: &mut Criterion) {
    c.bench_function("keypair_generation", |b| {
        b.iter(|| KeyPair::generate().expect("Failed to generate keypair"))
    });
}

/// Benchmark deterministic key pair from seed
fn bench_keypair_from_seed(c: &mut Criterion) {
    let seed = [42u8; 32];

    c.bench_function("keypair_from_seed", |b| {
        b.iter(|| KeyPair::from_seed(black_box(&seed)))
    });
}

/// Benchmark hash combining (Merkle node construction)
fn bench_hash_combine(c: &mut Criterion) {
    let left = hash_sha256_bytes(b"left-child-hash");
    let right = hash_sha256_bytes(b"right-child-hash");

    c.bench_function("hash_combine", |b| {
        b.iter(|| combine_hashes(black_box(&left), black_box(&right)))
    });
}

/// Benchmark hash combining in a chain (simulating Merkle path)
fn bench_hash_combine_chain(c: &mut Criterion) {
    let hashes: Vec<[u8; 32]> = (0..10)
        .map(|i| hash_sha256_bytes(format!("hash-{}", i).as_bytes()))
        .collect();

    c.bench_function("hash_combine_chain_10", |b| {
        b.iter(|| {
            let mut current = hashes[0];
            for sibling in hashes.iter().skip(1) {
                current = combine_hashes(&current, sibling);
            }
            current
        })
    });
}

/// Benchmark SHA-256 with various sizes to establish throughput curve
fn bench_sha256_throughput(c: &mut Criterion) {
    let sizes = [64, 256, 1024, 4096, 16384, 65536];

    let mut group = c.benchmark_group("sha256_throughput");

    for size in sizes {
        let data = create_test_data(size);
        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &data, |b, data| {
            b.iter(|| hash_sha256(black_box(data)))
        });
    }

    group.finish();
}

/// Benchmark Ed25519 sign/verify cycle (full roundtrip)
fn bench_ed25519_roundtrip(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let data = create_test_data(256);

    c.bench_function("ed25519_roundtrip", |b| {
        b.iter(|| {
            let sig = keypair.sign(black_box(&data));
            keypair.verify(&data, &sig)
        })
    });
}

criterion_group!(
    benches,
    bench_sha256_small,
    bench_sha256_large,
    bench_sha3_256_small,
    bench_sha3_256_large,
    bench_ed25519_sign,
    bench_ed25519_verify,
    bench_keypair_generation,
    bench_keypair_from_seed,
    bench_hash_combine,
    bench_hash_combine_chain,
    bench_sha256_throughput,
    bench_ed25519_roundtrip,
);

criterion_main!(benches);
