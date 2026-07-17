//! Criterion benchmarks for ggen.
//!
//! Three benchmark patterns are demonstrated:
//! 1. **Throughput** — measures bytes/sec for a hashing operation
//! 2. **Latency** — measures single-operation round-trip time
//! 3. **Scaling** — parametric sweep with `BenchmarkId` over input sizes

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::hint::black_box;

// ---------------------------------------------------------------------------
// Subject under benchmark
// ---------------------------------------------------------------------------

/// Compute a BLAKE3 digest of `data` and return the hex string.
/// This stands in for the canonical content-addressing path used throughout
/// the crate.  Replace with the real public API when the crate grows one.
fn hash_bytes(data: &[u8]) -> String {
    blake3::hash(data).to_hex().to_string()
}

/// Serialize a key-value payload to canonical JSON.
/// Mirrors the deterministic-serialization requirement for receipt events.
fn serialize_payload(key: &str, value: u64) -> Vec<u8> {
    // Manually built so there is no serde dependency in the bench binary.
    format!("{{\"key\":\"{key}\",\"value\":{value}}}").into_bytes()
}

/// Round-trip: serialize then hash.  Represents the emit-event hot path.
fn emit_round_trip(key: &str, value: u64) -> String {
    let bytes = serialize_payload(key, value);
    hash_bytes(&bytes)
}

// ---------------------------------------------------------------------------
// 1. Throughput benchmark — bytes/sec
// ---------------------------------------------------------------------------

fn bench_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput/hash_bytes");

    for size in [64_usize, 256, 1024, 4096, 16_384] {
        let data: Vec<u8> = (0..size).map(|i| (i & 0xFF) as u8).collect();

        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &data, |b, d| {
            b.iter(|| hash_bytes(black_box(d)));
        });
    }

    group.finish();
}

// ---------------------------------------------------------------------------
// 2. Latency benchmark — single operation
// ---------------------------------------------------------------------------

fn bench_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("latency");

    // serialize_payload: pure CPU, no I/O
    group.bench_function("serialize_payload", |b| {
        b.iter(|| serialize_payload(black_box("event_type"), black_box(42)));
    });

    // hash_bytes on a small, realistic event payload (~60 bytes)
    let sample = serialize_payload("build", 1);
    group.bench_function("hash_bytes/small", |b| {
        b.iter(|| hash_bytes(black_box(&sample)));
    });

    // full emit round-trip (serialize + hash)
    group.bench_function("emit_round_trip", |b| {
        b.iter(|| emit_round_trip(black_box("build"), black_box(1)));
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// 3. Scaling benchmark — parametric with BenchmarkId
// ---------------------------------------------------------------------------

fn bench_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("scaling/emit_chain");

    // Simulate building a chain of N events (sequential hashing).
    for n in [1_usize, 10, 100, 500] {
        group.bench_with_input(BenchmarkId::new("chain_length", n), &n, |b, &len| {
            b.iter(|| {
                let mut rolling = String::new();
                for seq in 0..len {
                    let payload = serialize_payload("build", seq as u64);
                    // Mix previous hash into the next digest to model a chain.
                    let combined = [rolling.as_bytes(), &payload].concat();
                    rolling = hash_bytes(black_box(&combined));
                }
                black_box(rolling)
            });
        });
    }

    group.finish();
}

// ---------------------------------------------------------------------------
// Criterion entry points
// ---------------------------------------------------------------------------

criterion_group!(benches, bench_throughput, bench_latency, bench_scaling);
criterion_main!(benches);
