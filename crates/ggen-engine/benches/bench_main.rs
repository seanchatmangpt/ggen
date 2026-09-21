//! Criterion benchmarks for ggen.
//!
//! Three benchmark patterns are demonstrated:
//! 1. **Throughput** — measures bytes/sec for a hashing operation
//! 2. **Latency** — measures single-operation round-trip time
//! 3. **Scaling** — parametric sweep with `BenchmarkId` over input sizes

// `criterion_main!`'s generated `fn main` has no doc comment of its own
// (same reason `blue_river_dam.rs`, this crate's other bench binary,
// disables the lint) -- a bench binary is not a public API surface.
#![allow(missing_docs)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_engine::{pack::Pack, pack_scope::benchmark_dfcm_scopes};
use std::{
    collections::{BTreeMap, BTreeSet},
    hint::black_box,
    path::PathBuf,
};

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
        let data: Vec<u8> = (0..size)
            .map(|i| u8::try_from(i & 0xFF).unwrap_or(0))
            .collect();

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
// 4. DfCM dependency-scope benchmark
// ---------------------------------------------------------------------------

fn synthetic_pack(
    name: &str, dependencies: &[&str], semantic_type: &str, capability: &str,
) -> Pack {
    Pack {
        name: name.to_string(),
        version: "1.0.0".to_string(),
        description: "benchmark fixture".to_string(),
        dependencies: dependencies
            .iter()
            .map(|dependency| ((*dependency).to_string(), "1.0.0".to_string()))
            .collect::<BTreeMap<_, _>>(),
        semantic_types: BTreeSet::from([semantic_type.to_string()]),
        provides: BTreeSet::from([capability.to_string()]),
        requires: BTreeSet::new(),
        root: PathBuf::new(),
        ontology_path: PathBuf::new(),
        extra_ontology_paths: Vec::new(),
        template_paths: Vec::new(),
        lock: false,
    }
}

fn bench_dfcm_dependency_scope(c: &mut Criterion) {
    let packs = vec![
        synthetic_pack("a-root", &["b-direct"], "application", "cap.root"),
        synthetic_pack("b-direct", &["c-twohop"], "runtime", "cap.direct"),
        synthetic_pack("c-twohop", &[], "projection", "cap.twohop"),
        synthetic_pack("z-global", &[], "fallback", "cap.global"),
    ];
    let relevant = BTreeSet::from(["c-twohop".to_string()]);

    c.bench_function("dfcm/dependency_scope/local_direct_twohop_global", |b| {
        b.iter(|| {
            benchmark_dfcm_scopes(
                black_box(&packs),
                black_box("a-root"),
                black_box(&relevant),
                black_box(3),
            )
            .expect("synthetic scope benchmark")
        });
    });
}

// ---------------------------------------------------------------------------
// Criterion entry points
// ---------------------------------------------------------------------------

criterion_group!(
    benches,
    bench_throughput,
    bench_latency,
    bench_scaling,
    bench_dfcm_dependency_scope
);
criterion_main!(benches);
