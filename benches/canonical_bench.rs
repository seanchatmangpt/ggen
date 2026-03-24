use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_canonical::hash::compute_hash;
use ggen_canonical::json::{canonicalize_json, canonicalize_json_str, JsonCanonicalizer};
use ggen_canonical::Canonicalizer;
use serde_json::{json, Value};
use std::collections::HashMap;

fn bench_json_canonicalization_simple(c: &mut Criterion) {
    let input = json!({
        "z": 1,
        "a": 2,
        "m": 3
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("json_canonicalize_simple", |b| {
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });
}

fn bench_json_canonicalization_nested(c: &mut Criterion) {
    let input = json!({
        "outer1": {
            "z": 1,
            "a": 2,
            "inner": {
                "y": 3,
                "x": 4
            }
        },
        "outer2": {
            "b": 5,
            "a": 6
        }
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("json_canonicalize_nested", |b| {
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });
}

fn bench_json_canonicalization_array(c: &mut Criterion) {
    let input = json!({
        "items": [
            {"z": 1, "a": 2},
            {"y": 3, "b": 4},
            {"x": 5, "c": 6}
        ]
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("json_canonicalize_array", |b| {
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });
}

fn bench_json_canonicalization_large(c: &mut Criterion) {
    let mut group = c.benchmark_group("json_canonicalize_large");

    for size in [10, 100, 1000].iter() {
        let mut map = serde_json::Map::new();
        for i in 0..*size {
            map.insert(format!("field_{}", i), json!(i));
        }
        let input = Value::Object(map);

        group.throughput(Throughput::Elements(*size));
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &input,
            |b, input| {
                let canonicalizer = JsonCanonicalizer::new();
                b.iter(|| {
                    black_box(canonicalizer.canonicalize(input.clone()).unwrap());
                });
            },
        );
    }

    group.finish();
}

fn bench_json_string_canonicalization(c: &mut Criterion) {
    let input = r#"{"z":1,"a":2,"m":3}"#;

    c.bench_function("json_canonicalize_str", |b| {
        b.iter(|| {
            black_box(canonicalize_json_str(input).unwrap());
        });
    });
}

fn bench_json_struct_canonicalization(c: &mut Criterion) {
    #[derive(serde::Serialize)]
    struct TestStruct {
        z: i32,
        a: i32,
        m: i32,
    }

    let value = TestStruct { z: 1, a: 2, m: 3 };

    c.bench_function("json_canonicalize_struct", |b| {
        b.iter(|| {
            black_box(canonicalize_json(&value).unwrap());
        });
    });
}

fn bench_hash_computation(c: &mut Criterion) {
    let mut group = c.benchmark_group("hash_computation");

    for size in [100, 1_000, 10_000, 100_000].iter() {
        let data = vec![0u8; *size];

        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &data,
            |b, data| {
                b.iter(|| {
                    black_box(compute_hash(data).unwrap());
                });
            },
        );
    }

    group.finish();
}

fn bench_json_canonicalize_and_hash(c: &mut Criterion) {
    let input = json!({
        "operation": "test",
        "timestamp": "2024-01-01T00:00:00Z",
        "data": {
            "field1": "value1",
            "field2": "value2",
            "field3": "value3"
        }
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("json_canonicalize_and_hash", |b| {
        b.iter(|| {
            black_box(canonicalizer.hash(input.clone()).unwrap());
        });
    });
}

fn bench_determinism_verification(c: &mut Criterion) {
    let input = json!({
        "field3": "value3",
        "field1": "value1",
        "field2": "value2"
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("determinism_verification", |b| {
        b.iter(|| {
            let result1 = canonicalizer.canonicalize(input.clone()).unwrap();
            let result2 = canonicalizer.canonicalize(input.clone()).unwrap();
            black_box(result1 == result2);
        });
    });
}

fn bench_pretty_vs_compact(c: &mut Criterion) {
    let input = json!({
        "a": 1,
        "b": 2,
        "c": {
            "d": 3,
            "e": 4
        }
    });

    let mut group = c.benchmark_group("pretty_vs_compact");

    group.bench_function("compact", |b| {
        let canonicalizer = JsonCanonicalizer::new();
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });

    group.bench_function("pretty", |b| {
        let canonicalizer = JsonCanonicalizer::new_pretty();
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });

    group.finish();
}

fn bench_real_world_receipt(c: &mut Criterion) {
    let receipt = json!({
        "operation_id": "op-12345",
        "timestamp": "2024-01-01T12:00:00Z",
        "input_hashes": [
            "abc123def456",
            "ghi789jkl012"
        ],
        "output_hashes": [
            "mno345pqr678"
        ],
        "signature": "sig_abcdef123456789",
        "previous_receipt_hash": "prev_hash_987654321"
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("real_world_receipt", |b| {
        b.iter(|| {
            black_box(canonicalizer.hash(receipt.clone()).unwrap());
        });
    });
}

fn bench_batch_canonicalization(c: &mut Criterion) {
    let mut group = c.benchmark_group("batch_canonicalization");

    for count in [10, 100, 1000].iter() {
        let inputs: Vec<Value> = (0..*count)
            .map(|i| {
                json!({
                    "id": i,
                    "field_z": "value_z",
                    "field_a": "value_a",
                    "field_m": "value_m"
                })
            })
            .collect();

        group.throughput(Throughput::Elements(*count));
        group.bench_with_input(
            BenchmarkId::from_parameter(count),
            &inputs,
            |b, inputs| {
                let canonicalizer = JsonCanonicalizer::new();
                b.iter(|| {
                    for input in inputs {
                        black_box(canonicalizer.canonicalize(input.clone()).unwrap());
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_complex_nested_structure(c: &mut Criterion) {
    let input = json!({
        "level1_z": {
            "level2_y": {
                "level3_x": {
                    "z": 1,
                    "a": 2
                },
                "level3_a": {
                    "m": 3,
                    "b": 4
                }
            },
            "level2_a": {
                "data": [1, 2, 3, 4, 5]
            }
        },
        "level1_a": {
            "simple": "value"
        }
    });

    let canonicalizer = JsonCanonicalizer::new();

    c.bench_function("complex_nested_structure", |b| {
        b.iter(|| {
            black_box(canonicalizer.canonicalize(input.clone()).unwrap());
        });
    });
}

criterion_group!(
    benches,
    bench_json_canonicalization_simple,
    bench_json_canonicalization_nested,
    bench_json_canonicalization_array,
    bench_json_canonicalization_large,
    bench_json_string_canonicalization,
    bench_json_struct_canonicalization,
    bench_hash_computation,
    bench_json_canonicalize_and_hash,
    bench_determinism_verification,
    bench_pretty_vs_compact,
    bench_real_world_receipt,
    bench_batch_canonicalization,
    bench_complex_nested_structure,
);
criterion_main!(benches);
