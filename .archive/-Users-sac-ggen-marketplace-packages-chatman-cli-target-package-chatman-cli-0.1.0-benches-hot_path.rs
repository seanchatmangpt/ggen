use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Duration;

// Hot-path benchmarks: In-memory operations targeting ≤2ns
fn hot_path_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("hot_path");
    group.measurement_time(Duration::from_secs(10));
    group.warm_up_time(Duration::from_secs(3));

    // Benchmark 1: Hash computation (core operation)
    group.bench_function("hash_computation", |b| {
        use sha2::{Sha256, Digest};
        let data = b"execution_id_12345";
        b.iter(|| {
            let mut hasher = Sha256::new();
            hasher.update(black_box(data));
            black_box(hasher.finalize())
        });
    });

    // Benchmark 2: Pattern lookup (in-memory)
    group.bench_function("pattern_lookup", |b| {
        use std::collections::HashMap;
        let mut patterns = HashMap::new();
        patterns.insert("sequence", "Sequential execution pattern");
        patterns.insert("parallel", "Parallel execution pattern");
        patterns.insert("conditional", "Conditional branching pattern");

        b.iter(|| {
            black_box(patterns.get("sequence"))
        });
    });

    // Benchmark 3: Small string operations
    group.bench_function("string_concat", |b| {
        b.iter(|| {
            let result = format!("{}_{}", black_box("pattern"), black_box("123"));
            black_box(result)
        });
    });

    // Benchmark 4: JSON parsing (small payload)
    group.bench_function("json_parse_small", |b| {
        let json = r#"{"pattern": "sequence", "id": 123}"#;
        b.iter(|| {
            let value: serde_json::Value = serde_json::from_str(black_box(json)).unwrap();
            black_box(value)
        });
    });

    group.finish();
}

// Warm-path benchmarks: File system cached operations
fn warm_path_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("warm_path");
    group.measurement_time(Duration::from_secs(10));

    // Benchmark: Pattern execution with cached data
    group.bench_function("pattern_execution_cached", |b| {
        b.iter(|| {
            // Simulate cached pattern execution
            let pattern_name = black_box("sequence");
            let _result = format!("Executing {}", pattern_name);
            black_box(_result)
        });
    });

    group.finish();
}

criterion_group!(benches, hot_path_benchmarks, warm_path_benchmarks);
criterion_main!(benches);
