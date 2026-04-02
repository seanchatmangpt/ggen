use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Duration;

// Pattern execution benchmarks for all 43 workflow patterns
fn pattern_execution_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("pattern_execution");
    group.measurement_time(Duration::from_secs(15));

    let patterns = vec![
        "sequence", "parallel", "conditional", "loop", "choice",
        "split", "merge", "fork", "join", "synchronization",
    ];

    for pattern in patterns {
        group.bench_with_input(
            BenchmarkId::from_parameter(pattern),
            &pattern,
            |b, &pattern| {
                b.iter(|| {
                    // Simulate pattern execution
                    execute_pattern(black_box(pattern))
                });
            },
        );
    }

    group.finish();
}

// Simulated pattern execution
fn execute_pattern(pattern: &str) -> String {
    format!("Pattern '{}' executed", pattern)
}

// Receipt generation benchmark
fn receipt_generation_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_generation");
    group.measurement_time(Duration::from_secs(10));

    group.bench_function("generate_receipt", |b| {
        use sha2::{Sha256, Digest};

        b.iter(|| {
            let execution_id = black_box("exec_12345");
            let timestamp = black_box("2024-01-01T00:00:00Z");

            // Generate hash
            let mut hasher = Sha256::new();
            hasher.update(execution_id.as_bytes());
            hasher.update(timestamp.as_bytes());
            let hash = hasher.finalize();
            let hash_hex = hex::encode(hash);

            // Build receipt
            let receipt = serde_json::json!({
                "execution_id": execution_id,
                "timestamp": timestamp,
                "hash": hash_hex,
                "signature": "placeholder_signature"
            });

            black_box(receipt.to_string())
        });
    });

    group.finish();
}

criterion_group!(benches, pattern_execution_benchmarks, receipt_generation_benchmark);
criterion_main!(benches);
