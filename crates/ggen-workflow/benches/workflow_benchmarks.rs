//! Workflow benchmarks for performance measurement
//!
//! This benchmark suite measures the performance of various workflow patterns
//! and operations to ensure they meet SLO targets and identify optimization opportunities.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Duration;

// Import the workflow modules
use ggen_workflow::{
    Sequence, Parallel, Choice, Sync, WorkflowContext,
    ReceiptGenerator, ReceiptStore, Constants
};

/// Benchmark workflow pattern execution
fn benchmark_workflow_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("Workflow Patterns");

    // Configure benchmark timeout
    group.measurement_time(Duration::from_secs(30));

    // Benchmark sequence pattern
    group.bench_with_input(
        BenchmarkId::new("sequence", 10),
        &10,
        |b, &steps| {
            let sequence = Sequence {
                steps: (1..=steps).map(|i| format!("step_{}", i)).collect(),
            };

            let context = WorkflowContext::default();

            b.iter(|| {
                black_box(sequence.execute(&context).unwrap())
            });
        },
    );

    // Benchmark parallel pattern
    group.bench_with_input(
        BenchmarkId::new("parallel", 10),
        &10,
        |b, &tasks| {
            let parallel = Parallel {
                steps: (1..=tasks).map(|i| format!("task_{}", i)).collect(),
                sync_config: Default::default(),
            };

            let context = WorkflowContext::default();

            b.iter(|| {
                black_box(parallel.execute(&context).unwrap())
            });
        },
    );

    // Benchmark sync pattern
    group.bench_with_input(
        BenchmarkId::new("sync", 10),
        &10,
        |b, &barriers| {
            let sync = Sync {
                steps: (1..=barriers).map(|i| format!("barrier_{}", i)).collect(),
                sync_config: Default::default(),
            };

            let context = WorkflowContext::default();

            b.iter(|| {
                black_box(sync.execute(&context).unwrap())
            });
        },
    );

    group.finish();
}

/// Benchmark receipt generation performance
fn benchmark_receipt_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("Receipt Generation");

    group.measurement_time(Duration::from_secs(15));

    let generator = ReceiptGenerator::new();

    // Benchmark receipt generation with different input sizes
    group.bench_with_input(
        BenchmarkId::new("small_receipt", 10),
        &10,
        |b, &size| {
            let mut context = WorkflowContext::default();

            // Add input data
            for i in 0..size {
                context.input.insert(
                    format!("input_{}", i),
                    serde_json::json!({
                        "id": i,
                        "value": i * 2,
                        "active": i % 2 == 0
                    })
                );
            }

            b.iter(|| {
                black_box(generator.generate_receipt(&context).unwrap())
            });
        },
    );

    // Benchmark receipt verification
    group.bench_function("receipt_verification", |b| {
        let mut context = WorkflowContext::default();
        context.input.insert("test".to_string(), serde_json::json!("value"));

        let receipt = generator.generate_receipt(&context).unwrap();

        b.iter(|| {
            black_box(generator.verify_receipt(&receipt).unwrap())
        });
    });

    group.finish();
}

/// Benchmark workflow constants access
fn benchmark_constants_access(c: &mut Criterion) {
    c.bench_function("constants_access", |b| {
        b.iter(|| {
            black_box(Constants::default())
        });
    });
}

/// Benchmark trace event creation
fn benchmark_trace_creation(c: &mut Criterion) {
    c.bench_function("trace_event_creation", |b| {
        b.iter(|| {
            use ggen_workflow::TraceEvent;
            black_box(TraceEvent {
                step: "test_step".to_string(),
                event_type: "test_event".to_string(),
                timestamp: chrono::Utc::now(),
                message: "Test message".to_string(),
            })
        });
    });
}

/// Benchmark workflow context operations
fn benchmark_context_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("Context Operations");

    group.measurement_time(Duration::from_secs(10));

    // Benchmark context creation
    group.bench_function("context_creation", |b| {
        b.iter(|| {
            black_box(WorkflowContext::default())
        });
    });

    // Benchmark input insertion
    group.bench_function("input_insertion", |b| {
        let mut context = WorkflowContext::default();
        b.iter(|| {
            context.input.insert("key".to_string(), serde_json::json!("value"));
        });
    });

    // Benchmark output insertion
    group.bench_function("output_insertion", |b| {
        let mut context = WorkflowContext::default();
        b.iter(|| {
            context.output.insert("result".to_string(), serde_json::json!({"success": true}));
        });
    });

    group.finish();
}

/// Benchmark receipt storage operations
fn benchmark_receipt_storage(c: &mut Criterion) {
    let mut group = c.benchmark_group("Receipt Storage");

    group.measurement_time(Duration::from_secs(10));

    let mut store = ReceiptStore::new();
    let generator = ReceiptGenerator::new();
    let mut context = WorkflowContext::default();
    context.input.insert("test".to_string(), serde_json::json!("value"));

    let receipt = generator.generate_receipt(&context).unwrap();

    // Benchmark receipt storage
    group.bench_function("store_receipt", |b| {
        let mut store = ReceiptStore::new();
        let receipt = generator.generate_receipt(&context).unwrap();

        b.iter(|| {
            black_box(store.store_receipt(receipt.clone()).unwrap())
        });
    });

    // Benchmark retrieval
    group.bench_function("retrieve_receipt", |b| {
        let mut store = ReceiptStore::new();
        let receipt = generator.generate_receipt(&context).unwrap();
        store.store_receipt(receipt).unwrap();

        b.iter(|| {
            black_box(store.get_receipt("receipt_id"))
        });
    });

    // Benchmark listing all receipts
    group.bench_function("list_receipts", |b| {
        let mut store = ReceiptStore::new();
        for i in 0..100 {
            let mut ctx = WorkflowContext::default();
            ctx.input.insert("test".to_string(), serde_json::json!(i));
            let receipt = generator.generate_receipt(&ctx).unwrap();
            store.store_receipt(receipt).unwrap();
        }

        b.iter(|| {
            black_box(store.list_receipts())
        });
    });

    group.finish();
}

/// Benchmark overall workflow performance
fn benchmark_workflow_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("Overall Performance");

    group.measurement_time(Duration::from_secs(30));

    // Simulate a realistic workflow scenario
    group.bench_function("realistic_workflow", |b| {
        let sequence = Sequence {
            steps: vec![
                "initialize".to_string(),
                "validate_input".to_string(),
                "process_data".to_string(),
                "generate_output".to_string(),
                "cleanup".to_string(),
            ],
        };

        let parallel = Parallel {
            steps: vec![
                "log_event".to_string(),
                "notify_users".to_string(),
                "update_metrics".to_string(),
            ],
            sync_config: Default::default(),
        };

        let context = WorkflowContext::default();
        context.input.insert("workflow_type".to_string(), serde_json::json!("realistic_test"));

        b.iter(|| {
            black_box(sequence.execute(&context).unwrap());
            black_box(parallel.execute(&context).unwrap());
        });
    });

    group.finish();
}

// Criterion main function
criterion_group!(
    benches,
    benchmark_workflow_patterns,
    benchmark_receipt_generation,
    benchmark_constants_access,
    benchmark_trace_creation,
    benchmark_context_operations,
    benchmark_receipt_storage,
    benchmark_workflow_performance
);
criterion_main!(benches);