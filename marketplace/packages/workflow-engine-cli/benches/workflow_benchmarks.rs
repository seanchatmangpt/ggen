use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use workflow_engine::prelude::*;

fn workflow_creation_benchmark(c: &mut Criterion) {
    c.bench_function("workflow creation", |b| {
        b.iter(|| {
            WorkflowManager::create_from_file(
                black_box("bench/test.bpmn"),
                black_box("Benchmark Workflow"),
                black_box("1.0.0")
            )
        });
    });
}

fn workflow_validation_benchmark(c: &mut Criterion) {
    c.bench_function("workflow validation", |b| {
        b.iter(|| {
            WorkflowManager::validate_file(
                black_box("bench/test.bpmn"),
                black_box(true)
            )
        });
    });
}

async fn process_start_helper(workflow_id: &str) {
    let variables = serde_json::json!({"test": true});
    let _ = ProcessExecutor::start(workflow_id, variables).await;
}

fn process_start_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("process start", |b| {
        b.to_async(&runtime).iter(|| async {
            process_start_helper(black_box("wf-bench-001")).await
        });
    });
}

async fn task_completion_helper(task_id: &str) {
    let variables = serde_json::json!({"result": "success"});
    let _ = TaskExecutor::complete(task_id, variables).await;
}

fn task_completion_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("task completion", |b| {
        b.to_async(&runtime).iter(|| async {
            task_completion_helper(black_box("task-bench-001")).await
        });
    });
}

fn concurrent_process_starts(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    let mut group = c.benchmark_group("concurrent_starts");
    for count in [10, 50, 100].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(count), count, |b, &count| {
            b.to_async(&runtime).iter(|| async move {
                let handles: Vec<_> = (0..count)
                    .map(|i| {
                        tokio::spawn(async move {
                            let variables = serde_json::json!({"test": true});
                            ProcessExecutor::start(&format!("wf-{}", i), variables).await
                        })
                    })
                    .collect();

                for handle in handles {
                    let _ = handle.await;
                }
            });
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    workflow_creation_benchmark,
    workflow_validation_benchmark,
    process_start_benchmark,
    task_completion_benchmark,
    concurrent_process_starts
);
criterion_main!(benches);
