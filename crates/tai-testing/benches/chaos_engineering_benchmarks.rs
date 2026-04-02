use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tai_testing::ChaosExperiment;

fn bench_pod_kill_experiment(c: &mut Criterion) {
    c.bench_function("pod_kill_execution", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let exp = ChaosExperiment::pod_kill(
                    black_box("bench-cluster"),
                    black_box("test-service"),
                    black_box(2),
                );
                let _ = exp.execute().await;
            });
    });
}

fn bench_network_partition_experiment(c: &mut Criterion) {
    c.bench_function("network_partition_execution", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let exp = ChaosExperiment::network_partition(
                    black_box("bench-cluster"),
                    black_box("test-service"),
                );
                let _ = exp.execute().await;
            });
    });
}

fn bench_cascading_failure_experiment(c: &mut Criterion) {
    c.bench_function("cascading_failure_execution", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let exp = ChaosExperiment::cascading_failure(
                    black_box("bench-cluster"),
                    vec![black_box("svc1"), black_box("svc2"), black_box("svc3")],
                );
                let _ = exp.execute().await;
            });
    });
}

criterion_group!(
    benches,
    bench_pod_kill_experiment,
    bench_network_partition_experiment,
    bench_cascading_failure_experiment
);
criterion_main!(benches);
