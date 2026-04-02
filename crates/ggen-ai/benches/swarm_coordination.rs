//! Swarm Coordination Performance Benchmarks
//!
//! Benchmarks the AI swarm coordinator focusing on:
//! 1. Pipeline execution latency
//! 2. Stage dependency resolution
//! 3. Agent timeout handling
//! 4. Failure recovery overhead
//! 5. Parallel stage execution

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_ai::swarm::coordinator::{
    ExecutionPipeline, PipelineStage, StageConfig, SwarmCoordinator,
};
use ggen_ai::swarm::{SwarmContext, SwarmInput, UltrathinkSwarm};
use std::collections::HashMap;
use std::time::Duration;
use tokio::runtime::Runtime;

/// Benchmark pipeline execution latency
fn bench_pipeline_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("pipeline_execution");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for num_stages in [3, 6, 10, 15].iter() {
        group.throughput(Throughput::Elements(*num_stages as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_stages),
            num_stages,
            |b, &num_stages| {
                b.to_async(&rt).iter(|| async {
                    let swarm = UltrathinkSwarm::development();
                    let input = create_test_input();

                    let start = std::time::Instant::now();
                    let _result = swarm.execute(input).await.unwrap();
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark stage dependency resolution
fn bench_dependency_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("dependency_resolution");
    group.measurement_time(Duration::from_secs(10));

    for depth in [2, 4, 6, 8].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(depth), depth, |b, &depth| {
            let pipeline = create_dependency_chain(depth);

            b.iter(|| {
                let start = std::time::Instant::now();

                // Simulate dependency checking
                for stage in &pipeline.stages {
                    for _dep in &stage.dependencies {
                        black_box(_dep);
                    }
                }

                black_box(start.elapsed())
            });
        });
    }

    group.finish();
}

/// Benchmark concurrent agent execution
fn bench_concurrent_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_execution");
    group.measurement_time(Duration::from_secs(15));

    let rt = Runtime::new().unwrap();

    for concurrency in [2, 5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*concurrency as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(concurrency),
            concurrency,
            |b, &concurrency| {
                b.to_async(&rt).iter(|| async {
                    let start = std::time::Instant::now();

                    let tasks: Vec<_> = (0..concurrency)
                        .map(|_| {
                            tokio::spawn(async {
                                let swarm = UltrathinkSwarm::development();
                                let input = create_test_input();
                                swarm.execute(input).await
                            })
                        })
                        .collect();

                    for task in tasks {
                        let _result = task.await;
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark coordinator semaphore overhead
fn bench_semaphore_coordination(c: &mut Criterion) {
    let mut group = c.benchmark_group("semaphore_coordination");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for permits in [5, 10, 20, 50].iter() {
        group.throughput(Throughput::Elements(*permits as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(permits),
            permits,
            |b, &permits| {
                b.to_async(&rt).iter(|| async {
                    use std::sync::Arc;
                    use tokio::sync::Semaphore;

                    let semaphore = Arc::new(Semaphore::new(permits));
                    let start = std::time::Instant::now();

                    let tasks: Vec<_> = (0..permits)
                        .map(|_| {
                            let sem = Arc::clone(&semaphore);
                            tokio::spawn(async move {
                                let _permit = sem.acquire().await.unwrap();
                                tokio::time::sleep(Duration::from_micros(100)).await;
                            })
                        })
                        .collect();

                    for task in tasks {
                        task.await.unwrap();
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark timeout handling overhead
fn bench_timeout_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("timeout_overhead");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for timeout_ms in [10, 50, 100, 500].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(timeout_ms),
            timeout_ms,
            |b, &timeout_ms| {
                b.to_async(&rt).iter(|| async {
                    use tokio::time::{timeout, Duration};

                    let start = std::time::Instant::now();

                    let result = timeout(Duration::from_millis(timeout_ms), async {
                        tokio::time::sleep(Duration::from_micros(100)).await;
                        42
                    })
                    .await;

                    black_box(result);
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark agent artifact conversion
fn bench_artifact_conversion(c: &mut Criterion) {
    let mut group = c.benchmark_group("artifact_conversion");

    for num_artifacts in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*num_artifacts as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_artifacts),
            num_artifacts,
            |b, &num_artifacts| {
                let output_data = create_mock_agent_output(num_artifacts);

                b.iter(|| {
                    let start = std::time::Instant::now();

                    // Simulate artifact conversion
                    for _ in 0..num_artifacts {
                        black_box(serde_json::to_string(&output_data).unwrap());
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark memory allocation for large swarms
fn bench_swarm_memory_allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("swarm_memory");
    group.measurement_time(Duration::from_secs(10));

    for swarm_size in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Bytes(*swarm_size * 4096));

        group.bench_with_input(
            BenchmarkId::from_parameter(swarm_size),
            swarm_size,
            |b, &swarm_size| {
                b.iter(|| {
                    let start = std::time::Instant::now();

                    let _swarms: Vec<_> = (0..swarm_size)
                        .map(|_| UltrathinkSwarm::development())
                        .collect();

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

// Helper functions
fn create_test_input() -> SwarmInput {
    SwarmInput {
        query: "Test query".to_string(),
        context: HashMap::new(),
        parameters: HashMap::new(),
    }
}

fn create_dependency_chain(depth: usize) -> ExecutionPipeline {
    let mut stages = Vec::new();
    let mut dependencies = HashMap::new();

    for i in 0..depth {
        let stage_name = format!("stage-{}", i);
        let deps = if i > 0 {
            vec![format!("stage-{}", i - 1)]
        } else {
            vec![]
        };

        stages.push(PipelineStage {
            name: stage_name.clone(),
            agent: format!("agent-{}", i),
            priority: i as u32,
            dependencies: deps.clone(),
            config: StageConfig {
                enabled: true,
                timeout_seconds: 30,
                retry_attempts: 2,
                parameters: HashMap::new(),
            },
        });

        dependencies.insert(stage_name, deps);
    }

    ExecutionPipeline {
        stages,
        dependencies,
        stage_configs: HashMap::new(),
    }
}

fn create_mock_agent_output(size: usize) -> serde_json::Value {
    let items: Vec<_> = (0..size)
        .map(|i| {
            serde_json::json!({
                "id": i,
                "data": format!("artifact-{}", i),
                "type": "mock"
            })
        })
        .collect();

    serde_json::json!(items)
}

criterion_group!(
    benches,
    bench_pipeline_execution,
    bench_dependency_resolution,
    bench_concurrent_execution,
    bench_semaphore_coordination,
    bench_timeout_overhead,
    bench_artifact_conversion,
    bench_swarm_memory_allocation,
);

criterion_main!(benches);
