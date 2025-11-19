//! Hive Mind Coordination Performance Benchmarks
//!
//! Benchmarks the critical 20% operations that represent 80% of swarm usage:
//! 1. Consensus mechanism (HiveQueen orchestration)
//! 2. Agent spawning and coordination
//! 3. Conflict detection and resolution
//! 4. Configuration analysis
//! 5. Inter-agent communication

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::config::hive_coordinator::{AgentRole, HiveAgent, HiveQueen};
use ggen_core::config::ontology_config::{CompositionStrategy, OntologyConfig, OntologyPackRef};
use std::time::Duration;
use tokio::runtime::Runtime;

/// Benchmark consensus mechanism latency
fn bench_consensus_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("consensus_latency");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    // Test different configuration complexities
    for num_packs in [1, 3, 5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*num_packs as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_packs),
            num_packs,
            |b, &num_packs| {
                b.to_async(&rt).iter(|| async {
                    let config = create_test_config(num_packs);
                    let mut hive = HiveQueen::new(config).await.unwrap();

                    let start = std::time::Instant::now();
                    let _result = hive.orchestrate().await.unwrap();
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark agent spawning overhead
fn bench_agent_spawning(c: &mut Criterion) {
    let mut group = c.benchmark_group("agent_spawning");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for num_agents in [4, 8, 16, 32].iter() {
        group.throughput(Throughput::Elements(*num_agents as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_agents),
            num_agents,
            |b, &num_agents| {
                b.to_async(&rt).iter(|| async {
                    let config = create_test_config(*num_agents);
                    let start = std::time::Instant::now();
                    let _hive = HiveQueen::new(config).await.unwrap();
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark conflict detection performance
fn bench_conflict_detection(c: &mut Criterion) {
    let mut group = c.benchmark_group("conflict_detection");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for num_packs in [5, 10, 20, 50].iter() {
        group.throughput(Throughput::Elements(*num_packs as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_packs),
            num_packs,
            |b, &num_packs| {
                b.to_async(&rt).iter(|| async {
                    // Create config with potential conflicts
                    let config = create_conflicting_config(num_packs);
                    let mut hive = HiveQueen::new(config).await.unwrap();

                    let start = std::time::Instant::now();
                    let _result = hive.orchestrate().await.unwrap();
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark configuration analysis phase
fn bench_configuration_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("config_analysis");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for complexity in [1, 5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*complexity as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(complexity),
            complexity,
            |b, &complexity| {
                b.to_async(&rt).iter(|| async {
                    let agent = HiveAgent::new(
                        "analyzer-bench",
                        AgentRole::Analyzer,
                        vec!["versioning".to_string()],
                    );
                    let config = create_test_config(complexity);

                    let start = std::time::Instant::now();
                    let _analysis = agent.analyze_config(&config).await.unwrap();
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark parallel agent execution
fn bench_parallel_agents(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel_agents");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for num_concurrent in [2, 4, 8, 16].iter() {
        group.throughput(Throughput::Elements(*num_concurrent as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_concurrent),
            num_concurrent,
            |b, &num_concurrent| {
                b.to_async(&rt).iter(|| async {
                    let config = create_test_config(10);

                    let start = std::time::Instant::now();
                    let tasks: Vec<_> = (0..num_concurrent)
                        .map(|i| {
                            let cfg = config.clone();
                            tokio::spawn(async move {
                                let agent = HiveAgent::new(
                                    &format!("agent-{}", i),
                                    AgentRole::Analyzer,
                                    vec!["test".to_string()],
                                );
                                agent.analyze_config(&cfg).await
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

/// Benchmark memory allocation overhead
fn bench_memory_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_overhead");
    group.measurement_time(Duration::from_secs(10));

    let rt = Runtime::new().unwrap();

    for size in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Bytes(*size * 1024));

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                let start = std::time::Instant::now();

                // Simulate memory allocation for large swarm
                let mut configs = Vec::with_capacity(size);
                for i in 0..size {
                    configs.push(create_test_config(i % 20 + 1));
                }

                black_box(configs);
                black_box(start.elapsed())
            });
        });
    }

    group.finish();
}

/// Benchmark end-to-end orchestration throughput
fn bench_orchestration_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("orchestration_throughput");
    group.measurement_time(Duration::from_secs(15));

    let rt = Runtime::new().unwrap();

    for iterations in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*iterations as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(iterations),
            iterations,
            |b, &iterations| {
                b.to_async(&rt).iter(|| async {
                    let start = std::time::Instant::now();

                    for i in 0..iterations {
                        let config = create_test_config((i % 10) + 1);
                        let mut hive = HiveQueen::new(config).await.unwrap();
                        let _result = hive.orchestrate().await.unwrap();
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

// Helper functions
fn create_test_config(num_packs: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new().with_composition(CompositionStrategy::Union);

    for i in 0..num_packs {
        config = config.with_pack(OntologyPackRef {
            name: format!("pack-{}", i),
            version: "1.0.0".to_string(),
            namespace: Some(format!("ns-{}", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    config
}

fn create_conflicting_config(num_packs: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new().with_composition(CompositionStrategy::Priority);

    for i in 0..num_packs {
        // Create some conflicting versions
        let version = if i % 2 == 0 { "^1.0.0" } else { "~1.0.0" };

        config = config.with_pack(OntologyPackRef {
            name: format!("pack-{}", i / 2),
            version: version.to_string(),
            namespace: Some(format!("ns-{}", i % 3)), // Reuse namespaces to create conflicts
            classes: None,
            properties: None,
            source: None,
        });
    }

    config
}

criterion_group!(
    benches,
    bench_consensus_latency,
    bench_agent_spawning,
    bench_conflict_detection,
    bench_configuration_analysis,
    bench_parallel_agents,
    bench_memory_overhead,
    bench_orchestration_throughput,
);

criterion_main!(benches);
