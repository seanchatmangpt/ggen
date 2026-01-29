//! Erlang Cluster Benchmarks - Criterion Harness
//!
//! Comprehensive performance benchmarks for Erlang distributed clusters:
//! - Cluster formation time vs. node count (1-200 nodes)
//! - RPC throughput vs. cluster size
//! - Global registry operations vs. cluster size
//! - Network overhead vs. cluster size
//! - Memory consumption vs. cluster size
//!
//! Run with: `cargo make bench -- erlang_cluster`
//! HTML reports: `target/criterion/erlang_cluster_*/report/index.html`

use criterion::{
    black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput,
};
use ggen_core::benchmarks::erlang_cluster_benchmark::{
    benchmark_cluster_formation, benchmark_global_registry, benchmark_rpc_throughput,
    BenchmarkReporter, ErlangClusterManager, NetworkMode,
};
use std::path::PathBuf;

// ============================================================================
// BENCHMARK 1: Cluster Formation Time vs. Node Count
// ============================================================================

fn bench_cluster_formation(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_cluster_formation");
    group.sample_size(10); // Reduce for expensive cluster operations

    for &node_count in &[2, 5, 10, 25, 50] {
        group.throughput(Throughput::Elements(node_count));

        group.bench_with_input(
            BenchmarkId::from_parameter(node_count),
            &node_count,
            |b, &n| {
                b.iter(|| {
                    let cluster =
                        ErlangClusterManager::spawn_cluster(n, NetworkMode::DockerBridge)
                            .expect("Cluster spawn should succeed");

                    let connection_time = cluster
                        .connect_nodes()
                        .expect("Connection should succeed");

                    let metrics = cluster
                        .collect_metrics()
                        .expect("Metrics collection should succeed");

                    cluster.cleanup().expect("Cleanup should succeed");

                    black_box((connection_time, metrics))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 2: RPC Throughput vs. Cluster Size
// ============================================================================

fn bench_rpc_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_rpc_throughput");
    group.sample_size(10);

    let message_sizes = vec![
        ("1KB", 1024),
        ("10KB", 10 * 1024),
        ("100KB", 100 * 1024),
    ];

    for (size_name, message_size) in message_sizes {
        for &node_count in &[2, 5, 10, 25] {
            group.throughput(Throughput::Bytes(message_size as u64));

            group.bench_with_input(
                BenchmarkId::new(format!("{}_{}_nodes", size_name, node_count), node_count),
                &(node_count, message_size),
                |b, &(n, msg_size)| {
                    b.iter(|| {
                        let cluster =
                            ErlangClusterManager::spawn_cluster(n, NetworkMode::DockerBridge)
                                .expect("Cluster spawn should succeed");

                        cluster
                            .connect_nodes()
                            .expect("Connection should succeed");

                        // Execute 100 RPC calls
                        let message_payload = vec![0u8; msg_size];
                        let mut total_latency = std::time::Duration::ZERO;

                        for i in 0..100 {
                            let target_node = i % n;
                            let latency = cluster
                                .execute_rpc(
                                    target_node,
                                    "benchmark",
                                    "echo",
                                    &message_payload,
                                )
                                .expect("RPC should succeed");
                            total_latency += latency;
                        }

                        let metrics = cluster
                            .collect_metrics()
                            .expect("Metrics collection should succeed");

                        cluster.cleanup().expect("Cleanup should succeed");

                        black_box((total_latency, metrics))
                    });
                },
            );
        }
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 3: Global Registry Operations vs. Cluster Size
// ============================================================================

fn bench_global_registry_ops(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_global_registry");
    group.sample_size(10);

    for &node_count in &[2, 5, 10, 25, 50] {
        group.throughput(Throughput::Elements(node_count));

        group.bench_with_input(
            BenchmarkId::from_parameter(node_count),
            &node_count,
            |b, &n| {
                b.iter(|| {
                    let cluster =
                        ErlangClusterManager::spawn_cluster(n, NetworkMode::DockerBridge)
                            .expect("Cluster spawn should succeed");

                    cluster
                        .connect_nodes()
                        .expect("Connection should succeed");

                    let ops_per_sec = cluster
                        .measure_global_registry_ops(100)
                        .expect("Registry ops should succeed");

                    let metrics = cluster
                        .collect_metrics()
                        .expect("Metrics collection should succeed");

                    cluster.cleanup().expect("Cleanup should succeed");

                    black_box((ops_per_sec, metrics))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 4: Network Mode Comparison
// ============================================================================

fn bench_network_modes(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_network_modes");
    group.sample_size(10);

    let modes = vec![
        ("docker_bridge", NetworkMode::DockerBridge),
        ("host_network", NetworkMode::HostNetwork),
    ];

    for (mode_name, network_mode) in modes {
        for &node_count in &[5, 10, 25] {
            group.bench_with_input(
                BenchmarkId::new(mode_name, node_count),
                &(node_count, network_mode),
                |b, &(n, mode)| {
                    b.iter(|| {
                        let cluster = ErlangClusterManager::spawn_cluster(n, mode)
                            .expect("Cluster spawn should succeed");

                        let connection_time = cluster
                            .connect_nodes()
                            .expect("Connection should succeed");

                        // Execute sample RPC workload
                        let message = vec![0u8; 1024];
                        let mut total_rpc_time = std::time::Duration::ZERO;

                        for i in 0..50 {
                            let target = i % n;
                            let latency = cluster
                                .execute_rpc(target, "bench", "test", &message)
                                .expect("RPC should succeed");
                            total_rpc_time += latency;
                        }

                        let metrics = cluster
                            .collect_metrics()
                            .expect("Metrics collection should succeed");

                        cluster.cleanup().expect("Cleanup should succeed");

                        black_box((connection_time, total_rpc_time, metrics))
                    });
                },
            );
        }
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 5: Memory Consumption vs. Cluster Size
// ============================================================================

fn bench_memory_consumption(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_memory_consumption");
    group.sample_size(10);

    for &node_count in &[2, 10, 25, 50, 100] {
        group.throughput(Throughput::Elements(node_count));

        group.bench_with_input(
            BenchmarkId::from_parameter(node_count),
            &node_count,
            |b, &n| {
                b.iter(|| {
                    let cluster =
                        ErlangClusterManager::spawn_cluster(n, NetworkMode::DockerBridge)
                            .expect("Cluster spawn should succeed");

                    cluster
                        .connect_nodes()
                        .expect("Connection should succeed");

                    let metrics = cluster
                        .collect_metrics()
                        .expect("Metrics collection should succeed");

                    let memory_stats = (
                        metrics.total_memory_mb,
                        metrics.avg_memory_per_node_mb,
                        metrics.peak_memory_per_node_mb,
                    );

                    cluster.cleanup().expect("Cleanup should succeed");

                    black_box(memory_stats)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 6: End-to-End Scenario Benchmarks
// ============================================================================

fn bench_e2e_scenarios(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_e2e_scenarios");
    group.sample_size(10);

    // Scenario 1: Formation benchmark (comprehensive)
    group.bench_function("formation_comprehensive", |b| {
        b.iter(|| {
            let node_counts = vec![2, 10, 25, 50];
            let results = benchmark_cluster_formation(&node_counts)
                .expect("Formation benchmark should succeed");

            black_box(results)
        });
    });

    // Scenario 2: RPC throughput benchmark (comprehensive)
    group.bench_function("rpc_throughput_comprehensive", |b| {
        b.iter(|| {
            let node_counts = vec![2, 5, 10];
            let message_count = 100;
            let message_size = 1024;

            let results = benchmark_rpc_throughput(&node_counts, message_count, message_size)
                .expect("RPC benchmark should succeed");

            black_box(results)
        });
    });

    // Scenario 3: Global registry benchmark (comprehensive)
    group.bench_function("registry_comprehensive", |b| {
        b.iter(|| {
            let node_counts = vec![2, 5, 10];
            let operations = 50;

            let results = benchmark_global_registry(&node_counts, operations)
                .expect("Registry benchmark should succeed");

            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 7: Report Generation Performance
// ============================================================================

fn bench_report_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_report_generation");

    // Benchmark JSON report generation
    group.bench_function("json_report", |b| {
        let node_counts = vec![2, 5, 10];
        let metrics = benchmark_cluster_formation(&node_counts)
            .expect("Formation benchmark should succeed");

        let mut reporter = BenchmarkReporter::new();
        reporter.add_scenario("test_scenario".to_string(), metrics.clone());

        b.iter(|| {
            let json = reporter
                .generate_json()
                .expect("JSON generation should succeed");
            black_box(json)
        });
    });

    // Benchmark Markdown report generation
    group.bench_function("markdown_report", |b| {
        let node_counts = vec![2, 5, 10];
        let metrics = benchmark_cluster_formation(&node_counts)
            .expect("Formation benchmark should succeed");

        let mut reporter = BenchmarkReporter::new();
        reporter.add_scenario("test_scenario".to_string(), metrics.clone());

        b.iter(|| {
            let md = reporter
                .generate_markdown()
                .expect("Markdown generation should succeed");
            black_box(md)
        });
    });

    group.finish();
}

// ============================================================================
// CRITERION GROUPS
// ============================================================================

criterion_group!(
    cluster_formation_benches,
    bench_cluster_formation
);

criterion_group!(
    rpc_throughput_benches,
    bench_rpc_throughput
);

criterion_group!(
    global_registry_benches,
    bench_global_registry_ops
);

criterion_group!(
    network_mode_benches,
    bench_network_modes
);

criterion_group!(
    memory_consumption_benches,
    bench_memory_consumption
);

criterion_group!(
    e2e_scenario_benches,
    bench_e2e_scenarios
);

criterion_group!(
    report_generation_benches,
    bench_report_generation
);

criterion_main!(
    cluster_formation_benches,
    rpc_throughput_benches,
    global_registry_benches,
    network_mode_benches,
    memory_consumption_benches,
    e2e_scenario_benches,
    report_generation_benches
);
