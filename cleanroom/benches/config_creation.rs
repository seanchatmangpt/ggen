//! Benchmark suite for configuration creation
//!
//! This benchmark measures the performance of creating CleanroomConfig
//! instances with various configurations and the new builder pattern.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use cleanroom::config::CleanroomConfig;
use cleanroom::builder::CleanroomBuilder;
use cleanroom::policy::SecurityPolicy;
use cleanroom::limits::ResourceLimits;
use std::time::Duration;

fn bench_config_creation_default(c: &mut Criterion) {
    c.bench_function("config_creation_default", |b| {
        b.iter(|| {
            black_box(CleanroomConfig::default())
        })
    });
}

fn bench_config_creation_builder(c: &mut Criterion) {
    c.bench_function("config_creation_builder", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new())
        })
    });
}

fn bench_config_creation_builder_with_timeout(c: &mut Criterion) {
    c.bench_function("config_creation_builder_with_timeout", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_timeout(Duration::from_secs(30)))
        })
    });
}

fn bench_config_creation_builder_with_security(c: &mut Criterion) {
    c.bench_function("config_creation_builder_with_security", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_security_policy(SecurityPolicy::locked()))
        })
    });
}

fn bench_config_creation_builder_with_resources(c: &mut Criterion) {
    c.bench_function("config_creation_builder_with_resources", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_resource_limits(ResourceLimits {
                    max_cpu_usage_percent: 80.0,
                    max_memory_usage_bytes: 1024 * 1024 * 1024,
                    max_disk_usage_bytes: 10 * 1024 * 1024 * 1024,
                    max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024,
                    max_container_count: 10,
                    max_test_execution_time: Duration::from_secs(300),
                    enable_resource_monitoring: true,
                    resource_cleanup_timeout: Duration::from_secs(30),
                }))
        })
    });
}

fn bench_config_creation_builder_complete(c: &mut Criterion) {
    c.bench_function("config_creation_builder_complete", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_timeout(Duration::from_secs(30))
                .with_security_policy(SecurityPolicy::locked())
                .with_resource_limits(ResourceLimits {
                    max_cpu_usage_percent: 80.0,
                    max_memory_usage_bytes: 1024 * 1024 * 1024,
                    max_disk_usage_bytes: 10 * 1024 * 1024 * 1024,
                    max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024,
                    max_container_count: 10,
                    max_test_execution_time: Duration::from_secs(300),
                    enable_resource_monitoring: true,
                    resource_cleanup_timeout: Duration::from_secs(30),
                })
                .with_deterministic_execution(Some(42))
                .with_coverage_tracking(true)
                .with_snapshot_testing(true)
                .with_tracing(true))
        })
    });
}

fn bench_config_creation_convenience_methods(c: &mut Criterion) {
    let mut group = c.benchmark_group("config_creation_convenience");
    
    group.bench_function("secure", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::secure())
        })
    });
    
    group.bench_function("performance", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::performance())
        })
    });
    
    group.bench_function("deterministic", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::deterministic(42))
        })
    });
    
    group.bench_function("development", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::development())
        })
    });
    
    group.finish();
}

fn bench_config_creation_serialization(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    
    c.bench_function("config_serialization", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&config).unwrap())
        })
    });
}

fn bench_config_creation_deserialization(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let serialized = serde_json::to_string(&config).unwrap();
    
    c.bench_function("config_deserialization", |b| {
        b.iter(|| {
            black_box(serde_json::from_str::<CleanroomConfig>(&serialized).unwrap())
        })
    });
}

fn bench_config_creation_clone(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    
    c.bench_function("config_clone", |b| {
        b.iter(|| {
            black_box(config.clone())
        })
    });
}

fn bench_config_creation_hash(c: &mut Criterion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let config = CleanroomConfig::default();
    
    c.bench_function("config_hash", |b| {
        b.iter(|| {
            let mut hasher = DefaultHasher::new();
            config.hash(&mut hasher);
            black_box(hasher.finish())
        })
    });
}

fn bench_config_creation_equality(c: &mut Criterion) {
    let config1 = CleanroomConfig::default();
    let config2 = CleanroomConfig::default();
    
    c.bench_function("config_equality", |b| {
        b.iter(|| {
            black_box(config1 == config2)
        })
    });
}

fn bench_config_creation_builder_state_transitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("builder_state_transitions");
    
    group.bench_function("initial_to_timeout", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_timeout(Duration::from_secs(30)))
        })
    });
    
    group.bench_function("timeout_to_security", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_timeout(Duration::from_secs(30))
                .with_security_policy(SecurityPolicy::locked()))
        })
    });
    
    group.bench_function("security_to_resources", |b| {
        b.iter(|| {
            black_box(CleanroomBuilder::new()
                .with_timeout(Duration::from_secs(30))
                .with_security_policy(SecurityPolicy::locked())
                .with_resource_limits(ResourceLimits {
                    max_cpu_usage_percent: 80.0,
                    max_memory_usage_bytes: 1024 * 1024 * 1024,
                    max_disk_usage_bytes: 10 * 1024 * 1024 * 1024,
                    max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024,
                    max_container_count: 10,
                    max_test_execution_time: Duration::from_secs(300),
                    enable_resource_monitoring: true,
                    resource_cleanup_timeout: Duration::from_secs(30),
                }))
        })
    });
    
    group.finish();
}

fn bench_config_creation_memory_usage(c: &mut Criterion) {
    c.bench_function("config_memory_usage", |b| {
        b.iter(|| {
            let configs: Vec<CleanroomConfig> = (0..1000)
                .map(|_| CleanroomConfig::default())
                .collect();
            black_box(configs.len())
        })
    });
}

fn bench_config_creation_builder_memory_usage(c: &mut Criterion) {
    c.bench_function("builder_memory_usage", |b| {
        b.iter(|| {
            let builders: Vec<_> = (0..1000)
                .map(|_| CleanroomBuilder::new())
                .collect();
            black_box(builders.len())
        })
    });
}

criterion_group!(
    benches,
    bench_config_creation_default,
    bench_config_creation_builder,
    bench_config_creation_builder_with_timeout,
    bench_config_creation_builder_with_security,
    bench_config_creation_builder_with_resources,
    bench_config_creation_builder_complete,
    bench_config_creation_convenience_methods,
    bench_config_creation_serialization,
    bench_config_creation_deserialization,
    bench_config_creation_clone,
    bench_config_creation_hash,
    bench_config_creation_equality,
    bench_config_creation_builder_state_transitions,
    bench_config_creation_memory_usage,
    bench_config_creation_builder_memory_usage
);

criterion_main!(benches);
