//! Benchmark suite for container lifecycle operations
//!
//! This benchmark measures the performance of container registration,
//! unregistration, and lifecycle management operations.

use cleanroom::cleanroom::CleanroomEnvironment;
use cleanroom::config::CleanroomConfig;
use cleanroom::guards::{ContainerGuard, container_guard};
use cleanroom::ids::{ContainerId, container_id};
use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use std::sync::Arc;
use std::time::Duration;

fn bench_container_id_creation(c: &mut Criterion) {
    c.bench_function("container_id_creation", |b| {
        b.iter(|| black_box(ContainerId::new()))
    });
}

fn bench_container_id_convenience(c: &mut Criterion) {
    c.bench_function("container_id_convenience", |b| {
        b.iter(|| black_box(container_id()))
    });
}

fn bench_container_id_serialization(c: &mut Criterion) {
    let id = ContainerId::new();

    c.bench_function("container_id_serialization", |b| {
        b.iter(|| black_box(serde_json::to_string(&id).unwrap()))
    });
}

fn bench_container_id_deserialization(c: &mut Criterion) {
    let id = ContainerId::new();
    let serialized = serde_json::to_string(&id).unwrap();

    c.bench_function("container_id_deserialization", |b| {
        b.iter(|| black_box(serde_json::from_str::<ContainerId>(&serialized).unwrap()))
    });
}

fn bench_container_id_hash(c: &mut Criterion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let id = ContainerId::new();

    c.bench_function("container_id_hash", |b| {
        b.iter(|| {
            let mut hasher = DefaultHasher::new();
            id.hash(&mut hasher);
            black_box(hasher.finish())
        })
    });
}

fn bench_container_id_equality(c: &mut Criterion) {
    let id1 = ContainerId::new();
    let id2 = ContainerId::new();

    c.bench_function("container_id_equality", |b| {
        b.iter(|| black_box(id1 == id2))
    });
}

fn bench_container_id_display(c: &mut Criterion) {
    let id = ContainerId::new();

    c.bench_function("container_id_display", |b| {
        b.iter(|| black_box(format!("{}", id)))
    });
}

fn bench_container_id_conversion(c: &mut Criterion) {
    let id = ContainerId::new();

    c.bench_function("container_id_to_u64", |b| b.iter(|| black_box(id.value())));
}

fn bench_container_id_from_value(c: &mut Criterion) {
    c.bench_function("container_id_from_value", |b| {
        b.iter(|| black_box(ContainerId::from_value(42)))
    });
}

fn bench_container_id_registry_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("container_id_registry");

    group.bench_function("register", |b| {
        b.iter(|| {
            let mut registry = cleanroom::ids::IdRegistry::new();
            let id = ContainerId::new();
            black_box(registry.register_container(id))
        })
    });

    group.bench_function("unregister", |b| {
        b.iter(|| {
            let mut registry = cleanroom::ids::IdRegistry::new();
            let id = ContainerId::new();
            registry.register_container(id);
            black_box(registry.unregister_container(&id))
        })
    });

    group.bench_function("is_registered", |b| {
        b.iter(|| {
            let mut registry = cleanroom::ids::IdRegistry::new();
            let id = ContainerId::new();
            registry.register_container(id);
            black_box(registry.is_container_registered(&id))
        })
    });

    group.finish();
}

fn bench_container_guard_creation(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_guard_creation", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(ContainerGuard::new(environment_arc.clone(), id))
        })
    });
}

fn bench_container_guard_convenience(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_guard_convenience", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(container_guard(environment_arc.clone(), id))
        })
    });
}

fn bench_container_guard_cleanup_action(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_guard_cleanup_action", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(
                ContainerGuard::new(environment_arc.clone(), id).add_cleanup_action(|| {
                    // Simulate cleanup action
                }),
            )
        })
    });
}

fn bench_container_guard_drop(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_guard_drop", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            let guard = ContainerGuard::new(environment_arc.clone(), id).add_cleanup_action(|| {
                // Simulate cleanup action
            });
            drop(guard);
        })
    });
}

fn bench_container_health_guard(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_health_guard", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::ContainerHealthGuard::new(
                environment_arc.clone(),
                id,
            ))
        })
    });
}

fn bench_container_resource_guard(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_resource_guard", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::ContainerResourceGuard::new(
                environment_arc.clone(),
                id,
                80.0,
                1024 * 1024 * 1024,
                10 * 1024 * 1024 * 1024,
            ))
        })
    });
}

fn bench_container_lifecycle_guard(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_lifecycle_guard", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::ContainerLifecycleGuard::new(
                environment_arc.clone(),
                id,
                Duration::from_millis(100),
            ))
        })
    });
}

fn bench_container_id_generator(c: &mut Criterion) {
    c.bench_function("container_id_generator", |b| {
        b.iter(|| {
            let mut generator = cleanroom::ids::container::ContainerIdGenerator::new();
            black_box(generator.next())
        })
    });
}

fn bench_deterministic_container_id_generator(c: &mut Criterion) {
    c.bench_function("deterministic_container_id_generator", |b| {
        b.iter(|| {
            let mut generator = cleanroom::ids::container::ContainerIdGenerator::deterministic();
            black_box(generator.next())
        })
    });
}

fn bench_container_id_generator_reset(c: &mut Criterion) {
    c.bench_function("container_id_generator_reset", |b| {
        b.iter(|| {
            let mut generator = cleanroom::ids::container::ContainerIdGenerator::deterministic();
            generator.next();
            generator.next();
            generator.next();
            generator.reset();
            black_box(generator.counter())
        })
    });
}

fn bench_container_metadata_creation(c: &mut Criterion) {
    c.bench_function("container_metadata_creation", |b| {
        b.iter(|| {
            let id = ContainerId::new();
            black_box(cleanroom::ids::container::ContainerMetadata {
                id,
                name: "test-container".to_string(),
                image: "alpine:latest".to_string(),
                created_at: std::time::Instant::now(),
                last_health_check: None,
                resource_usage: None,
                status: cleanroom::guards::container::ContainerStatus::Starting,
            })
        })
    });
}

fn bench_container_resource_usage(c: &mut Criterion) {
    c.bench_function("container_resource_usage", |b| {
        b.iter(|| {
            black_box(cleanroom::guards::container::ContainerResources {
                cpu_usage_percent: 25.0,
                memory_usage_bytes: 512 * 1024 * 1024,
                disk_usage_bytes: 2 * 1024 * 1024 * 1024,
                network_bytes_sent: 1000,
                network_bytes_received: 2000,
            })
        })
    });
}

fn bench_container_health_status(c: &mut Criterion) {
    let status = cleanroom::guards::container::ContainerHealth::Healthy;

    c.bench_function("container_health_status", |b| {
        b.iter(|| {
            black_box(matches!(
                status,
                cleanroom::guards::container::ContainerHealth::Healthy
            ))
        })
    });
}

fn bench_container_registry_statistics(c: &mut Criterion) {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).unwrap();
    let environment_arc = Arc::new(environment);

    c.bench_function("container_registry_statistics", |b| {
        b.iter(|| {
            let registry = cleanroom::ids::container::ContainerIdRegistry::new();
            black_box(registry.total_count())
        })
    });
}

fn bench_container_convenience_functions(c: &mut Criterion) {
    let mut group = c.benchmark_group("container_convenience");

    group.bench_function("container_health_guard", |b| {
        b.iter(|| {
            let config = CleanroomConfig::default();
            let environment = CleanroomEnvironment::new(config).unwrap();
            let environment_arc = Arc::new(environment);
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::container_health_guard(
                environment_arc,
                id,
            ))
        })
    });

    group.bench_function("container_resource_guard", |b| {
        b.iter(|| {
            let config = CleanroomConfig::default();
            let environment = CleanroomEnvironment::new(config).unwrap();
            let environment_arc = Arc::new(environment);
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::container_resource_guard(
                environment_arc,
                id,
                80.0,
                1024 * 1024 * 1024,
                10 * 1024 * 1024 * 1024,
            ))
        })
    });

    group.bench_function("container_lifecycle_guard", |b| {
        b.iter(|| {
            let config = CleanroomConfig::default();
            let environment = CleanroomEnvironment::new(config).unwrap();
            let environment_arc = Arc::new(environment);
            let id = ContainerId::new();
            black_box(cleanroom::guards::container::container_lifecycle_guard(
                environment_arc,
                id,
                Duration::from_millis(100),
            ))
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_container_id_creation,
    bench_container_id_convenience,
    bench_container_id_serialization,
    bench_container_id_deserialization,
    bench_container_id_hash,
    bench_container_id_equality,
    bench_container_id_display,
    bench_container_id_conversion,
    bench_container_id_from_value,
    bench_container_id_registry_operations,
    bench_container_guard_creation,
    bench_container_guard_convenience,
    bench_container_guard_cleanup_action,
    bench_container_guard_drop,
    bench_container_health_guard,
    bench_container_resource_guard,
    bench_container_lifecycle_guard,
    bench_container_id_generator,
    bench_deterministic_container_id_generator,
    bench_container_id_generator_reset,
    bench_container_metadata_creation,
    bench_container_resource_usage,
    bench_container_health_status,
    bench_container_registry_statistics,
    bench_container_convenience_functions
);

criterion_main!(benches);
