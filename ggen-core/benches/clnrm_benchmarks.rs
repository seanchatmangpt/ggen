//! Comprehensive Performance Benchmarks for ggen using Cleanroom (clnrm)
//!
//! This benchmark suite provides:
//! - Deterministic test environments using cleanroom
//! - Marketplace operation benchmarks
//! - Lifecycle phase benchmarks
//! - Stress test scenarios
//! - Performance regression detection
//! - Baseline metrics for production readiness

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::hint::black_box;
use ggen_core::lifecycle::{
    cache_key, load_state, run_phase, run_pipeline, save_state, Context, LifecycleState, Make, Phase, Project,
};
use ggen_core::registry::{PackMetadata, RegistryIndex, VersionMetadata};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use tempfile::TempDir;

// ============================================================================
// CLEANROOM ENVIRONMENT UTILITIES
// ============================================================================

// Note: Cleanroom integration is demonstrated in tests/integration/performance_benchmarks.rs
// These benchmarks focus on pure performance without the cleanroom environment overhead

// ============================================================================
// BENCHMARK 1: MARKETPLACE OPERATIONS
// ============================================================================

/// Create a mock registry with deterministic data
fn create_mock_registry_deterministic(pack_count: usize, seed: u64) -> RegistryIndex {
    let mut packs = HashMap::new();

    for i in 0..pack_count {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();

        for j in 0..5 {
            let version = format!("{}.0.0", j);
            versions.insert(
                version.clone(),
                VersionMetadata {
                    version: version.clone(),
                    git_url: format!("https://github.com/test/{}.git", id),
                    git_rev: format!("v{}", version),
                    manifest_url: None,
                    sha256: format!("hash{}-{}", seed, j),
                },
            );
        }

        packs.insert(
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: format!("Description for package {}", i),
                tags: vec![format!("tag{}", i % 10), "rust".to_string()],
                keywords: vec![format!("keyword{}", i % 20), "cli".to_string()],
                category: Some("development".to_string()),
                author: Some("Test Author".to_string()),
                latest_version: "4.0.0".to_string(),
                versions,
                downloads: Some((i as u64) * 100 + seed),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            },
        );
    }

    RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    }
}

fn bench_marketplace_search_deterministic(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_search_deterministic");

    for size in [100, 1000, 10000].iter() {
        let index = create_mock_registry_deterministic(*size, 42);

        group.bench_with_input(
            BenchmarkId::new("simple_query", size),
            size,
            |b, _| {
                b.iter(|| {
                    let query = black_box("rust");
                    let query_lower = query.to_lowercase();

                    let results: Vec<_> = index
                        .packs
                        .iter()
                        .filter(|(_, pack)| {
                            pack.name.to_lowercase().contains(&query_lower)
                                || pack.description.to_lowercase().contains(&query_lower)
                                || pack.tags.iter().any(|t| t.to_lowercase().contains(&query_lower))
                        })
                        .collect();

                    black_box(results.len())
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("complex_query_with_filters", size),
            size,
            |b, _| {
                b.iter(|| {
                    let query = black_box("rust");
                    let category = black_box(Some("development"));
                    let keyword = black_box(Some("cli"));

                    let query_lower = query.to_lowercase();
                    let results: Vec<_> = index
                        .packs
                        .iter()
                        .filter(|(_, pack)| {
                            // Category filter
                            if let Some(cat) = category {
                                if pack.category.as_ref().map_or(true, |c| c != cat) {
                                    return false;
                                }
                            }

                            // Keyword filter
                            if let Some(kw) = keyword {
                                if !pack.keywords.iter().any(|k| k.contains(kw)) {
                                    return false;
                                }
                            }

                            // Query filter
                            pack.name.to_lowercase().contains(&query_lower)
                                || pack.description.to_lowercase().contains(&query_lower)
                                || pack.tags.iter().any(|t| t.to_lowercase().contains(&query_lower))
                        })
                        .collect();

                    black_box(results.len())
                });
            },
        );
    }

    group.finish();
}

fn bench_marketplace_version_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_version_resolution");
    let index = create_mock_registry_deterministic(1000, 42);

    group.bench_function("resolve_latest_version", |b| {
        b.iter(|| {
            let package_id = black_box("package-500");
            if let Some(pack) = index.packs.get(package_id) {
                let latest = &pack.latest_version;
                pack.versions.get(latest)
            } else {
                None
            }
        });
    });

    group.bench_function("resolve_specific_version", |b| {
        b.iter(|| {
            let package_id = black_box("package-500");
            let version = black_box("2.0.0");
            if let Some(pack) = index.packs.get(package_id) {
                pack.versions.get(version)
            } else {
                None
            }
        });
    });

    group.bench_function("resolve_semver_compatible", |b| {
        b.iter(|| {
            let package_id = black_box("package-500");
            let required = semver::VersionReq::parse("^2.0.0").unwrap();

            if let Some(pack) = index.packs.get(package_id) {
                let mut compatible: Vec<_> = pack
                    .versions
                    .iter()
                    .filter_map(|(ver_str, meta)| {
                        semver::Version::parse(ver_str).ok().and_then(|v| {
                            if required.matches(&v) {
                                Some((v, meta.clone()))
                            } else {
                                None
                            }
                        })
                    })
                    .collect();

                compatible.sort_by(|a, b| b.0.cmp(&a.0));
                compatible.first().cloned()
            } else {
                None
            }
        });
    });

    group.finish();
}

fn bench_marketplace_cache_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_cache_operations");

    group.bench_function("cache_index_serialization", |b| {
        let index = create_mock_registry_deterministic(1000, 42);
        b.iter(|| {
            black_box(serde_json::to_string(&index).unwrap())
        });
    });

    group.bench_function("cache_index_deserialization", |b| {
        let index = create_mock_registry_deterministic(1000, 42);
        let json = serde_json::to_string(&index).unwrap();
        b.iter(|| {
            black_box(serde_json::from_str::<RegistryIndex>(&json).unwrap())
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 2: LIFECYCLE PHASE OPERATIONS
// ============================================================================

fn create_test_lifecycle_make(phases: usize, has_hooks: bool) -> Make {
    let mut lifecycle = BTreeMap::new();

    for i in 0..phases {
        lifecycle.insert(
            format!("phase_{}", i),
            Phase {
                description: Some(format!("Phase {} description", i)),
                command: Some(format!("echo 'Phase {}'", i)),
                commands: None,
                watch: None,
                port: None,
                outputs: None,
                cache: Some(true),
                workspaces: None,
                parallel: None,
            },
        );
    }

    let hooks = if has_hooks {
        Some(ggen_core::lifecycle::Hooks {
            before_all: Some(vec!["phase_0".to_string()]),
            after_all: Some(vec!["phase_1".to_string()]),
            before_init: None,
            after_init: None,
            before_setup: None,
            after_setup: None,
            before_build: None,
            after_build: None,
            before_test: None,
            after_test: None,
            before_deploy: None,
            after_deploy: None,
        })
    } else {
        None
    };

    Make {
        project: Project {
            name: "benchmark_project".to_string(),
            project_type: Some("rust".to_string()),
            version: Some("1.0.0".to_string()),
            description: Some("Performance benchmark project".to_string()),
        },
        workspace: None,
        lifecycle,
        hooks,
    }
}

fn bench_lifecycle_phase_execution_deterministic(c: &mut Criterion) {
    let mut group = c.benchmark_group("lifecycle_phase_execution_deterministic");
    group.sample_size(10);

    for phase_count in [5, 10, 20].iter() {
        group.bench_with_input(
            BenchmarkId::new("sequential_phases", phase_count),
            phase_count,
            |b, &count| {
                let temp = TempDir::new().unwrap();
                let make = create_test_lifecycle_make(count, false);
                let ctx = Context::new(
                    temp.path().to_path_buf(),
                    Arc::new(make),
                    temp.path().join(".ggen/state.json"),
                    vec![],
                );

                b.iter(|| {
                    let phases: Vec<String> = (0..count).map(|i| format!("phase_{}", i)).collect();
                    run_pipeline(&ctx, &phases).unwrap();
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("phases_with_hooks", phase_count),
            phase_count,
            |b, &count| {
                let temp = TempDir::new().unwrap();
                let make = create_test_lifecycle_make(count, true);
                let ctx = Context::new(
                    temp.path().to_path_buf(),
                    Arc::new(make),
                    temp.path().join(".ggen/state.json"),
                    vec![],
                );

                b.iter(|| {
                    let phases: Vec<String> = (0..count).map(|i| format!("phase_{}", i)).collect();
                    run_pipeline(&ctx, &phases).unwrap();
                });
            },
        );
    }

    group.finish();
}

fn bench_lifecycle_cache_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("lifecycle_cache_validation");

    let temp = TempDir::new().unwrap();
    let cache_dir = temp.path().join(".ggen/cache");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create cache entries
    for i in 0..100 {
        let phase_dir = cache_dir.join(format!("phase_{}", i));
        std::fs::create_dir_all(&phase_dir).unwrap();
        std::fs::write(phase_dir.join(format!("key_{}", i)), "").unwrap();
    }

    group.bench_function("validate_100_cache_entries", |b| {
        b.iter(|| {
            for i in 0..100 {
                let phase = format!("phase_{}", i);
                let key = format!("key_{}", i);
                black_box(ggen_core::lifecycle::cache::is_cache_valid(
                    &cache_dir,
                    &phase,
                    &key,
                ));
            }
        });
    });

    group.finish();
}

fn bench_lifecycle_state_persistence(c: &mut Criterion) {
    let mut group = c.benchmark_group("lifecycle_state_persistence");

    let temp = TempDir::new().unwrap();
    let state_path = temp.path().join(".ggen/state.json");

    for record_count in [100, 500, 1000].iter() {
        let mut state = LifecycleState::default();

        for i in 0..*record_count {
            state.record_run(
                format!("phase_{}", i % 10),
                1000000 + i as u128,
                50 + (i % 100) as u128,
                true,
            );
            state.add_cache_key(format!("phase_{}", i % 10), format!("key_{:x}", i));
        }

        group.bench_with_input(
            BenchmarkId::new("save_state", record_count),
            record_count,
            |b, _| {
                b.iter(|| {
                    save_state(&state_path, &state).unwrap();
                });
            },
        );

        save_state(&state_path, &state).unwrap();

        group.bench_with_input(
            BenchmarkId::new("load_state", record_count),
            record_count,
            |b, _| {
                b.iter(|| {
                    black_box(load_state(&state_path).unwrap());
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 3: STRESS TESTS
// ============================================================================

fn bench_stress_concurrent_searches(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress_concurrent_searches");
    group.sample_size(10);

    let index = create_mock_registry_deterministic(10000, 42);

    group.bench_function("100_concurrent_searches", |b| {
        use rayon::prelude::*;

        b.iter(|| {
            let queries: Vec<&str> = vec![
                "rust", "web", "cli", "database", "async", "http", "json", "xml", "parser", "api",
            ];

            let _results: Vec<_> = queries
                .par_iter()
                .flat_map(|query| {
                    let query_lower = query.to_lowercase();
                    index
                        .packs
                        .iter()
                        .filter(|(_, pack)| {
                            pack.name.to_lowercase().contains(&query_lower)
                                || pack.description.to_lowercase().contains(&query_lower)
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
        });
    });

    group.finish();
}

fn bench_stress_high_volume_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress_high_volume_cache");
    group.throughput(Throughput::Elements(10000));

    let commands: Vec<String> = (0..10000)
        .map(|i| format!("command_{}", i))
        .collect();

    group.bench_function("generate_10000_cache_keys", |b| {
        b.iter(|| {
            let keys: Vec<String> = commands
                .iter()
                .map(|cmd| cache_key("test", &[cmd.clone()], &[], &[]))
                .collect();
            black_box(keys.len())
        });
    });

    group.finish();
}

fn bench_stress_large_registry_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress_large_registry");
    group.sample_size(10);

    let index = create_mock_registry_deterministic(50000, 42);

    group.bench_function("serialize_50k_packages", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&index).unwrap())
        });
    });

    let json = serde_json::to_string(&index).unwrap();

    group.bench_function("deserialize_50k_packages", |b| {
        b.iter(|| {
            black_box(serde_json::from_str::<RegistryIndex>(&json).unwrap())
        });
    });

    group.bench_function("search_50k_packages", |b| {
        b.iter(|| {
            let query = black_box("rust");
            let query_lower = query.to_lowercase();

            let results: Vec<_> = index
                .packs
                .iter()
                .filter(|(_, pack)| {
                    pack.name.to_lowercase().contains(&query_lower)
                        || pack.tags.iter().any(|t| t.to_lowercase().contains(&query_lower))
                })
                .collect();

            black_box(results.len())
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 4: PERFORMANCE REGRESSION DETECTION
// ============================================================================

fn bench_baseline_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("baseline_operations");

    // Baseline: Registry index creation
    group.bench_function("baseline_create_1000_packages", |b| {
        b.iter(|| {
            black_box(create_mock_registry_deterministic(1000, 42))
        });
    });

    // Baseline: Simple search
    let index = create_mock_registry_deterministic(1000, 42);
    group.bench_function("baseline_search_1000_packages", |b| {
        b.iter(|| {
            let query = black_box("rust");
            let query_lower = query.to_lowercase();
            let results: Vec<_> = index
                .packs
                .iter()
                .filter(|(_, pack)| pack.name.to_lowercase().contains(&query_lower))
                .collect();
            black_box(results.len())
        });
    });

    // Baseline: Cache key generation
    group.bench_function("baseline_cache_key_generation", |b| {
        let commands = vec!["cargo build".to_string(), "cargo test".to_string()];
        b.iter(|| {
            black_box(cache_key("build", &commands, &[], &[]))
        });
    });

    // Baseline: Lifecycle phase execution
    group.bench_function("baseline_lifecycle_phase", |b| {
        let temp = TempDir::new().unwrap();
        let make = create_test_lifecycle_make(1, false);
        let ctx = Context::new(
            temp.path().to_path_buf(),
            Arc::new(make),
            temp.path().join(".ggen/state.json"),
            vec![],
        );

        b.iter(|| {
            run_phase(&ctx, "phase_0").unwrap();
        });
    });

    group.finish();
}

// Note: Cleanroom determinism benchmarks are in tests/integration/performance_benchmarks.rs
// to avoid benchmark overhead. See that file for deterministic testing examples.

// ============================================================================
// BENCHMARK GROUPS
// ============================================================================

criterion_group!(
    marketplace_benches,
    bench_marketplace_search_deterministic,
    bench_marketplace_version_resolution,
    bench_marketplace_cache_operations,
);

criterion_group!(
    lifecycle_benches,
    bench_lifecycle_phase_execution_deterministic,
    bench_lifecycle_cache_validation,
    bench_lifecycle_state_persistence,
);

criterion_group!(
    stress_benches,
    bench_stress_concurrent_searches,
    bench_stress_high_volume_cache,
    bench_stress_large_registry_operations,
);

criterion_group!(
    regression_benches,
    bench_baseline_operations,
);

criterion_main!(
    marketplace_benches,
    lifecycle_benches,
    stress_benches,
    regression_benches,
);
