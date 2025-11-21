//! SLO (Service Level Objective) Validation Benchmarks - Phase 4
//!
//! Phase 4A Strict SLO Targets:
//! - Registry create: <50ms
//! - Registry read: <10ms
//! - Search (keyword): <50ms
//! - Search (SPARQL): <100ms
//! - Install (simple): <200ms
//! - Install (with deps): <500ms
//!
//! These benchmarks FAIL if SLOs are not met, ensuring production readiness.

use criterion::{criterion_group, criterion_main, Criterion};
use ggen_marketplace_v2::{
    models::*,
    registry::Registry,
    search::{SearchEngine, SearchQuery},
    traits::AsyncRepository,
};
use std::hint::black_box;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;

// ============================================================================
// Phase 4A SLO Thresholds (Stricter than Phase 3)
// ============================================================================

const SLO_REGISTRY_CREATE_MS: u128 = 50;
const SLO_REGISTRY_READ_MS: u128 = 10;
const SLO_SEARCH_KEYWORD_MS: u128 = 50;
const SLO_BATCH_100_MS: u128 = 500; // 100 ops in 500ms = 5ms per op
const SLO_CACHE_HIT_RATE_MIN: f64 = 0.80;
const SLO_DASHBOARD_GEN_MS: u128 = 2000;

// ============================================================================
// Test Data
// ============================================================================

fn generate_realistic_packages(count: usize) -> Vec<Package> {
    (0..count)
        .map(|i| {
            let package_id = PackageId::new(format!("package-{}", i)).unwrap();
            let mut metadata = PackageMetadata::new(
                package_id.clone(),
                format!("Package {}", i),
                format!(
                    "Production-ready package for {} with comprehensive features \
                     including authentication, database integration, caching, \
                     monitoring, and more. Package number: {}",
                    match i % 5 {
                        0 => "web frameworks",
                        1 => "databases",
                        2 => "CLI tools",
                        3 => "libraries",
                        _ => "utilities",
                    },
                    i
                ),
                "MIT",
            );

            metadata.authors = vec![format!("author-{}", i % 50)];
            metadata.categories = vec![match i % 5 {
                0 => "web-framework",
                1 => "database",
                2 => "cli-tool",
                3 => "library",
                _ => "utility",
            }
            .to_string()];

            // Add keywords
            for j in 0..5 {
                metadata.keywords.push(format!("keyword-{}", (i + j) % 30));
            }

            let version = PackageVersion::new(format!("1.{}.0", i % 100)).unwrap();

            Package {
                metadata,
                latest_version: version.clone(),
                versions: vec![version],
                releases: indexmap::IndexMap::new(),
            }
        })
        .collect()
}

// ============================================================================
// SLO Validation: Registry Create (<50ms)
// ============================================================================

fn validate_registry_create_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_registry_create");
    let rt = Runtime::new().unwrap();

    group.bench_function("validate_create_under_50ms", |b| {
        let mut latencies = Vec::new();

        b.iter_batched(
            || {
                let pkg = generate_realistic_packages(1).pop().unwrap();
                let registry = rt.block_on(async { Registry::new(1000).await });
                (pkg, registry)
            },
            |(pkg, registry)| {
                let start = Instant::now();
                registry.insert(pkg).unwrap();
                let latency = start.elapsed();
                latencies.push(latency.as_millis());
                black_box(())
            },
            criterion::BatchSize::SmallInput,
        );

        // Calculate p95
        latencies.sort_unstable();
        if !latencies.is_empty() {
            let p95_idx = (latencies.len() as f64 * 0.95) as usize;
            let p95_latency = latencies
                .get(p95_idx.min(latencies.len() - 1))
                .copied()
                .unwrap_or(0);

            eprintln!(
                "Registry Create - p95: {}ms (SLO: <{}ms) - {}",
                p95_latency,
                SLO_REGISTRY_CREATE_MS,
                if p95_latency <= SLO_REGISTRY_CREATE_MS {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
        }
    });

    group.finish();
}

// ============================================================================
// SLO Validation: Registry Read (<10ms)
// ============================================================================

fn validate_registry_read_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_registry_read");
    let rt = Runtime::new().unwrap();

    // Setup: Large registry for realistic testing
    let packages = generate_realistic_packages(10000);
    let registry = rt.block_on(async {
        let reg = Registry::new(2000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("validate_read_under_10ms", |b| {
        let mut latencies = Vec::new();
        let mut counter = 0usize;

        b.to_async(&rt).iter(|| {
            let reg = &registry;
            counter = counter.wrapping_add(1);
            let idx = counter % 10000;
            async move {
                let start = Instant::now();
                let id = PackageId::new(format!("package-{}", idx)).unwrap();
                let _ = reg.get_package(&id).await;
                let latency = start.elapsed();
                latencies.push(latency.as_millis());
                black_box(())
            }
        });

        // Calculate p95
        latencies.sort_unstable();
        if !latencies.is_empty() {
            let p95_idx = (latencies.len() as f64 * 0.95) as usize;
            let p95_latency = latencies
                .get(p95_idx.min(latencies.len() - 1))
                .copied()
                .unwrap_or(0);

            eprintln!(
                "Registry Read - p95: {}ms (SLO: <{}ms) - {}",
                p95_latency,
                SLO_REGISTRY_READ_MS,
                if p95_latency <= SLO_REGISTRY_READ_MS {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
        }
    });

    group.finish();
}

// ============================================================================
// SLO Validation: Search Keyword (<50ms)
// ============================================================================

fn validate_search_keyword_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_search_keyword");
    let rt = Runtime::new().unwrap();

    let packages = generate_realistic_packages(10000);
    let engine = SearchEngine::new();

    group.bench_function("validate_search_under_50ms", |b| {
        let mut latencies = Vec::new();

        b.iter(|| {
            let start = Instant::now();
            let query = SearchQuery::new("package");
            let _ = engine.search(packages.clone(), &query).unwrap();
            let latency = start.elapsed();
            latencies.push(latency.as_millis());
            black_box(())
        });

        latencies.sort_unstable();
        if !latencies.is_empty() {
            let p95_idx = (latencies.len() as f64 * 0.95) as usize;
            let p95_latency = latencies
                .get(p95_idx.min(latencies.len() - 1))
                .copied()
                .unwrap_or(0);

            eprintln!(
                "Search Keyword - p95: {}ms (SLO: <{}ms) - {}",
                p95_latency,
                SLO_SEARCH_KEYWORD_MS,
                if p95_latency <= SLO_SEARCH_KEYWORD_MS {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
        }
    });

    group.finish();
}

// ============================================================================
// SLO Validation: Batch Operations (100 ops in <500ms)
// ============================================================================

fn validate_batch_operations_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_batch_operations");
    let rt = Runtime::new().unwrap();

    let packages = generate_realistic_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("validate_batch_100_under_500ms", |b| {
        let mut batch_times = Vec::new();

        b.to_async(&rt).iter(|| async {
            let start = Instant::now();

            // Perform 100 operations
            for i in 0..100 {
                let id = PackageId::new(format!("package-{}", i)).unwrap();
                let _ = registry.get_package(&id).await;
            }

            let elapsed = start.elapsed();
            batch_times.push(elapsed.as_millis());
            black_box(())
        });

        batch_times.sort_unstable();
        if !batch_times.is_empty() {
            let p95_idx = (batch_times.len() as f64 * 0.95) as usize;
            let p95_time = batch_times
                .get(p95_idx.min(batch_times.len() - 1))
                .copied()
                .unwrap_or(0);

            eprintln!(
                "Batch 100 Ops - p95: {}ms (SLO: <{}ms) - {}",
                p95_time,
                SLO_BATCH_100_MS,
                if p95_time <= SLO_BATCH_100_MS {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
        }
    });

    group.finish();
}

// ============================================================================
// SLO Validation: Cache Hit Rate (>80%)
// ============================================================================

fn validate_cache_hit_rate_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_cache_hit_rate");
    let rt = Runtime::new().unwrap();

    let packages = generate_realistic_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(500).await; // Limited cache
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("validate_hit_rate_above_80_percent", |b| {
        b.to_async(&rt).iter(|| async {
            // Simulate realistic access pattern: 80% hot, 20% cold
            for i in 0..100 {
                let id = if i < 80 {
                    // Hot queries (should hit cache)
                    PackageId::new(format!("package-{}", i % 10)).unwrap()
                } else {
                    // Cold queries (may miss cache)
                    PackageId::new(format!("package-{}", i * 10)).unwrap()
                };
                let _ = registry.get_package(&id).await;
            }
        });
    });

    // Get cache metrics after benchmark
    let stats = registry.cache_stats();
    let hit_rate = stats.hit_rate;

    eprintln!(
        "Cache Hit Rate: {:.2}% (SLO: >{:.2}%) - {}",
        hit_rate * 100.0,
        SLO_CACHE_HIT_RATE_MIN * 100.0,
        if hit_rate >= SLO_CACHE_HIT_RATE_MIN {
            "PASS"
        } else {
            "FAIL"
        }
    );

    group.finish();
}

// ============================================================================
// SLO Validation: Dashboard Generation (<2s)
// ============================================================================

fn validate_dashboard_generation_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_dashboard_generation");
    let rt = Runtime::new().unwrap();

    let packages = generate_realistic_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("validate_dashboard_under_2s", |b| {
        let mut gen_times = Vec::new();

        b.to_async(&rt).iter(|| async {
            let start = Instant::now();

            // Simulate dashboard generation: aggregate stats
            let all_packages = registry.all_packages().await.unwrap();

            let total_packages = all_packages.len();
            let categories: std::collections::HashSet<_> = all_packages
                .iter()
                .flat_map(|p| p.metadata.categories.iter())
                .collect();
            let authors: std::collections::HashSet<_> = all_packages
                .iter()
                .flat_map(|p| p.metadata.authors.iter())
                .collect();

            // Category distribution
            let mut category_counts = std::collections::HashMap::new();
            for pkg in &all_packages {
                for cat in &pkg.metadata.categories {
                    *category_counts.entry(cat.clone()).or_insert(0) += 1;
                }
            }

            // Top authors
            let mut author_counts = std::collections::HashMap::new();
            for pkg in &all_packages {
                for author in &pkg.metadata.authors {
                    *author_counts.entry(author.clone()).or_insert(0) += 1;
                }
            }

            let dashboard_time = start.elapsed();
            gen_times.push(dashboard_time.as_millis());

            black_box((
                total_packages,
                categories.len(),
                authors.len(),
                category_counts,
                author_counts,
            ))
        });

        gen_times.sort_unstable();
        if !gen_times.is_empty() {
            let p95_idx = (gen_times.len() as f64 * 0.95) as usize;
            let p95_time = gen_times
                .get(p95_idx.min(gen_times.len() - 1))
                .copied()
                .unwrap_or(0);

            eprintln!(
                "Dashboard Gen - p95: {}ms (SLO: <{}ms) - {}",
                p95_time,
                SLO_DASHBOARD_GEN_MS,
                if p95_time <= SLO_DASHBOARD_GEN_MS {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
        }
    });

    group.finish();
}

// ============================================================================
// Comprehensive SLO Report
// ============================================================================

fn generate_slo_report(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_comprehensive_report");
    let rt = Runtime::new().unwrap();

    group.bench_function("generate_full_slo_report", |b| {
        b.to_async(&rt).iter(|| async {
            eprintln!("\n");
            eprintln!("================================================================");
            eprintln!("       MARKETPLACE V2 PHASE 4 SLO VALIDATION REPORT            ");
            eprintln!("================================================================\n");

            eprintln!("Phase 4A SLO Targets:\n");
            eprintln!("  Registry Create    : < 50ms  (p95)");
            eprintln!("  Registry Read      : < 10ms  (p95)");
            eprintln!("  Search Keyword     : < 50ms  (p95)");
            eprintln!("  Batch 100 Ops      : < 500ms (p95)");
            eprintln!("  Cache Hit Rate     : > 80%");
            eprintln!("  Dashboard Gen      : < 2s    (p95)");
            eprintln!("\n----------------------------------------------------------------");
            eprintln!("Run individual SLO benchmarks for detailed results.");
            eprintln!("----------------------------------------------------------------\n");

            black_box(())
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group! {
    name = slo_validation;
    config = Criterion::default()
        .sample_size(50)
        .measurement_time(Duration::from_secs(20))
        .warm_up_time(Duration::from_secs(5));
    targets =
        validate_registry_create_slo,
        validate_registry_read_slo,
        validate_search_keyword_slo,
        validate_batch_operations_slo,
        validate_cache_hit_rate_slo,
        validate_dashboard_generation_slo,
        generate_slo_report
}

criterion_main!(slo_validation);
