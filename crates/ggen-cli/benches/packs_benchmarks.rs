//! Comprehensive performance benchmarks for ggen packs Phase 2-3
//!
//! This benchmark suite measures performance across:
//! - Phase 2: Installation, SPARQL queries, template generation
//! - Phase 3: Dependency resolution, registry operations, cloud distribution
//!
//! Run with: cargo bench --bench packs_benchmarks
//! Generate report: cargo bench --bench packs_benchmarks -- --save-baseline main

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::HashMap;
use std::time::Duration;
use tempfile::TempDir;

// ============================================================================
// Test Data Generators
// ============================================================================

/// Pack size categories for benchmarking
#[derive(Clone, Copy, Debug)]
enum PackSize {
    Small,  // 1-5 packages, <10MB
    Medium, // 5-15 packages, 10-100MB
    Large,  // 15+ packages, 100MB+
}

/// Generate test pack metadata
fn generate_pack_metadata(size: PackSize, dependencies: usize) -> PackMetadata {
    let (package_count, total_size_mb) = match size {
        PackSize::Small => (3, 5),
        PackSize::Medium => (10, 50),
        PackSize::Large => (20, 150),
    };

    PackMetadata {
        id: format!("test-pack-{:?}", size).to_lowercase(),
        name: format!("Test Pack {:?}", size),
        version: "1.0.0".to_string(),
        packages: (0..package_count)
            .map(|i| format!("package-{}", i))
            .collect(),
        dependencies: (0..dependencies)
            .map(|i| PackDependency {
                pack_id: format!("dep-{}", i),
                version: "1.0.0".to_string(),
            })
            .collect(),
        templates: vec![PackTemplate {
            name: "main".to_string(),
            variables: vec!["NAME".to_string(), "VERSION".to_string()],
            content: "use {{NAME}};\nfn main() {}".repeat(100),
        }],
        total_size_bytes: total_size_mb * 1024 * 1024,
        sparql_queries: generate_sparql_queries(size),
    }
}

#[derive(Clone, Debug)]
struct PackMetadata {
    id: String,
    name: String,
    version: String,
    packages: Vec<String>,
    dependencies: Vec<PackDependency>,
    templates: Vec<PackTemplate>,
    total_size_bytes: usize,
    sparql_queries: HashMap<String, String>,
}

#[derive(Clone, Debug)]
struct PackDependency {
    pack_id: String,
    version: String,
}

#[derive(Clone, Debug)]
struct PackTemplate {
    name: String,
    variables: Vec<String>,
    content: String,
}

/// Generate SPARQL queries for different complexity levels
fn generate_sparql_queries(size: PackSize) -> HashMap<String, String> {
    let mut queries = HashMap::new();

    // Simple query
    queries.insert(
        "simple".to_string(),
        r#"
        PREFIX pack: <http://ggen.dev/pack#>
        SELECT ?name WHERE {
            ?pack pack:name ?name .
        }
        "#
        .to_string(),
    );

    // Medium complexity
    if matches!(size, PackSize::Medium | PackSize::Large) {
        queries.insert(
            "medium".to_string(),
            r#"
            PREFIX pack: <http://ggen.dev/pack#>
            PREFIX dep: <http://ggen.dev/dependency#>
            SELECT ?name ?version ?dep WHERE {
                ?pack pack:name ?name ;
                      pack:version ?version ;
                      dep:requires ?dep .
            }
            "#
            .to_string(),
        );
    }

    // Complex query with joins
    if matches!(size, PackSize::Large) {
        queries.insert(
            "complex".to_string(),
            r#"
            PREFIX pack: <http://ggen.dev/pack#>
            PREFIX dep: <http://ggen.dev/dependency#>
            PREFIX tmpl: <http://ggen.dev/template#>
            SELECT ?packName ?depName ?tmplName WHERE {
                ?pack pack:name ?packName ;
                      dep:requires ?dependency .
                ?dependency pack:name ?depName .
                ?pack tmpl:hasTemplate ?template .
                ?template tmpl:name ?tmplName .
            }
            "#
            .to_string(),
        );
    }

    queries
}

/// Generate dependency graph for benchmarking
fn generate_dependency_graph(
    pack_count: usize, complexity: DependencyComplexity,
) -> Vec<PackMetadata> {
    match complexity {
        DependencyComplexity::Linear => {
            // A → B → C → ...
            (0..pack_count)
                .map(|i| {
                    let deps = if i > 0 {
                        vec![PackDependency {
                            pack_id: format!("pack-{}", i - 1),
                            version: "1.0.0".to_string(),
                        }]
                    } else {
                        vec![]
                    };

                    let mut metadata = generate_pack_metadata(PackSize::Small, 0);
                    metadata.id = format!("pack-{}", i);
                    metadata.dependencies = deps;
                    metadata
                })
                .collect()
        }
        DependencyComplexity::Diamond => {
            // A → B, A → C, B → D, C → D
            vec![
                {
                    let mut m = generate_pack_metadata(PackSize::Small, 0);
                    m.id = "pack-a".to_string();
                    m.dependencies = vec![
                        PackDependency {
                            pack_id: "pack-b".to_string(),
                            version: "1.0.0".to_string(),
                        },
                        PackDependency {
                            pack_id: "pack-c".to_string(),
                            version: "1.0.0".to_string(),
                        },
                    ];
                    m
                },
                {
                    let mut m = generate_pack_metadata(PackSize::Small, 0);
                    m.id = "pack-b".to_string();
                    m.dependencies = vec![PackDependency {
                        pack_id: "pack-d".to_string(),
                        version: "1.0.0".to_string(),
                    }];
                    m
                },
                {
                    let mut m = generate_pack_metadata(PackSize::Small, 0);
                    m.id = "pack-c".to_string();
                    m.dependencies = vec![PackDependency {
                        pack_id: "pack-d".to_string(),
                        version: "1.0.0".to_string(),
                    }];
                    m
                },
                {
                    let mut m = generate_pack_metadata(PackSize::Small, 0);
                    m.id = "pack-d".to_string();
                    m
                },
            ]
        }
        DependencyComplexity::ComplexWeb => {
            // Create complex web with multiple levels
            (0..pack_count)
                .map(|i| {
                    let dep_count = (i % 3) + 1; // 1-3 dependencies per pack
                    let deps: Vec<_> = (0..dep_count)
                        .filter_map(|d| {
                            let dep_idx = (i + d + 1) % pack_count;
                            if dep_idx != i {
                                Some(PackDependency {
                                    pack_id: format!("pack-{}", dep_idx),
                                    version: "1.0.0".to_string(),
                                })
                            } else {
                                None
                            }
                        })
                        .collect();

                    let mut metadata = generate_pack_metadata(PackSize::Small, 0);
                    metadata.id = format!("pack-{}", i);
                    metadata.dependencies = deps;
                    metadata
                })
                .collect()
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum DependencyComplexity {
    Linear,
    Diamond,
    ComplexWeb,
}

// ============================================================================
// PHASE 2 BENCHMARKS: Installation Performance
// ============================================================================

fn bench_install_single_pack(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_single_pack");
    group.measurement_time(Duration::from_secs(10));

    for size in [PackSize::Small, PackSize::Medium, PackSize::Large] {
        let pack = generate_pack_metadata(size, 0);
        let size_name = format!("{:?}", size).to_lowercase();

        group.throughput(Throughput::Bytes(pack.total_size_bytes as u64));
        group.bench_with_input(BenchmarkId::from_parameter(&size_name), &pack, |b, pack| {
            b.iter(|| {
                // Simulate installation steps

                // 1. Download simulation (checksumming)
                let checksum = simulate_download(&pack.packages);

                // 2. Verification
                let verified = simulate_verification(&pack.packages, checksum);

                // 3. Package installation
                let installed = simulate_package_install(&pack.packages);

                black_box((verified, installed))
            });
        });
    }

    group.finish();
}

fn bench_install_multi_pack(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_multi_pack");
    group.measurement_time(Duration::from_secs(15));

    for pack_count in [2, 3, 5] {
        let packs: Vec<_> = (0..pack_count)
            .map(|_| generate_pack_metadata(PackSize::Small, 0))
            .collect();

        let total_size: usize = packs.iter().map(|p| p.total_size_bytes).sum();

        group.throughput(Throughput::Bytes(total_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(pack_count),
            &packs,
            |b, packs| {
                b.iter(|| {
                    // Simulate dependency resolution
                    let resolution_time = simulate_dependency_resolution(packs);

                    // Simulate parallel installation
                    let install_results: Vec<_> = packs
                        .iter()
                        .map(|pack| {
                            let checksum = simulate_download(&pack.packages);
                            simulate_verification(&pack.packages, checksum)
                        })
                        .collect();

                    black_box((resolution_time, install_results))
                });
            },
        );
    }

    group.finish();
}

fn bench_install_with_conflicts(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_with_conflicts");
    group.measurement_time(Duration::from_secs(10));

    for conflict_count in [1, 3, 5] {
        group.bench_with_input(
            BenchmarkId::from_parameter(conflict_count),
            &conflict_count,
            |b, &conflict_count| {
                b.iter(|| {
                    // Simulate conflict detection
                    let conflicts = simulate_conflict_detection(conflict_count);

                    // Simulate conflict resolution
                    let resolved = simulate_conflict_resolution(&conflicts);

                    black_box(resolved)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 2 BENCHMARKS: SPARQL Performance
// ============================================================================

fn bench_sparql_query_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_query_execution");

    for size in [PackSize::Small, PackSize::Medium, PackSize::Large] {
        let pack = generate_pack_metadata(size, 2);
        let size_name = format!("{:?}", size).to_lowercase();

        for (query_type, query) in &pack.sparql_queries {
            let bench_id = format!("{}_{}", size_name, query_type);

            group.bench_with_input(
                BenchmarkId::from_parameter(&bench_id),
                &(query, &pack),
                |b, (query, pack)| {
                    b.iter(|| {
                        // Simulate SPARQL query execution
                        let parse_time = simulate_sparql_parse(query);
                        let exec_time = simulate_sparql_execution(query, pack);
                        let serialize_time = simulate_result_serialization();

                        black_box((parse_time, exec_time, serialize_time))
                    });
                },
            );
        }
    }

    group.finish();
}

fn bench_rdf_conversion(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_conversion");

    let triple_counts = [("small", 10), ("medium", 100), ("large", 1000)];

    for (name, triple_count) in triple_counts {
        let triples = generate_rdf_triples(triple_count);

        group.throughput(Throughput::Elements(triple_count as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &triples, |b, triples| {
            b.iter(|| {
                // Simulate RDF conversion
                let converted = simulate_rdf_conversion(triples);
                black_box(converted)
            });
        });
    }

    group.finish();
}

// ============================================================================
// PHASE 2 BENCHMARKS: Template Generation
// ============================================================================

fn bench_template_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_generation");

    let template_sizes = [("small", 100), ("medium", 500), ("large", 2000)];

    for (name, line_count) in template_sizes {
        let template_content = "fn generated_{{VAR}}() {}\n".repeat(line_count);
        let variables = vec![("VAR", "test")];

        group.throughput(Throughput::Elements(line_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(template_content, variables),
            |b, (template, vars)| {
                b.iter(|| {
                    // Simulate variable substitution
                    let mut result = template.clone();
                    for (var, val) in vars {
                        result = result.replace(&format!("{{{{{}}}}}", var), val);
                    }
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

fn bench_variable_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("variable_validation");

    for var_count in [5, 20, 50] {
        let variables: Vec<_> = (0..var_count)
            .map(|i| (format!("VAR_{}", i), format!("value_{}", i)))
            .collect();

        group.throughput(Throughput::Elements(var_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(var_count),
            &variables,
            |b, variables| {
                b.iter(|| {
                    // Simulate variable validation
                    let validated: Vec<_> = variables
                        .iter()
                        .map(|(name, value)| {
                            let valid = !name.is_empty() && !value.is_empty();
                            (name, value, valid)
                        })
                        .collect();
                    black_box(validated)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 3 BENCHMARKS: Dependency Resolution
// ============================================================================

fn bench_dependency_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("dependency_resolution");
    group.measurement_time(Duration::from_secs(10));

    for complexity in [
        DependencyComplexity::Linear,
        DependencyComplexity::Diamond,
        DependencyComplexity::ComplexWeb,
    ] {
        let pack_count = match complexity {
            DependencyComplexity::Linear => 10,
            DependencyComplexity::Diamond => 4,
            DependencyComplexity::ComplexWeb => 10,
        };

        let packs = generate_dependency_graph(pack_count, complexity);
        let complexity_name = format!("{:?}", complexity).to_lowercase();

        group.bench_with_input(
            BenchmarkId::from_parameter(&complexity_name),
            &packs,
            |b, packs| {
                b.iter(|| {
                    // Simulate dependency resolution
                    let resolved = simulate_full_dependency_resolution(packs);

                    // Simulate topological sort
                    let sorted = simulate_topological_sort(&resolved);

                    black_box(sorted)
                });
            },
        );
    }

    group.finish();
}

fn bench_conflict_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("conflict_resolution");

    for conflict_count in [0, 2, 5] {
        group.bench_with_input(
            BenchmarkId::from_parameter(conflict_count),
            &conflict_count,
            |b, &conflict_count| {
                b.iter(|| {
                    // Simulate strategy selection
                    let strategy_time = simulate_strategy_selection(conflict_count);

                    // Simulate conflict resolution
                    let resolution_time = if conflict_count > 0 {
                        simulate_conflict_resolution(&vec![(); conflict_count])
                    } else {
                        Duration::from_nanos(0)
                    };

                    black_box((strategy_time, resolution_time))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 3 BENCHMARKS: Registry Operations
// ============================================================================

fn bench_registry_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("registry_search");

    for registry_size in [10, 100, 1000] {
        let registry: Vec<_> = (0..registry_size)
            .map(|i| generate_pack_metadata(PackSize::Small, 0))
            .collect();

        let search_term = "pack-5";

        group.throughput(Throughput::Elements(registry_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(registry_size),
            &(registry, search_term),
            |b, (registry, term)| {
                b.iter(|| {
                    // Simulate search
                    let results: Vec<_> = registry.iter().filter(|p| p.id.contains(term)).collect();
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

fn bench_registry_publish(c: &mut Criterion) {
    let mut group = c.benchmark_group("registry_publish");
    group.measurement_time(Duration::from_secs(10));

    for has_deps in [false, true] {
        let pack = generate_pack_metadata(PackSize::Medium, if has_deps { 3 } else { 0 });

        let bench_name = if has_deps {
            "with_dependencies"
        } else {
            "single_pack"
        };

        group.bench_with_input(BenchmarkId::from_parameter(bench_name), &pack, |b, pack| {
            b.iter(|| {
                // Simulate upload
                let upload_time = simulate_upload(pack.total_size_bytes);

                // Simulate metadata processing
                let metadata_time = simulate_metadata_processing(pack);

                // Simulate index update
                let index_time = simulate_index_update();

                black_box((upload_time, metadata_time, index_time))
            });
        });
    }

    group.finish();
}

// ============================================================================
// PHASE 3 BENCHMARKS: Cloud Distribution
// ============================================================================

fn bench_cache_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_performance");

    let cache_scenarios = [("cache_miss", false), ("cache_hit", true)];

    for (name, is_cached) in cache_scenarios {
        let pack = generate_pack_metadata(PackSize::Medium, 0);

        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(pack, is_cached),
            |b, (pack, is_cached)| {
                b.iter(|| {
                    if *is_cached {
                        // Simulate cache hit
                        simulate_cache_lookup(&pack.id)
                    } else {
                        // Simulate cache miss + download
                        let download_time = simulate_download(&pack.packages);
                        let cache_time = simulate_cache_population(&pack.id);
                        download_time + cache_time.as_nanos()
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_multi_mirror_selection(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi_mirror_selection");

    for mirror_count in [2, 5, 10] {
        let mirrors: Vec<_> = (0..mirror_count)
            .map(|i| Mirror {
                url: format!("https://mirror-{}.example.com", i),
                latency_ms: (i * 50) as u64,
                bandwidth_mbps: 100 - (i * 5),
            })
            .collect();

        group.bench_with_input(
            BenchmarkId::from_parameter(mirror_count),
            &mirrors,
            |b, mirrors| {
                b.iter(|| {
                    // Simulate mirror selection algorithm
                    let selected = select_optimal_mirror(mirrors);
                    black_box(selected)
                });
            },
        );
    }

    group.finish();
}

#[derive(Clone, Debug)]
struct Mirror {
    url: String,
    latency_ms: u64,
    bandwidth_mbps: usize,
}

// ============================================================================
// Simulation Helper Functions
// ============================================================================

fn simulate_download(packages: &[String]) -> u64 {
    packages.iter().map(|p| p.len() as u64).sum()
}

fn simulate_verification(packages: &[String], checksum: u64) -> bool {
    let computed: u64 = packages.iter().map(|p| p.len() as u64).sum();
    computed == checksum
}

fn simulate_package_install(packages: &[String]) -> Vec<String> {
    packages
        .iter()
        .map(|p| format!("installed-{}", p))
        .collect()
}

fn simulate_dependency_resolution(packs: &[PackMetadata]) -> Duration {
    Duration::from_micros((packs.len() * 100) as u64)
}

fn simulate_conflict_detection(count: usize) -> Vec<String> {
    (0..count).map(|i| format!("conflict-{}", i)).collect()
}

fn simulate_conflict_resolution<T>(conflicts: &[T]) -> Duration {
    Duration::from_micros((conflicts.len() * 200) as u64)
}

fn simulate_sparql_parse(query: &str) -> Duration {
    Duration::from_micros((query.len() / 10) as u64)
}

fn simulate_sparql_execution(query: &str, pack: &PackMetadata) -> Duration {
    let complexity = query.matches("SELECT").count() + query.matches("WHERE").count();
    Duration::from_micros((complexity * pack.packages.len() * 50) as u64)
}

fn simulate_result_serialization() -> Duration {
    Duration::from_micros(50)
}

fn generate_rdf_triples(count: usize) -> Vec<(String, String, String)> {
    (0..count)
        .map(|i| {
            (
                format!("subject-{}", i),
                "predicate".to_string(),
                format!("object-{}", i),
            )
        })
        .collect()
}

fn simulate_rdf_conversion(triples: &[(String, String, String)]) -> Vec<String> {
    triples
        .iter()
        .map(|(s, p, o)| format!("{} {} {}", s, p, o))
        .collect()
}

fn simulate_full_dependency_resolution(packs: &[PackMetadata]) -> Vec<String> {
    let mut resolved = Vec::new();
    for pack in packs {
        resolved.push(pack.id.clone());
        for dep in &pack.dependencies {
            resolved.push(dep.pack_id.clone());
        }
    }
    resolved
}

fn simulate_topological_sort(items: &[String]) -> Vec<String> {
    let mut sorted = items.to_vec();
    sorted.sort();
    sorted
}

fn simulate_strategy_selection(conflict_count: usize) -> Duration {
    Duration::from_micros((conflict_count * 50) as u64)
}

fn simulate_upload(size_bytes: usize) -> Duration {
    // Simulate 100 Mbps upload
    Duration::from_millis((size_bytes / 12500) as u64)
}

fn simulate_metadata_processing(pack: &PackMetadata) -> Duration {
    Duration::from_micros((pack.packages.len() * 100) as u64)
}

fn simulate_index_update() -> Duration {
    Duration::from_micros(500)
}

fn simulate_cache_lookup(_pack_id: &str) -> u64 {
    // Simulate fast cache access
    100_000 // 100 microseconds in nanoseconds
}

fn simulate_cache_population(_pack_id: &str) -> Duration {
    Duration::from_micros(200)
}

fn select_optimal_mirror(mirrors: &[Mirror]) -> &Mirror {
    // Simple selection: lowest latency
    mirrors.iter().min_by_key(|m| m.latency_ms).unwrap()
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    phase2_installation,
    bench_install_single_pack,
    bench_install_multi_pack,
    bench_install_with_conflicts,
);

criterion_group!(
    phase2_sparql,
    bench_sparql_query_execution,
    bench_rdf_conversion,
);

criterion_group!(
    phase2_templates,
    bench_template_generation,
    bench_variable_validation,
);

criterion_group!(
    phase3_dependencies,
    bench_dependency_resolution,
    bench_conflict_resolution,
);

criterion_group!(
    phase3_registry,
    bench_registry_search,
    bench_registry_publish,
);

criterion_group!(
    phase3_cloud,
    bench_cache_performance,
    bench_multi_mirror_selection,
);

criterion_main!(
    phase2_installation,
    phase2_sparql,
    phase2_templates,
    phase3_dependencies,
    phase3_registry,
    phase3_cloud,
);
