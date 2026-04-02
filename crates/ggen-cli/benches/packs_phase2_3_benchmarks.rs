//! Standalone performance benchmarks for ggen packs Phase 2-3
//!
//! This benchmark suite is designed to run independently without requiring
//! full ggen-domain compilation. It focuses on measuring performance characteristics
//! that are critical for production deployment.
//!
//! Run with: cargo bench --bench packs_phase2_3_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::HashMap;
use std::time::Duration;

// ============================================================================
// PHASE 2: Installation Performance Benchmarks
// ============================================================================

/// Simulate package download with checksum calculation
fn bench_package_download_simulation(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_package_download");
    group.measurement_time(Duration::from_secs(5));

    let scenarios = vec![
        ("small_1mb", 1 * 1024 * 1024),
        ("medium_10mb", 10 * 1024 * 1024),
        ("large_50mb", 50 * 1024 * 1024),
    ];

    for (name, size_bytes) in scenarios {
        // Generate fake package data
        let package_data = vec![0u8; size_bytes];

        group.throughput(Throughput::Bytes(size_bytes as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &package_data,
            |b, data| {
                b.iter(|| {
                    // Simulate download verification with SHA-256
                    use sha2::{Digest, Sha256};
                    let mut hasher = Sha256::new();
                    hasher.update(data);
                    let checksum = hasher.finalize();
                    black_box(checksum)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark parallel package installation
fn bench_parallel_installation(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_parallel_install");
    group.measurement_time(Duration::from_secs(10));

    let rt = tokio::runtime::Runtime::new().unwrap();

    for package_count in [2, 5, 10] {
        let packages: Vec<_> = (0..package_count)
            .map(|i| (format!("package-{}", i), 1024 * 1024)) // 1MB each
            .collect();

        group.throughput(Throughput::Elements(package_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            &packages,
            |b, packages| {
                b.to_async(&rt).iter(|| {
                    let packages_clone = packages.clone();
                    async move {
                        let mut tasks = Vec::new();

                        for (name, size) in packages_clone {
                            let task = tokio::spawn(async move {
                                // Simulate package installation work
                                let data = vec![0u8; size];
                                use sha2::{Digest, Sha256};
                                let mut hasher = Sha256::new();
                                hasher.update(&data);
                                hasher.finalize();
                                name
                            });
                            tasks.push(task);
                        }

                        let mut results = Vec::new();
                        for task in tasks {
                            if let Ok(name) = task.await {
                                results.push(name);
                            }
                        }

                        black_box(results)
                    }
                });
            },
        );
    }

    group.finish();
}

/// Benchmark dependency resolution algorithm
fn bench_dependency_resolution_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_dependency_resolution");

    for scenario in ["linear_5", "diamond_4", "complex_10"] {
        let dependencies = match scenario {
            "linear_5" => create_linear_deps(5),
            "diamond_4" => create_diamond_deps(),
            "complex_10" => create_complex_deps(10),
            _ => HashMap::new(),
        };

        group.bench_with_input(
            BenchmarkId::from_parameter(scenario),
            &dependencies,
            |b, deps| {
                b.iter(|| {
                    let resolved = topological_sort(deps);
                    black_box(resolved)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 2: SPARQL Query Performance Benchmarks
// ============================================================================

/// Benchmark SPARQL query parsing
fn bench_sparql_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_sparql_parsing");

    let queries = vec![
        ("simple", "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"),
        (
            "medium",
            "PREFIX pack: <http://ggen.dev/pack#>
             SELECT ?name ?version WHERE {
                 ?pack pack:name ?name .
                 ?pack pack:version ?version .
             }",
        ),
        (
            "complex",
            "PREFIX pack: <http://ggen.dev/pack#>
             PREFIX dep: <http://ggen.dev/dependency#>
             SELECT ?name ?depName WHERE {
                 ?pack pack:name ?name .
                 ?pack dep:requires ?dep .
                 ?dep pack:name ?depName .
                 FILTER regex(?name, \"test\")
             }",
        ),
    ];

    for (name, query) in queries {
        group.bench_with_input(BenchmarkId::from_parameter(name), &query, |b, q| {
            b.iter(|| {
                // Simulate parsing work
                let tokens: Vec<_> = q.split_whitespace().collect();
                let prefix_count = q.matches("PREFIX").count();
                let select_count = q.matches("SELECT").count();
                let where_count = q.matches("WHERE").count();
                black_box((tokens.len(), prefix_count, select_count, where_count))
            });
        });
    }

    group.finish();
}

/// Benchmark RDF triple conversion
fn bench_rdf_triple_conversion(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_rdf_conversion");

    for triple_count in [10, 100, 1000] {
        let triples: Vec<_> = (0..triple_count)
            .map(|i| {
                (
                    format!("http://example.org/subject{}", i),
                    format!("http://example.org/predicate{}", i % 10),
                    format!("http://example.org/object{}", i),
                )
            })
            .collect();

        group.throughput(Throughput::Elements(triple_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(triple_count),
            &triples,
            |b, triples| {
                b.iter(|| {
                    // Simulate RDF serialization
                    let serialized: Vec<String> = triples
                        .iter()
                        .map(|(s, p, o)| format!("<{}> <{}> <{}> .", s, p, o))
                        .collect();
                    black_box(serialized)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 2: Template Generation Performance
// ============================================================================

/// Benchmark template variable substitution
fn bench_template_substitution(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_template_substitution");

    for (name, line_count) in [("small", 100), ("medium", 500), ("large", 2000)] {
        let template =
            "fn {{FUNCTION_NAME}}() -> {{RETURN_TYPE}} {\n    Ok(())\n}\n".repeat(line_count);
        let variables = vec![
            ("FUNCTION_NAME", "test_function"),
            ("RETURN_TYPE", "Result<()>"),
        ];

        group.throughput(Throughput::Elements(line_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(template, variables),
            |b, (template, vars)| {
                b.iter(|| {
                    let mut result = template.clone();
                    for (key, value) in vars {
                        result = result.replace(&format!("{{{{{}}}}}", key), value);
                    }
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark template validation
fn bench_template_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase2_template_validation");

    for var_count in [5, 20, 50] {
        let variables: HashMap<String, String> = (0..var_count)
            .map(|i| (format!("VAR_{}", i), format!("value_{}", i)))
            .collect();

        let required_vars: Vec<String> = (0..var_count).map(|i| format!("VAR_{}", i)).collect();

        group.throughput(Throughput::Elements(var_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(var_count),
            &(variables, required_vars),
            |b, (vars, required)| {
                b.iter(|| {
                    // Validate all required variables are present
                    let missing: Vec<_> =
                        required.iter().filter(|r| !vars.contains_key(*r)).collect();
                    let valid = missing.is_empty();
                    black_box((valid, missing.len()))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 3: Advanced Dependency Resolution
// ============================================================================

/// Benchmark conflict detection
fn bench_conflict_detection(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase3_conflict_detection");

    for conflict_scenario in ["none", "few", "many"] {
        let (packs, _conflicts) = match conflict_scenario {
            "none" => (generate_packs(5, 0), 0),
            "few" => (generate_packs(10, 2), 2),
            "many" => (generate_packs(20, 5), 5),
            _ => (vec![], 0),
        };

        group.bench_with_input(
            BenchmarkId::from_parameter(conflict_scenario),
            &packs,
            |b, packs| {
                b.iter(|| {
                    let detected_conflicts = detect_version_conflicts(packs);
                    black_box(detected_conflicts)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark version resolution strategies
fn bench_version_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase3_version_resolution");

    for version_count in [3, 10, 20] {
        let versions: Vec<String> = (0..version_count).map(|i| format!("1.{}.0", i)).collect();

        group.bench_with_input(
            BenchmarkId::from_parameter(version_count),
            &versions,
            |b, versions| {
                b.iter(|| {
                    let selected = select_best_version(versions);
                    black_box(selected)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 3: Registry Operations
// ============================================================================

/// Benchmark registry search
fn bench_registry_search_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase3_registry_search");

    for registry_size in [100, 1000, 10000] {
        let packages: Vec<String> = (0..registry_size)
            .map(|i| format!("package-{:05}", i))
            .collect();

        let query = "package-00500";

        group.throughput(Throughput::Elements(registry_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(registry_size),
            &(packages, query),
            |b, (packages, query)| {
                b.iter(|| {
                    // Linear search
                    let results: Vec<_> = packages.iter().filter(|p| p.contains(query)).collect();
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark indexed search
fn bench_indexed_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase3_indexed_search");

    for registry_size in [100, 1000, 10000] {
        // Build search index
        let mut index: HashMap<String, Vec<String>> = HashMap::new();
        for i in 0..registry_size {
            let package = format!("package-{:05}", i);
            let key = format!("package-{:03}", i / 100);
            index.entry(key).or_default().push(package);
        }

        group.throughput(Throughput::Elements(registry_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(registry_size),
            &index,
            |b, index| {
                b.iter(|| {
                    // Indexed lookup
                    let key = "package-005";
                    let results = index.get(key);
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// PHASE 3: Caching Performance
// ============================================================================

/// Benchmark cache hit vs miss performance
fn bench_cache_hit_vs_miss(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase3_cache_performance");

    let mut cache: HashMap<String, Vec<u8>> = HashMap::new();
    let cached_data = vec![0u8; 1024 * 1024]; // 1MB
    cache.insert("cached-package".to_string(), cached_data.clone());

    group.bench_function("cache_hit", |b| {
        b.iter(|| {
            let data = cache.get("cached-package").unwrap();
            black_box(data)
        });
    });

    group.bench_function("cache_miss_with_load", |b| {
        b.iter(|| {
            // Simulate cache miss + load
            let key = "uncached-package";
            let data = cache.get(key).or_else(|| {
                // Simulate loading
                Some(&cached_data)
            });
            black_box(data)
        });
    });

    group.finish();
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_linear_deps(count: usize) -> HashMap<String, Vec<String>> {
    let mut deps = HashMap::new();
    for i in 1..count {
        deps.insert(format!("pkg-{}", i), vec![format!("pkg-{}", i - 1)]);
    }
    deps.insert(format!("pkg-0"), vec![]);
    deps
}

fn create_diamond_deps() -> HashMap<String, Vec<String>> {
    let mut deps = HashMap::new();
    deps.insert(
        "pkg-a".to_string(),
        vec!["pkg-b".to_string(), "pkg-c".to_string()],
    );
    deps.insert("pkg-b".to_string(), vec!["pkg-d".to_string()]);
    deps.insert("pkg-c".to_string(), vec!["pkg-d".to_string()]);
    deps.insert("pkg-d".to_string(), vec![]);
    deps
}

fn create_complex_deps(count: usize) -> HashMap<String, Vec<String>> {
    let mut deps = HashMap::new();
    for i in 0..count {
        let dep_count = (i % 3) + 1;
        let package_deps: Vec<_> = (0..dep_count)
            .map(|d| format!("pkg-{}", (i + d + 1) % count))
            .filter(|d| !d.contains(&format!("pkg-{}", i)))
            .collect();
        deps.insert(format!("pkg-{}", i), package_deps);
    }
    deps
}

fn topological_sort(deps: &HashMap<String, Vec<String>>) -> Vec<String> {
    // Simple topological sort implementation
    let mut sorted = Vec::new();
    let mut visited = std::collections::HashSet::new();

    fn visit(
        node: &str, deps: &HashMap<String, Vec<String>>,
        visited: &mut std::collections::HashSet<String>, sorted: &mut Vec<String>,
    ) {
        if visited.contains(node) {
            return;
        }
        visited.insert(node.to_string());

        if let Some(node_deps) = deps.get(node) {
            for dep in node_deps {
                visit(dep, deps, visited, sorted);
            }
        }

        sorted.push(node.to_string());
    }

    for node in deps.keys() {
        visit(node, deps, &mut visited, &mut sorted);
    }

    sorted
}

#[derive(Clone)]
struct PackInfo {
    name: String,
    version: String,
    dependencies: Vec<String>,
}

fn generate_packs(count: usize, conflicts: usize) -> Vec<PackInfo> {
    (0..count)
        .map(|i| PackInfo {
            name: format!("pack-{}", i),
            version: if i < conflicts {
                "1.0.0".to_string()
            } else {
                format!("1.{}.0", i)
            },
            dependencies: vec![],
        })
        .collect()
}

fn detect_version_conflicts(packs: &[PackInfo]) -> Vec<String> {
    let mut conflicts = Vec::new();
    let mut seen = HashMap::new();

    for pack in packs {
        if let Some(existing_version) = seen.get(&pack.name) {
            if existing_version != &pack.version {
                conflicts.push(format!(
                    "{}: {} vs {}",
                    pack.name, existing_version, pack.version
                ));
            }
        } else {
            seen.insert(pack.name.clone(), pack.version.clone());
        }
    }

    conflicts
}

fn select_best_version(versions: &[String]) -> String {
    // Simple semver selection: highest version
    versions
        .iter()
        .max()
        .cloned()
        .unwrap_or_else(|| "0.0.0".to_string())
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    phase2_install,
    bench_package_download_simulation,
    bench_parallel_installation,
    bench_dependency_resolution_performance,
);

criterion_group!(
    phase2_sparql,
    bench_sparql_parsing,
    bench_rdf_triple_conversion,
);

criterion_group!(
    phase2_templates,
    bench_template_substitution,
    bench_template_validation,
);

criterion_group!(
    phase3_resolution,
    bench_conflict_detection,
    bench_version_resolution,
);

criterion_group!(
    phase3_registry,
    bench_registry_search_performance,
    bench_indexed_search,
);

criterion_group!(phase3_caching, bench_cache_hit_vs_miss,);

criterion_main!(
    phase2_install,
    phase2_sparql,
    phase2_templates,
    phase3_resolution,
    phase3_registry,
    phase3_caching,
);
