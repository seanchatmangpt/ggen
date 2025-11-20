//! Comprehensive performance benchmarks for ggen marketplace V2
//!
//! SLO Targets:
//! - List 1000 packages: ≤500ms end-to-end
//! - SPARQL search: ≤200ms for text search across 1000 packages
//! - Version resolution: ≤100ms for dependency resolution
//! - Install operation: ≤3s end-to-end (fetch, verify, extract, register)
//! - Publish operation: ≤1s for metadata validation and storage

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_marketplace_v2::{
    config::MarketplaceConfig,
    models::{Package, PackageManifest, PackageMetadata, PackageVersion},
    registry::MarketplaceRegistry,
    sparql::SparqlQueryEngine,
};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;

// ============================================================================
// Benchmark Data Generation
// ============================================================================

/// Generate realistic package datasets for benchmarking
struct BenchmarkDataset {
    packages: Vec<Package>,
    temp_dir: TempDir,
    registry_path: PathBuf,
}

impl BenchmarkDataset {
    /// Create a new benchmark dataset with specified number of packages
    fn new(package_count: usize) -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let registry_path = temp_dir.path().join("registry");
        std::fs::create_dir_all(&registry_path).expect("Failed to create registry dir");

        let packages = Self::generate_packages(package_count);

        Self {
            packages,
            temp_dir,
            registry_path,
        }
    }

    /// Generate realistic packages with varying complexity
    fn generate_packages(count: usize) -> Vec<Package> {
        let mut packages = Vec::with_capacity(count);

        for i in 0..count {
            let package = Package {
                metadata: PackageMetadata {
                    name: format!("test-package-{}", i),
                    version: PackageVersion::parse(&format!("1.{}.0", i % 100))
                        .expect("Valid version"),
                    description: Some(format!("Test package {} for benchmarking", i)),
                    authors: vec![format!("author-{}", i % 10)],
                    license: Some("MIT".to_string()),
                    repository: Some(format!("https://github.com/test/package-{}", i)),
                    homepage: Some(format!("https://package-{}.io", i)),
                    documentation: Some(format!("https://docs.package-{}.io", i)),
                    keywords: vec![
                        format!("keyword-{}", i % 20),
                        format!("category-{}", i % 5),
                        "benchmark".to_string(),
                    ],
                    categories: vec![format!("category-{}", i % 5)],
                    dependencies: Self::generate_dependencies(i, count),
                    dev_dependencies: HashMap::new(),
                    readme: Some(format!("# Package {}\n\nThis is a test package.", i)),
                    manifest_path: None,
                },
                manifest: PackageManifest {
                    name: format!("test-package-{}", i),
                    version: format!("1.{}.0", i % 100),
                    description: Some(format!("Test package {} for benchmarking", i)),
                    authors: Some(vec![format!("author-{}", i % 10)]),
                    license: Some("MIT".to_string()),
                    dependencies: Self::generate_dependencies(i, count),
                    dev_dependencies: Some(HashMap::new()),
                    build_dependencies: Some(HashMap::new()),
                    features: Some(HashMap::new()),
                    metadata: None,
                },
                checksum: format!("sha256:{:064x}", i),
                download_url: format!(
                    "https://registry.io/packages/test-package-{}/1.{}.0",
                    i,
                    i % 100
                ),
                published_at: chrono::Utc::now(),
            };

            packages.push(package);
        }

        packages
    }

    /// Generate realistic dependency graphs
    fn generate_dependencies(package_idx: usize, total_packages: usize) -> HashMap<String, String> {
        let mut deps = HashMap::new();

        // Create realistic dependency patterns:
        // - Most packages have 0-5 dependencies
        // - Some packages have deeper dependency trees
        // - Avoid circular dependencies but allow complex graphs

        let dep_count = match package_idx % 10 {
            0..=5 => package_idx % 3,       // 0-2 deps for most packages
            6..=8 => 3 + (package_idx % 3), // 3-5 deps for some
            9 => 5 + (package_idx % 5),     // 5-9 deps for complex packages
            _ => 0,
        };

        for j in 0..dep_count {
            // Only depend on earlier packages to avoid circular deps
            if package_idx > 0 {
                let dep_idx = (package_idx - 1 - (j * (package_idx / (j + 1)))) % package_idx;
                deps.insert(
                    format!("test-package-{}", dep_idx),
                    format!("1.{}.0", dep_idx % 100),
                );
            }
        }

        deps
    }

    /// Create a registry populated with packages
    fn create_registry(&self) -> MarketplaceRegistry {
        let config = MarketplaceConfig {
            registry_path: self.registry_path.clone(),
            cache_dir: self.temp_dir.path().join("cache"),
            default_registry_url: "https://registry.io".to_string(),
            max_cache_size_mb: 1000,
            timeout_seconds: 30,
        };

        let mut registry = MarketplaceRegistry::new(config).expect("Failed to create registry");

        // Populate registry with packages
        for package in &self.packages {
            registry
                .register_package(package.clone())
                .expect("Failed to register package");
        }

        registry
    }
}

/// Generate pathological test cases for edge cases
struct PathologicalDataset {
    deeply_nested: BenchmarkDataset,
    wide_dependencies: BenchmarkDataset,
    diamond_dependencies: BenchmarkDataset,
}

impl PathologicalDataset {
    fn new() -> Self {
        Self {
            deeply_nested: Self::create_deeply_nested(50),
            wide_dependencies: Self::create_wide_dependencies(100),
            diamond_dependencies: Self::create_diamond_dependencies(100),
        }
    }

    /// Create a dataset with deeply nested dependencies (chain of 50 packages)
    fn create_deeply_nested(depth: usize) -> BenchmarkDataset {
        let mut dataset = BenchmarkDataset::new(depth);

        // Modify packages to create deep dependency chain
        for i in 1..depth {
            let deps = &mut dataset.packages[i].metadata.dependencies;
            deps.clear();
            deps.insert(
                format!("test-package-{}", i - 1),
                format!("1.{}.0", (i - 1) % 100),
            );
        }

        dataset
    }

    /// Create a dataset with wide dependencies (1 package depends on 99 others)
    fn create_wide_dependencies(count: usize) -> BenchmarkDataset {
        let mut dataset = BenchmarkDataset::new(count);

        // Make the last package depend on all others
        let last_idx = count - 1;
        let deps = &mut dataset.packages[last_idx].metadata.dependencies;
        deps.clear();

        for i in 0..last_idx {
            deps.insert(format!("test-package-{}", i), format!("1.{}.0", i % 100));
        }

        dataset
    }

    /// Create a dataset with diamond dependencies
    /// A -> B, C
    /// B -> D
    /// C -> D
    fn create_diamond_dependencies(count: usize) -> BenchmarkDataset {
        let mut dataset = BenchmarkDataset::new(count);

        // Create multiple diamond patterns
        for base in (0..count).step_by(4) {
            if base + 3 < count {
                // A depends on B and C
                let deps_a = &mut dataset.packages[base].metadata.dependencies;
                deps_a.clear();
                deps_a.insert(format!("test-package-{}", base + 1), "1.0.0".to_string());
                deps_a.insert(format!("test-package-{}", base + 2), "1.0.0".to_string());

                // B depends on D
                let deps_b = &mut dataset.packages[base + 1].metadata.dependencies;
                deps_b.clear();
                deps_b.insert(format!("test-package-{}", base + 3), "1.0.0".to_string());

                // C depends on D
                let deps_c = &mut dataset.packages[base + 2].metadata.dependencies;
                deps_c.clear();
                deps_c.insert(format!("test-package-{}", base + 3), "1.0.0".to_string());

                // D has no dependencies
                dataset.packages[base + 3].metadata.dependencies.clear();
            }
        }

        dataset
    }
}

// ============================================================================
// Benchmark 1: List Packages (SLO: ≤500ms for 1000 packages)
// ============================================================================

fn bench_list_packages(c: &mut Criterion) {
    let mut group = c.benchmark_group("list_packages");

    for package_count in [100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*package_count as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            package_count,
            |b, &count| {
                let dataset = BenchmarkDataset::new(count);
                let registry = dataset.create_registry();

                b.iter(|| {
                    let packages = registry.list_packages().expect("Failed to list packages");
                    black_box(packages)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 2: SPARQL Search (SLO: ≤200ms for text search)
// ============================================================================

fn bench_sparql_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_search");

    for package_count in [100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*package_count as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            package_count,
            |b, &count| {
                let dataset = BenchmarkDataset::new(count);
                let registry = dataset.create_registry();
                let query_engine = SparqlQueryEngine::new(registry.get_rdf_store());

                // Test various query patterns
                let queries = vec![
                    // Simple text search
                    "SELECT ?package WHERE { ?package a :Package . ?package :name ?name . FILTER(CONTAINS(?name, 'test')) }",
                    // Keyword search
                    "SELECT ?package WHERE { ?package a :Package . ?package :keywords ?kw . FILTER(?kw = 'benchmark') }",
                    // Version constraint
                    "SELECT ?package WHERE { ?package a :Package . ?package :version ?v . FILTER(?v >= '1.0.0') }",
                ];

                b.iter(|| {
                    for query in &queries {
                        let results = query_engine
                            .execute(query)
                            .expect("Failed to execute query");
                        black_box(results);
                    }
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 3: SPARQL with/without Indexes
// ============================================================================

fn bench_sparql_indexes(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_indexes");

    let dataset = BenchmarkDataset::new(1000);
    let registry = dataset.create_registry();

    // Without indexes
    group.bench_function("without_indexes", |b| {
        let query_engine = SparqlQueryEngine::new(registry.get_rdf_store());

        b.iter(|| {
            let results = query_engine
                .execute("SELECT ?package WHERE { ?package a :Package . ?package :name ?name . FILTER(CONTAINS(?name, 'test-package-500')) }")
                .expect("Failed to execute query");
            black_box(results)
        });
    });

    // With indexes (if supported by implementation)
    group.bench_function("with_indexes", |b| {
        let mut query_engine = SparqlQueryEngine::new(registry.get_rdf_store());
        query_engine.create_index("name_index").expect("Failed to create index");
        query_engine.create_index("version_index").expect("Failed to create index");

        b.iter(|| {
            let results = query_engine
                .execute("SELECT ?package WHERE { ?package a :Package . ?package :name ?name . FILTER(CONTAINS(?name, 'test-package-500')) }")
                .expect("Failed to execute query");
            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 4: Version Resolution (SLO: ≤100ms)
// ============================================================================

fn bench_version_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("version_resolution");

    for package_count in [100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*package_count as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            package_count,
            |b, &count| {
                let dataset = BenchmarkDataset::new(count);
                let registry = dataset.create_registry();

                // Test resolution of package with dependencies
                let test_package = &dataset.packages[count / 2];

                b.iter(|| {
                    let resolved = registry
                        .resolve_dependencies(
                            &test_package.metadata.name,
                            &test_package.metadata.version.to_string(),
                        )
                        .expect("Failed to resolve dependencies");
                    black_box(resolved)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 5: Pathological Cases
// ============================================================================

fn bench_pathological_cases(c: &mut Criterion) {
    let mut group = c.benchmark_group("pathological_cases");

    let datasets = PathologicalDataset::new();

    // Deeply nested dependencies
    group.bench_function("deeply_nested_50", |b| {
        let registry = datasets.deeply_nested.create_registry();
        let last_package = &datasets.deeply_nested.packages[49];

        b.iter(|| {
            let resolved = registry
                .resolve_dependencies(
                    &last_package.metadata.name,
                    &last_package.metadata.version.to_string(),
                )
                .expect("Failed to resolve deeply nested dependencies");
            black_box(resolved)
        });
    });

    // Wide dependencies
    group.bench_function("wide_dependencies_99", |b| {
        let registry = datasets.wide_dependencies.create_registry();
        let last_package = &datasets.wide_dependencies.packages[99];

        b.iter(|| {
            let resolved = registry
                .resolve_dependencies(
                    &last_package.metadata.name,
                    &last_package.metadata.version.to_string(),
                )
                .expect("Failed to resolve wide dependencies");
            black_box(resolved)
        });
    });

    // Diamond dependencies
    group.bench_function("diamond_dependencies", |b| {
        let registry = datasets.diamond_dependencies.create_registry();
        let test_package = &datasets.diamond_dependencies.packages[0];

        b.iter(|| {
            let resolved = registry
                .resolve_dependencies(
                    &test_package.metadata.name,
                    &test_package.metadata.version.to_string(),
                )
                .expect("Failed to resolve diamond dependencies");
            black_box(resolved)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 6: Install Operation (SLO: ≤3s)
// ============================================================================

fn bench_install_operation(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_operation");
    group.sample_size(10); // Fewer samples due to longer runtime

    for package_count in [100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            package_count,
            |b, &count| {
                let dataset = BenchmarkDataset::new(count);
                let registry = dataset.create_registry();

                // Test installation of package with moderate dependencies
                let test_package = &dataset.packages[count / 2];

                b.iter(|| {
                    // Simulate install: fetch, verify, extract, register
                    let result = registry.install_package(
                        &test_package.metadata.name,
                        &test_package.metadata.version.to_string(),
                    );
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 7: Publish Operation (SLO: ≤1s)
// ============================================================================

fn bench_publish_operation(c: &mut Criterion) {
    let mut group = c.benchmark_group("publish_operation");

    group.bench_function("publish_with_validation", |b| {
        let dataset = BenchmarkDataset::new(10);
        let registry = dataset.create_registry();

        let new_package = Package {
            metadata: PackageMetadata {
                name: "new-test-package".to_string(),
                version: PackageVersion::parse("1.0.0").expect("Valid version"),
                description: Some("New test package".to_string()),
                authors: vec!["author".to_string()],
                license: Some("MIT".to_string()),
                repository: None,
                homepage: None,
                documentation: None,
                keywords: vec!["test".to_string()],
                categories: vec![],
                dependencies: HashMap::new(),
                dev_dependencies: HashMap::new(),
                readme: Some("# New Package".to_string()),
                manifest_path: None,
            },
            manifest: PackageManifest {
                name: "new-test-package".to_string(),
                version: "1.0.0".to_string(),
                description: Some("New test package".to_string()),
                authors: Some(vec!["author".to_string()]),
                license: Some("MIT".to_string()),
                dependencies: HashMap::new(),
                dev_dependencies: Some(HashMap::new()),
                build_dependencies: Some(HashMap::new()),
                features: Some(HashMap::new()),
                metadata: None,
            },
            checksum: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_string(),
            download_url: "https://registry.io/packages/new-test-package/1.0.0".to_string(),
            published_at: chrono::Utc::now(),
        };

        b.iter(|| {
            let result = registry.publish_package(new_package.clone());
            black_box(result)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 8: Memory Usage Profiling
// ============================================================================

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");

    for package_count in [100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            package_count,
            |b, &count| {
                b.iter(|| {
                    let dataset = BenchmarkDataset::new(count);
                    let registry = dataset.create_registry();

                    // Measure memory by performing various operations
                    let _packages = registry.list_packages();
                    let _search = registry.search_packages("test");

                    black_box(registry)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 9: Concurrent Operations
// ============================================================================

fn bench_concurrent_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_operations");
    group.sample_size(10);

    group.bench_function("parallel_searches_10_threads", |b| {
        let dataset = BenchmarkDataset::new(1000);
        let registry = Arc::new(dataset.create_registry());

        b.iter(|| {
            let mut handles = vec![];

            for i in 0..10 {
                let registry_clone = Arc::clone(&registry);
                let handle = std::thread::spawn(move || {
                    let query = format!("test-package-{}", i * 100);
                    registry_clone.search_packages(&query)
                });
                handles.push(handle);
            }

            for handle in handles {
                let _ = handle.join();
            }
        });
    });

    group.bench_function("parallel_resolutions_10_threads", |b| {
        let dataset = BenchmarkDataset::new(1000);
        let registry = Arc::new(dataset.create_registry());

        b.iter(|| {
            let mut handles = vec![];

            for i in 0..10 {
                let registry_clone = Arc::clone(&registry);
                let package_name = format!("test-package-{}", i * 100);
                let handle = std::thread::spawn(move || {
                    registry_clone.resolve_dependencies(&package_name, "1.0.0")
                });
                handles.push(handle);
            }

            for handle in handles {
                let _ = handle.join();
            }
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Benchmark Groups
// ============================================================================

criterion_group!(
    benches,
    bench_list_packages,
    bench_sparql_search,
    bench_sparql_indexes,
    bench_version_resolution,
    bench_pathological_cases,
    bench_install_operation,
    bench_publish_operation,
    bench_memory_usage,
    bench_concurrent_operations,
);

criterion_main!(benches);
