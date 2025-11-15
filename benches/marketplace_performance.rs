use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::runtime::Runtime;

// ============================================================================
// Test Data Structures
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestPackage {
    name: String,
    version: String,
    description: String,
    author: String,
    tags: Vec<String>,
    dependencies: HashMap<String, String>,
    size_bytes: u64,
}

#[derive(Debug, Clone)]
struct TestRegistry {
    packages: Vec<TestPackage>,
    index_path: PathBuf,
    cache_dir: PathBuf,
}

// ============================================================================
// Test Registry Setup
// ============================================================================

fn setup_test_registry(num_packages: usize, temp_dir: &TempDir) -> TestRegistry {
    let index_path = temp_dir.path().join("registry");
    let cache_dir = temp_dir.path().join("cache");

    std::fs::create_dir_all(&index_path).unwrap();
    std::fs::create_dir_all(&cache_dir).unwrap();

    let mut packages = Vec::with_capacity(num_packages);

    // Create diverse package set
    for i in 0..num_packages {
        let category = i % 5;
        let (name, tags) = match category {
            0 => (
                format!("web-framework-{}", i),
                vec!["web", "framework", "http"],
            ),
            1 => (format!("cli-tool-{}", i), vec!["cli", "terminal", "tool"]),
            2 => (format!("data-parser-{}", i), vec!["data", "parser", "json"]),
            3 => (
                format!("util-helper-{}", i),
                vec!["utility", "helper", "core"],
            ),
            _ => (format!("misc-package-{}", i), vec!["misc", "general"]),
        };

        let num_deps = if i % 10 == 0 {
            5
        } else if i % 5 == 0 {
            2
        } else {
            0
        };
        let mut dependencies = HashMap::new();
        for d in 0..num_deps {
            dependencies.insert(format!("dep-{}-{}", i, d), format!("^{}.0.0", d + 1));
        }

        packages.push(TestPackage {
            name: name.clone(),
            version: format!("{}.{}.{}", i / 100 + 1, (i / 10) % 10, i % 10),
            description: format!("Test package {} for benchmarking", name),
            author: format!("author-{}", i % 20),
            tags: tags.iter().map(|s| s.to_string()).collect(),
            dependencies,
            size_bytes: (1024 * (10 + (i % 100))) as u64,
        });
    }

    // Write index file
    let index_file = index_path.join("index.json");
    let index_json = serde_json::to_string_pretty(&packages).unwrap();
    std::fs::write(&index_file, index_json).unwrap();

    TestRegistry {
        packages,
        index_path,
        cache_dir,
    }
}

fn setup_deep_dependency_tree(temp_dir: &TempDir) -> TestRegistry {
    let index_path = temp_dir.path().join("registry");
    let cache_dir = temp_dir.path().join("cache");

    std::fs::create_dir_all(&index_path).unwrap();
    std::fs::create_dir_all(&cache_dir).unwrap();

    let mut packages = Vec::new();

    // Create a deep dependency tree: root -> level1 -> level2 -> ... -> level10
    for level in 0..10 {
        for pkg in 0..5 {
            let name = format!("dep-level{}-pkg{}", level, pkg);
            let mut dependencies = HashMap::new();

            // Each package depends on packages from the next level
            if level < 9 {
                for next_pkg in 0..3 {
                    dependencies.insert(
                        format!("dep-level{}-pkg{}", level + 1, next_pkg),
                        "^1.0.0".to_string(),
                    );
                }
            }

            packages.push(TestPackage {
                name,
                version: "1.0.0".to_string(),
                description: format!("Dependency at level {}", level),
                author: "benchmark".to_string(),
                tags: vec!["dependency".to_string()],
                dependencies,
                size_bytes: 1024 * 10,
            });
        }
    }

    // Add root package
    let mut root_deps = HashMap::new();
    for pkg in 0..5 {
        root_deps.insert(format!("dep-level0-pkg{}", pkg), "^1.0.0".to_string());
    }

    packages.push(TestPackage {
        name: "root-package".to_string(),
        version: "1.0.0".to_string(),
        description: "Root package with deep dependencies".to_string(),
        author: "benchmark".to_string(),
        tags: vec!["root".to_string()],
        dependencies: root_deps,
        size_bytes: 1024 * 20,
    });

    // Write index
    let index_file = index_path.join("index.json");
    let index_json = serde_json::to_string_pretty(&packages).unwrap();
    std::fs::write(&index_file, index_json).unwrap();

    TestRegistry {
        packages,
        index_path,
        cache_dir,
    }
}

// ============================================================================
// Benchmark: Registry Index Loading
// ============================================================================

fn bench_registry_loading(c: &mut Criterion) {
    let mut group = c.benchmark_group("registry_loading");

    for size in [10, 100, 1000].iter() {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(*size, &temp_dir);

        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::new("load_index", size), size, |b, _| {
            b.iter(|| {
                let index_file = registry.index_path.join("index.json");
                let content = std::fs::read_to_string(&index_file).unwrap();
                let packages: Vec<TestPackage> = serde_json::from_str(&content).unwrap();
                black_box(packages)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark: Search Performance
// ============================================================================

fn bench_search_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_performance");

    for size in [100, 1000].iter() {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(*size, &temp_dir);

        // Load packages into searchable format
        let index_file = registry.index_path.join("index.json");
        let content = std::fs::read_to_string(&index_file).unwrap();
        let packages: Vec<TestPackage> = serde_json::from_str(&content).unwrap();

        // Benchmark exact keyword search
        group.bench_with_input(BenchmarkId::new("keyword_search", size), size, |b, _| {
            b.iter(|| {
                let results: Vec<_> = packages
                    .iter()
                    .filter(|p| p.name.contains("web-framework"))
                    .collect();
                black_box(results)
            });
        });

        // Benchmark tag filtering
        group.bench_with_input(BenchmarkId::new("tag_filter", size), size, |b, _| {
            b.iter(|| {
                let results: Vec<_> = packages
                    .iter()
                    .filter(|p| p.tags.contains(&"web".to_string()))
                    .collect();
                black_box(results)
            });
        });

        // Benchmark fuzzy search
        group.bench_with_input(BenchmarkId::new("fuzzy_search", size), size, |b, _| {
            b.iter(|| {
                let query = "web fram";
                let results: Vec<_> = packages
                    .iter()
                    .filter(|p| {
                        p.name.to_lowercase().contains(&query.to_lowercase())
                            || p.description.to_lowercase().contains(&query.to_lowercase())
                    })
                    .collect();
                black_box(results)
            });
        });

        // Benchmark combined filters
        group.bench_with_input(BenchmarkId::new("combined_filters", size), size, |b, _| {
            b.iter(|| {
                let results: Vec<_> = packages
                    .iter()
                    .filter(|p| {
                        p.tags.contains(&"web".to_string())
                            && p.author.starts_with("author-1")
                            && !p.dependencies.is_empty()
                    })
                    .collect();
                black_box(results)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark: Installation Performance
// ============================================================================

fn bench_installation_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("installation_performance");
    group.sample_size(20); // Reduce sample size for slower operations

    // Small package (no dependencies)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(50, &temp_dir);

        group.bench_function("install_small_no_deps", |b| {
            b.iter(|| {
                let install_dir = temp_dir.path().join("install_small");
                std::fs::create_dir_all(&install_dir).unwrap();

                // Simulate package extraction
                let pkg = &registry.packages[0];
                let pkg_dir = install_dir.join(&pkg.name);
                std::fs::create_dir_all(&pkg_dir).unwrap();

                // Write package metadata
                let metadata = serde_json::to_string_pretty(&pkg).unwrap();
                std::fs::write(pkg_dir.join("package.json"), metadata).unwrap();

                black_box(pkg_dir)
            });
        });
    }

    // Medium package (2-3 dependencies)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(50, &temp_dir);

        group.bench_function("install_medium_with_deps", |b| {
            b.iter(|| {
                let install_dir = temp_dir.path().join("install_medium");
                std::fs::create_dir_all(&install_dir).unwrap();

                // Find package with dependencies
                let pkg = registry
                    .packages
                    .iter()
                    .find(|p| p.dependencies.len() >= 2)
                    .unwrap();

                // Install main package
                let pkg_dir = install_dir.join(&pkg.name);
                std::fs::create_dir_all(&pkg_dir).unwrap();
                std::fs::write(
                    pkg_dir.join("package.json"),
                    serde_json::to_string_pretty(&pkg).unwrap(),
                )
                .unwrap();

                // Install dependencies
                for (dep_name, _version) in &pkg.dependencies {
                    let dep_dir = install_dir.join(dep_name);
                    std::fs::create_dir_all(&dep_dir).unwrap();
                }

                black_box(pkg_dir)
            });
        });
    }

    // Large package (5+ dependencies)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(50, &temp_dir);

        group.bench_function("install_large_many_deps", |b| {
            b.iter(|| {
                let install_dir = temp_dir.path().join("install_large");
                std::fs::create_dir_all(&install_dir).unwrap();

                // Find package with many dependencies
                let pkg = registry
                    .packages
                    .iter()
                    .find(|p| p.dependencies.len() >= 5)
                    .unwrap();

                // Install main package
                let pkg_dir = install_dir.join(&pkg.name);
                std::fs::create_dir_all(&pkg_dir).unwrap();
                std::fs::write(
                    pkg_dir.join("package.json"),
                    serde_json::to_string_pretty(&pkg).unwrap(),
                )
                .unwrap();

                // Install all dependencies
                for (dep_name, _version) in &pkg.dependencies {
                    let dep_dir = install_dir.join(dep_name);
                    std::fs::create_dir_all(&dep_dir).unwrap();

                    // Create dummy files to simulate package content
                    std::fs::write(dep_dir.join("index.js"), "module.exports = {};").unwrap();
                }

                black_box(pkg_dir)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark: Dependency Resolution
// ============================================================================

fn bench_dependency_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("dependency_resolution");

    // Shallow dependency tree (1-2 levels)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(100, &temp_dir);

        group.bench_function("resolve_shallow_tree", |b| {
            b.iter(|| {
                let pkg = registry
                    .packages
                    .iter()
                    .find(|p| p.dependencies.len() >= 2 && p.dependencies.len() <= 3)
                    .unwrap();

                let mut resolved = HashMap::new();
                resolved.insert(pkg.name.clone(), pkg.version.clone());

                // Resolve direct dependencies
                for (dep_name, dep_version) in &pkg.dependencies {
                    resolved.insert(dep_name.clone(), dep_version.clone());
                }

                black_box(resolved)
            });
        });
    }

    // Deep dependency tree (10 levels)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_deep_dependency_tree(&temp_dir);

        group.bench_function("resolve_deep_tree", |b| {
            b.iter(|| {
                let root = registry
                    .packages
                    .iter()
                    .find(|p| p.name == "root-package")
                    .unwrap();

                let mut resolved = HashMap::new();
                let mut to_resolve = vec![(root.name.clone(), root.version.clone())];

                // BFS dependency resolution
                while let Some((name, version)) = to_resolve.pop() {
                    if resolved.contains_key(&name) {
                        continue;
                    }

                    resolved.insert(name.clone(), version.clone());

                    if let Some(pkg) = registry.packages.iter().find(|p| p.name == name) {
                        for (dep_name, dep_version) in &pkg.dependencies {
                            to_resolve.push((dep_name.clone(), dep_version.clone()));
                        }
                    }
                }

                black_box(resolved)
            });
        });
    }

    // Large flat dependency list (50 packages)
    {
        let temp_dir = TempDir::new().unwrap();
        let registry = setup_test_registry(100, &temp_dir);

        group.bench_function("resolve_50_packages", |b| {
            b.iter(|| {
                let mut resolved = HashMap::new();

                // Resolve first 50 packages
                for pkg in registry.packages.iter().take(50) {
                    resolved.insert(pkg.name.clone(), pkg.version.clone());

                    for (dep_name, dep_version) in &pkg.dependencies {
                        resolved
                            .entry(dep_name.clone())
                            .or_insert(dep_version.clone());
                    }
                }

                black_box(resolved)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark: Cache Performance
// ============================================================================

fn bench_cache_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_performance");

    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("cache");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Prepare cache with some packages
    let test_packages = vec![
        ("cached-pkg-1", "1.0.0"),
        ("cached-pkg-2", "2.0.0"),
        ("cached-pkg-3", "3.0.0"),
    ];

    for (name, version) in &test_packages {
        let pkg_cache = cache_dir.join(format!("{}-{}", name, version));
        std::fs::create_dir_all(&pkg_cache).unwrap();
        std::fs::write(
            pkg_cache.join("package.json"),
            format!(r#"{{"name":"{}","version":"{}"}}"#, name, version),
        )
        .unwrap();
    }

    // Benchmark cache hit
    group.bench_function("cache_hit", |b| {
        b.iter(|| {
            let pkg_cache = cache_dir.join("cached-pkg-1-1.0.0");
            let exists = pkg_cache.exists();
            if exists {
                let content = std::fs::read_to_string(pkg_cache.join("package.json")).unwrap();
                black_box(content)
            } else {
                black_box(String::new())
            }
        });
    });

    // Benchmark cache miss
    group.bench_function("cache_miss", |b| {
        b.iter(|| {
            let pkg_cache = cache_dir.join("non-existent-pkg-1.0.0");
            let exists = pkg_cache.exists();
            black_box(exists)
        });
    });

    // Benchmark cache write
    group.bench_function("cache_write", |b| {
        b.iter(|| {
            let pkg_name = format!("new-pkg-{}", fastrand::u32(..));
            let pkg_cache = cache_dir.join(format!("{}-1.0.0", pkg_name));
            std::fs::create_dir_all(&pkg_cache).unwrap();
            std::fs::write(
                pkg_cache.join("package.json"),
                format!(r#"{{"name":"{}","version":"1.0.0"}}"#, pkg_name),
            )
            .unwrap();
            black_box(pkg_cache)
        });
    });

    // Benchmark cache cleanup
    group.bench_function("cache_cleanup", |b| {
        b.iter(|| {
            let entries = std::fs::read_dir(&cache_dir).unwrap();
            let mut count = 0;
            for entry in entries {
                if let Ok(_entry) = entry {
                    count += 1;
                }
            }
            black_box(count)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Concurrent Operations
// ============================================================================

fn bench_concurrent_operations(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("concurrent_operations");
    group.sample_size(10); // Reduce sample size for concurrent tests

    let temp_dir = TempDir::new().unwrap();
    let registry = Arc::new(setup_test_registry(1000, &temp_dir));

    // Parallel searches
    group.bench_function("parallel_searches_10", |b| {
        b.iter(|| {
            rt.block_on(async {
                let registry = Arc::clone(&registry);
                let mut handles = vec![];

                for i in 0..10 {
                    let registry = Arc::clone(&registry);
                    let handle = tokio::spawn(async move {
                        let query = match i % 3 {
                            0 => "web",
                            1 => "cli",
                            _ => "data",
                        };

                        let results: Vec<_> = registry
                            .packages
                            .iter()
                            .filter(|p| p.name.contains(query))
                            .collect();

                        results.len()
                    });
                    handles.push(handle);
                }

                // Wait for all tasks to complete
                let mut results = Vec::new();
                for handle in handles {
                    results.push(handle.await.unwrap());
                }
                black_box(results)
            })
        });
    });

    // Parallel installations
    group.bench_function("parallel_installs_5", |b| {
        b.iter(|| {
            rt.block_on(async {
                let registry = Arc::clone(&registry);
                let mut handles = vec![];

                for i in 0..5 {
                    let registry = Arc::clone(&registry);
                    let temp_dir_path = temp_dir.path().to_path_buf();

                    let handle = tokio::spawn(async move {
                        let install_dir = temp_dir_path.join(format!("concurrent_install_{}", i));
                        std::fs::create_dir_all(&install_dir).unwrap();

                        let pkg = &registry.packages[i * 10];
                        let pkg_dir = install_dir.join(&pkg.name);
                        std::fs::create_dir_all(&pkg_dir).unwrap();

                        std::fs::write(
                            pkg_dir.join("package.json"),
                            serde_json::to_string_pretty(&pkg).unwrap(),
                        )
                        .unwrap();

                        pkg_dir
                    });
                    handles.push(handle);
                }

                // Wait for all tasks to complete
                let mut results = Vec::new();
                for handle in handles {
                    results.push(handle.await.unwrap());
                }
                black_box(results)
            })
        });
    });

    // Mixed operations (search + install)
    group.bench_function("mixed_operations_10", |b| {
        b.iter(|| {
            rt.block_on(async {
                let registry = Arc::clone(&registry);
                let mut handles = vec![];

                for i in 0..10 {
                    let registry = Arc::clone(&registry);
                    let temp_dir_path = temp_dir.path().to_path_buf();

                    let handle = tokio::spawn(async move {
                        if i % 2 == 0 {
                            // Search operation
                            let results: Vec<_> = registry
                                .packages
                                .iter()
                                .filter(|p| p.tags.contains(&"web".to_string()))
                                .take(10)
                                .collect();
                            results.len()
                        } else {
                            // Install operation
                            let install_dir = temp_dir_path.join(format!("mixed_op_{}", i));
                            std::fs::create_dir_all(&install_dir).unwrap();

                            let pkg = &registry.packages[i];
                            let pkg_dir = install_dir.join(&pkg.name);
                            std::fs::create_dir_all(&pkg_dir).unwrap();
                            1
                        }
                    });
                    handles.push(handle);
                }

                // Wait for all tasks to complete
                let mut results = Vec::new();
                for handle in handles {
                    results.push(handle.await.unwrap());
                }
                black_box(results)
            })
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    benches,
    bench_registry_loading,
    bench_search_performance,
    bench_installation_performance,
    bench_dependency_resolution,
    bench_cache_performance,
    bench_concurrent_operations,
);

criterion_main!(benches);
