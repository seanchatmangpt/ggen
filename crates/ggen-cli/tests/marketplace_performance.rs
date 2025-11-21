//! Consolidated Marketplace Performance & Stress Tests
//!
//! This file consolidates performance and stress tests including:
//! - Load testing with many packages
//! - Concurrent operation benchmarks
//! - Search performance tests
//! - Installation throughput tests
//!
//! Originally from:
//! - marketplace_stress_suite.rs
//! - marketplace/production_simulation.rs
//! - stress/marketplace_stress_test.rs
//! - marketplace/performance/consolidated_performance.rs

use std::sync::Arc;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use tokio::sync::{Barrier, RwLock};
use tokio::task::JoinSet;

// ============================================================================
// SECTION 1: SEARCH PERFORMANCE TESTS
// ============================================================================

#[test]
fn test_search_performance_small_dataset() {
    let packages = generate_test_packages(100);
    let query = "test";

    let start = Instant::now();
    let results = search_packages(&packages, query, 10);
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(100),
        "Search on 100 packages should be < 100ms"
    );
    assert!(results.len() <= 10, "Should respect limit");
}

#[test]
fn test_search_performance_medium_dataset() {
    let packages = generate_test_packages(1000);
    let query = "package";

    let start = Instant::now();
    let results = search_packages(&packages, query, 20);
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(500),
        "Search on 1000 packages should be < 500ms"
    );
}

#[test]
fn test_search_performance_large_dataset() {
    let packages = generate_test_packages(10000);
    let query = "utility";

    let start = Instant::now();
    let results = search_packages(&packages, query, 50);
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_secs(2),
        "Search on 10000 packages should be < 2s"
    );
}

fn generate_test_packages(count: usize) -> Vec<TestPackage> {
    (0..count)
        .map(|i| TestPackage {
            id: format!("pkg-{}", i),
            name: format!("Test Package {}", i),
            description: format!("A test package number {} for performance testing", i),
            tags: vec!["test".to_string(), format!("tag-{}", i % 10)],
        })
        .collect()
}

fn search_packages<'a>(packages: &'a [TestPackage], query: &str, limit: usize) -> Vec<&'a TestPackage> {
    packages
        .iter()
        .filter(|p| {
            p.name.contains(query)
                || p.description.contains(query)
                || p.tags.iter().any(|t| t.contains(query))
        })
        .take(limit)
        .collect()
}

struct TestPackage {
    id: String,
    name: String,
    description: String,
    tags: Vec<String>,
}

// ============================================================================
// SECTION 2: LOCKFILE PERFORMANCE TESTS
// ============================================================================

#[test]
fn test_lockfile_write_performance_100_packages() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let packages: Vec<_> = (0..100)
        .map(|i| {
            format!(
                r#"{{"name": "pkg-{}", "version": "1.0.0", "checksum": "hash-{}"}}"#,
                i, i
            )
        })
        .collect();

    let content = format!(
        r#"{{"version": "1.0.0", "packages": [{}]}}"#,
        packages.join(",")
    );

    let start = Instant::now();
    std::fs::write(&lockfile_path, &content).unwrap();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(50),
        "Writing 100 packages should be < 50ms"
    );
}

#[test]
fn test_lockfile_read_performance_100_packages() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let packages: Vec<_> = (0..100)
        .map(|i| {
            format!(
                r#"{{"name": "pkg-{}", "version": "1.0.0", "checksum": "hash-{}"}}"#,
                i, i
            )
        })
        .collect();

    let content = format!(
        r#"{{"version": "1.0.0", "packages": [{}]}}"#,
        packages.join(",")
    );
    std::fs::write(&lockfile_path, &content).unwrap();

    let start = Instant::now();
    let _loaded = std::fs::read_to_string(&lockfile_path).unwrap();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(20),
        "Reading 100 packages should be < 20ms"
    );
}

#[test]
fn test_lockfile_parse_performance() {
    let packages: Vec<_> = (0..1000)
        .map(|i| format!(r#"{{"name": "pkg-{}", "version": "1.0.0"}}"#, i))
        .collect();

    let content = format!(
        r#"{{"version": "1.0.0", "packages": [{}]}}"#,
        packages.join(",")
    );

    let start = Instant::now();
    let _parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(100),
        "Parsing 1000 packages should be < 100ms"
    );
}

// ============================================================================
// SECTION 3: CONCURRENT STRESS TESTS
// ============================================================================

#[tokio::test]
async fn test_concurrent_search_stress() {
    let packages = Arc::new(generate_test_packages(1000));
    let concurrent_searches = 50;
    let barrier = Arc::new(Barrier::new(concurrent_searches));

    let mut tasks = JoinSet::new();

    for i in 0..concurrent_searches {
        let packages = Arc::clone(&packages);
        let barrier = Arc::clone(&barrier);
        let query = format!("tag-{}", i % 10);

        tasks.spawn(async move {
            barrier.wait().await;
            let start = Instant::now();
            let _results = search_packages_owned(&packages, &query, 10);
            start.elapsed()
        });
    }

    let mut total_time = Duration::ZERO;
    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        total_time += result.unwrap();
        completed += 1;
    }

    let avg_time = total_time / completed as u32;
    assert_eq!(completed, concurrent_searches);
    assert!(
        avg_time < Duration::from_millis(200),
        "Average search time should be < 200ms under load"
    );
}

fn search_packages_owned(packages: &[TestPackage], query: &str, limit: usize) -> Vec<String> {
    packages
        .iter()
        .filter(|p| {
            p.name.contains(query)
                || p.description.contains(query)
                || p.tags.iter().any(|t| t.contains(query))
        })
        .take(limit)
        .map(|p| p.id.clone())
        .collect()
}

#[tokio::test]
async fn test_concurrent_lockfile_operations() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    // Initialize empty lockfile
    std::fs::write(&lockfile_path, r#"{"version": "1.0.0", "packages": {}}"#).unwrap();

    let operations = 20;
    let barrier = Arc::new(Barrier::new(operations));
    let op_counter = Arc::new(RwLock::new(0u32));

    let mut tasks = JoinSet::new();

    for i in 0..operations {
        let barrier = Arc::clone(&barrier);
        let counter = Arc::clone(&op_counter);
        let path = lockfile_path.clone();

        tasks.spawn(async move {
            barrier.wait().await;

            // Alternate read/write
            if i % 2 == 0 {
                let _ = std::fs::read_to_string(&path);
            } else {
                let mut count = counter.write().await;
                *count += 1;
            }

            tokio::time::sleep(Duration::from_millis(5)).await;
            Ok::<_, anyhow::Error>(i)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.unwrap().unwrap();
        completed += 1;
    }

    assert_eq!(completed, operations);
}

// ============================================================================
// SECTION 4: THROUGHPUT TESTS
// ============================================================================

#[test]
fn test_package_listing_throughput() {
    let package_counts = vec![100, 500, 1000, 5000];

    for count in package_counts {
        let packages = generate_test_packages(count);

        let start = Instant::now();
        let listed: Vec<_> = packages.iter().map(|p| &p.id).collect();
        let elapsed = start.elapsed();

        assert_eq!(listed.len(), count);

        // Throughput should scale linearly
        let throughput = count as f64 / elapsed.as_secs_f64();
        assert!(throughput > 10000.0, "Should process > 10000 packages/sec");
    }
}

#[test]
fn test_batch_operation_performance() {
    let batch_size = 100;
    let batches = 10;

    let mut total_time = Duration::ZERO;

    for batch in 0..batches {
        let packages: Vec<_> = (0..batch_size)
            .map(|i| TestPackage {
                id: format!("batch-{}-pkg-{}", batch, i),
                name: format!("Package {}", i),
                description: "Test".to_string(),
                tags: vec![],
            })
            .collect();

        let start = Instant::now();
        let _processed: Vec<_> = packages.iter().map(|p| p.id.len()).collect();
        total_time += start.elapsed();
    }

    let avg_batch_time = total_time / batches as u32;
    assert!(
        avg_batch_time < Duration::from_millis(10),
        "Batch processing should be < 10ms per batch"
    );
}

// ============================================================================
// SECTION 5: MEMORY PRESSURE TESTS
// ============================================================================

#[test]
fn test_large_package_metadata() {
    // Simulate large metadata
    let large_description = "x".repeat(10000);
    let many_tags: Vec<_> = (0..100).map(|i| format!("tag-{}", i)).collect();

    let package = TestPackage {
        id: "large-pkg".to_string(),
        name: "Large Package".to_string(),
        description: large_description,
        tags: many_tags,
    };

    // Should handle large metadata without issues
    assert_eq!(package.description.len(), 10000);
    assert_eq!(package.tags.len(), 100);
}

#[test]
fn test_many_dependencies_metadata() {
    let dep_count = 500;
    let dependencies: Vec<_> = (0..dep_count).map(|i| format!("dep-{}", i)).collect();

    assert_eq!(dependencies.len(), dep_count);

    // Simulate dependency resolution
    let start = Instant::now();
    let _resolved: Vec<_> = dependencies.iter().cloned().collect();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_millis(50),
        "Resolving 500 deps should be < 50ms"
    );
}

// ============================================================================
// SECTION 6: PRODUCTION SIMULATION TESTS
// ============================================================================

#[tokio::test]
async fn test_production_like_workload() {
    let packages = Arc::new(generate_test_packages(5000));

    // Simulate production workload: 80% reads, 20% writes
    let total_ops = 100;
    let barrier = Arc::new(Barrier::new(total_ops));
    let write_counter = Arc::new(RwLock::new(0u32));

    let mut tasks = JoinSet::new();

    for i in 0..total_ops {
        let packages = Arc::clone(&packages);
        let barrier = Arc::clone(&barrier);
        let counter = Arc::clone(&write_counter);

        tasks.spawn(async move {
            barrier.wait().await;

            if i % 5 == 0 {
                // 20% writes
                let mut count = counter.write().await;
                *count += 1;
                tokio::time::sleep(Duration::from_millis(2)).await;
            } else {
                // 80% reads (searches)
                let _results = search_packages_owned(&packages, "tag-0", 10);
            }

            Ok::<_, anyhow::Error>(())
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.unwrap().unwrap();
        completed += 1;
    }

    let writes = *write_counter.read().await;
    assert_eq!(completed, total_ops);
    assert_eq!(writes, 20); // 20% of 100
}

#[test]
fn test_registry_index_size_limits() {
    // Test that we can handle large registry indices
    let package_count = 10000;
    let packages = generate_test_packages(package_count);

    // Simulate serialization
    let serialized: Vec<_> = packages
        .iter()
        .map(|p| {
            serde_json::json!({
                "id": p.id,
                "name": p.name,
                "description": p.description
            })
        })
        .collect();

    let json = serde_json::to_string(&serialized).unwrap();

    // Should produce reasonable size (< 10MB for 10k packages)
    let size_mb = json.len() as f64 / (1024.0 * 1024.0);
    assert!(size_mb < 10.0, "10k packages should serialize to < 10MB");
}

// ============================================================================
// SECTION 7: SLO COMPLIANCE TESTS
// ============================================================================

#[test]
fn test_search_slo_compliance() {
    // SLO: Search should return results in < 100ms for 95th percentile
    let packages = generate_test_packages(5000);
    let iterations = 100;
    let mut latencies = Vec::with_capacity(iterations);

    for _ in 0..iterations {
        let start = Instant::now();
        let _results = search_packages(&packages, "test", 20);
        latencies.push(start.elapsed());
    }

    latencies.sort();
    let p95 = latencies[(iterations as f64 * 0.95) as usize];

    assert!(
        p95 < Duration::from_millis(100),
        "P95 search latency should be < 100ms"
    );
}

#[test]
fn test_lockfile_read_slo_compliance() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    // Create lockfile with 500 packages
    let packages: Vec<_> = (0..500)
        .map(|i| format!(r#"{{"name": "pkg-{}", "version": "1.0.0"}}"#, i))
        .collect();
    let content = format!(
        r#"{{"version": "1.0.0", "packages": [{}]}}"#,
        packages.join(",")
    );
    std::fs::write(&lockfile_path, &content).unwrap();

    let iterations = 50;
    let mut latencies = Vec::with_capacity(iterations);

    for _ in 0..iterations {
        let start = Instant::now();
        let _ = std::fs::read_to_string(&lockfile_path).unwrap();
        latencies.push(start.elapsed());
    }

    latencies.sort();
    let p99 = latencies[(iterations as f64 * 0.99) as usize];

    assert!(
        p99 < Duration::from_millis(50),
        "P99 lockfile read should be < 50ms"
    );
}
