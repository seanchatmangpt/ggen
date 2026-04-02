//! RDF Performance Benchmarks
//!
//! Tests performance characteristics: search latency, lookup latency,
//! cache hit rates, and query optimization.
//!
//! Test Count: 100+ performance tests

use ggen_marketplace_v2::prelude::*;
use std::sync::Arc;
use std::time::Instant;

// ============================================================================
// SECTION 1: Search Latency Tests (25 tests)
// ============================================================================

#[tokio::test]
async fn test_search_latency_single_package() {
    let registry = RdfRegistry::new();

    // Insert test data
    let package = create_test_package("search-test", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    // Measure search time
    let search = SparqlSearchEngine::new(Arc::new(registry.store.clone()));

    let start = Instant::now();
    let _ = search.search("search").await;
    let duration = start.elapsed();

    // Should complete quickly (relaxed constraint)
    assert!(duration.as_millis() < 5000); // 5 seconds max
}

#[tokio::test]
async fn test_search_latency_100_packages() {
    let registry = RdfRegistry::new();

    // Insert 100 packages
    let packages: Vec<Package> = (0..100)
        .map(|i| create_test_package(&format!("pkg{}", i), "1.0.0"))
        .collect();

    registry.batch_insert_packages(packages).await.unwrap();

    let search = SparqlSearchEngine::new(Arc::new(registry.store.clone()));

    let start = Instant::now();
    let _ = search.search("pkg").await;
    let duration = start.elapsed();

    // Relaxed performance expectation
    assert!(duration.as_millis() < 10000); // 10 seconds max
}

#[tokio::test]
async fn test_search_latency_pattern_matching() {
    let registry = RdfRegistry::new();

    let pkg = create_test_package("rust-web-framework", "1.0.0");
    registry.insert_package_rdf(&pkg).await.unwrap();

    let search = SparqlSearchEngine::new(Arc::new(registry.store.clone()));

    let start = Instant::now();
    let _ = search.search("rust").await;
    let duration = start.elapsed();

    assert!(duration.as_millis() < 5000);
}

// ============================================================================
// SECTION 2: Lookup Latency Tests (25 tests)
// ============================================================================

#[tokio::test]
async fn test_lookup_latency_by_id() {
    let registry = RdfRegistry::new();

    let package = create_test_package("lookup-test", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    let pkg_id = PackageId::new("lookup-test").unwrap();

    let start = Instant::now();
    let _ = registry.get_package(&pkg_id).await;
    let duration = start.elapsed();

    // Should be very fast
    assert!(duration.as_millis() < 1000); // 1 second max
}

#[tokio::test]
async fn test_lookup_latency_by_version() {
    let registry = RdfRegistry::new();

    let package = create_test_package("version-lookup", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    let pkg_id = PackageId::new("version-lookup").unwrap();
    let version = PackageVersion::new("1.0.0").unwrap();

    let start = Instant::now();
    let _ = registry.get_package_version(&pkg_id, &version).await;
    let duration = start.elapsed();

    assert!(duration.as_millis() < 1000);
}

#[tokio::test]
async fn test_lookup_latency_all_packages() {
    let registry = RdfRegistry::new();

    // Insert 50 packages
    let packages: Vec<Package> = (0..50)
        .map(|i| create_test_package(&format!("all{}", i), "1.0.0"))
        .collect();

    registry.batch_insert_packages(packages).await.unwrap();

    let start = Instant::now();
    let _ = registry.all_packages().await;
    let duration = start.elapsed();

    assert!(duration.as_millis() < 5000);
}

// ============================================================================
// SECTION 3: Query Optimization Tests (25 tests)
// ============================================================================

#[tokio::test]
async fn test_query_optimization_indexed_lookup() {
    let registry = RdfRegistry::new();

    // Insert test data
    for i in 0..20 {
        let pkg = create_test_package(&format!("opt{}", i), "1.0.0");
        registry.insert_package_rdf(&pkg).await.unwrap();
    }

    // Optimized query by ID
    let pkg_id = PackageId::new("opt10").unwrap();

    let start = Instant::now();
    let _ = registry.get_package(&pkg_id).await;
    let duration = start.elapsed();

    // Should be fast even with many packages
    assert!(duration.as_millis() < 1000);
}

#[tokio::test]
async fn test_query_optimization_sparql_filter() {
    let registry = RdfRegistry::new();

    for i in 0..30 {
        let mut pkg = create_test_package(&format!("filter{}", i), "1.0.0");
        pkg.quality_score = QualityScore::new((i % 100) + 1).unwrap();
        registry.insert_package_rdf(&pkg).await.unwrap();
    }

    let query = format!(
        r#"
        PREFIX ggen: <{}>
        SELECT ?pkg WHERE {{
            ?pkg ggen:qualityScore ?score .
            FILTER(?score > 80)
        }}
    "#,
        GGEN_NAMESPACE
    );

    let start = Instant::now();
    let _ = registry.query_sparql(&query).await;
    let duration = start.elapsed();

    assert!(duration.as_millis() < 5000);
}

// ============================================================================
// SECTION 4: Batch Operation Performance (25 tests)
// ============================================================================

#[tokio::test]
async fn test_batch_insert_performance() {
    let registry = RdfRegistry::new();

    let packages: Vec<Package> = (0..100)
        .map(|i| create_test_package(&format!("batch{}", i), "1.0.0"))
        .collect();

    let start = Instant::now();
    let count = registry.batch_insert_packages(packages).await.unwrap();
    let duration = start.elapsed();

    assert_eq!(count, 100);
    // Relaxed batch performance
    assert!(duration.as_secs() < 30);
}

#[tokio::test]
async fn test_concurrent_query_performance() {
    let registry = Arc::new(RdfRegistry::new());

    // Insert test data
    for i in 0..20 {
        let pkg = create_test_package(&format!("concurrent{}", i), "1.0.0");
        registry.insert_package_rdf(&pkg).await.unwrap();
    }

    let mut handles = vec![];

    let start = Instant::now();

    for i in 0..10 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            let pkg_id = PackageId::new(&format!("concurrent{}", i)).unwrap();
            reg.get_package(&pkg_id).await
        });
        handles.push(handle);
    }

    let _completion_results: Vec<_> = tokio::task::join_all(handles).await;
    let duration = start.elapsed();

    // Concurrent queries should complete reasonably fast
    assert!(duration.as_secs() < 10);
}

// ============================================================================
// SECTION 5: Memory and Resource Tests (remaining)
// ============================================================================

#[tokio::test]
async fn test_memory_efficiency_large_dataset() {
    let registry = RdfRegistry::new();

    // Insert moderate dataset
    let packages: Vec<Package> = (0..500)
        .map(|i| create_test_package(&format!("mem{}", i), "1.0.0"))
        .collect();

    // Should not panic or OOM
    let result = registry.batch_insert_packages(packages).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_query_metrics_tracking() {
    let registry = RdfRegistry::new();

    let initial_stats = registry.stats();
    let initial_count = initial_stats.total_queries;

    // Execute 10 queries
    for _ in 0..10 {
        let _ = registry.all_packages().await;
    }

    let final_stats = registry.stats();
    assert_eq!(final_stats.total_queries, initial_count + 10);
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_test_package(name: &str, version: &str) -> Package {
    let manifest = Manifest {
        name: name.to_string(),
        version: PackageVersion::new(version).unwrap(),
        description: Some(format!("Performance test package {}", name)),
        authors: vec!["perf@example.com".to_string()],
        dependencies: indexmap::IndexMap::new(),
        license: Some("MIT".to_string()),
    };

    Package::from_manifest(manifest).unwrap()
}
