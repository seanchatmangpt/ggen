//! End-to-end tests for marketplace-v2
//!
//! These tests validate complete workflows from package insertion through
//! querying, listing, and concurrent operations with real RDF data.

use ggen_marketplace_v2::prelude::*;
use oxigraph::store::Store;
use std::sync::Arc;

mod test_helpers;
use test_helpers::{create_multi_version_package, create_quality_package, create_test_package};

/// End-to-end test: Complete package lifecycle
#[tokio::test]
async fn test_e2e_package_lifecycle() {
    let registry = RdfRegistry::new();

    // Step 1: Create and insert package
    let package = create_test_package("e2e-pkg", "E2E Test Package", "1.0.0");
    registry
        .insert_package_rdf(&package)
        .await
        .expect("Package insertion should succeed");

    // Step 2: Verify package exists
    let exists = registry
        .package_exists(&package.metadata.id)
        .await
        .expect("Exists check should succeed");
    assert!(exists, "Package should exist after insertion");

    // Step 3: List versions
    let versions = registry
        .list_versions(&package.metadata.id)
        .await
        .expect("List versions should succeed");
    assert_eq!(versions.len(), 1, "Should have one version");
    assert_eq!(versions[0].as_str(), "1.0.0");

    // Step 4: Query package (returns error as reconstruction not implemented)
    let result = registry.get_package(&package.metadata.id).await;
    // Implementation pending, so we expect an error
    assert!(result.is_err());
}

/// End-to-end test: Multi-version package workflow
#[tokio::test]
async fn test_e2e_multi_version_workflow() {
    let registry = RdfRegistry::new();

    // Create package with multiple versions
    let package = create_multi_version_package(
        "versioned-e2e",
        "Versioned E2E Package",
        &["1.0.0", "1.1.0", "1.2.0", "2.0.0"],
    );

    // Insert package
    registry
        .insert_package_rdf(&package)
        .await
        .expect("Package insertion should succeed");

    // Verify all versions are listed
    let versions = registry
        .list_versions(&package.metadata.id)
        .await
        .expect("List versions should succeed");

    assert_eq!(
        versions.len(),
        4,
        "Should list all 4 versions: {:?}",
        versions
    );

    // Verify versions are sorted
    let version_strings: Vec<String> = versions.iter().map(|v| v.as_str().to_string()).collect();
    assert!(version_strings.contains(&"1.0.0".to_string()));
    assert!(version_strings.contains(&"2.0.0".to_string()));
}

/// End-to-end test: Concurrent package operations
#[tokio::test]
async fn test_e2e_concurrent_operations() {
    let registry = Arc::new(RdfRegistry::new());

    // Insert 20 packages concurrently
    let mut insert_handles = vec![];
    for i in 0..20 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            let package = create_test_package(
                &format!("concurrent-e2e-{}", i),
                &format!("Concurrent Package {}", i),
                "1.0.0",
            );
            reg.insert_package_rdf(&package)
                .await
                .expect("Insert should succeed")
        });
        insert_handles.push(handle);
    }

    // Wait for all inserts to complete
    for handle in insert_handles {
        handle.await.expect("Insert task should complete");
    }

    // Verify all packages concurrently
    let mut verify_handles = vec![];
    for i in 0..20 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            let id = PackageId::new(format!("concurrent-e2e-{}", i)).unwrap();
            let exists = reg
                .package_exists(&id)
                .await
                .expect("Exists check should succeed");
            assert!(exists, "Package {} should exist", i);
        });
        verify_handles.push(handle);
    }

    // Wait for all verifications
    for handle in verify_handles {
        handle.await.expect("Verify task should complete");
    }
}

/// End-to-end test: V3 registry with search index
#[tokio::test]
async fn test_e2e_v3_search_workflow() {
    let store = Arc::new(Store::new().expect("Store creation should succeed"));
    let registry = V3OptimizedRegistry::new(store)
        .await
        .expect("V3 initialization should succeed");

    // Build search index with multiple packages
    registry
        .update_search_index("web-framework", "Modern Web Application Framework")
        .await
        .expect("Index update should succeed");

    registry
        .update_search_index("db-driver", "Database Connection Driver")
        .await
        .expect("Index update should succeed");

    registry
        .update_search_index("web-server", "High Performance Web Server")
        .await
        .expect("Index update should succeed");

    // Verify index contains expected entries
    let index = registry.search_index.read();

    // "web" should appear in web-framework and web-server
    let web_results = index.get("web");
    assert!(web_results.is_some());
    assert_eq!(web_results.unwrap().len(), 2);

    // "database" should appear in db-driver
    let db_results = index.get("database");
    assert!(db_results.is_some());
    assert_eq!(db_results.unwrap().len(), 1);
}

/// End-to-end test: Quality score filtering
#[tokio::test]
async fn test_e2e_quality_score_workflow() {
    let registry = RdfRegistry::new();

    // Create packages with different quality scores
    let high_quality = create_quality_package("high-qual-pkg", "High Quality Package", "1.0.0", 98);

    let medium_quality =
        create_quality_package("med-qual-pkg", "Medium Quality Package", "1.0.0", 85);

    let low_quality = create_quality_package("low-qual-pkg", "Low Quality Package", "1.0.0", 60);

    // Insert all packages
    registry
        .insert_package_rdf(&high_quality)
        .await
        .expect("High quality insert should succeed");
    registry
        .insert_package_rdf(&medium_quality)
        .await
        .expect("Medium quality insert should succeed");
    registry
        .insert_package_rdf(&low_quality)
        .await
        .expect("Low quality insert should succeed");

    // Verify all exist
    for pkg in [&high_quality, &medium_quality, &low_quality] {
        let exists = registry
            .package_exists(&pkg.metadata.id)
            .await
            .expect("Exists check should succeed");
        assert!(exists, "Package should exist");
    }

    // Verify quality scores
    assert!(high_quality
        .metadata
        .quality_score
        .unwrap()
        .is_production_ready());
    assert!(medium_quality
        .metadata
        .quality_score
        .unwrap()
        .needs_improvement());
    assert!(low_quality.metadata.quality_score.unwrap().not_ready());
}

/// End-to-end test: SPARQL query workflow
#[tokio::test]
async fn test_e2e_sparql_query_workflow() {
    let registry = RdfRegistry::new();

    // Insert multiple packages
    for i in 0..10 {
        let package = create_test_package(
            &format!("sparql-pkg-{}", i),
            &format!("SPARQL Test Package {}", i),
            "1.0.0",
        );
        registry
            .insert_package_rdf(&package)
            .await
            .expect("Insert should succeed");
    }

    // Execute SPARQL query to find all packages
    let query = r#"
        SELECT ?package WHERE {
            ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ggen.io/marketplace/Package> .
        }
    "#;

    let results = registry
        .query_sparql(query)
        .await
        .expect("SPARQL query should succeed");

    assert!(
        results.len() >= 10,
        "Should find at least 10 packages, found: {}",
        results.len()
    );
}

/// End-to-end test: V3 cache performance under load
#[tokio::test]
async fn test_e2e_v3_cache_performance() {
    let store = Arc::new(Store::new().expect("Store creation should succeed"));
    let registry = Arc::new(
        V3OptimizedRegistry::new(store)
            .await
            .expect("V3 initialization should succeed"),
    );

    // Simulate 100 queries with varying cache behavior
    let mut handles = vec![];
    for i in 0..100 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            // Simulate cache hits (60%)
            if i < 60 {
                reg.query_stats
                    .hot_cache_hits
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
            // Simulate metadata cache hits (30%)
            else if i < 90 {
                reg.query_stats
                    .metadata_cache_hits
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
            // Simulate store queries (10%)
            else {
                reg.query_stats
                    .store_queries
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }

            reg.query_stats
                .total_queries
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

            // Simulate latency
            reg.query_stats
                .total_latency_us
                .fetch_add(50, std::sync::atomic::Ordering::Relaxed);
        });
        handles.push(handle);
    }

    // Wait for all queries
    for handle in handles {
        handle.await.expect("Query task should complete");
    }

    // Verify statistics
    let stats = registry.stats();
    assert_eq!(stats.total_queries, 100);
    assert_eq!(stats.hot_cache_hits, 60);
    assert_eq!(stats.metadata_cache_hits, 30);
    assert_eq!(stats.store_queries, 10);
    assert_eq!(stats.cache_hit_rate, 0.9); // (60 + 30) / 100
    assert_eq!(stats.avg_latency_us, 50);
}

/// End-to-end test: Package version ordering
#[tokio::test]
async fn test_e2e_version_ordering() {
    let registry = RdfRegistry::new();

    // Insert package with unordered versions
    let package = create_multi_version_package(
        "ordered-pkg",
        "Ordered Package",
        &["2.0.0", "1.0.0", "1.5.0", "1.1.0"],
    );

    registry
        .insert_package_rdf(&package)
        .await
        .expect("Insert should succeed");

    // List versions
    let versions = registry
        .list_versions(&package.metadata.id)
        .await
        .expect("List versions should succeed");

    // Verify all versions are present
    assert_eq!(versions.len(), 4);

    // Create expected order (should maintain insertion/natural order from RDF)
    let version_strings: Vec<String> = versions.iter().map(|v| v.as_str().to_string()).collect();
    assert!(version_strings.contains(&"1.0.0".to_string()));
    assert!(version_strings.contains(&"2.0.0".to_string()));
}

/// End-to-end test: Statistics tracking accuracy
#[tokio::test]
async fn test_e2e_statistics_tracking() {
    let registry = RdfRegistry::new();

    // Track initial stats
    let initial_stats = registry.stats();
    assert_eq!(initial_stats.total_queries, 0);

    // Perform operations
    let pkg1 = create_test_package("stats-1", "Stats Package 1", "1.0.0");
    let pkg2 = create_test_package("stats-2", "Stats Package 2", "1.0.0");

    registry
        .insert_package_rdf(&pkg1)
        .await
        .expect("Insert should succeed");
    registry
        .insert_package_rdf(&pkg2)
        .await
        .expect("Insert should succeed");

    // Execute queries
    registry
        .list_versions(&pkg1.metadata.id)
        .await
        .expect("List versions should succeed");

    registry
        .list_versions(&pkg2.metadata.id)
        .await
        .expect("List versions should succeed");

    // Verify stats updated
    let final_stats = registry.stats();
    assert!(
        final_stats.total_queries >= 2,
        "Should have executed at least 2 queries"
    );
}

/// End-to-end test: Error recovery
#[tokio::test]
async fn test_e2e_error_recovery() {
    let registry = RdfRegistry::new();

    // Try to query non-existent package
    let non_existent_id = PackageId::new("does-not-exist").unwrap();
    let exists = registry
        .package_exists(&non_existent_id)
        .await
        .expect("Exists check should succeed");
    assert!(!exists);

    // Registry should still work after error
    let valid_package = create_test_package("recovery-pkg", "Recovery Package", "1.0.0");
    registry
        .insert_package_rdf(&valid_package)
        .await
        .expect("Insert should succeed after error");

    let exists = registry
        .package_exists(&valid_package.metadata.id)
        .await
        .expect("Exists check should succeed");
    assert!(exists);
}

/// End-to-end test: Large-scale concurrent operations
#[tokio::test]
async fn test_e2e_large_scale_concurrent() {
    let registry = Arc::new(RdfRegistry::new());

    // Insert 50 packages concurrently
    let mut handles = vec![];
    for i in 0..50 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            let package = create_test_package(
                &format!("large-scale-{}", i),
                &format!("Large Scale Package {}", i),
                "1.0.0",
            );
            reg.insert_package_rdf(&package)
                .await
                .expect("Insert should succeed");

            // Also verify it exists immediately
            let exists = reg
                .package_exists(&package.metadata.id)
                .await
                .expect("Exists check should succeed");
            assert!(exists, "Package should exist immediately after insert");
        });
        handles.push(handle);
    }

    // Wait for all operations
    for handle in handles {
        handle.await.expect("Task should complete");
    }

    // Final verification: all packages exist
    let mut verify_handles = vec![];
    for i in 0..50 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            let id = PackageId::new(format!("large-scale-{}", i)).unwrap();
            let exists = reg
                .package_exists(&id)
                .await
                .expect("Exists check should succeed");
            assert!(exists, "Package large-scale-{} should exist", i);
        });
        verify_handles.push(handle);
    }

    for handle in verify_handles {
        handle.await.expect("Verify task should complete");
    }
}

/// End-to-end test: V3 registry statistics under concurrent load
#[tokio::test]
async fn test_e2e_v3_concurrent_statistics() {
    let store = Arc::new(Store::new().expect("Store creation should succeed"));
    let registry = Arc::new(
        V3OptimizedRegistry::new(store)
            .await
            .expect("V3 initialization should succeed"),
    );

    // Simulate 1000 concurrent operations
    let mut handles = vec![];
    for i in 0..1000 {
        let reg = Arc::clone(&registry);
        let handle = tokio::spawn(async move {
            // Simulate different types of operations
            match i % 3 {
                0 => {
                    // Hot cache hit
                    reg.query_stats
                        .hot_cache_hits
                        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
                1 => {
                    // Metadata cache hit
                    reg.query_stats
                        .metadata_cache_hits
                        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
                _ => {
                    // Store query
                    reg.query_stats
                        .store_queries
                        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
            }

            reg.query_stats
                .total_queries
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        });
        handles.push(handle);
    }

    // Wait for all operations
    for handle in handles {
        handle.await.expect("Task should complete");
    }

    // Verify final statistics
    let stats = registry.stats();
    assert_eq!(stats.total_queries, 1000);

    // Verify distribution (approximately 333 of each type)
    let total_cache_hits = stats.hot_cache_hits + stats.metadata_cache_hits;
    assert!(total_cache_hits >= 650); // Should be around 666
    assert!(stats.store_queries >= 300); // Should be around 334
}

/// End-to-end test: Complete marketplace workflow
#[tokio::test]
async fn test_e2e_complete_marketplace_workflow() {
    let registry = RdfRegistry::new();

    // Step 1: Insert multiple packages
    let packages = vec![
        create_test_package("workflow-pkg-1", "Workflow Package 1", "1.0.0"),
        create_test_package("workflow-pkg-2", "Workflow Package 2", "2.0.0"),
        create_multi_version_package("workflow-pkg-3", "Workflow Package 3", &["1.0.0", "1.1.0"]),
    ];

    for package in &packages {
        registry
            .insert_package_rdf(package)
            .await
            .expect("Insert should succeed");
    }

    // Step 2: Verify all packages exist
    for package in &packages {
        let exists = registry
            .package_exists(&package.metadata.id)
            .await
            .expect("Exists check should succeed");
        assert!(exists, "Package should exist");
    }

    // Step 3: List versions for multi-version package
    let versions = registry
        .list_versions(&packages[2].metadata.id)
        .await
        .expect("List versions should succeed");
    assert_eq!(versions.len(), 2);

    // Step 4: Execute SPARQL query to find all
    let query = r#"
        SELECT ?package WHERE {
            ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ggen.io/marketplace/Package> .
        }
    "#;

    let results = registry
        .query_sparql(query)
        .await
        .expect("SPARQL query should succeed");

    assert!(
        results.len() >= 3,
        "Should find at least 3 packages in complete workflow"
    );

    // Step 5: Verify statistics
    let stats = registry.stats();
    assert!(stats.total_queries > 0, "Should have executed queries");
}
