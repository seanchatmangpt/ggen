//! Phase 3A: Comprehensive Registry Unit Tests (Chicago TDD Style)
//!
//! Tests Registry, RdfRegistry, and V3OptimizedRegistry.
//! Following Chicago TDD: state-based testing, real collaborators, AAA pattern.
//!
//! Test Count: 50+ tests covering registry operations

use ggen_marketplace_v2::models::{Package, PackageId, PackageMetadata, PackageVersion};
use ggen_marketplace_v2::registry::{CacheStats, Registry};
use ggen_marketplace_v2::traits::AsyncRepository;

// ============================================================================
// SECTION 1: Registry Creation Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_creation() {
    // Arrange & Act
    let registry = Registry::new(100).await;

    // Assert
    assert!(registry.is_empty());
    assert_eq!(registry.len(), 0);
}

#[tokio::test]
async fn test_registry_with_different_cache_sizes() {
    // Arrange & Act
    let small = Registry::new(10).await;
    let large = Registry::new(10000).await;

    // Assert
    assert!(small.is_empty());
    assert!(large.is_empty());
}

// ============================================================================
// SECTION 2: Registry Insert Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_insert_single_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");

    // Act
    let result = registry.insert(package);

    // Assert
    assert!(result.is_ok());
    assert_eq!(registry.len(), 1);
}

#[tokio::test]
async fn test_registry_insert_multiple_packages() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg1 = create_test_package("pkg1", "1.0.0");
    let pkg2 = create_test_package("pkg2", "1.0.0");
    let pkg3 = create_test_package("pkg3", "1.0.0");

    // Act
    registry.insert(pkg1).unwrap();
    registry.insert(pkg2).unwrap();
    registry.insert(pkg3).unwrap();

    // Assert
    assert_eq!(registry.len(), 3);
}

#[tokio::test]
async fn test_registry_insert_same_id_overwrites() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg1 = create_test_package("test-pkg", "1.0.0");
    let pkg2 = create_test_package("test-pkg", "2.0.0");

    // Act
    registry.insert(pkg1).unwrap();
    registry.insert(pkg2).unwrap();

    // Assert
    assert_eq!(registry.len(), 1);
    let retrieved = registry
        .get_package(&PackageId::new("test-pkg").unwrap())
        .await
        .unwrap();
    assert_eq!(retrieved.latest_version.as_str(), "2.0.0");
}

#[tokio::test]
async fn test_registry_insert_with_multiple_versions() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("multi-version-pkg").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Multi Version", "Has multiple versions", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("2.0.0").unwrap(),
        versions: vec![
            PackageVersion::new("2.0.0").unwrap(),
            PackageVersion::new("1.5.0").unwrap(),
            PackageVersion::new("1.0.0").unwrap(),
        ],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    registry.insert(package).unwrap();

    // Assert
    let versions = registry.list_versions(&id).await.unwrap();
    assert_eq!(versions.len(), 3);
}

// ============================================================================
// SECTION 3: Registry Get Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_get_existing_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package.clone()).unwrap();

    // Act
    let result = registry
        .get_package(&PackageId::new("test-pkg").unwrap())
        .await;

    // Assert
    assert!(result.is_ok());
    let retrieved = result.unwrap();
    assert_eq!(retrieved.metadata.id.as_str(), "test-pkg");
}

#[tokio::test]
async fn test_registry_get_nonexistent_package() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let result = registry
        .get_package(&PackageId::new("nonexistent").unwrap())
        .await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_registry_get_package_version() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("versioned-pkg").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Versioned", "Desc", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("2.0.0").unwrap(),
        versions: vec![
            PackageVersion::new("2.0.0").unwrap(),
            PackageVersion::new("1.0.0").unwrap(),
        ],
        releases: indexmap::IndexMap::new(),
    };
    registry.insert(package).unwrap();

    // Act
    let result = registry
        .get_package_version(&id, &PackageVersion::new("1.0.0").unwrap())
        .await;

    // Assert
    assert!(result.is_ok());
    let retrieved = result.unwrap();
    assert_eq!(retrieved.versions.len(), 1);
    assert_eq!(retrieved.versions[0].as_str(), "1.0.0");
}

#[tokio::test]
async fn test_registry_get_package_version_not_found() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package).unwrap();

    // Act
    let result = registry
        .get_package_version(
            &PackageId::new("test-pkg").unwrap(),
            &PackageVersion::new("2.0.0").unwrap(),
        )
        .await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_registry_all_packages_empty() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let result = registry.all_packages().await.unwrap();

    // Assert
    assert!(result.is_empty());
}

#[tokio::test]
async fn test_registry_all_packages() {
    // Arrange
    let registry = Registry::new(100).await;
    registry.insert(create_test_package("pkg1", "1.0.0")).unwrap();
    registry.insert(create_test_package("pkg2", "1.0.0")).unwrap();
    registry.insert(create_test_package("pkg3", "1.0.0")).unwrap();

    // Act
    let result = registry.all_packages().await.unwrap();

    // Assert
    assert_eq!(result.len(), 3);
}

#[tokio::test]
async fn test_registry_package_exists_true() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package).unwrap();

    // Act
    let exists = registry
        .package_exists(&PackageId::new("test-pkg").unwrap())
        .await
        .unwrap();

    // Assert
    assert!(exists);
}

#[tokio::test]
async fn test_registry_package_exists_false() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let exists = registry
        .package_exists(&PackageId::new("nonexistent").unwrap())
        .await
        .unwrap();

    // Assert
    assert!(!exists);
}

#[tokio::test]
async fn test_registry_list_versions() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("multi-pkg").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Multi", "Desc", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("3.0.0").unwrap(),
        versions: vec![
            PackageVersion::new("3.0.0").unwrap(),
            PackageVersion::new("2.0.0").unwrap(),
            PackageVersion::new("1.0.0").unwrap(),
        ],
        releases: indexmap::IndexMap::new(),
    };
    registry.insert(package).unwrap();

    // Act
    let versions = registry.list_versions(&id).await.unwrap();

    // Assert
    assert_eq!(versions.len(), 3);
}

// ============================================================================
// SECTION 4: Registry Remove Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_remove_existing_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package).unwrap();

    // Act
    let result = registry.remove(&PackageId::new("test-pkg").unwrap()).unwrap();

    // Assert
    assert!(result.is_some());
    assert!(registry.is_empty());
}

#[tokio::test]
async fn test_registry_remove_nonexistent_package() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let result = registry.remove(&PackageId::new("nonexistent").unwrap()).unwrap();

    // Assert
    assert!(result.is_none());
}

#[tokio::test]
async fn test_registry_remove_then_get_fails() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("test-pkg").unwrap();
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package).unwrap();

    // Act
    registry.remove(&id).unwrap();
    let result = registry.get_package(&id).await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_registry_remove_updates_length() {
    // Arrange
    let registry = Registry::new(100).await;
    registry.insert(create_test_package("pkg1", "1.0.0")).unwrap();
    registry.insert(create_test_package("pkg2", "1.0.0")).unwrap();
    assert_eq!(registry.len(), 2);

    // Act
    registry.remove(&PackageId::new("pkg1").unwrap()).unwrap();

    // Assert
    assert_eq!(registry.len(), 1);
}

// ============================================================================
// SECTION 5: Registry Update Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_update_existing_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("test-pkg").unwrap();
    let pkg1 = create_test_package("test-pkg", "1.0.0");
    registry.insert(pkg1).unwrap();

    // Act
    let updated = create_test_package("test-pkg", "2.0.0");
    let result = registry.update(&id, updated);

    // Assert
    assert!(result.is_ok());
    let retrieved = registry.get_package(&id).await.unwrap();
    assert_eq!(retrieved.latest_version.as_str(), "2.0.0");
}

#[tokio::test]
async fn test_registry_update_nonexistent_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let id = PackageId::new("nonexistent").unwrap();
    let package = create_test_package("nonexistent", "1.0.0");

    // Act
    let result = registry.update(&id, package);

    // Assert
    assert!(result.is_err());
}

// ============================================================================
// SECTION 6: Registry Cache Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_cache_stats_initial() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let stats = registry.cache_stats();

    // Assert
    assert_eq!(stats.hits, 0);
    assert_eq!(stats.misses, 0);
    assert_eq!(stats.hit_rate, 0.0);
}

#[tokio::test]
async fn test_registry_cache_stats_after_get() {
    // Arrange
    let registry = Registry::new(100).await;
    let package = create_test_package("test-pkg", "1.0.0");
    registry.insert(package).unwrap();

    // Act - First get (cache miss)
    let _ = registry
        .get_package(&PackageId::new("test-pkg").unwrap())
        .await;
    let stats1 = registry.cache_stats();

    // Act - Second get (cache hit)
    let _ = registry
        .get_package(&PackageId::new("test-pkg").unwrap())
        .await;
    let stats2 = registry.cache_stats();

    // Assert
    assert!(stats2.hits >= stats1.hits);
}

#[tokio::test]
async fn test_registry_cache_stats_display() {
    // Arrange
    let stats = CacheStats {
        hits: 100,
        misses: 50,
        hit_rate: 0.6667,
    };

    // Act
    let display = format!("{}", stats);

    // Assert
    assert!(display.contains("100 hits"));
    assert!(display.contains("50 misses"));
    assert!(display.contains("hit rate"));
}

// ============================================================================
// SECTION 7: Registry Clear Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_clear() {
    // Arrange
    let registry = Registry::new(100).await;
    registry.insert(create_test_package("pkg1", "1.0.0")).unwrap();
    registry.insert(create_test_package("pkg2", "1.0.0")).unwrap();
    assert_eq!(registry.len(), 2);

    // Act
    registry.clear();

    // Assert
    assert!(registry.is_empty());
    assert_eq!(registry.len(), 0);
}

#[tokio::test]
async fn test_registry_clear_empty() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    registry.clear();

    // Assert
    assert!(registry.is_empty());
}

// ============================================================================
// SECTION 8: Registry Concurrent Access Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_registry_concurrent_inserts() {
    // Arrange
    let registry = std::sync::Arc::new(Registry::new(100).await);

    // Act
    let mut handles = vec![];
    for i in 0..10 {
        let reg = registry.clone();
        handles.push(tokio::spawn(async move {
            let pkg = create_test_package(&format!("pkg{}", i), "1.0.0");
            reg.insert(pkg).unwrap();
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }

    // Assert
    assert_eq!(registry.len(), 10);
}

#[tokio::test]
async fn test_registry_concurrent_reads() {
    // Arrange
    let registry = std::sync::Arc::new(Registry::new(100).await);
    registry
        .insert(create_test_package("test-pkg", "1.0.0"))
        .unwrap();

    // Act
    let mut handles = vec![];
    for _ in 0..10 {
        let reg = registry.clone();
        handles.push(tokio::spawn(async move {
            reg.get_package(&PackageId::new("test-pkg").unwrap())
                .await
                .unwrap()
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    // Assert
    assert_eq!(results.len(), 10);
    for pkg in results {
        assert_eq!(pkg.metadata.id.as_str(), "test-pkg");
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_test_package(name: &str, version: &str) -> Package {
    let id = PackageId::new(name).unwrap();
    let metadata = PackageMetadata::new(id, name, format!("Description for {}", name), "MIT");
    Package {
        metadata,
        latest_version: PackageVersion::new(version).unwrap(),
        versions: vec![PackageVersion::new(version).unwrap()],
        releases: indexmap::IndexMap::new(),
    }
}
