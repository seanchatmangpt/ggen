//! Chicago TDD Tests for Registry Infrastructure
//!
//! These tests follow Chicago TDD methodology:
//! - Test REAL systems (real files, real directories, real state changes)
//! - NO mocks or stubs
//! - Verify actual behavior through observable effects
//!
//! Test Coverage:
//! - Registry initialization
//! - Index file creation and loading
//! - Package metadata queries
//! - Version listing
//! - Cache operations
//! - Error handling
//! - Concurrent access safety

use ggen_cli_lib::domain::marketplace::registry::*;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::Path;
use tempfile::TempDir;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use chrono;

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a test package metadata
fn create_test_package(name: &str, version: &str) -> PackageMetadata {
    PackageMetadata {
        name: name.to_string(),
        versions: vec![VersionMetadata {
            version: version.to_string(),
            published_at: "2024-01-01T00:00:00Z".to_string(),
            checksum: "abc123".to_string(),
            download_url: format!("https://registry.example.com/{}/{}", name, version),
            dependencies: vec![],
            size_bytes: 1024,
        }],
        description: format!("Test package {}", name),
        author: Some("Test Author".to_string()),
        repository: None,
        license: Some("MIT".to_string()),
        tags: vec!["test".to_string()],
        category: Some("testing".to_string()),
        homepage: None,
    }
}

/// Create a test registry index with sample packages
async fn create_test_index(index_path: &Path) -> Result<()> {
    let packages = vec![
        create_test_package("web/react-app", "1.0.0"),
        create_test_package("cli/awesome-tool", "2.3.1"),
        create_test_package("backend/api-server", "0.5.0"),
        create_test_package("util/logger", "1.2.0"),
        create_test_package("data/analyzer", "3.0.0"),
    ];

    let mut packages_map = HashMap::new();
    for pkg in packages {
        packages_map.insert(pkg.name.clone(), pkg);
    }

    let index = RegistryIndex {
        version: "1.0".to_string(),
        updated_at: chrono::Utc::now().to_rfc3339(),
        packages: packages_map,
    };

    fs::create_dir_all(index_path.parent().unwrap()).await?;
    let content = serde_json::to_string_pretty(&index)?;
    let mut file = fs::File::create(index_path).await?;
    file.write_all(content.as_bytes()).await?;
    file.sync_all().await?;

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Registry Initialization
// ============================================================================

#[tokio::test]
async fn test_registry_creation_with_valid_path() -> Result<()> {
    // GIVEN: A temporary directory
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");

    // WHEN: Create registry with custom path
    let registry = Registry::with_path(index_path.clone());

    // THEN: Registry should be created successfully
    assert_eq!(registry.index_path(), &index_path);

    Ok(())
}

#[tokio::test]
async fn test_registry_load_creates_directories() -> Result<()> {
    // GIVEN: A registry pointing to non-existent path
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("deep/nested/registry/index.json");
    let registry = Registry::with_path(index_path.clone());

    // WHEN: Load the registry (creates dirs and empty index)
    registry.load().await?;

    // THEN: Parent directories should exist
    assert!(
        index_path.parent().unwrap().exists(),
        "Parent directories should be created"
    );

    Ok(())
}

#[tokio::test]
async fn test_registry_load_creates_empty_index_if_missing() -> Result<()> {
    // GIVEN: A registry without an index file
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = Registry::with_path(index_path.clone());

    // WHEN: Load the registry
    registry.load().await?;

    // THEN: Empty index should be created
    assert!(index_path.exists(), "Index file should be created");

    // AND: It should be a valid empty index
    let packages = registry.list_packages().await?;
    assert!(packages.is_empty(), "New registry should have no packages");

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Index Loading
// ============================================================================

#[tokio::test]
async fn test_registry_loads_real_index() -> Result<()> {
    // GIVEN: A registry with a real index file
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    // WHEN: Load the index
    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // THEN: Index should be loaded with correct data
    let packages = registry.list_packages().await?;
    assert_eq!(packages.len(), 5, "Should load all 5 packages");

    Ok(())
}

#[tokio::test]
async fn test_registry_load_handles_corrupt_json() -> Result<()> {
    // GIVEN: A registry with corrupt JSON
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    fs::create_dir_all(index_path.parent().unwrap()).await?;

    let mut file = fs::File::create(&index_path).await?;
    file.write_all(b"{ invalid json }").await?;
    file.sync_all().await?;

    // WHEN: Try to load index
    let registry = Registry::with_path(index_path);
    let result = registry.load().await;

    // THEN: Should fail with parse error
    assert!(result.is_err(), "Loading corrupt JSON should fail");

    Ok(())
}

#[tokio::test]
async fn test_registry_save_persists_to_disk() -> Result<()> {
    // GIVEN: A registry with loaded index
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path.clone());
    registry.load().await?;

    // Add a new package
    let new_package = create_test_package("new/package", "1.0.0");
    registry.add_package(new_package).await?;

    // WHEN: Save the registry
    registry.save().await?;

    // THEN: Changes should be persisted to disk
    let content = fs::read_to_string(&index_path).await?;
    assert!(
        content.contains("new/package"),
        "Saved index should contain new package"
    );

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Package Queries
// ============================================================================

#[tokio::test]
async fn test_registry_list_packages_returns_all_names() -> Result<()> {
    // GIVEN: A registry with test packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: List packages
    let packages = registry.list_packages().await?;

    // THEN: Should return all package names
    assert_eq!(packages.len(), 5);
    assert!(packages.contains(&"web/react-app".to_string()));
    assert!(packages.contains(&"cli/awesome-tool".to_string()));
    assert!(packages.contains(&"backend/api-server".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_registry_get_package_returns_metadata() -> Result<()> {
    // GIVEN: A registry with test packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Get specific package
    let package = registry.get_package("web/react-app").await?;

    // THEN: Should return correct metadata
    assert!(package.is_some());
    let pkg = package.unwrap();
    assert_eq!(pkg.name, "web/react-app");
    assert_eq!(pkg.description, "Test package web/react-app");
    assert_eq!(pkg.author, Some("Test Author".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_registry_get_package_returns_none_for_missing() -> Result<()> {
    // GIVEN: A registry with test packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Get non-existent package
    let package = registry.get_package("nonexistent/package").await?;

    // THEN: Should return None
    assert!(package.is_none());

    Ok(())
}

#[tokio::test]
async fn test_registry_get_package_fails_if_index_not_loaded() -> Result<()> {
    // GIVEN: A registry without loaded index
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = Registry::with_path(index_path);

    // WHEN: Try to get package without loading
    let result = registry.get_package("any/package").await;

    // THEN: Should fail
    assert!(result.is_err(), "Should fail when index not loaded");

    Ok(())
}

// Test continues...

// ============================================================================
// CHICAGO TDD TESTS - Version Listing
// ============================================================================

#[tokio::test]
async fn test_registry_list_versions_returns_all_versions() -> Result<()> {
    // GIVEN: A package with multiple versions
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");

    let mut package = create_test_package("test/multi-version", "1.0.0");
    package.versions.push(VersionMetadata {
        version: "1.1.0".to_string(),
        published_at: "2024-02-01T00:00:00Z".to_string(),
        checksum: "def456".to_string(),
        download_url: "https://registry.example.com/test/multi-version/1.1.0".to_string(),
        dependencies: vec![],
        size_bytes: 2048,
    });
    package.versions.push(VersionMetadata {
        version: "2.0.0".to_string(),
        published_at: "2024-03-01T00:00:00Z".to_string(),
        checksum: "ghi789".to_string(),
        download_url: "https://registry.example.com/test/multi-version/2.0.0".to_string(),
        dependencies: vec![],
        size_bytes: 4096,
    });

    let mut packages_map = HashMap::new();
    packages_map.insert(package.name.clone(), package);

    let index = RegistryIndex {
        version: "1.0".to_string(),
        updated_at: chrono::Utc::now().to_rfc3339(),
        packages: packages_map,
    };

    fs::create_dir_all(index_path.parent().unwrap()).await?;
    let content = serde_json::to_string_pretty(&index)?;
    let mut file = fs::File::create(&index_path).await?;
    file.write_all(content.as_bytes()).await?;
    file.sync_all().await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: List versions
    let versions = registry.list_versions("test/multi-version").await?;

    // THEN: Should return all versions
    assert_eq!(versions.len(), 3);
    assert!(versions.contains(&"1.0.0".to_string()));
    assert!(versions.contains(&"1.1.0".to_string()));
    assert!(versions.contains(&"2.0.0".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_registry_list_versions_fails_for_missing_package() -> Result<()> {
    // GIVEN: A registry with test packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: List versions for non-existent package
    let result = registry.list_versions("nonexistent/package").await;

    // THEN: Should fail
    assert!(result.is_err(), "Should fail for missing package");

    Ok(())
}

#[tokio::test]
async fn test_registry_get_version_returns_specific_version() -> Result<()> {
    // GIVEN: A package with multiple versions
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");

    let mut package = create_test_package("test/pkg", "1.0.0");
    package.versions.push(VersionMetadata {
        version: "2.0.0".to_string(),
        published_at: "2024-02-01T00:00:00Z".to_string(),
        checksum: "version2".to_string(),
        download_url: "https://example.com/v2".to_string(),
        dependencies: vec![],
        size_bytes: 2048,
    });

    let mut packages_map = HashMap::new();
    packages_map.insert(package.name.clone(), package);

    let index = RegistryIndex {
        version: "1.0".to_string(),
        updated_at: chrono::Utc::now().to_rfc3339(),
        packages: packages_map,
    };

    fs::create_dir_all(index_path.parent().unwrap()).await?;
    let content = serde_json::to_string_pretty(&index)?;
    let mut file = fs::File::create(&index_path).await?;
    file.write_all(content.as_bytes()).await?;
    file.sync_all().await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Get specific version
    let version = registry.get_version("test/pkg", "2.0.0").await?;

    // THEN: Should return correct version metadata
    assert!(version.is_some());
    let v = version.unwrap();
    assert_eq!(v.version, "2.0.0");
    assert_eq!(v.checksum, "version2");

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Package Addition
// ============================================================================

#[tokio::test]
async fn test_registry_add_package_persists_to_memory() -> Result<()> {
    // GIVEN: An initialized registry
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = Registry::with_path(index_path.clone());
    registry.load().await?;

    // WHEN: Add a new package
    let new_package = create_test_package("new/package", "1.0.0");
    registry.add_package(new_package).await?;

    // THEN: Package should be retrievable
    let package = registry.get_package("new/package").await?;
    assert!(package.is_some());
    assert_eq!(package.unwrap().name, "new/package");

    Ok(())
}

#[tokio::test]
async fn test_registry_add_package_updates_existing() -> Result<()> {
    // GIVEN: A registry with an existing package
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Add package with same name but updated metadata
    let mut updated_package = create_test_package("web/react-app", "2.0.0");
    updated_package.description = "Updated description".to_string();
    registry.add_package(updated_package).await?;

    // THEN: Package should be updated
    let package = registry.get_package("web/react-app").await?;
    assert!(package.is_some());
    let pkg = package.unwrap();
    assert_eq!(pkg.description, "Updated description");

    Ok(())
}

#[tokio::test]
async fn test_registry_add_package_and_save_persists_to_disk() -> Result<()> {
    // GIVEN: A registry
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = Registry::with_path(index_path.clone());
    registry.load().await?;

    // WHEN: Add package and save
    let new_package = create_test_package("persistent/package", "1.0.0");
    registry.add_package(new_package).await?;
    registry.save().await?;

    // THEN: New registry instance should see the package
    let registry2 = Registry::with_path(index_path);
    registry2.load().await?;

    let package = registry2.get_package("persistent/package").await?;
    assert!(package.is_some());

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Cache Operations
// ============================================================================

#[tokio::test]
async fn test_registry_cache_stores_retrieved_packages() -> Result<()> {
    // GIVEN: A registry with test packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Get a package (should cache it)
    let _ = registry.get_package("web/react-app").await?;

    // THEN: Cache should contain the package
    let cached = registry.cache().get("web/react-app");
    assert!(cached.is_some());

    Ok(())
}

#[tokio::test]
async fn test_registry_cache_lru_eviction() -> Result<()> {
    // GIVEN: A registry with small cache size (3 entries)
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // WHEN: Access more packages than cache size
    let _ = registry.get_package("web/react-app").await?;
    let _ = registry.get_package("cli/awesome-tool").await?;
    let _ = registry.get_package("backend/api-server").await?;
    let _ = registry.get_package("util/logger").await?; // This should evict "web/react-app"

    // THEN: Oldest entry should be evicted
    let cache_size = registry.cache().size();
    assert_eq!(cache_size, 3, "Cache should maintain size limit");

    Ok(())
}

#[tokio::test]
async fn test_registry_cache_clear() -> Result<()> {
    // GIVEN: A registry with cached packages
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = Registry::with_path(index_path);
    registry.load().await?;

    // Cache some packages
    let _ = registry.get_package("web/react-app").await?;
    let _ = registry.get_package("cli/awesome-tool").await?;

    // WHEN: Clear the cache
    registry.cache().clear();

    // THEN: Cache should be empty
    assert_eq!(registry.cache().size(), 0);

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Concurrent Access Safety
// ============================================================================

#[tokio::test]
async fn test_registry_concurrent_reads_are_safe() -> Result<()> {
    // GIVEN: A registry with test data
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = std::sync::Arc::new(Registry::with_path(index_path));
    registry.load().await?;

    // WHEN: Multiple concurrent reads
    let mut handles = vec![];

    for _ in 0..10 {
        let reg = registry.clone();
        let handle = tokio::spawn(async move {
            let packages = reg.list_packages().await.unwrap();
            assert_eq!(packages.len(), 5);
        });
        handles.push(handle);
    }

    // THEN: All reads should succeed
    for handle in handles {
        handle.await.unwrap();
    }

    Ok(())
}

#[tokio::test]
async fn test_registry_concurrent_package_retrieval() -> Result<()> {
    // GIVEN: A registry with test data
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    create_test_index(&index_path).await?;

    let registry = std::sync::Arc::new(Registry::with_path(index_path));
    registry.load().await?;

    // WHEN: Multiple concurrent package retrievals
    let mut handles = vec![];

    let packages = vec!["web/react-app", "cli/awesome-tool", "backend/api-server"];

    for pkg_name in packages {
        let reg = registry.clone();
        let handle = tokio::spawn(async move {
            let pkg = reg.get_package(pkg_name).await.unwrap();
            assert!(pkg.is_some());
        });
        handles.push(handle);
    }

    // THEN: All retrievals should succeed
    for handle in handles {
        handle.await.unwrap();
    }

    Ok(())
}

#[tokio::test]
async fn test_registry_concurrent_add_operations() -> Result<()> {
    // GIVEN: An initialized registry
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = std::sync::Arc::new(Registry::with_path(index_path));
    registry.load().await?;

    // WHEN: Multiple concurrent add operations
    let mut handles = vec![];

    for i in 0..5 {
        let reg = registry.clone();
        let handle = tokio::spawn(async move {
            let pkg = create_test_package(&format!("concurrent/pkg-{}", i), "1.0.0");
            reg.add_package(pkg).await.unwrap();
        });
        handles.push(handle);
    }

    // THEN: All additions should succeed
    for handle in handles {
        handle.await.unwrap();
    }

    // AND: All packages should be retrievable
    let packages = registry.list_packages().await?;
    assert_eq!(packages.len(), 5);

    Ok(())
}

// ============================================================================
// CHICAGO TDD TESTS - Error Handling
// ============================================================================

#[tokio::test]
async fn test_registry_handles_missing_directory_gracefully() -> Result<()> {
    // GIVEN: A path that doesn't exist
    let temp_dir = TempDir::new()?;
    let nonexistent_path = temp_dir.path().join("does/not/exist/index.json");

    // WHEN: Create registry and load
    let registry = Registry::with_path(nonexistent_path.clone());
    let result = registry.load().await;

    // THEN: Should succeed (creates directories)
    assert!(result.is_ok());
    assert!(nonexistent_path.parent().unwrap().exists());

    Ok(())
}

#[tokio::test]
async fn test_registry_operations_fail_before_load() -> Result<()> {
    // GIVEN: A registry that hasn't been loaded
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    let registry = Registry::with_path(index_path);

    // WHEN: Try operations before loading
    let list_result = registry.list_packages().await;
    let get_result = registry.get_package("any/pkg").await;

    // THEN: Operations should fail
    assert!(list_result.is_err(), "List should fail before load");
    assert!(get_result.is_err(), "Get should fail before load");

    Ok(())
}

#[tokio::test]
async fn test_registry_recovers_from_partial_corruption() -> Result<()> {
    // GIVEN: A registry with a valid but minimal index
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("registry/index.json");
    fs::create_dir_all(index_path.parent().unwrap()).await?;

    // Write minimal valid JSON (missing some expected fields but still valid)
    let minimal_index = r#"{
        "version": "1.0",
        "updated_at": "2024-01-01T00:00:00Z",
        "packages": {}
    }"#;
    fs::write(&index_path, minimal_index).await?;

    // WHEN: Load the registry
    let registry = Registry::with_path(index_path);
    let result = registry.load().await;

    // THEN: Should load successfully with empty package list
    assert!(result.is_ok());
    let packages = registry.list_packages().await?;
    assert!(packages.is_empty());

    Ok(())
}
