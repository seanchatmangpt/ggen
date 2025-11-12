//! Chicago TDD tests for marketplace domain logic
//!
//! Tests the actual domain layer with real collaborators (no mocks).
//! Focus: LocalRegistry operations with real filesystem.

use tempfile::TempDir;
use tokio;

// Import domain logic (not CLI)
use ggen_marketplace::backend::local::LocalRegistry;
use ggen_marketplace::models::{
    Category, ContentId, HashAlgorithm, Package, PackageId, PackageMetadata, PackageStats, Version,
};
use ggen_marketplace::traits::Registry;

/// Test 1: LocalRegistry creation and initialization
#[tokio::test]
async fn test_local_registry_creation() {
    // GIVEN: A temporary directory
    let temp_dir = TempDir::new().unwrap();

    // WHEN: Creating a new LocalRegistry
    let result = LocalRegistry::new(temp_dir.path().to_path_buf()).await;

    // THEN: Registry should be created successfully
    assert!(
        result.is_ok(),
        "Failed to create LocalRegistry: {:?}",
        result.err()
    );

    // AND: Registry directory should exist
    assert!(temp_dir.path().exists(), "Registry directory should exist");
}

/// Test 2: Package storage and retrieval workflow
#[tokio::test]
async fn test_package_add_and_retrieve() {
    // GIVEN: A local registry
    let temp_dir = TempDir::new().unwrap();
    let registry = LocalRegistry::new(temp_dir.path().to_path_buf())
        .await
        .expect("Failed to create registry");

    // AND: A test package
    let package_id = PackageId::new("test", "my-package");
    let version = Version::new(1, 0, 0);
    let content_id = ContentId::new("abc123", HashAlgorithm::Sha256);

    let package = Package {
        id: package_id.clone(),
        version: version.clone(),
        metadata: PackageMetadata {
            title: "Test Package".to_string(),
            description: "A test package for Chicago TDD".to_string(),
            long_description: None,
            categories: vec![Category::Testing],
            tags: vec!["test".to_string()],
            license: "MIT".to_string(),
            authors: vec![],
            homepage: None,
            repository: Some("https://github.com/test/repo".to_string()),
            documentation: None,
            readme: None,
            changelog: None,
            custom_fields: std::collections::HashMap::new(),
        },
        content_id,
        dependencies: vec![],
        stats: PackageStats::default(),
        created_at: chrono::Utc::now(),
        updated_at: chrono::Utc::now(),
    };

    // WHEN: Adding the package to the registry
    let add_result = registry.add_package(package.clone()).await;

    // THEN: Package should be added successfully
    assert!(
        add_result.is_ok(),
        "Failed to add package: {:?}",
        add_result.err()
    );

    // AND: Package should be searchable
    let query = ggen_marketplace::models::Query::new("Test Package");
    let search_results = registry.search(&query).await;
    assert!(search_results.is_ok(), "Failed to search for package");

    let results = search_results.unwrap();
    assert!(results.len() > 0, "Package not found in search");
    assert_eq!(results[0].id, package_id, "Package ID mismatch in search");
}

/// Test 3: Search functionality with real filesystem
#[tokio::test]
async fn test_search_by_name() {
    // GIVEN: Registry with packages
    let temp_dir = TempDir::new().unwrap();
    let registry = LocalRegistry::new(temp_dir.path().to_path_buf())
        .await
        .expect("Failed to create registry");

    // Add test packages
    let package1 = create_test_package("org1", "web-server", "1.0.0", "Web server package");
    let package2 = create_test_package("org1", "web-client", "1.0.0", "Web client package");
    let package3 = create_test_package("org2", "database", "1.0.0", "Database package");

    registry
        .add_package(package1)
        .await
        .expect("Failed to add package1");
    registry
        .add_package(package2)
        .await
        .expect("Failed to add package2");
    registry
        .add_package(package3)
        .await
        .expect("Failed to add package3");

    // WHEN: Searching for "web" packages
    let query = ggen_marketplace::models::Query::new("web");
    let results = registry.search(&query).await.expect("Search failed");

    // THEN: Should find web-related packages
    assert!(
        results.len() >= 2,
        "Should find at least 2 web packages, found {}",
        results.len()
    );
}

// Helper function to create test packages with correct structure
fn create_test_package(namespace: &str, name: &str, version: &str, description: &str) -> Package {
    let parts: Vec<&str> = version.split('.').collect();
    let version_obj = Version::new(
        parts.get(0).and_then(|s| s.parse().ok()).unwrap_or(1),
        parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0),
        parts.get(2).and_then(|s| s.parse().ok()).unwrap_or(0),
    );

    Package {
        id: PackageId::new(namespace, name),
        version: version_obj,
        metadata: PackageMetadata {
            title: name.to_string(),
            description: description.to_string(),
            long_description: None,
            categories: vec![Category::Testing],
            tags: vec![],
            license: "MIT".to_string(),
            authors: vec![],
            homepage: None,
            repository: None,
            documentation: None,
            readme: None,
            changelog: None,
            custom_fields: std::collections::HashMap::new(),
        },
        content_id: ContentId::new(
            format!("hash-{}-{}", namespace, name),
            HashAlgorithm::Sha256,
        ),
        dependencies: vec![],
        stats: PackageStats::default(),
        created_at: chrono::Utc::now(),
        updated_at: chrono::Utc::now(),
    }
}
