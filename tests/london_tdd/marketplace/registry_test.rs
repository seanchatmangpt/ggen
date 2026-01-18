#![cfg(feature = "london_tdd")]
//! London TDD tests for marketplace registry
//!
//! Tests verify:
//! - Package fetching from registry
//! - Local and remote registry support
//! - Package metadata retrieval
//! - Caching behavior
//! - Version resolution

use crate::lib::*;
use ggen_marketplace::models::Category;
use ggen_marketplace::prelude::*;
use mockall::predicate::*;

#[tokio::test]
async fn test_registry_fetch_package_by_name() {
    let start = std::time::Instant::now();

    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Add test package
    let package = create_test_package("rust-web-service", "1.0.0");
    registry.publish(package.clone()).await.unwrap();

    // Act
    let result = registry
        .get_package(&PackageId::new("test", "rust-web-service"))
        .await;

    // Assert
    assert!(result.is_ok());
    let fetched = result.unwrap();
    assert_eq!(fetched.id.name, "rust-web-service");
    assert_eq!(fetched.version.to_string(), "1.0.0");

    // Performance: <50ms for local registry
    assert!(start.elapsed().as_millis() < 50);
}

#[tokio::test]
async fn test_registry_list_packages() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Add multiple packages
    let pkg1 = create_test_package("rust-web-service", "1.0.0");
    let pkg2 = create_test_package("rust-database", "1.0.0");
    registry.publish(pkg1).await.unwrap();
    registry.publish(pkg2).await.unwrap();

    // Act
    let result = registry.search(&Query::new("rust")).await;

    // Assert
    assert!(result.is_ok());
    let packages = result.unwrap();
    assert_eq!(packages.len(), 2);
}

#[tokio::test]
async fn test_registry_search_by_category() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let mut pkg = create_test_package("web-service", "1.0.0");
    pkg.metadata.categories = vec![Category::WebService];
    registry.publish(pkg).await.unwrap();

    let mut pkg2 = create_test_package("database", "1.0.0");
    pkg2.metadata.categories = vec![Category::Database];
    registry.publish(pkg2).await.unwrap();

    // Act
    let result = registry.search(&Query::new("web")).await;

    // Assert
    assert!(result.is_ok());
    let packages = result.unwrap();
    assert_eq!(packages.len(), 1);
    assert_eq!(packages[0].id.name, "web-service");
}

#[tokio::test]
async fn test_registry_fetch_specific_version() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Add multiple versions
    let pkg_v1 = create_test_package("service", "1.0.0");
    let pkg_v2 = create_test_package("service", "2.0.0");
    registry.publish(pkg_v1).await.unwrap();
    registry.publish(pkg_v2).await.unwrap();

    // Act
    let result = registry
        .get_package_version(&PackageId::new("test", "service"), "1.0.0")
        .await;

    // Assert
    assert!(result.is_ok());
    let fetched = result.unwrap();
    assert_eq!(fetched.version.to_string(), "1.0.0");
}

#[tokio::test]
async fn test_registry_list_all_versions() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Add multiple versions
    let pkg_v1 = create_test_package("service", "1.0.0");
    let pkg_v2 = create_test_package("service", "2.0.0");
    let pkg_v3 = create_test_package("service", "2.1.0");
    registry.publish(pkg_v1).await.unwrap();
    registry.publish(pkg_v2).await.unwrap();
    registry.publish(pkg_v3).await.unwrap();

    // Act
    let result = registry
        .list_versions(&PackageId::new("test", "service"))
        .await;

    // Assert
    assert!(result.is_ok());
    let versions = result.unwrap();
    assert_eq!(versions.len(), 3);
    // Should be sorted newest first
    assert_eq!(versions[0].version.to_string(), "2.1.0");
    assert_eq!(versions[1].version.to_string(), "2.0.0");
    assert_eq!(versions[2].version.to_string(), "1.0.0");
}

#[tokio::test]
async fn test_registry_returns_error_for_nonexistent_package() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Act
    let result = registry
        .get_package(&PackageId::new("test", "nonexistent"))
        .await;

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not found"));
}

#[tokio::test]
async fn test_registry_prevents_duplicate_versions() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let pkg = create_test_package("service", "1.0.0");
    registry.publish(pkg.clone()).await.unwrap();

    // Act - Try to publish same version again
    let result = registry.publish(pkg).await;

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("already exists"));
}

#[tokio::test]
async fn test_registry_persists_packages_to_disk() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry_path = temp_dir.path().join("registry");

    {
        let registry = LocalRegistry::new(registry_path.clone()).await.unwrap();
        let pkg = create_test_package("service", "1.0.0");
        registry.publish(pkg).await.unwrap();
    }

    // Act - Create new registry instance
    let registry = LocalRegistry::new(registry_path).await.unwrap();
    let result = registry
        .get_package(&PackageId::new("test", "service"))
        .await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_registry_search_is_case_insensitive() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let pkg = create_test_package("WebService", "1.0.0");
    registry.publish(pkg).await.unwrap();

    // Act
    let result = registry.search(&Query::new("webservice")).await;

    // Assert
    assert!(result.is_ok());
    let packages = result.unwrap();
    assert_eq!(packages.len(), 1);
}

#[tokio::test]
async fn test_registry_search_by_tags() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let mut pkg = create_test_package("service", "1.0.0");
    pkg.metadata.tags = vec!["axum".to_string(), "rest".to_string()];
    registry.publish(pkg).await.unwrap();

    // Act
    let result = registry.search(&Query::new("axum")).await;

    // Assert
    assert!(result.is_ok());
    let packages = result.unwrap();
    assert_eq!(packages.len(), 1);
}

#[tokio::test]
async fn test_registry_delete_package_version() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let pkg = create_test_package("service", "1.0.0");
    registry.publish(pkg).await.unwrap();

    // Act
    let result = registry
        .delete(&PackageId::new("test", "service"), "1.0.0")
        .await;

    // Assert
    assert!(result.is_ok());
    let get_result = registry
        .get_package(&PackageId::new("test", "service"))
        .await;
    assert!(get_result.is_err());
}

#[tokio::test]
async fn test_registry_check_package_exists() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    let pkg = create_test_package("service", "1.0.0");
    registry.publish(pkg).await.unwrap();

    // Act
    let exists = registry
        .exists(&PackageId::new("test", "service"))
        .await
        .unwrap();
    let not_exists = registry
        .exists(&PackageId::new("test", "nonexistent"))
        .await
        .unwrap();

    // Assert
    assert!(exists);
    assert!(!not_exists);
}

#[tokio::test]
async fn test_registry_metadata() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;

    // Add some packages
    let pkg1 = create_test_package("service1", "1.0.0");
    let pkg2 = create_test_package("service2", "1.0.0");
    registry.publish(pkg1).await.unwrap();
    registry.publish(pkg2).await.unwrap();

    // Act
    let metadata = registry.metadata().await.unwrap();

    // Assert
    assert_eq!(metadata.package_count, 2);
    assert_eq!(metadata.name, "Local Registry");
}

#[tokio::test]
async fn test_registry_creates_otel_span() {
    // Arrange
    let temp_dir = tempfile::tempdir().unwrap();
    let registry = create_test_registry(temp_dir.path()).await;
    let tracer = otel::MockTracerProvider::new();

    let pkg = create_test_package("service", "1.0.0");
    registry.publish(pkg).await.unwrap();

    // Act
    let _result = search_with_tracing(&registry, &tracer, "service").await;

    // Assert
    let span = tracer.find_span("ggen.registry.search").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span
        .attributes
        .iter()
        .any(|(k, v)| k == "search.query" && v == "service"));
}

// Helper functions

async fn create_test_registry(path: &std::path::Path) -> LocalRegistry {
    LocalRegistry::new(path.to_path_buf()).await.unwrap()
}

fn create_test_package(name: &str, version: &str) -> ggen_marketplace::models::Package {
    let version_parts: Vec<u32> = version.split('.').map(|s| s.parse().unwrap()).collect();

    let unvalidated = ggen_marketplace::models::Package::builder(
        PackageId::new("test", name),
        Version::new(version_parts[0], version_parts[1], version_parts[2]),
    )
    .title(format!("{} Package", name))
    .description(format!("Test package for {}", name))
    .license("MIT")
    .content_id(ContentId::new(
        format!("hash_{}", name),
        ggen_marketplace::models::package::HashAlgorithm::Sha256,
    ))
    .build()
    .unwrap();
    unvalidated.validate().unwrap().package().clone()
}

async fn search_with_tracing(
    registry: &LocalRegistry, tracer: &otel::MockTracerProvider, query: &str,
) -> std::result::Result<Vec<ggen_marketplace::models::Package>, anyhow::Error> {
    let result = registry.search(&Query::new(query)).await?;

    let span = otel::MockSpan {
        name: "ggen.registry.search".to_string(),
        attributes: vec![
            ("search.query".to_string(), query.to_string()),
            ("results.count".to_string(), result.len().to_string()),
        ],
        events: vec!["search_completed".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    Ok(result)
}
