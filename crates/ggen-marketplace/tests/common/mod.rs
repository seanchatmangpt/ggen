//! Common Test Utilities
//!
//! Shared helper functions and test fixtures for ggen-marketplace tests.
//! Follows the 80/20 principle: provide high-value helpers that eliminate boilerplate.

use ggen_marketplace::prelude::*;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Registry Helpers
// ============================================================================

/// Create a local registry with temporary storage.
/// Returns the registry and the temp directory (keep it alive for test duration).
pub async fn setup_local_registry() -> (LocalRegistry, TempDir) {
    let temp_dir = tempfile::tempdir().expect("failed to create temp dir");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path)
        .await
        .expect("failed to create registry");
    (registry, temp_dir)
}

// ============================================================================
// Storage Helpers
// ============================================================================

/// Create a filesystem store with temporary storage.
/// Returns the store and the temp directory (keep it alive for test duration).
pub async fn setup_filesystem_store() -> (FilesystemStore, TempDir) {
    let temp_dir = tempfile::tempdir().expect("failed to create temp dir");
    let storage_path = temp_dir.path().join("storage");
    let store = FilesystemStore::new(storage_path)
        .await
        .expect("failed to create storage");
    (store, temp_dir)
}

/// Create an in-memory store for fast testing.
pub fn setup_memory_store() -> MemoryStore {
    MemoryStore::new()
}

// ============================================================================
// Package Builders
// ============================================================================

/// Create a test package with sensible defaults.
///
/// # Arguments
/// * `name` - Package name (namespace will be "test")
/// * `version` - Version string (e.g., "1.0.0")
///
/// # Example
/// ```
/// let pkg = create_test_package("my-package", "1.0.0")?;
/// ```
/// Create a validated test package for use in tests.
///
/// **Root Cause Fix**: Returns validated Package instead of UnvalidatedPackage.
/// This prevents tests from accessing fields directly without validation.
///
/// # Example
/// ```
/// let pkg = create_test_package("my-package", "1.0.0")?;
/// ```
pub fn create_test_package(name: &str, version: &str) -> Result<Package> {
    let version_parts: Vec<&str> = version.split('.').collect();
    let major = version_parts[0].parse().unwrap_or(1);
    let minor = version_parts.get(1).and_then(|v| v.parse().ok()).unwrap_or(0);
    let patch = version_parts.get(2).and_then(|v| v.parse().ok()).unwrap_or(0);

    let unvalidated = Package::builder(
        PackageId::new("test", name),
        Version::new(major, minor, patch),
    )
    .title(format!("Test Package {}", name))
    .description(format!("Test description for {}", name))
    .license("MIT")
    .tag("test")
    .content_id(ContentId::new(
        format!("hash_{}", name),
        HashAlgorithm::Sha256,
    ))
    .build()?;
    
    // Validate package before returning (Poka-yoke: ensures package meets requirements)
    let validated = unvalidated.validate()?;
    Ok(validated.package().clone())
}

/// Create a test package with custom metadata.
///
/// # Example
/// ```
/// let pkg = create_custom_package(
///     "my-package",
///     "1.0.0",
///     "Custom Title",
///     "Custom description",
///     "Apache-2.0",
/// )?;
/// ```
pub fn create_custom_package(
    name: &str,
    version: &str,
    title: &str,
    description: &str,
    license: &str,
) -> Result<Package> {
    let version_parts: Vec<&str> = version.split('.').collect();
    let major = version_parts[0].parse().unwrap_or(1);
    let minor = version_parts.get(1).and_then(|v| v.parse().ok()).unwrap_or(0);
    let patch = version_parts.get(2).and_then(|v| v.parse().ok()).unwrap_or(0);

    let unvalidated = Package::builder(
        PackageId::new("test", name),
        Version::new(major, minor, patch),
    )
    .title(title)
    .description(description)
    .license(license)
    .content_id(ContentId::new(
        format!("hash_{}_{}", name, version),
        HashAlgorithm::Sha256,
    ))
    .build()?;

    // Validate package before returning (Poka-yoke: ensures package meets requirements)
    let validated = unvalidated.validate()?;
    Ok(validated.package().clone())
}

/// Create multiple test packages at once.
///
/// # Example
/// ```
/// let packages = create_test_packages(&[
///     ("web-framework", "1.0.0"),
///     ("cli-tool", "2.0.0"),
///     ("database", "1.5.0"),
/// ])?;
/// ```
pub fn create_test_packages(specs: &[(&str, &str)]) -> Result<Vec<Package>> {
    specs
        .iter()
        .map(|(name, version)| create_test_package(name, version))
        .collect()
}

// ============================================================================
// Content Helpers
// ============================================================================

/// Generate test content of specified size.
pub fn generate_test_content(size: usize) -> Vec<u8> {
    vec![0u8; size]
}

/// Generate test content with pattern (useful for detecting corruption).
pub fn generate_patterned_content(size: usize) -> Vec<u8> {
    (0..size).map(|i| (i % 256) as u8).collect()
}

// ============================================================================
// Assertion Helpers
// ============================================================================

/// Assert that two packages are equal (compares all fields).
pub fn assert_package_eq(actual: &Package, expected: &Package, context: &str) {
    assert_eq!(actual.id, expected.id, "{}: ID mismatch", context);
    assert_eq!(actual.version, expected.version, "{}: Version mismatch", context);
    assert_eq!(
        actual.metadata.title, expected.metadata.title,
        "{}: Title mismatch", context
    );
    assert_eq!(
        actual.metadata.description, expected.metadata.description,
        "{}: Description mismatch", context
    );
    assert_eq!(
        actual.metadata.license, expected.metadata.license,
        "{}: License mismatch", context
    );
}

/// Assert that a package matches expected values.
pub fn assert_package_has(
    package: &Package,
    expected_name: &str,
    expected_version: &str,
    context: &str,
) {
    assert_eq!(
        package.id.name, expected_name,
        "{}: Name mismatch", context
    );

    let expected_ver = parse_version(expected_version);
    assert_eq!(
        package.version, expected_ver,
        "{}: Version mismatch", context
    );
}

/// Parse version string into Version struct.
fn parse_version(version_str: &str) -> Version {
    let parts: Vec<&str> = version_str.split('.').collect();
    let major = parts[0].parse().unwrap_or(0);
    let minor = parts.get(1).and_then(|v| v.parse().ok()).unwrap_or(0);
    let patch = parts.get(2).and_then(|v| v.parse().ok()).unwrap_or(0);
    Version::new(major, minor, patch)
}

// ============================================================================
// Test Fixtures
// ============================================================================

/// Standard test packages for use across tests.
pub struct TestFixtures {
    pub packages: Vec<Package>,
}

impl TestFixtures {
    /// Create standard test fixtures with common packages.
    pub fn new() -> Result<Self> {
        let packages = create_test_packages(&[
            ("web-framework", "1.0.0"),
            ("web-server", "1.5.0"),
            ("cli-tool", "2.0.0"),
            ("database", "1.0.0"),
            ("logging", "0.5.0"),
        ])?;

        Ok(Self { packages })
    }

    /// Get a package by name.
    pub fn get(&self, name: &str) -> Option<&Package> {
        self.packages.iter().find(|p| p.id.name == name)
    }
}

impl Default for TestFixtures {
    fn default() -> Self {
        Self::new().expect("failed to create test fixtures")
    }
}

// ============================================================================
// Async Test Helpers
// ============================================================================

/// Publish multiple packages to a registry.
pub async fn publish_packages<R: Registry>(
    registry: &R,
    packages: &[Package],
) -> Result<()> {
    for package in packages {
        registry.publish(package.clone()).await?;
    }
    Ok(())
}

/// Store multiple contents in a store, return their IDs.
pub async fn store_contents<S: PackageStore>(
    store: &S,
    contents: &[&[u8]],
) -> Result<Vec<ContentId>> {
    let mut ids = Vec::new();
    for content in contents {
        let id = store.store(content).await?;
        ids.push(id);
    }
    Ok(ids)
}

// ============================================================================
// Performance Helpers
// ============================================================================

use std::time::{Duration, Instant};

/// Measure execution time of an async operation.
///
/// # Example
/// ```
/// let elapsed = measure_async(|| async {
///     registry.search(&Query::new("test")).await
/// }).await;
///
/// assert!(elapsed < Duration::from_millis(100), "Too slow");
/// ```
pub async fn measure_async<F, Fut, T>(f: F) -> Duration
where
    F: FnOnce() -> Fut,
    Fut: std::future::Future<Output = T>,
{
    let start = Instant::now();
    let _ = f().await;
    start.elapsed()
}

/// Assert that an async operation completes within a time limit.
pub async fn assert_fast_async<F, Fut, T>(
    f: F,
    max_duration: Duration,
    operation: &str,
) -> T
where
    F: FnOnce() -> Fut,
    Fut: std::future::Future<Output = T>,
{
    let start = Instant::now();
    let result = f().await;
    let elapsed = start.elapsed();

    assert!(
        elapsed <= max_duration,
        "{} took {:?}, expected <= {:?}",
        operation,
        elapsed,
        max_duration
    );

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_helper_setup_local_registry() {
        let (registry, _temp) = setup_local_registry().await;
        // Should be able to search immediately
        let result = registry.search(&Query::new("test")).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_helper_create_package() {
        let pkg = create_test_package("example", "1.2.3").unwrap();
        assert_eq!(pkg.id.name, "example");
        assert_eq!(pkg.version, Version::new(1, 2, 3));
    }

    #[test]
    fn test_helper_fixtures() {
        let fixtures = TestFixtures::new().unwrap();
        assert_eq!(fixtures.packages.len(), 5);
        assert!(fixtures.get("web-framework").is_some());
    }
}
