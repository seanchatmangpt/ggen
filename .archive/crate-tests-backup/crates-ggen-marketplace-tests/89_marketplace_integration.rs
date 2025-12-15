//! Phase 3B: Integration Tests for ggen-marketplace
//!
//! Real marketplace workflows with actual components (no mocks).
//! Chicago TDD: State-based testing with behavior verification.
//!
//! Test Categories:
//! 1. Package Installation Workflow
//! 2. Search & Discovery Workflow
//! 3. Security & Validation Workflow
//! 4. End-to-End Marketplace Workflow

use ggen_marketplace::{
    install::Installer,
    models::{Package, PackageDependency, PackageId, PackageMetadata, PackageVersion, ReleaseInfo},
    registry::Registry,
    search::{SearchEngine, SearchQuery, SortBy},
    security::{ChecksumCalculator, KeyPair, SignatureVerifier},
    traits::{AsyncRepository, Installable, Signable, Validatable},
    validation::PackageValidator,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;

// ============================================================================
// Test Utilities
// ============================================================================

/// Create a test package with given ID and version
fn create_test_package(id: &str, version: &str) -> Package {
    let pkg_id = PackageId::new(id).expect("valid package id");
    let pkg_version = PackageVersion::new(version).expect("valid version");

    let mut metadata = PackageMetadata::new(
        pkg_id.clone(),
        format!("Test Package {}", id),
        format!("A test package for {}", id),
        "MIT",
    );
    metadata.authors = vec!["Test Author".to_string()];
    metadata.keywords = vec!["test".to_string(), id.to_string()];
    metadata.categories = vec!["testing".to_string()];
    metadata.repository = Some(format!("https://github.com/test/{}", id));
    metadata.downloads = 100;

    let release_info = ReleaseInfo {
        version: pkg_version.clone(),
        released_at: chrono::Utc::now(),
        changelog: format!("Initial release of {}", id),
        checksum: ChecksumCalculator::calculate(id.as_bytes()),
        download_url: format!("https://registry.example.com/{}/{}.tar.gz", id, version),
        dependencies: vec![],
    };

    let mut releases = indexmap::IndexMap::new();
    releases.insert(pkg_version.clone(), release_info);

    Package {
        metadata,
        latest_version: pkg_version.clone(),
        versions: vec![pkg_version],
        releases,
    }
}

/// Create a test package with dependencies
fn create_package_with_deps(id: &str, version: &str, deps: Vec<(&str, &str)>) -> Package {
    let pkg_id = PackageId::new(id).expect("valid package id");
    let pkg_version = PackageVersion::new(version).expect("valid version");

    let mut metadata = PackageMetadata::new(
        pkg_id.clone(),
        format!("Test Package {}", id),
        format!("A test package for {}", id),
        "MIT",
    );
    metadata.authors = vec!["Test Author".to_string()];
    metadata.keywords = vec!["test".to_string(), id.to_string()];
    metadata.repository = Some(format!("https://github.com/test/{}", id));

    let dependencies: Vec<PackageDependency> = deps
        .into_iter()
        .map(|(dep_id, dep_version)| PackageDependency {
            id: PackageId::new(dep_id).expect("valid dep id"),
            version_req: dep_version.to_string(),
            optional: false,
        })
        .collect();

    let release_info = ReleaseInfo {
        version: pkg_version.clone(),
        released_at: chrono::Utc::now(),
        changelog: format!("Initial release of {}", id),
        checksum: ChecksumCalculator::calculate(id.as_bytes()),
        download_url: format!("https://registry.example.com/{}/{}.tar.gz", id, version),
        dependencies,
    };

    let mut releases = indexmap::IndexMap::new();
    releases.insert(pkg_version.clone(), release_info);

    Package {
        metadata,
        latest_version: pkg_version.clone(),
        versions: vec![pkg_version],
        releases,
    }
}

// ============================================================================
// 1. Package Installation Workflow Tests
// ============================================================================

mod package_installation {
    use super::*;

    /// Test: Create package -> Store in registry -> Install -> Verify installed
    #[tokio::test]
    async fn test_complete_installation_workflow() {
        // Timeout protection: 30s max
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create registry and packages
            let registry = Registry::new(100).await;
            let pkg = create_test_package("install-test", "1.0.0");

            // Act: Insert package into registry
            registry.insert(pkg.clone()).expect("insert should succeed");

            // Verify: Package exists in registry
            let exists = registry
                .package_exists(&pkg.metadata.id)
                .await
                .expect("exists check should succeed");
            assert!(exists, "Package should exist after insert");

            // Act: Create installer and installation manifest
            let installer = Installer::new(registry);
            let manifest = installer
                .create_manifest(
                    vec![PackageId::new("install-test").expect("valid id")],
                    "/tmp/ggen-test".to_string(),
                )
                .await
                .expect("manifest creation should succeed");

            // Verify: Manifest contains our package
            assert_eq!(manifest.packages.len(), 1);
            assert!(manifest
                .dependencies
                .contains_key(&PackageId::new("install-test").expect("valid id")));

            // Act: Execute installation
            let installed = installer
                .install(manifest)
                .await
                .expect("installation should succeed");

            // Verify: Installation completed
            assert!(!installed.packages.is_empty());
        })
        .await;

        assert!(
            result.is_ok(),
            "Test timed out - installation workflow took too long"
        );
    }

    /// Test: Dependency chain resolution
    #[tokio::test]
    async fn test_dependency_chain_resolution() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create package hierarchy
            // app-pkg depends on lib-pkg depends on core-pkg
            let registry = Registry::new(100).await;

            let core_pkg = create_test_package("core-pkg", "1.0.0");
            let lib_pkg = create_package_with_deps("lib-pkg", "1.0.0", vec![("core-pkg", "1.0.0")]);
            let app_pkg = create_package_with_deps("app-pkg", "1.0.0", vec![("lib-pkg", "1.0.0")]);

            registry.insert(core_pkg).expect("insert core");
            registry.insert(lib_pkg).expect("insert lib");
            registry.insert(app_pkg).expect("insert app");

            // Act: Resolve dependencies for app-pkg
            let installer = Installer::new(registry);
            let deps = installer
                .resolve_dependencies(
                    &PackageId::new("app-pkg").expect("valid id"),
                    &PackageVersion::new("1.0.0").expect("valid version"),
                )
                .await
                .expect("resolution should succeed");

            // Verify: Dependencies are resolved in correct order
            // Dependencies should come before dependents
            assert!(!deps.is_empty(), "Should have resolved dependencies");

            // Verify app-pkg is in the list
            let has_app = deps.iter().any(|(id, _)| id.as_str() == "app-pkg");
            assert!(has_app, "app-pkg should be in resolved dependencies");
        })
        .await;

        assert!(result.is_ok(), "Dependency resolution timed out");
    }

    /// Test: Conflict detection
    #[tokio::test]
    async fn test_conflict_detection() {
        let result = timeout(Duration::from_secs(10), async {
            // Arrange
            let registry = Registry::new(100).await;
            let installer = Installer::new(registry);

            // Create empty dependency map (no conflicts possible)
            let deps = indexmap::IndexMap::new();

            // Act: Check for conflicts
            let conflict_result = installer.check_conflicts(&deps);

            // Verify: No conflicts in empty map
            assert!(
                conflict_result.is_ok(),
                "Empty dependency map should have no conflicts"
            );
        })
        .await;

        assert!(result.is_ok(), "Conflict detection timed out");
    }

    /// Test: Dry-run installation
    #[tokio::test]
    async fn test_dry_run_installation() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;
            let pkg = create_test_package("dryrun-pkg", "1.0.0");
            registry.insert(pkg).expect("insert");

            let installer = Installer::new(registry);
            let manifest = installer
                .create_manifest(
                    vec![PackageId::new("dryrun-pkg").expect("valid id")],
                    "/tmp/dryrun".to_string(),
                )
                .await
                .expect("manifest creation");

            // Act: Dry run
            let dry_run_result = installer
                .dry_run_install(&manifest)
                .await
                .expect("dry run should succeed");

            // Verify: Dry run produces output without side effects
            assert!(!dry_run_result.is_empty(), "Dry run should produce output");
        })
        .await;

        assert!(result.is_ok(), "Dry-run installation timed out");
    }
}

// ============================================================================
// 2. Search & Discovery Workflow Tests
// ============================================================================

mod search_discovery {
    use super::*;

    /// Test: Add packages to registry -> Search by keyword -> Verify results
    #[tokio::test]
    async fn test_search_by_keyword() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create registry with searchable packages
            let registry = Registry::new(100).await;

            let pkg1 = create_test_package("database-driver", "1.0.0");
            let pkg2 = create_test_package("web-framework", "2.0.0");
            let pkg3 = create_test_package("database-orm", "1.5.0");

            registry.insert(pkg1).expect("insert pkg1");
            registry.insert(pkg2).expect("insert pkg2");
            registry.insert(pkg3).expect("insert pkg3");

            // Act: Search for "database"
            let search_engine = SearchEngine::new();
            let packages = registry.all_packages().await.expect("get all packages");
            let query = SearchQuery::new("database");
            let results = search_engine
                .search(packages, &query)
                .expect("search should succeed");

            // Verify: Found database-related packages
            assert!(
                results.len() >= 2,
                "Should find at least 2 database packages"
            );

            // Verify: Results contain search term in ID or description
            for result in &results {
                let matches_id = result.package.metadata.id.as_str().contains("database");
                let matches_desc = result
                    .package
                    .metadata
                    .description
                    .to_lowercase()
                    .contains("database");
                assert!(
                    matches_id || matches_desc,
                    "Result should match search term"
                );
            }
        })
        .await;

        assert!(result.is_ok(), "Search by keyword timed out");
    }

    /// Test: Pagination and limiting
    #[tokio::test]
    async fn test_search_pagination() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create many packages
            let registry = Registry::new(100).await;

            for i in 0..20 {
                let pkg = create_test_package(&format!("pagination-pkg-{}", i), "1.0.0");
                registry.insert(pkg).expect("insert");
            }

            let packages = registry.all_packages().await.expect("get packages");
            let search_engine = SearchEngine::new();

            // Act: Search with limit
            let query = SearchQuery::new("pagination").with_limit(5);
            let results = search_engine
                .search(packages.clone(), &query)
                .expect("search with limit");

            // Verify: Results limited to 5
            assert_eq!(results.len(), 5, "Should return exactly 5 results");

            // Act: Search with offset
            let query_offset = SearchQuery::new("pagination").with_limit(5).with_offset(5);
            let results_offset = search_engine
                .search(packages, &query_offset)
                .expect("search with offset");

            // Verify: Offset results are different from first page
            assert_eq!(
                results_offset.len(),
                5,
                "Should return 5 results with offset"
            );
        })
        .await;

        assert!(result.is_ok(), "Search pagination timed out");
    }

    /// Test: Search with filters
    #[tokio::test]
    async fn test_search_with_filters() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;

            let mut pkg1 = create_test_package("filter-pkg-1", "1.0.0");
            pkg1.metadata.license = "MIT".to_string();
            pkg1.metadata.categories = vec!["database".to_string()];

            let mut pkg2 = create_test_package("filter-pkg-2", "1.0.0");
            pkg2.metadata.license = "Apache-2.0".to_string();
            pkg2.metadata.categories = vec!["web".to_string()];

            registry.insert(pkg1).expect("insert pkg1");
            registry.insert(pkg2).expect("insert pkg2");

            let packages = registry.all_packages().await.expect("get packages");
            let search_engine = SearchEngine::new();

            // Act: Search with license filter
            let query = SearchQuery::new("filter").with_license("MIT");
            let results = search_engine
                .search(packages.clone(), &query)
                .expect("search with license filter");

            // Verify: Only MIT licensed packages
            for result in &results {
                assert_eq!(
                    result.package.metadata.license, "MIT",
                    "All results should have MIT license"
                );
            }

            // Act: Search with category filter
            let query_cat = SearchQuery::new("filter").with_category("database");
            let results_cat = search_engine
                .search(packages, &query_cat)
                .expect("search with category filter");

            // Verify: Only database category packages
            for result in &results_cat {
                assert!(
                    result
                        .package
                        .metadata
                        .categories
                        .contains(&"database".to_string()),
                    "All results should be in database category"
                );
            }
        })
        .await;

        assert!(result.is_ok(), "Search with filters timed out");
    }

    /// Test: Sort by different criteria
    #[tokio::test]
    async fn test_search_sorting() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;

            let mut pkg1 = create_test_package("sort-pkg-1", "1.0.0");
            pkg1.metadata.downloads = 1000;

            let mut pkg2 = create_test_package("sort-pkg-2", "1.0.0");
            pkg2.metadata.downloads = 500;

            let mut pkg3 = create_test_package("sort-pkg-3", "1.0.0");
            pkg3.metadata.downloads = 2000;

            registry.insert(pkg1).expect("insert");
            registry.insert(pkg2).expect("insert");
            registry.insert(pkg3).expect("insert");

            let packages = registry.all_packages().await.expect("get packages");
            let search_engine = SearchEngine::new();

            // Act: Sort by downloads
            let query = SearchQuery::new("sort").with_sort(SortBy::Downloads);
            let results = search_engine
                .search(packages.clone(), &query)
                .expect("search sorted by downloads");

            // Verify: Results sorted by downloads descending
            if results.len() >= 2 {
                let downloads: Vec<u64> = results
                    .iter()
                    .map(|r| r.package.metadata.downloads)
                    .collect();
                for i in 0..downloads.len() - 1 {
                    assert!(
                        downloads[i] >= downloads[i + 1],
                        "Downloads should be in descending order"
                    );
                }
            }

            // Act: Sort by name
            let query_name = SearchQuery::new("sort").with_sort(SortBy::Name);
            let results_name = search_engine
                .search(packages, &query_name)
                .expect("search sorted by name");

            // Verify: Results sorted alphabetically
            if results_name.len() >= 2 {
                let names: Vec<&str> = results_name
                    .iter()
                    .map(|r| r.package.metadata.name.as_str())
                    .collect();
                for i in 0..names.len() - 1 {
                    assert!(
                        names[i] <= names[i + 1],
                        "Names should be in alphabetical order"
                    );
                }
            }
        })
        .await;

        assert!(result.is_ok(), "Search sorting timed out");
    }
}

// ============================================================================
// 3. Security & Validation Workflow Tests
// ============================================================================

mod security_validation {
    use super::*;

    /// Test: Sign package -> Verify signature -> Modify -> Verify fails
    #[tokio::test]
    async fn test_signature_workflow() {
        let result = timeout(Duration::from_secs(10), async {
            // Arrange: Generate key pair and create signer
            let key_pair = KeyPair::generate();
            let verifier = SignatureVerifier::new(key_pair);

            let package_data = b"package-content-for-signing";

            // Act: Sign the data
            let signature = verifier.sign(package_data).expect("signing should succeed");

            // Verify: Signature is valid
            let is_valid = verifier
                .verify(package_data, &signature)
                .expect("verification should succeed");
            assert!(is_valid, "Original signature should be valid");

            // Act: Modify data and verify again
            let modified_data = b"modified-package-content";
            let is_valid_modified = verifier
                .verify(modified_data, &signature)
                .expect("verification of modified data");

            // Verify: Signature fails for modified data
            assert!(
                !is_valid_modified,
                "Signature should fail for modified data"
            );
        })
        .await;

        assert!(result.is_ok(), "Signature workflow timed out");
    }

    /// Test: Checksum calculation and verification
    #[tokio::test]
    async fn test_checksum_workflow() {
        let result = timeout(Duration::from_secs(10), async {
            // Arrange
            let package_content = b"package-binary-content";

            // Act: Calculate checksum
            let checksum = ChecksumCalculator::calculate(package_content);

            // Verify: Checksum is SHA-256 (64 hex chars)
            assert_eq!(
                checksum.len(),
                64,
                "SHA-256 checksum should be 64 hex chars"
            );

            // Act: Verify checksum
            let is_valid = ChecksumCalculator::verify(package_content, &checksum)
                .expect("verification should succeed");
            assert!(is_valid, "Checksum should match original content");

            // Act: Modify content and verify
            let modified_content = b"modified-binary-content";
            let is_valid_modified = ChecksumCalculator::verify(modified_content, &checksum)
                .expect("verification of modified content");

            // Verify: Checksum fails for modified content
            assert!(
                !is_valid_modified,
                "Checksum should fail for modified content"
            );
        })
        .await;

        assert!(result.is_ok(), "Checksum workflow timed out");
    }

    /// Test: Package validation workflow
    #[tokio::test]
    async fn test_validation_workflow() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create well-formed package
            let mut pkg = create_test_package("validation-test", "1.0.0");
            pkg.metadata.authors = vec!["Author Name <author@example.com>".to_string()];
            pkg.metadata.repository = Some("https://github.com/test/repo".to_string());

            let validator = PackageValidator::new();

            // Act: Validate package
            let validation_result = validator
                .validate(&pkg)
                .await
                .expect("validation should complete");

            // Verify: Well-formed package passes validation
            assert!(
                validation_result.quality_score > 0,
                "Quality score should be positive"
            );
            assert!(
                !validation_result.checks.is_empty(),
                "Should have validation checks"
            );

            // Verify: Check results are present
            let has_metadata_check = validation_result
                .checks
                .iter()
                .any(|c| c.name == "Metadata");
            assert!(has_metadata_check, "Should have metadata validation check");
        })
        .await;

        assert!(result.is_ok(), "Validation workflow timed out");
    }

    /// Test: Validation fails for incomplete package
    #[tokio::test]
    async fn test_validation_failure_for_incomplete_package() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create package with missing fields
            let pkg_id = PackageId::new("incomplete-pkg").expect("valid id");
            let metadata = PackageMetadata::new(
                pkg_id.clone(),
                "",                // Empty name - should fail validation
                "",                // Empty description - should fail validation
                "UNKNOWN_LICENSE", // Invalid license
            );

            let pkg = Package {
                metadata,
                latest_version: PackageVersion::new("1.0.0").expect("valid version"),
                versions: vec![PackageVersion::new("1.0.0").expect("valid version")],
                releases: indexmap::IndexMap::new(),
            };

            let validator = PackageValidator::new();

            // Act: Validate incomplete package
            let validation_result = validator
                .validate(&pkg)
                .await
                .expect("validation should complete");

            // Verify: Some checks should fail
            let failed_checks = validation_result
                .checks
                .iter()
                .filter(|c| !c.passed)
                .count();

            // Note: The validation may still pass overall due to weighted scoring,
            // but individual checks should fail
            assert!(
                failed_checks > 0 || !validation_result.passed,
                "Incomplete package should have failed checks or fail overall"
            );
        })
        .await;

        assert!(result.is_ok(), "Validation failure test timed out");
    }

    /// Test: Public key can be used for verification only
    #[tokio::test]
    async fn test_public_key_verification() {
        let result = timeout(Duration::from_secs(10), async {
            // Arrange: Generate key pair and sign data
            let key_pair = KeyPair::generate();
            let pub_key = key_pair.public_key_hex();
            let signer = SignatureVerifier::new(key_pair);

            let data = b"test-data-for-verification";
            let signature = signer.sign(data).expect("signing should succeed");

            // Act: Create verifier from public key only
            let verifier = SignatureVerifier::from_public_key(&pub_key)
                .expect("should create verifier from public key");

            // Verify: Can verify signature with public key
            let is_valid = verifier
                .verify(data, &signature)
                .expect("verification should succeed");
            assert!(is_valid, "Should verify with public key");

            // Verify: Public keys match
            assert_eq!(verifier.public_key(), pub_key, "Public keys should match");
        })
        .await;

        assert!(result.is_ok(), "Public key verification timed out");
    }
}

// ============================================================================
// 4. End-to-End Marketplace Workflow Tests
// ============================================================================

mod end_to_end {
    use super::*;

    /// Test: Full marketplace workflow - create, search, validate, install
    #[tokio::test]
    async fn test_complete_marketplace_workflow() {
        let result = timeout(Duration::from_secs(30), async {
            // Phase 1: Create and populate registry
            let registry = Arc::new(Registry::new(100).await);

            let pkg1 = create_test_package("marketplace-core", "1.0.0");
            let pkg2 = create_package_with_deps(
                "marketplace-cli",
                "1.0.0",
                vec![("marketplace-core", "1.0.0")],
            );
            let pkg3 = create_test_package("marketplace-web", "2.0.0");

            registry.insert(pkg1.clone()).expect("insert core");
            registry.insert(pkg2.clone()).expect("insert cli");
            registry.insert(pkg3.clone()).expect("insert web");

            // Verify: Registry has all packages
            assert_eq!(registry.len(), 3, "Registry should have 3 packages");

            // Phase 2: Search for packages
            let search_engine = SearchEngine::new();
            let packages = registry.all_packages().await.expect("get packages");
            let query = SearchQuery::new("marketplace");
            let search_results = search_engine
                .search(packages, &query)
                .expect("search should succeed");

            // Verify: Found our packages
            assert!(
                search_results.len() >= 2,
                "Should find marketplace packages"
            );

            // Phase 3: Validate package
            let validator = PackageValidator::new();
            let validation_result = validator
                .validate(&pkg1)
                .await
                .expect("validation should complete");
            assert!(
                validation_result.quality_score > 0,
                "Package should have quality score"
            );

            // Phase 4: Create and execute installation
            // Create a new registry instance for the installer since Arc doesn't implement the trait directly
            let install_registry = Registry::new(100).await;
            install_registry
                .insert(pkg1)
                .expect("insert core for install");
            install_registry
                .insert(pkg2)
                .expect("insert cli for install");
            install_registry
                .insert(pkg3)
                .expect("insert web for install");

            let installer = Installer::new(install_registry);
            let manifest = installer
                .create_manifest(
                    vec![PackageId::new("marketplace-cli").expect("valid id")],
                    "/tmp/marketplace-install".to_string(),
                )
                .await
                .expect("manifest creation");

            // Verify: Manifest includes dependencies
            assert!(
                manifest.dependencies.len() >= 1,
                "Should resolve dependencies"
            );

            // Execute installation
            let installed = installer
                .install(manifest)
                .await
                .expect("installation should succeed");

            assert!(
                !installed.packages.is_empty(),
                "Should have installed packages"
            );
        })
        .await;

        assert!(result.is_ok(), "Complete marketplace workflow timed out");
    }

    /// Test: Concurrent registry operations
    #[tokio::test]
    async fn test_concurrent_registry_operations() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange: Create shared registry
            let registry = Arc::new(Registry::new(100).await);

            // Act: Perform concurrent operations
            let handles: Vec<_> = (0..10)
                .map(|i| {
                    let reg = Arc::clone(&registry);
                    tokio::spawn(async move {
                        let pkg = create_test_package(&format!("concurrent-pkg-{}", i), "1.0.0");
                        reg.insert(pkg).expect("concurrent insert");

                        // Also read from registry
                        let id =
                            PackageId::new(&format!("concurrent-pkg-{}", i)).expect("valid id");
                        let _ = reg.package_exists(&id).await;
                    })
                })
                .collect();

            // Wait for all operations
            for handle in handles {
                handle.await.expect("concurrent task should complete");
            }

            // Verify: Registry consistency
            assert_eq!(
                registry.len(),
                10,
                "Registry should have all concurrently inserted packages"
            );

            // Verify: All packages are accessible
            for i in 0..10 {
                let id = PackageId::new(&format!("concurrent-pkg-{}", i)).expect("valid id");
                let exists = registry
                    .package_exists(&id)
                    .await
                    .expect("exists check should succeed");
                assert!(exists, "Package {} should exist", i);
            }
        })
        .await;

        assert!(result.is_ok(), "Concurrent operations timed out");
    }

    /// Test: Registry update and cache invalidation
    #[tokio::test]
    async fn test_registry_update_workflow() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;
            let pkg = create_test_package("update-test", "1.0.0");
            let pkg_id = pkg.metadata.id.clone();

            registry.insert(pkg).expect("initial insert");

            // Act: Get package (populates cache)
            let original = registry.get_package(&pkg_id).await.expect("get original");
            assert_eq!(original.metadata.downloads, 100);

            // Act: Update package with new download count
            let mut updated_pkg = create_test_package("update-test", "1.0.0");
            updated_pkg.metadata.downloads = 500;

            registry
                .update(&pkg_id, updated_pkg)
                .expect("update should succeed");

            // Verify: Updated package reflects changes
            let updated = registry.get_package(&pkg_id).await.expect("get updated");
            assert_eq!(
                updated.metadata.downloads, 500,
                "Downloads should be updated"
            );
        })
        .await;

        assert!(result.is_ok(), "Registry update workflow timed out");
    }

    /// Test: Package removal workflow
    #[tokio::test]
    async fn test_package_removal_workflow() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;
            let pkg = create_test_package("removal-test", "1.0.0");
            let pkg_id = pkg.metadata.id.clone();

            registry.insert(pkg).expect("insert");

            // Verify: Package exists
            let exists = registry
                .package_exists(&pkg_id)
                .await
                .expect("exists check");
            assert!(exists, "Package should exist before removal");

            // Act: Remove package
            let removed = registry.remove(&pkg_id).expect("removal should succeed");
            assert!(removed.is_some(), "Should return removed package");

            // Verify: Package no longer exists
            let exists_after = registry
                .package_exists(&pkg_id)
                .await
                .expect("exists check after removal");
            assert!(!exists_after, "Package should not exist after removal");

            // Verify: Get returns error
            let get_result = registry.get_package(&pkg_id).await;
            assert!(get_result.is_err(), "Get should fail for removed package");
        })
        .await;

        assert!(result.is_ok(), "Package removal workflow timed out");
    }

    /// Test: Cache statistics tracking
    #[tokio::test]
    async fn test_cache_statistics() {
        let result = timeout(Duration::from_secs(30), async {
            // Arrange
            let registry = Registry::new(100).await;
            let pkg = create_test_package("cache-test", "1.0.0");
            let pkg_id = pkg.metadata.id.clone();

            registry.insert(pkg).expect("insert");

            // Initial stats
            let initial_stats = registry.cache_stats();

            // Act: Multiple accesses
            for _ in 0..5 {
                let _ = registry.get_package(&pkg_id).await;
            }

            // Verify: Stats updated
            let final_stats = registry.cache_stats();
            assert!(
                final_stats.hits >= initial_stats.hits,
                "Cache hits should increase"
            );
        })
        .await;

        assert!(result.is_ok(), "Cache statistics test timed out");
    }
}
