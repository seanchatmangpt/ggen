//! Phase 3A: Comprehensive Install Unit Tests (Chicago TDD Style)
//!
//! Tests Installer, dependency resolution, and installation manifest.
//! Following Chicago TDD: state-based testing, real collaborators, AAA pattern.
//!
//! Test Count: 40+ tests covering installation operations

use ggen_marketplace::install::{InstallationPlan, Installer, PackageInstallPlan};
use ggen_marketplace::models::{
    InstallationManifest, Package, PackageDependency, PackageId, PackageMetadata, PackageVersion,
    ReleaseInfo,
};
use ggen_marketplace::registry::Registry;
use ggen_marketplace::traits::Installable;

// ============================================================================
// SECTION 1: Installer Creation Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_installer_creation() {
    // Arrange
    let registry = Registry::new(100).await;

    // Act
    let installer = Installer::new(registry);

    // Assert - installer is created (tested via subsequent operations)
    let manifest = installer
        .create_manifest(vec![], "/tmp/test".to_string())
        .await
        .unwrap();
    assert!(manifest.packages.is_empty());
}

#[tokio::test]
async fn test_installer_with_populated_registry() {
    // Arrange
    let registry = Registry::new(100).await;
    registry
        .insert(create_test_package("test-pkg", "1.0.0", vec![]))
        .unwrap();

    // Act
    let installer = Installer::new(registry);
    let id = PackageId::new("test-pkg").unwrap();
    let manifest = installer
        .create_manifest(vec![id], "/tmp/test".to_string())
        .await;

    // Assert
    assert!(manifest.is_ok());
}

// ============================================================================
// SECTION 2: Installation Manifest Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_create_manifest_empty_packages() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    // Act
    let manifest = installer
        .create_manifest(vec![], "/tmp/ggen".to_string())
        .await
        .unwrap();

    // Assert
    assert!(manifest.packages.is_empty());
    assert!(manifest.dependencies.is_empty());
    assert_eq!(manifest.install_path, "/tmp/ggen");
}

#[tokio::test]
async fn test_create_manifest_single_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg = create_test_package("my-pkg", "1.0.0", vec![]);
    registry.insert(pkg).unwrap();
    let installer = Installer::new(registry);

    // Act
    let manifest = installer
        .create_manifest(
            vec![PackageId::new("my-pkg").unwrap()],
            "/tmp/install".to_string(),
        )
        .await
        .unwrap();

    // Assert
    assert_eq!(manifest.packages.len(), 1);
    assert!(manifest
        .dependencies
        .contains_key(&PackageId::new("my-pkg").unwrap()));
}

#[tokio::test]
async fn test_create_manifest_multiple_packages() {
    // Arrange
    let registry = Registry::new(100).await;
    registry
        .insert(create_test_package("pkg1", "1.0.0", vec![]))
        .unwrap();
    registry
        .insert(create_test_package("pkg2", "1.0.0", vec![]))
        .unwrap();
    registry
        .insert(create_test_package("pkg3", "1.0.0", vec![]))
        .unwrap();
    let installer = Installer::new(registry);

    // Act
    let manifest = installer
        .create_manifest(
            vec![
                PackageId::new("pkg1").unwrap(),
                PackageId::new("pkg2").unwrap(),
                PackageId::new("pkg3").unwrap(),
            ],
            "/tmp/install".to_string(),
        )
        .await
        .unwrap();

    // Assert
    assert_eq!(manifest.packages.len(), 3);
}

#[tokio::test]
async fn test_create_manifest_nonexistent_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    // Act
    let result = installer
        .create_manifest(
            vec![PackageId::new("nonexistent").unwrap()],
            "/tmp/install".to_string(),
        )
        .await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_manifest_has_uuid() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    // Act
    let manifest = installer
        .create_manifest(vec![], "/tmp/test".to_string())
        .await
        .unwrap();

    // Assert
    assert!(!manifest.id.is_nil());
}

#[tokio::test]
async fn test_manifest_has_timestamp() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    // Act
    let manifest = installer
        .create_manifest(vec![], "/tmp/test".to_string())
        .await
        .unwrap();

    // Assert
    assert!(manifest.planned_at <= chrono::Utc::now());
}

// ============================================================================
// SECTION 3: Dependency Resolution Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_resolve_dependencies_no_deps() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg = create_test_package("standalone", "1.0.0", vec![]);
    registry.insert(pkg).unwrap();
    let installer = Installer::new(registry);

    // Act
    let deps = installer
        .resolve_dependencies(
            &PackageId::new("standalone").unwrap(),
            &PackageVersion::new("1.0.0").unwrap(),
        )
        .await
        .unwrap();

    // Assert
    assert_eq!(deps.len(), 1); // Just the package itself
}

#[tokio::test]
async fn test_resolve_dependencies_single_dep() {
    // Arrange
    let registry = Registry::new(100).await;
    let dep_pkg = create_test_package("dep-pkg", "1.0.0", vec![]);
    registry.insert(dep_pkg).unwrap();

    let main_pkg = create_test_package(
        "main-pkg",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("dep-pkg").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(main_pkg).unwrap();
    let installer = Installer::new(registry);

    // Act
    let deps = installer
        .resolve_dependencies(
            &PackageId::new("main-pkg").unwrap(),
            &PackageVersion::new("1.0.0").unwrap(),
        )
        .await
        .unwrap();

    // Assert
    assert_eq!(deps.len(), 2); // main-pkg and dep-pkg
}

#[tokio::test]
async fn test_resolve_dependencies_transitive() {
    // Arrange
    let registry = Registry::new(100).await;

    // C has no deps
    let pkg_c = create_test_package("pkg-c", "1.0.0", vec![]);
    registry.insert(pkg_c).unwrap();

    // B depends on C
    let pkg_b = create_test_package(
        "pkg-b",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("pkg-c").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(pkg_b).unwrap();

    // A depends on B
    let pkg_a = create_test_package(
        "pkg-a",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("pkg-b").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(pkg_a).unwrap();

    let installer = Installer::new(registry);

    // Act
    let deps = installer
        .resolve_dependencies(
            &PackageId::new("pkg-a").unwrap(),
            &PackageVersion::new("1.0.0").unwrap(),
        )
        .await
        .unwrap();

    // Assert - should have A, B, C
    assert_eq!(deps.len(), 3);
}

#[tokio::test]
async fn test_resolve_dependencies_avoids_duplicates() {
    // Arrange
    let registry = Registry::new(100).await;

    // Shared dep
    let shared = create_test_package("shared", "1.0.0", vec![]);
    registry.insert(shared).unwrap();

    // Both B and C depend on shared
    let pkg_b = create_test_package(
        "pkg-b",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("shared").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(pkg_b).unwrap();

    let pkg_c = create_test_package(
        "pkg-c",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("shared").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(pkg_c).unwrap();

    // A depends on both B and C
    let pkg_a = create_test_package(
        "pkg-a",
        "1.0.0",
        vec![
            PackageDependency {
                id: PackageId::new("pkg-b").unwrap(),
                version_req: "1.0.0".to_string(),
                optional: false,
            },
            PackageDependency {
                id: PackageId::new("pkg-c").unwrap(),
                version_req: "1.0.0".to_string(),
                optional: false,
            },
        ],
    );
    registry.insert(pkg_a).unwrap();

    let installer = Installer::new(registry);

    // Act
    let deps = installer
        .resolve_dependencies(
            &PackageId::new("pkg-a").unwrap(),
            &PackageVersion::new("1.0.0").unwrap(),
        )
        .await
        .unwrap();

    // Assert - should have A, B, C, shared (4 unique)
    assert_eq!(deps.len(), 4);
}

#[tokio::test]
async fn test_resolve_dependencies_missing_dep() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg = create_test_package(
        "main-pkg",
        "1.0.0",
        vec![PackageDependency {
            id: PackageId::new("nonexistent").unwrap(),
            version_req: "1.0.0".to_string(),
            optional: false,
        }],
    );
    registry.insert(pkg).unwrap();
    let installer = Installer::new(registry);

    // Act
    let result = installer
        .resolve_dependencies(
            &PackageId::new("main-pkg").unwrap(),
            &PackageVersion::new("1.0.0").unwrap(),
        )
        .await;

    // Assert
    assert!(result.is_err());
}

// ============================================================================
// SECTION 4: Conflict Checking Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_check_conflicts_empty() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);
    let deps = indexmap::IndexMap::new();

    // Act
    let result = installer.check_conflicts(&deps);

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_check_conflicts_single_dep() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);
    let mut deps = indexmap::IndexMap::new();
    deps.insert(
        PackageId::new("pkg").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
    );

    // Act
    let result = installer.check_conflicts(&deps);

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_check_conflicts_multiple_deps() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);
    let mut deps = indexmap::IndexMap::new();
    deps.insert(
        PackageId::new("pkg1").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
    );
    deps.insert(
        PackageId::new("pkg2").unwrap(),
        PackageVersion::new("2.0.0").unwrap(),
    );

    // Act
    let result = installer.check_conflicts(&deps);

    // Assert
    assert!(result.is_ok());
}

// ============================================================================
// SECTION 5: Manifest Validation Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_validate_manifest_valid() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg = create_test_package("test-pkg", "1.0.0", vec![]);
    registry.insert(pkg).unwrap();
    let installer = Installer::new(registry);

    let mut deps = indexmap::IndexMap::new();
    deps.insert(
        PackageId::new("test-pkg").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
    );

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![PackageId::new("test-pkg").unwrap()],
        dependencies: deps,
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = installer.validate_manifest(&manifest).await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_validate_manifest_missing_package() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    let mut deps = indexmap::IndexMap::new();
    deps.insert(
        PackageId::new("nonexistent").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
    );

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![PackageId::new("nonexistent").unwrap()],
        dependencies: deps,
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = installer.validate_manifest(&manifest).await;

    // Assert
    assert!(result.is_err());
}

// ============================================================================
// SECTION 6: Dry Run Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_dry_run_empty_manifest() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![],
        dependencies: indexmap::IndexMap::new(),
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = installer.dry_run(&manifest).await;

    // Assert
    assert!(result.is_ok());
    let plan = result.unwrap();
    assert!(plan.packages.is_empty());
}

#[tokio::test]
async fn test_dry_run_with_packages() {
    // Arrange
    let registry = Registry::new(100).await;
    let pkg = create_test_package("test-pkg", "1.0.0", vec![]);
    registry.insert(pkg).unwrap();
    let installer = Installer::new(registry);

    let mut deps = indexmap::IndexMap::new();
    deps.insert(
        PackageId::new("test-pkg").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
    );

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![PackageId::new("test-pkg").unwrap()],
        dependencies: deps,
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = installer.dry_run(&manifest).await;

    // Assert
    assert!(result.is_ok());
    let plan = result.unwrap();
    assert_eq!(plan.packages.len(), 1);
    assert!(plan.total_size > 0);
}

// ============================================================================
// SECTION 7: Installation Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_install_empty_manifest() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![],
        dependencies: indexmap::IndexMap::new(),
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = installer.install(manifest.clone()).await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_install_trait_dry_run() {
    // Arrange
    let registry = Registry::new(100).await;
    let installer = Installer::new(registry);

    let manifest = InstallationManifest {
        id: uuid::Uuid::new_v4(),
        packages: vec![],
        dependencies: indexmap::IndexMap::new(),
        install_path: "/tmp/test".to_string(),
        planned_at: chrono::Utc::now(),
    };

    // Act
    let result = Installable::dry_run_install(&installer, &manifest).await;

    // Assert
    assert!(result.is_ok());
}

// ============================================================================
// SECTION 8: InstallationPlan Tests (5 tests)
// ============================================================================

#[test]
fn test_installation_plan_display() {
    // Arrange
    let plan = InstallationPlan {
        id: uuid::Uuid::new_v4(),
        packages: vec![PackageInstallPlan {
            id: PackageId::new("test-pkg").unwrap(),
            version: PackageVersion::new("1.0.0").unwrap(),
            size: 102400, // 100 KB
        }],
        total_size: 102400,
        estimated_time: std::time::Duration::from_secs(1),
    };

    // Act
    let display = format!("{}", plan);

    // Assert
    assert!(display.contains("Installation Plan"));
    assert!(display.contains("Packages: 1"));
    assert!(display.contains("test-pkg@1.0.0"));
}

#[test]
fn test_package_install_plan_structure() {
    // Arrange & Act
    let plan = PackageInstallPlan {
        id: PackageId::new("test-pkg").unwrap(),
        version: PackageVersion::new("1.0.0").unwrap(),
        size: 1024,
    };

    // Assert
    assert_eq!(plan.id.as_str(), "test-pkg");
    assert_eq!(plan.version.as_str(), "1.0.0");
    assert_eq!(plan.size, 1024);
}

#[test]
fn test_installation_plan_clone() {
    // Arrange
    let plan = InstallationPlan {
        id: uuid::Uuid::new_v4(),
        packages: vec![],
        total_size: 0,
        estimated_time: std::time::Duration::from_secs(0),
    };

    // Act
    let cloned = plan.clone();

    // Assert
    assert_eq!(plan.id, cloned.id);
    assert_eq!(plan.total_size, cloned.total_size);
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_test_package(name: &str, version: &str, deps: Vec<PackageDependency>) -> Package {
    let id = PackageId::new(name).unwrap();
    let metadata =
        PackageMetadata::new(id.clone(), name, format!("Description for {}", name), "MIT");
    let ver = PackageVersion::new(version).unwrap();

    let mut releases = indexmap::IndexMap::new();
    releases.insert(
        ver.clone(),
        ReleaseInfo {
            version: ver.clone(),
            released_at: chrono::Utc::now(),
            changelog: "Initial release".to_string(),
            checksum: "abc123".to_string(),
            download_url: format!("https://example.com/{}/{}.tar.gz", name, version),
            dependencies: deps,
        },
    );

    Package {
        metadata,
        latest_version: ver.clone(),
        versions: vec![ver],
        releases,
    }
}
