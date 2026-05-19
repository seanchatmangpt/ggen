//! Integration tests for pack version migration system
//!
//! Chicago TDD approach: Real pack objects, real migration execution,
//! real state transitions (no mocks).

#![allow(missing_docs)]

use ggen_core::marketplace::migration::{Migrator, UpgradeEdge};
use ggen_core::marketplace::models::{Package, PackageId, PackageMetadata, PackageVersion};

/// Helper to create a test package with specified version
fn create_test_package(id: &str, version: &str) -> Package {
    let pkg_id = PackageId::new(id).expect("valid package id");
    Package {
        metadata: PackageMetadata::new(pkg_id, "Test Package", "A test package", "MIT"),
        latest_version: PackageVersion::new(version).expect("valid version"),
        versions: vec![PackageVersion::new(version).expect("valid version")],
        releases: indexmap::IndexMap::new(),
    }
}

#[test]
fn linear_upgrade_path_v1_to_v3() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");

    migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

    let path = migrator
        .compute_upgrade_path(&v1, &v3)
        .expect("path should exist");

    assert_eq!(path.len(), 3);
    assert_eq!(path[0], v1);
    assert_eq!(path[1], v2);
    assert_eq!(path[2], v3);
}

#[test]
fn direct_upgrade_single_step() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");

    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

    assert!(migrator.can_upgrade_directly(&v1, &v2));
    assert!(!migrator.can_upgrade_directly(&v2, &v1));
}

#[test]
fn no_upgrade_path_available() {
    let migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");

    let result = migrator.compute_upgrade_path(&v1, &v2);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("No upgrade path found"));
}

#[test]
fn branching_upgrade_paths() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2a = PackageVersion::new("2.0.0").expect("valid");
    let v2b = PackageVersion::new("2.1.0").expect("valid");

    // v1 can upgrade to either v2a or v2b
    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2a.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2b.clone()));

    let targets = migrator.get_upgrade_targets(&v1);
    assert_eq!(targets.len(), 2);
    assert!(targets.contains(&v2a));
    assert!(targets.contains(&v2b));
}

#[test]
fn same_version_path() {
    let migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");

    let path = migrator
        .compute_upgrade_path(&v1, &v1)
        .expect("same version path");

    assert_eq!(path.len(), 1);
    assert_eq!(path[0], v1);
}

#[test]
fn migrate_package_to_new_version() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");

    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

    let mut package = create_test_package("test-pkg", "1.0.0");
    let initial_id = package.metadata.id.clone();

    let result = migrator.migrate(package.clone(), &v1, &v2);
    assert!(result.is_ok());

    let migrated = result.unwrap();
    assert_eq!(migrated.latest_version, v2);
    assert_eq!(migrated.metadata.id, initial_id);
    assert!(migrated.versions.contains(&v2));
}

#[test]
fn migrate_through_multiple_versions() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");

    migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

    let package = create_test_package("test-pkg", "1.0.0");

    let result = migrator.migrate(package, &v1, &v3);
    assert!(result.is_ok());

    let migrated = result.unwrap();
    assert_eq!(migrated.latest_version, v3);
    // Both v1 and v3 should be in versions (plus any versions already there)
    assert!(migrated.versions.contains(&v3));
}

#[test]
fn compatibility_matrix_generation() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");

    migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

    let matrix = migrator.get_compatibility_matrix();

    // v1 can upgrade to v2
    assert_eq!(matrix[&v1].len(), 1);
    assert_eq!(matrix[&v1][0], v2);

    // v2 can upgrade to v3
    assert_eq!(matrix[&v2].len(), 1);
    assert_eq!(matrix[&v2][0], v3);

    // v3 cannot upgrade anywhere
    assert_eq!(matrix[&v3].len(), 0);
}

#[test]
fn get_all_registered_versions() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");

    migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

    let versions = migrator.get_all_versions();
    assert_eq!(versions.len(), 3);
}

#[test]
fn indirect_edge_marking() {
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");

    let edge = UpgradeEdge::new(v1, v3).indirect();
    assert!(!edge.is_direct);
}

#[test]
fn complex_upgrade_graph() {
    let mut migrator = Migrator::new();

    // Create a complex upgrade graph:
    //     v1
    //    / \
    //   v2  v2a
    //   |    |
    //   v3  v3a
    //    \ /
    //     v4

    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");
    let v2a = PackageVersion::new("2.1.0").expect("valid");
    let v3 = PackageVersion::new("3.0.0").expect("valid");
    let v3a = PackageVersion::new("3.1.0").expect("valid");
    let v4 = PackageVersion::new("4.0.0").expect("valid");

    // Add edges for the graph structure
    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2a.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v2.clone(), v3.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v2a.clone(), v3a.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v3.clone(), v4.clone()));
    migrator.add_upgrade_edge(UpgradeEdge::new(v3a.clone(), v4.clone()));

    // v1 → v4 should find a path
    let path = migrator
        .compute_upgrade_path(&v1, &v4)
        .expect("path should exist");
    assert!(path.len() >= 3); // At least 3 steps (v1 → v2/v2a → v3/v3a → v4)

    // v2 can upgrade to v3
    assert!(migrator.can_upgrade(&v2, &v3));

    // v2a can upgrade to v3a
    assert!(migrator.can_upgrade(&v2a, &v3a));
}

#[test]
fn upgrade_preserves_package_metadata() {
    let mut migrator = Migrator::new();
    let v1 = PackageVersion::new("1.0.0").expect("valid");
    let v2 = PackageVersion::new("2.0.0").expect("valid");

    migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

    let mut package = create_test_package("my-package", "1.0.0");
    let original_name = package.metadata.name.clone();
    let original_description = package.metadata.description.clone();

    let result = migrator.migrate(package, &v1, &v2);
    assert!(result.is_ok());

    let migrated = result.unwrap();
    assert_eq!(migrated.metadata.name, original_name);
    assert_eq!(migrated.metadata.description, original_description);
    assert_eq!(migrated.latest_version, v2);
}

#[test]
fn migrator_default_construction() {
    let migrator = Migrator::default();
    assert_eq!(migrator.get_all_versions().len(), 0);
}
