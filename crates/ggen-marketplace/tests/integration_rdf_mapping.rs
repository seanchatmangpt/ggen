//! Integration tests for RDF mapping and storage
//!
//! Tests the complete RDF integration including:
//! - Package → RDF conversion
//! - RDF → Package reconstruction
//! - Round-trip data integrity
//! - AsyncRepository implementation
//! - Migration utilities
//! - Consistency validation

use ggen_marketplace_v2::{
    migration::MigrationCoordinator,
    models::{Package, PackageId, PackageMetadata, PackageVersion, ReleaseInfo},
    registry_rdf::RdfRegistry,
    traits::AsyncRepository,
};
use std::sync::Arc;

#[tokio::test]
async fn test_package_to_rdf_basic_metadata() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("basic-test").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Basic Test", "A basic test package", "MIT");

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![PackageVersion::new("1.0.0").unwrap()],
        releases: indexmap::IndexMap::new(),
    };

    // Insert package
    registry.insert_package_rdf(&package).await.unwrap();

    // Retrieve package
    let retrieved = registry.get_package(&id).await.unwrap();

    // Verify basic metadata
    assert_eq!(retrieved.metadata.id, id);
    assert_eq!(retrieved.metadata.name, "Basic Test");
    assert_eq!(retrieved.metadata.description, "A basic test package");
    assert_eq!(retrieved.metadata.license, "MIT");
}

#[tokio::test]
async fn test_package_with_authors_and_keywords() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("authored-pkg").unwrap();
    let mut metadata = PackageMetadata::new(
        id.clone(),
        "Authored Package",
        "Package with authors",
        "Apache-2.0",
    );
    metadata.authors = vec![
        "Alice".to_string(),
        "Bob".to_string(),
        "Charlie".to_string(),
    ];
    metadata.keywords = vec![
        "rust".to_string(),
        "rdf".to_string(),
        "semantic".to_string(),
    ];

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("2.0.0").unwrap(),
        versions: vec![PackageVersion::new("2.0.0").unwrap()],
        releases: indexmap::IndexMap::new(),
    };

    registry.insert_package_rdf(&package).await.unwrap();
    let retrieved = registry.get_package(&id).await.unwrap();

    assert_eq!(retrieved.metadata.authors.len(), 3);
    assert!(retrieved.metadata.authors.contains(&"Alice".to_string()));
    assert!(retrieved.metadata.authors.contains(&"Bob".to_string()));
    assert_eq!(retrieved.metadata.keywords.len(), 3);
}

#[tokio::test]
async fn test_package_with_multiple_versions() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("multi-version").unwrap();
    let metadata = PackageMetadata::new(
        id.clone(),
        "Multi Version",
        "Package with versions",
        "BSD-3-Clause",
    );

    let mut releases = indexmap::IndexMap::new();
    releases.insert(
        PackageVersion::new("1.0.0").unwrap(),
        ReleaseInfo {
            version: PackageVersion::new("1.0.0").unwrap(),
            released_at: chrono::Utc::now(),
            changelog: "Initial release".to_string(),
            checksum: "abc123".to_string(),
            download_url: "https://example.com/pkg-1.0.0.tar.gz".to_string(),
            dependencies: vec![],
        },
    );
    releases.insert(
        PackageVersion::new("1.1.0").unwrap(),
        ReleaseInfo {
            version: PackageVersion::new("1.1.0").unwrap(),
            released_at: chrono::Utc::now(),
            changelog: "Added new features".to_string(),
            checksum: "def456".to_string(),
            download_url: "https://example.com/pkg-1.1.0.tar.gz".to_string(),
            dependencies: vec![],
        },
    );

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.1.0").unwrap(),
        versions: vec![
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("1.1.0").unwrap(),
        ],
        releases,
    };

    registry.insert_package_rdf(&package).await.unwrap();
    let retrieved = registry.get_package(&id).await.unwrap();

    assert_eq!(retrieved.versions.len(), 2);
    assert_eq!(retrieved.releases.len(), 2);
    assert_eq!(retrieved.latest_version.as_str(), "1.1.0");
}

#[tokio::test]
async fn test_get_package_version() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("versioned").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Versioned", "Package", "MIT");

    let mut releases = indexmap::IndexMap::new();
    releases.insert(
        PackageVersion::new("1.0.0").unwrap(),
        ReleaseInfo {
            version: PackageVersion::new("1.0.0").unwrap(),
            released_at: chrono::Utc::now(),
            changelog: "v1".to_string(),
            checksum: "abc".to_string(),
            download_url: "https://example.com/1.0.0.tar.gz".to_string(),
            dependencies: vec![],
        },
    );
    releases.insert(
        PackageVersion::new("2.0.0").unwrap(),
        ReleaseInfo {
            version: PackageVersion::new("2.0.0").unwrap(),
            released_at: chrono::Utc::now(),
            changelog: "v2".to_string(),
            checksum: "def".to_string(),
            download_url: "https://example.com/2.0.0.tar.gz".to_string(),
            dependencies: vec![],
        },
    );

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("2.0.0").unwrap(),
        versions: vec![
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("2.0.0").unwrap(),
        ],
        releases,
    };

    registry.insert_package_rdf(&package).await.unwrap();

    // Get specific version
    let v1 = registry
        .get_package_version(&id, &PackageVersion::new("1.0.0").unwrap())
        .await
        .unwrap();

    assert_eq!(v1.versions.len(), 1);
    assert_eq!(v1.versions[0].as_str(), "1.0.0");
    assert_eq!(v1.releases.len(), 1);
}

#[tokio::test]
async fn test_list_versions() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("list-versions").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "List Test", "Test", "MIT");

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("3.0.0").unwrap(),
        versions: vec![
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("2.0.0").unwrap(),
            PackageVersion::new("3.0.0").unwrap(),
        ],
        releases: indexmap::IndexMap::new(),
    };

    registry.insert_package_rdf(&package).await.unwrap();

    let versions = registry.list_versions(&id).await.unwrap();
    assert_eq!(versions.len(), 3);
}

#[tokio::test]
async fn test_package_exists() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("exists-test").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Exists Test", "Test", "MIT");

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![PackageVersion::new("1.0.0").unwrap()],
        releases: indexmap::IndexMap::new(),
    };

    registry.insert_package_rdf(&package).await.unwrap();

    assert!(registry.package_exists(&id).await.unwrap());
    assert!(!registry
        .package_exists(&PackageId::new("nonexistent").unwrap())
        .await
        .unwrap());
}

#[tokio::test]
async fn test_all_packages() {
    let registry = RdfRegistry::new();

    // Insert multiple packages
    for i in 1..=5 {
        let id = PackageId::new(&format!("pkg-{}", i)).unwrap();
        let metadata = PackageMetadata::new(id.clone(), &format!("Package {}", i), "Test", "MIT");

        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        };

        registry.insert_package_rdf(&package).await.unwrap();
    }

    let all = registry.all_packages().await.unwrap();
    assert_eq!(all.len(), 5);
}

#[tokio::test]
async fn test_batch_insert() {
    let registry = Arc::new(RdfRegistry::new());

    let mut packages = Vec::new();
    for i in 1..=10 {
        let id = PackageId::new(&format!("batch-{}", i)).unwrap();
        let metadata = PackageMetadata::new(id.clone(), &format!("Batch {}", i), "Test", "MIT");

        packages.push(Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        });
    }

    let inserted = registry.batch_insert_packages(packages).await.unwrap();
    assert_eq!(inserted, 10);
}

#[tokio::test]
async fn test_migration_coordinator() {
    let registry = Arc::new(RdfRegistry::new());
    let coordinator = MigrationCoordinator::new(Arc::clone(&registry));

    // Create v1 packages
    let mut v1_packages = Vec::new();
    for i in 1..=5 {
        let id = PackageId::new(&format!("migrate-{}", i)).unwrap();
        let metadata = PackageMetadata::new(id.clone(), &format!("Migrate {}", i), "Test", "MIT");

        v1_packages.push(Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        });
    }

    // Perform migration
    let report = coordinator
        .migrate_packages(v1_packages.clone())
        .await
        .unwrap();

    assert_eq!(report.total_packages, 5);
    assert_eq!(report.migrated_packages, 5);
    assert!(report.is_successful());

    // Verify migration
    let verification = coordinator.verify_migration(v1_packages).await.unwrap();
    assert_eq!(verification.verified_packages, 5);
    assert!(verification.is_valid());
}

#[tokio::test]
async fn test_round_trip_data_integrity() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("roundtrip").unwrap();
    let mut metadata =
        PackageMetadata::new(id.clone(), "Round Trip Test", "Complete test", "GPL-3.0");
    metadata.authors = vec!["Developer".to_string()];
    metadata.keywords = vec!["test".to_string(), "rdf".to_string()];
    metadata.repository = Some("https://github.com/test/roundtrip".to_string());
    metadata.homepage = Some("https://test.example.com".to_string());

    let mut releases = indexmap::IndexMap::new();
    releases.insert(
        PackageVersion::new("1.0.0").unwrap(),
        ReleaseInfo {
            version: PackageVersion::new("1.0.0").unwrap(),
            released_at: chrono::Utc::now(),
            changelog: "Initial release".to_string(),
            checksum: "sha256abc123".to_string(),
            download_url: "https://example.com/roundtrip-1.0.0.tar.gz".to_string(),
            dependencies: vec![],
        },
    );

    let original_package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![PackageVersion::new("1.0.0").unwrap()],
        releases,
    };

    // Convert to RDF
    registry
        .insert_package_rdf(&original_package)
        .await
        .unwrap();

    // Reconstruct from RDF
    let reconstructed = registry.get_package(&id).await.unwrap();

    // Verify all fields match
    assert_eq!(reconstructed.metadata.id, original_package.metadata.id);
    assert_eq!(reconstructed.metadata.name, original_package.metadata.name);
    assert_eq!(
        reconstructed.metadata.description,
        original_package.metadata.description
    );
    assert_eq!(
        reconstructed.metadata.license,
        original_package.metadata.license
    );
    assert_eq!(
        reconstructed.metadata.authors,
        original_package.metadata.authors
    );
    assert_eq!(
        reconstructed.metadata.keywords,
        original_package.metadata.keywords
    );
    assert_eq!(
        reconstructed.metadata.repository,
        original_package.metadata.repository
    );
    assert_eq!(
        reconstructed.metadata.homepage,
        original_package.metadata.homepage
    );
    assert_eq!(reconstructed.versions, original_package.versions);
    assert_eq!(
        reconstructed.latest_version,
        original_package.latest_version
    );
}

#[tokio::test]
async fn test_invalid_package_id() {
    let registry = RdfRegistry::new();

    let result = registry
        .get_package(&PackageId::new("nonexistent-pkg").unwrap())
        .await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_invalid_version() {
    let registry = RdfRegistry::new();

    let id = PackageId::new("version-test").unwrap();
    let metadata = PackageMetadata::new(id.clone(), "Version Test", "Test", "MIT");

    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![PackageVersion::new("1.0.0").unwrap()],
        releases: indexmap::IndexMap::new(),
    };

    registry.insert_package_rdf(&package).await.unwrap();

    let result = registry
        .get_package_version(&id, &PackageVersion::new("999.0.0").unwrap())
        .await;

    assert!(result.is_err());
}
