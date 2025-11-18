//! Test helpers and fixtures for marketplace-v2 tests
//!
//! Provides reusable test utilities, fixtures, and mock data for comprehensive testing.

use chrono::Utc;
use ggen_marketplace_v2::{
    Package, PackageId, PackageMetadata, PackageVersion, QualityScore, ReleaseInfo,
};

/// Create a test package with minimal required fields
pub fn create_test_package(id: &str, name: &str, version: &str) -> Package {
    let pkg_id = PackageId::new(id).expect("Valid package ID");
    let pkg_version = PackageVersion::new(version).expect("Valid version");

    let metadata = PackageMetadata::new(
        pkg_id.clone(),
        name,
        format!("Test package: {}", name),
        "MIT",
    );

    Package {
        metadata,
        latest_version: pkg_version.clone(),
        versions: vec![pkg_version.clone()],
        releases: {
            let mut releases = indexmap::IndexMap::new();
            releases.insert(
                pkg_version.clone(),
                ReleaseInfo {
                    version: pkg_version,
                    released_at: Utc::now(),
                    changelog: "Initial release".to_string(),
                    checksum: "test-checksum".to_string(),
                    download_url: format!("https://example.com/{}", id),
                    dependencies: vec![],
                },
            );
            releases
        },
    }
}

/// Create a package with multiple versions
pub fn create_multi_version_package(id: &str, name: &str, versions: &[&str]) -> Package {
    let pkg_id = PackageId::new(id).expect("Valid package ID");
    let mut version_objs: Vec<PackageVersion> = versions
        .iter()
        .map(|v| PackageVersion::new(*v).expect("Valid version"))
        .collect();

    // Sort versions in descending order (newest first)
    version_objs.sort_by(|a, b| b.cmp(a));

    let latest = version_objs.first().cloned().expect("At least one version");

    let metadata = PackageMetadata::new(
        pkg_id.clone(),
        name,
        format!("Multi-version test package: {}", name),
        "MIT",
    );

    let releases = version_objs
        .iter()
        .map(|v| {
            (
                v.clone(),
                ReleaseInfo {
                    version: v.clone(),
                    released_at: Utc::now(),
                    changelog: format!("Release {}", v),
                    checksum: format!("checksum-{}", v),
                    download_url: format!("https://example.com/{}/{}", id, v),
                    dependencies: vec![],
                },
            )
        })
        .collect();

    Package {
        metadata,
        latest_version: latest,
        versions: version_objs,
        releases,
    }
}

/// Create a package with quality score
pub fn create_quality_package(id: &str, name: &str, version: &str, quality: u32) -> Package {
    let mut package = create_test_package(id, name, version);
    package.metadata.quality_score = Some(QualityScore::new(quality).expect("Valid quality score"));
    package
}

/// Create a package with keywords and categories
pub fn create_searchable_package(
    id: &str, name: &str, keywords: Vec<String>, categories: Vec<String>,
) -> Package {
    let mut package = create_test_package(id, name, "1.0.0");
    package.metadata.keywords = keywords;
    package.metadata.categories = categories;
    package
}

/// Assert that two packages are equal (ignoring timestamps)
pub fn assert_packages_equal(actual: &Package, expected: &Package) {
    assert_eq!(actual.metadata.id, expected.metadata.id);
    assert_eq!(actual.metadata.name, expected.metadata.name);
    assert_eq!(actual.latest_version, expected.latest_version);
    assert_eq!(actual.versions.len(), expected.versions.len());
}

/// Common test package IDs
pub mod test_ids {
    pub const PKG_A: &str = "test-package-a";
    pub const PKG_B: &str = "test-package-b";
    pub const PKG_C: &str = "test-package-c";
    pub const INVALID_ID: &str = "invalid package!";
}

/// Common test versions
pub mod test_versions {
    pub const V1_0_0: &str = "1.0.0";
    pub const V1_1_0: &str = "1.1.0";
    pub const V2_0_0: &str = "2.0.0";
    pub const INVALID: &str = "not-a-version";
}
