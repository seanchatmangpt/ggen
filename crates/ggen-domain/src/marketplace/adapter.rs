//! Marketplace adapter trait for supporting multiple backend implementations
//!
//! NOTE: This trait is legacy code. The CLI uses `ggen-marketplace-v2` directly
//! and bypasses this adapter layer. This code is kept for reference.
//!
//! # Current Architecture
//!
//! ```text
//! ┌─────────────────────┐
//! │   CLI Commands      │
//! │ (search, publish)   │
//! └──────────┬──────────┘
//!            │
//!            ▼
//! ┌──────────────────────────────────┐
//! │  ggen-marketplace-v2            │
//! │  (direct usage, no adapter)     │
//! └──────────────────────────────────┘
//! ```

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use ggen_utils::error::Result;

/// Unified marketplace registry interface
///
/// NOTE: This trait is legacy code. The CLI uses `ggen-marketplace-v2` directly
/// and does not use this trait. This code is kept for reference.
#[async_trait]
pub trait MarketplaceRegistry: Send + Sync {
    // ==================== Core Package Operations ====================

    /// Retrieve a single package by ID
    async fn get_package(&self, id: &str) -> Result<PackageInfo>;

    /// Retrieve all packages in the marketplace
    async fn list_all(&self) -> Result<Vec<PackageInfo>>;

    /// Publish a new package to the marketplace
    async fn publish(&self, package: &PackagePublish) -> Result<PublishSuccess>;

    // ==================== Search Operations ====================

    /// Generic search by query string
    ///
    /// Searches across package names, descriptions, and keywords.
    /// In v1: Uses fuzzy matching and keyword indexing
    /// In v2: Uses SPARQL semantic queries
    async fn search(&self, query: &str) -> Result<Vec<SearchMatch>>;

    /// Search by exact or keyword matching
    async fn search_by_keyword(&self, keyword: &str) -> Result<Vec<SearchMatch>>;

    /// Search by package author
    async fn search_by_author(&self, author: &str) -> Result<Vec<SearchMatch>>;

    /// Search by minimum quality score
    async fn search_by_quality(&self, min_score: u32) -> Result<Vec<SearchMatch>>;

    /// Search by description content
    async fn search_by_description(&self, text: &str) -> Result<Vec<SearchMatch>>;

    /// Get package version list for a given package
    async fn list_versions(&self, package_id: &str) -> Result<Vec<VersionInfo>>;

    // ==================== Discovery Operations ====================

    /// Get trending packages (sorted by downloads)
    async fn trending_packages(&self, limit: usize) -> Result<Vec<SearchMatch>>;

    /// Get recently published packages
    async fn recent_packages(&self, limit: usize) -> Result<Vec<SearchMatch>>;

    // ==================== Validation Operations ====================

    /// Validate package meets quality standards
    async fn validate_package(&self, package: &PackageInfo) -> Result<ValidationResult>;

    /// Get validation results for all packages
    async fn validate_all(&self) -> Result<Vec<ValidationResult>>;

    // ==================== Recommendation Operations ====================

    /// Get package recommendations based on a given package
    ///
    /// Returns packages with similar dependencies, quality, or domain
    async fn get_recommendations(&self, package_id: &str) -> Result<Vec<Recommendation>>;

    /// Compare multiple packages
    async fn compare_packages(&self, ids: &[String]) -> Result<ComparisonResult>;

    // ==================== Installation Operations ====================

    /// Resolve dependencies for a package
    async fn resolve_dependencies(&self, package_id: &str) -> Result<Vec<DependencyInfo>>;

    /// Get installation manifest for a package
    async fn get_installation_manifest(&self, package_id: &str) -> Result<InstallationManifest>;
}

// ==================== Domain Types ====================

/// Basic package information
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackageInfo {
    /// Package ID (unique identifier)
    pub id: String,

    /// Human-readable name
    pub name: String,

    /// Short description
    pub description: String,

    /// Current version
    pub version: String,

    /// Package author
    pub author: String,

    /// Quality score (0-100)
    pub quality_score: u32,

    /// Number of downloads
    pub downloads: u64,

    /// Whether package is marked as production-ready
    pub is_production_ready: bool,

    /// Repository URL (if available)
    pub repository: Option<String>,

    /// License (if specified)
    pub license: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,

    /// Installation location on disk
    pub location: PathBuf,

    /// Disk usage in bytes
    pub size_bytes: u64,

    /// When the package version was published
    pub published_at: DateTime<Utc>,

    /// Last time the package was used
    pub last_used: DateTime<Utc>,

    /// When the package was installed/created locally
    pub created_at: DateTime<Utc>,

    /// Deprecation flag
    pub is_deprecated: bool,

    /// Deprecation notice (if any)
    pub deprecation_notice: Option<String>,

    /// Semantic metadata (for v2)
    #[serde(default)]
    pub metadata: HashMap<String, String>,
}

/// Package information for publication
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PackagePublish {
    pub name: String,
    pub description: String,
    pub version: String,
    pub author: String,
    pub dependencies: Vec<String>,
    pub repository: Option<String>,
    pub license: Option<String>,
    pub readme: Option<String>,
}

/// Search result match
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SearchMatch {
    pub package_id: String,
    pub name: String,
    pub version: String,
    pub relevance_score: f64, // 0.0-1.0
    pub description: Option<String>,
}

/// Version information
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct VersionInfo {
    pub version: String,
    pub published_at: String,
    pub is_stable: bool,
}

/// Package validation result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ValidationResult {
    pub package_id: String,
    pub is_valid: bool,
    pub quality_score: u32,
    pub checks_passed: Vec<String>,
    pub checks_failed: Vec<String>,
    pub warnings: Vec<String>,
    pub recommendations: Vec<String>,
}

/// Package recommendation
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Recommendation {
    pub package_id: String,
    pub reason: RecommendationReason,
    pub similarity_score: f64,
}

/// Why a package is recommended
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RecommendationReason {
    SimilarDependencies,
    SameAuthor,
    SimilarQuality,
    ComplementaryFunctionality,
}

/// Result of comparing multiple packages
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ComparisonResult {
    pub packages: Vec<PackageComparison>,
    pub summary: String,
}

/// Individual package in a comparison
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PackageComparison {
    pub package_id: String,
    pub properties: HashMap<String, String>,
}

/// Dependency information
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DependencyInfo {
    pub package_id: String,
    pub required_version: String,
    pub is_optional: bool,
}

/// Installation manifest
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstallationManifest {
    pub package_id: String,
    pub dependencies: Vec<DependencyInfo>,
    pub install_order: Vec<String>,
    pub estimated_download_size: u64,
}

/// Result of publishing a package
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PublishSuccess {
    pub package_id: String,
    pub version: String,
    pub url: String,
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::expect_used)]
    #[test]
    fn test_trait_is_object_safe() {
        // Verify MarketplaceRegistry can be used as &dyn MarketplaceRegistry
        // This test just checks that the code compiles; the actual test is the compilation itself
        fn _takes_dyn_registry(_: &dyn MarketplaceRegistry) {}
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_package_info_serialization() {
        let pkg = PackageInfo {
            id: "test-pkg".to_string(),
            name: "Test Package".to_string(),
            description: "A test package".to_string(),
            version: "1.0.0".to_string(),
            author: "Test Author".to_string(),
            quality_score: 85,
            downloads: 1000,
            is_production_ready: true,
            repository: Some("https://example.com".to_string()),
            license: Some("MIT".to_string()),
            homepage: Some("https://example.com/docs".to_string()),
            location: PathBuf::from("/opt/packs/test"),
            size_bytes: 1_024_000,
            published_at: Utc::now(),
            last_used: Utc::now(),
            created_at: Utc::now(),
            is_deprecated: false,
            deprecation_notice: None,
            metadata: Default::default(),
        };

        let json = serde_json::to_string(&pkg).expect("serialization failed");
        let deserialized: PackageInfo =
            serde_json::from_str(&json).expect("deserialization failed");

        assert_eq!(pkg, deserialized);
    }

    #[test]
    fn test_search_match_relevance_bounds() {
        let mut matches = vec![
            SearchMatch {
                package_id: "pkg1".to_string(),
                name: "Package 1".to_string(),
                version: "1.0.0".to_string(),
                relevance_score: 1.0,
                description: None,
            },
            SearchMatch {
                package_id: "pkg2".to_string(),
                name: "Package 2".to_string(),
                version: "2.0.0".to_string(),
                relevance_score: 0.5,
                description: None,
            },
        ];

        // Sort by relevance (highest first)
        matches.sort_by(|a, b| b.relevance_score.partial_cmp(&a.relevance_score).unwrap());

        assert_eq!(matches[0].package_id, "pkg1");
        assert_eq!(matches[1].package_id, "pkg2");
    }

    #[test]
    fn test_validation_result_completeness() {
        let result = ValidationResult {
            package_id: "test".to_string(),
            is_valid: true,
            quality_score: 95,
            checks_passed: vec!["readme".to_string(), "tests".to_string()],
            checks_failed: vec![],
            warnings: vec!["outdated dependency".to_string()],
            recommendations: vec!["add changelog".to_string()],
        };

        assert!(result.is_valid);
        assert_eq!(result.checks_passed.len(), 2);
        assert_eq!(result.warnings.len(), 1);
        assert_eq!(result.recommendations.len(), 1);
    }
}
