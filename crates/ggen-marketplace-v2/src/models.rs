//! Data models for the marketplace
//!
//! Uses type-level programming and poka-yoke design to prevent invalid states at compile time.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::num::NonZeroU32;
use std::str::FromStr;
use uuid::Uuid;

use crate::error::{Error, Result};

/// Typestate marker for draft packages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Draft;

/// Typestate marker for published packages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Published;

/// Package state enum for backward compatibility with tests
/// NOTE: Deprecated - use typestate markers (Draft, Published) instead
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PackageState {
    Draft,
    Published,
    Deprecated,
    Yanked,
}

// QualityScore is defined below with manual Serialize/Deserialize implementation

/// Ggen ontology wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenOntology {
    pub namespace: String,
}

impl GgenOntology {
    pub fn new(namespace: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
        }
    }
}

/// Signature algorithm for package verification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SignatureAlgorithm {
    Ed25519,
    Rsa2048,
    Rsa4096,
}

/// License identifier (SPDX format)
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LicenseId(String);

impl LicenseId {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn mit() -> Self {
        Self("MIT".to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for LicenseId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Author email address
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AuthorEmail(String);

impl AuthorEmail {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Package keyword
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Keyword(String);

impl Keyword {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Package category
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Category(String);

impl Category {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Package checksum
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Checksum(String);

impl Checksum {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Repository URL
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct RepositoryUrl(String);

impl RepositoryUrl {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A validated package identifier
///
/// Guarantees:
/// - Non-empty
/// - Contains only alphanumeric, hyphens, and underscores
/// - Does not start or end with hyphen
/// - Lowercase
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct PackageId(String);

impl PackageId {
    /// Create a new validated PackageId
    pub fn new(s: impl Into<String>) -> Result<Self> {
        let s = s.into().to_lowercase();

        // Validate format
        if s.is_empty() {
            return Err(Error::invalid_package_id("Package ID cannot be empty"));
        }

        if s.len() > 200 {
            return Err(Error::invalid_package_id(
                "Package ID too long (max 200 chars)",
            ));
        }

        if !s
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            return Err(Error::invalid_package_id(
                "Package ID can only contain alphanumeric, hyphens, and underscores",
            ));
        }

        if s.starts_with('-') || s.ends_with('-') {
            return Err(Error::invalid_package_id(
                "Package ID cannot start or end with hyphen",
            ));
        }

        Ok(Self(s))
    }

    /// Get the package ID as a string
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for PackageId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Self::new(s)
    }
}

impl AsRef<str> for PackageId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Semantic version with validation
///
/// Guarantees:
/// - Valid semantic versioning (MAJOR.MINOR.PATCH)
/// - Pre-release and build metadata supported
/// - Sortable and comparable
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct PackageVersion(String);

impl PackageVersion {
    /// Create a new validated version
    pub fn new(v: impl Into<String>) -> Result<Self> {
        let v = v.into();

        // Basic semantic version validation
        if v.is_empty() {
            return Err(Error::invalid_version(&v, "Version cannot be empty"));
        }

        // Remove v prefix if present
        let normalized = v.strip_prefix('v').unwrap_or(&v);

        // Split by . to validate basic structure
        let parts: Vec<&str> = normalized.split('.').take(3).collect();
        if parts.len() < 3 {
            return Err(Error::invalid_version(
                &v,
                "Must follow semantic versioning (MAJOR.MINOR.PATCH)",
            ));
        }

        // Validate major, minor, patch are numbers
        if !parts[0].parse::<u32>().is_ok()
            || !parts[1].parse::<u32>().is_ok()
            || !parts[2]
                .split(|c: char| c == '-' || c == '+')
                .next()
                .unwrap_or("")
                .parse::<u32>()
                .is_ok()
        {
            return Err(Error::invalid_version(
                &v,
                "Major, minor, patch must be numeric",
            ));
        }

        Ok(Self(normalized.to_string()))
    }

    /// Get the version as a string
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for PackageVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for PackageVersion {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Self::new(s)
    }
}

impl PartialOrd for PackageVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PackageVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        // Simple version comparison (real implementation would use semver crate)
        let self_parts: Vec<u32> = self
            .0
            .split(|c: char| !c.is_numeric())
            .take(3)
            .filter_map(|p| p.parse().ok())
            .collect();

        let other_parts: Vec<u32> = other
            .0
            .split(|c: char| !c.is_numeric())
            .take(3)
            .filter_map(|p| p.parse().ok())
            .collect();

        self_parts.cmp(&other_parts)
    }
}

/// Quality score (0-100)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct QualityScore(NonZeroU32);

// Manual serialization/deserialization for QualityScore
impl Serialize for QualityScore {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_u32(self.0.get())
    }
}

impl<'de> Deserialize<'de> for QualityScore {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let val = u32::deserialize(deserializer)?;
        NonZeroU32::new(val)
            .map(QualityScore)
            .ok_or_else(|| serde::de::Error::custom("QualityScore must be > 0"))
    }
}

impl QualityScore {
    /// Create a new quality score
    pub fn new(score: u32) -> Result<Self> {
        if score > 100 {
            return Err(Error::ValidationFailed {
                reason: format!("Quality score must be <= 100, got {}", score),
            });
        }
        NonZeroU32::new(score)
            .map(Self)
            .ok_or_else(|| Error::ValidationFailed {
                reason: "Quality score must be > 0".to_string(),
            })
    }

    /// Get the raw score value
    pub fn value(self) -> u32 {
        self.0.get()
    }

    /// Check if score indicates production ready (>= 95)
    pub fn is_production_ready(self) -> bool {
        self.value() >= 95
    }

    /// Check if score indicates needs improvement (80-94)
    pub fn needs_improvement(self) -> bool {
        let v = self.value();
        v >= 80 && v < 95
    }

    /// Check if score indicates not ready (< 80)
    pub fn not_ready(self) -> bool {
        self.value() < 80
    }
}

impl fmt::Display for QualityScore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}%", self.value())
    }
}

impl Default for QualityScore {
    fn default() -> Self {
        // SAFETY: 50 is a valid quality score (> 0 and <= 100)
        #[allow(clippy::expect_used)]
        Self::new(50).expect("50 is a valid quality score")
    }
}

/// Package metadata
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PackageMetadata {
    /// Unique identifier
    pub id: PackageId,
    /// Human-readable name
    pub name: String,
    /// Detailed description
    pub description: String,
    /// Package author(s)
    pub authors: Vec<String>,
    /// License identifier (SPDX)
    pub license: String,
    /// Repository URL
    pub repository: Option<String>,
    /// Homepage URL
    pub homepage: Option<String>,
    /// Keywords for discovery
    pub keywords: Vec<String>,
    /// Categories
    pub categories: Vec<String>,
    /// When package was created
    pub created_at: DateTime<Utc>,
    /// When package was last updated
    pub updated_at: DateTime<Utc>,
    /// Download count
    pub downloads: u64,
    /// Quality score (0-100)
    pub quality_score: Option<QualityScore>,
}

impl PackageMetadata {
    /// Create new package metadata
    pub fn new(
        id: PackageId, name: impl Into<String>, description: impl Into<String>,
        license: impl Into<String>,
    ) -> Self {
        let now = Utc::now();
        Self {
            id,
            name: name.into(),
            description: description.into(),
            authors: Vec::new(),
            license: license.into(),
            repository: None,
            homepage: None,
            keywords: Vec::new(),
            categories: Vec::new(),
            created_at: now,
            updated_at: now,
            downloads: 0,
            quality_score: None,
        }
    }
}

/// Package manifest (Cargo.toml equivalent)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Manifest {
    /// Package ID
    pub id: PackageId,
    /// Version
    pub version: PackageVersion,
    /// Metadata
    pub metadata: PackageMetadata,
    /// Dependencies
    pub dependencies: Vec<PackageDependency>,
    /// Optional features
    pub features: indexmap::IndexMap<String, Vec<String>>,
}

/// A package dependency
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PackageDependency {
    /// Dependency package ID
    pub id: PackageId,
    /// Version requirement
    pub version_req: String,
    /// Whether this is optional
    pub optional: bool,
}

/// A complete package with all versions
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    /// Package metadata
    pub metadata: PackageMetadata,
    /// Latest version
    pub latest_version: PackageVersion,
    /// All available versions (sorted newest first)
    pub versions: Vec<PackageVersion>,
    /// Release information per version
    pub releases: indexmap::IndexMap<PackageVersion, ReleaseInfo>,
}

/// Information about a specific release
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReleaseInfo {
    /// Version
    pub version: PackageVersion,
    /// Release date
    pub released_at: DateTime<Utc>,
    /// Release notes/changelog
    pub changelog: String,
    /// Checksum (SHA256)
    pub checksum: String,
    /// Download URL
    pub download_url: String,
    /// Dependencies for this version
    pub dependencies: Vec<PackageDependency>,
}

/// Search query result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SearchResult {
    /// Matched package
    pub package: Package,
    /// Relevance score (0.0-1.0)
    pub relevance: f64,
}

/// Installation manifest
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstallationManifest {
    /// Installation ID
    pub id: Uuid,
    /// Packages to install
    pub packages: Vec<PackageId>,
    /// Resolved dependencies
    pub dependencies: indexmap::IndexMap<PackageId, PackageVersion>,
    /// Installation path
    pub install_path: String,
    /// When installation was planned
    pub planned_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_id_validation() {
        assert!(PackageId::new("valid-package").is_ok());
        assert!(PackageId::new("valid_package").is_ok());
        assert!(PackageId::new("123-pkg").is_ok());

        assert!(PackageId::new("").is_err());
        assert!(PackageId::new("-invalid").is_err());
        assert!(PackageId::new("invalid-").is_err());
        assert!(PackageId::new("invalid package").is_err());
    }

    #[test]
    fn test_package_version_validation() {
        assert!(PackageVersion::new("1.0.0").is_ok());
        assert!(PackageVersion::new("v1.0.0").is_ok());
        assert!(PackageVersion::new("1.0.0-alpha").is_ok());

        assert!(PackageVersion::new("1.0").is_err());
        assert!(PackageVersion::new("abc").is_err());
    }

    #[test]
    fn test_quality_score() {
        assert!(QualityScore::new(95).is_ok());
        assert!(QualityScore::new(150).is_err());

        let score = QualityScore::new(95).unwrap();
        assert!(score.is_production_ready());

        let score = QualityScore::new(85).unwrap();
        assert!(score.needs_improvement());
    }
}
