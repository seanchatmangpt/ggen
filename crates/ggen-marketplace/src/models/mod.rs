//! Core data models for the marketplace
//!
//! This module defines the core data structures used throughout the marketplace
//! system, including packages, versions, queries, signatures, and template packages.
//!
//! ## Key Types
//!
//! - **Package**: Complete package metadata with version, dependencies, and content
//! - **Version**: Semantic versioning support
//! - **Query**: Search query structures with filters and facets
//! - **Signature**: Cryptographic signatures for package verification
//! - **TemplatePackage**: Template-specific package metadata

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

pub mod package;
pub mod query;
pub mod signature;
pub mod template_package;

pub use package::{
    ContentId, HashAlgorithm, Package, PackageId, PackageMetadata, UnvalidatedPackage,
    ValidatedPackage, ValidationMetadata,
};
pub use query::{Query, SearchQuery, SearchResults};
pub use signature::{PublicKey, Signature};
pub use template_package::{
    TemplateExample, TemplateInfo, TemplatePackage, TemplateType, TemplateVariable,
};

/// Package version following semantic versioning
///
/// Represents a semantic version (SemVer) with major, minor, and patch components.
///
/// # Examples
///
/// ```rust
/// use ggen_marketplace::models::Version;
///
/// # fn main() {
/// let version = Version::new(1, 2, 3);
/// assert_eq!(version.major, 1);
/// assert_eq!(version.minor, 2);
/// assert_eq!(version.patch, 3);
///
/// let version_with_pre = Version::new(1, 0, 0).with_pre_release("alpha.1");
/// assert_eq!(version_with_pre.to_string(), "1.0.0-alpha.1");
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre_release: Option<String>,
}

impl Version {
    /// Create a new version
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_marketplace::models::Version;
    ///
    /// # fn main() {
    /// let version = Version::new(1, 2, 3);
    /// assert_eq!(version.to_string(), "1.2.3");
    /// # }
    /// ```
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
            pre_release: None,
        }
    }

    pub fn with_pre_release(mut self, pre_release: impl Into<String>) -> Self {
        self.pre_release = Some(pre_release.into());
        self
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if let Some(pre) = &self.pre_release {
            write!(f, "-{}", pre)?;
        }
        Ok(())
    }
}

/// Package category for classification
///
/// Categorizes packages by their primary purpose or domain.
///
/// # Examples
///
/// ```rust
/// use ggen_marketplace::models::Category;
///
/// # fn main() {
/// let category = Category::WebService;
/// assert_eq!(category.to_string(), "web-service");
///
/// let custom = Category::Custom("my-category".to_string());
/// assert_eq!(custom.to_string(), "my-category");
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Category {
    /// Web service templates
    WebService,
    /// Database templates
    Database,
    /// Authentication templates
    Authentication,
    /// Middleware templates
    Middleware,
    /// Testing templates
    Testing,
    /// Deployment templates
    Deployment,
    /// Monitoring templates
    Monitoring,
    /// Documentation templates
    Documentation,
    /// Utility templates
    Utility,
    /// Custom category
    Custom(String),
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WebService => write!(f, "web-service"),
            Self::Database => write!(f, "database"),
            Self::Authentication => write!(f, "authentication"),
            Self::Middleware => write!(f, "middleware"),
            Self::Testing => write!(f, "testing"),
            Self::Deployment => write!(f, "deployment"),
            Self::Monitoring => write!(f, "monitoring"),
            Self::Documentation => write!(f, "documentation"),
            Self::Utility => write!(f, "utility"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// Package statistics and metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageStats {
    pub downloads: u64,
    pub stars: u32,
    pub watchers: u32,
    pub forks: u32,
    pub contributors: u32,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}

impl Default for PackageStats {
    fn default() -> Self {
        Self {
            downloads: 0,
            stars: 0,
            watchers: 0,
            forks: 0,
            contributors: 0,
            last_updated: chrono::Utc::now(),
        }
    }
}

/// Package dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub package_id: PackageId,
    pub version_requirement: VersionRequirement,
    pub optional: bool,
}

/// Version requirement for dependencies
///
/// Specifies version constraints for package dependencies.
///
/// # Examples
///
/// ```rust
/// use ggen_marketplace::models::{Version, VersionRequirement};
///
/// # fn main() {
/// // Exact version
/// let req = VersionRequirement::Exact(Version::new(1, 2, 3));
///
/// // Version range
/// let req = VersionRequirement::Range {
///     min: Version::new(1, 0, 0),
///     max: Version::new(2, 0, 0),
/// };
///
/// // Minimum version
/// let req = VersionRequirement::Minimum(Version::new(1, 0, 0));
///
/// // Compatible version (^x.y.z)
/// let req = VersionRequirement::Compatible(Version::new(1, 0, 0));
///
/// // Any version
/// let req = VersionRequirement::Any;
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum VersionRequirement {
    /// Exact version match required
    Exact(Version),
    /// Version must be within range
    Range { min: Version, max: Version },
    /// Minimum version required
    Minimum(Version),
    /// Compatible version (^x.y.z semantic)
    Compatible(Version),
    /// Any version acceptable
    Any,
}

/// Registry configuration metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryMetadata {
    pub name: String,
    pub description: String,
    pub version: String,
    pub package_count: usize,
    pub api_version: String,
    pub features: Vec<String>,
}

/// Registry capabilities and features
///
/// Indicates which operations a registry supports.
///
/// # Examples
///
/// ```rust
/// use ggen_marketplace::models::RegistryCapability;
///
/// # fn main() {
/// let capability = RegistryCapability::Search;
/// match capability {
///     RegistryCapability::Search => assert!(true),
///     RegistryCapability::Publish => assert!(true),
///     RegistryCapability::Delete => assert!(true),
///     RegistryCapability::Analytics => assert!(true),
///     RegistryCapability::Webhooks => assert!(true),
///     RegistryCapability::Mirroring => assert!(true),
/// }
/// # }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegistryCapability {
    /// Registry supports search operations
    Search,
    /// Registry supports publishing packages
    Publish,
    /// Registry supports deleting packages
    Delete,
    /// Registry provides analytics
    Analytics,
    /// Registry supports webhooks
    Webhooks,
    /// Registry supports mirroring
    Mirroring,
}

/// Rate limit configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimit {
    pub requests_per_minute: u32,
    pub burst_size: u32,
}

/// User or organization identity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identity {
    pub id: String,
    pub name: String,
    pub email: Option<String>,
    pub public_key: PublicKey,
    pub metadata: HashMap<String, String>,
}
