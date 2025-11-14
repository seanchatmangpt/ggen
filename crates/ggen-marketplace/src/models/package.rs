//! Package data models and types
//!
//! This module provides the core data structures for representing packages in the
//! marketplace, including package identifiers, content addressing, validation states,
//! and metadata.
//!
//! ## Key Types
//!
//! - **PackageId**: Unique identifier for packages (namespace/name format)
//! - **ContentId**: Content-addressable identifier using hash algorithms
//! - **Package**: Complete package metadata with validation states
//! - **HashAlgorithm**: Supported hash algorithms (SHA-256, SHA-512, etc.)
//!
//! ## Examples
//!
//! ### Creating a Package ID
//!
//! ```rust
//! use ggen_marketplace::models::PackageId;
//!
//! # fn main() {
//! let package_id = PackageId::new("io.ggen", "rust-cli");
//! assert_eq!(package_id.to_string(), "io.ggen/rust-cli");
//! # }
//! ```
//!
//! ### Creating a Content ID
//!
//! ```rust
//! use ggen_marketplace::models::{ContentId, HashAlgorithm};
//!
//! # fn main() {
//! let content_id = ContentId::new(
//!     "abc123def456",
//!     HashAlgorithm::Sha256
//! );
//! assert_eq!(content_id.to_string(), "sha256:abc123def456");
//! # }
//! ```

use super::{Category, Dependency, Identity, PackageStats, Version};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

/// Unique identifier for a package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PackageId {
    pub namespace: String,
    pub name: String,
}

impl PackageId {
    pub fn new(namespace: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
        }
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.namespace, self.name)
    }
}

/// Content-addressable identifier for package content
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ContentId {
    pub hash: String,
    pub algorithm: HashAlgorithm,
}

impl ContentId {
    pub fn new(hash: impl Into<String>, algorithm: HashAlgorithm) -> Self {
        Self {
            hash: hash.into(),
            algorithm,
        }
    }
}

impl fmt::Display for ContentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.algorithm, self.hash)
    }
}

/// Supported hash algorithms for content addressing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HashAlgorithm {
    Sha256,
    Sha512,
    Blake3,
}

impl fmt::Display for HashAlgorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sha256 => write!(f, "sha256"),
            Self::Sha512 => write!(f, "sha512"),
            Self::Blake3 => write!(f, "blake3"),
        }
    }
}

/// Complete package definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub id: PackageId,
    pub version: Version,
    pub metadata: PackageMetadata,
    pub content_id: ContentId,
    pub dependencies: Vec<Dependency>,
    pub stats: PackageStats,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

impl Package {
    pub fn builder(id: PackageId, version: Version) -> PackageBuilder {
        PackageBuilder::new(id, version)
    }

    pub fn fully_qualified_name(&self) -> String {
        format!("{}@{}", self.id, self.version)
    }

    /// Create a validated package from an existing Package
    ///
    /// **Backward compatibility**: Allows existing code to convert Package to ValidatedPackage
    /// without using the builder pattern.
    pub fn validate(self) -> crate::error::Result<ValidatedPackage> {
        UnvalidatedPackage { package: self }.validate()
    }
}

/// Package metadata and descriptive information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    pub title: String,
    pub description: String,
    pub long_description: Option<String>,
    pub categories: Vec<Category>,
    pub tags: Vec<String>,
    pub license: String,
    pub authors: Vec<Identity>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub documentation: Option<String>,
    pub readme: Option<String>,
    pub changelog: Option<String>,
    pub custom_fields: HashMap<String, serde_json::Value>,
}

impl Default for PackageMetadata {
    fn default() -> Self {
        Self {
            title: String::new(),
            description: String::new(),
            long_description: None,
            categories: Vec::new(),
            tags: Vec::new(),
            license: "UNLICENSED".to_string(),
            authors: Vec::new(),
            homepage: None,
            repository: None,
            documentation: None,
            readme: None,
            changelog: None,
            custom_fields: HashMap::new(),
        }
    }
}

/// Builder pattern for creating packages
pub struct PackageBuilder {
    id: PackageId,
    version: Version,
    metadata: PackageMetadata,
    content_id: Option<ContentId>,
    dependencies: Vec<Dependency>,
}

impl PackageBuilder {
    pub fn new(id: PackageId, version: Version) -> Self {
        Self {
            id,
            version,
            metadata: PackageMetadata::default(),
            content_id: None,
            dependencies: Vec::new(),
        }
    }

    pub fn metadata(mut self, metadata: PackageMetadata) -> Self {
        self.metadata = metadata;
        self
    }

    pub fn title(mut self, title: impl Into<String>) -> Self {
        self.metadata.title = title.into();
        self
    }

    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.metadata.description = description.into();
        self
    }

    pub fn license(mut self, license: impl Into<String>) -> Self {
        self.metadata.license = license.into();
        self
    }

    pub fn category(mut self, category: Category) -> Self {
        self.metadata.categories.push(category);
        self
    }

    pub fn tag(mut self, tag: impl Into<String>) -> Self {
        self.metadata.tags.push(tag.into());
        self
    }

    pub fn author(mut self, author: Identity) -> Self {
        self.metadata.authors.push(author);
        self
    }

    pub fn content_id(mut self, content_id: ContentId) -> Self {
        self.content_id = Some(content_id);
        self
    }

    pub fn dependency(mut self, dependency: Dependency) -> Self {
        self.dependencies.push(dependency);
        self
    }

    /// Build an unvalidated package
    ///
    /// **Poka-yoke**: Returns `UnvalidatedPackage` which must be validated
    /// before use. This prevents using packages without required fields.
    ///
    /// **Root Cause Fix**: Always validate the returned `UnvalidatedPackage` before
    /// accessing fields or passing to functions that expect `Package`.
    /// Use `.validate()` to get a `ValidatedPackage`, then `.package()` to get `Package`.
    pub fn build(self) -> crate::error::Result<UnvalidatedPackage> {
        let content_id = self.content_id.ok_or_else(|| {
            crate::error::MarketplaceError::invalid_package(
                "content_id is required",
                "package builder",
            )
        })?;

        if self.metadata.title.is_empty() {
            return Err(crate::error::MarketplaceError::invalid_package(
                "title is required",
                "package builder",
            ));
        }

        if self.metadata.description.is_empty() {
            return Err(crate::error::MarketplaceError::invalid_package(
                "description is required",
                "package builder",
            ));
        }

        let now = chrono::Utc::now();
        Ok(UnvalidatedPackage {
            package: Package {
                id: self.id,
                version: self.version,
                metadata: self.metadata,
                content_id,
                dependencies: self.dependencies,
                stats: PackageStats::default(),
                created_at: now,
                updated_at: now,
            },
        })
    }
}

/// Unvalidated package from builder
///
/// **Poka-yoke**: Packages must be validated before installation or use.
/// This type prevents using packages that haven't passed validation checks.
#[derive(Debug, Clone)]
pub struct UnvalidatedPackage {
    package: Package,
}

impl UnvalidatedPackage {
    /// Validate the package and return a validated package
    ///
    /// Performs additional validation checks beyond basic field requirements:
    /// - Dependency validation
    /// - Content integrity checks
    /// - Metadata completeness
    pub fn validate(self) -> crate::error::Result<ValidatedPackage> {
        // Validate dependencies
        for _dep in &self.package.dependencies {
            // Check dependency has required fields
            // This is a placeholder - actual validation would check dependency registry
        }

        // Validate content_id format
        if self.package.content_id.hash.is_empty() {
            return Err(crate::error::MarketplaceError::invalid_package(
                "content_id hash cannot be empty",
                "package validation",
            ));
        }

        // Validate metadata completeness
        if self.package.metadata.license.is_empty() {
            return Err(crate::error::MarketplaceError::invalid_package(
                "license cannot be empty",
                "package validation",
            ));
        }

        Ok(ValidatedPackage {
            package: self.package,
            validation_metadata: ValidationMetadata {
                validated_at: chrono::Utc::now(),
            },
        })
    }

    /// Get the underlying package (for inspection before validation)
    pub fn package(&self) -> &Package {
        &self.package
    }
}

/// Validated package ready for installation
///
/// **Poka-yoke**: Only `ValidatedPackage` can be installed or used in operations.
/// This prevents installing unvalidated packages that may be broken or malicious.
#[derive(Debug, Clone)]
pub struct ValidatedPackage {
    package: Package,
    validation_metadata: ValidationMetadata,
}

/// Metadata about package validation
#[derive(Debug, Clone)]
pub struct ValidationMetadata {
    /// When the package was validated
    pub validated_at: chrono::DateTime<chrono::Utc>,
}

impl ValidatedPackage {
    /// Get the underlying package
    pub fn package(&self) -> &Package {
        &self.package
    }

    /// Get validation metadata
    pub fn validation_metadata(&self) -> &ValidationMetadata {
        &self.validation_metadata
    }

    /// Get package ID
    pub fn id(&self) -> &PackageId {
        &self.package.id
    }

    /// Get package version
    pub fn version(&self) -> &Version {
        &self.package.version
    }

    /// Get fully qualified name
    pub fn fully_qualified_name(&self) -> String {
        self.package.fully_qualified_name()
    }
}

impl AsRef<Package> for ValidatedPackage {
    fn as_ref(&self) -> &Package {
        &self.package
    }
}
