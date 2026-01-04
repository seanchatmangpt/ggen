//! Package manifest parsing and validation (T005)
//!
//! This module handles the gpack.toml manifest format, including:
//! - Manifest parsing and serialization
//! - Dependency declarations
//! - Feature flags
//! - Metadata fields
//!
//! ## Validation Rules
//!
//! - `name`: Must end with "-gpack" suffix for marketplace packages
//! - `version`: Must follow SemVer (MAJOR.MINOR.PATCH)
//! - `description`: Maximum 500 characters
//! - `quality_tier`: Required for published packages
//!
//! ## Serialization
//!
//! Supports YAML, TOML, and JSON via serde.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::error::{GpackError, GpackResult};

/// Maximum allowed length for package description
pub const MAX_DESCRIPTION_LENGTH: usize = 500;

/// Required suffix for gpack crate names in marketplace
pub const GPACK_SUFFIX: &str = "-gpack";

/// The main gpack manifest structure (gpack.toml)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GpackManifest {
    /// Package metadata section
    pub package: PackageMetadata,
    /// Dependencies section
    #[serde(default)]
    pub dependencies: HashMap<String, PackageDependency>,
    /// Dev dependencies section
    #[serde(default)]
    pub dev_dependencies: HashMap<String, PackageDependency>,
    /// Feature flags
    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,
    /// Build configuration
    #[serde(default)]
    pub build: Option<BuildConfig>,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    /// Package name (must be unique in registry, should end with -gpack for marketplace)
    pub name: String,
    /// Package version (semver)
    pub version: String,
    /// Package description (max 500 chars)
    #[serde(default)]
    pub description: Option<String>,
    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,
    /// License identifier (SPDX)
    #[serde(default)]
    pub license: Option<String>,
    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,
    /// Package keywords for search
    #[serde(default)]
    pub keywords: Vec<String>,
    /// Package categories
    #[serde(default)]
    pub categories: Vec<String>,
    /// Minimum ggen version required
    #[serde(default)]
    pub ggen_version: Option<String>,
    /// Package homepage URL
    #[serde(default)]
    pub homepage: Option<String>,
    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// Reference to FMEA analysis document
    #[serde(default)]
    pub fmea_reference: Option<String>,
    /// Quality tier classification (Gold, Silver, Bronze)
    #[serde(default)]
    pub quality_tier: Option<QualityTier>,
}

/// Quality tier classification for marketplace packages
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QualityTier {
    /// Highest quality - extensive testing, documentation, FMEA compliance
    Gold,
    /// Good quality - solid testing and documentation
    Silver,
    /// Basic quality - meets minimum requirements
    Bronze,
}

/// A package dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PackageDependency {
    /// Simple version constraint (e.g., "1.0")
    Simple(String),
    /// Detailed dependency specification
    Detailed(DetailedDependency),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetailedDependency {
    /// Version constraint
    pub version: Option<String>,
    /// Git repository URL
    pub git: Option<String>,
    /// Git branch
    pub branch: Option<String>,
    /// Git tag
    pub tag: Option<String>,
    /// Git revision
    pub rev: Option<String>,
    /// Local path
    pub path: Option<String>,
    /// Optional dependency
    #[serde(default)]
    pub optional: bool,
    /// Required features
    #[serde(default)]
    pub features: Vec<String>,
    /// Default features enabled
    #[serde(default = "default_true")]
    pub default_features: bool,
}

fn default_true() -> bool {
    true
}

/// Build configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    /// Build script path
    pub script: Option<String>,
    /// Environment variables
    #[serde(default)]
    pub env: HashMap<String, String>,
}

impl GpackManifest {
    /// Parse a manifest from TOML string
    pub fn from_toml(content: &str) -> GpackResult<Self> {
        toml::from_str(content).map_err(|e| GpackError::ManifestError(e.to_string()))
    }

    /// Serialize manifest to TOML string
    pub fn to_toml(&self) -> GpackResult<String> {
        toml::to_string_pretty(self).map_err(|e| GpackError::ManifestError(e.to_string()))
    }

    /// Parse a manifest from YAML string
    pub fn from_yaml(content: &str) -> GpackResult<Self> {
        serde_yaml::from_str(content).map_err(|e| GpackError::ManifestError(e.to_string()))
    }

    /// Serialize manifest to YAML string
    pub fn to_yaml(&self) -> GpackResult<String> {
        serde_yaml::to_string(self).map_err(|e| GpackError::ManifestError(e.to_string()))
    }

    /// Parse a manifest from JSON string
    pub fn from_json(content: &str) -> GpackResult<Self> {
        serde_json::from_str(content).map_err(|e| GpackError::SerializationError(e.to_string()))
    }

    /// Serialize manifest to JSON string
    pub fn to_json(&self) -> GpackResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| GpackError::SerializationError(e.to_string()))
    }

    /// Validate the manifest
    pub fn validate(&self) -> GpackResult<()> {
        // Validate package name
        if self.package.name.is_empty() {
            return Err(GpackError::ManifestError(
                "Package name cannot be empty".to_string(),
            ));
        }

        // Validate version is valid semver
        if semver::Version::parse(&self.package.version).is_err() {
            return Err(GpackError::ManifestError(format!(
                "Invalid version: {}",
                self.package.version
            )));
        }

        // Validate description length if present
        if let Some(ref desc) = self.package.description {
            if desc.len() > MAX_DESCRIPTION_LENGTH {
                return Err(GpackError::ManifestError(format!(
                    "Description exceeds maximum length of {} characters (got {})",
                    MAX_DESCRIPTION_LENGTH,
                    desc.len()
                )));
            }
        }

        Ok(())
    }

    /// Validate the manifest for marketplace publishing
    /// Requires -gpack suffix, quality tier, and other marketplace requirements
    pub fn validate_for_marketplace(&self) -> GpackResult<()> {
        // First run basic validation
        self.validate()?;

        // Validate gpack suffix for marketplace packages
        if !self.package.name.ends_with(GPACK_SUFFIX) {
            return Err(GpackError::ManifestError(format!(
                "Marketplace packages must end with '{}' suffix, got: {}",
                GPACK_SUFFIX, self.package.name
            )));
        }

        // Quality tier required for marketplace
        if self.package.quality_tier.is_none() {
            return Err(GpackError::ManifestError(
                "Quality tier is required for marketplace packages".to_string(),
            ));
        }

        // At least one author required
        if self.package.authors.is_empty() {
            return Err(GpackError::ManifestError(
                "At least one author is required for marketplace packages".to_string(),
            ));
        }

        // License required
        if self.package.license.is_none() {
            return Err(GpackError::ManifestError(
                "License is required for marketplace packages".to_string(),
            ));
        }

        Ok(())
    }

    /// Check if this is a valid gpack marketplace name
    pub fn is_valid_gpack_name(&self) -> bool {
        self.package.name.ends_with(GPACK_SUFFIX)
    }

    /// Get the base name without the gpack suffix
    pub fn base_name(&self) -> &str {
        self.package
            .name
            .strip_suffix(GPACK_SUFFIX)
            .unwrap_or(&self.package.name)
    }

    /// Get all dependencies (including dev dependencies)
    pub fn all_dependencies(&self) -> impl Iterator<Item = (&String, &PackageDependency)> {
        self.dependencies
            .iter()
            .chain(self.dev_dependencies.iter())
    }
}

impl PackageDependency {
    /// Get the version constraint, if any
    pub fn version(&self) -> Option<&str> {
        match self {
            PackageDependency::Simple(v) => Some(v),
            PackageDependency::Detailed(d) => d.version.as_deref(),
        }
    }

    /// Check if this is a git dependency
    pub fn is_git(&self) -> bool {
        matches!(self, PackageDependency::Detailed(d) if d.git.is_some())
    }

    /// Check if this is a path dependency
    pub fn is_path(&self) -> bool {
        matches!(self, PackageDependency::Detailed(d) if d.path.is_some())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_metadata() -> PackageMetadata {
        PackageMetadata {
            name: "test-package".to_string(),
            version: "1.0.0".to_string(),
            description: Some("A test package".to_string()),
            authors: vec!["Test Author".to_string()],
            license: Some("MIT".to_string()),
            repository: None,
            keywords: vec![],
            categories: vec![],
            ggen_version: None,
            homepage: None,
            documentation: None,
            fmea_reference: None,
            quality_tier: None,
        }
    }

    #[test]
    fn test_parse_simple_manifest() {
        let toml = r#"
[package]
name = "test-package"
version = "1.0.0"
description = "A test package"

[dependencies]
serde = "1.0"
"#;

        let manifest = GpackManifest::from_toml(toml).unwrap();
        assert_eq!(manifest.package.name, "test-package");
        assert_eq!(manifest.package.version, "1.0.0");
        assert!(manifest.dependencies.contains_key("serde"));
    }

    #[test]
    fn test_detailed_dependency() {
        let toml = r#"
[package]
name = "test"
version = "1.0.0"

[dependencies.my-dep]
version = "2.0"
features = ["async"]
optional = true
"#;

        let manifest = GpackManifest::from_toml(toml).unwrap();
        let dep = manifest.dependencies.get("my-dep").unwrap();
        assert!(matches!(dep, PackageDependency::Detailed(_)));
    }

    #[test]
    fn test_validate_empty_name() {
        let manifest = GpackManifest {
            package: PackageMetadata {
                name: String::new(),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.validate().is_err());
    }

    #[test]
    fn test_validate_invalid_version() {
        let manifest = GpackManifest {
            package: PackageMetadata {
                name: "test".to_string(),
                version: "not-a-version".to_string(),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.validate().is_err());
    }

    #[test]
    fn test_validate_description_too_long() {
        let long_desc = "a".repeat(MAX_DESCRIPTION_LENGTH + 1);
        let manifest = GpackManifest {
            package: PackageMetadata {
                description: Some(long_desc),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.validate().is_err());
    }

    #[test]
    fn test_validate_for_marketplace_missing_suffix() {
        let manifest = GpackManifest {
            package: PackageMetadata {
                name: "test-package".to_string(), // Missing -gpack suffix
                quality_tier: Some(QualityTier::Silver),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.validate_for_marketplace().is_err());
    }

    #[test]
    fn test_validate_for_marketplace_success() {
        let manifest = GpackManifest {
            package: PackageMetadata {
                name: "test-package-gpack".to_string(),
                quality_tier: Some(QualityTier::Gold),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.validate_for_marketplace().is_ok());
    }

    #[test]
    fn test_is_valid_gpack_name() {
        let manifest = GpackManifest {
            package: PackageMetadata {
                name: "my-cool-gpack".to_string(),
                ..create_test_metadata()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        assert!(manifest.is_valid_gpack_name());
        assert_eq!(manifest.base_name(), "my-cool");
    }

    #[test]
    fn test_yaml_roundtrip() {
        let manifest = GpackManifest {
            package: create_test_metadata(),
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        let yaml = manifest.to_yaml().unwrap();
        let parsed = GpackManifest::from_yaml(&yaml).unwrap();
        assert_eq!(manifest.package.name, parsed.package.name);
    }

    #[test]
    fn test_json_roundtrip() {
        let manifest = GpackManifest {
            package: create_test_metadata(),
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        };

        let json = manifest.to_json().unwrap();
        let parsed = GpackManifest::from_json(&json).unwrap();
        assert_eq!(manifest.package.name, parsed.package.name);
    }

    #[test]
    fn test_quality_tier_serialization() {
        assert_eq!(
            serde_json::to_string(&QualityTier::Gold).unwrap(),
            "\"gold\""
        );
        assert_eq!(
            serde_json::to_string(&QualityTier::Silver).unwrap(),
            "\"silver\""
        );
        assert_eq!(
            serde_json::to_string(&QualityTier::Bronze).unwrap(),
            "\"bronze\""
        );
    }
}
