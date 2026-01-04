//! T011: Manifest Serialization - YAML to TOML Conversion
//!
//! Converts gpack YAML manifests to Cargo.toml format for crates.io publication.
//! Handles dependencies, optional fields, features, and metadata mapping.
//!
//! ## Design
//!
//! - **GpackManifest**: The source YAML format used by ggen marketplace
//! - **CargoManifest**: The target TOML format required by crates.io
//! - **ManifestConverter**: Bidirectional conversion between formats
//!
//! ## Chicago TDD Note
//!
//! Tests verify observable state changes (file contents, parsed structures)
//! not internal implementation details.

use crate::error::{Error, Result};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Gpack manifest format (YAML source)
///
/// This is the developer-facing format used in ggen marketplace packages.
/// It provides a more ergonomic YAML syntax that gets converted to Cargo.toml.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GpackManifest {
    /// Package section
    pub package: GpackPackage,
    /// Dependencies section (name -> version or detailed spec)
    #[serde(default)]
    pub dependencies: IndexMap<String, DependencySpec>,
    /// Dev dependencies
    #[serde(default)]
    pub dev_dependencies: IndexMap<String, DependencySpec>,
    /// Build dependencies
    #[serde(default)]
    pub build_dependencies: IndexMap<String, DependencySpec>,
    /// Features section
    #[serde(default)]
    pub features: IndexMap<String, Vec<String>>,
    /// Target-specific dependencies (e.g., [target.'cfg(unix)'.dependencies])
    #[serde(default)]
    pub target: IndexMap<String, TargetDependencies>,
    /// Workspace configuration (if this is a workspace root)
    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,
}

/// Package metadata in gpack format
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GpackPackage {
    /// Package name (must follow crate naming rules)
    pub name: String,
    /// SemVer version string
    pub version: String,
    /// Rust edition (2015, 2018, 2021, 2024)
    #[serde(default = "default_edition")]
    pub edition: String,
    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,
    /// Short description
    #[serde(default)]
    pub description: Option<String>,
    /// SPDX license identifier
    #[serde(default)]
    pub license: Option<String>,
    /// License file path (alternative to license)
    #[serde(default)]
    pub license_file: Option<String>,
    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,
    /// Homepage URL
    #[serde(default)]
    pub homepage: Option<String>,
    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// README file path
    #[serde(default)]
    pub readme: Option<String>,
    /// Keywords for crates.io (max 5)
    #[serde(default)]
    pub keywords: Vec<String>,
    /// Categories for crates.io (max 5)
    #[serde(default)]
    pub categories: Vec<String>,
    /// Files to exclude from package
    #[serde(default)]
    pub exclude: Vec<String>,
    /// Files to include in package
    #[serde(default)]
    pub include: Vec<String>,
    /// Whether to publish this package
    #[serde(default = "default_publish")]
    pub publish: bool,
    /// FMEA requirement - must have failure mode documentation
    #[serde(default)]
    pub fmea_documented: Option<bool>,
}

fn default_edition() -> String {
    "2021".to_string()
}

fn default_publish() -> bool {
    true
}

/// Dependency specification (can be simple version or detailed)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version string (e.g., "1.0")
    Version(String),
    /// Detailed specification with optional fields
    Detailed(DetailedDependency),
}

impl DependencySpec {
    /// Get the version requirement from any dependency spec
    pub fn version(&self) -> Option<&str> {
        match self {
            Self::Version(v) => Some(v),
            Self::Detailed(d) => d.version.as_deref(),
        }
    }

    /// Check if this is an optional dependency
    pub fn is_optional(&self) -> bool {
        match self {
            Self::Version(_) => false,
            Self::Detailed(d) => d.optional.unwrap_or(false),
        }
    }
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DetailedDependency {
    /// Version requirement
    #[serde(default)]
    pub version: Option<String>,
    /// Path to local crate
    #[serde(default)]
    pub path: Option<String>,
    /// Git repository URL
    #[serde(default)]
    pub git: Option<String>,
    /// Git branch
    #[serde(default)]
    pub branch: Option<String>,
    /// Git tag
    #[serde(default)]
    pub tag: Option<String>,
    /// Git revision
    #[serde(default)]
    pub rev: Option<String>,
    /// Features to enable
    #[serde(default)]
    pub features: Vec<String>,
    /// Use default features
    #[serde(default = "default_true")]
    pub default_features: bool,
    /// Whether this dependency is optional
    #[serde(default)]
    pub optional: Option<bool>,
    /// Package name (if different from key)
    #[serde(default)]
    pub package: Option<String>,
    /// Registry to use
    #[serde(default)]
    pub registry: Option<String>,
}

fn default_true() -> bool {
    true
}

/// Target-specific dependencies
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct TargetDependencies {
    /// Dependencies for this target
    #[serde(default)]
    pub dependencies: IndexMap<String, DependencySpec>,
    /// Dev dependencies for this target
    #[serde(default)]
    pub dev_dependencies: IndexMap<String, DependencySpec>,
    /// Build dependencies for this target
    #[serde(default)]
    pub build_dependencies: IndexMap<String, DependencySpec>,
}

/// Workspace configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct WorkspaceConfig {
    /// Workspace members (glob patterns)
    pub members: Vec<String>,
    /// Packages to exclude from workspace
    #[serde(default)]
    pub exclude: Vec<String>,
    /// Dependency resolver version
    #[serde(default)]
    pub resolver: Option<String>,
}

/// Cargo.toml manifest format (TOML target)
///
/// This is the format required by crates.io for publication.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CargoManifest {
    /// Package section
    pub package: CargoPackage,
    /// Dependencies section
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub dependencies: IndexMap<String, toml::Value>,
    /// Dev dependencies
    #[serde(
        default,
        skip_serializing_if = "IndexMap::is_empty",
        rename = "dev-dependencies"
    )]
    pub dev_dependencies: IndexMap<String, toml::Value>,
    /// Build dependencies
    #[serde(
        default,
        skip_serializing_if = "IndexMap::is_empty",
        rename = "build-dependencies"
    )]
    pub build_dependencies: IndexMap<String, toml::Value>,
    /// Features section
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub features: IndexMap<String, Vec<String>>,
    /// Target-specific sections
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub target: IndexMap<String, toml::Value>,
    /// Workspace configuration
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace: Option<CargoWorkspace>,
}

/// Package section in Cargo.toml
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CargoPackage {
    /// Package name
    pub name: String,
    /// Version
    pub version: String,
    /// Edition
    #[serde(default)]
    pub edition: Option<String>,
    /// Authors
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub authors: Vec<String>,
    /// Description
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// License
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    /// License file
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        rename = "license-file"
    )]
    pub license_file: Option<String>,
    /// Repository
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    /// Homepage
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,
    /// Documentation
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
    /// README
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub readme: Option<String>,
    /// Keywords
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub keywords: Vec<String>,
    /// Categories
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub categories: Vec<String>,
    /// Exclude patterns
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exclude: Vec<String>,
    /// Include patterns
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub include: Vec<String>,
    /// Publish configuration
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub publish: Option<bool>,
}

/// Workspace section in Cargo.toml
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CargoWorkspace {
    /// Workspace members
    pub members: Vec<String>,
    /// Excluded packages
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exclude: Vec<String>,
    /// Resolver version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resolver: Option<String>,
}

/// Manifest converter for bidirectional YAML <-> TOML conversion
#[derive(Debug, Default)]
pub struct ManifestConverter;

impl ManifestConverter {
    /// Create a new manifest converter
    pub fn new() -> Self {
        Self
    }

    /// Convert YAML gpack manifest to TOML Cargo.toml format
    pub fn yaml_to_toml(&self, gpack: &GpackManifest) -> Result<CargoManifest> {
        let package = CargoPackage {
            name: gpack.package.name.clone(),
            version: gpack.package.version.clone(),
            edition: Some(gpack.package.edition.clone()),
            authors: gpack.package.authors.clone(),
            description: gpack.package.description.clone(),
            license: gpack.package.license.clone(),
            license_file: gpack.package.license_file.clone(),
            repository: gpack.package.repository.clone(),
            homepage: gpack.package.homepage.clone(),
            documentation: gpack.package.documentation.clone(),
            readme: gpack.package.readme.clone(),
            keywords: gpack.package.keywords.clone(),
            categories: gpack.package.categories.clone(),
            exclude: gpack.package.exclude.clone(),
            include: gpack.package.include.clone(),
            publish: if gpack.package.publish { None } else { Some(false) },
        };

        let dependencies = self.convert_dependencies(&gpack.dependencies)?;
        let dev_dependencies = self.convert_dependencies(&gpack.dev_dependencies)?;
        let build_dependencies = self.convert_dependencies(&gpack.build_dependencies)?;

        let target = self.convert_targets(&gpack.target)?;

        let workspace = gpack.workspace.as_ref().map(|ws| CargoWorkspace {
            members: ws.members.clone(),
            exclude: ws.exclude.clone(),
            resolver: ws.resolver.clone(),
        });

        Ok(CargoManifest {
            package,
            dependencies,
            dev_dependencies,
            build_dependencies,
            features: gpack.features.clone(),
            target,
            workspace,
        })
    }

    /// Convert TOML Cargo.toml to YAML gpack manifest format
    pub fn toml_to_yaml(&self, cargo: &CargoManifest) -> Result<GpackManifest> {
        let package = GpackPackage {
            name: cargo.package.name.clone(),
            version: cargo.package.version.clone(),
            edition: cargo.package.edition.clone().unwrap_or_else(default_edition),
            authors: cargo.package.authors.clone(),
            description: cargo.package.description.clone(),
            license: cargo.package.license.clone(),
            license_file: cargo.package.license_file.clone(),
            repository: cargo.package.repository.clone(),
            homepage: cargo.package.homepage.clone(),
            documentation: cargo.package.documentation.clone(),
            readme: cargo.package.readme.clone(),
            keywords: cargo.package.keywords.clone(),
            categories: cargo.package.categories.clone(),
            exclude: cargo.package.exclude.clone(),
            include: cargo.package.include.clone(),
            publish: cargo.package.publish.unwrap_or(true),
            fmea_documented: None,
        };

        let dependencies = self.convert_toml_dependencies(&cargo.dependencies)?;
        let dev_dependencies = self.convert_toml_dependencies(&cargo.dev_dependencies)?;
        let build_dependencies = self.convert_toml_dependencies(&cargo.build_dependencies)?;
        let target = self.convert_toml_targets(&cargo.target)?;

        let workspace = cargo.workspace.as_ref().map(|ws| WorkspaceConfig {
            members: ws.members.clone(),
            exclude: ws.exclude.clone(),
            resolver: ws.resolver.clone(),
        });

        Ok(GpackManifest {
            package,
            dependencies,
            dev_dependencies,
            build_dependencies,
            features: cargo.features.clone(),
            target,
            workspace,
        })
    }

    /// Parse a YAML string to GpackManifest
    pub fn parse_yaml(&self, yaml_str: &str) -> Result<GpackManifest> {
        serde_yaml::from_str(yaml_str).map_err(|e| Error::ConfigError(format!("Invalid YAML: {e}")))
    }

    /// Parse a TOML string to CargoManifest
    pub fn parse_toml(&self, toml_str: &str) -> Result<CargoManifest> {
        toml::from_str(toml_str).map_err(|e| Error::ConfigError(format!("Invalid TOML: {e}")))
    }

    /// Serialize GpackManifest to YAML string
    pub fn to_yaml(&self, gpack: &GpackManifest) -> Result<String> {
        serde_yaml::to_string(gpack).map_err(|e| Error::ConfigError(format!("YAML error: {e}")))
    }

    /// Serialize CargoManifest to TOML string
    pub fn to_toml(&self, cargo: &CargoManifest) -> Result<String> {
        toml::to_string_pretty(cargo).map_err(|e| Error::ConfigError(format!("TOML error: {e}")))
    }

    /// Load gpack manifest from YAML file
    pub fn load_yaml_file(&self, path: &Path) -> Result<GpackManifest> {
        let content = std::fs::read_to_string(path)?;
        self.parse_yaml(&content)
    }

    /// Load Cargo manifest from TOML file
    pub fn load_toml_file(&self, path: &Path) -> Result<CargoManifest> {
        let content = std::fs::read_to_string(path)?;
        self.parse_toml(&content)
    }

    /// Write gpack manifest to YAML file
    pub fn write_yaml_file(&self, gpack: &GpackManifest, path: &Path) -> Result<()> {
        let yaml = self.to_yaml(gpack)?;
        std::fs::write(path, yaml)?;
        Ok(())
    }

    /// Write Cargo manifest to TOML file
    pub fn write_toml_file(&self, cargo: &CargoManifest, path: &Path) -> Result<()> {
        let toml = self.to_toml(cargo)?;
        std::fs::write(path, toml)?;
        Ok(())
    }

    // ========== Internal Conversion Helpers ==========

    fn convert_dependencies(
        &self, deps: &IndexMap<String, DependencySpec>,
    ) -> Result<IndexMap<String, toml::Value>> {
        let mut result = IndexMap::new();

        for (name, spec) in deps {
            let value = match spec {
                DependencySpec::Version(v) => toml::Value::String(v.clone()),
                DependencySpec::Detailed(d) => self.detailed_to_toml(d)?,
            };
            result.insert(name.clone(), value);
        }

        Ok(result)
    }

    fn detailed_to_toml(&self, dep: &DetailedDependency) -> Result<toml::Value> {
        let mut table = toml::map::Map::new();

        if let Some(v) = &dep.version {
            table.insert("version".to_string(), toml::Value::String(v.clone()));
        }
        if let Some(p) = &dep.path {
            table.insert("path".to_string(), toml::Value::String(p.clone()));
        }
        if let Some(g) = &dep.git {
            table.insert("git".to_string(), toml::Value::String(g.clone()));
        }
        if let Some(b) = &dep.branch {
            table.insert("branch".to_string(), toml::Value::String(b.clone()));
        }
        if let Some(t) = &dep.tag {
            table.insert("tag".to_string(), toml::Value::String(t.clone()));
        }
        if let Some(r) = &dep.rev {
            table.insert("rev".to_string(), toml::Value::String(r.clone()));
        }
        if !dep.features.is_empty() {
            let features: Vec<toml::Value> =
                dep.features.iter().map(|f| toml::Value::String(f.clone())).collect();
            table.insert("features".to_string(), toml::Value::Array(features));
        }
        if !dep.default_features {
            table.insert("default-features".to_string(), toml::Value::Boolean(false));
        }
        if let Some(opt) = dep.optional {
            table.insert("optional".to_string(), toml::Value::Boolean(opt));
        }
        if let Some(p) = &dep.package {
            table.insert("package".to_string(), toml::Value::String(p.clone()));
        }
        if let Some(r) = &dep.registry {
            table.insert("registry".to_string(), toml::Value::String(r.clone()));
        }

        Ok(toml::Value::Table(table))
    }

    fn convert_targets(
        &self, targets: &IndexMap<String, TargetDependencies>,
    ) -> Result<IndexMap<String, toml::Value>> {
        let mut result = IndexMap::new();

        for (target, deps) in targets {
            let mut target_table = toml::map::Map::new();

            if !deps.dependencies.is_empty() {
                let converted = self.convert_dependencies(&deps.dependencies)?;
                let table: toml::map::Map<String, toml::Value> = converted.into_iter().collect();
                target_table.insert("dependencies".to_string(), toml::Value::Table(table));
            }
            if !deps.dev_dependencies.is_empty() {
                let converted = self.convert_dependencies(&deps.dev_dependencies)?;
                let table: toml::map::Map<String, toml::Value> = converted.into_iter().collect();
                target_table.insert("dev-dependencies".to_string(), toml::Value::Table(table));
            }
            if !deps.build_dependencies.is_empty() {
                let converted = self.convert_dependencies(&deps.build_dependencies)?;
                let table: toml::map::Map<String, toml::Value> = converted.into_iter().collect();
                target_table.insert("build-dependencies".to_string(), toml::Value::Table(table));
            }

            if !target_table.is_empty() {
                result.insert(target.clone(), toml::Value::Table(target_table));
            }
        }

        Ok(result)
    }

    fn convert_toml_dependencies(
        &self, deps: &IndexMap<String, toml::Value>,
    ) -> Result<IndexMap<String, DependencySpec>> {
        let mut result = IndexMap::new();

        for (name, value) in deps {
            let spec = match value {
                toml::Value::String(s) => DependencySpec::Version(s.clone()),
                toml::Value::Table(t) => DependencySpec::Detailed(self.toml_to_detailed(t)?),
                _ => {
                    return Err(Error::ConfigError(format!(
                        "Invalid dependency format for {}",
                        name
                    )))
                }
            };
            result.insert(name.clone(), spec);
        }

        Ok(result)
    }

    fn toml_to_detailed(&self, table: &toml::map::Map<String, toml::Value>) -> Result<DetailedDependency> {
        let get_string = |key: &str| -> Option<String> {
            table.get(key).and_then(|v| v.as_str()).map(String::from)
        };

        let features = table
            .get("features")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        let default_features = table
            .get("default-features")
            .and_then(|v| v.as_bool())
            .unwrap_or(true);

        let optional = table.get("optional").and_then(|v| v.as_bool());

        Ok(DetailedDependency {
            version: get_string("version"),
            path: get_string("path"),
            git: get_string("git"),
            branch: get_string("branch"),
            tag: get_string("tag"),
            rev: get_string("rev"),
            features,
            default_features,
            optional,
            package: get_string("package"),
            registry: get_string("registry"),
        })
    }

    fn convert_toml_targets(
        &self, targets: &IndexMap<String, toml::Value>,
    ) -> Result<IndexMap<String, TargetDependencies>> {
        let mut result = IndexMap::new();

        for (target, value) in targets {
            if let toml::Value::Table(table) = value {
                let mut target_deps = TargetDependencies::default();

                if let Some(toml::Value::Table(deps)) = table.get("dependencies") {
                    let deps_map: IndexMap<String, toml::Value> =
                        deps.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    target_deps.dependencies = self.convert_toml_dependencies(&deps_map)?;
                }
                if let Some(toml::Value::Table(deps)) = table.get("dev-dependencies") {
                    let deps_map: IndexMap<String, toml::Value> =
                        deps.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    target_deps.dev_dependencies = self.convert_toml_dependencies(&deps_map)?;
                }
                if let Some(toml::Value::Table(deps)) = table.get("build-dependencies") {
                    let deps_map: IndexMap<String, toml::Value> =
                        deps.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    target_deps.build_dependencies = self.convert_toml_dependencies(&deps_map)?;
                }

                if !target_deps.dependencies.is_empty()
                    || !target_deps.dev_dependencies.is_empty()
                    || !target_deps.build_dependencies.is_empty()
                {
                    result.insert(target.clone(), target_deps);
                }
            }
        }

        Ok(result)
    }
}

// ==================== TESTS ====================

#[cfg(test)]
mod tests {
    use super::*;

    // Test fixture: minimal valid gpack manifest YAML
    fn minimal_gpack_yaml() -> &'static str {
        r#"
package:
  name: my-crate
  version: 1.0.0
"#
    }

    // Test fixture: full gpack manifest YAML
    fn full_gpack_yaml() -> &'static str {
        r#"
package:
  name: my-crate
  version: 1.2.3
  edition: "2021"
  authors:
    - "Test Author <test@example.com>"
  description: A test package
  license: MIT
  repository: https://github.com/test/my-crate
  homepage: https://example.com
  keywords:
    - test
    - example
  categories:
    - development-tools

dependencies:
  serde: "1.0"
  tokio:
    version: "1.0"
    features:
      - full
    optional: true

dev_dependencies:
  proptest: "1.0"

features:
  default:
    - serde
  full:
    - tokio
"#
    }

    #[test]
    fn test_parse_minimal_yaml() {
        let converter = ManifestConverter::new();
        let result = converter.parse_yaml(minimal_gpack_yaml());
        assert!(result.is_ok());

        let manifest = result.unwrap();
        assert_eq!(manifest.package.name, "my-crate");
        assert_eq!(manifest.package.version, "1.0.0");
        assert_eq!(manifest.package.edition, "2021"); // default
    }

    #[test]
    fn test_parse_full_yaml() {
        let converter = ManifestConverter::new();
        let manifest = converter.parse_yaml(full_gpack_yaml()).unwrap();

        assert_eq!(manifest.package.name, "my-crate");
        assert_eq!(manifest.package.version, "1.2.3");
        assert_eq!(manifest.package.edition, "2021");
        assert_eq!(manifest.package.authors.len(), 1);
        assert_eq!(manifest.package.description, Some("A test package".to_string()));
        assert_eq!(manifest.package.license, Some("MIT".to_string()));
        assert_eq!(manifest.package.keywords.len(), 2);
        assert_eq!(manifest.package.categories.len(), 1);

        // Check dependencies
        assert_eq!(manifest.dependencies.len(), 2);
        assert!(matches!(
            manifest.dependencies.get("serde"),
            Some(DependencySpec::Version(v)) if v == "1.0"
        ));

        let tokio_dep = manifest.dependencies.get("tokio").unwrap();
        assert!(tokio_dep.is_optional());

        // Check features
        assert_eq!(manifest.features.len(), 2);
        assert!(manifest.features.contains_key("default"));
        assert!(manifest.features.contains_key("full"));
    }

    #[test]
    fn test_yaml_to_toml_conversion() {
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(full_gpack_yaml()).unwrap();
        let cargo = converter.yaml_to_toml(&gpack).unwrap();

        // Verify package section
        assert_eq!(cargo.package.name, "my-crate");
        assert_eq!(cargo.package.version, "1.2.3");
        assert_eq!(cargo.package.edition, Some("2021".to_string()));

        // Verify dependencies converted
        assert_eq!(cargo.dependencies.len(), 2);
        assert!(cargo.dependencies.contains_key("serde"));
        assert!(cargo.dependencies.contains_key("tokio"));

        // Verify features preserved
        assert_eq!(cargo.features.len(), 2);
    }

    #[test]
    fn test_toml_to_yaml_conversion() {
        let converter = ManifestConverter::new();

        // First convert YAML -> TOML
        let gpack = converter.parse_yaml(full_gpack_yaml()).unwrap();
        let cargo = converter.yaml_to_toml(&gpack).unwrap();

        // Then convert back TOML -> YAML
        let gpack_roundtrip = converter.toml_to_yaml(&cargo).unwrap();

        // Verify roundtrip preserves data
        assert_eq!(gpack_roundtrip.package.name, gpack.package.name);
        assert_eq!(gpack_roundtrip.package.version, gpack.package.version);
        assert_eq!(gpack_roundtrip.dependencies.len(), gpack.dependencies.len());
    }

    #[test]
    fn test_serialize_to_toml_string() {
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(minimal_gpack_yaml()).unwrap();
        let cargo = converter.yaml_to_toml(&gpack).unwrap();
        let toml_str = converter.to_toml(&cargo).unwrap();

        // Verify TOML string contains expected content
        assert!(toml_str.contains("[package]"));
        assert!(toml_str.contains("name = \"my-crate\""));
        assert!(toml_str.contains("version = \"1.0.0\""));
    }

    #[test]
    fn test_serialize_to_yaml_string() {
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(minimal_gpack_yaml()).unwrap();
        let yaml_str = converter.to_yaml(&gpack).unwrap();

        // Verify YAML string contains expected content
        assert!(yaml_str.contains("name: my-crate"));
        assert!(yaml_str.contains("version: 1.0.0"));
    }

    #[test]
    fn test_dependency_spec_version() {
        let simple = DependencySpec::Version("1.0".to_string());
        assert_eq!(simple.version(), Some("1.0"));
        assert!(!simple.is_optional());

        let detailed = DependencySpec::Detailed(DetailedDependency {
            version: Some("2.0".to_string()),
            optional: Some(true),
            ..Default::default()
        });
        assert_eq!(detailed.version(), Some("2.0"));
        assert!(detailed.is_optional());
    }

    #[test]
    fn test_detailed_dependency_git() {
        let yaml = r#"
package:
  name: test
  version: 0.1.0

dependencies:
  my-dep:
    git: https://github.com/user/repo
    branch: main
    features:
      - feature1
"#;
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(yaml).unwrap();

        let dep = gpack.dependencies.get("my-dep").unwrap();
        if let DependencySpec::Detailed(d) = dep {
            assert_eq!(d.git, Some("https://github.com/user/repo".to_string()));
            assert_eq!(d.branch, Some("main".to_string()));
            assert_eq!(d.features, vec!["feature1"]);
        } else {
            panic!("Expected detailed dependency");
        }
    }

    #[test]
    fn test_target_dependencies() {
        let yaml = r#"
package:
  name: test
  version: 0.1.0

target:
  "cfg(unix)":
    dependencies:
      libc: "0.2"
  "cfg(windows)":
    dependencies:
      winapi: "0.3"
"#;
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(yaml).unwrap();

        assert_eq!(gpack.target.len(), 2);
        assert!(gpack.target.contains_key("cfg(unix)"));
        assert!(gpack.target.contains_key("cfg(windows)"));

        let unix_deps = &gpack.target.get("cfg(unix)").unwrap().dependencies;
        assert!(unix_deps.contains_key("libc"));
    }

    #[test]
    fn test_workspace_config() {
        let yaml = r#"
package:
  name: test
  version: 0.1.0

workspace:
  members:
    - crates/*
    - tools/cli
  exclude:
    - crates/deprecated
  resolver: "2"
"#;
        let converter = ManifestConverter::new();
        let gpack = converter.parse_yaml(yaml).unwrap();

        let ws = gpack.workspace.as_ref().unwrap();
        assert_eq!(ws.members.len(), 2);
        assert_eq!(ws.exclude.len(), 1);
        assert_eq!(ws.resolver, Some("2".to_string()));
    }

    #[test]
    fn test_invalid_yaml_returns_error() {
        let converter = ManifestConverter::new();
        let result = converter.parse_yaml("not: valid: yaml: at: all");
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_required_fields() {
        let converter = ManifestConverter::new();

        // Missing version
        let result = converter.parse_yaml(
            r#"
package:
  name: test
"#,
        );
        assert!(result.is_err());

        // Missing name
        let result = converter.parse_yaml(
            r#"
package:
  version: 1.0.0
"#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_file_roundtrip() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let yaml_path = temp_dir.path().join("gpack.yaml");
        let toml_path = temp_dir.path().join("Cargo.toml");

        let converter = ManifestConverter::new();
        let original = converter.parse_yaml(full_gpack_yaml()).unwrap();

        // Write YAML
        converter.write_yaml_file(&original, &yaml_path).unwrap();

        // Read back YAML
        let loaded_yaml = converter.load_yaml_file(&yaml_path).unwrap();
        assert_eq!(loaded_yaml.package.name, original.package.name);

        // Convert to TOML and write
        let cargo = converter.yaml_to_toml(&original).unwrap();
        converter.write_toml_file(&cargo, &toml_path).unwrap();

        // Read back TOML
        let loaded_toml = converter.load_toml_file(&toml_path).unwrap();
        assert_eq!(loaded_toml.package.name, original.package.name);
    }
}

impl Default for DetailedDependency {
    fn default() -> Self {
        Self {
            version: None,
            path: None,
            git: None,
            branch: None,
            tag: None,
            rev: None,
            features: Vec::new(),
            default_features: true,
            optional: None,
            package: None,
            registry: None,
        }
    }
}
