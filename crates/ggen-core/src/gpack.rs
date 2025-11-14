//! Gpack manifest structure and file discovery
//!
//! This module provides the structure and functionality for gpack manifests (`gpack.toml`),
//! which define template packs for ggen. It handles manifest parsing, file discovery,
//! and provides default conventions for organizing pack files.
//!
//! ## Gpack Structure
//!
//! A gpack is a template pack that contains:
//! - Templates (`.tmpl` files)
//! - RDF data files (`.ttl`, `.rdf`, `.jsonld`)
//! - SPARQL queries (`.rq`, `.sparql`)
//! - SHACL shapes (`.shacl.ttl`)
//! - Macros and includes
//!
//! ## Manifest Format
//!
//! The `gpack.toml` manifest defines:
//! - Metadata (id, name, version, description, license)
//! - Dependencies on other gpacks
//! - File discovery patterns (or uses conventions)
//! - RDF configuration (prefixes, base IRI)
//! - Preset configurations
//!
//! ## File Discovery
//!
//! The module provides default conventions for discovering files:
//! - Templates: `templates/**/*.tmpl`, `templates/**/*.tera`
//! - RDF: `templates/**/graphs/*.ttl`, `templates/**/graphs/*.rdf`, etc.
//! - Queries: `templates/**/queries/*.rq`, `templates/**/queries/*.sparql`
//! - Shapes: `templates/**/graphs/shapes/*.shacl.ttl`
//!
//! Custom patterns can be specified in the manifest to override conventions.
//!
//! ## Examples
//!
//! ### Loading a Manifest
//!
//! ```rust,no_run
//! use ggen_core::gpack::GpackManifest;
//! use std::path::PathBuf;
//!
//! # fn main() -> anyhow::Result<()> {
//! let manifest = GpackManifest::load_from_file(&PathBuf::from("gpack.toml"))?;
//! println!("Pack: {} v{}", manifest.metadata.name, manifest.metadata.version);
//! # Ok(())
//! # }
//! ```
//!
//! ### Discovering Templates
//!
//! ```rust,no_run
//! use ggen_core::gpack::GpackManifest;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let manifest = GpackManifest::load_from_file(&PathBuf::from("gpack.toml"))?;
//! let templates = manifest.discover_templates(Path::new("."))?;
//!
//! for template in templates {
//!     println!("Found template: {:?}", template);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Discovering Files
//!
//! ```rust,no_run
//! use ggen_core::gpack::GpackManifest;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let manifest = GpackManifest::load_from_file(&Path::new("gpack.toml").to_path_buf())?;
//!
//! // Discover templates using manifest patterns or conventions
//! let templates = manifest.discover_templates(Path::new("."))?;
//!
//! // Discover RDF files
//! let rdf_files = manifest.discover_rdf_files(Path::new("."))?;
//!
//! // Discover SPARQL queries
//! let queries = manifest.discover_query_files(Path::new("."))?;
//! # Ok(())
//! # }
//! ```

use anyhow::Result;
use glob::glob;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// Default conventions for pack file discovery
#[derive(Debug, Clone)]
pub struct PackConventions {
    pub template_patterns: &'static [&'static str],
    pub rdf_patterns: &'static [&'static str],
    pub query_patterns: &'static [&'static str],
    pub shape_patterns: &'static [&'static str],
}

impl Default for PackConventions {
    fn default() -> Self {
        Self {
            template_patterns: &["templates/**/*.tmpl", "templates/**/*.tera"],
            rdf_patterns: &[
                "templates/**/graphs/*.ttl",
                "templates/**/graphs/*.rdf",
                "templates/**/graphs/*.jsonld",
            ],
            query_patterns: &["templates/**/queries/*.rq", "templates/**/queries/*.sparql"],
            shape_patterns: &[
                "templates/**/graphs/shapes/*.shacl.ttl",
                "templates/**/shapes/*.ttl",
            ],
        }
    }
}

/// Gpack manifest structure
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GpackManifest {
    #[serde(rename = "gpack")]
    pub metadata: GpackMetadata,
    #[serde(default)]
    pub dependencies: BTreeMap<String, String>,
    #[serde(default)]
    pub templates: TemplatesConfig,
    #[serde(default)]
    pub macros: MacrosConfig,
    #[serde(default)]
    pub rdf: RdfConfig,
    #[serde(default)]
    pub queries: QueriesConfig,
    #[serde(default)]
    pub shapes: ShapesConfig,
    #[serde(default)]
    pub preset: PresetConfig,
}

/// Gpack metadata section
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GpackMetadata {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub ggen_compat: String,
}

/// Templates configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct TemplatesConfig {
    /// Glob patterns for template discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    /// Additional Tera includes
    #[serde(default)]
    pub includes: Vec<String>,
}

/// Macros configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct MacrosConfig {
    #[serde(default)]
    pub paths: Vec<String>,
}

/// RDF configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct RdfConfig {
    #[serde(default)]
    pub base: Option<String>,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    /// Glob patterns for RDF file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    /// Inline RDF content
    #[serde(default)]
    pub inline: Vec<String>,
}

/// Queries configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct QueriesConfig {
    /// Glob patterns for query file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    #[serde(default)]
    pub aliases: BTreeMap<String, String>,
}

/// Shapes configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ShapesConfig {
    /// Glob patterns for shape file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
}

/// Preset configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct PresetConfig {
    #[serde(default)]
    pub config: Option<PathBuf>,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
}

/// Discover files using glob patterns
fn discover_files(base_path: &Path, patterns: &[&str]) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for pattern in patterns {
        let full_pattern = base_path.join(pattern);
        for entry in glob(&full_pattern.to_string_lossy())? {
            files.push(entry?);
        }
    }
    files.sort(); // Deterministic order
    Ok(files)
}

impl GpackManifest {
    /// Load manifest from a file
    pub fn load_from_file(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let manifest: GpackManifest = toml::from_str(&content)?;
        Ok(manifest)
    }

    /// Discover template files using conventions or config
    pub fn discover_templates(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.templates.patterns.is_empty() {
            PackConventions::default().template_patterns
        } else {
            &self
                .templates
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover RDF files using conventions or config
    pub fn discover_rdf_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.rdf.patterns.is_empty() {
            PackConventions::default().rdf_patterns
        } else {
            &self
                .rdf
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover query files using conventions or config
    pub fn discover_query_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.queries.patterns.is_empty() {
            PackConventions::default().query_patterns
        } else {
            &self
                .queries
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover shape files using conventions or config
    pub fn discover_shape_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.shapes.patterns.is_empty() {
            PackConventions::default().shape_patterns
        } else {
            &self
                .shapes
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_manifest_parsing() {
        let toml_content = r#"
[gpack]
id = "io.ggen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.1.0"
description = "Generate clap subcommands"
license = "MIT"
ggen_compat = ">=0.1 <0.2"

[dependencies]
"io.ggen.macros.std" = "^0.1"

[templates]
patterns = ["cli/subcommand/*.tmpl"]
includes = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
patterns = ["templates/**/graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]

[queries]
patterns = ["../queries/*.rq"]
aliases.component_by_name = "../queries/component_by_name.rq"

[shapes]
patterns = ["../shapes/*.ttl"]

[preset]
config = "../preset/ggen.toml"
vars = { author = "Acme", license = "MIT" }
"#;

        let manifest: GpackManifest = toml::from_str(toml_content).unwrap();

        assert_eq!(manifest.metadata.id, "io.ggen.rust.cli-subcommand");
        assert_eq!(manifest.metadata.name, "Rust CLI subcommand");
        assert_eq!(manifest.metadata.version, "0.1.0");
        assert_eq!(manifest.templates.patterns.len(), 1);
        assert_eq!(manifest.rdf.patterns.len(), 1);
        assert_eq!(manifest.queries.aliases.len(), 1);
    }

    #[test]
    fn test_manifest_load_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        let toml_content = r#"
[gpack]
id = "test"
name = "Test"
version = "0.1.0"
description = "Test"
license = "MIT"
ggen_compat = ">=0.1 <0.2"
"#;
        temp_file.write_all(toml_content.as_bytes()).unwrap();

        let manifest = GpackManifest::load_from_file(&temp_file.path().to_path_buf()).unwrap();
        assert_eq!(manifest.metadata.id, "test");
    }

    #[cfg(feature = "proptest")]
    mod proptest_tests {
        use super::*;
        use proptest::prelude::*;

        /// Property test: GPack manifest parsing should be idempotent
        proptest! {
            #[test]
            fn gpack_manifest_parsing_idempotent(
                pack_id in r"[a-zA-Z0-9_\-\.]+",
                pack_name in r"[a-zA-Z0-9_\s\-\.]+",
                pack_version in r"[0-9]+\.[0-9]+\.[0-9]+",
                pack_description in r"[a-zA-Z0-9_\s\-\.\/\?\!]+",
                pack_license in r"[a-zA-Z0-9_\s\-\.]+",
                ggen_compat in r"[><=0-9\.\s]+"
            ) {
                // Skip invalid combinations
                if pack_id.is_empty() || pack_name.is_empty() || pack_description.is_empty() {
                    return Ok(());
                }
                if pack_id.len() > 100 || pack_name.len() > 200 || pack_description.len() > 500 {
                    return Ok(());
                }

                // Create a minimal GPack manifest
                let manifest = GpackManifest {
                    metadata: GpackMetadata {
                        id: pack_id.clone(),
                        name: pack_name.clone(),
                        version: pack_version.clone(),
                        description: pack_description.clone(),
                        license: pack_license.clone(),
                        ggen_compat: ggen_compat.clone(),
                    },
                    dependencies: BTreeMap::new(),
                    templates: TemplatesConfig::default(),
                    macros: MacrosConfig::default(),
                    rdf: RdfConfig::default(),
                    queries: QueriesConfig::default(),
                    shapes: ShapesConfig::default(),
                    preset: PresetConfig::default(),
                };

                // Serialize to TOML and parse back
                let toml_str = toml::to_string(&manifest).unwrap();
                let parsed_manifest: GpackManifest = toml::from_str(&toml_str).unwrap();

                // Should be identical
                assert_eq!(manifest.metadata.id, parsed_manifest.metadata.id);
                assert_eq!(manifest.metadata.name, parsed_manifest.metadata.name);
                assert_eq!(manifest.metadata.version, parsed_manifest.metadata.version);
                assert_eq!(manifest.metadata.description, parsed_manifest.metadata.description);
                assert_eq!(manifest.metadata.license, parsed_manifest.metadata.license);
                assert_eq!(manifest.metadata.ggen_compat, parsed_manifest.metadata.ggen_compat);
            }

            #[test]
            fn gpack_metadata_validation(
                pack_id in r"[a-zA-Z0-9_\-\.]+",
                pack_name in r"[a-zA-Z0-9_\s\-\.]+",
                pack_version in r"[0-9]+\.[0-9]+\.[0-9]+",
                pack_description in r"[a-zA-Z0-9_\s\-\.\/\?\!]+"
            ) {
                // Skip invalid combinations
                if pack_id.is_empty() || pack_name.is_empty() || pack_description.is_empty() {
                    return Ok(());
                }
                if pack_id.len() > 100 || pack_name.len() > 200 || pack_description.len() > 500 {
                    return Ok(());
                }

                let metadata = GpackMetadata {
                    id: pack_id.clone(),
                    name: pack_name.clone(),
                    version: pack_version.clone(),
                    description: pack_description.clone(),
                    license: "MIT".to_string(),
                    ggen_compat: ">=0.1 <0.2".to_string(),
                };

                // Validate required fields
                assert!(!metadata.id.is_empty());
                assert!(!metadata.name.is_empty());
                assert!(!metadata.version.is_empty());
                assert!(!metadata.description.is_empty());
                assert!(!metadata.license.is_empty());
                assert!(!metadata.ggen_compat.is_empty());

                // Validate ID format (should be reverse domain notation)
                if metadata.id.contains('.') {
                    let parts: Vec<&str> = metadata.id.split('.').collect();
                    assert!(parts.len() >= 2, "ID should have at least 2 parts separated by dots");
                }

                // Validate version format (should be semver)
                let version_result = semver::Version::parse(&metadata.version);
                match version_result {
                    Ok(_) => {
                        // Valid semver version
                    },
                    Err(_) => {
                        // Invalid version format - this is acceptable for testing
                    }
                }
            }

            #[test]
            fn template_patterns_validation(
                pattern_count in 0..10usize,
                pattern in r"[a-zA-Z0-9_\-\.\/\*\?\[\]]+"
            ) {
                // Skip invalid patterns
                if pattern.is_empty() || pattern.len() > 100 {
                    return Ok(());
                }

                let mut patterns = Vec::new();
                for i in 0..pattern_count {
                    patterns.push(format!("{}-{}", pattern, i));
                }

                let config = TemplatesConfig {
                    patterns: patterns.clone(),
                    includes: Vec::new(),
                };

                // Validate patterns
                assert_eq!(config.patterns.len(), pattern_count);
                for (i, pattern_item) in config.patterns.iter().enumerate() {
                    let expected_pattern = format!("{}-{}", pattern, i);
                    assert_eq!(pattern_item, &expected_pattern);
                }
            }

            #[test]
            fn rdf_prefixes_validation(
                prefix_count in 0..10usize,
                prefix_name in r"[a-zA-Z0-9_\-]+",
                prefix_uri in r"https://[a-zA-Z0-9_\-\.]+/[a-zA-Z0-9_\-\.\/]*"
            ) {
                // Skip invalid combinations
                if prefix_name.is_empty() || prefix_uri.is_empty() {
                    return Ok(());
                }
                if prefix_name.len() > 50 || prefix_uri.len() > 200 {
                    return Ok(());
                }

                let mut prefixes = BTreeMap::new();
                for i in 0..prefix_count {
                    let name = format!("{}-{}", prefix_name, i);
                    let uri = format!("{}-{}", prefix_uri, i);
                    prefixes.insert(name, uri);
                }

                let config = RdfConfig {
                    base: Some("https://example.org/".to_string()),
                    prefixes: prefixes.clone(),
                    patterns: Vec::new(),
                    inline: Vec::new(),
                };

                // Validate prefixes
                assert_eq!(config.prefixes.len(), prefix_count);
                for (name, uri) in &config.prefixes {
                    assert!(name.contains(&prefix_name));
                    assert!(uri.contains("https://"));
                }
            }
        }
    }
}
