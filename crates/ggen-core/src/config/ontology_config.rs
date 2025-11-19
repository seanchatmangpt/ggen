//! Ontology configuration system
//!
//! Integrates ontology packs with ggen.toml configuration, enabling:
//! - Declarative ontology pack management
//! - Automatic composition and resolution
//! - Version constraints and lock files
//! - Multi-language code generation configuration

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

/// Ontology configuration section in ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyConfig {
    /// Installed ontology pack references
    pub packs: Vec<OntologyPackRef>,

    /// Composition strategy when multiple packs are used
    pub composition: CompositionStrategy,

    /// Code generation targets and options
    pub targets: BTreeMap<String, TargetConfig>,

    /// Feature flags for code generation
    pub features: HashMap<String, bool>,

    /// Lock file configuration
    pub lock: LockConfig,
}

/// Reference to an installed ontology pack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyPackRef {
    /// Pack name (e.g., "schema-org", "dublin-core")
    pub name: String,

    /// Version constraint (e.g., "^3.13.0", "~1.11.0", "latest")
    pub version: String,

    /// Namespace filter (extract only specified namespace)
    pub namespace: Option<String>,

    /// Classes to include (if None, include all)
    pub classes: Option<Vec<String>>,

    /// Properties to include (if None, include all)
    pub properties: Option<Vec<String>>,

    /// Optional source URL for custom/private packs
    pub source: Option<String>,
}

/// Strategy for composing multiple ontologies
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CompositionStrategy {
    /// Union: Include all classes and properties from all packs
    Union,

    /// Intersection: Only include classes/properties common to all packs
    Intersection,

    /// Priority: First pack takes precedence in conflicts
    Priority,

    /// Custom: Apply custom merge rules
    Custom {
        /// Rules for handling conflicts (pack_name: resolution)
        rules: HashMap<String, ConflictResolution>,
    },
}

/// How to resolve conflicts when composing ontologies
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ConflictResolution {
    /// Use the definition from the first pack
    UseFirst,

    /// Use the definition from the second pack
    UseSecond,

    /// Merge conflicting definitions
    Merge,

    /// Exclude the conflicting element
    Exclude,
}

/// Code generation target configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetConfig {
    /// Target language (typescript, rust, python, go)
    pub language: String,

    /// Output directory for generated code
    pub output_dir: PathBuf,

    /// Features to enable for this language
    pub features: Vec<String>,

    /// Custom template path (overrides pack templates)
    pub template_path: Option<PathBuf>,

    /// Post-generation hooks
    pub hooks: Option<GenerationHooks>,
}

/// Hooks to run during/after code generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationHooks {
    /// Command to run before generation
    pub pre_generate: Option<String>,

    /// Command to run after generation
    pub post_generate: Option<String>,

    /// Validate generated code
    pub validate: Option<String>,
}

/// Lock file configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockConfig {
    /// Path to lock file
    pub file: PathBuf,

    /// Auto-create lock file on update
    pub auto_update: bool,

    /// Require lock file for reproducible builds
    pub enforce: bool,
}

impl Default for OntologyConfig {
    fn default() -> Self {
        Self {
            packs: Vec::new(),
            composition: CompositionStrategy::Union,
            targets: BTreeMap::new(),
            features: HashMap::new(),
            lock: LockConfig::default(),
        }
    }
}

impl Default for LockConfig {
    fn default() -> Self {
        Self {
            file: PathBuf::from("ggen.lock"),
            auto_update: true,
            enforce: false,
        }
    }
}

impl OntologyConfig {
    /// Create a new ontology configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a pack reference
    pub fn with_pack(mut self, pack: OntologyPackRef) -> Self {
        self.packs.push(pack);
        self
    }

    /// Set composition strategy
    pub fn with_composition(mut self, strategy: CompositionStrategy) -> Self {
        self.composition = strategy;
        self
    }

    /// Add a target configuration
    pub fn with_target(mut self, name: String, config: TargetConfig) -> Self {
        self.targets.insert(name, config);
        self
    }

    /// Add a feature flag
    pub fn with_feature(mut self, name: String, enabled: bool) -> Self {
        self.features.insert(name, enabled);
        self
    }

    /// Load from TOML file section
    pub fn from_toml_section(toml_content: &str) -> Result<Self> {
        toml::from_str(toml_content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to parse ontology config: {}", e))
        })
    }

    /// Save to TOML format
    pub fn to_toml(&self) -> Result<String> {
        toml::to_string_pretty(self).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize ontology config: {}", e))
        })
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        if self.packs.is_empty() {
            return Err(ggen_utils::error::Error::new(
                "No ontology packs configured",
            ));
        }

        // Validate pack references
        for pack in &self.packs {
            if pack.name.is_empty() {
                return Err(ggen_utils::error::Error::new("Pack name cannot be empty"));
            }

            if pack.version.is_empty() {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Version for pack '{}' cannot be empty",
                    pack.name
                )));
            }
        }

        // Validate targets
        for (name, target) in &self.targets {
            if target.language.is_empty() {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Language for target '{}' cannot be empty",
                    name
                )));
            }
        }

        Ok(())
    }

    /// Get all referenced pack names
    pub fn pack_names(&self) -> Vec<&str> {
        self.packs.iter().map(|p| p.name.as_str()).collect()
    }

    /// Get all target languages
    pub fn target_languages(&self) -> Vec<&str> {
        self.targets.values().map(|t| t.language.as_str()).collect()
    }

    /// Check if a feature is enabled
    pub fn is_feature_enabled(&self, name: &str) -> bool {
        self.features.get(name).copied().unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ontology_config_builder() {
        let config = OntologyConfig::new()
            .with_pack(OntologyPackRef {
                name: "schema-org".to_string(),
                version: "^3.13.0".to_string(),
                namespace: Some("https://schema.org/".to_string()),
                classes: None,
                properties: None,
                source: None,
            })
            .with_composition(CompositionStrategy::Union)
            .with_feature("zod".to_string(), true);

        assert_eq!(config.packs.len(), 1);
        assert_eq!(config.packs[0].name, "schema-org");
        assert!(config.is_feature_enabled("zod"));
        assert!(!config.is_feature_enabled("serde"));
    }

    #[test]
    fn test_ontology_config_validation() {
        let config = OntologyConfig::new();
        assert!(config.validate().is_err()); // No packs

        let valid_config = OntologyConfig::new().with_pack(OntologyPackRef {
            name: "schema-org".to_string(),
            version: "3.13.0".to_string(),
            namespace: None,
            classes: None,
            properties: None,
            source: None,
        });

        assert!(valid_config.validate().is_ok());
    }

    #[test]
    fn test_composition_strategies() {
        assert_eq!(CompositionStrategy::Union, CompositionStrategy::Union);
        assert_ne!(
            CompositionStrategy::Union,
            CompositionStrategy::Intersection
        );
    }
}
