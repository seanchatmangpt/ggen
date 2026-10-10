//! Core types for packs domain

use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};

/// Pack definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pack {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    pub author: Option<String>,
    pub repository: Option<String>,
    pub license: Option<String>,

    /// Registry type for external registries
    #[serde(default)]
    pub registry_type: Option<String>,

    /// Packages included in this pack
    pub packages: Vec<String>,

    /// Templates included in this pack
    #[serde(default)]
    pub templates: Vec<PackTemplate>,

    /// SPARQL queries for semantic operations
    #[serde(default)]
    pub sparql_queries: HashMap<String, String>,

    /// Dependencies on other packs
    #[serde(default)]
    pub dependencies: Vec<PackDependency>,

    /// Tags for discoverability
    #[serde(default)]
    pub tags: Vec<String>,

    /// Keywords for search
    #[serde(default)]
    pub keywords: Vec<String>,

    /// Production readiness flag
    #[serde(default)]
    pub production_ready: bool,

    /// Metadata for scoring
    #[serde(default)]
    pub metadata: PackMetadata,
}

/// Pack template definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackTemplate {
    pub name: String,
    pub path: String,
    pub description: String,
    #[serde(default)]
    pub variables: Vec<String>,
}

/// Pack dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackDependency {
    pub pack_id: String,
    pub version: String,
    #[serde(default)]
    pub optional: bool,
}

/// Pack metadata
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PackMetadata {
    #[serde(default)]
    pub test_coverage: Option<String>,
    #[serde(default)]
    pub rdf_ontology_size: Option<String>,
    #[serde(default)]
    pub sparql_templates: Option<usize>,
    #[serde(default)]
    pub code_examples: Option<usize>,
    #[serde(default)]
    pub documentation_files: Option<usize>,
}

/// Pack composition strategy
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum CompositionStrategy {
    /// Merge all packs (default)
    #[default]
    Merge,
    /// Layer packs (apply in order)
    Layer,
    /// Custom composition with rules
    Custom(HashMap<String, serde_json::Value>),
}

/// Pack file format (for serialization)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackFile {
    pub pack: Pack,

    /// Declared capability surface (`[capabilities]` table in pack.toml).
    ///
    /// `None` for all pre-annotation packs — the table is optional and defaults
    /// to absent so every existing pack.toml parses unchanged (zero drift).
    #[serde(default)]
    pub capabilities: Option<PackCapabilitiesFile>,
}

/// Declared capability surface from a pack.toml `[capabilities]` table.
///
/// Mirrors the engine's shape exactly: `provides`/`requires`/`types` are
/// string arrays of `urn:ggen:pack:*` URNs deserialized into `BTreeSet`
/// (sorted, deduped — duplicate URNs collapse silently, matching the engine's
/// `ggen-engine::pack::PackCapabilities` set semantics). Unknown keys are
/// refused (`deny_unknown_fields`): a typo'd key (`provide = [...]`) is a
/// parse error, never a silent drop.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PackCapabilitiesFile {
    /// Capability types declared by this pack
    #[serde(default)]
    pub types: Option<BTreeSet<String>>,
    /// Capability URNs this pack provides
    #[serde(default)]
    pub provides: Option<BTreeSet<String>>,
    /// Capability URNs this pack depends on
    #[serde(default)]
    pub requires: Option<BTreeSet<String>>,
}
