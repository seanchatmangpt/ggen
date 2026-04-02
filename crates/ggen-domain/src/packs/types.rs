//! Core types for packs domain

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompositionStrategy {
    /// Merge all packs (default)
    Merge,
    /// Layer packs (apply in order)
    Layer,
    /// Custom composition with rules
    Custom(HashMap<String, serde_json::Value>),
}

impl Default for CompositionStrategy {
    fn default() -> Self {
        Self::Merge
    }
}

/// Pack file format (for serialization)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackFile {
    pub pack: Pack,
}
