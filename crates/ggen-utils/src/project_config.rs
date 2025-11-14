//! Project configuration types
//!
//! This module provides configuration structures for ggen projects, including
//! project metadata, RDF configuration, and variable definitions.
//!
//! ## Configuration Structure
//!
//! The `GgenConfig` structure represents the root configuration for a ggen project:
//!
//! - **Project**: Project-specific settings (output directory)
//! - **RDF**: RDF data sources (files and inline data)
//! - **Prefixes**: RDF namespace prefixes
//! - **Vars**: Template variables
//!
//! ## Examples
//!
//! ### Loading Configuration
//!
//! ```rust,no_run
//! use ggen_utils::project_config::GgenConfig;
//! use std::fs;
//!
//! # fn main() -> anyhow::Result<()> {
//! let content = fs::read_to_string("ggen.toml")?;
//! let config: GgenConfig = toml::from_str(&content)?;
//!
//! println!("Output directory: {:?}", config.project.output_dir);
//! # Ok(())
//! # }
//! ```
//!
//! ### RDF Configuration
//!
//! ```rust,no_run
//! use ggen_utils::project_config::{GgenConfig, RdfConfig};
//!
//! // RDF can be loaded from files or inline strings
//! let rdf_config = RdfConfig {
//!     files: vec!["data.ttl".into()],
//!     inline: vec!["@prefix ex: <http://example.org/> .".to_string()],
//! };
//! ```

use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
pub struct GgenConfig {
    pub project: Project,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    #[serde(rename = "rdf")]
    pub rdf: RdfConfig,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
}

#[derive(Debug, Deserialize)]
pub struct Project {
    pub output_dir: PathBuf,
}

#[derive(Debug, Deserialize)]
pub struct RdfConfig {
    #[serde(default)]
    pub files: Vec<PathBuf>,
    #[serde(default)]
    pub inline: Vec<String>,
}
