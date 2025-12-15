//! Convention presets for common project structures
//!
//! This module provides pre-configured convention presets that generate complete
//! project structures following established patterns. Presets include RDF ontologies,
//! templates, and configuration files needed for specific project types.
//!
//! ## Features
//!
//! - **Pre-configured Structures**: Complete project scaffolding with RDF and templates
//! - **Extensible Design**: Trait-based system for adding new presets
//! - **RDF Integration**: Automatic generation of RDF ontology files
//! - **Template Generation**: Pre-configured template files for common patterns
//!
//! ## Available Presets
//!
//! - **clap-noun-verb**: CLI projects using clap with noun-verb command structure
//!
//! ## Examples
//!
//! ### Using a Preset
//!
//! ```rust,ignore
//! use ggen_cli::conventions::presets::{get_preset, ConventionPreset};
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let preset = get_preset("clap-noun-verb")
//!     .expect("Preset not found");
//!
//! // Create project structure
//! preset.create_structure(Path::new("."))?;
//!
//! // Get RDF files to create
//! let rdf_files = preset.rdf_files();
//! for (path, content) in rdf_files {
//!     println!("RDF file: {}", path);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Listing Available Presets
//!
//! ```rust,ignore
//! use ggen_cli::conventions::presets::list_presets;
//!
//! let presets = list_presets();
//! for preset_name in presets {
//!     println!("Available preset: {}", preset_name);
//! }
//! ```

use ggen_utils::error::Result;
use std::path::Path;

pub mod clap_noun_verb;

/// Trait for convention presets
pub trait ConventionPreset {
    /// Name of this preset
    fn name(&self) -> &str;

    /// Create the project structure in the given root directory
    fn create_structure(&self, root: &Path) -> Result<()>;

    /// Get RDF files to create (path relative to .ggen/rdf, content)
    fn rdf_files(&self) -> Vec<(&str, &str)>;

    /// Get template files to create (path relative to .ggen/templates, content)
    fn templates(&self) -> Vec<(&str, &str)>;

    /// Get convention config content
    fn config_content(&self) -> String {
        format!("preset = \"{}\"\n", self.name())
    }
}

/// Get preset by name
pub fn get_preset(name: &str) -> Option<Box<dyn ConventionPreset>> {
    match name {
        "clap-noun-verb" => Some(Box::new(clap_noun_verb::ClapNounVerbPreset)),
        _ => None,
    }
}

/// List all available presets
pub fn list_presets() -> Vec<&'static str> {
    vec!["clap-noun-verb"]
}
