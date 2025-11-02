//! Convention presets for common project structures

use anyhow::Result;
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
