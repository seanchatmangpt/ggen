//! Template system configuration
//!
//! Configuration for template search paths, defaults, and generation options

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Template system configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateConfig {
    /// Template search paths (in order of priority)
    pub search_paths: Vec<PathBuf>,

    /// Default variable values
    pub default_variables: HashMap<String, String>,

    /// RDF metadata storage location
    pub metadata_store: PathBuf,

    /// Template cache directory
    pub cache_dir: Option<PathBuf>,

    /// Generation options
    pub generation: GenerationOptions,

    /// Template marketplace settings
    pub marketplace: MarketplaceSettings,
}

/// Template generation options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationOptions {
    /// Default output directory
    pub default_output_dir: PathBuf,

    /// Auto-format generated files
    pub auto_format: bool,

    /// Run post-generation hooks by default
    pub run_hooks: bool,

    /// Interactive mode by default
    pub interactive: bool,

    /// Force overwrite by default
    pub force_overwrite: bool,

    /// Validate templates before generation
    pub validate_before_gen: bool,
}

/// Marketplace integration settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceSettings {
    /// Enable marketplace template discovery
    pub enabled: bool,

    /// Template package cache directory
    pub package_cache: PathBuf,

    /// Auto-update template packages
    pub auto_update: bool,

    /// Trusted template sources
    pub trusted_sources: Vec<String>,
}

impl Default for TemplateConfig {
    fn default() -> Self {
        Self {
            search_paths: vec![PathBuf::from("templates"), PathBuf::from(".ggen/templates")],
            default_variables: HashMap::new(),
            metadata_store: PathBuf::from(".ggen/metadata.ttl"),
            cache_dir: Some(PathBuf::from(".ggen/template-cache")),
            generation: GenerationOptions::default(),
            marketplace: MarketplaceSettings::default(),
        }
    }
}

impl Default for GenerationOptions {
    fn default() -> Self {
        Self {
            default_output_dir: PathBuf::from("."),
            auto_format: true,
            run_hooks: true,
            interactive: false,
            force_overwrite: false,
            validate_before_gen: true,
        }
    }
}

impl Default for MarketplaceSettings {
    fn default() -> Self {
        Self {
            enabled: true,
            package_cache: PathBuf::from(".ggen/packages"),
            auto_update: false,
            trusted_sources: vec![
                "ggen-official".to_string(),
                "community-verified".to_string(),
            ],
        }
    }
}

impl TemplateConfig {
    /// Load configuration from file
    pub fn load(path: &PathBuf) -> ggen_utils::error::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: Self = toml::from_str(&content)?;
        Ok(config)
    }

    /// Save configuration to file
    pub fn save(&self, path: &PathBuf) -> ggen_utils::error::Result<()> {
        let content = toml::to_string_pretty(self).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize config: {}", e))
        })?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Add search path
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Set default variable
    pub fn set_default_variable(&mut self, key: String, value: String) {
        self.default_variables.insert(key, value);
    }

    /// Get default variable
    pub fn get_default_variable(&self, key: &str) -> Option<&String> {
        self.default_variables.get(key)
    }

    /// Find template in search paths
    pub fn find_template(&self, template_name: &str) -> Option<PathBuf> {
        for search_path in &self.search_paths {
            let template_path = search_path.join(template_name);
            if template_path.exists() {
                return Some(template_path);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    test!(test_default_config, {
        let config = TemplateConfig::default();
        assert_eq!(config.search_paths.len(), 2);
        assert!(config.generation.auto_format);
        assert!(config.marketplace.enabled);
    });

    test!(test_add_search_path, {
        let mut config = TemplateConfig::default();
        let new_path = PathBuf::from("/custom/templates");

        config.add_search_path(new_path.clone());
        assert!(config.search_paths.contains(&new_path));

        // Adding same path again should not duplicate
        let len_before = config.search_paths.len();
        config.add_search_path(new_path);
        assert_eq!(config.search_paths.len(), len_before);
    });

    test!(test_default_variables, {
        let mut config = TemplateConfig::default();

        config.set_default_variable("project_name".to_string(), "my-project".to_string());
        config.set_default_variable("version".to_string(), "1.0.0".to_string());

        assert_eq!(
            config.get_default_variable("project_name").unwrap(),
            "my-project"
        );
        assert_eq!(config.get_default_variable("version").unwrap(), "1.0.0");
        assert!(config.get_default_variable("nonexistent").is_none());
    });
}
