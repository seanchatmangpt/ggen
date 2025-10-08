use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RgenConfig {
    /// Templates directory (relative to config file)
    pub templates_dir: Option<String>,
    
    /// Base IRI for RDF operations
    pub base: Option<String>,
    
    /// Global prefixes for RDF operations
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    
    /// RDF configuration
    pub rdf: Option<RdfConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfConfig {
    /// RDF files to load (relative to config file)
    #[serde(default)]
    pub files: Vec<String>,
    
    /// Inline RDF content
    #[serde(default)]
    pub inline: Vec<String>,
}

impl Default for RgenConfig {
    fn default() -> Self {
        Self {
            templates_dir: Some("templates".to_string()),
            base: None,
            prefixes: BTreeMap::new(),
            rdf: None,
        }
    }
}

impl RgenConfig {
    /// Load rgen.toml from the given directory, walking up the tree
    pub fn discover_and_load(start_dir: &Path) -> Result<Option<(Self, PathBuf)>> {
        let mut current = start_dir.to_path_buf();
        
        loop {
            let config_path = current.join("rgen.toml");
            if config_path.exists() {
                let config = Self::load_from_file(&config_path)?;
                return Ok(Some((config, config_path)));
            }
            
            // Move up one directory
            match current.parent() {
                Some(parent) => current = parent.to_path_buf(),
                None => break, // Reached root
            }
        }
        
        Ok(None)
    }
    
    /// Load rgen.toml from a specific file
    pub fn load_from_file(config_path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(config_path)?;
        let config: RgenConfig = toml::from_str(&content)?;
        Ok(config)
    }
    
    /// Resolve a path relative to the config file directory
    pub fn resolve_path(&self, config_dir: &Path, path: &str) -> PathBuf {
        if Path::new(path).is_absolute() {
            PathBuf::from(path)
        } else {
            config_dir.join(path)
        }
    }
    
    /// Get templates directory path, resolved relative to config file
    pub fn templates_dir_path(&self, config_dir: &Path) -> PathBuf {
        let templates_dir = self.templates_dir.as_deref().unwrap_or("templates");
        self.resolve_path(config_dir, templates_dir)
    }
    
    /// Get RDF file paths, resolved relative to config file
    pub fn rdf_file_paths(&self, config_dir: &Path) -> Vec<PathBuf> {
        self.rdf
            .as_ref()
            .map(|rdf| {
                rdf.files
                    .iter()
                    .map(|file| self.resolve_path(config_dir, file))
                    .collect()
            })
            .unwrap_or_default()
    }
    
    /// Get inline RDF content
    pub fn rdf_inline_content(&self) -> Vec<String> {
        self.rdf
            .as_ref()
            .map(|rdf| rdf.inline.clone())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;

    #[test]
    fn test_config_discovery() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let config_dir = temp_dir.path().join("project").join("subdir");
        fs::create_dir_all(&config_dir)?;
        
        // Create rgen.toml in project root
        let config_path = temp_dir.path().join("project").join("rgen.toml");
        fs::write(&config_path, r#"
templates_dir = "custom_templates"
base = "http://example.org/"
[prefixes]
ex = "http://example.org/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
"#)?;
        
        let (config, found_path) = RgenConfig::discover_and_load(&config_dir)?.unwrap();
        
        assert_eq!(found_path, config_path);
        assert_eq!(config.templates_dir, Some("custom_templates".to_string()));
        assert_eq!(config.base, Some("http://example.org/".to_string()));
        assert_eq!(config.prefixes.get("ex"), Some(&"http://example.org/".to_string()));
        
        Ok(())
    }
    
    #[test]
    fn test_config_not_found() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let result = RgenConfig::discover_and_load(temp_dir.path())?;
        assert!(result.is_none());
        Ok(())
    }
    
    #[test]
    fn test_path_resolution() -> Result<()> {
        let config = RgenConfig::default();
        let config_dir = Path::new("/project");
        
        // Relative path
        let resolved = config.resolve_path(config_dir, "templates");
        assert_eq!(resolved, PathBuf::from("/project/templates"));
        
        // Absolute path
        let resolved = config.resolve_path(config_dir, "/absolute/path");
        assert_eq!(resolved, PathBuf::from("/absolute/path"));
        
        Ok(())
    }
    
    #[test]
    fn test_rdf_config() -> Result<()> {
        let config_content = r#"
templates_dir = "templates"
base = "http://example.org/"
[prefixes]
ex = "http://example.org/"

[rdf]
files = ["graphs/core.ttl", "graphs/shapes.ttl"]
inline = [
    "@prefix ex: <http://example.org/> . ex:test a ex:Thing ."
]
"#;
        
        let config: RgenConfig = toml::from_str(config_content)?;
        let config_dir = Path::new("/project");
        
        let rdf_paths = config.rdf_file_paths(config_dir);
        assert_eq!(rdf_paths.len(), 2);
        assert_eq!(rdf_paths[0], PathBuf::from("/project/graphs/core.ttl"));
        assert_eq!(rdf_paths[1], PathBuf::from("/project/graphs/shapes.ttl"));
        
        let inline_content = config.rdf_inline_content();
        assert_eq!(inline_content.len(), 1);
        assert!(inline_content[0].contains("ex:test a ex:Thing"));
        
        Ok(())
    }
}
