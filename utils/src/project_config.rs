use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use super::error::Result;

#[derive(Debug, Deserialize)]
pub struct RgenConfig {
    pub project: Project,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    #[serde(default)]
    pub rdf: RdfSection,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
    #[serde(default)]
    pub shell: Option<ShellHooks>,
}

#[derive(Debug, Deserialize)]
pub struct Project {
    pub name: String,
    pub version: Option<String>,
    pub description: Option<String>,
    pub templates_dir: PathBuf,
    pub graphs_dir: PathBuf,
    pub output_dir: PathBuf,
}

#[derive(Debug, Default, Deserialize)]
pub struct RdfSection {
    #[serde(default)]
    pub files: Vec<PathBuf>,
    #[serde(default)]
    pub inline: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct ShellHooks {
    pub before: Option<String>,
    pub after: Option<String>,
}

impl RgenConfig {
    /// Load rgen.toml from a specific path
    pub fn load_from_path(path: &Path) -> Result<Self> {
        let contents = std::fs::read_to_string(path)?;
        let config: RgenConfig = toml::from_str(&contents)?;
        Ok(config)
    }

    /// Find rgen.toml by walking up the directory tree from start_dir
    pub fn find_and_load(start_dir: &Path) -> Result<Option<Self>> {
        if let Some(manifest_path) = Self::find_rgen_toml(start_dir) {
            Ok(Some(Self::load_from_path(&manifest_path)?))
        } else {
            Ok(None)
        }
    }

    /// Walk up directory tree to find rgen.toml
    pub fn find_rgen_toml(start_dir: &Path) -> Option<PathBuf> {
        let mut current = start_dir.to_path_buf();
        
        loop {
            let manifest_path = current.join("rgen.toml");
            if manifest_path.exists() {
                return Some(manifest_path);
            }
            
            // Stop at filesystem root
            if !current.pop() {
                break;
            }
        }
        
        None
    }

    /// Merge this config with CLI variables, with CLI taking precedence
    pub fn merge_vars(&self, cli_vars: &BTreeMap<String, String>) -> BTreeMap<String, String> {
        let mut merged = self.vars.clone();
        for (k, v) in cli_vars {
            merged.insert(k.clone(), v.clone());
        }
        merged
    }

    /// Get the project root directory (where rgen.toml was found)
    pub fn project_root(&self, manifest_path: &Path) -> PathBuf {
        manifest_path.parent().unwrap_or_else(|| Path::new(".")).to_path_buf()
    }
}
