use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct RgenConfig {
    /// Directory where templates live (relative to rgen.toml)
    pub templates_dir: PathBuf,

    /// Base IRI for RDF operations
    pub base: Option<String>,

    /// Global prefixes for RDF operations
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,

    /// Graph/RDF configuration
    #[serde(rename = "rdf")]
    pub graph: GraphConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct GraphConfig {
    /// RDF files to load (relative to rgen.toml)
    pub files: Vec<PathBuf>,
    /// Inline Turtle blocks
    pub inline: Vec<String>,
}

impl Default for RgenConfig {
    fn default() -> Self {
        Self {
            templates_dir: PathBuf::from("templates"),
            base: None,
            prefixes: BTreeMap::new(),
            graph: GraphConfig::default(),
        }
    }
}

impl Default for GraphConfig {
    fn default() -> Self {
        Self { files: Vec::new(), inline: Vec::new() }
    }
}

/// Loaded project config bound to its filesystem root (the folder containing rgen.toml).
#[derive(Debug, Clone)]
pub struct ProjectConfig {
    root: PathBuf,
    cfg: RgenConfig,
}

impl ProjectConfig {
    /// Walk up from `start_dir` to find nearest rgen.toml. Returns None if not found.
    pub fn discover(start_dir: &Path) -> Result<Option<Self>> {
        let mut cur = start_dir.absolutize();
        loop {
            let candidate = cur.join("rgen.toml");
            if candidate.exists() {
                return Self::load_file(&candidate).map(Some);
            }
            if !cur.pop() { return Ok(None); }
        }
    }

    /// Load specific rgen.toml.
    pub fn load_file(path: &Path) -> Result<Self> {
        let root = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let content = fs::read_to_string(path)?;
        let cfg: RgenConfig = toml::from_str(&content)?;
        Ok(Self { root, cfg })
    }

    /// Underlying config (read-only).
    pub fn cfg(&self) -> &RgenConfig { &self.cfg }

    /// Project root (directory containing rgen.toml).
    pub fn root(&self) -> &Path { &self.root }

    /// Absolute templates directory.
    pub fn templates_dir(&self) -> PathBuf {
        self.root.join(&self.cfg.templates_dir)
    }

    /// Absolute RDF file paths.
    pub fn rdf_files(&self) -> Vec<PathBuf> {
        self.cfg.graph.files.iter().map(|p| self.root.join(p)).collect()
    }

    /// Inline RDF blocks.
    pub fn rdf_inline(&self) -> &[String] { &self.cfg.graph.inline }

    /// Global prefixes.
    pub fn prefixes(&self) -> &BTreeMap<String, String> { &self.cfg.prefixes }

    /// Base IRI.
    pub fn base(&self) -> Option<&str> { self.cfg.base.as_deref() }
}

/* -------- small path helper -------- */
trait Absolutize {
    fn absolutize(&self) -> PathBuf;
}
impl Absolutize for &Path {
    fn absolutize(&self) -> PathBuf {
        if self.is_absolute() { self.to_path_buf() } else { std::env::current_dir().unwrap().join(self) }
    }
}

/* ---------------- tests ---------------- */

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn load_specific_file() -> Result<()> {
        let td = TempDir::new()?;
        let root = td.path();
        let toml = r#"
templates_dir = "custom_templates"
base = "http://example.org/"
[prefixes]
ex = "http://example.org/"
[rdf]
files = ["graphs/core.ttl","graphs/shapes.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:x a ex:T ."]
"#;
        let path = root.join("rgen.toml");
        fs::write(&path, toml)?;

        let pc = ProjectConfig::load_file(&path)?;
        assert_eq!(pc.templates_dir(), root.join("custom_templates"));
        assert_eq!(pc.base(), Some("http://example.org/"));
        assert_eq!(pc.prefixes().get("ex").unwrap(), "http://example.org/");
        let files = pc.rdf_files();
        assert_eq!(files, vec![root.join("graphs/core.ttl"), root.join("graphs/shapes.ttl")]);
        assert_eq!(pc.rdf_inline().len(), 1);
        Ok(())
    }

    #[test]
    fn discover_walks_up() -> Result<()> {
        let td = TempDir::new()?;
        let root = td.path().join("proj");
        let sub = root.join("a/b/c");
        fs::create_dir_all(&sub)?;
        fs::write(root.join("rgen.toml"), "templates_dir = 't'")?;

        let found = ProjectConfig::discover(&sub)?.unwrap();
        assert_eq!(found.templates_dir(), root.join("t"));
        Ok(())
    }

    #[test]
    fn discover_none() -> Result<()> {
        let td = TempDir::new()?;
        assert!(ProjectConfig::discover(td.path())?.is_none());
        Ok(())
    }
}
