use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use utils::error::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frontmatter {
    pub to: String,
    pub vars: BTreeMap<String, String>,
    pub rdf: Vec<String>,
    pub shape: Vec<String>,
    pub sparql: Vec<String>,
    pub determinism: DeterminismConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterminismConfig {
    pub seed: Option<String>,
    pub sort_order: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TemplateSpec {
    pub path: std::path::PathBuf,
    pub output_path: std::path::PathBuf,
    pub content: String,
    pub frontmatter: Frontmatter,
}

impl Frontmatter {
    pub fn from_spec(spec: &TemplateSpec) -> Self {
        spec.frontmatter.clone()
    }

    pub fn parse_frontmatter(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let (frontmatter, _) = parse_yaml_frontmatter(&content)?;
        Ok(frontmatter)
    }

    pub fn rdf_sources(&self) -> Vec<String> {
        self.rdf.clone()
    }

    pub fn shape_sources(&self) -> Vec<String> {
        self.shape.clone()
    }

    pub fn var_queries(&self) -> Vec<String> {
        self.sparql.clone()
    }

    pub fn matrix_query(&self) -> Option<String> {
        None // TODO: Implement matrix queries
    }

    pub fn matrix_bind(&self) -> BTreeMap<String, String> {
        BTreeMap::new() // TODO: Implement matrix binding
    }

    pub fn defaults(&self) -> BTreeMap<String, String> {
        self.vars.clone()
    }

    pub fn seed(&self) -> String {
        self.determinism.seed.clone().unwrap_or_default()
    }

    pub fn sort_keys(&self) -> Vec<String> {
        self.determinism.sort_order.clone()
    }
}

fn parse_yaml_frontmatter(content: &str) -> Result<(Frontmatter, String)> {
    if !content.starts_with("---\n") {
        return Err(utils::error::Error::new("Missing YAML frontmatter"));
    }

    let end = content
        .find("\n---\n")
        .ok_or_else(|| utils::error::Error::new("Missing frontmatter end marker"))?;

    let yaml_content = &content[4..end];
    let template_content = &content[end + 5..];

    let frontmatter: Frontmatter = serde_yaml::from_str(yaml_content)?;
    Ok((frontmatter, template_content.to_string()))
}
