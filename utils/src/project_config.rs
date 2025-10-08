use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
pub struct RgenConfig {
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