use anyhow::Result;
use glob::glob;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// Default conventions for pack file discovery
#[derive(Debug, Clone)]
pub struct PackConventions {
    pub template_patterns: &'static [&'static str],
    pub rdf_patterns: &'static [&'static str],
    pub query_patterns: &'static [&'static str],
    pub shape_patterns: &'static [&'static str],
}

impl Default for PackConventions {
    fn default() -> Self {
        Self {
            template_patterns: &["templates/**/*.tmpl", "templates/**/*.tera"],
            rdf_patterns: &[
                "templates/**/graphs/*.ttl", 
                "templates/**/graphs/*.rdf", 
                "templates/**/graphs/*.jsonld"
            ],
            query_patterns: &["templates/**/queries/*.rq", "templates/**/queries/*.sparql"],
            shape_patterns: &[
                "templates/**/graphs/shapes/*.shacl.ttl", 
                "templates/**/shapes/*.ttl"
            ],
        }
    }
}

/// Rpack manifest structure
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RpackManifest {
    #[serde(rename = "rpack")]
    pub metadata: RpackMetadata,
    #[serde(default)]
    pub dependencies: BTreeMap<String, String>,
    #[serde(default)]
    pub templates: TemplatesConfig,
    #[serde(default)]
    pub macros: MacrosConfig,
    #[serde(default)]
    pub rdf: RdfConfig,
    #[serde(default)]
    pub queries: QueriesConfig,
    #[serde(default)]
    pub shapes: ShapesConfig,
    #[serde(default)]
    pub preset: PresetConfig,
}

/// Rpack metadata section
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RpackMetadata {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub rgen_compat: String,
}

/// Templates configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct TemplatesConfig {
    /// Glob patterns for template discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    /// Additional Tera includes
    #[serde(default)]
    pub includes: Vec<String>,
}

/// Macros configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct MacrosConfig {
    #[serde(default)]
    pub paths: Vec<String>,
}

/// RDF configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct RdfConfig {
    #[serde(default)]
    pub base: Option<String>,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    /// Glob patterns for RDF file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    /// Inline RDF content
    #[serde(default)]
    pub inline: Vec<String>,
}

/// Queries configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct QueriesConfig {
    /// Glob patterns for query file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
    #[serde(default)]
    pub aliases: BTreeMap<String, String>,
}

/// Shapes configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ShapesConfig {
    /// Glob patterns for shape file discovery (empty = use conventions)
    #[serde(default)]
    pub patterns: Vec<String>,
}

/// Preset configuration
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct PresetConfig {
    #[serde(default)]
    pub config: Option<PathBuf>,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
}

/// Discover files using glob patterns
fn discover_files(base_path: &Path, patterns: &[&str]) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for pattern in patterns {
        let full_pattern = base_path.join(pattern);
        for entry in glob(&full_pattern.to_string_lossy())? {
            files.push(entry?);
        }
    }
    files.sort(); // Deterministic order
    Ok(files)
}

impl RpackManifest {
    /// Load manifest from a file
    pub fn load_from_file(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let manifest: RpackManifest = toml::from_str(&content)?;
        Ok(manifest)
    }

    /// Discover template files using conventions or config
    pub fn discover_templates(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.templates.patterns.is_empty() {
            PackConventions::default().template_patterns
        } else {
            &self
                .templates
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover RDF files using conventions or config
    pub fn discover_rdf_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.rdf.patterns.is_empty() {
            PackConventions::default().rdf_patterns
        } else {
            &self
                .rdf
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover query files using conventions or config
    pub fn discover_query_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.queries.patterns.is_empty() {
            PackConventions::default().query_patterns
        } else {
            &self
                .queries
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }

    /// Discover shape files using conventions or config
    pub fn discover_shape_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let patterns = if self.shapes.patterns.is_empty() {
            PackConventions::default().shape_patterns
        } else {
            &self
                .shapes
                .patterns
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        };

        discover_files(base_path, patterns)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_manifest_parsing() {
        let toml_content = r#"
[rpack]
id = "io.rgen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.1.0"
description = "Generate clap subcommands"
license = "MIT"
rgen_compat = ">=0.1 <0.2"

[dependencies]
"io.rgen.macros.std" = "^0.1"

[templates]
patterns = ["cli/subcommand/*.tmpl"]
includes = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
patterns = ["templates/**/graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]

[queries]
patterns = ["../queries/*.rq"]
aliases.component_by_name = "../queries/component_by_name.rq"

[shapes]
patterns = ["../shapes/*.ttl"]

[preset]
config = "../preset/rgen.toml"
vars = { author = "Acme", license = "MIT" }
"#;

        let manifest: RpackManifest = toml::from_str(toml_content).unwrap();

        assert_eq!(manifest.metadata.id, "io.rgen.rust.cli-subcommand");
        assert_eq!(manifest.metadata.name, "Rust CLI subcommand");
        assert_eq!(manifest.metadata.version, "0.1.0");
        assert_eq!(manifest.templates.patterns.len(), 1);
        assert_eq!(manifest.rdf.patterns.len(), 1);
        assert_eq!(manifest.queries.aliases.len(), 1);
    }

    #[test]
    fn test_manifest_load_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        let toml_content = r#"
[rpack]
id = "test"
name = "Test"
version = "0.1.0"
description = "Test"
license = "MIT"
rgen_compat = ">=0.1 <0.2"
"#;
        temp_file.write_all(toml_content.as_bytes()).unwrap();

        let manifest = RpackManifest::load_from_file(&temp_file.path().to_path_buf()).unwrap();
        assert_eq!(manifest.metadata.id, "test");
    }
}
