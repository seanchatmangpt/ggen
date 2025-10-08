use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Rpack manifest structure
#[derive(Debug, Deserialize, Serialize)]
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
#[derive(Debug, Deserialize, Serialize)]
pub struct RpackMetadata {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub rgen_compat: String,
}

/// Templates configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct TemplatesConfig {
    #[serde(default)]
    pub entrypoints: Vec<String>,
    #[serde(default)]
    pub includes: Vec<String>,
}

/// Macros configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct MacrosConfig {
    #[serde(default)]
    pub paths: Vec<String>,
}

/// RDF configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct RdfConfig {
    #[serde(default)]
    pub base: Option<String>,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    #[serde(default)]
    pub files: Vec<PathBuf>,
    #[serde(default)]
    pub inline: Vec<String>,
}

/// Queries configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct QueriesConfig {
    #[serde(default)]
    pub files: Vec<PathBuf>,
    #[serde(default)]
    pub aliases: BTreeMap<String, String>,
}

/// Shapes configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct ShapesConfig {
    #[serde(default)]
    pub files: Vec<PathBuf>,
}

/// Preset configuration
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct PresetConfig {
    #[serde(default)]
    pub config: Option<PathBuf>,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
}

impl RpackManifest {
    /// Load manifest from a file
    pub fn load_from_file(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let manifest: RpackManifest = toml::from_str(&content)?;
        Ok(manifest)
    }

    /// Validate the manifest
    pub fn validate(&self) -> Result<()> {
        // Validate rgen compatibility
        self.validate_rgen_compat()?;
        
        // Validate that entrypoints exist
        self.validate_entrypoints()?;
        
        // Validate that referenced files exist
        self.validate_file_references()?;
        
        Ok(())
    }

    fn validate_rgen_compat(&self) -> Result<()> {
        // For now, just check that it's not empty
        if self.metadata.rgen_compat.is_empty() {
            return Err(anyhow::anyhow!("rgen_compat is required"));
        }
        Ok(())
    }

    fn validate_entrypoints(&self) -> Result<()> {
        // This would need the base path to validate file existence
        // For now, just check that entrypoints are not empty if specified
        if !self.templates.entrypoints.is_empty() {
            for entrypoint in &self.templates.entrypoints {
                if entrypoint.is_empty() {
                    return Err(anyhow::anyhow!("Empty entrypoint found"));
                }
            }
        }
        Ok(())
    }

    fn validate_file_references(&self) -> Result<()> {
        // This would need the base path to validate file existence
        // For now, just validate that paths are not empty
        for file in &self.rdf.files {
            if file.to_string_lossy().is_empty() {
                return Err(anyhow::anyhow!("Empty RDF file path found"));
            }
        }
        
        for file in &self.queries.files {
            if file.to_string_lossy().is_empty() {
                return Err(anyhow::anyhow!("Empty query file path found"));
            }
        }
        
        for file in &self.shapes.files {
            if file.to_string_lossy().is_empty() {
                return Err(anyhow::anyhow!("Empty shape file path found"));
            }
        }
        
        Ok(())
    }

    /// Get all template paths that should be included
    pub fn get_template_paths(&self, base_path: &PathBuf) -> Vec<PathBuf> {
        let mut paths = Vec::new();
        
        // Add entrypoints
        for entrypoint in &self.templates.entrypoints {
            paths.push(base_path.join(entrypoint));
        }
        
        // Add includes
        for include in &self.templates.includes {
            paths.push(base_path.join(include));
        }
        
        paths
    }

    /// Get all RDF files that should be loaded
    pub fn get_rdf_files(&self, base_path: &PathBuf) -> Vec<PathBuf> {
        self.rdf.files.iter()
            .map(|file| base_path.join(file))
            .collect()
    }

    /// Get all query files that should be loaded
    pub fn get_query_files(&self, base_path: &PathBuf) -> Vec<PathBuf> {
        self.queries.files.iter()
            .map(|file| base_path.join(file))
            .collect()
    }

    /// Get all shape files that should be loaded
    pub fn get_shape_files(&self, base_path: &PathBuf) -> Vec<PathBuf> {
        self.shapes.files.iter()
            .map(|file| base_path.join(file))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    use std::io::Write;

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
entrypoints = ["cli/subcommand/rust.tmpl"]
includes = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
files = ["../graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]

[queries]
files = ["../queries/*.rq"]
aliases.component_by_name = "../queries/component_by_name.rq"

[shapes]
files = ["../shapes/*.ttl"]

[preset]
config = "../preset/rgen.toml"
vars = { author = "Acme", license = "MIT" }
"#;

        let manifest: RpackManifest = toml::from_str(toml_content).unwrap();
        
        assert_eq!(manifest.metadata.id, "io.rgen.rust.cli-subcommand");
        assert_eq!(manifest.metadata.name, "Rust CLI subcommand");
        assert_eq!(manifest.metadata.version, "0.1.0");
        assert_eq!(manifest.templates.entrypoints.len(), 1);
        assert_eq!(manifest.rdf.files.len(), 1);
        assert_eq!(manifest.queries.aliases.len(), 1);
    }

    #[test]
    fn test_manifest_validation() {
        let mut manifest = RpackManifest {
            metadata: RpackMetadata {
                id: "test".to_string(),
                name: "Test".to_string(),
                version: "0.1.0".to_string(),
                description: "Test pack".to_string(),
                license: "MIT".to_string(),
                rgen_compat: ">=0.1 <0.2".to_string(),
            },
            dependencies: BTreeMap::new(),
            templates: TemplatesConfig::default(),
            macros: MacrosConfig::default(),
            rdf: RdfConfig::default(),
            queries: QueriesConfig::default(),
            shapes: ShapesConfig::default(),
            preset: PresetConfig::default(),
        };

        // Should pass validation
        assert!(manifest.validate().is_ok());

        // Should fail with empty rgen_compat
        manifest.metadata.rgen_compat = String::new();
        assert!(manifest.validate().is_err());
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
