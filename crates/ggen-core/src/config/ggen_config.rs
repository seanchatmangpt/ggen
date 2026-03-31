//! ggen.toml configuration file parsing and loading
//!
//! This module handles loading and parsing ggen.toml files that define
//! generation rules, ontology configuration, and project metadata.

use serde::de::Deserializer;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Errors that can occur while loading or parsing ggen.toml
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Failed to read config file: {0}")]
    ReadError(#[from] std::io::Error),

    #[error("Failed to parse TOML: {0}")]
    TomlError(#[from] toml::de::Error),

    #[error("Invalid configuration: {0}")]
    ValidationError(String),

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Path resolution error: {0}")]
    PathError(String),
}

pub type ConfigResult<T> = Result<T, ConfigError>;

/// Generation mode for output files
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum GenerationMode {
    /// Overwrite existing file
    Overwrite,
    /// Only create if file doesn't exist
    Create,
    /// Append to existing file
    Append,
    /// Skip if file exists
    Skip,
}

impl Default for GenerationMode {
    fn default() -> Self {
        GenerationMode::Overwrite
    }
}

/// Intermediate struct for deserializing query/template file references
#[derive(Debug, Clone, Serialize, Deserialize)]
struct FileRef {
    file: PathBuf,
}

/// A single generation rule extracted from [[generation.rules]]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    /// Unique identifier for the rule (e.g., "jpa-entities")
    pub name: String,

    /// Path to SPARQL query file (relative to config dir)
    #[serde(rename = "query", deserialize_with = "deserialize_file_ref")]
    pub query_file: PathBuf,

    /// Path to Tera template file (relative to config dir)
    #[serde(rename = "template", deserialize_with = "deserialize_file_ref")]
    pub template_file: PathBuf,

    /// Output file pattern with Tera variables (e.g., "java/{{ packagePath }}/Entity.java")
    pub output_file: String,

    /// How to handle existing files
    #[serde(default)]
    pub mode: GenerationMode,

    /// Skip generation if result would be empty
    #[serde(default)]
    pub skip_empty: bool,
}

/// Custom deserializer for FileRef inline tables
fn deserialize_file_ref<'de, D>(deserializer: D) -> Result<PathBuf, D::Error>
where
    D: Deserializer<'de>,
{
    let file_ref = FileRef::deserialize(deserializer)?;
    Ok(file_ref.file)
}

impl Rule {
    /// Resolve query_file path relative to config directory
    pub fn resolve_query_path(&self, config_dir: &Path) -> PathBuf {
        if self.query_file.is_absolute() {
            self.query_file.clone()
        } else {
            config_dir.join(&self.query_file)
        }
    }

    /// Resolve template_file path relative to config directory
    pub fn resolve_template_path(&self, config_dir: &Path) -> PathBuf {
        if self.template_file.is_absolute() {
            self.template_file.clone()
        } else {
            config_dir.join(&self.template_file)
        }
    }
}

/// Ontology source configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct OntologyConfigSection {
    /// Main ontology source file (Turtle format)
    pub source: String,

    /// Additional ontology files to import
    #[serde(default)]
    pub imports: Vec<String>,

    /// Base IRI for the ontology
    #[serde(default)]
    pub base_iri: Option<String>,

    /// Namespace prefixes used in the ontology
    #[serde(default)]
    pub prefixes: HashMap<String, String>,
}

/// Project metadata from [project] section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,

    /// Project version
    #[serde(default)]
    pub version: Option<String>,

    /// Project description
    #[serde(default)]
    pub description: Option<String>,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        ProjectConfig {
            name: String::new(),
            version: None,
            description: None,
        }
    }
}

/// Generation configuration section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationConfigSection {
    /// Output directory for generated files
    #[serde(default)]
    pub output_dir: String,

    /// Generation rules
    #[serde(default)]
    pub rules: Vec<Rule>,
}

impl Default for GenerationConfigSection {
    fn default() -> Self {
        GenerationConfigSection {
            output_dir: "generated".to_string(),
            rules: Vec::new(),
        }
    }
}

/// Inference configuration (for future use)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceConfigSection {
    /// SPARQL inference rules
    #[serde(default)]
    pub rules: Vec<String>,
}

impl Default for InferenceConfigSection {
    fn default() -> Self {
        InferenceConfigSection { rules: Vec::new() }
    }
}

/// Complete ggen.toml configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenConfig {
    /// Project metadata
    #[serde(default)]
    pub project: ProjectConfig,

    /// Ontology configuration
    #[serde(default)]
    pub ontology: OntologyConfigSection,

    /// Generation rules
    #[serde(default)]
    pub generation: GenerationConfigSection,

    /// Inference rules (optional)
    #[serde(default)]
    pub inference: Option<InferenceConfigSection>,
}

impl GgenConfig {
    /// Load and parse ggen.toml from the given path
    ///
    /// # Arguments
    ///
    /// * `path` - Path to ggen.toml file
    ///
    /// # Returns
    ///
    /// Parsed GgenConfig with all relative paths resolved
    pub fn load(path: &Path) -> ConfigResult<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: GgenConfig = toml::from_str(&content)?;
        Ok(config)
    }

    /// Load and parse ggen.toml, validating all required fields
    pub fn load_and_validate(path: &Path) -> ConfigResult<Self> {
        let config = Self::load(path)?;

        // Validate project has a name
        if config.project.name.is_empty() {
            return Err(ConfigError::MissingField("project.name".to_string()));
        }

        // Validate ontology source is specified
        if config.ontology.source.is_empty() {
            return Err(ConfigError::MissingField("ontology.source".to_string()));
        }

        // Validate that all rules have required fields
        for rule in &config.generation.rules {
            if rule.name.is_empty() {
                return Err(ConfigError::MissingField(
                    "generation.rules[].name".to_string(),
                ));
            }
            if rule.query_file.as_os_str().is_empty() {
                return Err(ConfigError::MissingField(format!(
                    "generation.rules[{}].query_file",
                    rule.name
                )));
            }
            if rule.template_file.as_os_str().is_empty() {
                return Err(ConfigError::MissingField(format!(
                    "generation.rules[{}].template_file",
                    rule.name
                )));
            }
            if rule.output_file.is_empty() {
                return Err(ConfigError::MissingField(format!(
                    "generation.rules[{}].output_file",
                    rule.name
                )));
            }
        }

        Ok(config)
    }

    /// Get a rule by name
    pub fn get_rule(&self, name: &str) -> Option<&Rule> {
        self.generation.rules.iter().find(|r| r.name == name)
    }

    /// Get all rule names in order
    pub fn rule_names(&self) -> Vec<&str> {
        self.generation
            .rules
            .iter()
            .map(|r| r.name.as_str())
            .collect()
    }

    /// Get rules that match a filter predicate
    pub fn filter_rules<F>(&self, predicate: F) -> Vec<&Rule>
    where
        F: Fn(&Rule) -> bool,
    {
        self.generation
            .rules
            .iter()
            .filter(|r| predicate(r))
            .collect()
    }

    /// Resolve all query file paths relative to config directory
    pub fn resolve_query_paths(&self, config_dir: &Path) -> Vec<PathBuf> {
        self.generation
            .rules
            .iter()
            .map(|r| r.resolve_query_path(config_dir))
            .collect()
    }

    /// Resolve all template file paths relative to config directory
    pub fn resolve_template_paths(&self, config_dir: &Path) -> Vec<PathBuf> {
        self.generation
            .rules
            .iter()
            .map(|r| r.resolve_template_path(config_dir))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn create_test_config() -> (NamedTempFile, PathBuf) {
        let config_content = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology/test.ttl"
imports = ["ontology/imports.ttl"]
base_iri = "https://example.org/#"

[ontology.prefixes]
ex = "https://example.org/#"
owl = "http://www.w3.org/2002/07/owl#"

[generation]
output_dir = "generated"

[[generation.rules]]
name = "test-rule"
query = { file = "queries/test.rq" }
template = { file = "templates/test.tera" }
output_file = "output/{{ name }}.java"
mode = "Overwrite"
skip_empty = true

[[generation.rules]]
name = "another-rule"
query = { file = "queries/another.rq" }
template = { file = "templates/another.tera" }
output_file = "output/{{ name }}.java"
mode = "Create"
skip_empty = false
"#;

        let mut file = NamedTempFile::new().unwrap();
        write!(file, "{}", config_content).unwrap();
        let path = file.path().to_path_buf();
        (file, path)
    }

    #[test]
    fn test_load_config() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");

        assert_eq!(config.project.name, "test-project");
        assert_eq!(config.project.version, Some("1.0.0".to_string()));
        assert_eq!(config.ontology.source, "ontology/test.ttl");
        assert_eq!(config.generation.output_dir, "generated");
        assert_eq!(config.generation.rules.len(), 2);
    }

    #[test]
    fn test_rule_parsing() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");

        let rule = &config.generation.rules[0];
        assert_eq!(rule.name, "test-rule");
        assert_eq!(rule.query_file, PathBuf::from("queries/test.rq"));
        assert_eq!(rule.template_file, PathBuf::from("templates/test.tera"));
        assert_eq!(rule.output_file, "output/{{ name }}.java");
        assert_eq!(rule.mode, GenerationMode::Overwrite);
        assert!(rule.skip_empty);
    }

    #[test]
    fn test_get_rule() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");

        let rule = config.get_rule("test-rule").expect("Rule not found");
        assert_eq!(rule.name, "test-rule");

        assert!(config.get_rule("nonexistent").is_none());
    }

    #[test]
    fn test_rule_names() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");

        let names = config.rule_names();
        assert_eq!(names, vec!["test-rule", "another-rule"]);
    }

    #[test]
    fn test_filter_rules() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");

        let skip_empty = config.filter_rules(|r| r.skip_empty);
        assert_eq!(skip_empty.len(), 1);
        assert_eq!(skip_empty[0].name, "test-rule");
    }

    #[test]
    fn test_path_resolution() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load(&path).expect("Failed to load config");
        let config_dir = path.parent().unwrap();

        let query_paths = config.resolve_query_paths(config_dir);
        assert_eq!(query_paths.len(), 2);

        // Paths should be resolved relative to config dir
        assert!(query_paths[0].ends_with("queries/test.rq"));

        let template_paths = config.resolve_template_paths(config_dir);
        assert_eq!(template_paths.len(), 2);
        assert!(template_paths[0].ends_with("templates/test.tera"));
    }

    #[test]
    fn test_load_and_validate() {
        let (_file, path) = create_test_config();
        let config = GgenConfig::load_and_validate(&path).expect("Validation failed");

        assert_eq!(config.project.name, "test-project");
        assert_eq!(config.generation.rules.len(), 2);
    }

    #[test]
    #[ignore] // Only run if ggen.toml exists
    fn test_load_real_yawl_ggen_toml() {
        let path = PathBuf::from("/Users/sac/yawlv6/yawl-core/.claude/ggen/ggen.toml");
        if !path.exists() {
            return;
        }

        let config = GgenConfig::load_and_validate(&path).expect("Failed to load real ggen.toml");

        // Verify project metadata
        assert_eq!(config.project.name, "yawl-codegen");
        assert_eq!(config.project.version, Some("6.0.0".to_string()));

        // Verify ontology config
        assert_eq!(config.ontology.source, "ontology/yawl-domain.ttl");
        assert!(config.ontology.imports.len() > 0);
        assert_eq!(
            config.ontology.base_iri,
            Some("https://yawlfoundation.org/ontology#".to_string())
        );
        assert!(config.ontology.prefixes.len() > 0);

        // Verify generation rules loaded (at least 14 rules)
        assert!(
            config.generation.rules.len() >= 14,
            "Expected at least 14 rules, got {}",
            config.generation.rules.len()
        );

        // Verify specific rule names exist
        let rule_names = config.rule_names();
        assert!(rule_names.contains(&"jpa-entities"));
        assert!(rule_names.contains(&"repository-interfaces"));
        assert!(rule_names.contains(&"hibernate-repositories"));
        assert!(rule_names.contains(&"virtual-thread-daos"));
        assert!(rule_names.contains(&"services"));
        assert!(rule_names.contains(&"dto-records"));
        assert!(rule_names.contains(&"supporting-enums"));

        // Verify jpa-entities rule specifically
        let jpa_rule = config
            .get_rule("jpa-entities")
            .expect("jpa-entities rule not found");
        assert_eq!(
            jpa_rule.query_file,
            PathBuf::from("queries/jpa-entities.rq")
        );
        assert_eq!(
            jpa_rule.template_file,
            PathBuf::from("templates/jpa-entity.java.tera")
        );
        assert!(jpa_rule.output_file.contains("{{ packagePath }}"));
        assert_eq!(jpa_rule.mode, GenerationMode::Overwrite);
        assert!(jpa_rule.skip_empty);

        // Verify dto-records rule uses Create mode
        let dto_rule = config
            .get_rule("dto-records")
            .expect("dto-records rule not found");
        assert_eq!(dto_rule.mode, GenerationMode::Create);

        // Verify output_dir
        assert_eq!(config.generation.output_dir, "generated");
    }
}
