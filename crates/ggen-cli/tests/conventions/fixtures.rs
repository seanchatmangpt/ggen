use mockall::mock;
use std::path::{Path, PathBuf};
use anyhow::Result;

/// Mock filesystem abstraction for testing file discovery
mock! {
    pub FileSystem {
        fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>>;
        fn read_to_string(&self, path: &Path) -> Result<String>;
        fn exists(&self, path: &Path) -> bool;
        fn is_file(&self, path: &Path) -> bool;
        fn is_dir(&self, path: &Path) -> bool;
        fn canonicalize(&self, path: &Path) -> Result<PathBuf>;
    }
}

/// Mock RDF loader for testing data discovery
mock! {
    pub RdfLoader {
        fn load_file(&self, path: &Path) -> Result<String>;
        fn parse_turtle(&self, content: &str) -> Result<Vec<(String, String, String)>>;
    }
}

/// Mock template engine for testing template processing
mock! {
    pub TemplateEngine {
        fn render(&self, template: &str, context: &serde_json::Value) -> Result<String>;
        fn parse_frontmatter(&self, content: &str) -> Result<(serde_json::Value, String)>;
    }
}

/// Mock generation planner for testing workflow orchestration
mock! {
    pub GenerationPlanner {
        fn build_plan(&self, config: &ConventionConfig) -> Result<GenerationPlan>;
        fn resolve_dependencies(&self, templates: &[TemplateMetadata]) -> Result<Vec<TemplateMetadata>>;
    }
}

/// Convention configuration fixture
#[derive(Debug, Clone)]
pub struct ConventionConfig {
    pub rdf_dir: PathBuf,
    pub templates_dir: PathBuf,
    pub queries_dir: PathBuf,
    pub output_dir: PathBuf,
    pub preset: Option<String>,
}

impl Default for ConventionConfig {
    fn default() -> Self {
        Self {
            rdf_dir: PathBuf::from("rdf"),
            templates_dir: PathBuf::from("templates"),
            queries_dir: PathBuf::from("queries"),
            output_dir: PathBuf::from("src"),
            preset: None,
        }
    }
}

/// Template metadata fixture
#[derive(Debug, Clone, PartialEq)]
pub struct TemplateMetadata {
    pub name: String,
    pub path: PathBuf,
    pub mode: GenerationMode,
    pub when_trigger: Option<String>,
    pub output_path: Option<String>,
    pub query: Option<String>,
    pub dependencies: Vec<String>,
}

/// Generation mode enum
#[derive(Debug, Clone, PartialEq)]
pub enum GenerationMode {
    ForEach,  // Generate once per RDF file
    Once,     // Generate single output
}

/// Generation plan fixture
#[derive(Debug, Clone)]
pub struct GenerationPlan {
    pub templates: Vec<TemplateMetadata>,
    pub rdf_files: Vec<PathBuf>,
    pub output_mappings: Vec<(PathBuf, PathBuf)>,
}

/// Create test RDF file content
pub fn sample_rdf_content() -> String {
    r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User1 rdf:type ex:User ;
    ex:name "Alice" ;
    ex:email "alice@example.com" .
"#.to_string()
}

/// Create test template content with frontmatter
pub fn sample_template_content() -> String {
    r#"---
name: user_model
mode: foreach
when: "**/*user*.ttl"
output: "models/{{ name }}.rs"
query: get_users
---
pub struct {{ name }} {
    pub email: String,
}
"#.to_string()
}

/// Create test .ggen config content
pub fn sample_ggen_config() -> String {
    r#"
[conventions]
rdf_dir = "data/rdf"
templates_dir = "templates"
output_dir = "generated"
preset = "clap-noun-verb"
"#.to_string()
}
