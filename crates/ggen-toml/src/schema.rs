use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Source types — model the TOML forms ggen.toml uses for query / template
// ---------------------------------------------------------------------------

/// Where a SPARQL query comes from.
///
/// Handles all three TOML forms found in real ggen.toml files:
/// - plain string path: `query = "queries/extract.rq"`
/// - inline table:      `query = { inline = "SELECT ?s ..." }`
/// - file table:        `query = { file = "queries/extract.rq" }`
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum QuerySource {
    /// Literal SPARQL string embedded in the TOML table.
    Inline { inline: String },
    /// Path stored in a TOML table: `{ file = "..." }`.
    FileRef { file: String },
    /// Plain string path (most common in stage configs).
    Path(String),
}

impl QuerySource {
    /// Return the SPARQL text if it is inline, or `None` for file references.
    pub fn as_inline(&self) -> Option<&str> {
        match self {
            Self::Inline { inline } => Some(inline),
            _ => None,
        }
    }

    /// Return the file path string, whatever the TOML form.
    pub fn as_path(&self) -> Option<&str> {
        match self {
            Self::Path(p) | Self::FileRef { file: p } => Some(p),
            Self::Inline { .. } => None,
        }
    }
}

/// Where a Tera template comes from.
///
/// Handles:
/// - plain string path:  `template = "templates/foo.tera"`
/// - file table:         `template = { file = "templates/foo.tera" }`
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum TemplateSource {
    /// Path stored in a TOML table: `{ file = "..." }`.
    FileRef { file: String },
    /// Plain string path.
    Path(String),
}

impl TemplateSource {
    /// Return the path string regardless of which TOML form was used.
    pub fn path(&self) -> &str {
        match self {
            Self::FileRef { file } | Self::Path(file) => file,
        }
    }
}

/// What to do when the output file already exists.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub enum OutputMode {
    /// Replace the file on every sync (default).
    #[default]
    Overwrite,
    /// Append to the existing file.
    Append,
    /// Leave the file untouched if it already exists.
    SkipIfExists,
}

// ---------------------------------------------------------------------------
// Generation rule — [[generation.rules]]
// ---------------------------------------------------------------------------

/// One code-generation rule from `[[generation.rules]]`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationRule {
    /// Rule name, used in diagnostics and receipts.
    pub name: String,

    /// SPARQL query (inline, file table, or path string).
    #[serde(default)]
    pub query: Option<QuerySource>,

    /// Tera template (file table or path string).
    #[serde(default)]
    pub template: Option<TemplateSource>,

    /// Path of the output file, relative to `generation.output_dir`.
    #[serde(default)]
    pub output_file: Option<String>,

    /// How to handle an existing output file.
    #[serde(default)]
    pub mode: OutputMode,
}

// ---------------------------------------------------------------------------
// Pipeline stage — [[generation.stage]]
// ---------------------------------------------------------------------------

/// One stage in a multi-stage pipeline from `[[generation.stage]]`.
///
/// Stage configs use `output` instead of `output_file` and often omit the template.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStage {
    /// Stage name.
    pub name: String,

    /// SPARQL query for this stage.
    #[serde(default)]
    pub query: Option<QuerySource>,

    /// Tera template (optional — extraction-only stages have no template).
    #[serde(default)]
    pub template: Option<TemplateSource>,

    /// Output file path (relative to `generation.output_dir`).
    #[serde(default)]
    pub output: Option<String>,
}

// ---------------------------------------------------------------------------
// Generation section — [generation]
// ---------------------------------------------------------------------------

/// SHACL validation settings embedded in `[generation.validation]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ValidationConfig {
    /// Path to the SHACL shapes file.
    #[serde(default)]
    pub shacl: Option<String>,
    /// Fail the pipeline on any SHACL violation (default: true when shacl is set).
    #[serde(default)]
    pub fail_on_violation: bool,
}

/// The `[generation]` section.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GenerationSection {
    /// Ontology path, when set at the generation level rather than `[ontology]`.
    #[serde(default)]
    pub ontology: Option<String>,

    /// Directory where generated files are written.
    #[serde(default)]
    pub output_dir: Option<String>,

    /// Generation rules (`[[generation.rules]]`).
    #[serde(default)]
    pub rules: Vec<GenerationRule>,

    /// Pipeline stages (`[[generation.stage]]`).
    #[serde(default)]
    pub stage: Vec<PipelineStage>,

    /// SHACL validation settings.
    #[serde(default)]
    pub validation: Option<ValidationConfig>,

    /// Enable LLM-assisted generation.
    #[serde(default)]
    pub enable_llm: bool,

    /// LLM provider name when `enable_llm = true`.
    #[serde(default)]
    pub llm_provider: Option<String>,

    /// LLM model name when `enable_llm = true`.
    #[serde(default)]
    pub llm_model: Option<String>,
}

// ---------------------------------------------------------------------------
// Root config — GgenToml
// ---------------------------------------------------------------------------

/// Project identity fields from `[project]`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub authors: Option<Vec<String>>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
}

/// RDF ontology settings from `[ontology]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OntologyConfig {
    /// Path to the Turtle (.ttl) ontology file.
    #[serde(default)]
    pub source: Option<String>,

    /// Restrict to standard ontologies (schema.org, FOAF, etc.).
    #[serde(default)]
    pub standard_only: bool,
}

/// Sync behaviour settings from `[sync]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SyncConfig {
    #[serde(default = "bool_true")]
    pub enabled: bool,

    /// When to re-sync: `"manual"`, `"on_save"`, `"on_commit"`.
    #[serde(default)]
    pub on_change: Option<String>,

    /// Run validation after sync completes.
    #[serde(default)]
    pub validate_after: bool,

    /// How to handle conflicts: `"fail"`, `"overwrite"`, `"skip"`.
    #[serde(default)]
    pub conflict_mode: Option<String>,
}

fn bool_true() -> bool {
    true
}

/// RDF/SPARQL settings from `[rdf]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RdfSection {
    #[serde(default)]
    pub formats: Vec<String>,

    #[serde(default)]
    pub default_format: Option<String>,

    #[serde(default)]
    pub strict_validation: bool,
}

/// Template engine settings from `[templates]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TemplatesSection {
    #[serde(default)]
    pub enable_caching: bool,

    #[serde(default)]
    pub auto_reload: bool,

    /// Directory containing Tera templates.
    #[serde(default)]
    pub directory: Option<String>,
}

/// Output formatting settings from `[output]`.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OutputSection {
    #[serde(default)]
    pub formatting: Option<String>,

    #[serde(default)]
    pub line_length: Option<u32>,

    #[serde(default)]
    pub indent: Option<u32>,
}

/// Root config struct for a `ggen.toml` file.
///
/// Covers the standard schema used by `ggen init` and most real-world ggen.toml files:
/// `[project]`, `[ontology]`, `[generation]` with `[[generation.rules]]` or
/// `[[generation.stage]]`, `[sync]`, `[rdf]`, `[templates]`, `[output]`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenToml {
    pub project: ProjectConfig,

    #[serde(default)]
    pub ontology: Option<OntologyConfig>,

    #[serde(default)]
    pub generation: Option<GenerationSection>,

    #[serde(default)]
    pub sync: Option<SyncConfig>,

    #[serde(default)]
    pub rdf: Option<RdfSection>,

    #[serde(default)]
    pub templates: Option<TemplatesSection>,

    #[serde(default)]
    pub output: Option<OutputSection>,
}

// ---------------------------------------------------------------------------
// Validate trait — optional self-validation for any config type
// ---------------------------------------------------------------------------

/// Config types that can check their own invariants implement this.
pub trait Validate {
    type Error: std::error::Error + Send + Sync + 'static;
    fn validate(&self) -> std::result::Result<(), Self::Error>;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::loader::TomlLoader;

    const ROOT_GGEN_TOML: &str = r#"
[project]
name = "my-ggen-project"
version = "0.1.0"
description = "A ggen project"
authors = ["ggen init"]
license = "MIT"

[ontology]
source = "schema/domain.ttl"
standard_only = true

[generation]
output_dir = "."

[[generation.rules]]
name = "example-rule"
query = { inline = "SELECT ?s WHERE { ?s ?p ?o }" }
template = { file = "templates/example.txt.tera" }
output_file = "ontology-summary.txt"
mode = "Overwrite"

[sync]
enabled = true
on_change = "manual"
validate_after = true
conflict_mode = "fail"

[rdf]
formats = ["turtle"]
default_format = "turtle"
strict_validation = false

[templates]
enable_caching = true
auto_reload = true

[output]
formatting = "default"
line_length = 100
indent = 2
"#;

    const SPECIFY_GGEN_TOML: &str = r#"
[project]
name = "ggen"
version = "26.5.4"

[generation]
ontology = ".specify/ontology/cli-ggen.ttl"
output_dir = "crates/ggen-cli/src"

[[generation.stage]]
name = "extract-commands"
query = ".specify/queries/cli/commands.rq"
template = ".specify/templates/cli/main.rs.tera"
output = "src/commands.rs"

[[generation.stage]]
name = "extract-subcommands"
query = ".specify/queries/cli/subcommands.rq"

[generation.validation]
shacl = ".specify/ontology/cli-ggen-shacl.ttl"
"#;

    #[test]
    fn parses_root_ggen_toml() {
        let cfg: GgenToml = TomlLoader::from_str(ROOT_GGEN_TOML).unwrap();
        assert_eq!(cfg.project.name, "my-ggen-project");
        assert_eq!(cfg.project.version, "0.1.0");

        let gen = cfg.generation.as_ref().unwrap();
        assert_eq!(gen.output_dir.as_deref(), Some("."));
        assert_eq!(gen.rules.len(), 1);

        let rule = &gen.rules[0];
        assert_eq!(rule.name, "example-rule");
        assert!(matches!(
            rule.query.as_ref().unwrap(),
            QuerySource::Inline { inline } if inline.contains("SELECT")
        ));
        assert!(matches!(
            rule.template.as_ref().unwrap(),
            TemplateSource::FileRef { file } if file == "templates/example.txt.tera"
        ));
        assert_eq!(rule.output_file.as_deref(), Some("ontology-summary.txt"));
        assert_eq!(rule.mode, OutputMode::Overwrite);
    }

    #[test]
    fn parses_specify_ggen_toml() {
        let cfg: GgenToml = TomlLoader::from_str(SPECIFY_GGEN_TOML).unwrap();
        assert_eq!(cfg.project.name, "ggen");

        let gen = cfg.generation.as_ref().unwrap();
        assert_eq!(gen.ontology.as_deref(), Some(".specify/ontology/cli-ggen.ttl"));
        assert_eq!(gen.stage.len(), 2);

        let stage0 = &gen.stage[0];
        assert_eq!(stage0.name, "extract-commands");
        assert!(matches!(
            stage0.query.as_ref().unwrap(),
            QuerySource::Path(p) if p == ".specify/queries/cli/commands.rq"
        ));
        assert_eq!(stage0.output.as_deref(), Some("src/commands.rs"));

        // stage 1 has no template or output (extraction only)
        let stage1 = &gen.stage[1];
        assert_eq!(stage1.name, "extract-subcommands");
        assert!(stage1.template.is_none());
        assert!(stage1.output.is_none());

        let validation = gen.validation.as_ref().unwrap();
        assert_eq!(validation.shacl.as_deref(), Some(".specify/ontology/cli-ggen-shacl.ttl"));
    }

    #[test]
    fn query_source_path_helper() {
        let path = QuerySource::Path("queries/foo.rq".to_string());
        assert_eq!(path.as_path(), Some("queries/foo.rq"));
        assert!(path.as_inline().is_none());

        let file_ref = QuerySource::FileRef { file: "queries/bar.rq".to_string() };
        assert_eq!(file_ref.as_path(), Some("queries/bar.rq"));

        let inline = QuerySource::Inline { inline: "SELECT *".to_string() };
        assert_eq!(inline.as_inline(), Some("SELECT *"));
        assert!(inline.as_path().is_none());
    }

    #[test]
    fn template_source_path_helper() {
        let path = TemplateSource::Path("templates/a.tera".to_string());
        assert_eq!(path.path(), "templates/a.tera");

        let file_ref = TemplateSource::FileRef { file: "templates/b.tera".to_string() };
        assert_eq!(file_ref.path(), "templates/b.tera");
    }
}
