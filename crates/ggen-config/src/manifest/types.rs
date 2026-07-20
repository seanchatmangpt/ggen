//! Manifest type definitions for ggen.toml parsing
//!
//! These types map directly to the TOML structure defined in the specification.
//! All collections use BTreeMap for deterministic serialization.
//!
//! Ported from `ggen-core/src/manifest/types.rs` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md). This is the real, actively-consumed
//! typed schema for `[project]`/`[ontology]`/`[inference]`/`[generation]`/`[validation]`/
//! `[[packs]]` in `ggen.toml` -- see `crate::config_lib::schema::GgenConfig`'s own removal
//! note for the reconciliation with that crate's separate (dead, now-retired) duplicate
//! definitions of `InferenceConfig`/`InferenceRule`/`GenerationConfig`.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Default SPARQL query timeout (5 seconds)
fn default_sparql_timeout() -> u64 {
    5000
}

/// Default reasoning timeout (5 seconds)
fn default_reasoning_timeout() -> u64 {
    5000
}

/// Default output directory (project root, relative to ggen.toml)
fn default_output_dir() -> PathBuf {
    PathBuf::from(".")
}

/// A reference to a ggen pack declared in ggen.toml
///
/// Not the same type as `ggen_engine::config::PackRef` — that one is an
/// untagged `Path { path, extra_ontologies, lock } | Git { git, version }`
/// enum, not this flat struct. See [`GgenManifest`]'s struct doc for the
/// fuller schema-divergence note.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PackRef {
    /// Pack name (used to reference this pack from generation rules)
    pub name: String,

    /// Registry type: "local", "marketplace", etc.
    #[serde(default = "default_registry")]
    pub registry: String,

    /// Local filesystem path (used when registry = "local")
    #[serde(default)]
    pub path: Option<PathBuf>,

    /// Pack version constraint (used when registry != "local")
    #[serde(default)]
    pub version: Option<String>,
}

fn default_registry() -> String {
    "local".to_string()
}

/// Describes a pack's named output directories, read from `<pack_root>/package.toml`.
/// Missing or unparseable file is treated as empty outputs (fail-open, falls back to literal key).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PackageToml {
    /// The [pack] section of package.toml
    #[serde(default)]
    pub pack: Option<PackSection>,

    /// Named output directories: key → relative directory path within the pack.
    #[serde(default)]
    pub outputs: std::collections::HashMap<String, String>,
}

/// The [pack] section of package.toml
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PackSection {
    /// Named output directories: key → relative directory path within the pack.
    #[serde(default)]
    pub outputs: std::collections::HashMap<String, String>,
}

impl PackageToml {
    /// Load from `<pack_root>/package.toml`. Returns empty struct if file is missing or invalid.
    #[must_use]
    pub fn load(pack_root: &std::path::Path) -> Self {
        let path = pack_root.join("package.toml");
        let Ok(content) = std::fs::read_to_string(&path) else {
            return Self::default();
        };
        toml::from_str(&content).unwrap_or_default()
    }

    /// Resolve an output key to its directory path, or return the key itself as fallback.
    #[must_use]
    pub fn resolve_output_key<'a>(&'a self, key: &'a str) -> &'a str {
        // Check top-level [outputs] first, then fall back to [pack.outputs].
        if let Some(val) = self.outputs.get(key) {
            return val.as_str();
        }
        self.pack
            .as_ref()
            .and_then(|p| p.outputs.get(key))
            .map_or(key, |s| s.as_str())
    }
}

/// Root manifest structure from ggen.toml
///
/// Schema unification (specs/014-ggen-core-replacement T023 follow-up,
/// docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md): this is now the single
/// authoritative Rust type for every `ggen.toml` top-level table this
/// workspace recognizes. Before this pass, the operational/infrastructure
/// sections below (`ai`, `rdf`, `sparql`, `lifecycle`, `security`,
/// `performance`, `logging`, `telemetry`, `features`, `env`, `templates`,
/// plus the newly-added `build`/`test`/`package`/`mcp`/`a2a`) were either a
/// raw, unread `Option<toml::Value>` passthrough here, or lived only in
/// `crate::config_lib::GgenConfig` (a second, independently-evolved struct
/// with no `[[generation.rules]]`/`[ontology]`/`[inference]` concept at
/// all). They are now the *same* `crate::config_lib::{AiConfig, RdfConfig,
/// ...}` types in both places — reused directly, not re-defined — so there
/// is exactly one typed, `star_toml::Validate`-checked definition per
/// section, and `config_lib::GgenConfig`'s own `Validate` impls (already
/// correct and already covered by that module's tests) are exercised
/// unchanged when a manifest's `[ai]`/`[mcp]`/etc. table is validated here
/// (see `validation.rs`'s `impl Validate for GgenManifest`).
///
/// `sync` and `output` remain untyped `Option<toml::Value>`: neither has a
/// counterpart in `config_lib::GgenConfig`, neither has a real reader
/// anywhere in the workspace (confirmed via grep, 2026-07-16), and the root
/// project's own `ggen.toml` uses both with fields (`[sync].on_change`,
/// `[output].line_length`) that don't correspond to any typed schema this
/// migration is chartered to invent from scratch. Typing them is explicitly
/// out of this reconciliation's scope; left as a documented, bounded gap
/// rather than a silent one.
///
/// `law` is new: N3/Datalog rule files for `ggen-engine`'s `praxis-graphlaw`
/// materialization stage (see `crate::manifest::types::Law`). Its SHACL
/// shapes are deliberately **not** duplicated as `law.shapes` — the existing
/// `validation.shacl: Vec<PathBuf>` field remains the one and only place a
/// manifest declares SHACL shape files; a `law`-aware consumer reads
/// `law.rules` for N3 and `validation.shacl` for shapes. See the "Engine
/// wiring" design note in specs/014-ggen-core-replacement/tasks.md for the
/// (not yet implemented) consumer-side plan.
///
/// A second, independently-defined `ggen.toml` root type also exists:
/// `ggen_engine::config::GgenConfig`. Both share the same top-level table
/// names (`project`/`ontology`/`packs`/`law`/…) but are deliberately
/// different shapes — this type's `[[packs]]` is an array-of-tables with a
/// flat `PackRef`, and its `Law` is rules-only (shapes live in
/// `validation.shacl` above), whereas `GgenConfig` uses a `[packs]`
/// table-of-tables keyed by pack name with an untagged
/// `ggen_engine::config::PackRef`, and a `Law` that carries both `rules`
/// and `shapes`. Which schema a given `ggen.toml` parses as is decided by
/// `ggen_engine::generation_rules::has_generation_rules` on the raw TOML
/// text, before either typed parse runs. There is intentionally no
/// automated cross-schema equivalence guard between the two.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GgenManifest {
    /// Project metadata
    pub project: ProjectConfig,

    /// Ontology loading configuration
    pub ontology: OntologyConfig,

    /// Inference rules (CONSTRUCT-based)
    #[serde(default)]
    pub inference: InferenceConfig,

    /// Code generation rules
    pub generation: GenerationConfig,

    /// Validation settings
    #[serde(default)]
    pub validation: ValidationConfig,

    /// Pack declarations (resolved before generation)
    #[serde(default)]
    pub packs: Vec<PackRef>,

    /// N3/Datalog law-state inputs for a `praxis-graphlaw`-backed engine.
    /// SHACL shapes are *not* duplicated here — see `validation.shacl`.
    #[serde(default)]
    pub law: Law,

    /// Unreconciled, unread passthrough — see the struct-level doc comment.
    #[serde(default)]
    pub sync: Option<toml::Value>,
    /// Unreconciled, unread passthrough — see the struct-level doc comment.
    #[serde(default)]
    pub output: Option<toml::Value>,

    /// `[rdf]` — reuses `config_lib::RdfConfig` (was an unread `toml::Value`
    /// passthrough here before this reconciliation).
    #[serde(default)]
    pub rdf: Option<crate::config_lib::RdfConfig>,
    /// `[templates]` — reuses `config_lib::TemplatesConfig` (directory,
    /// output_directory, backup/idempotent flags). Distinct from
    /// `generation.output_dir`, which is the codegen-rules output root.
    #[serde(default)]
    pub templates: Option<crate::config_lib::TemplatesConfig>,
    /// `[ai]` — reuses `config_lib::AiConfig`.
    #[serde(default)]
    pub ai: Option<crate::config_lib::AiConfig>,
    /// `[sparql]` — reuses `config_lib::SparqlConfig`.
    #[serde(default)]
    pub sparql: Option<crate::config_lib::SparqlConfig>,
    /// `[lifecycle]` — reuses `config_lib::LifecycleConfig`.
    #[serde(default)]
    pub lifecycle: Option<crate::config_lib::LifecycleConfig>,
    /// `[security]` — reuses `config_lib::SecurityConfig`.
    #[serde(default)]
    pub security: Option<crate::config_lib::SecurityConfig>,
    /// `[performance]` — reuses `config_lib::PerformanceConfig`.
    #[serde(default)]
    pub performance: Option<crate::config_lib::PerformanceConfig>,
    /// `[logging]` — reuses `config_lib::LoggingConfig`.
    #[serde(default)]
    pub logging: Option<crate::config_lib::LoggingConfig>,
    /// `[telemetry]` — reuses `config_lib::TelemetryConfig`.
    #[serde(default)]
    pub telemetry: Option<crate::config_lib::TelemetryConfig>,
    /// `[features]` — flat flag map, same shape as `config_lib::GgenConfig`.
    #[serde(default)]
    pub features: Option<std::collections::HashMap<String, bool>>,
    /// `[env]` — environment overrides, same shape as `config_lib::GgenConfig`.
    #[serde(default)]
    pub env: Option<std::collections::HashMap<String, serde_json::Value>>,
    /// `[build]` — reuses `config_lib::BuildConfig`. New: absent from
    /// `GgenManifest` before this reconciliation (only `config_lib::GgenConfig`
    /// had it); purely additive (`#[serde(default)]`), so no previously-valid
    /// manifest is affected.
    #[serde(default)]
    pub build: Option<crate::config_lib::BuildConfig>,
    /// `[test]` — reuses `config_lib::TestConfig`. New, same additivity note
    /// as `build` above.
    #[serde(default)]
    pub test: Option<crate::config_lib::TestConfig>,
    /// `[package]` — reuses `config_lib::PackageMetadata`. New, same
    /// additivity note as `build` above.
    #[serde(default)]
    pub package: Option<crate::config_lib::PackageMetadata>,
    /// `[mcp]` — reuses `config_lib::McpConfig`. New, same additivity note
    /// as `build` above.
    #[serde(default)]
    pub mcp: Option<crate::config_lib::McpConfig>,
    /// `[a2a]` — reuses `config_lib::A2AConfig`. New, same additivity note
    /// as `build` above.
    #[serde(default)]
    pub a2a: Option<crate::config_lib::A2AConfig>,
}

/// `[law]` — law-state inputs for a `praxis-graphlaw`-backed sync pipeline.
///
/// Mirrors `ggen_engine::config::Law`'s `rules` field; `shapes` is
/// deliberately absent — see `GgenManifest`'s struct-level doc comment.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct Law {
    /// N3/Datalog rule file paths, relative to the manifest, loaded and
    /// materialized in listed order.
    #[serde(default)]
    pub rules: Vec<PathBuf>,
}

/// Project metadata section
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectConfig {
    /// Project name (used in generated code headers)
    pub name: String,

    /// Semantic version
    pub version: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,

    /// Project authors (optional)
    #[serde(default)]
    pub authors: Option<Vec<String>>,

    /// Project license (optional)
    #[serde(default)]
    pub license: Option<String>,

    /// Project repository URL (optional)
    #[serde(default)]
    pub repository: Option<String>,
}

/// Ontology configuration section
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OntologyConfig {
    /// Primary ontology file path (relative to ggen.toml)
    pub source: PathBuf,

    /// Additional ontology imports
    #[serde(default)]
    pub imports: Vec<PathBuf>,

    /// Base IRI for relative URIs
    #[serde(default)]
    pub base_iri: Option<String>,

    /// Prefix mappings for SPARQL queries (BTreeMap for determinism)
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,

    /// Use standard ontologies only
    #[serde(default)]
    pub standard_only: Option<bool>,
}

/// Inference configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct InferenceConfig {
    /// Named inference rules executed in order
    #[serde(default)]
    pub rules: Vec<InferenceRule>,

    /// Maximum time for all inference (ms)
    #[serde(default = "default_reasoning_timeout")]
    pub max_reasoning_timeout_ms: u64,
}

/// A single inference rule (CONSTRUCT query)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InferenceRule {
    /// Rule identifier (for audit trail)
    pub name: String,

    /// Human-readable description
    #[serde(default)]
    pub description: Option<String>,

    /// SPARQL CONSTRUCT query
    pub construct: String,

    /// Execution order (lower = earlier)
    #[serde(default)]
    pub order: i32,

    /// Skip if condition fails (SPARQL ASK)
    #[serde(default)]
    pub when: Option<String>,
}

/// Generation configuration section
///
/// `#[derive(JsonSchema)]` is load-bearing (specs/014-ggen-core-replacement, T070): it lets
/// `ggen-engine`'s `tests/ggen_manifest_schema_match.rs` compare this struct's *actual* field
/// set (via `schemars::schema_for!`) against `schema/ggen-manifest-schema.ttl`, the same
/// drift-proof contract `ggen-engine::config::GgenConfig` already uses for its own schema
/// (`tests/ggen_toml_schema_match.rs`). Scoped to exactly the types
/// `ggen-engine`'s declarative `[[generation.rules]]` sync path consumes --
/// `GenerationRule`/`QuerySource`/`TemplateSource`/`GenerationMode` below carry the same derive
/// for the same reason. Sibling types (`InferenceConfig`, `ValidationConfig`, ...) are
/// deliberately NOT given this derive: they have no ggen-engine consumer yet (see the "Engine
/// wiring" design note in specs/014-ggen-core-replacement/tasks.md), so extending the
/// schema-drift contract to them would assert more than this pass verified.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct GenerationConfig {
    /// Code generation rules
    pub rules: Vec<GenerationRule>,

    /// Maximum SPARQL query timeout (ms)
    #[serde(default = "default_sparql_timeout")]
    pub max_sparql_timeout_ms: u64,

    /// Generate audit.json
    #[serde(default)]
    pub require_audit_trail: bool,

    /// Salt for deterministic IRI generation
    #[serde(default)]
    pub determinism_salt: Option<String>,

    /// Output directory (relative to ggen.toml)
    #[serde(default = "default_output_dir")]
    pub output_dir: PathBuf,

    /// Enable LLM-based auto-generation for skills (default: false)
    #[serde(default)]
    pub enable_llm: bool,

    /// LLM provider (groq, openai, anthropic, etc.)
    #[serde(default)]
    pub llm_provider: Option<String>,

    /// LLM model identifier
    #[serde(default)]
    pub llm_model: Option<String>,
}

/// A single generation rule
///
/// `#[derive(JsonSchema)]` -- see `GenerationConfig`'s doc comment.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct GenerationRule {
    /// Rule identifier
    pub name: String,

    /// SPARQL query (file path or inline)
    pub query: QuerySource,

    /// Tera template (file path or inline)
    pub template: TemplateSource,

    /// Output file pattern (supports {{variables}})
    pub output_file: String,

    /// Skip generation if query returns empty
    #[serde(default)]
    pub skip_empty: bool,

    /// File generation mode
    #[serde(default)]
    pub mode: GenerationMode,

    /// Skip if condition fails (SPARQL ASK query)
    #[serde(default)]
    pub when: Option<String>,
}

/// Source for a SPARQL query - file, inline, or pack output key
///
/// `#[derive(JsonSchema)]` -- see `GenerationConfig`'s doc comment. `schemars` represents this
/// untagged enum's variants under `oneOf`, each with its own `properties` object -- see
/// `ggen-engine`'s `tests/ggen_manifest_schema_match.rs::untagged_variant_fields`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum QuerySource {
    /// Load query from a declared pack's named output
    ///
    /// Example in ggen.toml:
    /// ```toml
    /// query = { pack = "wasm4pm-compat", output = "queries", file = "pm-rust-bridge.rq" }
    /// ```
    Pack {
        /// Pack name (must be declared in [[packs]])
        pack: String,
        /// Named output key from [pack.outputs] in the pack's package.toml
        output: String,
        /// File within that output directory
        file: PathBuf,
    },
    /// Load query from file
    File {
        /// Path to .sparql file
        file: PathBuf,
    },
    /// Inline query string
    Inline {
        /// SPARQL query text
        inline: String,
    },
}

/// Source for a Tera template - file, inline, git, pack output, or package manager
///
/// `#[derive(JsonSchema)]` -- see `GenerationConfig`'s doc comment.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum TemplateSource {
    /// Load template from a declared pack's named output
    ///
    /// Example in ggen.toml:
    /// ```toml
    /// template = { pack = "wasm4pm-compat", output = "templates", file = "rust-struct.tera" }
    /// ```
    Pack {
        /// Pack name (must be declared in [[packs]])
        pack: String,
        /// Named output key from [pack.outputs] in the pack's package.toml
        output: String,
        /// File within that output directory
        file: PathBuf,
    },
    /// Load template from file
    File {
        /// Path to .tera file
        file: PathBuf,
    },
    /// Inline template string
    Inline {
        /// Tera template text
        inline: String,
    },
    /// Load template from a Git repository
    Git {
        /// Git repository URL
        git: String,
        /// Optional branch or tag
        branch: Option<String>,
        /// Path within the repository
        path: PathBuf,
    },
    /// Load template from a Package Manager (e.g. ggen marketplace)
    Package {
        /// Package name
        package: String,
        /// Optional version
        version: Option<String>,
        /// Path within the package
        path: PathBuf,
    },
}

/// File generation mode
///
/// `#[derive(JsonSchema)]` -- see `GenerationConfig`'s doc comment. Fieldless variants
/// serialize as plain strings (serde's default external tagging); `schemars` represents this
/// as a top-level `"oneOf"` array whose entries are `{"type": "string", "const": "<Variant>"}`
/// (verified via `schemars::schema_for!`, not assumed) -- see `ggen-engine`'s
/// `tests/ggen_manifest_schema_match.rs::enum_variant_names`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, Default, PartialEq, Eq)]
pub enum GenerationMode {
    /// Write on first sync; silently skip on subsequent syncs if the file already exists.
    ///
    /// This is the default mode for bootstrap scaffolding: the first `ggen sync`
    /// writes the file; subsequent syncs leave it untouched so hand-written
    /// modifications are preserved. Use `Overwrite` to replace the file on every
    /// sync, or `Merge` to combine generated sections with hand-written sections.
    #[default]
    Create,
    /// Overwrite existing
    Overwrite,
    /// Merge with existing (marker-based)
    Merge,
}

/// Validation configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct ValidationConfig {
    /// SHACL shape files — **legacy, refused at sync time by ggen-engine's
    /// declarative-rules path**: the SHACL sync gate was replaced by
    /// `gates` (engine-independent SPARQL gate queries); a non-empty
    /// `shacl` list makes `ggen sync` fail closed with a typed
    /// `[FM-LAW-017]` migration error. Kept deserializable so an old
    /// manifest gets that clear refusal, not a serde unknown-field error.
    /// Author-time tooling (`ggen law validate`) still reads it.
    #[serde(default)]
    pub shacl: Vec<PathBuf>,

    /// SPARQL gate file paths (`*.rq`), relative to the manifest, each
    /// evaluated against the post-inference/materialization graph in
    /// listed order by ggen-engine's declarative-rules sync path. Each
    /// file: optional leading `# MESSAGE: <text>` comment line(s), then
    /// one ASK (true = violation) or SELECT (any row = violation) query.
    #[serde(default)]
    pub gates: Vec<PathBuf>,

    /// Validate generated Rust syntax
    #[serde(default)]
    pub validate_syntax: bool,

    /// Reject code containing unsafe
    #[serde(default)]
    pub no_unsafe: bool,

    /// Elevate determinism warnings (e.g. missing ORDER BY) to hard errors
    #[serde(default)]
    pub strict_mode: bool,

    /// Custom validation rules (SPARQL ASK)
    #[serde(default)]
    pub rules: Vec<ValidationRule>,
}

/// A custom validation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ValidationRule {
    /// Rule identifier
    pub name: String,

    /// Description shown on failure
    pub description: String,

    /// SPARQL ASK query (true = valid)
    pub ask: String,

    /// Error severity
    #[serde(default)]
    pub severity: ValidationSeverity,
}

/// Validation error severity
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub enum ValidationSeverity {
    /// Fails generation
    #[default]
    Error,
    /// Logged but continues
    Warning,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            name: String::new(),
            version: String::new(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        }
    }
}

impl Default for OntologyConfig {
    fn default() -> Self {
        Self {
            source: PathBuf::new(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
            standard_only: None,
        }
    }
}

impl Default for GenerationConfig {
    fn default() -> Self {
        Self {
            rules: vec![],
            max_sparql_timeout_ms: default_sparql_timeout(),
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: default_output_dir(),
            enable_llm: false,
            llm_model: None,
            llm_provider: None,
        }
    }
}

impl Default for GgenManifest {
    fn default() -> Self {
        Self {
            project: ProjectConfig::default(),
            ontology: OntologyConfig::default(),
            inference: InferenceConfig::default(),
            generation: GenerationConfig::default(),
            validation: ValidationConfig::default(),
            packs: vec![],
            law: Law::default(),
            sync: None,
            output: None,
            rdf: None,
            templates: None,
            ai: None,
            sparql: None,
            lifecycle: None,
            security: None,
            performance: None,
            logging: None,
            telemetry: None,
            features: None,
            env: None,
            build: None,
            test: None,
            package: None,
            mcp: None,
            a2a: None,
        }
    }
}

// `OntologyConfig::resolved_sources` (formerly here, delegating to
// `ggen_core::ontology::resolver::OntologyResolver::resolve`) is intentionally not ported:
// it's a filesystem-walking (`walkdir`) convenience used only by ggen-core's own (retired)
// codegen pipeline (`codegen/{pipeline,executor}.rs`, `ontology/loader.rs`) -- confirmed via
// grep, zero callers outside ggen-core. Out of scope for ggen-config, which owns manifest
// parsing/validation, not ontology-source resolution. ggen-engine's own sync pipeline
// resolves ontology sources independently.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        assert_eq!(default_sparql_timeout(), 5000);
        assert_eq!(default_reasoning_timeout(), 5000);
        assert_eq!(default_output_dir(), PathBuf::from("."));
    }

    #[test]
    fn test_generation_mode_default() {
        let mode: GenerationMode = Default::default();
        assert_eq!(mode, GenerationMode::Create);
    }

    #[test]
    fn test_validation_severity_default() {
        let severity: ValidationSeverity = Default::default();
        assert_eq!(severity, ValidationSeverity::Error);
    }

    #[test]
    fn test_package_toml_resolve_output_key() {
        let mut pack_outputs = std::collections::HashMap::new();
        pack_outputs.insert("queries".to_string(), "legacy/sparql".to_string());
        pack_outputs.insert("shared".to_string(), "shared/dir".to_string());

        let mut top_outputs = std::collections::HashMap::new();
        top_outputs.insert("queries".to_string(), "src/sparql".to_string());

        let pkg = PackageToml {
            pack: Some(PackSection {
                outputs: pack_outputs,
            }),
            outputs: top_outputs,
        };

        // Preferred top-level key
        assert_eq!(pkg.resolve_output_key("queries"), "src/sparql");

        // Fallback to legacy pack key
        assert_eq!(pkg.resolve_output_key("shared"), "shared/dir");

        // Fallback to literal key
        assert_eq!(pkg.resolve_output_key("missing"), "missing");

        // Empty package.toml
        let empty_pkg = PackageToml::default();
        assert_eq!(empty_pkg.resolve_output_key("queries"), "queries");
    }
}
