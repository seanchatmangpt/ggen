//! `ggen.toml` configuration model, loaded via `star_toml`.
//!
//! Structural shape is closed at parse time (`deny_unknown_fields` on every
//! table, matching `schema/ggen-toml-schema.ttl` field-for-field — see
//! `tests/ggen_toml_schema_match.rs`). Semantic constraints beyond shape
//! (non-empty names, no path traversal in `[ontology].source`/`[templates].dir`)
//! are enforced by implementing `star_toml`'s [`star_toml::Validate`] trait
//! and running it via [`star_toml::Validate::validated`] after deserialization
//! — reusing `star_toml::Validator::check_path`'s existing traversal/null-byte
//! checks rather than reimplementing path safety here.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use star_toml::{Validate, Validator};

use crate::error::{AppError, Result};

/// Root model of a `ggen.toml` manifest.
///
/// `#[derive(JsonSchema)]` is load-bearing, not decorative: it is what lets
/// `tests/ggen_toml_schema_match.rs` compare this struct's *actual* field
/// set (via `schemars::schema_for!`) against `schema/ggen-toml-schema.ttl`,
/// instead of a hand-maintained mirror list that could itself drift — the
/// exact failure mode found in every sibling implementation's LSP/validator.
///
/// A second, independently-defined `ggen.toml` root type also exists:
/// [`ggen_config::manifest::GgenManifest`]. Both share the same top-level
/// table names (`project`/`ontology`/`packs`/`law`/…) but are deliberately
/// different shapes — this type's `[packs]` is a table-of-tables keyed by
/// pack name with an untagged [`PackRef`], and its [`Law`] carries both
/// `rules` and `shapes`; `GgenManifest` instead uses `[[packs]]`
/// array-of-tables with a flat `ggen_config::manifest::PackRef`, and a
/// rules-only `Law` (shapes live in `validation.shacl` there). Which schema
/// a given `ggen.toml` parses as is decided by `crate::schema_dispatch::load`
/// (backed by the shared `ggen_config::classify_ggen_toml` structural
/// classifier), before either typed parse runs. There is intentionally no
/// automated cross-schema equivalence guard between the two.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct GgenConfig {
    /// `[project]` table.
    pub project: Project,
    /// `[ontology]` table.
    pub ontology: Ontology,
    /// `[packs]` table: pack name → source reference.
    #[serde(default)]
    pub packs: BTreeMap<String, PackRef>,
    /// `[templates]` table.
    pub templates: Templates,
    /// `[law]` table: N3/Datalog rule files and SHACL shapes gating a sync.
    /// Optional — an absent table means no law stage runs (existing
    /// projects unchanged).
    #[serde(default)]
    pub law: Law,
}

/// `[law]` — law-state inputs for the sync pipeline: rule files are
/// materialized into the graph after the Enrich stage; shapes files gate
/// rendering (violations are a typed refusal).
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Law {
    /// N3/Datalog rule file paths, relative to the manifest, loaded in
    /// listed order.
    #[serde(default)]
    pub rules: Vec<PathBuf>,
    /// Turtle SHACL shapes file paths, relative to the manifest, each
    /// validated against the post-materialization graph.
    #[serde(default)]
    pub shapes: Vec<PathBuf>,
    /// Opt-in reflexive receipts: when `true`, `<root>/.ggen-v2/receipt-log.jsonl`
    /// (if it exists) is parsed at Stage 1 and each successfully-parsed sync
    /// receipt is inserted into the graph as a fixed-shape `ggenr:Sync`
    /// fact cluster, before any template renders — so SPARQL SELECTs in
    /// templates can see the project's own sync history. Defaults `false`:
    /// existing projects are entirely unaffected (byte-identical output and
    /// receipt/lock behavior) unless they opt in.
    #[serde(default)]
    pub reflexive: bool,
}

/// `[project]` — identity of the generating project.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Project {
    /// Project name.
    pub name: String,
}

/// `[ontology]` — the RDF source of truth and its namespace prefixes.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Ontology {
    /// Path to the ontology file (Turtle), relative to the manifest.
    pub source: PathBuf,
    /// Prefix → namespace IRI map.
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
}

/// A pack source reference: either a local path or a git coordinate.
///
/// Not the same type as [`ggen_config::manifest::PackRef`] — that one is a
/// flat `{name, registry, path, version}` struct, not this untagged
/// `Path | Git` enum. See [`GgenConfig`]'s struct doc for the fuller
/// schema-divergence note.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(untagged)]
pub enum PackRef {
    /// Local pack: `{ path = "…" }`, optionally with
    /// `extra_ontologies = ["…"]` — additional Turtle files, resolved
    /// relative to the manifest, unioned into the pack's graph after its
    /// own `ontology.ttl`. This replaces the per-pack `make-ontology.sh`
    /// committed-union convention (ocel-bench-pack): the union is declared
    /// in `ggen.toml` and materialized at sync time, so a pack ontology can
    /// no longer drift from its sources. Each extra file joins the pack's
    /// content hash (ggen.lock) and the sync input closure.
    Path {
        /// Filesystem path to the pack directory.
        path: PathBuf,
        /// Additional ontology Turtle files unioned into the pack graph,
        /// relative to the manifest. Empty when omitted.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        extra_ontologies: Vec<PathBuf>,
        /// Whether this pack participates in `ggen.lock` content-hash
        /// pinning. Defaults to `true` (existing behavior, unchanged). Set
        /// to `false` for a pack whose `ontology.ttl` is a regenerated
        /// output projection rather than a stable source (e.g. a sibling
        /// checkout's `standing-pack`, rewritten by every `just standing`
        /// run) — content-hash pinning is fundamentally the wrong contract
        /// for such a pack, since every regeneration would otherwise trip a
        /// spurious `FM-PACK-008` content-hash-mismatch refusal. An
        /// unlocked pack is never checked against `ggen.lock` and never
        /// written to it (see `pack::lock_entries`).
        #[serde(default = "default_true")]
        lock: bool,
    },
    /// Remote pack: `{ git = "…", version = "…" }`.
    Git {
        /// Git repository URL.
        git: String,
        /// Version requirement (tag or semver).
        version: String,
    },
}

/// `serde(default = "…")` helper: `PackRef::Path::lock` defaults to `true`
/// (existing pin-and-check behavior) when the key is omitted from
/// `ggen.toml`.
fn default_true() -> bool {
    true
}

/// `[templates]` — where Tera templates live.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Templates {
    /// Template directory, relative to the manifest.
    pub dir: PathBuf,
    /// When `true`, sync emits one engine-owned `src/ggen_pack_mods.rs`
    /// aggregator listing a `#[path = "..."] pub mod ...;` mount for every
    /// generated `src/*.rs` output (excluding the aggregator itself), so a
    /// consumer's own `lib.rs` needs exactly one permanent
    /// `include!("ggen_pack_mods.rs");` line instead of one hand-written
    /// mount per pack. Single writer (the engine), so two packs can never
    /// collide on it the way per-pack `to: src/lib.rs` templates did
    /// (`FM-WRITE-008`). Default `false`: existing consumers unchanged.
    #[serde(default)]
    pub aggregate_modules: bool,
}

impl GgenConfig {
    /// Load and parse a `ggen.toml` file, then run semantic validation.
    ///
    /// # Errors
    /// Returns `[FM-CONFIG-001]` when the file is missing or unreadable,
    /// `[FM-CONFIG-002]` on TOML syntax errors or unknown keys, and
    /// `[FM-CONFIG-003]` when parsing succeeds but a semantic invariant
    /// fails (empty name, unsafe path) — fail closed at every stage.
    pub fn load(path: &Path) -> Result<Self> {
        let parsed = star_toml::load_file::<Self>(path).map_err(|e| match e {
            star_toml::Error::FileNotFound(p) => AppError::fm_config(
                1,
                format!(
                    "ggen.toml not found at `{}`. Remediation: create the manifest or fix the path.",
                    p.display()
                ),
            ),
            star_toml::Error::Io { .. } => AppError::fm_config(
                1,
                format!("cannot read `{}`: {e}. Remediation: check file permissions.", path.display()),
            ),
            other => AppError::fm_config(
                2,
                format!(
                    "invalid ggen.toml at `{}`: {other}. Remediation: fix the TOML syntax or remove unknown keys.",
                    path.display()
                ),
            ),
        })?;
        parsed.validated().map_err(|errs| {
            AppError::fm_config(
                3,
                format!(
                    "ggen.toml at `{}` failed semantic validation: {errs}. \
                     Remediation: fix the reported field(s).",
                    path.display()
                ),
            )
        })
    }

    /// Parse a `ggen.toml` document from a string (env vars expanded by
    /// `star_toml` before parsing), then run semantic validation.
    ///
    /// # Errors
    /// Returns `[FM-CONFIG-002]` on TOML syntax errors or unknown keys, and
    /// `[FM-CONFIG-003]` when a semantic invariant fails.
    pub fn from_toml_str(toml: &str) -> Result<Self> {
        let parsed = star_toml::from_str::<Self>(toml).map_err(|e| {
            AppError::fm_config(
                2,
                format!("invalid ggen.toml document: {e}. Remediation: fix the TOML syntax or remove unknown keys."),
            )
        })?;
        parsed.validated().map_err(|errs| {
            AppError::fm_config(
                3,
                format!(
                    "ggen.toml failed semantic validation: {errs}. \
                     Remediation: fix the reported field(s)."
                ),
            )
        })
    }
}

impl Validate for Project {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("name", &self.name);
    }
}

impl Validate for Ontology {
    fn validate(&self, v: &mut Validator) {
        // Declared "relative to the manifest" in schema/ggen-toml-schema.ttl;
        // `must_be_absolute: Some(false)` matches that contract.
        v.check_path("source", &self.source.to_string_lossy(), Some(false));
    }
}

impl Validate for Templates {
    fn validate(&self, v: &mut Validator) {
        v.check_path("dir", &self.dir.to_string_lossy(), Some(false));
    }
}

impl Validate for PackRef {
    fn validate(&self, v: &mut Validator) {
        match self {
            // Pack paths legitimately reference sibling directories
            // (`../foo-pack`, per tests/cross_pack_matrix.rs and the pack
            // layout convention found across every real consumer pack
            // surveyed for this ticket) — `check_path`'s traversal rejection
            // does not apply here, unlike `ontology.source`/`templates.dir`
            // which must stay within the project. Only non-emptiness is
            // required.
            Self::Path {
                path,
                extra_ontologies,
                lock: _,
            } => {
                v.check_non_empty("path", &path.to_string_lossy());
                // Extra ontologies resolve relative to the manifest and must
                // stay inside the project (same contract as
                // `ontology.source`), unlike the pack `path` itself which
                // may legitimately reference a sibling checkout.
                for (i, extra) in extra_ontologies.iter().enumerate() {
                    v.check_path(
                        &format!("extra_ontologies[{i}]"),
                        &extra.to_string_lossy(),
                        Some(false),
                    );
                }
                // `lock` is a bool — nothing to validate.
            }
            Self::Git { git, version } => {
                v.check_non_empty("git", git);
                v.check_non_empty("version", version);
            }
        }
    }
}

impl Validate for Law {
    fn validate(&self, v: &mut Validator) {
        for (i, rule) in self.rules.iter().enumerate() {
            v.check_path(&format!("rules[{i}]"), &rule.to_string_lossy(), Some(false));
        }
        for (i, shape) in self.shapes.iter().enumerate() {
            v.check_path(
                &format!("shapes[{i}]"),
                &shape.to_string_lossy(),
                Some(false),
            );
        }
    }
}

impl Validate for GgenConfig {
    fn validate(&self, v: &mut Validator) {
        v.field("project", |v| self.project.validate(v));
        v.field("ontology", |v| self.ontology.validate(v));
        v.field("templates", |v| self.templates.validate(v));
        v.field("law", |v| self.law.validate(v));
        for (name, pack_ref) in &self.packs {
            v.field(&format!("packs.{name}"), |v| pack_ref.validate(v));
        }
    }
}
