//! A single, shared structural classifier for `ggen.toml`'s two
//! independently-defined, incompatible schemas.
//!
//! # Why this exists (specs/014-ggen-core-replacement, correction 2 / Blocker A part 1)
//!
//! A prior investigation (see the call-site map in this ticket's own history) found
//! that `ggen.toml` is parsed by one of two independently-defined struct
//! hierarchies, chosen by a raw-text pre-parse before any typed parse runs:
//!
//! - [`crate::manifest::GgenManifest`] (this crate) — the "declarative-rules"
//!   schema: `[[generation.rules]]`, flat `PackRef { name, registry, path, version }`
//!   as a `[[packs]]` array-of-tables, a rules-only `Law` (SHACL shapes live in
//!   `validation.shacl` instead), and a long tail of optional operational tables
//!   (`[ai]`, `[sparql]`, `[mcp]`, …) reused from [`crate::config_lib`].
//! - `ggen_engine::config::GgenConfig` (a *different* crate, not this one) — the
//!   "frontmatter" schema: a `[templates].dir`-driven per-template-file
//!   convention, an untagged `PackRef` enum (`{path,…}` / `{git,…}`) as a
//!   `[packs]` table-of-tables, and a `Law{rules, shapes}` with both fields.
//!
//! Only one call site (`ggen_engine::sync::sync`'s Stage 0 dispatch, via
//! `ggen_engine::generation_rules::has_generation_rules`) currently picks between
//! the two correctly. Five others (`ggen doctor`, `ggen graph validate`
//! project-mode, all five `ggen law *` verbs, and `ggen-lsp`'s
//! `ProjectIndex::from_root_with_overlay`) each hardcode one schema
//! unconditionally and reject the other schema's real, valid manifests. This
//! module is the ONE classifier every one of those call sites should be
//! repointed to (that repointing is a *following* task — this module only
//! needs to be correct and well-tested here; see the module-level `# Scope`
//! note below for exactly what "correct" means).
//!
//! # Why this lives in `ggen-config`, not `ggen-engine`
//!
//! `ggen-engine` depends on `ggen-config` (for [`crate::manifest::GgenManifest`]
//! and [`crate::config_lib`]) — confirmed via `crates/ggen-engine/Cargo.toml`'s
//! `ggen-config = { path = "../ggen-config", … }` entry. `ggen_engine::config::
//! GgenConfig` (the frontmatter schema's *type*) lives in `ggen-engine` itself.
//! If this classifier needed to construct or pattern-match on that concrete
//! type, this module would have to live in `ggen-engine`, or `ggen-config`
//! would need a new `ggen-config -> ggen-engine` dependency edge — which,
//! combined with the existing `ggen-engine -> ggen-config` edge, would be a
//! real dependency cycle (confirmed: `grep -n "ggen-engine" crates/ggen-config/
//! Cargo.toml` finds no dependency, only comments — this crate currently has
//! zero edge back to `ggen-engine`, and this module must not create one).
//!
//! The way out is the same one `ggen_engine::generation_rules::
//! has_generation_rules` (this module's narrower ancestor) already uses: reason
//! about the raw `toml::Table` *structurally* — table/array shape, key
//! presence/absence — never by constructing either schema's typed struct. That
//! needs only the `toml` crate (already a direct dependency of this crate;
//! see `Cargo.toml`'s `toml = "1.1.0"`), so this module has zero new
//! dependencies and is reachable, dependency-wise, from every real call site
//! in the investigation's map: `ggen-engine` (already depends on
//! `ggen-config`), `ggen-lsp` (already depends on both), and `ggen-cli`
//! (already depends on both).
//!
//! # Scope — a shape classifier, not a validator
//!
//! **This module answers "which schema does this document's *shape* belong
//! to", never "is this document fully valid."** It deliberately does not
//! attempt to construct either schema's typed struct (see above for why), so
//! it cannot and does not catch:
//!
//! - A document missing a field required by the schema its shape otherwise
//!   matches (e.g. `[[generation.rules]]`-shaped but missing `[ontology]`
//!   entirely — [`GgenManifest::ontology`] has no `#[serde(default)]`, so a
//!   real [`crate::manifest::ManifestParser::parse_str`] call would reject
//!   it, but this classifier still reports [`ConfigSchemaClassification::
//!   DeclarativeRules`], because the *shape* it detected is real). See
//!   `missing_required_field_still_classifies_by_shape_not_full_validity`
//!   below for a test that proves this split (both halves): shape says
//!   `DeclarativeRules`, the real typed parser independently says `Err`.
//! - An unrecognized top-level table that neither schema declares (e.g.
//!   `[bogus_table]`) — a real typed parse would reject it via
//!   `#[serde(deny_unknown_fields)]` on both schemas' root structs; this
//!   classifier has no such check and simply does not count it as evidence
//!   for either schema. See `unrecognized_extra_table_does_not_change_shape_classification`.
//!
//! Both gaps are deliberate, not oversights: closing them would require
//! attempting an actual typed parse of at least one schema, which is exactly
//! the circular-dependency trap described above. The five-way outcome this
//! module *does* commit to is purely structural: syntax validity, then
//! table/array shape, then required-vs-forbidden key presence for each
//! schema's *set of keys* (not each key's value validity).
//!
//! # Decision procedure
//!
//! ```text
//! read raw ggen.toml text
//!   -> not syntactically valid TOML                         => Malformed
//!   -> detect structural markers for each known schema
//!      -> a *strong*, schema-specific declarative marker fired
//!         ([generation] table present, or [[packs]] array-of-tables)
//!         and no frontmatter marker fired                    => DeclarativeRules
//!         (these two markers are structurally impossible on the
//!          frontmatter schema at all, so they are decisive alone --
//!          see `# Marker strength` below)
//!      -> more than one schema's markers fired               => Ambiguous
//!      -> only *weak* declarative marker(s) fired (bare [project].version
//!         presence, or a DECLARATIVE_ONLY_TABLES name that GgenManifest
//!         happens to also declare) and no frontmatter marker fired
//!         -> also positively satisfies the frontmatter schema's three
//!            required fields (project.name, ontology.source,
//!            templates.dir)                                  => Ambiguous
//!            (a weak, coincidental declarative signal must not silently
//!             override a complete, positive frontmatter shape match --
//!             see `# Marker strength` below, F2)
//!         -> otherwise                                        => Unsupported
//!            (weak markers alone are not confident evidence for either
//!             schema)
//!      -> a known *older* schema's markers fired              => (reserved: CONFIG_SCHEMA_MIGRATION_REQUIRED;
//!                                                                  not implemented this pass -- there is no
//!                                                                  older schema registered yet, see correction 2)
//!      -> no schema's markers fired
//!         -> satisfies the frontmatter schema's three required
//!            fields (project.name, ontology.source, templates.dir) => Frontmatter (positive
//!                                                                      confirmation, not "assumed
//!                                                                      by elimination")
//!         -> otherwise                                        => Unsupported
//! ```
//!
//! # Marker strength (correction 3, F2)
//!
//! Not every declarative marker is equally decisive. `[generation]`'s mere
//! presence and `[[packs]]`'s array shape are *structurally impossible* on
//! the frontmatter schema (`ggen_engine::config::GgenConfig` has no
//! `generation` field at all, `deny_unknown_fields`; its `packs` is a
//! map-of-tables, never an array-of-tables) -- these are **strong** markers,
//! decisive on their own.
//!
//! `[project].version`'s bare presence and a `DECLARATIVE_ONLY_TABLES` name
//! are **weak**: real signals, but not schema-specific enough to be trusted
//! alone. A real, pre-existing repository file
//! (`specs/012-grand-unified-kgc-thesis/ggen.toml`) proves why: it has
//! `[project]` with both `name` and `version`, and coincidentally uses
//! `[sparql]`/`[logging]`/`[security]` (all on `DECLARATIVE_ONLY_TABLES`) for
//! its own unrelated purposes, but has no `[generation]` table and no
//! `[ontology]` table at all -- a legacy/frontmatter-shaped project that a
//! weak-marker-only reading previously misclassified as `DeclarativeRules`,
//! which then failed the real typed parser with a confusing
//! `unknown field 'author'` error that never mentioned the actual problem
//! (missing `[generation]`/`[ontology]`). When only weak markers fire and no
//! frontmatter marker fires, this classifier now defers to
//! `satisfies_frontmatter_minimum` before committing to `DeclarativeRules` --
//! see `weak_declarative_marker_alone_does_not_override_full_frontmatter_match`
//! and `weak_declarative_markers_alone_without_frontmatter_minimum_are_unsupported`
//! below.
//!
//! This is a pure function of the input text: no filesystem access beyond
//! what the caller already did to obtain `raw`, no environment inspection, no
//! "first parser that happens to succeed" ordering dependency — the exact
//! failure mode this classifier replaces (see the five bypassing call sites
//! in the module's opening paragraph, each of which picks a parser by
//! hardcoded call-site identity rather than by asking the document what it
//! is).

use std::collections::BTreeSet;

use toml::Value;

// ---------------------------------------------------------------------------
// Typed outcome identifiers
// ---------------------------------------------------------------------------
//
// This crate's manifest-validation surface already has one typed-code
// convention (`crate::manifest::validation`'s inline `"error[E00NN]: …"`
// strings, e.g. E0010/E0011/E0013/E0014) and `ggen-engine`'s `AppError` has a
// second, bracket-prefixed one (`[FM-CONFIG-{code:03}]`, via
// `AppError::fm_config`). Both are "a short, stable, grep-able code embedded
// in the diagnostic text." This module reuses the second convention's
// *shape* (`FM-CONFIG-NNN`) rather than inventing a third: it is the more
// apt family for a `ggen.toml`-*configuration* concern specifically (as
// opposed to `manifest::validation`'s E00NN codes, which are about a single
// already-typed `GgenManifest`'s semantic content, not about which schema a
// raw document belongs to). These are ggen-config's own `FM-CONFIG-1xx`
// sub-range -- there is no shared cross-crate code registry, so there is no
// literal collision with `ggen_engine::AppError::fm_config`'s low-numbered
// codes (1/2/3); the numbering was chosen to *read* as clearly distinct
// anyway.

/// `[FM-CONFIG-100]` Exactly one recognized schema's structural markers
/// fired — see [`ConfigSchemaClassification::DeclarativeRules`] /
/// [`ConfigSchemaClassification::Frontmatter`].
pub const CONFIG_SCHEMA_SUPPORTED: &str = "FM-CONFIG-100";

/// `[FM-CONFIG-101]` Structural markers from more than one recognized schema
/// fired simultaneously — see [`ConfigSchemaClassification::Ambiguous`].
pub const CONFIG_SCHEMA_AMBIGUOUS: &str = "FM-CONFIG-101";

/// `[FM-CONFIG-102]` No recognized schema's structural markers fired.
///
/// The document is syntactically valid TOML, but it does not positively
/// satisfy the frontmatter schema's minimum required fields either — see
/// [`ConfigSchemaClassification::Unsupported`].
pub const CONFIG_SCHEMA_UNSUPPORTED: &str = "FM-CONFIG-102";

/// `[FM-CONFIG-103]` The document is not syntactically valid TOML — see
/// [`ConfigSchemaClassification::Malformed`].
pub const CONFIG_PARSE_FAILED: &str = "FM-CONFIG-103";

/// `[FM-CONFIG-104]` RESERVED, not implemented this pass.
///
/// Would mark a document whose structural markers match a *known older*
/// (superseded) schema this classifier has been taught to recognize,
/// distinct from today's "doesn't match anything"
/// [`CONFIG_SCHEMA_UNSUPPORTED`]. No [`ConfigSchemaClassification`] variant
/// currently produces this code — there is no older schema registered yet.
/// Migration support is explicitly sequenced *after* this classifier is
/// proven correct on today's five-way outcome (correction 2's own
/// ordering).
#[allow(dead_code)]
pub const CONFIG_SCHEMA_MIGRATION_REQUIRED: &str = "FM-CONFIG-104";

// ---------------------------------------------------------------------------
// ConfigSchemaClassification
// ---------------------------------------------------------------------------

/// Five-way outcome of classifying a raw `ggen.toml` document's *shape* — see
/// the module doc comment's `# Scope` section for exactly what this does and
/// does not verify.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigSchemaClassification {
    /// The document's structural markers match the declarative-rules schema
    /// ([`crate::manifest::GgenManifest`]) and only that schema.
    DeclarativeRules,
    /// The document's structural markers match the frontmatter schema
    /// (`ggen_engine::config::GgenConfig`) and only that schema.
    Frontmatter,
    /// Structural markers from more than one recognized schema fired at
    /// once. `matched` lists every fired marker (sorted, deduplicated),
    /// each prefixed by the schema it belongs to (`"declarative:…"` /
    /// `"frontmatter:…"`) so a caller can see exactly which signals
    /// conflicted.
    Ambiguous { matched: Vec<String> },
    /// The document is syntactically valid TOML, but no recognized schema's
    /// structural markers fired, and it does not positively satisfy the
    /// frontmatter schema's minimum required fields either.
    /// `observed_markers` lists the document's top-level table names
    /// (`"unknown_top_level_table:<name>"`) as a diagnostic breadcrumb —
    /// empty for a genuinely empty document.
    Unsupported { observed_markers: Vec<String> },
    /// The document is not syntactically valid TOML. `diagnostic` embeds
    /// [`CONFIG_PARSE_FAILED`] and the underlying TOML parser's error.
    Malformed { diagnostic: String },
}

impl ConfigSchemaClassification {
    /// The typed outcome identifier for this classification — one of
    /// [`CONFIG_SCHEMA_SUPPORTED`], [`CONFIG_SCHEMA_AMBIGUOUS`],
    /// [`CONFIG_SCHEMA_UNSUPPORTED`], or [`CONFIG_PARSE_FAILED`].
    #[must_use]
    pub fn code(&self) -> &'static str {
        match self {
            Self::DeclarativeRules | Self::Frontmatter => CONFIG_SCHEMA_SUPPORTED,
            Self::Ambiguous { .. } => CONFIG_SCHEMA_AMBIGUOUS,
            Self::Unsupported { .. } => CONFIG_SCHEMA_UNSUPPORTED,
            Self::Malformed { .. } => CONFIG_PARSE_FAILED,
        }
    }
}

// ---------------------------------------------------------------------------
// Marker detection
// ---------------------------------------------------------------------------

/// Top-level tables that exist on [`crate::manifest::GgenManifest`] (directly
/// or via a `#[serde(default)]` `Option<config_lib::*>` field) but do not
/// exist at all on `ggen_engine::config::GgenConfig` (whose only top-level
/// fields are `project`/`ontology`/`packs`/`templates`/`law`, all under
/// `#[serde(deny_unknown_fields)]`). `generation` and `law` are handled by
/// their own dedicated checks below (both are more specific/higher-signal
/// than a bare presence check), so they are intentionally not repeated here.
const DECLARATIVE_ONLY_TABLES: &[&str] = &[
    "inference",
    "validation",
    "sync",
    "output",
    "ai",
    "sparql",
    "lifecycle",
    "security",
    "performance",
    "logging",
    "telemetry",
    "features",
    "env",
    "build",
    "test",
    "package",
    "mcp",
    "a2a",
];

/// Classify a raw `ggen.toml` document's text against the two schemas this
/// workspace currently recognizes.
///
/// Pure function: no filesystem access, no environment inspection — see the
/// module doc comment for the full decision procedure and its documented
/// scope boundary.
#[must_use]
pub fn classify_ggen_toml(raw: &str) -> ConfigSchemaClassification {
    let table: toml::Table = match raw.parse() {
        Ok(t) => t,
        Err(e) => {
            return ConfigSchemaClassification::Malformed {
                diagnostic: format!(
                    "[{CONFIG_PARSE_FAILED}] invalid ggen.toml document: {e}. \
                     Remediation: fix the TOML syntax."
                ),
            };
        }
    };

    let mut declarative: BTreeSet<String> = BTreeSet::new();
    let mut frontmatter: BTreeSet<String> = BTreeSet::new();

    // -- [project].version: required (non-Option) on GgenManifest's
    //    ProjectConfig; entirely absent from GgenConfig's Project{name}. Its
    //    presence/absence under an existing [project] table is a two-way
    //    discriminator (mutually exclusive by construction: a key is either
    //    present or absent, never both). --------------------------------
    if let Some(project) = table.get("project").and_then(Value::as_table) {
        if project.contains_key("version") {
            declarative.insert("declarative:project_version_present".to_string());
        } else {
            frontmatter.insert("frontmatter:project_missing_version".to_string());
        }
    }

    // -- [generation]: a *required* field on GgenManifest (no
    //    `#[serde(default)]`) that does not exist at all on GgenConfig
    //    (deny_unknown_fields) -- mere presence, even with an empty/absent
    //    `rules` array, is already fatal to a frontmatter parse. This
    //    generalizes `ggen_engine::generation_rules::has_generation_rules`'s
    //    narrower non-empty-array-only check, closing the gap that prior
    //    investigation flagged (a `[generation]\nrules = []` document was
    //    previously mis-routed to the frontmatter parser). ------------------
    if let Some(generation) = table.get("generation").and_then(Value::as_table) {
        declarative.insert("declarative:generation_table_present".to_string());
        if generation
            .get("rules")
            .and_then(Value::as_array)
            .is_some_and(|a| !a.is_empty())
        {
            declarative.insert("declarative:generation_rules_non_empty".to_string());
        }
    }

    // -- [law].shapes: present on GgenConfig's Law{rules, shapes}; GgenManifest's
    //    Law has only `rules` (deny_unknown_fields), so `shapes` is fatal there. --
    if table
        .get("law")
        .and_then(Value::as_table)
        .is_some_and(|law| law.contains_key("shapes"))
    {
        frontmatter.insert("frontmatter:law_shapes_present".to_string());
    }

    // -- packs shape: GgenManifest's `packs: Vec<PackRef>` is `[[packs]]`
    //    array-of-tables; GgenConfig's `packs: BTreeMap<String, PackRef>` is a
    //    `[packs]` table-of-tables. A TOML value is an array XOR a table, never
    //    both, so this is a hard, content-independent discriminator. The
    //    `name` key inside each entry is a secondary, redundant confirmation
    //    (GgenManifest's flat PackRef requires it; GgenConfig's untagged
    //    Path|Git enum has no `name` variant field at all). --------------------
    match table.get("packs") {
        Some(Value::Array(entries)) => {
            declarative.insert("declarative:packs_array_shaped".to_string());
            if entries
                .iter()
                .any(|e| e.as_table().is_some_and(|t| t.contains_key("name")))
            {
                declarative.insert("declarative:packref_entry_has_name".to_string());
            }
        }
        Some(Value::Table(map)) => {
            frontmatter.insert("frontmatter:packs_table_shaped".to_string());
            if map
                .values()
                .any(|v| v.as_table().is_some_and(|t| !t.contains_key("name")))
            {
                frontmatter.insert("frontmatter:packref_entry_missing_name".to_string());
            }
        }
        _ => {}
    }

    // -- Any other GgenManifest-only top-level table. None of these exist on
    //    GgenConfig's deny_unknown_fields shape, so any one is individually
    //    fatal to a frontmatter parse while GgenManifest declares all of them
    //    as optional. -----------------------------------------------------
    for name in DECLARATIVE_ONLY_TABLES {
        if table.contains_key(*name) {
            declarative.insert(format!("declarative:extra_table_present:{name}"));
        }
    }

    // A minimal, schema-specific subset of `declarative`'s markers -- see the
    // module doc comment's `# Marker strength` section (F2). Each is
    // structurally impossible on the frontmatter schema at all, so any one
    // alone is decisive regardless of anything else observed. Every other
    // declarative marker (bare `[project].version` presence, or a
    // `DECLARATIVE_ONLY_TABLES` name) is comparatively weak: real, but not
    // schema-specific enough to override a document that otherwise fully
    // matches the frontmatter schema's required shape.
    const STRONG_DECLARATIVE_MARKERS: &[&str] = &[
        "declarative:generation_table_present",
        "declarative:packs_array_shaped",
    ];
    let has_strong_declarative = declarative
        .iter()
        .any(|m| STRONG_DECLARATIVE_MARKERS.contains(&m.as_str()));

    match (declarative.is_empty(), frontmatter.is_empty()) {
        (false, true) if has_strong_declarative => ConfigSchemaClassification::DeclarativeRules,
        (false, true) => {
            // Only weak declarative marker(s) fired and no frontmatter
            // marker fired -- not schema-specific enough to force
            // `DeclarativeRules` on its own (module doc `# Marker strength`,
            // F2). A document that also positively satisfies the
            // frontmatter minimum genuinely conflicts (it would fail a real
            // parse of *both* schemas) rather than being silently forced
            // one way; a document that satisfies neither is honestly
            // `Unsupported`, not a confident (and wrong) `DeclarativeRules`.
            if satisfies_frontmatter_minimum(&table) {
                let mut matched: Vec<String> = declarative.into_iter().collect();
                matched.push("frontmatter:satisfies_minimum_shape".to_string());
                matched.sort();
                ConfigSchemaClassification::Ambiguous { matched }
            } else {
                ConfigSchemaClassification::Unsupported {
                    observed_markers: observed_top_level_tables(&table),
                }
            }
        }
        (true, false) => ConfigSchemaClassification::Frontmatter,
        (false, false) => {
            let mut matched: Vec<String> = declarative.into_iter().collect();
            matched.extend(frontmatter);
            matched.sort();
            ConfigSchemaClassification::Ambiguous { matched }
        }
        (true, true) => {
            if satisfies_frontmatter_minimum(&table) {
                ConfigSchemaClassification::Frontmatter
            } else {
                ConfigSchemaClassification::Unsupported {
                    observed_markers: observed_top_level_tables(&table),
                }
            }
        }
    }
}

/// Top-level table names of `table`, each prefixed `unknown_top_level_table:`
/// -- a diagnostic breadcrumb shared by both [`ConfigSchemaClassification::Unsupported`]
/// call sites in [`classify_ggen_toml`].
fn observed_top_level_tables(table: &toml::Table) -> Vec<String> {
    table
        .keys()
        .map(|k| format!("unknown_top_level_table:{k}"))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// Positive confirmation of the frontmatter schema's three required fields
/// (`project.name`, `ontology.source`, `templates.dir`) — used only when no
/// exclusive marker fired for either schema, so a document is never assumed
/// to be "frontmatter" merely by failing to look declarative. `templates.dir`
/// itself is the frontmatter-only marker the investigation flagged as too
/// weak to be a hard discriminator on its own (GgenManifest's optional,
/// non-`deny_unknown_fields` `TemplatesConfig` silently tolerates a stray
/// `dir` key) — folding it into this three-field *conjunction* rather than
/// treating it as an independent marker is exactly what keeps it from
/// spuriously forcing [`ConfigSchemaClassification::Ambiguous`] against an
/// unrelated declarative marker elsewhere in the same document.
fn satisfies_frontmatter_minimum(table: &toml::Table) -> bool {
    let has_project_name = table
        .get("project")
        .and_then(Value::as_table)
        .and_then(|p| p.get("name"))
        .and_then(Value::as_str)
        .is_some_and(|s| !s.is_empty());
    let has_ontology_source = table
        .get("ontology")
        .and_then(Value::as_table)
        .and_then(|o| o.get("source"))
        .and_then(Value::as_str)
        .is_some();
    let has_templates_dir = table
        .get("templates")
        .and_then(Value::as_table)
        .and_then(|t| t.get("dir"))
        .and_then(Value::as_str)
        .is_some();
    has_project_name && has_ontology_source && has_templates_dir
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use crate::manifest::ManifestParser;

    // -- 1. Authoritative (declarative-rules) schema, valid ------------------

    #[test]
    fn declarative_rules_schema_classifies_as_declarative_rules() {
        let toml = r#"
[project]
name = "my-domain"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[[generation.rules]]
name = "structs"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "src/models/out.rs"
"#;
        let got = classify_ggen_toml(toml);
        assert_eq!(got, ConfigSchemaClassification::DeclarativeRules);
        assert_eq!(got.code(), CONFIG_SCHEMA_SUPPORTED);
        // Real cross-check: the actual typed parser for this schema agrees.
        assert!(ManifestParser::parse_str(toml).is_ok());
    }

    #[test]
    fn declarative_rules_schema_with_empty_rules_array_still_classifies_correctly() {
        // Closes the gap the prior investigation flagged: `has_generation_rules`
        // alone would return `false` here (empty array), but the `[generation]`
        // table's mere presence is still fatal to a frontmatter parse and
        // required on GgenManifest, so this must still classify DeclarativeRules.
        let toml = "[project]\nname = \"x\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[generation]\nrules = []\n";
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::DeclarativeRules
        );
    }

    // -- 2. Compatible (frontmatter) schema, valid ---------------------------

    #[test]
    fn frontmatter_schema_classifies_as_frontmatter() {
        let toml = r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"
"#;
        let got = classify_ggen_toml(toml);
        assert_eq!(got, ConfigSchemaClassification::Frontmatter);
        assert_eq!(got.code(), CONFIG_SCHEMA_SUPPORTED);
    }

    #[test]
    fn frontmatter_schema_with_law_shapes_classifies_as_frontmatter() {
        let toml = r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"

[law]
rules = ["r.n3"]
shapes = ["s.ttl"]
"#;
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::Frontmatter
        );
    }

    #[test]
    fn frontmatter_schema_with_table_shaped_packs_classifies_as_frontmatter() {
        let toml = r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"

[packs.mypack]
path = "../mypack"
"#;
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::Frontmatter
        );
    }

    // -- 3. Malformed TOML ----------------------------------------------------

    #[test]
    fn malformed_toml_syntax_is_malformed() {
        let got = classify_ggen_toml("not [ valid toml");
        match got {
            ConfigSchemaClassification::Malformed { diagnostic } => {
                assert!(diagnostic.contains(CONFIG_PARSE_FAILED), "{diagnostic}");
            }
            other => panic!("expected Malformed, got {other:?}"),
        }
    }

    // -- 4. Missing required field (shape says one thing, real parse disagrees) --

    #[test]
    fn missing_required_field_still_classifies_by_shape_not_full_validity() {
        // `[ontology]` is entirely absent -- a required (non-Option) field on
        // GgenManifest. The document's *shape* (project.version + [generation])
        // is unambiguously declarative-rules, so the classifier must still say
        // so -- catching a missing required field is explicitly out of scope
        // for a shape classifier (see the module doc comment's `# Scope`
        // section). The real typed parser independently proves the field really
        // is missing, so this test exhibits both halves of the split, not just
        // asserts it.
        let toml = "[project]\nname = \"x\"\nversion = \"1.0.0\"\n\n[generation]\nrules = []\n";
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::DeclarativeRules
        );
        let err = ManifestParser::parse_str(toml).expect_err("ontology is required, must fail");
        assert!(
            err.to_string().to_lowercase().contains("ontology"),
            "expected the real parser to complain about missing `ontology`, got: {err}"
        );
    }

    // -- 5. Unknown/unrecognized field ----------------------------------------

    #[test]
    fn unrecognized_extra_table_does_not_change_shape_classification() {
        // `[bogus_table]` is on neither schema's known-field list. A real typed
        // parse of either schema (both `deny_unknown_fields`) would reject it,
        // but this classifier has no such check by design (see `# Scope`) --
        // the document's shape is still unambiguously frontmatter.
        let toml = r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"

[bogus_table]
whatever = 1
"#;
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::Frontmatter
        );
    }

    // -- 6. Genuinely ambiguous document ---------------------------------------

    #[test]
    fn document_with_conflicting_markers_is_ambiguous() {
        // Frontmatter-shaped (`[project]` without `version`) but also carries
        // `[ai]`, a GgenManifest-only table that is individually fatal to a
        // frontmatter parse (deny_unknown_fields) -- a real project that
        // started as frontmatter and had an operational block added.
        let toml = r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"

[ai]
provider = "openai"
"#;
        match classify_ggen_toml(toml) {
            ConfigSchemaClassification::Ambiguous { matched } => {
                assert!(
                    matched.contains(&"frontmatter:project_missing_version".to_string()),
                    "{matched:?}"
                );
                assert!(
                    matched.contains(&"declarative:extra_table_present:ai".to_string()),
                    "{matched:?}"
                );
            }
            other => panic!("expected Ambiguous, got {other:?}"),
        }
    }

    #[test]
    fn ambiguous_document_reports_the_ambiguous_code() {
        let toml = "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[templates]\ndir = \"t\"\n\n[mcp]\nenabled = true\n";
        let got = classify_ggen_toml(toml);
        assert!(matches!(got, ConfigSchemaClassification::Ambiguous { .. }));
        assert_eq!(got.code(), CONFIG_SCHEMA_AMBIGUOUS);
    }

    // -- 7. Document matching neither schema ------------------------------------

    #[test]
    fn document_matching_neither_schema_is_unsupported() {
        let toml = "[some_other_tool]\nkey = \"value\"\n";
        match classify_ggen_toml(toml) {
            ConfigSchemaClassification::Unsupported { observed_markers } => {
                assert!(
                    observed_markers
                        .contains(&"unknown_top_level_table:some_other_tool".to_string()),
                    "{observed_markers:?}"
                );
            }
            other => panic!("expected Unsupported, got {other:?}"),
        }
    }

    #[test]
    fn empty_document_is_unsupported_not_malformed() {
        // Syntactically valid TOML (an empty document parses to an empty
        // table), but it satisfies neither schema's markers nor the
        // frontmatter minimum -- Unsupported, not a parse failure.
        let got = classify_ggen_toml("");
        match &got {
            ConfigSchemaClassification::Unsupported { observed_markers } => {
                assert!(observed_markers.is_empty(), "{observed_markers:?}");
            }
            other => panic!("expected Unsupported, got {other:?}"),
        }
        assert_eq!(got.code(), CONFIG_SCHEMA_UNSUPPORTED);
    }

    #[test]
    fn unsupported_document_missing_project_and_templates_is_unsupported() {
        // No `[project]` table at all, so neither the declarative
        // (`project.version` present) nor the frontmatter (`project` present
        // *without* `version`) project marker fires -- both marker sets are
        // genuinely empty, exercising the `satisfies_frontmatter_minimum`
        // fallback itself. `ontology.source` alone is not enough to satisfy
        // that fallback's three-field conjunction (no `project.name`, no
        // `templates.dir`), so this must be Unsupported, not Frontmatter.
        let toml = "[ontology]\nsource = \"o.ttl\"\n";
        assert!(matches!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::Unsupported { .. }
        ));
    }

    #[test]
    fn project_present_without_version_is_a_frontmatter_marker_even_without_templates() {
        // This is the mirror case: `[project]` present without `version` is
        // itself a firing frontmatter marker (not merely part of the minimum
        // fallback), so it classifies Frontmatter immediately even though
        // `[templates]` is entirely missing -- exactly the "shape, not full
        // validity" scope boundary the module doc comment documents (a real
        // typed parse would separately reject the missing `templates`
        // field).
        let toml = "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n";
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::Frontmatter
        );
    }

    // -- 8. F2 red-team finding: weak declarative markers must not override
    //       a positive frontmatter match, or be trusted alone -------------

    #[test]
    fn weak_declarative_marker_alone_does_not_override_full_frontmatter_match() {
        // F2 synthetic repro: `[project].version` is present (the
        // classifier's own weak, non-schema-specific declarative marker),
        // but every other structural signal -- `project.name`,
        // `ontology.source`, `templates.dir` -- affirmatively matches the
        // frontmatter schema's three required fields, and there is no
        // `[generation]` table at all. `version` really would be fatal to a
        // real frontmatter parse too (`ggen_engine::config::Project` is
        // `#[serde(deny_unknown_fields)]` with only `name`), so this
        // document would fail BOTH real typed parsers -- `Ambiguous` (not a
        // silent, wrong `DeclarativeRules`) is the honest outcome. Before
        // the fix, `classify_ggen_toml` returned `DeclarativeRules` here.
        let toml = "[project]\nname = \"x\"\nversion = \"1.0.0\"\n\n\
                    [ontology]\nsource = \"o.ttl\"\n\n\
                    [templates]\ndir = \"templates\"\n";
        match classify_ggen_toml(toml) {
            ConfigSchemaClassification::Ambiguous { matched } => {
                assert!(
                    matched.contains(&"declarative:project_version_present".to_string()),
                    "{matched:?}"
                );
                assert!(
                    matched.contains(&"frontmatter:satisfies_minimum_shape".to_string()),
                    "{matched:?}"
                );
            }
            other => panic!("expected Ambiguous, got {other:?}"),
        }
        // Real cross-check: the declarative-rules typed parser really does
        // reject this document too (no `[generation]` at all) -- proving
        // `DeclarativeRules` would have been an actively wrong
        // classification, not merely an unverified one.
        assert!(ManifestParser::parse_str(toml).is_err());
    }

    #[test]
    fn weak_declarative_markers_alone_without_frontmatter_minimum_are_unsupported() {
        // F2's real, pre-existing repository file
        // (`specs/012-grand-unified-kgc-thesis/ggen.toml`) structurally
        // distilled: `[project]` carries both `name` and `version`, and
        // `[sparql]`/`[logging]`/`[security]` (all on
        // `DECLARATIVE_ONLY_TABLES`) are present for unrelated reasons --
        // but there is no `[generation]` table, no `[[packs]]`, and no
        // `[ontology]`/`templates.dir` pair either. Weak markers alone, with
        // neither a strong declarative marker nor the frontmatter minimum
        // satisfied, must not force `DeclarativeRules`.
        let toml = "[project]\nname = \"x\"\nversion = \"1.0.0\"\n\n\
                    [sparql]\ntimeout = 30\n\n\
                    [logging]\nlevel = \"info\"\n\n\
                    [security]\nvalidate_paths = true\n";
        assert!(
            matches!(
                classify_ggen_toml(toml),
                ConfigSchemaClassification::Unsupported { .. }
            ),
            "{:?}",
            classify_ggen_toml(toml)
        );
    }

    #[test]
    fn real_repo_file_with_no_generation_table_and_coincidental_declarative_table_names_is_not_declarative_rules(
    ) {
        // Grounded directly in the real, pre-existing repository file cited
        // by the F2 finding (not a synthetic stand-in): `[project]` carries
        // both `name` and `version`, `[templates]` uses
        // `source_dir`/`output_dir` (not `dir`), and
        // `[sparql]`/`[logging]`/`[security]` are present -- all three are
        // on `DECLARATIVE_ONLY_TABLES` -- but there is no `[generation]`
        // table and no `[ontology]` table at all. Before this fix, the bare
        // `project.version` marker plus these coincidental table names alone
        // forced `DeclarativeRules`, and the real typed parser then failed
        // with a confusing "unknown field `author`" error that never
        // mentioned the actual problem (no `[generation]`/`[ontology]`).
        let raw = include_str!("../../../specs/012-grand-unified-kgc-thesis/ggen.toml");
        let got = classify_ggen_toml(raw);
        assert!(
            matches!(got, ConfigSchemaClassification::Unsupported { .. }),
            "expected Unsupported, got {got:?}"
        );
        // Real cross-check: the declarative-rules typed parser really does
        // reject this document (proving `DeclarativeRules` would have been
        // an actively wrong classification).
        assert!(ManifestParser::parse_str(raw).is_err());
    }

    #[test]
    fn generation_table_present_still_wins_even_when_frontmatter_minimum_also_matches() {
        // Regression guard against overcorrection: a genuinely
        // declarative-rules project's `[generation]` table is a
        // schema-specific, decisive (strong) marker -- `[templates].dir`
        // also happening to be present (`GgenManifest`'s `TemplatesConfig`
        // tolerates a stray `dir` key, per the module doc) must not
        // downgrade this to `Ambiguous`.
        let toml = "[project]\nname = \"x\"\nversion = \"1.0.0\"\n\n\
                    [ontology]\nsource = \"o.ttl\"\n\n\
                    [templates]\ndir = \"templates\"\n\n\
                    [generation]\nrules = []\n";
        assert_eq!(
            classify_ggen_toml(toml),
            ConfigSchemaClassification::DeclarativeRules
        );
    }
}
