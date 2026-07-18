//! Project index: discovers a `ggen.toml` manifest under a project root and
//! builds a [`RuleIndexEntry`] for every `[[generation.rules]]` entry.
//!
//! GGEN-TPL-001 — Living LSP. This is the entry point the rest of the LSP uses
//! to obtain a resolved, queryable view of a project's generation rules.
//! Manifest parsing is delegated to `ggen_config::manifest` (no hand-rolled TOML).
//!
//! # Schema dispatch (specs/014-ggen-core-replacement, correction 2 / Blocker A part 2)
//!
//! `ggen.toml` recognizes two independently-defined schemas (see
//! `ggen_config::config_schema`'s own module doc comment for the full
//! design): the declarative-rules schema (`ggen_config::manifest::
//! GgenManifest`, `[[generation.rules]]`) this index covers, and a
//! frontmatter-per-template-file schema this index has nothing to say
//! about. Before this repointing, [`ProjectIndex::from_root_with_overlay`]
//! hardcoded [`ManifestParser::parse`] unconditionally, so a real,
//! syntactically-valid frontmatter-schema project was rejected with the
//! *same* [`IndexError::ManifestParse`] a genuinely broken manifest would
//! produce — every one of this crate's ~11 best-effort call sites (`let
//! Ok(project) = ProjectIndex::from_root(root) else { return Vec::new() }`)
//! could not tell "this project legitimately uses the other schema" apart
//! from "this manifest has a typo." Now the shared
//! [`ggen_config::classify_ggen_toml`] classifier runs first: a
//! `Frontmatter` classification returns an explicit, honest *empty* index
//! (`Ok` with zero rule entries — this project's ggen.toml is valid, it is
//! simply out of this index's scope), and `Ambiguous`/`Unsupported`
//! classifications return their own typed [`IndexError`] variants rather
//! than being folded into `ManifestParse`. Only a `DeclarativeRules`
//! classification proceeds to the real [`ManifestParser::parse_str`] call
//! (a `Malformed` classification -- not syntactically valid TOML at all --
//! still surfaces as `ManifestParse`, unchanged: that outcome always meant
//! "could not even parse as TOML").

use std::path::{Path, PathBuf};

use ggen_config::{manifest::ManifestParser, ConfigSchemaClassification};

use crate::rule_index::RuleIndexEntry;

/// Open-buffer overlay: disk path → in-buffer (possibly unsaved) content.
///
/// LIVE-BUFFER-001. The cross-surface index reads a rule's query/template either
/// from this overlay (when the file is open in the editor) or, on a miss, from
/// disk. An **empty** overlay makes every read fall back to `std::fs::read_to_string`
/// — byte-identical to the disk-only path, so existing disk-fixture tests are
/// unaffected. Keys are the same `manifest_dir.join(file)` / `root.join(...)` paths
/// the index already computes (no canonicalization), so a buffer keyed by
/// `Url::to_file_path()` matches the index's resolved read path on the same OS.
pub type BufferOverlay = std::collections::HashMap<PathBuf, String>;

/// Structured errors for project-index construction.
///
/// Index-level *rule* problems (missing query/template files, unsupported
/// sources) are non-fatal and recorded in [`RuleIndexEntry::issues`]. The
/// errors here are the ones that prevent building an index at all. Note
/// that a `ggen.toml` classified as `Frontmatter` (the other, non-rules
/// schema) is *not* an error at all -- see [`ProjectIndex::
/// from_root_with_overlay`]'s doc comment -- it produces an explicit empty
/// [`ProjectIndex`] instead.
#[derive(Debug)]
pub enum IndexError {
    /// No `ggen.toml` was found at `<root>/ggen.toml`.
    ManifestNotFound {
        /// The path that was probed.
        path: PathBuf,
    },
    /// The `ggen.toml` either failed to parse as TOML at all
    /// (`ggen_config::classify_ggen_toml`'s `Malformed` outcome) or
    /// classified as the declarative-rules schema but still failed
    /// `ManifestParser::parse_str` (e.g. a missing required field).
    ManifestParse {
        /// The manifest path that failed to parse.
        path: PathBuf,
        /// Human-readable parse error message.
        message: String,
    },
    /// `ggen_config::classify_ggen_toml` found structural markers for more
    /// than one recognized schema at once -- distinct from `ManifestParse`:
    /// the document parses fine as TOML, it is just genuinely ambiguous
    /// which schema it belongs to.
    AmbiguousSchema {
        /// The manifest path.
        path: PathBuf,
        /// Every conflicting structural marker the classifier observed
        /// (see [`ggen_config::ConfigSchemaClassification::Ambiguous`]).
        matched: Vec<String>,
    },
    /// `ggen_config::classify_ggen_toml` found no recognized schema's
    /// structural markers at all -- distinct from `ManifestParse`: the
    /// document parses fine as TOML, it simply does not resemble either
    /// schema this workspace recognizes.
    UnsupportedSchema {
        /// The manifest path.
        path: PathBuf,
        /// The document's top-level tables, as a diagnostic breadcrumb
        /// (see [`ggen_config::ConfigSchemaClassification::Unsupported`]).
        observed_markers: Vec<String>,
    },
}

impl std::fmt::Display for IndexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexError::ManifestNotFound { path } => {
                write!(f, "ggen.toml not found at {}", path.display())
            }
            IndexError::ManifestParse { path, message } => {
                write!(f, "failed to parse {}: {}", path.display(), message)
            }
            IndexError::AmbiguousSchema { path, matched } => {
                write!(
                    f,
                    "[{}] {} is ambiguous between the declarative-rules and frontmatter \
                     schemas: conflicting markers {matched:?}",
                    ggen_config::CONFIG_SCHEMA_AMBIGUOUS,
                    path.display()
                )
            }
            IndexError::UnsupportedSchema {
                path,
                observed_markers,
            } => {
                write!(
                    f,
                    "[{}] {} matches neither the declarative-rules nor the frontmatter \
                     schema: observed top-level tables {observed_markers:?}",
                    ggen_config::CONFIG_SCHEMA_UNSUPPORTED,
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for IndexError {}

/// A resolved index of every generation rule in a project's `ggen.toml`.
#[derive(Debug, Clone)]
pub struct ProjectIndex {
    /// Project root the index was built from.
    pub root: PathBuf,
    /// One entry per `[[generation.rules]]` rule, in manifest order.
    pub rule_entries: Vec<RuleIndexEntry>,
}

impl ProjectIndex {
    /// Build a [`ProjectIndex`] from a project root directory.
    ///
    /// Looks for `<root>/ggen.toml`, parses it via `ggen_config::manifest`, and
    /// produces a [`RuleIndexEntry`] for each generation rule. If the manifest
    /// has no `[generation]` section (or no rules), the index is built with an
    /// empty `rule_entries` vector.
    ///
    /// # Errors
    /// - [`IndexError::ManifestNotFound`] if `<root>/ggen.toml` does not exist.
    /// - [`IndexError::ManifestParse`] if the manifest cannot be parsed.
    pub fn from_root(root: &Path) -> Result<ProjectIndex, IndexError> {
        Self::from_root_with_overlay(root, &BufferOverlay::new())
    }

    /// Build a [`ProjectIndex`] from a project root, consulting an open-buffer
    /// overlay before disk for each rule's query/template file (LIVE-BUFFER-001).
    ///
    /// A file present in `overlay` (keyed by its resolved on-disk path) is read
    /// from the buffer; any file absent from the overlay falls back to disk. With
    /// an **empty** overlay this is byte-identical to [`Self::from_root`] — the
    /// disk path is preserved exactly (`from_root` is now a thin empty-overlay
    /// delegate).
    ///
    /// The `ggen.toml` manifest itself is still read from disk (its existence and
    /// rule-file resolution anchor on the on-disk manifest directory); only the
    /// rule **query/template** reads are overlay-aware, which is exactly the
    /// producer-surface gap the living loop closes.
    ///
    /// Dispatches through the shared [`ggen_config::classify_ggen_toml`]
    /// classifier before attempting any typed parse (see this module's own
    /// doc comment for the full design): a `Frontmatter`-classified
    /// manifest returns an explicit *empty* index (`Ok`, zero rule
    /// entries), never [`IndexError::ManifestParse`].
    ///
    /// # Errors
    /// - [`IndexError::ManifestNotFound`] if `<root>/ggen.toml` does not exist.
    /// - [`IndexError::ManifestParse`] if the document is not syntactically
    ///   valid TOML, or classifies as declarative-rules but still fails
    ///   [`ManifestParser::parse_str`] (e.g. a missing required field).
    /// - [`IndexError::AmbiguousSchema`] / [`IndexError::UnsupportedSchema`]
    ///   for the classifier's own `Ambiguous`/`Unsupported` outcomes.
    pub fn from_root_with_overlay(
        root: &Path, overlay: &BufferOverlay,
    ) -> Result<ProjectIndex, IndexError> {
        let manifest_path = root.join("ggen.toml");
        if !manifest_path.is_file() {
            return Err(IndexError::ManifestNotFound {
                path: manifest_path,
            });
        }

        let raw =
            std::fs::read_to_string(&manifest_path).map_err(|e| IndexError::ManifestParse {
                path: manifest_path.clone(),
                message: e.to_string(),
            })?;

        let manifest = match ggen_config::classify_ggen_toml(&raw) {
            ConfigSchemaClassification::DeclarativeRules => ManifestParser::parse_str(&raw)
                .map_err(|err| IndexError::ManifestParse {
                    path: manifest_path.clone(),
                    message: err.to_string(),
                })?,
            ConfigSchemaClassification::Frontmatter => {
                // Not this index's schema, but a perfectly valid project --
                // an explicit empty index, not an error (see module doc
                // comment).
                return Ok(ProjectIndex {
                    root: root.to_path_buf(),
                    rule_entries: Vec::new(),
                });
            }
            ConfigSchemaClassification::Ambiguous { matched } => {
                return Err(IndexError::AmbiguousSchema {
                    path: manifest_path,
                    matched,
                });
            }
            ConfigSchemaClassification::Unsupported { observed_markers } => {
                return Err(IndexError::UnsupportedSchema {
                    path: manifest_path,
                    observed_markers,
                });
            }
            ConfigSchemaClassification::Malformed { diagnostic } => {
                return Err(IndexError::ManifestParse {
                    path: manifest_path,
                    message: diagnostic,
                });
            }
        };

        // `generation` is a required, non-optional field on `GgenManifest`.
        let rule_entries = manifest
            .generation
            .rules
            .iter()
            .map(|rule| RuleIndexEntry::from_rule_with_overlay(rule, &manifest_path, overlay))
            .collect();

        Ok(ProjectIndex {
            root: root.to_path_buf(),
            rule_entries,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;
    use tempfile::TempDir;

    fn vars(items: &[&str]) -> BTreeSet<String> {
        items.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn missing_manifest_returns_not_found_error() {
        // Arrange — empty project root, no ggen.toml.
        let dir = TempDir::new().expect("create temp dir");

        // Act
        let result = ProjectIndex::from_root(dir.path());

        // Assert
        match result {
            Err(IndexError::ManifestNotFound { path }) => {
                assert!(path.ends_with("ggen.toml"));
            }
            other => panic!("expected ManifestNotFound, got {:?}", other),
        }
    }

    #[test]
    fn project_with_inline_rule_builds_index() {
        // Arrange — write a real ggen.toml with one inline-query rule.
        let dir = TempDir::new().expect("create temp dir");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name ?email WHERE { ?p :name ?name ; :email ?email }" }
template = { inline = "{{ name }} <{{ email }}>" }
"#;
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        // Act
        let index = ProjectIndex::from_root(dir.path()).expect("build index");

        // Assert — one entry, vars extracted, no issues.
        assert_eq!(index.rule_entries.len(), 1);
        let entry = &index.rule_entries[0];
        assert_eq!(entry.rule_id, "people");
        assert_eq!(entry.output_file, "people.rs");
        assert!(entry.query_inline);
        assert_eq!(entry.selected_vars, vars(&["name", "email"]));
        assert_eq!(
            entry.template_content.as_deref(),
            Some("{{ name }} <{{ email }}>")
        );
        assert!(
            entry.issues.is_empty(),
            "unexpected issues: {:?}",
            entry.issues
        );
    }

    #[test]
    fn project_with_query_file_rule_reads_file() {
        // Arrange — manifest references an external .rq file that exists.
        let dir = TempDir::new().expect("create temp dir");
        std::fs::write(
            dir.path().join("q.rq"),
            "SELECT ?title WHERE { ?b :title ?title }",
        )
        .expect("write query file");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "books"
output_file = "books.rs"
query = { file = "q.rq" }
template = { inline = "{{ title }}" }
"#;
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        // Act
        let index = ProjectIndex::from_root(dir.path()).expect("build index");

        // Assert
        assert_eq!(index.rule_entries.len(), 1);
        let entry = &index.rule_entries[0];
        assert!(!entry.query_inline);
        assert_eq!(entry.selected_vars, vars(&["title"]));
        assert!(
            entry.issues.is_empty(),
            "unexpected issues: {:?}",
            entry.issues
        );
    }

    #[test]
    fn manifest_with_no_rules_yields_empty_entries() {
        // Arrange — valid manifest with an empty generation rule set.
        let dir = TempDir::new().expect("create temp dir");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[generation]
rules = []
"#;
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        // Act
        let index = ProjectIndex::from_root(dir.path()).expect("build index");

        // Assert
        assert!(index.rule_entries.is_empty());
        assert_eq!(index.root, dir.path());
    }

    #[test]
    fn rule_with_missing_template_file_surfaces_issue_not_error() {
        // Arrange — manifest is valid; template file is absent.
        let dir = TempDir::new().expect("create temp dir");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "broken"
output_file = "broken.rs"
query = { inline = "SELECT ?x WHERE { ?x a ?t }" }
template = { file = "nope.tera" }
"#;
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        // Act — building the index still succeeds.
        let index = ProjectIndex::from_root(dir.path()).expect("build index");

        // Assert — the problem is recorded per-rule, not as an IndexError.
        assert_eq!(index.rule_entries.len(), 1);
        let entry = &index.rule_entries[0];
        assert!(entry.template_content.is_none());
        assert!(
            entry
                .issues
                .iter()
                .any(|i| i.starts_with("template file missing:")),
            "expected template-missing issue, got: {:?}",
            entry.issues
        );
    }

    #[test]
    fn frontmatter_schema_project_yields_explicit_empty_index_not_a_parse_error() {
        // Arrange — a real, valid frontmatter-schema ggen.toml (no
        // [[generation.rules]] at all): before the classifier repointing
        // this hit ManifestParser::parse's deny_unknown_fields rejection
        // and surfaced as IndexError::ManifestParse, indistinguishable
        // from a genuinely broken manifest.
        let dir = TempDir::new().expect("create temp dir");
        let manifest = r#"
[project]
name = "demo"

[ontology]
source = "model.ttl"

[templates]
dir = "templates"
"#;
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        // Act
        let index = ProjectIndex::from_root(dir.path())
            .expect("a frontmatter-schema project must build an index, not error");

        // Assert — explicit empty, not an error.
        assert!(index.rule_entries.is_empty());
        assert_eq!(index.root, dir.path());
    }

    #[test]
    fn ambiguous_schema_project_yields_typed_ambiguous_error() {
        let dir = TempDir::new().expect("create temp dir");
        // Frontmatter-shaped ([project] without version) but also carries
        // [ai], a GgenManifest-only table.
        let manifest = "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[templates]\ndir = \"t\"\n\n[ai]\nprovider = \"openai\"\n";
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        let err = ProjectIndex::from_root(dir.path()).expect_err("must be ambiguous");
        match &err {
            IndexError::AmbiguousSchema { matched, .. } => {
                assert!(!matched.is_empty(), "{matched:?}");
            }
            other => panic!("expected AmbiguousSchema, got {other:?}"),
        }
        assert!(err_contains_code(
            &err,
            ggen_config::CONFIG_SCHEMA_AMBIGUOUS
        ));
    }

    #[test]
    fn unsupported_schema_project_yields_typed_unsupported_error() {
        let dir = TempDir::new().expect("create temp dir");
        let manifest = "[some_other_tool]\nkey = \"value\"\n";
        std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");

        let err = ProjectIndex::from_root(dir.path()).expect_err("must be unsupported");
        match &err {
            IndexError::UnsupportedSchema {
                observed_markers, ..
            } => {
                assert!(
                    observed_markers
                        .iter()
                        .any(|m| m.contains("some_other_tool")),
                    "{observed_markers:?}"
                );
            }
            other => panic!("expected UnsupportedSchema, got {other:?}"),
        }
        assert!(err_contains_code(
            &err,
            ggen_config::CONFIG_SCHEMA_UNSUPPORTED
        ));
    }

    #[test]
    fn malformed_toml_still_yields_manifest_parse_error() {
        let dir = TempDir::new().expect("create temp dir");
        std::fs::write(dir.path().join("ggen.toml"), "not [ valid toml").expect("write manifest");

        let err = ProjectIndex::from_root(dir.path()).expect_err("must fail");
        match err {
            IndexError::ManifestParse { .. } => {}
            other => panic!("expected ManifestParse, got {other:?}"),
        }
    }

    fn err_contains_code(err: &IndexError, code: &str) -> bool {
        err.to_string().contains(code)
    }
}
