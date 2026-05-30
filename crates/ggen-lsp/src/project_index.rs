//! Project index: discovers a `ggen.toml` manifest under a project root and
//! builds a [`RuleIndexEntry`] for every `[[generation.rules]]` entry.
//!
//! GGEN-TPL-001 — Living LSP. This is the entry point the rest of the LSP uses
//! to obtain a resolved, queryable view of a project's generation rules.
//! Manifest parsing is delegated to `ggen_core::manifest` (no hand-rolled TOML).

use std::path::{Path, PathBuf};

use ggen_core::manifest::ManifestParser;

use crate::rule_index::RuleIndexEntry;

/// Structured errors for project-index construction.
///
/// Index-level *rule* problems (missing query/template files, unsupported
/// sources) are non-fatal and recorded in [`RuleIndexEntry::issues`]. The
/// errors here are the ones that prevent building an index at all.
#[derive(Debug)]
pub enum IndexError {
    /// No `ggen.toml` was found at `<root>/ggen.toml`.
    ManifestNotFound {
        /// The path that was probed.
        path: PathBuf,
    },
    /// The `ggen.toml` existed but could not be parsed by `ggen_core::manifest`.
    ManifestParse {
        /// The manifest path that failed to parse.
        path: PathBuf,
        /// Human-readable parse error message.
        message: String,
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
    /// Looks for `<root>/ggen.toml`, parses it via `ggen_core::manifest`, and
    /// produces a [`RuleIndexEntry`] for each generation rule. If the manifest
    /// has no `[generation]` section (or no rules), the index is built with an
    /// empty `rule_entries` vector.
    ///
    /// # Errors
    /// - [`IndexError::ManifestNotFound`] if `<root>/ggen.toml` does not exist.
    /// - [`IndexError::ManifestParse`] if the manifest cannot be parsed.
    pub fn from_root(root: &Path) -> Result<ProjectIndex, IndexError> {
        let manifest_path = root.join("ggen.toml");
        if !manifest_path.is_file() {
            return Err(IndexError::ManifestNotFound {
                path: manifest_path,
            });
        }

        let manifest =
            ManifestParser::parse(&manifest_path).map_err(|err| IndexError::ManifestParse {
                path: manifest_path.clone(),
                message: err.to_string(),
            })?;

        // `generation` is a required, non-optional field on `GgenManifest`.
        let rule_entries = manifest
            .generation
            .rules
            .iter()
            .map(|rule| RuleIndexEntry::from_rule(rule, &manifest_path))
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
}
