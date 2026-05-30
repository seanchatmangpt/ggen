//! Rule index: a flattened, resolved view of a single `[[generation.rules]]`
//! entry from a `ggen.toml` manifest.
//!
//! GGEN-TPL-001 — Living LSP. This module turns a parsed `GenerationRule`
//! (from `ggen_core::manifest`) into a [`RuleIndexEntry`]: query/template text
//! is resolved (inline kept as-is, file references read from disk relative to
//! the manifest directory), and the SPARQL `SELECT` variables are extracted so
//! downstream analyzers can reason about which Tera variables are available.
//!
//! Index-level problems (missing files, unsupported template sources, `SELECT *`)
//! are recorded in [`RuleIndexEntry::issues`] rather than raised as hard errors,
//! so a single broken rule never sinks the whole project index.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use ggen_core::manifest::{GenerationRule, QuerySource, TemplateSource};

/// A fully-resolved view of one generation rule.
///
/// Built by [`RuleIndexEntry::from_rule`]. All file references are resolved
/// relative to the directory containing the `ggen.toml` manifest.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleIndexEntry {
    /// Rule identifier (`rule.name`).
    pub rule_id: String,
    /// Absolute (or as-supplied) path to the `ggen.toml` this rule came from.
    pub manifest_path: PathBuf,
    /// `true` when the SPARQL query was supplied inline in the manifest.
    pub query_inline: bool,
    /// Resolved SPARQL query text. Empty when a referenced query file is missing
    /// (the problem is then recorded in [`issues`](Self::issues)).
    pub query_content: String,
    /// Path to the template file, if the template source is a local file.
    /// `None` for inline templates and for unsupported (Git/Package) sources.
    pub template_path: Option<PathBuf>,
    /// Resolved Tera template text. `None` when the template file is missing or
    /// the source is unsupported for the MVP.
    pub template_content: Option<String>,
    /// Output file path for the rule (`rule.output_file`).
    pub output_file: String,
    /// SPARQL `SELECT` variable names, WITHOUT the leading `?`.
    pub selected_vars: BTreeSet<String>,
    /// Index-level problems encountered while resolving this rule
    /// (e.g. `"template file missing: X"`). Non-fatal.
    pub issues: Vec<String>,
}

impl RuleIndexEntry {
    /// Build a [`RuleIndexEntry`] from a parsed [`GenerationRule`].
    ///
    /// `manifest_path` is the path to the `ggen.toml` file the rule came from;
    /// its parent directory is used to resolve relative query/template file
    /// references. This never fails: filesystem and parsing problems are
    /// collected into [`RuleIndexEntry::issues`].
    pub fn from_rule(rule: &GenerationRule, manifest_path: &Path) -> RuleIndexEntry {
        let manifest_dir: PathBuf = manifest_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));

        let mut issues: Vec<String> = Vec::new();

        // --- Resolve the SPARQL query -------------------------------------
        let (query_inline, query_content) = match &rule.query {
            QuerySource::Inline { inline } => (true, inline.clone()),
            QuerySource::File { file } => {
                let resolved = manifest_dir.join(file);
                match std::fs::read_to_string(&resolved) {
                    Ok(text) => (false, text),
                    Err(err) => {
                        issues.push(format!(
                            "query file missing: {} ({})",
                            resolved.display(),
                            err
                        ));
                        (false, String::new())
                    }
                }
            }
        };

        // --- Resolve the template ----------------------------------------
        let (template_path, template_content) = match &rule.template {
            TemplateSource::Inline { inline } => (None, Some(inline.clone())),
            TemplateSource::File { file } => {
                let resolved = manifest_dir.join(file);
                match std::fs::read_to_string(&resolved) {
                    Ok(text) => (Some(resolved), Some(text)),
                    Err(_) => {
                        issues.push(format!("template file missing: {}", resolved.display()));
                        (Some(resolved), None)
                    }
                }
            }
            TemplateSource::Git { git, path, .. } => {
                issues.push(format!(
                    "unsupported template source for MVP: git ({}#{})",
                    git,
                    path.display()
                ));
                (None, None)
            }
            TemplateSource::Package { package, path, .. } => {
                issues.push(format!(
                    "unsupported template source for MVP: package ({}:{})",
                    package,
                    path.display()
                ));
                (None, None)
            }
        };

        // --- Extract SELECT variables ------------------------------------
        let selected_vars = extract_select_vars(&query_content, &mut issues);

        RuleIndexEntry {
            rule_id: rule.name.clone(),
            manifest_path: manifest_path.to_path_buf(),
            query_inline,
            query_content,
            template_path,
            template_content,
            output_file: rule.output_file.clone(),
            selected_vars,
            issues,
        }
    }
}

/// Minimal SPARQL `SELECT` variable extractor.
///
/// NOTE (Phase-1.5 consolidation): this intentionally duplicates the private
/// `extract_sparql_vars` logic in `crate::analyzers`. The analyzer version is
/// not `pub`, so we keep a small local copy here. When the analyzer surface is
/// stabilized this should be unified into a single shared helper.
///
/// Algorithm:
/// 1. Find `SELECT` (case-insensitive).
/// 2. Take the text from after `SELECT` up to the next `WHERE` (case-insensitive);
///    if no `WHERE`, use the rest of the string.
/// 3. Collect whitespace/paren-delimited tokens starting with `?`, strip the `?`,
///    and insert into a [`BTreeSet`].
/// 4. `SELECT DISTINCT` is handled naturally (`DISTINCT` is not a `?`-token).
/// 5. `SELECT *` yields an empty set and pushes an info issue.
fn extract_select_vars(query: &str, issues: &mut Vec<String>) -> BTreeSet<String> {
    let mut vars = BTreeSet::new();

    let lower = query.to_ascii_lowercase();
    let select_pos = match lower.find("select") {
        Some(pos) => pos,
        None => return vars,
    };

    // Slice starting right after the literal "select".
    let after_select = &query[select_pos + "select".len()..];
    let after_select_lower = &lower[select_pos + "select".len()..];

    // Bound the projection by the first WHERE (if present).
    let projection = match after_select_lower.find("where") {
        Some(where_pos) => &after_select[..where_pos],
        None => after_select,
    };

    let mut saw_star = false;
    for raw in projection.split(|c: char| c.is_whitespace() || c == '(' || c == ')') {
        let token = raw.trim();
        if token.is_empty() {
            continue;
        }
        if token == "*" {
            saw_star = true;
            continue;
        }
        if let Some(name) = token.strip_prefix('?') {
            // Guard against bindings like `?x` followed by punctuation; keep the
            // leading identifier-ish run only (SPARQL var chars).
            let cleaned: String = name
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if !cleaned.is_empty() {
                vars.insert(cleaned);
            }
        }
    }

    if vars.is_empty() && saw_star {
        issues.push("SELECT * is not introspectable: no explicit projection variables".to_string());
    }

    vars
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_core::manifest::{GenerationRule, QuerySource, TemplateSource};
    use std::collections::BTreeSet;
    use tempfile::TempDir;

    fn rule_with(query: QuerySource, template: TemplateSource) -> GenerationRule {
        GenerationRule {
            name: "test_rule".to_string(),
            query,
            template,
            output_file: "out.rs".to_string(),
            skip_empty: false,
            mode: ggen_core::manifest::GenerationMode::Create,
            when: None,
        }
    }

    fn vars(items: &[&str]) -> BTreeSet<String> {
        items.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn inline_query_extracts_selected_vars() {
        // Arrange — inline query, inline template, no filesystem.
        let dir = TempDir::new().expect("create temp dir");
        let manifest_path = dir.path().join("ggen.toml");
        let rule = rule_with(
            QuerySource::Inline {
                inline: "SELECT ?name ?title WHERE { ?s ?p ?o }".to_string(),
            },
            TemplateSource::Inline {
                inline: "{{ name }} - {{ title }}".to_string(),
            },
        );

        // Act
        let entry = RuleIndexEntry::from_rule(&rule, &manifest_path);

        // Assert — observable state, no issues.
        assert!(entry.query_inline);
        assert_eq!(
            entry.query_content,
            "SELECT ?name ?title WHERE { ?s ?p ?o }"
        );
        assert_eq!(entry.selected_vars, vars(&["name", "title"]));
        assert_eq!(
            entry.template_content.as_deref(),
            Some("{{ name }} - {{ title }}")
        );
        assert!(entry.template_path.is_none());
        assert!(
            entry.issues.is_empty(),
            "unexpected issues: {:?}",
            entry.issues
        );
    }

    #[test]
    fn query_file_is_read_and_vars_extracted() {
        // Arrange — write a real .rq file next to the manifest.
        let dir = TempDir::new().expect("create temp dir");
        let manifest_path = dir.path().join("ggen.toml");
        let rq_path = dir.path().join("query.rq");
        std::fs::write(
            &rq_path,
            "SELECT DISTINCT ?id ?label WHERE { ?id rdfs:label ?label }",
        )
        .expect("write query file");

        let rule = rule_with(
            QuerySource::File {
                file: std::path::PathBuf::from("query.rq"),
            },
            TemplateSource::Inline {
                inline: "{{ id }}".to_string(),
            },
        );

        // Act
        let entry = RuleIndexEntry::from_rule(&rule, &manifest_path);

        // Assert — file content resolved, DISTINCT handled, vars correct.
        assert!(!entry.query_inline);
        assert!(entry.query_content.contains("SELECT DISTINCT"));
        assert_eq!(entry.selected_vars, vars(&["id", "label"]));
        assert!(
            entry.issues.is_empty(),
            "unexpected issues: {:?}",
            entry.issues
        );
    }

    #[test]
    fn missing_template_file_is_reported_as_issue() {
        // Arrange — template points at a file that does not exist.
        let dir = TempDir::new().expect("create temp dir");
        let manifest_path = dir.path().join("ggen.toml");
        let rule = rule_with(
            QuerySource::Inline {
                inline: "SELECT ?x WHERE { ?x a ?t }".to_string(),
            },
            TemplateSource::File {
                file: std::path::PathBuf::from("does_not_exist.tera"),
            },
        );

        // Act
        let entry = RuleIndexEntry::from_rule(&rule, &manifest_path);

        // Assert — recorded in issues, not a panic; content is None but path kept.
        assert!(entry.template_content.is_none());
        assert!(entry.template_path.is_some());
        assert!(
            entry
                .issues
                .iter()
                .any(|i| i.starts_with("template file missing:")),
            "expected a 'template file missing' issue, got: {:?}",
            entry.issues
        );
    }

    #[test]
    fn missing_query_file_yields_empty_content_and_issue() {
        // Arrange
        let dir = TempDir::new().expect("create temp dir");
        let manifest_path = dir.path().join("ggen.toml");
        let rule = rule_with(
            QuerySource::File {
                file: std::path::PathBuf::from("absent.rq"),
            },
            TemplateSource::Inline {
                inline: "{{ x }}".to_string(),
            },
        );

        // Act
        let entry = RuleIndexEntry::from_rule(&rule, &manifest_path);

        // Assert
        assert!(entry.query_content.is_empty());
        assert!(entry.selected_vars.is_empty());
        assert!(
            entry
                .issues
                .iter()
                .any(|i| i.starts_with("query file missing:")),
            "expected a 'query file missing' issue, got: {:?}",
            entry.issues
        );
    }

    #[test]
    fn select_star_pushes_info_issue_and_empty_vars() {
        // Arrange
        let mut issues = Vec::new();

        // Act
        let result = extract_select_vars("SELECT * WHERE { ?s ?p ?o }", &mut issues);

        // Assert
        assert!(result.is_empty());
        assert!(
            issues.iter().any(|i| i.contains("SELECT *")),
            "expected SELECT * info issue, got: {:?}",
            issues
        );
    }

    #[test]
    fn lowercase_select_is_handled() {
        // Arrange + Act
        let mut issues = Vec::new();
        let result = extract_select_vars("select ?a ?b where { ?a ?p ?b }", &mut issues);

        // Assert — case-insensitive SELECT/WHERE.
        assert_eq!(result, vars(&["a", "b"]));
        assert!(issues.is_empty());
    }

    #[test]
    fn unsupported_git_template_source_records_issue() {
        // Arrange
        let dir = TempDir::new().expect("create temp dir");
        let manifest_path = dir.path().join("ggen.toml");
        let rule = rule_with(
            QuerySource::Inline {
                inline: "SELECT ?n WHERE { ?n a ?t }".to_string(),
            },
            TemplateSource::Git {
                git: "https://example.com/repo.git".to_string(),
                branch: None,
                path: std::path::PathBuf::from("tpl/foo.tera"),
            },
        );

        // Act
        let entry = RuleIndexEntry::from_rule(&rule, &manifest_path);

        // Assert
        assert!(entry.template_path.is_none());
        assert!(entry.template_content.is_none());
        assert!(
            entry
                .issues
                .iter()
                .any(|i| i.contains("unsupported template source")),
            "expected unsupported-source issue, got: {:?}",
            entry.issues
        );
    }
}
