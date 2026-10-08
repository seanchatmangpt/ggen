//! E0015 lint: a generation-rule SELECT containing GROUP_CONCAT must also pin
//! an explicit `separator`, otherwise the fold is not a deterministic law.
//! Mirrors the ENGINE-side E0013 (missing ORDER BY) shape: refused in strict
//! mode, warn-only otherwise. Covers only the Inline query variant (the same
//! purity split as E0013).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_config::manifest::{ManifestParser, ManifestValidator};
use std::path::Path;

fn manifest_toml(query: &str, strict: bool) -> String {
    format!(
        r#"
[project]
name = "e0015-lint"
version = "1.0.0"

[ontology]
source = "."

[validation]
strict_mode = {strict}

[[generation.rules]]
name = "fold-rule"
query = {{ inline = "{query}" }}
template = {{ inline = "{{% for row in rows %}}{{{{ row.n }}}}{{% endfor %}}" }}
output_file = "out.txt"
"#
    )
}

fn validate(toml: &str) -> Result<(), ggen_config::ConfigError> {
    let manifest = ManifestParser::parse_str(toml).expect("parse");
    ManifestValidator::new(&manifest, Path::new(".")).validate()
}

#[test]
fn group_concat_without_separator_refused_in_strict_mode() {
    let query =
        "SELECT (GROUP_CONCAT(?x) AS ?all) (COUNT(?s) AS ?n) WHERE { ?s ?p ?x } ORDER BY ?n";
    let err = validate(&manifest_toml(query, true)).expect_err("must refuse");
    let msg = match &err {
        ggen_config::ConfigError::Validation(m) => m.clone(),
        other => panic!("expected ConfigError::Validation, got {other:?}"),
    };
    assert!(msg.contains("E0015"), "expected E0015 in: {msg}");
    assert!(msg.contains("GROUP_CONCAT"), "expected GROUP_CONCAT in: {msg}");
    assert!(msg.contains("separator"), "expected separator in: {msg}");
}

#[test]
fn group_concat_with_separator_passes() {
    let query = "SELECT (GROUP_CONCAT(?x; separator=', ') AS ?all) (COUNT(?s) AS ?n) WHERE { ?s ?p ?x } ORDER BY ?n";
    validate(&manifest_toml(query, true)).expect("explicit separator must pass");
}

#[test]
fn group_concat_without_separator_warns_only_in_non_strict_mode() {
    let query =
        "SELECT (GROUP_CONCAT(?x) AS ?all) (COUNT(?s) AS ?n) WHERE { ?s ?p ?x } ORDER BY ?n";
    validate(&manifest_toml(query, false)).expect("non-strict mode must not refuse");
}

#[test]
fn separator_detection_is_case_insensitive_and_ignores_spacing() {
    use ggen_config::manifest::validation::{query_has_group_concat, query_has_separator};
    assert!(query_has_group_concat("select group_concat(?x) as ?a where {}"));
    assert!(!query_has_group_concat("select count(?x) where {}"));
    assert!(query_has_separator("GROUP_CONCAT(?x; SEPARATOR='')"));
    assert!(query_has_separator("GROUP_CONCAT(?x;separator = ', ')"));
    assert!(!query_has_separator("GROUP_CONCAT(?x)"));
}
