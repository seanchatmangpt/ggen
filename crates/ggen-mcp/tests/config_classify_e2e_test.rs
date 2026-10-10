//! Chicago TDD for `ggen_config_classify` (the MCP surface of
//! `ggen_config::classify_ggen_toml`): real `TempDir` + real `ggen.toml`
//! bytes on disk, called in-process. No mocks.
//!
//! Complements `introspection_tools_test.rs` (which asserts only schema
//! strings and read-only behavior) with the full typed contract: outcome
//! codes (`CONFIG_SCHEMA_*` / `CONFIG_PARSE_FAILED`), matched-marker
//! surfacing for ambiguous/unsupported verdicts, the malformed-diagnostic
//! path, and the not-found error path.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::config_classify::{config_classify, ConfigClassifyParams};

use ggen_config::{
    CONFIG_PARSE_FAILED, CONFIG_SCHEMA_AMBIGUOUS, CONFIG_SCHEMA_SUPPORTED,
    CONFIG_SCHEMA_UNSUPPORTED,
};

fn classify(root: &std::path::Path) -> ggen_mcp::tools::config_classify::ConfigClassifyResult {
    config_classify(&ConfigClassifyParams {
        root: root.display().to_string(),
    })
    .expect("classify should succeed whenever a readable ggen.toml exists")
}

fn write_manifest(root: &std::path::Path, contents: &str) {
    std::fs::write(root.join("ggen.toml"), contents).expect("write ggen.toml");
}

// -- 1. Frontmatter fixture -> `frontmatter` + FM-CONFIG-100 ---------------

#[test]
fn frontmatter_manifest_reports_frontmatter_and_supported_code() {
    let dir = tempfile::tempdir().expect("tempdir");
    // Satisfies the frontmatter minimum ([project].name + [ontology].source +
    // [templates].dir) with no declarative-only marker anywhere.
    write_manifest(
        dir.path(),
        r#"
[project]
name = "demo"

[ontology]
source = "ontology/domain.ttl"

[templates]
dir = "templates"

[packs]
io-github-seanchatmangpt-ggen = { path = "packs/foo" }
"#,
    );
    let got = classify(dir.path());
    assert!(got.ok);
    assert_eq!(got.schema, "frontmatter");
    assert_eq!(got.code, CONFIG_SCHEMA_SUPPORTED);
    assert!(got.code == "FM-CONFIG-100");
    assert!(got.markers.is_empty(), "no markers on a clean verdict");
    assert!(got.diagnostic.is_none());
    // resolve_root canonicalizes (macOS: /var/folders -> /private/var/folders),
    // so compare against the canonicalized root.
    let canonical_root = dir.path().canonicalize().expect("canonicalize");
    assert_eq!(
        got.manifest_path,
        canonical_root.join("ggen.toml").display().to_string()
    );
}

// -- 2. DeclarativeRules fixture -> `declarative_rules` + FM-CONFIG-100 -----

#[test]
fn declarative_rules_manifest_reports_declarative_and_supported_code() {
    let dir = tempfile::tempdir().expect("tempdir");
    // Strong declarative marker ([generation] table present).
    write_manifest(
        dir.path(),
        r#"
[project]
name = "demo"
version = "0.1.0"

[generation]
rules = []

[[packs]]
name = "foo"
path = "packs/foo"
"#,
    );
    let got = classify(dir.path());
    assert_eq!(got.schema, "declarative_rules");
    assert_eq!(got.code, CONFIG_SCHEMA_SUPPORTED);
    assert!(got.markers.is_empty());
    assert!(got.diagnostic.is_none());
}

// -- 3. Ambiguous fixture (both marker families fire) -> `ambiguous` --------

#[test]
fn manifest_firing_both_marker_families_is_ambiguous_with_matched_markers() {
    let dir = tempfile::tempdir().expect("tempdir");
    // declarative marker: [project].version present.
    // frontmatter marker: [packs] table-of-tables (a pack entry with no
    // `name` key).
    write_manifest(
        dir.path(),
        r#"
[project]
name = "demo"
version = "0.1.0"

[packs]
foo = { path = "packs/foo" }
"#,
    );
    let got = classify(dir.path());
    assert_eq!(got.schema, "ambiguous");
    assert_eq!(got.code, CONFIG_SCHEMA_AMBIGUOUS);
    assert!(got.diagnostic.is_none());
    let decl = got
        .markers
        .iter()
        .filter(|m| m.starts_with("declarative:"))
        .count();
    let fm = got
        .markers
        .iter()
        .filter(|m| m.starts_with("frontmatter:"))
        .count();
    assert!(
        decl >= 1,
        "declarative markers surfaced, got {:?}",
        got.markers
    );
    assert!(
        fm >= 1,
        "frontmatter markers surfaced, got {:?}",
        got.markers
    );
    assert!(
        got.markers
            .iter()
            .any(|m| m == "declarative:project_version_present"),
        "the exact fired marker is named: {:?}",
        got.markers
    );
}

// -- 4. Unsupported fixture -> `unsupported` + observed top-level tables ----

#[test]
fn manifest_matching_neither_schema_is_unsupported_with_observed_markers() {
    let dir = tempfile::tempdir().expect("tempdir");
    // A lone weak declarative marker ([project].version) with no
    // [generation]/[[packs]] and no frontmatter-minimum satisfaction.
    write_manifest(
        dir.path(),
        r#"
[project]
version = "0.1.0"

[thing_that_exists_nowhere]
x = 1
"#,
    );
    let got = classify(dir.path());
    assert_eq!(got.schema, "unsupported");
    assert_eq!(got.code, CONFIG_SCHEMA_UNSUPPORTED);
    assert!(
        got.markers
            .iter()
            .all(|m| m.starts_with("unknown_top_level_table:")),
        "unsupported markers list observed top-level tables: {:?}",
        got.markers
    );
    assert!(
        got.markers
            .iter()
            .any(|m| m == "unknown_top_level_table:thing_that_exists_nowhere"),
        "{:?}",
        got.markers
    );
}

// -- 5. Malformed TOML -> typed `malformed` result, not an error ------------

#[test]
fn malformed_toml_yields_malformed_result_with_parser_diagnostic() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_manifest(dir.path(), "not [ valid toml");
    let got = classify(dir.path());
    assert!(
        got.ok,
        "malformed is a successful classification, not an error"
    );
    assert_eq!(got.schema, "malformed");
    assert_eq!(got.code, CONFIG_PARSE_FAILED);
    assert!(got.markers.is_empty());
    let diagnostic = got.diagnostic.as_deref().expect("diagnostic present");
    assert!(
        diagnostic.contains(CONFIG_PARSE_FAILED),
        "diagnostic carries the typed code: {diagnostic}"
    );
}

// -- 6. Missing / unreadable ggen.toml -> NotFound error --------------------

#[test]
fn missing_manifest_is_a_typed_not_found_error() {
    let dir = tempfile::tempdir().expect("tempdir");
    let err = config_classify(&ConfigClassifyParams {
        root: dir.path().display().to_string(),
    })
    .expect_err("no ggen.toml -> error");
    assert!(matches!(err.category, ErrorCategory::NotFound), "{err:?}");
}

// -- 7. Manifest content is pinned by BLAKE3 --------------------------------

#[test]
fn blake3_pins_the_exact_bytes_classified() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_manifest(dir.path(), "[project]\nname = \"demo\"\n");
    let first = classify(dir.path());
    assert_eq!(
        first.manifest_blake3,
        blake3::hash(b"[project]\nname = \"demo\"\n")
            .to_hex()
            .to_string()
    );
    write_manifest(dir.path(), "[project]\nname = \"other\"\n");
    let second = classify(dir.path());
    assert_ne!(
        first.manifest_blake3, second.manifest_blake3,
        "different bytes -> different digest"
    );
}
