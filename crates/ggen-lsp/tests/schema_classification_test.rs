#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! LSP/engine schema-classification parity (`GGEN-TPL-001` / specs/014
//! correction 2).
//!
//! `ggen-lsp`'s [`ggen_lsp::project_index::ProjectIndex::
//! from_root_with_overlay`] and `ggen-engine`'s schema dispatch
//! (`crates/ggen-engine/src/schema_dispatch.rs::load`) must agree on the
//! *classification* of every `ggen.toml`: both call the one shared
//! `ggen_config::classify_ggen_toml` classifier, so for the same document
//! they must land in the same arm — DeclarativeRules / Frontmatter /
//! Ambiguous / Unsupported / Malformed.
//!
//! The two paths diverge *intentionally* one step after classification:
//! the engine parses DeclarativeRules documents with
//! `ManifestParser::parse_and_validate` (file-existence validation is a
//! hard sync-time error), while the LSP index uses the deliberately loose
//! `ManifestParser::parse_str` so a missing query/template file surfaces
//! as a per-rule `RuleIndexEntry::issues` entry the editor can live with,
//! never a fatal index failure. The final test pins that divergence.
//!
//! Chicago-style: every fixture is a real file in a real TempDir; the
//! parity oracle is `classify_ggen_toml` called directly on the same
//! bytes — no mocks, no doubles.

use std::path::Path;

use ggen_config::{classify_ggen_toml, ConfigSchemaClassification};
use ggen_lsp::project_index::{BufferOverlay, IndexError, ProjectIndex};

const DECLARATIVE_TOML: &str = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { inline = "{{ name }}" }
"#;

const FRONTMATTER_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "model.ttl"

[templates]
dir = "templates"
"#;

/// Frontmatter-shaped (no `[project].version`) *plus* `[ai]`, a
/// GgenManifest-only table — structural markers of both schemas at once.
const AMBIGUOUS_TOML: &str = "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[templates]\ndir = \"t\"\n\n[ai]\nprovider = \"openai\"\n";

const UNSUPPORTED_TOML: &str = "[some_other_tool]\nkey = \"value\"\n";

const MALFORMED_TOML: &str = "not [ valid toml";

fn write_project(dir: &Path, contents: &str) {
    std::fs::write(dir.join("ggen.toml"), contents).expect("write ggen.toml");
}

/// Parity oracle: LSP index outcome for the fixture must match the arm the
/// engine's dispatcher derives from `classify_ggen_toml` on the same bytes.
fn assert_lsp_matches_engine_arm(dir: &Path, expected: ConfigSchemaClassificationCode) {
    let raw = std::fs::read_to_string(dir.join("ggen.toml")).expect("read fixture");
    let classification = classify_ggen_toml(&raw);
    assert_eq!(
        classification_code(&classification),
        expected,
        "engine dispatcher arm for this fixture"
    );

    let result = ProjectIndex::from_root_with_overlay(dir, &BufferOverlay::new());
    match (expected, result) {
        (ConfigSchemaClassificationCode::DeclarativeRules, Ok(index)) => {
            assert_eq!(index.rule_entries.len(), 1);
        }
        (ConfigSchemaClassificationCode::Frontmatter, Ok(index)) => {
            // Explicit empty index, not an error — the other schema is
            // valid, just out of this index's scope.
            assert!(index.rule_entries.is_empty());
        }
        (ConfigSchemaClassificationCode::Ambiguous, Err(IndexError::AmbiguousSchema { .. })) => {}
        (
            ConfigSchemaClassificationCode::Unsupported,
            Err(IndexError::UnsupportedSchema { .. }),
        ) => {}
        (ConfigSchemaClassificationCode::Malformed, Err(IndexError::ManifestParse { .. })) => {}
        (expected, result) => {
            panic!(
                "LSP index outcome does not match the engine dispatcher arm \
                 {expected:?}: got {result:?}"
            )
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConfigSchemaClassificationCode {
    DeclarativeRules,
    Frontmatter,
    Ambiguous,
    Unsupported,
    Malformed,
}

fn classification_code(c: &ConfigSchemaClassification) -> ConfigSchemaClassificationCode {
    match c {
        ConfigSchemaClassification::DeclarativeRules => {
            ConfigSchemaClassificationCode::DeclarativeRules
        }
        ConfigSchemaClassification::Frontmatter => ConfigSchemaClassificationCode::Frontmatter,
        ConfigSchemaClassification::Ambiguous { .. } => ConfigSchemaClassificationCode::Ambiguous,
        ConfigSchemaClassification::Unsupported { .. } => {
            ConfigSchemaClassificationCode::Unsupported
        }
        ConfigSchemaClassification::Malformed { .. } => ConfigSchemaClassificationCode::Malformed,
    }
}

#[test]
fn declarative_rules_fixture_classifies_same_arm_in_lsp_and_engine() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), DECLARATIVE_TOML);
    assert_lsp_matches_engine_arm(dir.path(), ConfigSchemaClassificationCode::DeclarativeRules);
}

#[test]
fn frontmatter_fixture_classifies_same_arm_in_lsp_and_engine() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), FRONTMATTER_TOML);
    assert_lsp_matches_engine_arm(dir.path(), ConfigSchemaClassificationCode::Frontmatter);
}

#[test]
fn ambiguous_fixture_is_ambiguous_in_both_lsp_and_engine_paths() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), AMBIGUOUS_TOML);
    assert_lsp_matches_engine_arm(dir.path(), ConfigSchemaClassificationCode::Ambiguous);
    // And the typed code surfaces through the LSP error's Display, the same
    // `CONFIG_SCHEMA_AMBIGUOUS` the engine's error embeds.
    let err = ProjectIndex::from_root(dir.path()).expect_err("must be ambiguous");
    assert!(
        err.to_string()
            .contains(ggen_config::CONFIG_SCHEMA_AMBIGUOUS),
        "{err}"
    );
}

#[test]
fn unsupported_fixture_classifies_same_arm_in_lsp_and_engine() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), UNSUPPORTED_TOML);
    assert_lsp_matches_engine_arm(dir.path(), ConfigSchemaClassificationCode::Unsupported);
}

#[test]
fn malformed_fixture_classifies_same_arm_in_lsp_and_engine() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), MALFORMED_TOML);
    assert_lsp_matches_engine_arm(dir.path(), ConfigSchemaClassificationCode::Malformed);
}

#[test]
fn overlay_overrides_rule_template_content_but_classification_stays_disk_anchored() {
    // Overlay precedence, pinned against the real seam: the overlay covers
    // each rule's query/template reads only. The `ggen.toml` itself is
    // ALWAYS classified from disk — an overlay entry keyed at
    // `<root>/ggen.toml` must not change which classification arm runs
    // (LIVE-BUFFER-001: "the ggen.toml manifest itself is still read from
    // disk").
    let dir = tempfile::TempDir::new().expect("tempdir");
    write_project(dir.path(), DECLARATIVE_TOML);

    // A frontmatter-shaped buffer masquerading as the manifest on the
    // overlay: if the overlay ever leaked into manifest classification,
    // this would flip the arm to Frontmatter (empty index) and the rule
    // entry would vanish.
    let mut overlay = BufferOverlay::new();
    overlay.insert(dir.path().join("ggen.toml"), FRONTMATTER_TOML.to_string());
    // A real overlay override: the rule's template, as an unsaved buffer.
    overlay.insert(
        dir.path().join("people.rs.tera"),
        "buffered {{ name }}".to_string(),
    );

    let index = ProjectIndex::from_root_with_overlay(dir.path(), &overlay)
        .expect("classification is disk-anchored; declarative index still builds");
    assert_eq!(
        index.rule_entries.len(),
        1,
        "disk ggen.toml still classified DeclarativeRules despite overlay ggen.toml entry"
    );
    // The fixture's template is inline, not a file: the overlay is not
    // consulted at all and the inline content wins. That is the actual
    // precedence contract (overlay serves template FILES only — the
    // file-backed case is the next test).
    assert_eq!(
        index.rule_entries[0].template_content.as_deref(),
        Some("{{ name }}"),
        "inline template beats overlay (overlay only serves template FILES)"
    );
}

#[test]
fn overlay_overrides_template_file_read_for_disk_backed_rule() {
    // The real overlay-precedence case: the rule's template is a FILE on
    // disk; the same path keyed in the overlay (unsaved editor buffer)
    // must win.
    let dir = tempfile::TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), DECLARATIVE_TOML).expect("write manifest");
    // The rule as written uses an inline template; rewrite it to a file
    // reference for this fixture.
    let file_rule_toml = DECLARATIVE_TOML.replace(
        "template = { inline = \"{{ name }}\" }",
        "template = { file = \"people.tera\" }",
    );
    assert_ne!(file_rule_toml, DECLARATIVE_TOML, "rewrite must apply");
    std::fs::write(dir.path().join("ggen.toml"), file_rule_toml).expect("rewrite manifest");
    std::fs::write(dir.path().join("people.tera"), "disk {{ name }}").expect("write template");

    let mut overlay = BufferOverlay::new();
    overlay.insert(
        dir.path().join("people.tera"),
        "buffer {{ name }}".to_string(),
    );

    let index = ProjectIndex::from_root_with_overlay(dir.path(), &overlay).expect("index builds");
    assert_eq!(index.rule_entries.len(), 1);
    assert_eq!(
        index.rule_entries[0].template_content.as_deref(),
        Some("buffer {{ name }}"),
        "overlay buffer content must override the on-disk template file"
    );
}

#[test]
fn loose_validation_divergence_parse_str_accepts_what_parse_and_validate_refuses() {
    // Pinned intentional divergence: the LSP index parses
    // DeclarativeRules manifests with `ManifestParser::parse_str` (loose —
    // no file-existence validation), while the engine's dispatcher
    // (`schema_dispatch::load`) uses `parse_and_validate` (a missing
    // query/template/ontology file is a hard sync-time error). A
    // declarative manifest whose rule template file does not exist on
    // disk must therefore: classify DeclarativeRules in both paths,
    // REFUSE under parse_and_validate, and ACCEPT under the LSP index —
    // with the problem recorded per-rule, never as a fatal IndexError.
    let dir = tempfile::TempDir::new().expect("tempdir");
    let toml = DECLARATIVE_TOML.replace(
        "template = { inline = \"{{ name }}\" }",
        "template = { file = \"absent.tera\" }",
    );
    std::fs::write(dir.path().join("ggen.toml"), toml).expect("write manifest");
    // NB: `absent.tera` deliberately NOT written. `model.ttl` (ontology
    // source) is also absent — both are validated by parse_and_validate,
    // neither by parse_str.

    // Same classification in both paths...
    let raw = std::fs::read_to_string(dir.path().join("ggen.toml")).expect("read");
    assert!(matches!(
        classify_ggen_toml(&raw),
        ConfigSchemaClassification::DeclarativeRules
    ));

    // ...engine path refuses...
    let manifest_path = dir.path().join("ggen.toml");
    let engine_result = ggen_config::manifest::ManifestParser::parse_and_validate(&manifest_path);
    assert!(
        engine_result.is_err(),
        "parse_and_validate must refuse a manifest whose template file is absent: {engine_result:?}"
    );

    // ...LSP path accepts with a per-rule issue.
    let index = ProjectIndex::from_root(dir.path())
        .expect("LSP index must survive a missing template file (loose parse_str)");
    assert_eq!(index.rule_entries.len(), 1);
    assert!(
        index.rule_entries[0]
            .issues
            .iter()
            .any(|i| i.starts_with("template file missing:")),
        "expected template-missing issue, got: {:?}",
        index.rule_entries[0].issues
    );
}
