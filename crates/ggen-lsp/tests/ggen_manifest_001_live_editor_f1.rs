#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Red-team finding F1 (config-schema-dispatch, legacy-path-contamination) —
//! proof that the LIVE editor-facing LSP path (`ServerState::analyze_and_observe`,
//! `src/state.rs`) surfaces a `ggen.toml` that EXISTS but fails typed
//! `GgenManifest` deserialization, instead of silently reporting zero
//! diagnostics.
//!
//! Before the fix, `state.rs`'s four `detect_*_for` methods
//! (`detect_tpl_001_for`/`detect_out_001_for`/`detect_rule_001_for`/
//! `detect_harness_001_for`) discarded `ProjectIndex::from_root_with_overlay`'s
//! `Err` via a bare `Err(_) => Vec::new()` — the exact fail-open pattern
//! `check.rs`'s `fold_manifest_load_errors` was added to close for the batch
//! `ggen-lsp check` path (red-team finding F6). A syntactically-valid
//! `ggen.toml` whose `[[generation.rules]]` entry is missing the required
//! `output_file` field is real TOML (so `TomlAnalyzer`'s raw-syntax check
//! reports nothing) but fails `GgenManifest` deserialization
//! (`IndexError::ManifestParse`), so all four live cross-surface checks used
//! to report a clean editor while `ggen sync run` / the batch `check` path
//! would refuse to load the same manifest at all.
//!
//! Chicago TDD: real `TempDir`, real `ggen.toml` on disk, real
//! `ServerState::analyze_and_observe` — no mocks, no fabricated diagnostics.

use std::path::Path;

use ggen_lsp::ServerState;
use lsp_max::lsp_types::{NumberOrString, Url};

fn url_from_path(path: impl AsRef<Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

/// A `ggen.toml` that classifies as the declarative-rules schema (it has a
/// non-empty `[[generation.rules]]` array, so `ggen_config::classify_ggen_toml`
/// dispatches to the real `ManifestParser::parse_str`) but is missing the
/// `output_file` field `ggen_config::manifest::types::GenerationRule` requires
/// — syntactically valid TOML, semantically broken manifest.
const BROKEN_MANIFEST: &str = r#"[project]
name = "f1-broken-manifest-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
"#;

fn write_broken_project(dir: &Path) {
    std::fs::create_dir_all(dir.join("schema")).expect("schema dir");
    std::fs::create_dir_all(dir.join("queries")).expect("queries dir");
    std::fs::create_dir_all(dir.join("templates")).expect("templates dir");
    std::fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("ttl");
    std::fs::write(
        dir.join("queries/items.rq"),
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
    )
    .expect("rq");
    std::fs::write(dir.join("templates/item.tera"), r#"{{ row["name"] }}"#).expect("tera");
    std::fs::write(dir.join("ggen.toml"), BROKEN_MANIFEST).expect("ggen.toml");
}

/// Independently confirm the fixture actually reproduces `IndexError::
/// ManifestParse` at the `ProjectIndex` layer (not some other, unrelated
/// failure) before trusting the live-editor assertion below.
#[test]
fn fixture_manifest_reproduces_index_manifest_parse_error() {
    let tmp = tempfile::tempdir().expect("tempdir");
    write_broken_project(tmp.path());

    let err = ggen_lsp::project_index::ProjectIndex::from_root(tmp.path())
        .expect_err("a [[generation.rules]] entry missing output_file must fail to build");
    match err {
        ggen_lsp::project_index::IndexError::ManifestParse { .. } => {}
        other => panic!("expected IndexError::ManifestParse, got {other:?}"),
    }
}

/// The live editor path: `ServerState::analyze_and_observe` on the broken
/// `ggen.toml` itself (editing the manifest is exactly what
/// `state.rs::is_ggen_manifest` treats as a GGEN-TPL-001/OUT-001/RULE-001
/// trigger) must publish a `GGEN-MANIFEST-001` diagnostic anchored at
/// `ggen.toml`, not silently report zero diagnostics for it.
#[tokio::test]
async fn analyze_and_observe_surfaces_manifest_load_failure_not_silence() {
    // ── Arrange
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    write_broken_project(root);
    let state = ServerState::with_root(root);

    let manifest_path = root.join("ggen.toml");
    let manifest_uri = url_from_path(&manifest_path);
    let manifest_src = std::fs::read_to_string(&manifest_path).expect("read ggen.toml");

    // ── Act: the live editor analyzes the manifest it just "typed" (in this
    // test, the on-disk content already matches; a real editor session would
    // have called this once per keystroke reaching the same broken state).
    let published = state
        .analyze_and_observe(&manifest_uri, &manifest_src)
        .await;

    // ── Assert: SOME published entry for ggen.toml carries a GGEN-MANIFEST-001
    // diagnostic. Before the fix this fails: every detect_*_for call site
    // returned `Vec::new()` on `Err(_)`, so `published` contains only
    // `(manifest_uri, own_diags)` where `own_diags` is TomlAnalyzer's raw
    // syntax check output -- empty, because the manifest IS valid TOML.
    let has_manifest_001 = published.iter().any(|(u, diags)| {
        u == &manifest_uri
            && diags.iter().any(|d| {
                matches!(
                    &d.lsp.code,
                    Some(NumberOrString::String(s)) if s == "GGEN-MANIFEST-001"
                )
            })
    });
    assert!(
        has_manifest_001,
        "analyze_and_observe must surface GGEN-MANIFEST-001 for a ggen.toml \
         that exists but fails GgenManifest deserialization (red-team finding \
         F1) -- published: {published:?}"
    );

    // The manifest-load-failure diagnostic must be an ERROR (matching
    // check.rs's `fold_manifest_load_errors` severity), not a warning that a
    // strict-mode-off editor could shrug off.
    let is_error = published.iter().any(|(u, diags)| {
        u == &manifest_uri
            && diags.iter().any(|d| {
                matches!(&d.lsp.code, Some(NumberOrString::String(s)) if s == "GGEN-MANIFEST-001")
                    && d.lsp.severity == Some(lsp_max::lsp_types::DiagnosticSeverity::ERROR)
            })
    });
    assert!(
        is_error,
        "GGEN-MANIFEST-001 must be ERROR severity -- published: {published:?}"
    );
}

/// Regression guard for the *other* branch of the same fix: a project root
/// with NO `ggen.toml` at all must stay completely silent (`IndexError::
/// ManifestNotFound` is the ordinary "not a ggen project" case, not a load
/// failure) -- editing a lone `.tera`/`.rq` file outside any ggen project
/// must never manufacture a spurious GGEN-MANIFEST-001.
#[tokio::test]
async fn analyze_and_observe_stays_silent_when_no_manifest_present() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    std::fs::create_dir_all(root.join("templates")).expect("templates dir");
    let tera_path = root.join("templates/item.tera");
    std::fs::write(&tera_path, r#"{{ row["name"] }}"#).expect("tera");
    // Deliberately no ggen.toml anywhere under `root`.

    let state = ServerState::with_root(root);
    let tera_uri = url_from_path(&tera_path);
    let tera_src = std::fs::read_to_string(&tera_path).expect("read tera");

    let published = state.analyze_and_observe(&tera_uri, &tera_src).await;

    let has_manifest_001 = published.iter().any(|(_, diags)| {
        diags.iter().any(|d| {
            matches!(
                &d.lsp.code,
                Some(NumberOrString::String(s)) if s == "GGEN-MANIFEST-001"
            )
        })
    });
    assert!(
        !has_manifest_001,
        "a project with no ggen.toml at all must never raise GGEN-MANIFEST-001 \
         -- published: {published:?}"
    );
}
