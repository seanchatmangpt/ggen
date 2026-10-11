#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! INIT-MINE-LIB-1 — Chicago tests for the library entries `ggen_lsp::init_project`,
//! `ggen_lsp::mine`, and `ggen_lsp::check_files_in_root` over real fixture projects
//! in `TempDir`s. Asserts on on-disk state and grounded report contents, never on
//! call counts. (init.rs's own unit tests cover editor-config selection; this file
//! covers the aggregation entries as public API.)

use std::fs;
use std::path::Path;

use ggen_lsp::{check_files_in_root, init_project, mine, MineReport};
use lsp_max::lsp_types::NumberOrString;

fn code(d: &lsp_max::lsp_types::Diagnostic) -> Option<String> {
    match &d.code {
        Some(NumberOrString::String(s)) => Some(s.clone()),
        _ => None,
    }
}

// Identity CONSTRUCT *with* ORDER BY → fires E0015 (WARNING, active code) and
// nothing louder; same fixture style as field_status_test.rs.
const E0015_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } ORDER BY ?s\n";

#[test]
fn init_scaffolds_editor_configs_and_pack_on_disk() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let report = init_project(dir.path(), &[], &["generic".to_string()]).expect("init");

    // Whatever init scaffolds actually lands on disk.
    for rel in &report.files_written {
        assert!(
            dir.path().join(rel).is_file(),
            "reported file {rel} missing on disk"
        );
    }
    assert!(dir.path().join(".helix/languages.toml").is_file());
    assert!(dir.path().join(".ggen/editor/ggen-lsp.lua").is_file());
    assert!(dir
        .path()
        .join(".ggen/editor/vscode-lsp-note.json")
        .is_file());
    assert!(dir.path().join(".mcp.json").is_file());
    assert!(
        dir.path()
            .join(".agent-admissibility/hooks/generic/pre-commit.sh")
            .is_file(),
        "admissibility pack emitted"
    );
    assert_eq!(
        report.pack_dir,
        dir.path().join(".agent-admissibility").to_string_lossy()
    );
}

#[test]
fn init_is_idempotent_no_clobber_of_existing_configs() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let custom = "# my hand-tuned helix config";
    fs::create_dir_all(dir.path().join(".helix")).expect("mkdir");
    fs::write(dir.path().join(".helix/languages.toml"), custom).expect("seed");

    let first = init_project(dir.path(), &[], &["generic".to_string()]).expect("first init");
    let second = init_project(dir.path(), &[], &["generic".to_string()]).expect("second init");

    // Existing config preserved verbatim (no-clobber idempotence, not overwrite).
    let content = fs::read_to_string(dir.path().join(".helix/languages.toml")).expect("read");
    assert_eq!(content, custom);
    // The pre-existing file is reported as written only by the first run.
    assert!(first
        .files_written
        .iter()
        .any(|f| f.ends_with("ggen-lsp.lua")));
    assert!(
        !second
            .files_written
            .iter()
            .any(|f| f.ends_with("languages.toml")),
        "second run must not re-report (or rewrite) the existing config"
    );
    assert!(!first.files_written.is_empty());
}

#[test]
fn mine_over_captured_evidence_returns_grounded_report() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0015_SRC).expect("write fixture");

    // Real evidence first: a genuine gate run captured to the OCEL log.
    check_files_in_root(root, std::slice::from_ref(&rq), true).capture(root);
    let log = ggen_lsp::intel::IntelLog::at_root(root);
    let before = log.read().events.len();
    assert!(before > 0, "capture must produce events");

    let report: MineReport = mine(root).expect("mine");

    assert_eq!(report.event_count, before, "all captured events mined");
    assert!(!report.all_edges.is_empty(), "DFG discovered from real run");
    // Grounded: artifacts written under the root they were mined from.
    assert!(report.report_path.starts_with(root));
    assert!(report.report_path.is_file(), "discovery report on disk");
    assert!(report.promoted_path.starts_with(root));
    assert!(
        report.promoted_path.is_file(),
        "promoted-routes artifact on disk"
    );
    // Report content references the real mined evidence.
    let md = fs::read_to_string(&report.report_path).expect("read report");
    assert!(!md.is_empty());
    // Per family with evidence, one advisory route promoted.
    assert_eq!(
        report.promoted_count, 1,
        "one family (E0015) with evidence → one advisory route"
    );
}

#[test]
fn check_aggregates_known_code_from_fixture() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let rq = dir.path().join("q.rq");
    fs::write(&rq, E0015_SRC).expect("write fixture");

    let report = check_files_in_root(dir.path(), std::slice::from_ref(&rq), false);
    assert_eq!(report.error_count, 0, "E0015 is a warning");
    assert_eq!(report.warning_count, 1);
    let file = report
        .files
        .iter()
        .find(|f| Path::new(&f.path) == rq)
        .expect("fixture file appears in report");
    let codes: Vec<Option<String>> = file.diagnostics.iter().map(code).collect();
    assert!(
        codes.iter().any(|c| c.as_deref() == Some("E0015")),
        "identity-CONSTRUCT code E0015 in aggregated results, got {codes:?}"
    );
}

#[test]
fn mine_over_root_without_log_reports_empty_not_panic() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path().join("does-not-exist-yet");
    let report = mine(&root).expect("mine over logless root is typed-empty, not an error");
    assert_eq!(report.event_count, 0);
    assert!(report.all_edges.is_empty());
    // It still materializes its artifacts under the (created) root.
    assert!(report.report_path.is_file());
    assert!(report.promoted_path.is_file());
}
