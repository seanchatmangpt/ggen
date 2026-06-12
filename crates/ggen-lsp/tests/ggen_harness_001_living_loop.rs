#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! GGEN-HARNESS-001 — LIVING LSP LOOP proof (GALL-CHECKPOINT-002).
//!
//! Activates the harness-mismatch species from metadata-only to a *living*
//! diagnostic, mirroring the proven GGEN-TPL-001 living loop. The harness
//! mismatch: a `Cargo.toml` `[[test]]`/`[[bench]]` table declares an explicit
//! `path = "..."` whose file does NOT exist on disk. Source-law repair only:
//! fix the declaration or create/rename the proof file — never fabricate a
//! passing proof.
//!
//! This file proves:
//!   1. The headless gate (`check_files_in_root`) RAISES GGEN-HARNESS-001 ERROR
//!      on a real mismatch and bumps `error_count` (release-blocking).
//!   2. The gate CLEARS when the declared proof file is created (real repair).
//!   3. The repair route is source-law only (`proof-topology.repair`, seeded,
//!      all NoOp, no fabrication / emitted-output marker).
//!   4. Analysis writes no artifact.
//!   5. The live `analyze_and_observe` loop drives the full 6-link OCEL chain
//!      (DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied →
//!      GatePassed → ReceiptEmitted), proven from the EXTERNAL on-disk OCEL log.
//!   6. No-leak barriers: a HARNESS fixture raises ZERO GGEN-TPL-001.
//!
//! Chicago TDD: every test loads a *real* fixture crate tree from disk (or builds
//! one in a TempDir), runs the *real* harness index / *real* headless gate /
//! *real* `RouteRegistry`, and reads the *real* on-disk `IntelLog`. No mocks, no
//! test doubles, no fabricated diagnostics or events.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces, CheckReport};
use ggen_lsp::route::{Provenance, RouteRegistry};
use ggen_lsp::ServerState;

/// Absolute path to a fixture crate root under this crate's `tests/fixtures`.
fn fixture_root(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("ggen_harness_001_living_loop")
        .join(name)
}

/// True if a `Diagnostic.code` renders as exactly `GGEN-HARNESS-001`.
fn is_harness_001(d: &Diagnostic) -> bool {
    matches!(&d.code, Some(NumberOrString::String(s)) if s == "GGEN-HARNESS-001")
}

/// True if a `Diagnostic.code` renders as exactly `GGEN-TPL-001`.
fn is_tpl_001(d: &Diagnostic) -> bool {
    matches!(&d.code, Some(NumberOrString::String(s)) if s == "GGEN-TPL-001")
}

fn count_harness_001(report: &CheckReport) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_harness_001(d))
        .count()
}

fn count_tpl_001(report: &CheckReport) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_tpl_001(d))
        .count()
}

fn has_harness_001_error(report: &CheckReport) -> bool {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .any(|d| is_harness_001(d) && d.severity == Some(DiagnosticSeverity::ERROR))
}

/// Canonical GGEN-HARNESS-001 diagnostic key for the route-selection test
/// (`select_for_diagnostic` keys on the code; the range is the edit site).
fn harness_001_diag() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 7,
                character: 0,
            },
            end: Position {
                line: 7,
                character: 0,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String("GGEN-HARNESS-001".to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "harness mismatch: declared target `proof` does not exist".to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Recursively copy a directory tree using only std (+ tempfile). Real I/O.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Write a minimal crate with a harness mismatch: a `[[test]]` whose declared
/// `path` does not resolve. `[workspace]` keeps cargo from auto-discovering it.
fn write_mismatch_crate(dir: &Path, declared_path: &str) {
    std::fs::create_dir_all(dir.join("tests/proof")).expect("tests/proof dir");
    std::fs::write(
        dir.join("Cargo.toml"),
        format!(
            "[workspace]\n\n[package]\nname = \"seam-fixture\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[[test]]\nname = \"proof\"\npath = \"{declared_path}\"\n"
        ),
    )
    .expect("Cargo.toml");
}

/// Read the EXTERNAL OCEL intel log under
/// `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` — the unforgeable on-disk
/// surface the anti-cheating doctrine requires (not an in-process accessor).
fn read_log_lines(root: &Path) -> Vec<String> {
    let path = root
        .join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl");
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

/// True if some JSONL event line names this `activity`, the GGEN-HARNESS-001
/// `code`, and the manifest file — three substrings on one line identify a
/// single event for the manifest's harness-mismatch episode.
fn has_manifest_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("Cargo.toml")
            && l.contains("GGEN-HARNESS-001")
    })
}

// ---------------------------------------------------------------------------
// 1. LIVE RAISE: invalid harness declaration raises GGEN-HARNESS-001.
// ---------------------------------------------------------------------------

#[test]
fn invalid_harness_raises_001_through_headless_gate() {
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    assert!(
        paths.iter().any(|p| p.ends_with("Cargo.toml")),
        "fixture must contain a discoverable Cargo.toml law surface; got {paths:?}"
    );

    let report = check_files_in_root(&root, &paths, true);

    assert!(
        count_harness_001(&report) >= 1,
        "the [[test]] declares a path that does not exist; the gate must surface \
         GGEN-HARNESS-001. report.files: {:?}",
        report.files
    );
    assert!(
        has_harness_001_error(&report),
        "GGEN-HARNESS-001 must be reported at ERROR severity. report.files: {:?}",
        report.files
    );
    assert!(
        report.error_count >= 1,
        "a GGEN-HARNESS-001 ERROR must increment CheckReport.error_count (the hook \
         refusal signal). error_count={}",
        report.error_count
    );
    assert!(
        report.has_errors() && report.exit_code() != 0,
        "release_blocking: has_errors() true and nonzero exit on a real mismatch"
    );
    // No-leak barrier: a HARNESS fixture (no ggen.toml) raises ZERO GGEN-TPL-001.
    assert_eq!(
        count_tpl_001(&report),
        0,
        "a harness fixture must raise zero GGEN-TPL-001 (no leak). report.files: {:?}",
        report.files
    );
}

// ---------------------------------------------------------------------------
// 2. LAWFUL CASE: a declared path that resolves raises nothing.
// ---------------------------------------------------------------------------

#[test]
fn lawful_harness_stays_clean() {
    let root = fixture_root("valid_project");
    let paths = discover_law_surfaces(&root);
    let report = check_files_in_root(&root, &paths, true);

    assert_eq!(
        count_harness_001(&report),
        0,
        "the declared proof file exists; no GGEN-HARNESS-001 expected. report.files: {:?}",
        report.files
    );
    assert!(
        !report.has_errors(),
        "the lawful harness fixture must pass the gate"
    );
}

// ---------------------------------------------------------------------------
// 3. LIVE CLEAR: creating the missing proof file clears GGEN-HARNESS-001.
// ---------------------------------------------------------------------------

#[test]
fn creating_missing_proof_file_clears_001_through_gate() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Pre-repair: the copy must still raise GGEN-HARNESS-001 (faithful copy).
    let before = check_files_in_root(&dst, &discover_law_surfaces(&dst), true);
    assert!(
        count_harness_001(&before) >= 1,
        "pre-repair copy must raise GGEN-HARNESS-001 (else the clear is vacuous). \
         report.files: {:?}",
        before.files
    );

    // Repair (source law): create the proof file the declaration points at.
    let declared = dst.join("tests/proof/nonexistent.rs");
    std::fs::create_dir_all(declared.parent().expect("parent")).expect("mkdir");
    std::fs::write(&declared, "#[test]\nfn proof() { assert!(true); }\n").expect("write proof");

    // Post-repair: the gate must no longer raise GGEN-HARNESS-001.
    let after = check_files_in_root(&dst, &discover_law_surfaces(&dst), true);
    assert_eq!(
        count_harness_001(&after),
        0,
        "after creating the declared proof file, GGEN-HARNESS-001 must clear. \
         report.files: {:?}",
        after.files
    );
    assert!(!after.has_errors(), "repaired harness must pass the gate");
}

// ---------------------------------------------------------------------------
// 4. ROUTE METADATA: GGEN-HARNESS-001's repair route is source-law only.
// ---------------------------------------------------------------------------

#[test]
fn harness_001_route_is_source_law_only() {
    let registry = RouteRegistry::seeded();
    let route = registry
        .select_for_diagnostic(&harness_001_diag())
        .expect("GGEN-HARNESS-001 must resolve to a seeded repair route");

    assert_eq!(
        route.id.0, "proof-topology.repair",
        "GGEN-HARNESS-001 must select the proof-topology source-law route"
    );
    assert_eq!(route.provenance, Provenance::Seeded);
    assert!(route.steps.is_sound(), "route must be structurally sound");

    // No step may fabricate a proof or target an emitted/generated output; every
    // step must name a harness source-law surface.
    const FORBIDDEN: &[&str] = &[
        "out/",
        "output/",
        "dist/",
        "gen/",
        "emitted",
        "fabricate",
        "make the proof pass",
        "pass the test",
        "stub",
    ];
    const SOURCE_LAW: &[&str] = &["cargo.toml", "makefile.toml", "tests"];
    assert!(!route.steps.nodes.is_empty(), "route must offer steps");
    for step in &route.steps.nodes {
        assert!(
            matches!(step.edit, ggen_lsp::route::EditTemplate::NoOp),
            "HARNESS step {:?} must be advisory (NoOp)",
            step.id
        );
        let title = step.title.to_lowercase();
        for f in FORBIDDEN {
            assert!(
                !title.contains(f),
                "HARNESS step {:?} references forbidden marker {f:?}",
                step.title
            );
        }
        assert!(
            SOURCE_LAW.iter().any(|s| title.contains(s)),
            "HARNESS step {:?} must reference a source-law surface",
            step.title
        );
    }
}

// ---------------------------------------------------------------------------
// 5. NO ARTIFACT: running the gate never materializes anything spurious.
// ---------------------------------------------------------------------------

#[test]
fn headless_gate_writes_no_artifact() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Snapshot the (recursive) file set before, run the gate, compare after.
    let before: Vec<PathBuf> = walk_files(&dst);
    let _ = check_files_in_root(&dst, &discover_law_surfaces(&dst), true);
    let after: Vec<PathBuf> = walk_files(&dst);

    assert_eq!(
        before, after,
        "analysis must be read-only — the headless gate must not create, delete, \
         or rename any file under the project root"
    );
    // The declared (missing) proof file must NOT have been fabricated.
    assert!(
        !dst.join("tests/proof/nonexistent.rs").exists(),
        "the gate must NEVER fabricate the missing proof file"
    );
}

/// Recursively list every file path under `root`, sorted (deterministic).
fn walk_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    fn rec(dir: &Path, out: &mut Vec<PathBuf>) {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for e in entries.flatten() {
                let p = e.path();
                if p.is_dir() {
                    rec(&p, out);
                } else {
                    out.push(p);
                }
            }
        }
    }
    rec(root, &mut out);
    out.sort();
    out
}

// ---------------------------------------------------------------------------
// 6. LIVING LOOP: analyze_and_observe drives the full 6-link OCEL chain.
// ---------------------------------------------------------------------------

/// Drive the REAL live orchestration (`ServerState::analyze_and_observe`, the
/// Client-free core of `server::refresh_analyzer`) to RAISE then CLEAR a harness
/// mismatch, and prove all six lifecycle activities for the manifest +
/// GGEN-HARNESS-001 from the EXTERNAL on-disk OCEL log. The chain is read from
/// disk, never an in-process bool.
#[tokio::test]
async fn analyze_and_observe_records_live_harness_receipt_chain() {
    // ── Arrange: a crate whose [[test]] declares a path that does not exist.
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    write_mismatch_crate(root, "tests/proof/nonexistent.rs");
    let state = ServerState::with_root(root);

    let cargo_path = root.join("Cargo.toml");
    let cargo_uri = url_from_path(&cargo_path);
    let cargo_src = std::fs::read_to_string(&cargo_path).expect("read Cargo.toml");

    // ── Act 1 — RAISE: analyze the manifest through the real orchestration.
    let raised = state.analyze_and_observe(&cargo_uri, &cargo_src).await;
    assert!(
        raised
            .iter()
            .any(|(u, diags)| u == &cargo_uri && diags.iter().any(|d| is_harness_001(&d.lsp))),
        "analyze_and_observe must raise GGEN-HARNESS-001 on the manifest. published: {raised:?}"
    );

    // ── Act 2 — REPAIR (source law): create the declared proof file, then
    // re-analyze the manifest so the orchestration recomputes the harness index
    // and reconciles the now-lawful manifest's stale diagnostic.
    let declared = root.join("tests/proof/nonexistent.rs");
    std::fs::create_dir_all(declared.parent().expect("parent")).expect("mkdir");
    std::fs::write(&declared, "#[test]\nfn proof() {}\n").expect("write proof");
    let cleared = state.analyze_and_observe(&cargo_uri, &cargo_src).await;
    assert!(
        cleared
            .iter()
            .any(|(u, diags)| u == &cargo_uri && !diags.iter().any(|d| is_harness_001(&d.lsp))),
        "the proof-file repair must re-publish the manifest URI without GGEN-HARNESS-001. \
         published: {cleared:?}"
    );

    // ── Assert: the EXTERNAL intel log proves the full live chain on the manifest.
    let lines = read_log_lines(root);
    for activity in [
        "DiagnosticRaised",
        "RouteSelected",
        "RepairSuggested",
        "RepairApplied",
        "GatePassed",
        "ReceiptEmitted",
    ] {
        assert!(
            has_manifest_event(&lines, activity),
            "live chain link {activity:?} for GGEN-HARNESS-001 on the manifest missing.\nlog:\n{}",
            lines.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// 7. NO-LEAK BARRIER: a harness fixture raises zero GGEN-TPL-001 via the seam.
// ---------------------------------------------------------------------------

/// Symmetric to the TPL regression barrier: driving the live seam over a harness
/// mismatch (Cargo.toml, no ggen.toml) must raise GGEN-HARNESS-001 and NEVER a
/// GGEN-TPL-001 — the two species never cross-contaminate.
#[tokio::test]
async fn harness_seam_raises_zero_tpl_001() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    write_mismatch_crate(root, "tests/proof/nonexistent.rs");
    let state = ServerState::with_root(root);

    let cargo_uri = url_from_path(root.join("Cargo.toml"));
    let cargo_src = std::fs::read_to_string(root.join("Cargo.toml")).expect("read Cargo.toml");

    let published = state.analyze_and_observe(&cargo_uri, &cargo_src).await;

    let raised_harness = published
        .iter()
        .flat_map(|(_, d)| d.iter())
        .any(|d| is_harness_001(&d.lsp));
    let raised_tpl = published.iter().flat_map(|(_, d)| d.iter()).any(|d| is_tpl_001(&d.lsp));

    assert!(
        raised_harness,
        "harness mismatch must raise GGEN-HARNESS-001"
    );
    assert!(
        !raised_tpl,
        "a harness fixture must raise ZERO GGEN-TPL-001 (no leak). published: {published:?}"
    );
}
