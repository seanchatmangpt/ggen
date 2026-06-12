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
//! IMPROVE-RESULT-1 — earn (or refuse) the `improving` verdict from REAL cycles.
//!
//! No fabricated events. Two real mining cycles are driven through the apparatus:
//! - a stable WARNING family (E0011) that closes and gets promoted in cycle 1,
//!   then is reused as a *mined* route in cycle 2 (so `gate_pass_rate_mined > 0`);
//! - an ERROR family (E0023) that fails in cycle 1 (headless) and whose cumulative
//!   success *rises* in cycle 2 because the editor applies real fixes (APPLY-1's
//!   rework closure). The mean per-route success rises 0.5 → 0.7 → `improving`.
//!
//! The paired flat test proves the verdict is refused without real rework — more
//! cycles alone do not earn the claim.

use std::fs;
use std::path::{Path, PathBuf};

use ggen_lsp::intel::MetricValue;
use ggen_lsp::state::ServerState;
use ggen_lsp::{check_content, check_files_in_root, compute_metrics, mine};
use lsp_max::lsp_types::Url;
use tempfile::TempDir;

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

fn write(dir: &Path, name: &str, content: &str) -> PathBuf {
    let p = dir.join(name);
    fs::write(&p, content).expect("write");
    p
}

/// A real headless cycle: check (with routes) → capture OCEL under `root`.
fn headless_cycle(root: &Path, files: &[PathBuf]) {
    check_files_in_root(root, files, true).capture(root);
}

/// A real editor rework of a ConfigValue (E0023) file: broken → fixed, closing
/// the episode via APPLY-1's RepairApplied → GatePassed chain.
async fn editor_rework_config(state: &ServerState, root: &Path, name: &str) {
    let uri = url_from_path(root.join(name));
    let broken_diags = ggen_lsp::analyzers::build_analyzer(uri.path().as_str(), "[logging]\nlevel = \"verbose\"\n")
        .map(|a| a.diagnostics())
        .unwrap_or_default();
    state.observe_diagnostics(&uri, &broken_diags).await;
    let fixed_diags = ggen_lsp::analyzers::build_analyzer(uri.path().as_str(), "[logging]\nlevel = \"info\"\n")
        .map(|a| a.diagnostics())
        .unwrap_or_default();
    state.observe_diagnostics(&uri, &fixed_diags).await;
}

/// Cycle 1 (shared by both tests): a warning family that closes (E0011 ×3) and an
/// error family that fails (E0023 ×3). Promotes the warning route.
fn seed_cycle_one(root: &Path) {
    let mut files = Vec::new();
    for i in 0..3 {
        files.push(write(
            root,
            &format!("q{i}.rq"),
            "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n",
        ));
        files.push(write(
            root,
            &format!("c{i}.toml"),
            "[logging]\nlevel = \"verbose\"\n",
        ));
    }
    headless_cycle(root, &files);
    mine(root).expect("cycle 1 mine");
}

#[tokio::test]
async fn improving_is_earned_when_rework_raises_closure_across_cycles() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // --- Cycle 1: warning closes (sr 1.0), error fails (sr 0.0) → mean 0.5. ---
    seed_cycle_one(root);

    // --- Cycle 2: reuse the promoted warning route (now MINED + passing) AND
    //     apply real editor fixes to the error family (its success rises). ---
    let rq: Vec<PathBuf> = (0..3).map(|i| root.join(format!("q{i}.rq"))).collect();
    headless_cycle(root, &rq); // E0011 now resolved by the mined route, closes

    let state = ServerState::with_root(root);
    editor_rework_config(&state, root, "fix_a.toml").await;
    editor_rework_config(&state, root, "fix_b.toml").await;

    mine(root).expect("cycle 2 mine");

    // --- The apparatus MEASURED improvement. ---
    let m = compute_metrics(root);
    assert_eq!(m.cycles, 2, "two distinct mining cycles");
    assert!(
        matches!(m.gate_pass_rate_mined, MetricValue::Value(v) if v > 0.0),
        "a mined route was selected and passed (got {:?})",
        m.gate_pass_rate_mined
    );
    assert_eq!(
        m.verdict, "improving",
        "rising per-route closure across cycles earns the verdict (metrics: {m:?})"
    );
}

#[tokio::test]
async fn flat_evidence_refuses_improving_even_with_two_cycles() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Same cycle 1.
    seed_cycle_one(root);

    // Cycle 2: reuse the mined warning route (passes) but apply NO fixes to the
    // error family — its success does not rise. More cycles, no real improvement.
    let rq: Vec<PathBuf> = (0..3).map(|i| root.join(format!("q{i}.rq"))).collect();
    headless_cycle(root, &rq);
    mine(root).expect("cycle 2 mine");

    let m = compute_metrics(root);
    assert_eq!(m.cycles, 2, "two cycles exist");
    assert!(
        matches!(m.gate_pass_rate_mined, MetricValue::Value(v) if v > 0.0),
        "the mined route still passes"
    );
    assert_eq!(
        m.verdict, "insufficient_evidence",
        "without rising closure the claim is refused, even with 2 cycles + a passing mined route"
    );
}
