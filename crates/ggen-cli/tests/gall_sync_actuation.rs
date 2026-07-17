// ARCHIVED (v26.7.16 routing flip): every test here invokes the retired flat
// `ggen sync --manifest <path> --dry_run true` shape. `--manifest` is
// REFUSED on the new `sync run` (ggen-engine resolves the project root as
// cwd -- see docs/reference/ggen_sync_manual.md#0-migration-the-v26716-cli-routing-flip).
// Worse than an honest failure: the two tests that assert `.failure()`
// (`gall_sync_gates_before_actuating_and_refuses_incapable_boundary`,
// `gall_refused_sync_emits_no_phantom_receipt_or_artifact`) still report
// "ok" under `cargo test -p ggen-cli-lib` -- but only because a clap
// "unexpected argument '--manifest'" parse error is *also* a nonzero exit,
// not because the incapable-boundary refusal logic under test ran at all.
// They were passing for the wrong reason.
//
// Beyond the flag, this file also asserts on the OLD ggen-core receipt
// shape (`.ggen/receipts/latest.json` with top-level `input_hashes`/
// `output_hashes`/`signature` fields -- see
// docs/reference/ggen_sync_manual.md#8-cryptographic-receipts), which
// differs from ggen-engine's receipt (`.ggen-v2/receipt.json`, see
// crates/ggen-engine/tests/cli_boundary.rs). Re-deriving this file's O*
// -closure-binding assertions against ggen-engine's actual receipt schema
// is real work beyond a flag rename, and that schema is under active
// development in this workspace concurrently with this change (see
// `crates/ggen-engine/src/{sync,template}.rs`) -- attempting it here risked
// asserting on soon-to-be-stale internals. Left gated and BLOCKED rather
// than guessed at.
#![cfg(feature = "ggen-core-retired")]
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

//! GALL — `ggen sync` is the only actuation (the actuator-side foundation pier).
//!
//! GALL-INTEGRATION-1 proved the SENSING side: LSP/MCP/A2A converge on the same
//! diagnostic + route_id and (at most) leave OCEL *evidence* — never artifacts,
//! never an actuation receipt (see crates/ggen-lsp-a2a/tests, `from_a2a_*_pure`).
//!
//! This file proves the ACTUATOR side via the complete actuation triad:
//!
//!   incapable sync       → refused; no artifact, no receipt
//!   capable dry-run sync → preview only; no artifact, no receipt
//!   capable real sync    → artifact + receipt
//!
//! Sense (lsp/mcp/a2a) → route + OCEL evidence, no artifact, no receipt.
//! Actuate (ggen sync) → gate → artifact-or-honest-refusal → receipt.
//!
//! `sync` is a KEPT noun present in the default binary, so the real `ggen` binary
//! is exercised here via assert_cmd. The capable boundary is the SYNC-ACTUATOR-1
//! project (playground/sync-foundation), copied hermetically into a TempDir.

use assert_cmd::Command;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// A schema-shaped but INCAPABLE boundary: parseable manifest whose measurement
/// system is not capable (no generation/inference). `ggen sync` must refuse.
const INCAPABLE_MANIFEST: &str = r#"
[project]
name = "gall-actuation-fixture"
version = "0.1.0"

[ontology]
source = "o.ttl"
base_iri = "https://ex.org/"

[inference]
rules = []
"#;

const MINIMAL_ONTOLOGY: &str = "@prefix : <https://ex.org/> .\n:A a :Thing .\n";

/// Stage an incapable sync boundary in a fresh TempDir.
fn incapable_boundary() -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), INCAPABLE_MANIFEST).expect("write manifest");
    std::fs::write(dir.path().join("o.ttl"), MINIMAL_ONTOLOGY).expect("write ontology");
    dir
}

/// Stage the CAPABLE SYNC-ACTUATOR-1 boundary (playground/sync-foundation) in a
/// fresh TempDir — a real public-ontology-aligned O* from which sync can actuate.
fn capable_boundary() -> TempDir {
    let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../playground/sync-foundation");
    let dir = TempDir::new().expect("tempdir");
    std::fs::copy(src.join("ggen.toml"), dir.path().join("ggen.toml")).expect("copy manifest");
    copy_dir(&src.join("ontology"), &dir.path().join("ontology"));
    copy_dir(&src.join("templates"), &dir.path().join("templates"));
    dir
}

fn copy_dir(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src)
        .expect("read_dir")
        .filter_map(Result::ok)
    {
        let p = entry.path();
        let target = dst.join(entry.file_name());
        if p.is_dir() {
            copy_dir(&p, &target);
        } else {
            std::fs::copy(&p, &target).expect("copy file");
        }
    }
}

/// Count actuation receipts under a project root (the only counted-consequence record).
fn receipt_count(root: &Path) -> usize {
    std::fs::read_dir(root.join(".ggen").join("receipts"))
        .map(|rd| {
            rd.filter_map(Result::ok)
                .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("json"))
                .count()
        })
        .unwrap_or(0)
}

/// The single generated artifact the SYNC-ACTUATOR-1 boundary produces.
fn artifact_path(root: &Path) -> PathBuf {
    root.join("generated").join("gall_command_foundation.rs")
}

// ─────────────────────────────────────────────────────────────────────────────
// Triad 1: incapable sync → refused; no artifact, no receipt.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_sync_gates_before_actuating_and_refuses_incapable_boundary() {
    let dir = incapable_boundary();
    Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert()
        .failure(); // the actuator refuses; it does not fake success.
}

#[test]
fn gall_refused_sync_emits_no_phantom_receipt_or_artifact() {
    let dir = incapable_boundary();
    let before = non_ggen_files(dir.path());
    assert_eq!(before.len(), 2, "boundary starts as {{ggen.toml, o.ttl}}");

    let _ = Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert()
        .failure();

    assert_eq!(
        receipt_count(dir.path()),
        0,
        "a refused sync emits no phantom receipt"
    );
    assert_eq!(
        non_ggen_files(dir.path()),
        before,
        "a refused sync writes no artifacts (OCEL evidence under .ggen is permitted)"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Triad 2: capable dry-run → preview only; no artifact, no receipt.
// (Uses a CAPABLE boundary that would actuate if dry-run were false — so this
// genuinely proves preview-without-actuation, not just refusal.)
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_capable_sync_dry_run_previews_without_receipt_or_artifact() {
    let dir = capable_boundary();
    Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert()
        .success(); // a capable boundary previews cleanly.

    assert!(
        !artifact_path(dir.path()).exists(),
        "dry-run is a pure preview — it writes no artifact"
    );
    assert_eq!(
        receipt_count(dir.path()),
        0,
        "dry-run is non-actuating — it emits no receipt"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Triad 3: capable real sync → artifact + receipt (the actuator moves load).
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_capable_sync_actuates_and_emits_receipt_when_not_dry_run() {
    let dir = capable_boundary();
    Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml"])
        .assert()
        .success();

    // The artifact was materialized.
    let artifact = artifact_path(dir.path());
    assert!(artifact.exists(), "capable sync materializes the artifact");
    let body = std::fs::read_to_string(&artifact).expect("read artifact");
    assert!(
        body.contains("template.values-inline") && body.contains("E0011"),
        "the artifact carries the facts manufactured from the ontology"
    );

    // Exactly one actuation left a receipt, and it is genuinely signed.
    assert!(
        receipt_count(dir.path()) >= 1,
        "capable sync emits a receipt"
    );
    let latest =
        std::fs::read_to_string(dir.path().join(".ggen/receipts/latest.json")).expect("receipt");
    let receipt: serde_json::Value = serde_json::from_str(&latest).expect("receipt is JSON");
    let sig = receipt["signature"].as_str().unwrap_or("");
    assert!(
        !sig.is_empty(),
        "the actuation receipt carries a non-empty signature"
    );
    assert!(
        !receipt["output_hashes"]
            .as_array()
            .map(Vec::is_empty)
            .unwrap_or(true),
        "the receipt records the output it produced"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// O-STAR-RECEIPT-CLOSURE-1: the receipt binds the FULL O*, not just the manifest.
// R ⊢ A = μ(O*) is a lie if the receipt omits the ontology/template that determine
// the artifact. The receipt must witness the whole closure + the actuator identity.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_receipt_binds_full_o_star_closure() {
    let dir = capable_boundary();
    Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml"])
        .assert()
        .success();

    let latest =
        std::fs::read_to_string(dir.path().join(".ggen/receipts/latest.json")).expect("receipt");
    let receipt: serde_json::Value = serde_json::from_str(&latest).expect("receipt is JSON");
    let inputs: Vec<String> = receipt["input_hashes"]
        .as_array()
        .expect("input_hashes array")
        .iter()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect();
    let joined = inputs.join("\n");

    // The closure must bind: the manifest, the ontology, the template, and the actuator.
    assert!(joined.contains("ggen.toml:"), "binds the manifest");
    assert!(
        inputs.iter().any(|h| h.contains(".ttl:")),
        "binds the ontology source (was previously omitted): {joined}"
    );
    assert!(
        inputs.iter().any(|h| h.contains(".tera:")),
        "binds the Tera template (was previously omitted): {joined}"
    );
    assert!(
        inputs.iter().any(|h| h.starts_with("actuator:ggen-sync@")),
        "binds the actuator identity: {joined}"
    );
    // No MISSING closure inputs in a successful actuation.
    assert!(
        !joined.contains(":MISSING"),
        "every closure input was bound (no MISSING): {joined}"
    );
}

/// Relative file paths under `root`, EXCLUDING anything inside a `.ggen` directory
/// (sensing/evidence + actuation bookkeeping). Component-based for portability.
fn non_ggen_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    fn rec(dir: &Path, base: &Path, out: &mut Vec<PathBuf>) {
        if let Ok(rd) = std::fs::read_dir(dir) {
            for e in rd.filter_map(Result::ok) {
                let p = e.path();
                if p.is_dir() {
                    rec(&p, base, out);
                } else if let Ok(rel) = p.strip_prefix(base) {
                    if !rel.components().any(|c| c.as_os_str() == ".ggen") {
                        out.push(rel.to_path_buf());
                    }
                }
            }
        }
    }
    rec(root, root, &mut out);
    out.sort();
    out
}
