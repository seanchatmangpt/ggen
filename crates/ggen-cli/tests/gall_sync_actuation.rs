//! GALL — `ggen sync` is the only actuation (the actuator-side foundation pier).
//!
//! GALL-INTEGRATION-1 proved the SENSING side: LSP/MCP/A2A converge on the same
//! diagnostic + route_id and (at most) leave OCEL *evidence* — never artifacts,
//! never an actuation receipt (see crates/ggen-lsp-a2a/tests, `from_a2a_*_pure`).
//!
//! This file proves the ACTUATOR side of the same axiom:
//!
//!   Sense  (lsp/mcp/a2a) → route + OCEL evidence    — no artifact, no receipt
//!   Actuate (ggen sync)  → gate → artifact-or-honest-refusal → receipt
//!
//! Specifically: `ggen sync` gates BEFORE actuating, and a refused sync produces
//! NO phantom receipt and NO phantom artifact. A command that reported success
//! (or emitted a receipt) for work that did not happen would be the pre-Chatman
//! failure mode — motion counted as consequence. These tests fail loudly if the
//! actuator ever fakes success.
//!
//! `sync` is a KEPT noun present in the default binary, so the real `ggen` binary
//! is exercised here via assert_cmd (no feature gate needed).

use assert_cmd::Command;
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

/// Stage an incapable sync boundary in a fresh TempDir and return it.
fn incapable_boundary() -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), INCAPABLE_MANIFEST).expect("write manifest");
    std::fs::write(dir.path().join("o.ttl"), MINIMAL_ONTOLOGY).expect("write ontology");
    dir
}

/// Count actuation receipts under a project root (the only counted-consequence record).
fn receipt_count(root: &std::path::Path) -> usize {
    let receipts = root.join(".ggen").join("receipts");
    std::fs::read_dir(&receipts)
        .map(|rd| {
            rd.filter_map(Result::ok)
                .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("json"))
                .count()
        })
        .unwrap_or(0)
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier: ggen sync gates BEFORE actuating, and refuses an incapable boundary.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_sync_gates_before_actuating_and_refuses_incapable_boundary() {
    let dir = incapable_boundary();
    Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert()
        .failure(); // non-zero exit — the actuator refuses; it does not fake success.
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier: a refused actuation leaves NO phantom receipt and NO phantom artifact.
// (Contract-drift guard — the actuator never records consequence that did not happen.)
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_refused_sync_emits_no_phantom_receipt_or_artifact() {
    let dir = incapable_boundary();

    // Snapshot the boundary BEFORE actuation: only the two input files.
    let before: Vec<_> = walk(dir.path());
    assert_eq!(before.len(), 2, "boundary starts as {{ggen.toml, o.ttl}}");
    assert_eq!(receipt_count(dir.path()), 0, "no receipt before sync");

    let _ = Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert()
        .failure();

    // After a REFUSED actuation: no actuation receipt was written.
    assert_eq!(
        receipt_count(dir.path()),
        0,
        "a refused sync must not emit a phantom actuation receipt"
    );
    // And no generated artifact appeared beyond the inputs (OCEL evidence under
    // .ggen/ is permitted — that is sensing's trace, not an actuation artifact).
    let after: Vec<_> = walk(dir.path())
        .into_iter()
        .filter(|p| !p.contains("/.ggen/"))
        .collect();
    assert_eq!(
        after, before,
        "a refused sync must not write artifacts outside .ggen evidence"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier: dry-run is non-actuating — even a (would-be) capable run writes nothing
// in preview mode, so previewing is sensing, not actuation.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_sync_dry_run_writes_no_receipt() {
    let dir = incapable_boundary();
    let _ = Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "--manifest", "ggen.toml", "--dry_run", "true"])
        .assert();
    assert_eq!(
        receipt_count(dir.path()),
        0,
        "dry-run preview is non-actuating — no receipt"
    );
}

/// Recursively list relative file paths under `root` (sorted, stable).
fn walk(root: &std::path::Path) -> Vec<String> {
    let mut out = Vec::new();
    fn rec(dir: &std::path::Path, base: &std::path::Path, out: &mut Vec<String>) {
        if let Ok(rd) = std::fs::read_dir(dir) {
            for e in rd.filter_map(Result::ok) {
                let p = e.path();
                if p.is_dir() {
                    rec(&p, base, out);
                } else if let Ok(rel) = p.strip_prefix(base) {
                    out.push(format!("/{}", rel.display()));
                }
            }
        }
    }
    rec(root, root, &mut out);
    out.sort();
    out
}
