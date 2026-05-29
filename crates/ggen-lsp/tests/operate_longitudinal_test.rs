//! OPERATE-1 — the smallest real authoring loop, end to end.
//!
//! Drives the ACTUAL apparatus over a real file: real analyzer diagnostics →
//! real `capture` → real `mine` → real `compute_metrics`. Nothing is hand-written
//! into the OCEL log; the events are produced by the gate itself. The proof is
//! that one real cycle emits a full receipted event chain AND that the verdict
//! *correctly refuses* "improving" on a single cycle (no event → no metric).

use std::fs;
use std::path::{Path, PathBuf};

use ggen_lsp::intel::events::activity;
use ggen_lsp::{check_files_in_root, compute_metrics, mine, IntelLog};
use tempfile::TempDir;

fn write(dir: &Path, name: &str, content: &str) -> PathBuf {
    let p = dir.join(name);
    if let Some(parent) = p.parent() {
        fs::create_dir_all(parent).expect("mkdir");
    }
    fs::write(&p, content).expect("write");
    p
}

#[test]
fn operate_one_real_cycle_emits_full_chain_and_refuses_verdict() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Real law surface: a CONSTRUCT without ORDER BY → E0011 WARNING. A warning
    // passes the gate, so its episode lawfully closes (Raised ≺ GatePassed) and
    // earns a receipt — exercising the full chain.
    let rq = write(root, "q.rq", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n");

    // Real check (with routes) → real capture. No fabricated events.
    let report = check_files_in_root(root, &[rq], true);
    assert!(!report.has_errors(), "E0011 is a warning; the gate passes");
    report.capture(root);

    // The OCEL log must contain the full receipted chain, computed by the gate.
    let log = IntelLog::at_root(root).read();
    let has = |a: &str| log.events.iter().any(|e| e.activity == a);
    assert!(has(activity::DIAGNOSTIC_RAISED), "DiagnosticRaised emitted");
    assert!(has(activity::ROUTE_SELECTED), "RouteSelected emitted");
    assert!(has(activity::REPAIR_SUGGESTED), "RepairSuggested emitted");
    assert!(
        has(activity::GATE_PASSED),
        "GatePassed emitted (warning closes)"
    );
    assert!(
        has(activity::RECEIPT_EMITTED),
        "ReceiptEmitted (closed episode)"
    );

    // First cycle uses the seed route (no pack promoted yet).
    let src = log
        .events
        .iter()
        .find(|e| e.activity == activity::ROUTE_SELECTED)
        .and_then(|e| e.attributes.get("route_source").cloned());
    assert_eq!(
        src.as_deref(),
        Some("seed"),
        "first cycle uses the seed route"
    );

    // Mine writes the promoted artifact + a promotion receipt.
    let m = mine(root).expect("mine");
    assert!(m.promoted_path.is_file(), "repair-routes.json written");
    let receipts = fs::read_dir(root.join(".ggen/receipts")).expect("receipts dir");
    assert!(
        receipts
            .filter_map(Result::ok)
            .any(|e| e.file_name().to_string_lossy().starts_with("promotion-")),
        "a promotion receipt was written"
    );

    // One cycle → the verdict MUST refuse. The smallest loop closes, and the
    // claim is correctly withheld.
    let metrics = compute_metrics(root);
    assert_eq!(metrics.cycles, 1, "exactly one mining cycle");
    assert_eq!(
        metrics.verdict, "insufficient_evidence",
        "a single cycle cannot earn 'improving' — the claim is refused"
    );
}
