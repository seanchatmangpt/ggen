//! FIELD-STATUS-1 — the read-only process-mining cockpit reports what the real field
//! log proves, broken down by transport, and refuses a verdict the log doesn't earn.

use std::fs;
use std::path::Path;

use ggen_lsp::intel::MetricValue;
use ggen_lsp::{
    capture_request, check_files_in_root, compute_metrics, field_status, Attribution,
    FieldReadiness,
};
use tempfile::TempDir;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

#[test]
fn empty_project_reports_no_evidence() {
    let dir = TempDir::new().expect("tempdir");
    let s = field_status(dir.path());
    assert_eq!(s.event_count, 0);
    assert_eq!(s.episode_count, 0);
    assert_eq!(s.readiness, FieldReadiness::NoEvidence);
    assert_eq!(s.verdict, "insufficient_evidence");
    assert_eq!(s.conformance_rate, MetricValue::InsufficientEvidence);
    assert!(s.by_transport.is_empty());
}

#[test]
fn field_status_breaks_down_by_transport_from_real_evidence() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0011_SRC).expect("write");

    // Real evidence across three transports (no fabrication).
    check_files_in_root(root, std::slice::from_ref(&rq), true)
        .capture_attributed(root, &Attribution::new("alpha", "lsp", "s-lsp"));
    capture_request(root, "q.rq", E0011_SRC, &Attribution::new("beta", "mcp", "s-mcp"));
    capture_request(root, "q.rq", E0011_SRC, &Attribution::new("gamma", "a2a", "s-a2a"));

    let s = field_status(root);
    assert_eq!(s.episode_count, 3, "one E0011 episode per transport");
    assert_eq!(s.by_transport.get("lsp"), Some(&1));
    assert_eq!(s.by_transport.get("mcp"), Some(&1));
    assert_eq!(s.by_transport.get("a2a"), Some(&1));
    assert_eq!(s.distinct_sessions, 3);
    assert_eq!(s.by_agent.len(), 3);
    // E0011 is a warning → every episode closes (Raised ≺ GatePassed).
    assert_eq!(s.conformance_rate, MetricValue::Value(1.0));
    // No mine yet → accumulating, verdict refused.
    assert_eq!(s.readiness, FieldReadiness::Accumulating);
    assert_eq!(s.verdict, "insufficient_evidence");
    assert!(!s.reasons.is_empty(), "honest 'why' is reported");
}

#[test]
fn verdict_matches_compute_metrics_one_source_of_truth() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0011_SRC).expect("write");
    check_files_in_root(root, std::slice::from_ref(&rq), true).capture(root);

    let s = field_status(root);
    let m = compute_metrics(root);
    assert_eq!(s.verdict, m.verdict, "cockpit reuses the compute_metrics verdict verbatim");
    assert_eq!(s.cycles, m.cycles, "cycles agree — never a divergent count");
}

#[test]
fn distinct_variants_reflect_distinct_chains() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    // A warning (closes: …→GatePassed→ReceiptEmitted) and an error (fails:
    // …→GateFailed→RefusalEmitted) are genuinely different process variants.
    let rq = root.join("q.rq");
    fs::write(&rq, E0011_SRC).expect("write");
    let bad = root.join("bad.toml");
    fs::write(&bad, "[logging]\nlevel = \"verbose\"\n").expect("write");

    check_files_in_root(root, &[rq, bad], true).capture(root);

    let s = field_status(Path::new(root));
    assert_eq!(s.episode_count, 2);
    assert_eq!(s.distinct_variants, 2, "warning-close vs error-refusal are distinct variants");
}
