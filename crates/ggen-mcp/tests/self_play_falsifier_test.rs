//! Falsifiers for the self-play harness itself.
//!
//! `self_play_test.rs` passing across 73 packs is only meaningful if the
//! harness can actually *fail*. A referee that observes nothing would report
//! the same clean sweep, and a green suite would mean nothing at all.
//!
//! These tests deliberately inject each failure mode and assert it is
//! caught. They are the negative controls that make the positive result
//! evidence rather than decoration.

use std::path::{Path, PathBuf};

use ggen_mcp::selfplay::referee::{fingerprint, referee_verdict, Observation};
use ggen_mcp::selfplay::{Board, Case, CaseOrigin, Invariant};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("repo root")
        .to_path_buf()
}

fn a_real_pack() -> PathBuf {
    let p = repo_root().join("packs/mermaid-pack");
    assert!(p.join("ontology.ttl").is_file(), "fixture pack must exist");
    p
}

fn probe(id: &str, pack: &str) -> Case {
    Case {
        id: id.to_string(),
        pack: pack.to_string(),
        sparql: "SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s LIMIT 2".to_string(),
        to: "out/probe.txt".to_string(),
        body: "{% for row in probe %}{{ row.s }}\n{% endfor %}".to_string(),
        origin: CaseOrigin::Handwritten,
        expected_violation: None,
        note: None,
    }
}

/// The board must actually build a real, loadable consumer — not an empty
/// directory that silently produces zero observations.
#[test]
fn board_manufactures_a_real_loadable_consumer() {
    let board = Board::new(&a_real_pack()).expect("board");
    let consumer = board.consumer();
    assert!(
        consumer.join("ggen.toml").is_file(),
        "consumer needs a real ggen.toml"
    );
    assert!(
        consumer.join("ontology.ttl").is_file(),
        "consumer needs the pack's ontology"
    );

    let (_, obs) = board.play(&probe("liveness", "mermaid-pack"));
    // The query must have actually executed against a real graph.
    assert_eq!(obs.query_ok, Some(true), "query did not execute: {obs:?}");
    assert!(
        obs.reported_rows.is_some(),
        "no row count observed: {obs:?}"
    );
    assert_eq!(obs.syntax_valid, Some(true));
}

/// A lawful case must actually reach the write path and produce a verified
/// receipt. If it never wrote anything, every write-path invariant in the
/// sweep would be vacuous.
#[test]
fn a_lawful_case_actually_writes_and_receipts() {
    let board = Board::new(&a_real_pack()).expect("board");
    let (verdict, obs) = board.play(&probe("writes", "mermaid-pack"));
    assert!(
        verdict.clean(),
        "expected clean, got {:?}",
        verdict.violations
    );
    assert_eq!(obs.applied_ok, Some(true), "apply did not run: {obs:?}");
    assert!(
        !obs.written.is_empty(),
        "nothing was written, so the write-path invariants are vacuous: {obs:?}"
    );
    assert_eq!(
        obs.receipt_verified,
        Some(true),
        "a real write must leave a verifying receipt: {obs:?}"
    );
    assert_eq!(
        obs.second_apply_written,
        Some(0),
        "re-sync of unchanged input must write nothing: {obs:?}"
    );
}

/// The canary/containment mechanism must detect a write outside the
/// consumer root. Injected by hand rather than hoping some case triggers
/// it — this proves the detector works, independent of whether ggen ever
/// escapes.
#[test]
fn containment_detector_catches_an_injected_escape() {
    let board = Board::new(&a_real_pack()).expect("board");
    let arena = board.consumer().parent().expect("arena").to_path_buf();

    let before = fingerprint(&arena);
    // Simulate exactly what a root escape would look like on disk.
    std::fs::write(arena.join("canary/DO_NOT_TOUCH.txt"), b"TAMPERED\n").expect("tamper");
    let after = fingerprint(&arena);

    let mut outside = Vec::new();
    for (path, hash) in &after {
        if !path.starts_with("consumer/") && before.get(path) != Some(hash) {
            outside.push(path.clone());
        }
    }
    assert!(
        !outside.is_empty(),
        "the containment detector failed to notice a canary write — every \
         NeverEscapesRoot result in the sweep would be meaningless"
    );

    let verdict = referee_verdict(&Observation {
        query_ok: Some(true),
        syntax_valid: Some(true),
        changed_outside_root: outside,
        ..Default::default()
    });
    assert!(verdict.broken().contains(&Invariant::NeverEscapesRoot));
}

/// A traversal `to:` must be REFUSED, and the refusal must be observable —
/// not merely "nothing happened for unclear reasons". This distinguishes
/// "ggen blocked it" from "the harness never got that far".
#[test]
fn traversal_to_is_refused_and_nothing_lands_outside() {
    let board = Board::new(&a_real_pack()).expect("board");
    let arena = board.consumer().parent().expect("arena").to_path_buf();
    let canary_before =
        std::fs::read(arena.join("canary/DO_NOT_TOUCH.txt")).expect("canary readable");

    let mut case = probe("traversal", "mermaid-pack");
    case.to = "../canary/ESCAPED.txt".to_string();
    let (verdict, obs) = board.play(&case);

    // Whatever the engine reported, the bytes outside must be untouched.
    let canary_after =
        std::fs::read(arena.join("canary/DO_NOT_TOUCH.txt")).expect("canary readable");
    assert_eq!(canary_before, canary_after, "traversal modified the canary");
    assert!(
        !arena.join("canary/ESCAPED.txt").exists(),
        "traversal created a file outside the consumer root"
    );
    assert!(
        obs.changed_outside_root.is_empty(),
        "containment observed changes outside root: {:?}",
        obs.changed_outside_root
    );
    assert!(
        verdict.clean(),
        "expected a clean refusal, got {:?}",
        verdict.violations
    );
}

/// Malformed SPARQL must be refused, and the refusal must be visible in the
/// observation — proving the fail-open check has real input to judge.
#[test]
fn malformed_sparql_is_observably_refused() {
    let board = Board::new(&a_real_pack()).expect("board");
    let mut case = probe("malformed", "mermaid-pack");
    case.sparql = "SELECT ?s WHERE { ?s".to_string();
    let (verdict, obs) = board.play(&case);

    assert_eq!(
        obs.syntax_valid,
        Some(false),
        "the gate should call this invalid"
    );
    assert_eq!(
        obs.query_ok,
        Some(false),
        "an invalid query must not report success"
    );
    assert!(
        verdict.clean(),
        "a correct refusal is lawful: {:?}",
        verdict.violations
    );
}

/// The referee must not treat an absent observation as a pass. This is the
/// property that turns a harness bug (dropped response, timeout) into a
/// finding instead of a silent green.
#[test]
fn a_missing_observation_is_reported_not_ignored() {
    let verdict = referee_verdict(&Observation::default());
    assert!(
        verdict.broken().contains(&Invariant::AlwaysAnswers),
        "silence must be a violation, never an accidental pass"
    );
}
