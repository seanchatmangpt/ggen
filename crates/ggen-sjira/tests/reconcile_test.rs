//! Chicago-style integration tests for the reconcile law
//! (`ggen-sjira/src/reconcile.rs` - port of
//! `GgenIgniter.SemanticJira.Reconciler`).
//!
//! Real collaborators: real ndjson ledgers on disk, real sha256 digests,
//! real authority JSON graphs, real vector-clock law. No doubles.
//! Assertions on final state: the returned event/status AND the ledger
//! file's on-disk event count.

use ggen_sjira::admit_work_order;
use ggen_sjira::authority::{admission_digest, AuthorityGraph, AuthorityIndex};
use ggen_sjira::ledger;
use ggen_sjira::reconcile::{reconcile, tail_digest, ReconcileOpts, ReconcileRefusal, GENESIS_DIGEST};
use serde_json::{json, Value};
use std::fs;
use std::path::PathBuf;

/// A fresh temp ledger directory per test (house pattern: pid + test name).
fn ledger_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("sjira-reconcile-{}-{name}", std::process::id()));
    fs::remove_dir_all(&dir).ok();
    fs::create_dir_all(&dir).expect("mkdir");
    dir
}

/// A well-formed authority graph + its stamped digest (mirrors
/// tests/authority_test.rs's real admit/verify round-trip).
fn authority_index() -> AuthorityIndex {
    let v = json!({
        "authorities": [
            {
                "iri": "https://graph.test/auth/x1",
                "types": ["StrategicObjective"],
                "statements": { "sj:title": ["Ship the kernel"] }
            }
        ]
    });
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let digest = admission_digest(&graph.authorities[0]);
    let mut v = v;
    v["authorities"][0]["statements"]["admissionDigest"] = json!([digest]);
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    AuthorityIndex::build(&graph)
}

fn order_json() -> Value {
    json!({
        "identity": "X1",
        "title": "t",
        "description": "d",
        "subject": "src/x1.rs",
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "VERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://graph.test/auth/x1",
        "projections": ["jira"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"]
    })
}

fn receipt(order: &Value) -> Value {
    let admitted = admit_work_order(order).expect("fixture order admits");
    json!({
        "definition_digest": admitted.definition_digest,
        "snapshot_digest": format!("sha256:{}", "a".repeat(64)),
        "target": "PARTIAL_ALIVE",
        "evidence": {
            "subject": "src/x1.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "court_results": {"c1": {"passed": true}},
            "evidence_types": ["e1"],
            "acceptance_results": {"a1": true},
            "falsifier_results": {"f1": "survived"}
        }
    })
}

fn opts() -> ReconcileOpts {
    ReconcileOpts {
        authority: Some(authority_index()),
    }
}

#[test]
fn happy_path_appends_exactly_one_event() {
    let dir = ledger_dir("happy");
    let order = order_json();
    let rcpt = receipt(&order);
    let out = reconcile(&order, &rcpt, &dir, &opts()).expect("reconcile admits");
    assert_eq!(out.status, ledger::AppendStatus::Appended);
    assert_eq!(out.event["kind"], "standing_transition_event");
    assert_eq!(out.event["identity"], "X1");
    assert_eq!(out.event["from"], "UNKNOWN");
    assert_eq!(out.event["to"], "PARTIAL_ALIVE");
    assert_eq!(out.event["authority"], "NONE");
    // File state: the ledger holds exactly one real event.
    let events = ledger::fetch(&dir).expect("ledger readable");
    assert_eq!(events.len(), 1);
    assert_eq!(events[0]["receipt_digest"], out.event["receipt_digest"]);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn second_identical_call_is_already_recorded_and_ledger_stays_at_one_event() {
    let dir = ledger_dir("replay");
    let order = order_json();
    let rcpt = receipt(&order);
    let first = reconcile(&order, &rcpt, &dir, &opts()).expect("first call appends");
    assert_eq!(first.status, ledger::AppendStatus::Appended);

    let second = reconcile(&order, &rcpt, &dir, &opts())
        .expect("replay is Ok, not a refusal");
    assert_eq!(second.status, ledger::AppendStatus::AlreadyRecorded);
    assert_eq!(second.event["event_digest"], first.event["event_digest"]);
    assert_eq!(second.event["from"], "UNKNOWN");

    // File-state assert: still exactly 1 event on disk.
    let events = ledger::fetch(&dir).expect("ledger readable");
    assert_eq!(events.len(), 1);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn definition_mismatch_refuses() {
    let dir = ledger_dir("def-mismatch");
    let order = order_json();
    let mut rcpt = receipt(&order);
    rcpt["definition_digest"] = json!(format!("sha256:{}", "9".repeat(64)));
    assert_eq!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::DefinitionMismatch {
            expected: admit_work_order(&order).unwrap().definition_digest,
            found: format!("sha256:{}", "9".repeat(64)),
        })
    );
    // Nothing was written.
    assert_eq!(ledger::fetch(&dir).expect("readable").len(), 0);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn invalid_snapshot_digest_refuses() {
    let dir = ledger_dir("bad-snapshot");
    let order = order_json();
    let mut rcpt = receipt(&order);
    rcpt["snapshot_digest"] = json!("sha256:NOTAHASH");
    assert_eq!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::InvalidSnapshotDigest {
            found: "sha256:NOTAHASH".to_string(),
        })
    );
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn candidate_sha_mismatch_refuses_before_admission() {
    let dir = ledger_dir("sha-mismatch");
    let mut order = order_json();
    order["candidate_sha"] = json!("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
    let mut rcpt = receipt(&order_json()); // receipt built from the CLEAN order
    rcpt["candidate_sha"] = json!("cccccccccccccccccccccccccccccccccccccccc");
    assert_eq!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::CandidateShaMismatch {
            expected: "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb".to_string(),
            found: "cccccccccccccccccccccccccccccccccccccccc".to_string(),
        })
    );
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn vc_concurrent_two_clocks_neither_dominates_refuses_before_writing() {
    let dir = ledger_dir("vc");
    let order = order_json();
    let rcpt = receipt(&order);
    // First transition carries vc {"a":1}.
    let mut rcpt1 = rcpt.clone();
    rcpt1["vc"] = json!({"a": 1});
    let out1 = reconcile(&order, &rcpt1, &dir, &opts()).expect("first appends");
    assert_eq!(out1.status, ledger::AppendStatus::Appended);
    assert_eq!(out1.event["vc"], json!({"a": 1}));

    // Second receipt: different receipt body (so not a replay) with a
    // CONCURRENT clock {"b":1} - neither {"a":1} nor {"b":1} dominates.
    let mut rcpt2 = receipt(&order);
    rcpt2["target"] = json!("ALIVE");
    rcpt2["evidence"]["evidence_ceiling"] = json!("VERIFIED");
    rcpt2["evidence"]["observed_execution"] = json!(true);
    rcpt2["vc"] = json!({"b": 1});
    match reconcile(&order, &rcpt2, &dir, &opts()) {
        Err(ReconcileRefusal::VcConcurrent { incoming, last }) => {
            assert_eq!(incoming, json!({"b": 1}));
            assert_eq!(last, json!({"a": 1}));
        }
        other => panic!("expected VcConcurrent, got {other:?}"),
    }
    // File-state: the concurrent receipt wrote NOTHING - still 1 event.
    assert_eq!(ledger::fetch(&dir).expect("readable").len(), 1);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn ordered_vc_stamps_and_appends() {
    let dir = ledger_dir("vc-ordered");
    let order = order_json();
    let mut rcpt1 = receipt(&order);
    rcpt1["vc"] = json!({"a": 1});
    reconcile(&order, &rcpt1, &dir, &opts()).expect("first appends");
    let mut rcpt2 = receipt(&order);
    // Different receipt body (target differs) + dominating clock {"a":2}:
    // ordered, so it stamps and appends.
    rcpt2["target"] = json!("ALIVE");
    rcpt2["evidence"]["evidence_ceiling"] = json!("VERIFIED");
    rcpt2["evidence"]["observed_execution"] = json!(true);
    rcpt2["vc"] = json!({"a": 2});
    let out2 = reconcile(&order, &rcpt2, &dir, &opts()).expect("second appends");
    assert_eq!(out2.status, ledger::AppendStatus::Appended);
    assert_eq!(out2.event["from"], "PARTIAL_ALIVE");
    assert_eq!(out2.event["vc"], json!({"a": 2}));
    assert_eq!(ledger::fetch(&dir).expect("readable").len(), 2);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn tampered_ledger_refuses_before_any_admission() {
    let dir = ledger_dir("tampered");
    let order = order_json();
    let rcpt = receipt(&order);
    reconcile(&order, &rcpt, &dir, &opts()).expect("seed one event");
    // Tamper with the event file's body: the digest no longer recomputes.
    let event_path = fs::read_dir(&dir)
        .expect("readdir")
        .filter_map(|e| e.ok())
        .find(|e| e.file_name().to_string_lossy().ends_with(".json"))
        .expect("event file")
        .path();
    let body = fs::read_to_string(&event_path).expect("read");
    let mut event: Value = serde_json::from_str(&body).expect("decode");
    event["to"] = json!("ALIVE"); // forge the standing
    fs::write(&event_path, serde_json::to_string(&event).unwrap()).expect("write");

    let out = reconcile(&order, &rcpt, &dir, &opts());
    assert!(matches!(out, Err(ReconcileRefusal::Ledger(_))), "got {out:?}");
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn origin_not_admitted_refuses_when_authority_fresh() {
    let dir = ledger_dir("origin");
    let mut order = order_json();
    order["origin_authority"] = json!("https://graph.test/auth/fresh-iri");
    let rcpt = receipt(&order);
    assert!(matches!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::OriginNotAdmitted(
            ggen_sjira::authority::OriginRefusal::AuthorityNotAdmitted { .. }
        ))
    ));
    // Nothing written.
    assert_eq!(ledger::fetch(&dir).expect("readable").len(), 0);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn missing_authority_index_refuses_unavailable() {
    let dir = ledger_dir("no-index");
    let order = order_json();
    let rcpt = receipt(&order);
    assert_eq!(
        reconcile(&order, &rcpt, &dir, &ReconcileOpts { authority: None }),
        Err(ReconcileRefusal::AuthorityIndexUnavailable {
            detail: "no authority index supplied (ReconcileOpts.authority is None)".to_string(),
        })
    );
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn promotion_refused_when_court_unwitnessed() {
    let dir = ledger_dir("promote-refused");
    let order = order_json();
    let mut rcpt = receipt(&order);
    rcpt["evidence"]["court_results"]["c1"]["passed"] = json!(false);
    assert!(matches!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::PromotionRefused(
            ggen_sjira::promote::PromoteRefusal::UnwitnessedCourt { .. }
        ))
    ));
    // The refusal happened before any byte was written.
    assert_eq!(ledger::fetch(&dir).expect("readable").len(), 0);
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn unadmitted_order_refuses() {
    let dir = ledger_dir("unadmitted");
    let rcpt = receipt(&order_json());
    let mut order = order_json();
    order["repository"] = json!("not a repo");
    assert!(matches!(
        reconcile(&order, &rcpt, &dir, &opts()),
        Err(ReconcileRefusal::Unadmitted(_))
    ));
    fs::remove_dir_all(&dir).ok();
}

#[test]
fn tail_digest_returns_last_event_digest_and_genesis_for_empty_log() {
    let dir = ledger_dir("tail");
    assert_eq!(tail_digest(&[]), GENESIS_DIGEST);
    let order = order_json();
    let rcpt = receipt(&order);
    reconcile(&order, &rcpt, &dir, &opts()).expect("append");
    let events = ledger::fetch(&dir).expect("readable");
    assert_eq!(tail_digest(&events), events.last().unwrap()["event_digest"]);
    fs::remove_dir_all(&dir).ok();
}
