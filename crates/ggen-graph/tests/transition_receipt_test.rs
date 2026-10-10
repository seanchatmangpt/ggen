//! Transition-receipt court: determinism, tamper-evidence, replay, chain linkage,
//! and refusal for `DeterministicGraph::apply_delta` / `GraphReceipt`.

use ggen_graph::delta::RdfDelta;
use ggen_graph::graph::dataset::{DeterministicGraph, KnowledgeHook};
use ggen_graph::receipt::{GraphReceipt, ReplayVerifier};
use ggen_graph::GraphError;

const DELTA_A_NQUADS: &[&str] = &[
    "<http://example.org/s1> <http://example.org/p1> \"v1\" .",
    "<http://example.org/s2> <http://example.org/p2> <http://example.org/o2> .",
];

fn delta_a() -> RdfDelta {
    RdfDelta::new(
        DELTA_A_NQUADS.iter().map(|s| s.to_string()).collect(),
        vec![],
    )
}

/// (a) Same mutation from the same before-state yields byte-identical state
/// and delta hashes, 10x. The receipt `signature_or_hash` necessarily varies
/// because `GraphReceipt::new` binds `Utc::now()` — determinism is pinned for
/// the content fields, timestamp entropy documented as by-design.
#[test]
fn receipt_content_is_deterministic_across_ten_runs() {
    let mut first: Option<([u8; 32], [u8; 32], [u8; 32])> = None;

    for _ in 0..10 {
        let graph = DeterministicGraph::new().unwrap();
        let receipt = graph.apply_delta(&delta_a(), &[]).unwrap();

        let content = (
            receipt.pre_state_hash,
            receipt.post_state_hash,
            receipt.delta_hash,
        );
        match &first {
            None => first = Some(content),
            Some(expected) => assert_eq!(expected, &content, "receipt content diverged"),
        }

        // Receipt integrity holds on every run.
        receipt.verify().unwrap();
    }

    let (pre, post, delta_hash) = first.unwrap();
    assert_ne!(pre, post, "empty pre-state and post-state must differ");
    assert_eq!(delta_hash, delta_a().hash());
}

/// (b) Tamper-evidence: the receipt verify path re-derives the checksum over
/// the bound fields; mutating any bound field (here `delta_hash`) fails
/// verification with `GraphError::VerificationFailed`.
#[test]
fn tampered_receipt_fails_verification() {
    let graph = DeterministicGraph::new().unwrap();
    let receipt = graph.apply_delta(&delta_a(), &[]).unwrap();
    receipt.verify().unwrap(); // clean baseline

    let mut tampered = receipt.clone();
    tampered.delta_hash = [0u8; 32];
    match tampered.verify() {
        Err(GraphError::VerificationFailed(msg)) => {
            assert!(msg.contains("GraphReceipt"), "unexpected message: {msg}");
        }
        other => panic!("expected VerificationFailed, got {other:?}"),
    }

    // Tampering the post-state hash is also detected.
    let mut tampered2 = receipt.clone();
    tampered2.post_state_hash = [0xffu8; 32];
    assert!(tampered2.verify().is_err());
}

/// (c) Replay: applying the recorded delta to a fresh graph reproduces the
/// receipt's post-state hash exactly.
#[test]
fn recorded_delta_replays_to_post_state_hash() {
    let source = DeterministicGraph::new().unwrap();
    let receipt = source.apply_delta(&delta_a(), &[]).unwrap();

    // Reconstruct the pre-state (empty here) and replay the recorded delta.
    let fresh = DeterministicGraph::new().unwrap();
    let replayed = RdfDelta::new(
        DELTA_A_NQUADS.iter().map(|s| s.to_string()).collect(),
        vec![],
    );
    replayed.apply(&fresh).unwrap();

    assert_eq!(
        fresh.state_hash().unwrap(),
        receipt.post_state_hash,
        "replayed state must reproduce the receipt's post-state hash"
    );

    // And the replayed transition itself mints a matching-content receipt.
    let fresh_receipted = DeterministicGraph::new().unwrap();
    let receipt2 = fresh_receipted.apply_delta(&replayed, &[]).unwrap();
    assert_eq!(receipt.post_state_hash, receipt2.post_state_hash);
    assert_eq!(receipt.delta_hash, receipt2.delta_hash);
}

/// (d) Chain linkage: two sequential transitions commit — receipt_2's
/// pre-state hash equals receipt_1's post-state hash — and `ReplayVerifier`
/// enforces the linear history (accepts in order, refuses a replayed
/// receipt and a discontinuous one).
#[test]
fn sequential_receipts_chain_and_replay_verifier_enforces_history() {
    let graph = DeterministicGraph::new().unwrap();
    let receipt_1 = graph.apply_delta(&delta_a(), &[]).unwrap();

    let delta_b = RdfDelta::new(
        vec!["<http://example.org/s3> <http://example.org/p3> \"v3\" .".to_string()],
        vec![DELTA_A_NQUADS[0].to_string()],
    );
    let receipt_2 = graph.apply_delta(&delta_b, &[]).unwrap();

    // Chain linkage.
    assert_eq!(
        receipt_1.post_state_hash, receipt_2.pre_state_hash,
        "receipt_2 must be bound to the after-state of receipt_1"
    );
    assert_ne!(receipt_1.post_state_hash, receipt_2.post_state_hash);
    // Final graph state matches the last receipt's post-state.
    assert_eq!(graph.state_hash().unwrap(), receipt_2.post_state_hash);

    // ReplayVerifier accepts the linear chain.
    let mut verifier = ReplayVerifier::new(None);
    verifier.verify_transition(&receipt_1).unwrap();
    verifier.verify_transition(&receipt_2).unwrap();

    // Replaying receipt_1's signature is refused.
    let replay_result = verifier.verify_transition(&receipt_1);
    assert!(matches!(
        replay_result,
        Err(GraphError::VerificationFailed(_))
    ));

    // A discontinuous receipt (right content, wrong chain position) is refused.
    let detached = DeterministicGraph::new().unwrap();
    let orphan = detached.apply_delta(&delta_a(), &[]).unwrap();
    let mut verifier2 = ReplayVerifier::new(None);
    verifier2.verify_transition(&receipt_1).unwrap();
    let orphan_result = verifier2.verify_transition(&orphan);
    match orphan_result {
        Err(GraphError::VerificationFailed(msg)) => {
            assert!(msg.contains("Chain discontinuity"), "unexpected: {msg}");
        }
        other => panic!("expected chain discontinuity, got {other:?}"),
    }
}

/// (e) Refusal: a delta whose addition is not a parseable N-Quad (bad IRI)
/// is a typed error and NO receipt is emitted — the caller receives `Err`,
/// never a partial receipt. State is unchanged when the malformed entry is
/// the only entry.
#[test]
fn invalid_transition_returns_typed_error_and_no_receipt() {
    let graph = DeterministicGraph::new().unwrap();
    let pre_hash = graph.state_hash().unwrap();

    let bad_delta = RdfDelta::new(
        vec!["<not a valid iri> <http://example.org/p> \"x\" .".to_string()],
        vec![],
    );
    let result: Result<GraphReceipt, GraphError> = graph.apply_delta(&bad_delta, &[]);
    assert!(
        result.is_err(),
        "invalid transition must not produce a receipt"
    );

    // Typed error, not a panic or a stringly collapse.
    match result {
        Err(GraphError::Serialization(_)) => {}
        other => panic!("expected GraphError::Serialization, got {other:?}"),
    }

    // State unchanged; the failed transition left no artifact.
    assert_eq!(graph.state_hash().unwrap(), pre_hash);

    // Same refusal when a validation hook fails: typed HookFailed, rolled
    // back, no receipt.
    let hook_graph = DeterministicGraph::new().unwrap();
    let hook = KnowledgeHook::new(
        "no-s1".to_string(),
        "ASK { FILTER NOT EXISTS { ?s <http://example.org/p1> ?o } }".to_string(),
    );
    let hook_result = hook_graph.apply_delta(&delta_a(), &[hook]);
    match hook_result {
        Err(GraphError::HookFailed(msg)) => assert!(msg.contains("no-s1")),
        other => panic!("expected GraphError::HookFailed, got {other:?}"),
    }
}
