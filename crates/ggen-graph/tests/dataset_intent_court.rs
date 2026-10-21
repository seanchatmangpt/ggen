#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Chicago courts for the intent-segregated dataset groundwork (lane V17).
//!
//! Real Oxigraph store, real quads, assertions on final state — no doubles.

use ggen_graph::dataset::{CrossGraphLeak, DatasetStore, IntentGraph};
use ggen_graph::parse_nquad;

const SYS_A: &str =
    "<http://example.org/s1> <http://example.org/state> \"running\" <http://example.org/any> .";
const SYS_B: &str =
    "<http://example.org/s2> <http://example.org/state> \"idle\" <http://example.org/any> .";
const MUT_A: &str = "<http://example.org/m1> <http://example.org/permits> <http://example.org/restart> <http://example.org/any> .";
const PROOF_A: &str = "<http://example.org/r1> <http://example.org/receiptOf> <http://example.org/m1> <http://example.org/any> .";

#[test]
fn insert_and_read_per_intent() {
    let store = DatasetStore::new().unwrap();
    store
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_A).unwrap())
        .unwrap();
    store
        .insert_quad(
            IntentGraph::PermittedMutations,
            &parse_nquad(MUT_A).unwrap(),
        )
        .unwrap();
    store
        .insert_quad(IntentGraph::ReceiptProof, &parse_nquad(PROOF_A).unwrap())
        .unwrap();

    let sys = store.quads_for(IntentGraph::SystemState).unwrap();
    assert_eq!(sys.len(), 1);
    assert_eq!(
        sys[0].graph_name,
        oxigraph::model::GraphName::NamedNode(IntentGraph::SystemState.named_graph_iri())
    );
    assert_eq!(
        store
            .quads_for(IntentGraph::PermittedMutations)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(store.quads_for(IntentGraph::ReceiptProof).unwrap().len(), 1);
}

#[test]
fn cross_graph_leak_is_typed_refusal_and_writes_nothing() {
    let store = DatasetStore::new().unwrap();
    let quad = parse_nquad(SYS_A).unwrap();

    let err = store
        .insert_quad_for(IntentGraph::SystemState, IntentGraph::ReceiptProof, &quad)
        .unwrap_err();
    match err {
        ggen_graph::DatasetError::CrossGraphLeak(leak) => {
            assert_eq!(leak.authority, IntentGraph::SystemState);
            assert_eq!(leak.target, IntentGraph::ReceiptProof);
        }
        other @ ggen_graph::DatasetError::Store(_) => {
            panic!("expected CrossGraphLeak, got {other:?}")
        }
    }

    // Same refusal from the standalone kernel.
    assert_eq!(
        store.cross_graph_leak_check(IntentGraph::PermittedMutations, IntentGraph::SystemState),
        Err(CrossGraphLeak {
            authority: IntentGraph::PermittedMutations,
            target: IntentGraph::SystemState,
        })
    );
    assert_eq!(
        store.cross_graph_leak_check(IntentGraph::SystemState, IntentGraph::SystemState),
        Ok(())
    );

    // The refused write left no state behind.
    assert_eq!(store.quads_for(IntentGraph::ReceiptProof).unwrap().len(), 0);
    assert_eq!(store.quads_for(IntentGraph::SystemState).unwrap().len(), 0);
}

#[test]
fn digest_is_insertion_order_invariant_within_intent() {
    let build = || {
        let store = DatasetStore::new().unwrap();
        store
            .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_A).unwrap())
            .unwrap();
        store
            .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_B).unwrap())
            .unwrap();
        store
    };

    let a = build().dataset_digest().unwrap();
    let b = build().dataset_digest().unwrap();
    assert_eq!(
        a, b,
        "same intent content in same order must hash identically"
    );

    // Explicit order flip through two independent stores: digest must not change.
    let store1 = DatasetStore::new().unwrap();
    store1
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_A).unwrap())
        .unwrap();
    store1
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_B).unwrap())
        .unwrap();
    let store2 = DatasetStore::new().unwrap();
    store2
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_B).unwrap())
        .unwrap();
    store2
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_A).unwrap())
        .unwrap();
    assert_eq!(
        store1.dataset_digest().unwrap(),
        store2.dataset_digest().unwrap(),
        "digest must be order-invariant within an intent"
    );
}

#[test]
fn digest_diverges_across_intents() {
    let quad = parse_nquad(SYS_A).unwrap();

    let in_system = DatasetStore::new().unwrap();
    in_system
        .insert_quad(IntentGraph::SystemState, &quad)
        .unwrap();
    let in_proof = DatasetStore::new().unwrap();
    in_proof
        .insert_quad(IntentGraph::ReceiptProof, &quad)
        .unwrap();

    assert_ne!(
        in_system.dataset_digest().unwrap(),
        in_proof.dataset_digest().unwrap(),
        "same quad in a different intent graph must change the digest"
    );

    // Adding to one intent changes the whole-dataset digest.
    let with_extra = DatasetStore::new().unwrap();
    with_extra
        .insert_quad(IntentGraph::SystemState, &quad)
        .unwrap();
    let before = with_extra.dataset_digest().unwrap();
    with_extra
        .insert_quad(
            IntentGraph::PermittedMutations,
            &parse_nquad(MUT_A).unwrap(),
        )
        .unwrap();
    assert_ne!(before, with_extra.dataset_digest().unwrap());
}

#[test]
fn named_graph_count_tracks_distinct_intents() {
    let store = DatasetStore::new().unwrap();
    assert_eq!(store.named_graph_count().unwrap(), 0);

    store
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_A).unwrap())
        .unwrap();
    store
        .insert_quad(IntentGraph::SystemState, &parse_nquad(SYS_B).unwrap())
        .unwrap();
    assert_eq!(store.named_graph_count().unwrap(), 1);

    store
        .insert_quad(
            IntentGraph::PermittedMutations,
            &parse_nquad(MUT_A).unwrap(),
        )
        .unwrap();
    store
        .insert_quad(IntentGraph::ReceiptProof, &parse_nquad(PROOF_A).unwrap())
        .unwrap();
    assert_eq!(store.named_graph_count().unwrap(), 3);
}

#[test]
fn intent_graph_serde_tags_are_stable() {
    assert_eq!(
        serde_json::to_string(&IntentGraph::SystemState).unwrap(),
        "\"graph:SystemState\""
    );
    assert_eq!(
        serde_json::to_string(&IntentGraph::PermittedMutations).unwrap(),
        "\"graph:PermittedMutations\""
    );
    assert_eq!(
        serde_json::to_string(&IntentGraph::ReceiptProof).unwrap(),
        "\"graph:ReceiptProof\""
    );
    let back: IntentGraph = serde_json::from_str("\"graph:ReceiptProof\"").unwrap();
    assert_eq!(back, IntentGraph::ReceiptProof);
}
