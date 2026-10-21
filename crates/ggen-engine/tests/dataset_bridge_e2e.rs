//! Receipt→dataset bridge court (v26.10.20 dialect slice 2, lane G15).
//!
//! Chicago-TDD: real five-stage sync in a real tempdir project produces the
//! receipt; the real `DatasetStore` (real Oxigraph store) receives the
//! projection; the real `audit_isolation` sweep adjudicates. No mocks.
//!
//! Courts:
//! 1. a real sync-produced receipt projects into `ReceiptProof` and the
//!    isolation audit passes;
//! 2. a synthetic `ReceiptProof` triple sharing a `SystemState` subject is a
//!    typed leak refusal naming the subject and both intents;
//! 3. the dataset digest is invariant under two different projection orders.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::dataset_bridge::{
    audit_isolation, project_receipt_proof, ReceiptProofFacts, PROOF_NS,
};
use ggen_engine::sync::{sync, SyncOptions, RECEIPT_REL_PATH};
use ggen_graph::{DatasetStore, IntentGraph};
use oxigraph::model::{GraphName, Literal, NamedNode, Quad};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "dataset-bridge"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n";

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// Scaffold a real project and run a real five-stage sync; return the
/// deserialized receipt read from disk (same fixture pattern as
/// `envelope_receipt_binding_test.rs`).
fn sync_receipt(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
    sync(root, SyncOptions::default()).expect("sync succeeds");
}

fn read_receipt(root: &Path) -> ggen_engine::sync::SyncReceipt {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt.json");
    serde_json::from_str(&raw).expect("parse receipt.json")
}

/// One observed-fact quad in `SystemState` (real intent-authorized write).
fn state_quad(subject: &str) -> Quad {
    Quad::new(
        NamedNode::new_unchecked(subject),
        NamedNode::new_unchecked("http://example.org/observed"),
        Literal::new_simple_literal("fact"),
        GraphName::DefaultGraph,
    )
}

#[test]
fn real_receipt_projects_and_audit_passes() {
    let dir = TempDir::new().expect("tempdir");
    sync_receipt(dir.path());
    let receipt = read_receipt(dir.path());
    assert!(
        !receipt.record.chain_hash_hex.is_empty(),
        "fixture must carry a real chain hash"
    );

    let facts = ReceiptProofFacts::from_sync_receipt(&receipt, "ALIVE");
    let mut store = DatasetStore::new().expect("store");
    project_receipt_proof(&mut store, &facts).expect("projection succeeds");

    // Exact closed-vocabulary projection: chainHash always, generation iff
    // the receipt carries one, standing always.
    let quads = store.quads_for(IntentGraph::ReceiptProof).expect("quads");
    let expected = 2 + usize::from(receipt.payload.generation.is_some());
    assert_eq!(quads.len(), expected, "chainHash + [generation] + standing");
    assert!(
        quads.iter().all(|q| {
            q.graph_name == GraphName::NamedNode(IntentGraph::ReceiptProof.named_graph_iri())
        }),
        "every projected triple lands in the ReceiptProof intent graph"
    );
    assert!(
        quads
            .iter()
            .any(|q| q.predicate.as_str() == format!("{PROOF_NS}chainHash")
                && q.object
                    .to_string()
                    .contains(&receipt.record.chain_hash_hex)),
        "chain hash predicate carries the real receipt chain hash"
    );

    let report = audit_isolation(&store).expect("clean projection passes the audit");
    assert_eq!(report.proof_subjects, 1);
}

#[test]
fn shared_subject_across_intents_is_typed_leak_refusal() {
    let dir = TempDir::new().expect("tempdir");
    sync_receipt(dir.path());
    let receipt = read_receipt(dir.path());

    let _facts = ReceiptProofFacts::from_sync_receipt(&receipt, "ALIVE");
    let store = DatasetStore::new().expect("store");

    // A real observed-fact subject in SystemState.
    let state_subject = "urn:example:observed-system";
    store
        .insert_quad(IntentGraph::SystemState, &state_quad(state_subject))
        .expect("system-state write");

    // The attack: a ReceiptProof triple minted on the SystemState subject.
    // It must be insertable (each intent-authorized write is individually
    // lawful) yet caught by the isolation audit.
    let leak_quad = Quad::new(
        NamedNode::new_unchecked(state_subject),
        NamedNode::new_unchecked(format!("{PROOF_NS}standing")),
        Literal::new_simple_literal("ALIVE"),
        GraphName::DefaultGraph,
    );
    store
        .insert_quad(IntentGraph::ReceiptProof, &leak_quad)
        .expect("intent-authorized write succeeds (leak is cross-intent, not per-write)");

    let err = audit_isolation(&store).expect_err("shared subject must refuse");
    let ggen_engine::dataset_bridge::BridgeError::Leak(leak) = err else {
        panic!("expected a Leak variant, got a store error");
    };
    assert_eq!(leak.subject, state_subject, "refusal names the subject");
    assert_eq!(leak.leak.authority, IntentGraph::SystemState);
    assert_eq!(leak.leak.target, IntentGraph::ReceiptProof);
    let msg = leak.to_string();
    assert!(
        msg.contains(state_subject)
            && msg.contains("graph:SystemState")
            && msg.contains("graph:ReceiptProof"),
        "refusal message names subject and both intents: {msg}"
    );
}

#[test]
fn digest_is_order_invariant_across_projection_orders() {
    let dir = TempDir::new().expect("tempdir");
    sync_receipt(dir.path());
    let receipt = read_receipt(dir.path());
    let facts = ReceiptProofFacts::from_sync_receipt(&receipt, "ALIVE");

    // Two stores, same facts, different insertion sequences.
    let mut a = DatasetStore::new().expect("store");
    let mut b = DatasetStore::new().expect("store");

    // A: receipt facts first, then an unrelated SystemState fact.
    project_receipt_proof(&mut a, &facts).expect("project a");
    a.insert_quad(IntentGraph::SystemState, &state_quad("urn:example:sys-1"))
        .expect("state a");

    // B: reversed — SystemState fact first, then receipt facts. Also flip
    // the receipt's own field order by re-projecting standing-first via a
    // second facts pass (idempotent set semantics make this a no-op, so B
    // differs from A only in insertion sequence).
    b.insert_quad(IntentGraph::SystemState, &state_quad("urn:example:sys-1"))
        .expect("state b");
    project_receipt_proof(&mut b, &facts).expect("project b");

    let da = a.dataset_digest().expect("digest a");
    let db = b.dataset_digest().expect("digest b");
    assert_eq!(da, db, "digest must be invariant under insertion order");

    // Content-sensitivity control: a different standing must move the digest.
    let mut c = DatasetStore::new().expect("store");
    let stale = ReceiptProofFacts {
        standing: "PARTIAL".to_string(),
        ..facts.clone()
    };
    project_receipt_proof(&mut c, &stale).expect("project c");
    let dc = c.dataset_digest().expect("digest c");
    assert_ne!(dc, db, "changing receipt content must change the digest");
}
