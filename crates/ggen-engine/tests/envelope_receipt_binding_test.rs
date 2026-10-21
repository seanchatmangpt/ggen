//! Envelope→receipt binding (v26.10.20, lane A19): the sync receipt's input
//! closure must bind the envelope ontology file's bytes by BLAKE3 so that a
//! resource-cap change (memory/CPU caps on a `res:Envelope` individual)
//! invalidates the receipt chain — a cap change is a governing-input change
//! and must never replay under the old receipt.
//!
//! Chicago-TDD: real tempdir project, real five-stage sync, real BLAKE3
//! recomputation, the real chain verifier (`handle_receipt_history_in`) and
//! the real single-receipt verifier (`handle_receipt_verify_in`). No mocks.
//!
//! Seam disclosure: the CapTable→receipt *field* wiring in sync.rs is A05's
//! lane; this test proves the binding that already exists on the closure
//! path (the envelope TTL file is hashed into `payload.closure`), so the
//! cap change provably forks the receipt chain today. When A05 lands the
//! dedicated envelope receipt field, this file is where its court belongs.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH, RECEIPT_REL_PATH};
use ggen_engine::verbs::handlers::{handle_receipt_history_in, handle_receipt_verify_in};
use tempfile::TempDir;

/// Envelope vocabulary namespace (must mirror `crate::envelope::ENVELOPE_NS`
/// and `.specify/envelope-shapes.ttl`; kept literal so the test fails loudly
/// if the namespace drifts).
const ENVELOPE_NS: &str = "http://ggen.org/resource-envelope#";

const GGEN_TOML: &str = r#"
[project]
name = "envelope-binding"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// Ontology carrying one fact row AND one declared `res:Envelope` individual
/// with explicit cap values. `memory_bytes` is the mutation surface: the
/// second sync edits only this number, so any receipt difference is
/// attributable to the envelope cap, not to output drift.
fn ontology_ttl(memory_bytes: u64) -> String {
    format!(
        "@prefix ex: <http://example.org/> .\n\
         @prefix res: <{ENVELOPE_NS}> .\n\
         ex:alice ex:name \"alice\" .\n\
         ex:gen-budget a res:Envelope ;\n\
         \x20 res:resource ex:generation ;\n\
         \x20 res:memoryBytes {memory_bytes} ;\n\
         \x20 res:cpuMs 1000 .\n"
    )
}

fn scaffold(root: &Path, memory_bytes: u64) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ontology_ttl(memory_bytes)).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn read_receipt(root: &Path) -> SyncReceipt {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt.json");
    serde_json::from_str(&raw).expect("parse receipt.json")
}

fn read_log(root: &Path) -> Vec<SyncReceipt> {
    let raw = std::fs::read_to_string(root.join(RECEIPT_LOG_REL_PATH)).expect("read log");
    raw.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("parse log line"))
        .collect()
}

fn closure_ontology_hash(receipt: &SyncReceipt) -> &str {
    receipt
        .payload
        .closure
        .get("ontology.ttl")
        .expect("receipt closure must bind ontology.ttl")
}

/// The core law: the sync receipt's closure includes the envelope ontology
/// file's BLAKE3, so a cap change between two syncs changes the closure
/// hash, the payload, and the chain hash — the old receipt can no longer be
/// the verified head, i.e. it fails replay against the current tree.
#[test]
fn envelope_cap_change_forks_the_receipt_chain() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Sync 1: cap = 1 GiB.
    scaffold(root, 1_073_741_824);
    sync(root, SyncOptions::default()).expect("sync 1 must succeed");
    let receipt1_bytes =
        std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt 1");
    let receipt1 = read_receipt(root);

    // Closure binding is a real BLAKE3: 64 hex chars, and it equals the
    // BLAKE3 of the ontology bytes on disk (state-based, not shape-only).
    let closure_hash1 = closure_ontology_hash(&receipt1);
    assert_eq!(
        closure_hash1.len(),
        64,
        "closure ontology binding must be a BLAKE3 hex digest, got {closure_hash1:?}"
    );
    let expected = blake3::hash(&std::fs::read(root.join("ontology.ttl")).expect("read ttl"))
        .to_hex()
        .to_string();
    assert_eq!(
        closure_hash1, &expected,
        "closure binding must be the envelope ontology file's BLAKE3"
    );

    // Mutate ONLY the envelope cap value in the TTL, then sync again.
    std::fs::write(root.join("ontology.ttl"), ontology_ttl(2_147_483_648))
        .expect("rewrite ontology with new cap");
    sync(root, SyncOptions::default()).expect("sync 2 must succeed");

    let receipt2_bytes =
        std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt 2");
    let receipt2 = read_receipt(root);

    // Receipt must differ — closure binding, payload, and chain hash.
    assert_ne!(
        closure_ontology_hash(&receipt2),
        closure_hash1,
        "cap change must change the closure's envelope-ontology BLAKE3"
    );
    assert_ne!(
        receipt2_bytes, receipt1_bytes,
        "receipt.json must differ after a cap change"
    );
    assert_ne!(
        receipt2.record.chain_hash_hex, receipt1.record.chain_hash_hex,
        "chain hash must fork after a cap change"
    );

    // The two-entry chain (old cap → new cap) verifies as a whole.
    handle_receipt_history_in(root).expect("both chained receipts must verify");

    // Non-vacuity control: the verifier admits the TRUE head (receipt 2,
    // current caps) before the restore — the refusal below is not a vacuous
    // verifier-always-fails artifact.
    let verified = handle_receipt_verify_in(root).expect("current head must verify");
    assert_eq!(
        verified["chain_hash"], receipt2.record.chain_hash_hex,
        "verifier must report the current head's chain hash"
    );

    // Old receipt fails replay: restored as receipt.json it is no longer the
    // verified head — its chain hash diverges from the log tail, so replaying
    // the old receipt against the post-cap-change tree is refused by the
    // head-vs-log contract (the verifier's own output names the stale hash).
    std::fs::write(root.join(RECEIPT_REL_PATH), &receipt1_bytes).expect("restore old receipt");
    let log = read_log(root);
    assert_eq!(log.len(), 2, "two syncs must have appended two log entries");
    assert_ne!(
        receipt1.record.chain_hash_hex,
        log.last().expect("non-empty log").record.chain_hash_hex,
        "restored old receipt must fail replay: it is not the log head"
    );
    // (lane G04 closed A19's gap): the stale head is now REFUSED by the
    // head-vs-log check — a typed error naming BOTH the head and the
    // log-tail chain hash.
    let stale_err = handle_receipt_verify_in(root)
        .expect_err("stale head must be refused by the head-vs-log contract");
    let stale_msg = stale_err.to_string();
    assert!(
        stale_msg.contains(&receipt1.record.chain_hash_hex)
            && stale_msg.contains(&log.last().expect("non-empty log").record.chain_hash_hex),
        "head-vs-log refusal must name BOTH the head hash and the log-tail hash, got: {stale_msg}"
    );
}

/// Negative control for the positive law above: two syncs with the SAME
/// envelope caps must NOT fork the chain — the binding must be
/// cap-sensitive, not sync-count-sensitive (determinism side of the law).
#[test]
fn unchanged_envelope_caps_replay_identically() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, 1_073_741_824);
    sync(root, SyncOptions::default()).expect("sync 1 must succeed");
    let receipt1_bytes =
        std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt 1");

    // Byte-identical ontology rewrite (same caps), then sync again.
    std::fs::write(root.join("ontology.ttl"), ontology_ttl(1_073_741_824))
        .expect("rewrite ontology unchanged");
    sync(root, SyncOptions::default()).expect("sync 2 must succeed");

    let receipt2 = read_receipt(root);
    assert_eq!(
        closure_ontology_hash(&receipt2),
        blake3::hash(&std::fs::read(root.join("ontology.ttl")).expect("read ttl"))
            .to_hex()
            .to_string(),
        "closure binding must track the on-disk envelope ontology bytes"
    );
    handle_receipt_history_in(root).expect("identical-input chain must verify");

    // The first receipt remains a valid historical link: restoring it as the
    // head still diverges (the log advanced), but the receipt's OWN payload
    // hash must be identical to the second sync's payload binding for the
    // unchanged envelope — same closure hash as receipt 2.
    let receipt1: SyncReceipt = serde_json::from_str(&receipt1_bytes).expect("parse receipt 1");
    assert_eq!(
        closure_ontology_hash(&receipt1),
        closure_ontology_hash(&receipt2),
        "unchanged envelope caps must leave the closure binding identical"
    );
}
