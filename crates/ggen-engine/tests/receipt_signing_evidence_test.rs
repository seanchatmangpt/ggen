//! G6 evidence: real receipt-chain and `ggen receipt verify` proofs not
//! already covered by `tests/receipt_chain_e2e.rs`. Real filesystem
//! (`tempfile::TempDir`), real `sync()` calls, real `ggen` binary
//! subprocess via `chicago_tdd_tools::cli_proof::CliHarness` — no mocks.
//!
//! Scope, deliberately distinct from `receipt_chain_e2e.rs`:
//! 1. A fresh two-sync chain proving the second record's
//!    `prev_chain_hash_hex` equals the first's `chain_hash_hex` (the
//!    mission's own "real chain linkage, not assumed" checkpoint).
//! 2. `receipt verify` on a genuinely valid, real (non-hand-built) receipt
//!    — positive witness.
//! 3. `receipt verify` catches a tampered `payload_hash_hex` specifically
//!    (step 1 of `handle_receipt_verify`'s checks) — not exercised by the
//!    existing chain-hash/signature tamper tests in `receipt_chain_e2e.rs`.
//! 4. `receipt verify` refuses a receipt whose `version` field doesn't
//!    match `RECEIPT_RECORD_VERSION` — the schema-version guard ahead of
//!    any hash/signature check.

use std::path::Path;

use chicago_tdd_tools::cli_proof::CliHarness;
use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "evidence-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path, names: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    write_ontology(root, names);
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn write_ontology(root: &Path, names: &[&str]) {
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
}

fn read_receipt(root: &Path) -> SyncReceipt {
    serde_json::from_str(
        &std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt.json"),
    )
    .expect("parse receipt.json")
}

/// Positive witness: two real syncs, real BLAKE3 chain linkage — the
/// second record's `prev_chain_hash_hex` is bitwise-equal to the first's
/// `chain_hash_hex`, read from receipt.json (the head pointer), not
/// assumed from in-memory state.
#[test]
fn second_sync_prev_chain_hash_equals_first_sync_chain_hash() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 1 must succeed");
    let first = read_receipt(dir.path());
    assert_eq!(
        first.record.prev_chain_hash_hex,
        "0".repeat(64),
        "genesis sync must chain from the zero root"
    );

    write_ontology(dir.path(), &["alice", "bob"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 2 must succeed");
    let second = read_receipt(dir.path());

    assert_eq!(
        second.record.prev_chain_hash_hex, first.record.chain_hash_hex,
        "second sync must chain onto the exact first-sync chain_hash_hex, not a recomputed or default value"
    );
    assert_ne!(
        second.record.chain_hash_hex, first.record.chain_hash_hex,
        "distinct sync content must produce a distinct chain hash"
    );
}

/// Positive witness: `receipt verify` on an untouched, real sync's
/// receipt reports overall validity plus a real chain-hash recompute
/// match (not just an unconditional "success" exit).
#[test]
fn receipt_verify_passes_on_genuine_untampered_receipt() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("verify genuine receipt")
        .assert_success()
        .assert_stdout_json_field("valid", "true")
        .assert_stdout_json_field("signed", "true");
}

/// Negative falsifier: corrupting `payload_hash_hex` alone (chain hash and
/// signature untouched) must be caught at the payload-binding check
/// specifically, distinct from the chain-hash and signature failure
/// messages already proven in `receipt_chain_e2e.rs`.
#[test]
fn receipt_verify_fails_closed_on_tampered_payload_hash() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let receipt_path = dir.path().join(RECEIPT_REL_PATH);
    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt");
    let record: SyncReceipt = serde_json::from_str(&raw).expect("parse");
    let orig = record.record.payload_hash_hex.clone();
    let flipped = if orig.starts_with('f') {
        format!("e{}", &orig[1..])
    } else {
        format!("f{}", &orig[1..])
    };
    assert_eq!(
        raw.matches(&orig).count(),
        1,
        "payload_hash_hex must appear exactly once so substring substitution is unambiguous"
    );
    std::fs::write(&receipt_path, raw.replace(&orig, &flipped)).expect("tamper payload hash");

    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("verify tampered payload hash")
        .assert_failure()
        .assert_stderr_contains("payload hash mismatch");
}

/// Negative falsifier: a receipt whose `version` field does not match the
/// binary's `RECEIPT_RECORD_VERSION` is refused before any hash or
/// signature check runs (guard clause 0 in `handle_receipt_verify`).
#[test]
fn receipt_verify_refuses_unsupported_schema_version() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let receipt_path = dir.path().join(RECEIPT_REL_PATH);
    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt");
    let mut doc: serde_json::Value = serde_json::from_str(&raw).expect("parse json");
    let bumped = doc["record"]["version"].as_u64().expect("version is a u64") + 1000;
    doc["record"]["version"] = serde_json::Value::from(bumped);
    std::fs::write(
        &receipt_path,
        serde_json::to_string(&doc).expect("reserialize"),
    )
    .expect("write bumped-version receipt");

    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("verify unsupported version")
        .assert_failure()
        .assert_stderr_contains("unsupported receipt schema version");
}
