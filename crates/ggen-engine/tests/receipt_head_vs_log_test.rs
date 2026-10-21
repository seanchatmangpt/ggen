//! Head-vs-log receipt verification (v26.10.20, lane G04 closing A19's gap):
//! `ggen receipt verify` must not only check the receipt's internal
//! chain/signature — it must also compare the receipt head (`.ggen-v2/
//! receipt.json`) against the receipt-log tail (`.ggen-v2/receipt-log.jsonl`).
//!
//! Laws under test:
//! 1. A head whose chain hash equals the log tail verifies with
//!    `log_head_match: true` (+ `log_tail_generation` when the tail carries
//!    a generation epoch).
//! 2. A head that diverges from the log tail (stale restore, tampered
//!    head/log) is refused with a typed error naming BOTH hashes.
//! 3. A missing or empty log file is a LOUD SKIP in the output (`log_check`
//!    naming the skip), never a silent pass — receipts predating the log.
//!
//! Chicago-TDD: real tempdir project, real five-stage sync, real log
//! appends, the real verifier. No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH, RECEIPT_REL_PATH};
use ggen_engine::verbs::handlers::handle_receipt_verify_in;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "head-vs-log"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn ontology_ttl(name: &str) -> String {
    format!("@prefix ex: <http://example.org/> .\nex:{name} ex:name \"{name}\" .\n")
}

fn scaffold(root: &Path, name: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ontology_ttl(name)).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn read_receipt(root: &Path) -> SyncReceipt {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt.json");
    serde_json::from_str(&raw).expect("parse receipt.json")
}

/// Law 1: matching head verifies with `log_head_match: true` and the tail's
/// generation epoch.
#[test]
fn matching_head_reports_log_head_match_with_tail_generation() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, "alice");
    sync(root, SyncOptions::default()).expect("sync 1 must succeed");
    sync(root, SyncOptions::default()).expect("sync 2 must succeed (advances the log)");

    let receipt = read_receipt(root);
    let log_raw = std::fs::read_to_string(root.join(RECEIPT_LOG_REL_PATH)).expect("read log");
    let tail: SyncReceipt = serde_json::from_str(
        log_raw
            .lines()
            .rev()
            .find(|l| !l.trim().is_empty())
            .expect("non-empty log"),
    )
    .expect("parse log tail");

    assert_eq!(
        receipt.record.chain_hash_hex, tail.record.chain_hash_hex,
        "head must BE the log tail after a real sync"
    );

    let out = handle_receipt_verify_in(root).expect("matching head must verify");
    assert_eq!(out["valid"], serde_json::json!(true));
    assert_eq!(
        out["log_head_match"],
        serde_json::json!(true),
        "verifier must report the head-vs-log match explicitly"
    );
    if let Some(gen) = tail.payload.generation {
        assert_eq!(
            out["log_tail_generation"],
            serde_json::json!(gen),
            "verifier must report the log-tail generation epoch"
        );
    }
}

/// Law 2: a head diverging from the log tail (stale restore) is refused with
/// a typed error naming BOTH the head and the tail chain hashes.
#[test]
fn tampered_log_tail_refused_naming_both_hashes() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, "alice");
    sync(root, SyncOptions::default()).expect("sync 1");
    let head = read_receipt(root);

    // Tamper ONLY the log tail's chain hash (head untouched): the head no
    // longer matches the log.
    let log_path = root.join(RECEIPT_LOG_REL_PATH);
    let log_raw = std::fs::read_to_string(&log_path).expect("read log");
    let mut tail_line = log_raw
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .expect("non-empty log")
        .to_string();
    let tail: SyncReceipt = serde_json::from_str(&tail_line).expect("parse tail before tamper");
    let forged = format!("{:0>64}", "f");
    assert_ne!(
        forged, tail.record.chain_hash_hex,
        "forged hash must differ from the real tail hash"
    );
    tail_line = tail_line.replace(&tail.record.chain_hash_hex, &forged);
    let mut new_log: Vec<String> = log_raw
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(String::from)
        .collect();
    let last = new_log.len() - 1;
    new_log[last] = tail_line;
    std::fs::write(&log_path, new_log.join("\n") + "\n").expect("write tampered log");

    let err = handle_receipt_verify_in(root)
        .expect_err("head must be refused when the log tail diverges");
    let msg = err.to_string();
    assert!(
        msg.contains(&head.record.chain_hash_hex) && msg.contains(&forged),
        "refusal must name BOTH the head hash and the (tampered) log-tail hash, got: {msg}"
    );
}

/// Law 2b: a stale head (old receipt restored over a progressed log) is
/// refused the same way — the tampered-log court's mirror image.
#[test]
fn stale_head_restored_over_progressed_log_refused() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, "alice");
    sync(root, SyncOptions::default()).expect("sync 1");
    let head1_bytes = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt 1");
    let head1: SyncReceipt = serde_json::from_str(&head1_bytes).expect("parse receipt 1");

    sync(root, SyncOptions::default()).expect("sync 2 (log advances past receipt 1)");

    // Restore receipt 1 as the head: log tail is now receipt 2.
    std::fs::write(root.join(RECEIPT_REL_PATH), &head1_bytes).expect("restore stale head");

    let err = handle_receipt_verify_in(root).expect_err("stale head must be refused");
    let msg = err.to_string();
    assert!(
        msg.contains(&head1.record.chain_hash_hex),
        "refusal must name the stale head hash, got: {msg}"
    );
}

/// Law 3: missing log file = loud SKIP in the output, not a silent pass.
#[test]
fn missing_log_loud_skip_not_silent_pass() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, "alice");
    sync(root, SyncOptions::default()).expect("sync 1");
    // Remove the log entirely (receipt predates the log / log removed).
    std::fs::remove_file(root.join(RECEIPT_LOG_REL_PATH)).expect("remove log");

    let out = handle_receipt_verify_in(root).expect("internal checks still pass");
    assert_eq!(out["valid"], serde_json::json!(true));
    let skip = out["log_check"]
        .as_str()
        .expect("missing log must produce a LOUD skip in the output, not silence");
    assert!(
        skip.contains("SKIP") && skip.contains(RECEIPT_LOG_REL_PATH),
        "skip line must name the skip and the log path, got: {skip:?}"
    );
    assert!(
        out.get("log_head_match").is_none(),
        "log_head_match must be absent when no comparison ran"
    );
}

/// Law 3b: empty log file = loud SKIP, same contract as a missing log.
#[test]
fn empty_log_loud_skip_not_silent_pass() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    scaffold(root, "alice");
    sync(root, SyncOptions::default()).expect("sync 1");
    std::fs::write(root.join(RECEIPT_LOG_REL_PATH), "").expect("empty the log");

    let out = handle_receipt_verify_in(root).expect("internal checks still pass");
    assert_eq!(out["valid"], serde_json::json!(true));
    let skip = out["log_check"]
        .as_str()
        .expect("empty log must produce a LOUD skip in the output, not silence");
    assert!(
        skip.contains("SKIP"),
        "skip line must be explicit, got: {skip:?}"
    );
}
