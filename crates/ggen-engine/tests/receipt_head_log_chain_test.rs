//! A19 follow-up 4: `ggen receipt verify` must bind the head to the WHOLE
//! receipt log, fail-closed. Truncated, reordered, missing, or emptied logs
//! and a tampered head are refused with typed `[FM-CHAIN-*]` errors.
//!
//! Chicago-TDD: real TempDir project, real syncs, real file edits, the real
//! verifier.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, RECEIPT_LOG_REL_PATH, RECEIPT_REL_PATH};
use ggen_engine::verbs::handlers::handle_receipt_verify_in;
use tempfile::TempDir;

const GGEN_TOML: &str = "[project]\nname = \"head-log-chain\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n";
const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn project_with_syncs(n: usize) -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).unwrap();
    std::fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .unwrap();
    std::fs::create_dir_all(root.join("templates")).unwrap();
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).unwrap();
    for i in 0..n {
        sync(root, SyncOptions::default()).unwrap_or_else(|e| panic!("sync {i}: {e}"));
    }
    dir
}

fn log_lines(root: &Path) -> Vec<String> {
    std::fs::read_to_string(root.join(RECEIPT_LOG_REL_PATH))
        .unwrap()
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(String::from)
        .collect()
}

fn write_log(root: &Path, lines: &[String]) {
    std::fs::write(root.join(RECEIPT_LOG_REL_PATH), lines.join("\n") + "\n").unwrap();
}

#[test]
fn intact_three_record_log_verifies() {
    let dir = project_with_syncs(3);
    assert!(log_lines(dir.path()).len() >= 3, "need >=3 log records");
    let out = handle_receipt_verify_in(dir.path()).expect("intact chain verifies");
    assert_eq!(out["log_head_match"], serde_json::json!(true));
    assert_eq!(out["log_chain_verified"], serde_json::json!(true));
}

#[test]
fn reordered_log_is_refused() {
    let dir = project_with_syncs(3);
    let root = dir.path();
    let mut lines = log_lines(root);
    let n = lines.len();
    // Swap two non-tail records: tail (== head) is untouched.
    lines.swap(n - 3, n - 2);
    write_log(root, &lines);
    let err = handle_receipt_verify_in(root).expect_err("reordered log must be refused");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}

#[test]
fn log_truncated_at_head_is_refused() {
    let dir = project_with_syncs(3);
    let root = dir.path();
    let mut lines = log_lines(root);
    lines.pop(); // head now points at a record the log no longer has
    write_log(root, &lines);
    let err = handle_receipt_verify_in(root).expect_err("truncated log must be refused");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}

#[test]
fn log_with_dropped_middle_record_is_refused() {
    let dir = project_with_syncs(3);
    let root = dir.path();
    let mut lines = log_lines(root);
    let n = lines.len();
    lines.remove(n - 2);
    write_log(root, &lines);
    let err = handle_receipt_verify_in(root).expect_err("broken link must be refused");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}

#[test]
fn tampered_head_is_refused() {
    let dir = project_with_syncs(2);
    let root = dir.path();
    let head_path = root.join(RECEIPT_REL_PATH);
    let raw = std::fs::read_to_string(&head_path).unwrap();
    let mut v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    v["record"]["prev_chain_hash_hex"] = serde_json::json!("a".repeat(64));
    std::fs::write(&head_path, serde_json::to_string(&v).unwrap()).unwrap();
    let err = handle_receipt_verify_in(root).expect_err("tampered head must be refused");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}

#[test]
fn missing_log_is_refused_not_skipped() {
    let dir = project_with_syncs(1);
    let root = dir.path();
    std::fs::remove_file(root.join(RECEIPT_LOG_REL_PATH)).unwrap();
    let err = handle_receipt_verify_in(root).expect_err("missing log must fail closed");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}

#[test]
fn empty_log_is_refused_not_skipped() {
    let dir = project_with_syncs(1);
    let root = dir.path();
    std::fs::write(root.join(RECEIPT_LOG_REL_PATH), "").unwrap();
    let err = handle_receipt_verify_in(root).expect_err("empty log must fail closed");
    assert!(err.to_string().contains("FM-CHAIN"), "typed error: {err}");
}
