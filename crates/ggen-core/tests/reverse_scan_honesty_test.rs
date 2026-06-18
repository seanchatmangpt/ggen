//! Honesty test for `ggen reverse scan` (ARM 1: code → discovered RDF authority).
//!
//! Chicago TDD: real temp filesystem, real extraction, real RDF. The easiest
//! way to make these pass is to emit a real, parseable authority graph and an
//! honest receipt — faking is harder than the real thing.
//!
//! Proves (per `.claude/rules/coding-agent-mistakes.md`):
//! - real output: the `.ttl` parses in oxigraph with the expected facts (not a stub);
//! - real receipt: input/output hashes are real 64-hex digests (not placeholders);
//! - determinism: identical input → byte-identical `.ttl`;
//! - fail-loud sabotage: missing root / no services → `Err`, never an empty graph;
//! - no stale drift: changing a source file changes its recorded hash.

use std::path::PathBuf;

use ggen_core::graph::Graph;
use ggen_core::reverse::{scan_to_authority, ReverseReceipt};

/// Create a fresh, unique temp project directory with a `src/` subdir.
fn temp_project() -> PathBuf {
    let root =
        std::env::temp_dir().join(format!("ggen_reverse_scan_{}", uuid::Uuid::new_v4()));
    std::fs::create_dir_all(root.join("src")).expect("create src dir");
    root
}

fn is_hex64(s: &str) -> bool {
    s.len() == 64 && s.chars().all(|c| c.is_ascii_hexdigit())
}

#[test]
fn scan_emits_parseable_authority_graph_and_honest_receipt() {
    // Arrange: a real Rust source file with one struct.
    let proj = temp_project();
    std::fs::write(
        proj.join("src").join("lib.rs"),
        "pub struct Foo { id: u64, name: String }\n",
    )
    .expect("write source");

    // Act: real scan over the real filesystem.
    let report = scan_to_authority(&[proj.join("src")], &proj, "demo").expect("scan succeeds");

    // The authority graph exists, is non-empty, and carries real facts.
    assert!(report.authority_ttl.exists(), "authority ttl must exist");
    assert_eq!(report.services, 1, "exactly one service discovered");
    assert!(report.triples > 0, "graph must contain triples");
    let ttl = std::fs::read_to_string(&report.authority_ttl).expect("read ttl");
    assert!(ttl.contains("disco:Foo"), "service node present: {ttl}");
    assert!(ttl.contains("code:language \"rust\""), "language fact present");
    assert!(ttl.contains("disco:sourceHash"), "provenance hash present");
    assert!(ttl.contains("\"id\""), "field id present");
    assert!(ttl.contains("\"name\""), "field name present");
    assert!(
        !ttl.to_lowercase().contains("todo") && !ttl.contains("placeholder"),
        "no stub/placeholder markers"
    );

    // The strongest anti-stub proof: oxigraph parses it as real RDF.
    let graph = Graph::load_from_string(&ttl).expect("authority ttl must parse as real RDF");
    // And it is queryable for the discovered service.
    let _ = graph; // parse success is the gate; ARM 3 exercises queries.

    // The receipt exists and is honest.
    assert!(report.receipt_path.exists(), "receipt must exist");
    let raw = std::fs::read_to_string(&report.receipt_path).expect("read receipt");
    let receipt: ReverseReceipt = serde_json::from_str(&raw).expect("parse receipt");
    assert_eq!(receipt.operation, "reverse-scan");
    let id = uuid::Uuid::parse_str(&receipt.operation_id).expect("operation_id is a uuid");
    assert_ne!(id, uuid::Uuid::nil(), "operation_id must not be nil");
    // Every claimed input/output hash is a real 64-hex digest.
    assert!(
        receipt.input_hashes.contains_key("src:src/lib.rs"),
        "receipt binds the source file: {:?}",
        receipt.input_hashes
    );
    for (k, v) in receipt.input_hashes.iter().chain(receipt.output_hashes.iter()) {
        assert!(is_hex64(v), "hash for {k} must be 64-hex, got {v}");
    }
    assert!(
        !receipt.output_hashes.is_empty(),
        "receipt must list the output ttl"
    );

    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn scan_is_deterministic() {
    let src = "pub struct Account { id: u64, balance: u64 }\npub struct User { name: String }\n";

    let proj_a = temp_project();
    std::fs::write(proj_a.join("src").join("lib.rs"), src).expect("write a");
    let rep_a = scan_to_authority(&[proj_a.join("src")], &proj_a, "k").expect("scan a");

    let proj_b = temp_project();
    std::fs::write(proj_b.join("src").join("lib.rs"), src).expect("write b");
    let rep_b = scan_to_authority(&[proj_b.join("src")], &proj_b, "k").expect("scan b");

    let ttl_a = std::fs::read_to_string(&rep_a.authority_ttl).expect("read a");
    let ttl_b = std::fs::read_to_string(&rep_b.authority_ttl).expect("read b");
    assert_eq!(
        ttl_a, ttl_b,
        "identical input must produce byte-identical authority graphs"
    );

    let _ = std::fs::remove_dir_all(&proj_a);
    let _ = std::fs::remove_dir_all(&proj_b);
}

#[test]
fn missing_root_fails_loudly() {
    let proj = temp_project();
    let missing = proj.join("does-not-exist");
    let result = scan_to_authority(&[missing], &proj, "x");
    assert!(
        result.is_err(),
        "a non-existent root must error, not emit an empty graph"
    );
    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn no_services_refuses_empty_graph() {
    // A tree with no extractable services must NOT yield a decorative empty .ttl.
    let proj = temp_project();
    std::fs::write(proj.join("src").join("notes.txt"), "just prose, no code\n").expect("write");
    let result = scan_to_authority(&[proj.join("src")], &proj, "x");
    assert!(
        result.is_err(),
        "no discovered services must fail loudly (no empty authority graph)"
    );
    assert!(
        !proj.join(".specify/discovered/x.ttl").exists(),
        "no .ttl may be written when nothing was discovered"
    );
    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn changing_source_changes_recorded_hash() {
    // Sabotage / anti-drift: the receipt must reflect the current source, never a
    // stale prior run.
    let proj = temp_project();
    let src_file = proj.join("src").join("lib.rs");

    std::fs::write(&src_file, "pub struct Foo { id: u64 }\n").expect("write v1");
    let rep1 = scan_to_authority(&[proj.join("src")], &proj, "v").expect("scan v1");
    let raw1 = std::fs::read_to_string(&rep1.receipt_path).expect("read receipt v1");
    let receipt1: ReverseReceipt = serde_json::from_str(&raw1).expect("parse v1");
    let hash1 = receipt1.input_hashes["src:src/lib.rs"].clone();

    std::fs::write(&src_file, "pub struct Foo { id: u64, extra: bool }\n").expect("write v2");
    let rep2 = scan_to_authority(&[proj.join("src")], &proj, "v").expect("scan v2");
    let raw2 = std::fs::read_to_string(&rep2.receipt_path).expect("read receipt v2");
    let receipt2: ReverseReceipt = serde_json::from_str(&raw2).expect("parse v2");
    let hash2 = receipt2.input_hashes["src:src/lib.rs"].clone();

    assert_ne!(
        hash1, hash2,
        "a changed source file must change its recorded input hash"
    );

    let _ = std::fs::remove_dir_all(&proj);
}
