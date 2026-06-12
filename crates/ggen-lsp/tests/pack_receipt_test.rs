#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! CPMP-PACK-1 — a capability scan manufactures the pack, with a replayable,
//! tamper-evident scan→pack receipt.

use ggen_lsp::{emit_pack, verify_pack, PackOptions};
use tempfile::TempDir;

#[test]
fn pack_binds_scan_to_pack_then_tamper_breaks_verification() {
    let dir = TempDir::new().expect("tempdir");
    let out = dir.path().join(".agent-admissibility");

    let report = emit_pack(&PackOptions {
        agents: vec!["generic".to_string()],
        out_dir: out.clone(),
        scan_hash: Some("scan-aggregate-abc123".to_string()),
    })
    .expect("emit");

    assert!(!report.pack_hash.is_empty(), "pack content hash computed");
    assert!(report.receipt_sig.is_some(), "scan→pack receipt emitted");
    assert!(
        out.join("pack-provenance.json").is_file(),
        "provenance written"
    );

    // A freshly emitted pack reconstructs its scan→pack binding.
    let v = verify_pack(&out);
    assert!(v.matches, "fresh pack verifies: {}", v.reason);

    // Tampering any pack file changes the recomputed content hash → no match.
    std::fs::write(out.join("README.md"), "tampered content").expect("tamper");
    let v2 = verify_pack(&out);
    assert!(
        !v2.matches,
        "tampered pack must not verify (reason: {})",
        v2.reason
    );
}

#[test]
fn pack_without_scan_still_has_a_verifiable_self_receipt() {
    let dir = TempDir::new().expect("tempdir");
    let out = dir.path().join(".agent-admissibility");
    emit_pack(&PackOptions {
        agents: vec!["generic".to_string()],
        out_dir: out.clone(),
        scan_hash: None,
    })
    .expect("emit");
    assert!(
        verify_pack(&out).matches,
        "an unbound pack still binds its own content hash"
    );
}
