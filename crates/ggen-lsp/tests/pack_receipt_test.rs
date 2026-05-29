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
