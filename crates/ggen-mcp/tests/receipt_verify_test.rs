//! Chicago TDD for `ggen_receipt_verify` — real project, real sync, real
//! `.ggen-v2/receipt.json` on disk, real tampering.

mod common;

use common::write_frontmatter_project;

use ggen_mcp::tools::{
    receipt_verify::{receipt_verify, ReceiptVerifyParams},
    sync_dry_run::{sync_dry_run, SyncDryRunParams},
    write_apply::{write_apply, WriteApplyParams},
};

/// A clean, untampered receipt chain must verify successfully, with the
/// real chain/payload hashes populated on the result.
#[test]
fn clean_receipt_chain_verifies_successfully() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run");
    write_apply(&WriteApplyParams::new(dir.path().display().to_string(), true, pre.graph_hash))
    .expect("real sync, writes a real receipt");

    assert!(
        dir.path().join(".ggen-v2/receipt.json").exists(),
        "write_apply must have produced a real receipt file"
    );

    let got = receipt_verify(&ReceiptVerifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("receipt_verify tool call itself must not error on a clean receipt");

    assert!(got.valid, "a freshly-written receipt must verify clean");
    assert!(got.error_message.is_none(), "no error on a valid receipt");
    assert!(got.fm_code.is_none(), "no FM code on a valid receipt");
    assert!(
        got.chain_hash.as_deref().is_some_and(|h| !h.is_empty()),
        "chain_hash must be populated on success"
    );
    assert!(
        got.payload_hash.as_deref().is_some_and(|h| !h.is_empty()),
        "payload_hash must be populated on success"
    );
    assert_eq!(
        got.signed,
        Some(true),
        "ggen-engine's keys module generates a signing keypair on first real sync when \
         none is configured (crates/ggen-engine/src/keys.rs) -- a real sync therefore \
         always produces a signed receipt"
    );
    assert_eq!(
        got.signature_valid,
        Some(true),
        "the freshly-generated signing/verifying keypair must round-trip"
    );
}

/// Tampering with the receipt's stored chain hash must be caught: the tool
/// must report `valid:false` (never an `Err` -- the underlying pipeline
/// check itself failed, which is a reportable *result*, not a tool
/// malfunction) with the engine's real, verbatim refusal message.
///
/// This is also the test that pins down this session's real finding:
/// `handle_receipt_verify_in`'s own error paths (read at
/// `ggen-engine/src/verbs/handlers.rs:417-516`) carry **no** `FM-CHAIN-0NN`
/// prefix -- only `handle_receipt_history` (a sibling function this tool
/// does not call) uses `AppError::fm_chain`. So `fm_code` is `None` here;
/// asserting that directly (rather than asserting a code IS present) keeps
/// this test honest about what the wrapped function actually does today.
#[test]
fn tampered_chain_hash_is_reported_as_invalid_with_verbatim_message() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run");
    write_apply(&WriteApplyParams::new(dir.path().display().to_string(), true, pre.graph_hash))
    .expect("real sync, writes a real receipt");

    let receipt_path = dir.path().join(".ggen-v2/receipt.json");
    let raw = std::fs::read_to_string(&receipt_path).expect("read real receipt");
    let value: serde_json::Value = serde_json::from_str(&raw).expect("parse real receipt");

    // Drift-injection: flip one hex character of the stored chain hash so
    // it no longer matches what praxis-core recomputes from the (still
    // payload-hash-consistent) record -- a real tamper, not a synthetic
    // error path.
    //
    // Payload-hash verification (`stored_payload_hash`) hashes the RAW
    // `payload` bytes exactly as they appear in the file (a `RawValue`
    // borrow, not a re-serialization) -- round-tripping the whole document
    // through `serde_json::to_string_pretty` would reformat the payload's
    // whitespace and trip a payload-hash mismatch BEFORE the chain-hash
    // check this test targets even runs. So the tamper is a raw string
    // substitution against the original bytes, touching nothing else.
    let chain_hash = value["record"]["chain_hash_hex"]
        .as_str()
        .expect("receipt record has chain_hash_hex")
        .to_string();
    let mut tampered = chain_hash.clone();
    let flipped_char = if tampered.starts_with('0') { '1' } else { '0' };
    tampered.replace_range(0..1, &flipped_char.to_string());
    assert_ne!(tampered, chain_hash, "tamper must actually change the value");
    let occurrences = raw.matches(chain_hash.as_str()).count();
    assert_eq!(
        occurrences, 1,
        "chain_hash_hex must appear exactly once in the raw receipt for an unambiguous \
         substitution -- got {occurrences} occurrences"
    );
    let tampered_raw = raw.replacen(chain_hash.as_str(), tampered.as_str(), 1);

    std::fs::write(&receipt_path, tampered_raw).expect("write tampered receipt");

    let got = receipt_verify(&ReceiptVerifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("receipt_verify tool call must not itself error -- a failed check is a result");

    assert!(!got.valid, "a tampered chain hash must fail verification");
    let message = got
        .error_message
        .as_deref()
        .expect("a failed verification must carry the engine's real refusal message");
    assert!(
        message.contains("chain hash mismatch"),
        "must be the real chain-hash-mismatch refusal, got: {message:?}"
    );
    assert_eq!(
        got.fm_code, None,
        "handle_receipt_verify_in's own error paths carry no FM-CHAIN prefix today \
         (confirmed by reading handlers.rs:417-516) -- fm_code correctly extracts \
         nothing rather than fabricating a code that isn't in the message"
    );
    assert!(got.chain_hash.is_none(), "no success fields on a failed verification");
}

/// A missing receipt (never synced) must also come back as `valid:false`
/// with a real message, not an `Err` from the tool itself and not a panic.
#[test]
fn missing_receipt_is_reported_as_invalid_not_a_tool_error() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    // Deliberately never sync -- no .ggen-v2/receipt.json exists.

    let got = receipt_verify(&ReceiptVerifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("receipt_verify tool call must not error even when the receipt is missing");

    assert!(!got.valid, "no receipt means nothing to verify");
    let message = got
        .error_message
        .as_deref()
        .expect("a missing receipt must carry a real, readable refusal message");
    assert!(
        message.contains("unreadable"),
        "must be the real 'receipt unreadable' refusal, got: {message:?}"
    );
}
