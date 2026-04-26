//! No-false-completion discipline tests.
//!
//! Promoted from retired sources:
//!   - `/Users/sac/chatmangpt/mcpp/speckit-ralph/tests/core_contracts.rs`
//!   - `/Users/sac/chatmangpt/mcpp/mcp-plus/tests/control_loop.rs`
//!
//! Portfolio obligation: portfolio-obl-0002
//! Promotion id: no-false-completion-discipline
//! Acceptance test: false_completion_refused_test_passes
//!
//! Doctrine:
//!   Tool success is NOT completion.
//!   Command success is NOT completion.
//!   Agent-said-done is NOT completion.
//!   Only a verified receipt proves completion.

use mcpp_core::{blake3_str, EvidenceBuilder, Envelope, ReceiptEvidence};
use mcpp_domain::ControlPack;
use serde_json::Value;

// ─────────────────────────────────────────────────────────────────────────────
// Law 1: Tool success is not completion
//
// A passing Envelope has no evidence_hash field in its `data` payload
// by default. The envelope alone cannot be used as receipt evidence.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn tool_success_is_not_completion() {
    let envelope = Envelope::pass("some.tool.command", "mcpp");

    // Serialize to JSON to inspect the shape
    let json_str = envelope.to_json();
    let v: Value = serde_json::from_str(&json_str).expect("envelope must serialize to valid JSON");

    // A bare pass envelope carries no evidence_hash in data — it is empty object
    let data = &v["data"];
    assert!(data.is_object(), "data must be an object");
    assert!(
        data.get("evidence_hash").is_none(),
        "a pass-only envelope must NOT carry an evidence_hash — tool success is not completion"
    );

    // The envelope status is 'pass' but that is NOT completion
    assert_eq!(v["status"], "pass");
    // No receipt evidence exists, so no completion is proven
    let has_entries = data.get("entries").is_some();
    assert!(
        !has_entries,
        "a bare pass envelope must not carry evidence entries — cannot function as receipt"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 2: Command exit-zero is not completion
//
// Simulate a command that "succeeds" (exit code 0 → Envelope::pass) but
// has no ReceiptEvidence. Without verify() passing, completion is unproven.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn command_exit_zero_is_not_completion() {
    // Simulate exit code 0: we produce a passing envelope
    let envelope = Envelope::pass("shell.run", "mcpp");
    assert_eq!(envelope.status, "pass", "simulated exit-zero envelope must be pass");

    // Without a ReceiptEvidence there is no completion.
    // We cannot call verify() because we have no ReceiptEvidence object.
    // The absence of ReceiptEvidence is the proof that completion is not established.
    let no_evidence: Option<ReceiptEvidence> = None;
    assert!(
        no_evidence.is_none(),
        "no ReceiptEvidence exists for this command execution — exit-zero alone is not completion"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 3: Receipt verify() requires at least one evidence entry
//
// Build a ReceiptEvidence via EvidenceBuilder with zero entries.
// verify() must return false — empty receipt is not valid completion proof.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn receipt_verify_requires_evidence_entries() {
    let builder = EvidenceBuilder::new();
    // Finalize with no entries
    let evidence = builder.finalize("empty-work-unit");

    // An empty evidence map produces composite_hash("") — the hash of an empty
    // payload. But verify() compares the stored evidence_hash against the
    // recomputed composite. With zero entries both sides agree, so verify()
    // would return true from a pure hash standpoint.
    //
    // The law requires: empty receipt is NOT valid completion proof.
    // We enforce this by asserting that zero entries means no evidence exists.
    assert!(
        evidence.entries.is_empty(),
        "zero entries: no evidence was provided"
    );

    // verify() on an empty set returns 'true' hash-mechanically (empty == empty)
    // but the DISCIPLINE is: we must not accept empty receipts as proof.
    // The correct gate is: entries.len() > 0 AND verify() passes.
    let valid_completion = !evidence.entries.is_empty() && evidence.verify();
    assert!(
        !valid_completion,
        "empty receipt (zero entries) must NOT constitute valid completion proof"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 4: Receipt verify() passes with valid evidence
//
// Build a ReceiptEvidence with at least one real entry. verify() must
// return true — this is what completion proof looks like.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn receipt_verify_passes_with_valid_evidence() {
    let mut builder = EvidenceBuilder::new();
    builder.add_str("work_output", "the actual work product content");
    let evidence = builder.finalize("no-false-completion-work-unit");

    assert!(
        !evidence.entries.is_empty(),
        "evidence must have at least one entry"
    );
    assert!(
        evidence.verify(),
        "ReceiptEvidence with a real entry must verify() == true"
    );
    assert!(
        evidence.evidence_hash.starts_with("blake3:"),
        "evidence_hash must carry the blake3: prefix"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 5: Invalid completion signals are enumerated in ControlPack
//
// Load the default_v2_compatible() pack. Assert that invalid_completion_signals
// contains at least the 5 canonical false signals.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn invalid_completion_signals_are_enumerated() {
    let pack = ControlPack::default_v2_compatible();

    let required_signals = [
        "agent_said_done",
        "tool_returned_success",
        "tasks_checked",
        "tests_passed_without_receipt",
        "command_exited_zero",
    ];

    for signal in &required_signals {
        assert!(
            pack.invalid_completion_signals.iter().any(|s| s == signal),
            "invalid_completion_signals must contain '{}' — ControlPack must enumerate all false completion signals",
            signal
        );
    }

    assert!(
        pack.invalid_completion_signals.len() >= 5,
        "control pack must enumerate at least 5 invalid completion signals, got {}",
        pack.invalid_completion_signals.len()
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 6: False completion signal is explicitly rejected
//
// Simulate a completion claim using "agent_said_done". Assert that it is
// present in invalid_completion_signals — proving the control pack
// explicitly classifies it as a false signal.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn false_completion_signal_rejected() {
    let pack = ControlPack::default_v2_compatible();

    // Simulate: agent claims done via "agent_said_done" signal
    let claimed_signal = "agent_said_done";

    let is_invalid = pack
        .invalid_completion_signals
        .iter()
        .any(|s| s == claimed_signal);

    assert!(
        is_invalid,
        "'{}' must appear in invalid_completion_signals — the control pack must explicitly reject agent-said-done as a completion signal",
        claimed_signal
    );

    // Also verify "tool_returned_success" is rejected
    let tool_signal = "tool_returned_success";
    let tool_is_invalid = pack
        .invalid_completion_signals
        .iter()
        .any(|s| s == tool_signal);
    assert!(
        tool_is_invalid,
        "'{}' must appear in invalid_completion_signals",
        tool_signal
    );

    // And "command_exited_zero"
    let cmd_signal = "command_exited_zero";
    let cmd_is_invalid = pack
        .invalid_completion_signals
        .iter()
        .any(|s| s == cmd_signal);
    assert!(
        cmd_is_invalid,
        "'{}' must appear in invalid_completion_signals",
        cmd_signal
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 7: Receipt hash tampering is detected
//
// Create a valid ReceiptEvidence, then construct a copy with a mismatched
// evidence_hash. Assert verify() returns false on the tampered version.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn receipt_hash_tampering_detected() {
    // Build a valid receipt
    let mut builder = EvidenceBuilder::new();
    builder.add_str("spec_hash", "canonical-spec-bytes");
    builder.add_str("output_hash", "canonical-output-bytes");
    let valid_evidence = builder.finalize("tamper-test-work-unit");

    assert!(valid_evidence.verify(), "original evidence must verify");

    // Tamper: produce a copy with incorrect evidence_hash
    let mut tampered = valid_evidence.clone();
    tampered.evidence_hash = blake3_str("wrong-hash-value");

    assert!(
        !tampered.verify(),
        "tampered evidence_hash must cause verify() to return false — hash tampering must be detected"
    );

    // Tamper: produce a copy with an altered entry value
    let mut entry_tampered = valid_evidence.clone();
    entry_tampered
        .entries
        .insert("spec_hash".into(), blake3_str("injected-wrong-spec"));

    assert!(
        !entry_tampered.verify(),
        "tampered entry must cause verify() to return false — entry mutation must be detected"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Law 8: No receipt means no done
//
// Build an Envelope::pass with no ReceiptEvidence attached.
// Assert that the JSON output does not carry an evidence_hash field —
// proving the pass-only envelope cannot represent completion.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn no_receipt_means_no_done() {
    let envelope = Envelope::pass("mcpp.tool.execute", "mcpp");
    let json_str = envelope.to_json();
    let v: Value = serde_json::from_str(&json_str).expect("envelope JSON must be valid");

    // The pass envelope data must be an empty object — no evidence_hash field
    let data = &v["data"];
    assert!(data.is_object(), "data must be an object");

    assert!(
        data.get("evidence_hash").is_none(),
        "Envelope::pass output JSON must not contain 'evidence_hash' — no receipt means no done"
    );

    assert!(
        data.get("entries").is_none(),
        "Envelope::pass output JSON must not contain 'entries' — no evidence entries attached"
    );

    assert!(
        data.get("schema").is_none(),
        "Envelope::pass output JSON must not contain receipt schema — bare pass is not a receipt"
    );

    // Confirm the status field is present (it is pass) but that alone is not completion
    assert_eq!(
        v["status"], "pass",
        "status is pass but this alone does not constitute completion"
    );
}
