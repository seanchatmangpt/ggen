//! Receipt happy-path law — emit + verify policy.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/speckit-ralph/src/main.rs`
//! (portfolio-obl-0002, promotion id `receipt-happy-path-law`).
//!
//! Doctrine:
//! - Receipt emission + verification is the ONLY valid completion signal.
//! - A completion claim backed by anything other than a verified receipt is
//!   a false completion and must be rejected with a typed RECEIPT_DEFECT envelope.
//! - Faking must be harder than the real thing.

use crate::{Envelope, EvidenceBuilder, ReceiptEvidence, RECEIPT_DEFECT_CLASS};

/// Stable version identifier for the receipt policy law.
pub const RECEIPT_POLICY_VERSION: &str = "chatmangpt.mcpp.receipt-policy.v1";

/// Validate that a [`ReceiptEvidence`] satisfies happy-path completion.
///
/// Rules enforced:
/// 1. `work_unit` must be non-empty.
/// 2. At least one evidence entry must be present.
/// 3. `verify()` must return `true` (composite hash matches).
///
/// Returns `Ok(())` if all rules pass.
/// Returns `Err(Envelope)` typed as `RECEIPT_DEFECT` on any violation.
pub fn validate_receipt_for_completion(
    receipt: &ReceiptEvidence, command: &str, target: &str,
) -> Result<(), Envelope> {
    if receipt.work_unit.is_empty() {
        return Err(Envelope::fail(
            command,
            target,
            RECEIPT_DEFECT_CLASS,
            "receipt work_unit must not be empty",
        ));
    }
    if receipt.entries.is_empty() {
        return Err(Envelope::fail(
            command,
            target,
            RECEIPT_DEFECT_CLASS,
            "receipt must have at least one evidence entry",
        ));
    }
    if !receipt.verify() {
        return Err(Envelope::fail(
            command,
            target,
            RECEIPT_DEFECT_CLASS,
            "receipt evidence_hash verification failed — receipt is invalid",
        ));
    }
    Ok(())
}

/// Emit a receipt for a work unit with a set of (key, value) evidence entries.
///
/// Each entry is hashed under BLAKE3. The returned [`ReceiptEvidence`] has a
/// deterministic composite `evidence_hash` and will satisfy `verify()`.
///
/// Returns `Ok(ReceiptEvidence)` when the receipt is valid.
/// Returns `Err(Envelope)` typed as `RECEIPT_DEFECT` when:
/// - `work_unit` is empty.
/// - `entries` is empty.
pub fn emit_receipt(
    work_unit: &str, entries: Vec<(&str, &str)>, command: &str, target: &str,
) -> Result<ReceiptEvidence, Envelope> {
    if work_unit.is_empty() {
        return Err(Envelope::fail(
            command,
            target,
            RECEIPT_DEFECT_CLASS,
            "work_unit must not be empty",
        ));
    }
    if entries.is_empty() {
        return Err(Envelope::fail(
            command,
            target,
            RECEIPT_DEFECT_CLASS,
            "cannot emit receipt with no evidence entries",
        ));
    }
    let mut builder = EvidenceBuilder::new();
    for (k, v) in entries {
        builder.add_str(k, v);
    }
    Ok(builder.finalize(work_unit))
}

#[cfg(test)]
mod tests {
    use super::*;

    const CMD: &str = "mcpp.receipt.emit";
    const TGT: &str = "mcpp-core";

    // ── emit_receipt ──────────────────────────────────────────────────────────

    #[test]
    fn emit_receipt_with_valid_entries_succeeds() {
        let receipt = emit_receipt(
            "portfolio-obl-0002",
            vec![("input_hash", "abc"), ("output_hash", "def")],
            CMD,
            TGT,
        )
        .expect("valid emit must not fail");

        assert_eq!(receipt.work_unit, "portfolio-obl-0002");
        assert!(!receipt.entries.is_empty());
        assert!(receipt.verify(), "emitted receipt must verify");
    }

    #[test]
    fn emit_receipt_with_single_entry_succeeds() {
        let receipt = emit_receipt("single-unit", vec![("only_key", "only_val")], CMD, TGT)
            .expect("single-entry emit must succeed");
        assert_eq!(receipt.entries.len(), 1);
        assert!(receipt.verify());
    }

    #[test]
    fn emit_receipt_with_empty_work_unit_returns_defect() {
        let err = emit_receipt("", vec![("k", "v")], CMD, TGT)
            .expect_err("empty work_unit must return Err");
        assert_eq!(err.status, "fail");
        assert_eq!(err.errors[0]["class"], RECEIPT_DEFECT_CLASS);
        assert!(
            err.errors[0]["message"]
                .as_str()
                .unwrap()
                .contains("work_unit"),
            "error message must mention work_unit"
        );
    }

    #[test]
    fn emit_receipt_with_no_entries_returns_defect() {
        let err = emit_receipt("non-empty-unit", vec![], CMD, TGT)
            .expect_err("empty entries must return Err");
        assert_eq!(err.status, "fail");
        assert_eq!(err.errors[0]["class"], RECEIPT_DEFECT_CLASS);
        assert!(
            err.errors[0]["message"]
                .as_str()
                .unwrap()
                .contains("no evidence"),
            "error message must mention no evidence"
        );
    }

    // ── validate_receipt_for_completion ───────────────────────────────────────

    #[test]
    fn validate_receipt_passes_for_valid_receipt() {
        let receipt = emit_receipt(
            "happy-path-work",
            vec![("artifact", "bytes"), ("test_result", "pass")],
            CMD,
            TGT,
        )
        .unwrap();
        let result = validate_receipt_for_completion(&receipt, CMD, TGT);
        assert!(result.is_ok(), "valid receipt must pass validation");
    }

    #[test]
    fn validate_receipt_fails_for_empty_work_unit() {
        // Build a receipt directly to bypass emit_receipt guard, then corrupt work_unit.
        let mut builder = EvidenceBuilder::new();
        builder.add_str("k", "v");
        let mut receipt = builder.finalize("placeholder");
        receipt.work_unit = String::new(); // force empty

        let err = validate_receipt_for_completion(&receipt, CMD, TGT)
            .expect_err("empty work_unit must fail validation");
        assert_eq!(err.errors[0]["class"], RECEIPT_DEFECT_CLASS);
        assert!(err.errors[0]["message"]
            .as_str()
            .unwrap()
            .contains("work_unit"));
    }

    #[test]
    fn validate_receipt_fails_for_no_entries() {
        // Build a receipt with entries then drain them to test the guard.
        let mut builder = EvidenceBuilder::new();
        builder.add_str("k", "v");
        let mut receipt = builder.finalize("work-unit");
        receipt.entries.clear();
        // evidence_hash will no longer match (empty map composite != original)

        let err = validate_receipt_for_completion(&receipt, CMD, TGT)
            .expect_err("no entries must fail validation");
        assert_eq!(err.errors[0]["class"], RECEIPT_DEFECT_CLASS);
        // May fail on "at least one evidence entry" or on verify — both are RECEIPT_DEFECT
    }

    #[test]
    fn validate_receipt_fails_for_tampered_hash() {
        let mut builder = EvidenceBuilder::new();
        builder.add_str("input", "original");
        let mut receipt = builder.finalize("integrity-unit");

        // Tamper the evidence_hash without changing entries
        receipt.evidence_hash =
            "blake3:0000000000000000000000000000000000000000000000000000000000000000".to_string();

        let err = validate_receipt_for_completion(&receipt, CMD, TGT)
            .expect_err("tampered hash must fail validation");
        assert_eq!(err.errors[0]["class"], RECEIPT_DEFECT_CLASS);
        assert!(err.errors[0]["message"]
            .as_str()
            .unwrap()
            .contains("verification failed"));
    }

    // ── constant stability ─────────────────────────────────────────────────────

    #[test]
    fn receipt_policy_version_is_stable() {
        assert_eq!(
            RECEIPT_POLICY_VERSION, "chatmangpt.mcpp.receipt-policy.v1",
            "RECEIPT_POLICY_VERSION must not change — it is a stable schema identifier"
        );
    }

    // ── defect envelope structure ──────────────────────────────────────────────

    #[test]
    fn defect_envelope_is_valid_json_with_receipt_defect_class() {
        let err = emit_receipt("", vec![], CMD, TGT).expect_err("expected defect");
        let json_str = err.to_json();
        let v: serde_json::Value =
            serde_json::from_str(&json_str).expect("defect envelope must be valid JSON");
        assert_eq!(v["status"], "fail");
        assert_eq!(v["errors"][0]["class"], RECEIPT_DEFECT_CLASS);
        assert!(v["errors"][0]["message"].is_string());
    }

    #[test]
    fn emitted_receipt_survives_json_round_trip_and_still_verifies() {
        let receipt = emit_receipt(
            "round-trip-unit",
            vec![("spec", "sha256:abc"), ("output", "sha256:def")],
            CMD,
            TGT,
        )
        .unwrap();
        let json = serde_json::to_string(&receipt).unwrap();
        let parsed: ReceiptEvidence = serde_json::from_str(&json).unwrap();
        assert!(parsed.verify(), "round-tripped receipt must still verify");
        assert_eq!(parsed.work_unit, "round-trip-unit");
    }
}
