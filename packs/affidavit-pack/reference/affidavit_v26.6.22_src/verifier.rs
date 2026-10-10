//! The 7-stage certify pipeline returning a `Verdict` with per-stage outcomes.
//!
//! decode → check_format → check_chain_integrity → resolve_continuity →
//! verify_commitments → evaluate_profile → emit Verdict.
//!
//! Owned by the `verifier` phase-2 agent. Codes against `crate::types`.
//!
//! The pipeline never decides whether the underlying code was honest. It checks
//! a receipt (a witness) against a fixed format standard. Each stage is a single
//! decidable check; the verdict is pure and deterministic over the receipt bytes
//! and reads only payload commitments, never raw payloads.

use crate::chain::{recompute_chain, FORMAT_VERSION};
use crate::types::{CheckOutcome, ProfileId, Receipt, Verdict};
use std::collections::BTreeSet;

/// The format version this verifier knows how to certify.
///
/// This is the single format standard shared with the assembler
/// ([`crate::chain::FORMAT_VERSION`]); a receipt produced by `chain::finalize`
/// declares exactly this string.
const STANDARD_VERSION: &str = FORMAT_VERSION;

/// Expected hex length of a BLAKE3-256 digest (32 bytes → 64 hex chars).
const BLAKE3_HEX_LEN: usize = 64;

/// Whether a hex string is a well-formed lowercase BLAKE3-256 digest.
fn is_well_formed_hash(hex: &str) -> bool {
    hex.len() == BLAKE3_HEX_LEN
        && hex
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_uppercase())
}

/// Certify a receipt by running the seven-stage decidable pipeline.
///
/// Produces one [`CheckOutcome`] per stage in pipeline order and a final
/// [`Verdict`]. The verdict is `accepted` only when every prior stage passed;
/// `reason` summarizes the first failing stage, or reports that all stages
/// passed. The function is pure: the same receipt always yields the same verdict.
///
/// # Example: see `examples/verify_stages.rs` (run: `cargo run --example verify_stages`).
pub fn verify(receipt: &Receipt) -> Verdict {
    let outcomes: Vec<CheckOutcome> = vec![
        // Stage 1: decode — receipt structurally present and version parseable.
        stage_decode(receipt),
        // Stage 2: check_format — version equals the standard this verifier knows.
        stage_check_format(receipt),
        // Stage 3: chain_integrity — recompute and compare the rolling chain hash.
        stage_chain_integrity(receipt),
        // Stage 4: continuity — seq strictly increasing from 0, no gaps, unique ids.
        stage_continuity(receipt),
        // Stage 5: verify_commitments — every commitment is well-formed hex.
        stage_verify_commitments(receipt),
        // Stage 6: evaluate_profile (CoreV1) — event_type + commitment present.
        stage_evaluate_profile(receipt),
    ];

    // Stage 7: emit_verdict — accepted iff all prior stages passed.
    let first_failure = outcomes.iter().find(|o| !o.passed);
    let accepted = first_failure.is_none();
    let reason = match first_failure {
        Some(o) => format!("{}: {}", o.stage, o.detail),
        None => "all stages passed".to_string(),
    };

    Verdict {
        accepted,
        profile: ProfileId::CoreV1,
        outcomes,
        reason,
    }
}

/// Stage 1: the receipt is structurally present and its version is parseable.
fn stage_decode(receipt: &Receipt) -> CheckOutcome {
    let passed = !receipt.format_version.trim().is_empty();
    let detail = if passed {
        format!("{} event(s), format_version present", receipt.events.len())
    } else {
        "format_version is empty or unparseable".to_string()
    };
    CheckOutcome {
        stage: "decode".to_string(),
        passed,
        detail,
    }
}

/// Stage 2: the receipt's format version matches the verifier's standard.
fn stage_check_format(receipt: &Receipt) -> CheckOutcome {
    let passed = receipt.format_version == STANDARD_VERSION;
    let detail = if passed {
        format!("format_version == {STANDARD_VERSION}")
    } else {
        format!(
            "expected format_version {STANDARD_VERSION}, found {}",
            receipt.format_version
        )
    };
    CheckOutcome {
        stage: "check_format".to_string(),
        passed,
        detail,
    }
}

/// Stage 3: the recomputed rolling chain hash equals the stored chain hash.
fn stage_chain_integrity(receipt: &Receipt) -> CheckOutcome {
    match recompute_chain(&receipt.events) {
        Ok(computed) => {
            let passed = computed == receipt.chain_hash;
            let detail = if passed {
                "recomputed chain hash matches stored chain_hash".to_string()
            } else {
                format!(
                    "chain hash mismatch: stored {}, recomputed {}",
                    receipt.chain_hash, computed
                )
            };
            CheckOutcome {
                stage: "chain_integrity".to_string(),
                passed,
                detail,
            }
        }
        Err(e) => CheckOutcome {
            stage: "chain_integrity".to_string(),
            passed: false,
            detail: format!("could not canonicalize an event: {e}"),
        },
    }
}

/// Stage 4: seq numbers are strictly increasing from 0 with no gaps; ids unique.
fn stage_continuity(receipt: &Receipt) -> CheckOutcome {
    let mut seen_ids: BTreeSet<&str> = BTreeSet::new();
    for (index, event) in receipt.events.iter().enumerate() {
        let expected_seq = index as u64;
        if event.seq != expected_seq {
            return CheckOutcome {
                stage: "continuity".to_string(),
                passed: false,
                detail: format!(
                    "seq gap at position {index}: expected {expected_seq}, found {}",
                    event.seq
                ),
            };
        }
        if !seen_ids.insert(event.id.as_str()) {
            return CheckOutcome {
                stage: "continuity".to_string(),
                passed: false,
                detail: format!("duplicate event id: {}", event.id),
            };
        }
    }
    CheckOutcome {
        stage: "continuity".to_string(),
        passed: true,
        detail: format!(
            "{} event(s) with contiguous seq and unique ids",
            receipt.events.len()
        ),
    }
}

/// Stage 5: every event carries a well-formed (non-empty, correct-length) commitment.
fn stage_verify_commitments(receipt: &Receipt) -> CheckOutcome {
    for event in &receipt.events {
        let hex = event.payload_commitment.as_hex();
        if !is_well_formed_hash(hex) {
            return CheckOutcome {
                stage: "verify_commitments".to_string(),
                passed: false,
                detail: format!(
                    "event {} has a malformed commitment (expected {BLAKE3_HEX_LEN} lowercase hex chars)",
                    event.id
                ),
            };
        }
    }
    CheckOutcome {
        stage: "verify_commitments".to_string(),
        passed: true,
        detail: "all commitments are well-formed BLAKE3 digests".to_string(),
    }
}

/// Stage 6 (CoreV1 profile): each event has a non-empty event_type and commitment.
fn stage_evaluate_profile(receipt: &Receipt) -> CheckOutcome {
    for event in &receipt.events {
        if event.event_type.trim().is_empty() {
            return CheckOutcome {
                stage: "evaluate_profile".to_string(),
                passed: false,
                detail: format!("event {} has an empty event_type", event.id),
            };
        }
        if event.payload_commitment.as_hex().is_empty() {
            return CheckOutcome {
                stage: "evaluate_profile".to_string(),
                passed: false,
                detail: format!("event {} is missing a commitment", event.id),
            };
        }
    }
    CheckOutcome {
        stage: "evaluate_profile".to_string(),
        passed: true,
        detail: format!("profile {} satisfied", ProfileId::CoreV1.as_str()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Blake3Hash, ObjectRef, OperationEvent};

    /// Build an event with a well-formed commitment derived from its payload.
    fn event(id: &str, seq: u64, event_type: &str, payload: &[u8]) -> OperationEvent {
        OperationEvent {
            id: id.to_string(),
            seq,
            event_type: event_type.to_string(),
            objects: vec![ObjectRef {
                id: format!("obj-{id}"),
                obj_type: "artifact".to_string(),
                qualifier: None,
            }],
            payload_commitment: Blake3Hash::from_bytes(payload),
        }
    }

    /// Assemble a valid 2-event receipt with a correctly computed chain hash.
    fn valid_receipt() -> Receipt {
        let mut asm = crate::chain::ChainAssembler::new();
        asm.append(event("e0", 0, "emit", b"payload-zero"))
            .expect("append event");
        asm.append(event("e1", 1, "emit", b"payload-one"))
            .expect("append event");
        asm.finalize()
    }

    #[test]
    fn verif_valid_receipt_accepts() {
        let verdict = verify(&valid_receipt());
        assert!(verdict.accepted, "reason: {}", verdict.reason);
        assert_eq!(verdict.reason, "all stages passed");
        assert!(verdict.outcomes.iter().all(|o| o.passed));
    }

    #[test]
    fn verif_pure_deterministic() {
        let r = valid_receipt();
        assert_eq!(verify(&r), verify(&r));
    }

    #[test]
    fn verif_flipped_commitment_breaks_chain_integrity() {
        let mut r = valid_receipt();
        // Tamper a payload commitment without recomputing the chain hash.
        r.events[1].payload_commitment = Blake3Hash::from_bytes(b"tampered");
        let verdict = verify(&r);
        assert!(!verdict.accepted);
        let chain = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "chain_integrity")
            .expect("chain_integrity stage present");
        assert!(!chain.passed, "chain integrity must catch the tamper");
    }

    #[test]
    fn verif_seq_gap_fails_continuity() {
        let mut r = valid_receipt();
        r.events[1].seq = 2; // gap: expected 1
        let verdict = verify(&r);
        assert!(!verdict.accepted);
        let cont = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "continuity")
            .expect("continuity stage present");
        assert!(!cont.passed);
    }

    #[test]
    fn verif_wrong_format_fails_check_format() {
        let mut r = valid_receipt();
        r.format_version = "1.0.0".to_string();
        let verdict = verify(&r);
        assert!(!verdict.accepted);
        let fmt = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "check_format")
            .expect("check_format stage present");
        assert!(!fmt.passed);
    }

    #[test]
    fn verif_duplicate_id_fails_continuity() {
        let mut r = valid_receipt();
        r.events[1].id = r.events[0].id.clone();
        // Recompute chain so only the id check trips, not chain_integrity.
        r.chain_hash = recompute_chain(&r.events).expect("canonicalize");
        let verdict = verify(&r);
        let cont = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "continuity")
            .expect("continuity stage present");
        assert!(!cont.passed);
    }
}
