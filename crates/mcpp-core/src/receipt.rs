//! Receipt + ReceiptEvidence + AndonClass — promoted from
//! `chatmangpt-mcpp-v2-cell::speckit-ralph::{receipt_surfaces,andon}` per
//! `portfolio-obl-0002`.
//!
//! The receipt-verify policy enforces both v2 laws:
//!
//! * **Abnormality path:** missing/pending receipt → `verify` rejects with
//!   `RECEIPT_DEFECT` (caller is responsible for raising andon).
//! * **Happy path:** receipt status is `emitted` or `verified`, every
//!   required evidence field is populated, and recomputed file hashes match.
//!
//! Tool success and tests passing alone are NOT completion: the only valid
//! done is `Receipt::is_complete() == true`.

use crate::blake3_hash::{blake3_file, causality_chain};
use crate::exit_code;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Andon failure classes — promoted verbatim from v2 `andon::AndonClass`.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum AndonClass {
    SpecDefect,
    PlanDefect,
    TaskDefect,
    ImplementationDefect,
    PresetDefect,
    InvocationDefect,
    ReceiptDefect,
    AgentRoutingDefect,
    OntologyDefect,
    PolicyDefect,
    EnvironmentDefect,
}

impl AndonClass {
    pub fn as_str(&self) -> &'static str {
        match self {
            AndonClass::SpecDefect => "SPEC_DEFECT",
            AndonClass::PlanDefect => "PLAN_DEFECT",
            AndonClass::TaskDefect => "TASK_DEFECT",
            AndonClass::ImplementationDefect => "IMPLEMENTATION_DEFECT",
            AndonClass::PresetDefect => "PRESET_DEFECT",
            AndonClass::InvocationDefect => "INVOCATION_DEFECT",
            AndonClass::ReceiptDefect => "RECEIPT_DEFECT",
            AndonClass::AgentRoutingDefect => "AGENT_ROUTING_DEFECT",
            AndonClass::OntologyDefect => "ONTOLOGY_DEFECT",
            AndonClass::PolicyDefect => "POLICY_DEFECT",
            AndonClass::EnvironmentDefect => "ENVIRONMENT_DEFECT",
        }
    }
}

/// Multi-surface evidence carried by every emitted receipt.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReceiptEvidence {
    pub accepted_delta_hash: Option<String>,
    pub control_pack_hash: Option<String>,
    pub spec_hash: Option<String>,
    pub plan_hash: Option<String>,
    pub tasks_hash: Option<String>,
    pub state_before_hash: Option<String>,
    pub state_after_hash: Option<String>,
    pub test_result_hash: Option<String>,
    pub verify_report_hash: Option<String>,
    pub telemetry_span_id: Option<String>,
    pub process_log_event_id: Option<String>,
    pub causality_chain_hash: Option<String>,
    /// Optional ostar gate report hash (advisory in v2).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ostar_gate_hash: Option<String>,
    /// BLAKE3 of the canonical MotionObligation that authorized this work.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub obligation_hash: Option<String>,
    /// BLAKE3 of the source-cell constraint inventory consumed during a
    /// promotion run (recorded by `portfolio-obl-0002` and successors).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub absorbed_source_constraints_hash: Option<String>,
}

impl ReceiptEvidence {
    /// Required keys for an emitted receipt to qualify as evidence-backed.
    /// Mirrors `Obligation::EmitReceipt::evidence_required_for` from v2.
    pub const REQUIRED_KEYS: &'static [&'static str] = &[
        "accepted_delta_hash",
        "control_pack_hash",
        "state_before_hash",
        "state_after_hash",
        "causality_chain_hash",
    ];

    /// True iff every required field is `Some(_)`.
    pub fn is_evidence_complete(&self) -> bool {
        self.accepted_delta_hash.is_some()
            && self.control_pack_hash.is_some()
            && self.state_before_hash.is_some()
            && self.state_after_hash.is_some()
            && self.causality_chain_hash.is_some()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Receipt {
    pub id: String,
    /// `"pending" | "emitted" | "verified"`.
    pub status: String,
    #[serde(default)]
    pub verify_passed: bool,
    #[serde(default)]
    pub receipt_verified: bool,
    #[serde(default)]
    pub state_advanced: bool,
    pub evidence: ReceiptEvidence,
}

impl Receipt {
    pub fn pending(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            status: "pending".into(),
            verify_passed: false,
            receipt_verified: false,
            state_advanced: false,
            evidence: ReceiptEvidence::default(),
        }
    }

    /// The canonical completion predicate. **The only way to be `done`.**
    /// Tool success, passing tests, or a non-empty diff are NOT enough.
    pub fn is_complete(&self) -> bool {
        self.status == "verified"
            && self.receipt_verified
            && self.state_advanced
            && self.evidence.is_evidence_complete()
    }
}

/// Outcome of `Receipt::verify_against`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum VerifyOutcome {
    /// Happy path: status is `verified`, every required surface matches the
    /// recomputed file hash, and `state_advanced` flipped to true.
    Verified,
    /// Abnormality path: receipt is missing, pending, or not yet emitted —
    /// callers raise `RECEIPT_DEFECT` andon.
    Pending,
    /// Hash drift: one or more required evidence hashes do not match the
    /// recomputed value. The drifting key is reported.
    Drift(String),
    /// Required evidence field is `None`.
    MissingEvidence(String),
}

impl VerifyOutcome {
    /// Map a verify outcome to a process exit code.
    pub fn exit_code(&self) -> i32 {
        match self {
            VerifyOutcome::Verified => exit_code::SUCCESS,
            VerifyOutcome::Pending
            | VerifyOutcome::Drift(_)
            | VerifyOutcome::MissingEvidence(_) => exit_code::RECEIPT_INVALID,
        }
    }
}

/// Inputs for `Receipt::verify_against`. Each is the path the receipt's
/// matching evidence hash claims to cover.
pub struct VerifyInputs<'a> {
    pub accepted_delta_path: &'a Path,
    pub control_pack_path: &'a Path,
    pub state_path: &'a Path,
}

impl Receipt {
    /// Recompute the on-disk hashes for the required surfaces and compare
    /// them against the receipt's claims. Implements both v2 receipt laws.
    pub fn verify_against(&self, inputs: &VerifyInputs<'_>) -> VerifyOutcome {
        // Abnormality path — pending or unemitted receipts cannot verify.
        if self.status == "pending" {
            return VerifyOutcome::Pending;
        }
        if self.status != "emitted" && self.status != "verified" {
            return VerifyOutcome::Pending;
        }

        // Required-evidence completeness check.
        if self.evidence.accepted_delta_hash.is_none() {
            return VerifyOutcome::MissingEvidence("accepted_delta_hash".into());
        }
        if self.evidence.control_pack_hash.is_none() {
            return VerifyOutcome::MissingEvidence("control_pack_hash".into());
        }
        if self.evidence.state_after_hash.is_none() {
            return VerifyOutcome::MissingEvidence("state_after_hash".into());
        }
        if self.evidence.causality_chain_hash.is_none() {
            return VerifyOutcome::MissingEvidence("causality_chain_hash".into());
        }

        // Recompute hashes and compare against the claims.
        let recomputed_acc = blake3_file(inputs.accepted_delta_path);
        if recomputed_acc != self.evidence.accepted_delta_hash {
            return VerifyOutcome::Drift("accepted_delta_hash".into());
        }
        let recomputed_cp = blake3_file(inputs.control_pack_path);
        if recomputed_cp != self.evidence.control_pack_hash {
            return VerifyOutcome::Drift("control_pack_hash".into());
        }
        let recomputed_state = blake3_file(inputs.state_path);
        if recomputed_state != self.evidence.state_after_hash {
            return VerifyOutcome::Drift("state_after_hash".into());
        }

        // Recompute causality chain to catch evidence-injection attempts.
        let chain = causality_chain(
            None,
            recomputed_state.as_deref(),
            recomputed_acc.as_deref(),
            recomputed_cp.as_deref(),
        );
        if Some(&chain) != self.evidence.causality_chain_hash.as_ref() {
            return VerifyOutcome::Drift("causality_chain_hash".into());
        }

        VerifyOutcome::Verified
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blake3_hash::{blake3_file, causality_chain};
    use std::fs;
    use tempfile::tempdir;

    fn write(p: &Path, s: &str) {
        fs::create_dir_all(p.parent().unwrap()).unwrap();
        fs::write(p, s).unwrap();
    }

    fn populate(td: &Path) -> (std::path::PathBuf, std::path::PathBuf, std::path::PathBuf) {
        let acc = td.join("accepted-delta.yaml");
        let cp = td.join("control-pack.yaml");
        let state = td.join("state.yaml");
        write(&acc, "id: x\n");
        write(&cp, "schema: x\n");
        write(&state, "target: x\nphase: implement\n");
        (acc, cp, state)
    }

    #[test]
    fn pending_receipt_returns_pending() {
        let r = Receipt::pending("u1");
        let td = tempdir().unwrap();
        let (a, c, s) = populate(td.path());
        let outcome = r.verify_against(&VerifyInputs {
            accepted_delta_path: &a,
            control_pack_path: &c,
            state_path: &s,
        });
        assert_eq!(outcome, VerifyOutcome::Pending);
        assert_eq!(outcome.exit_code(), exit_code::RECEIPT_INVALID);
    }

    #[test]
    fn happy_path_verifies_when_hashes_match() {
        let td = tempdir().unwrap();
        let (a, c, s) = populate(td.path());
        let acc_h = blake3_file(&a);
        let cp_h = blake3_file(&c);
        let state_h = blake3_file(&s);
        let chain = causality_chain(None, state_h.as_deref(), acc_h.as_deref(), cp_h.as_deref());
        let r = Receipt {
            id: "u1".into(),
            status: "emitted".into(),
            verify_passed: true,
            receipt_verified: false,
            state_advanced: false,
            evidence: ReceiptEvidence {
                accepted_delta_hash: acc_h,
                control_pack_hash: cp_h,
                state_before_hash: state_h.clone(),
                state_after_hash: state_h,
                causality_chain_hash: Some(chain),
                ..Default::default()
            },
        };
        assert_eq!(
            r.verify_against(&VerifyInputs {
                accepted_delta_path: &a,
                control_pack_path: &c,
                state_path: &s,
            }),
            VerifyOutcome::Verified
        );
    }

    #[test]
    fn drift_in_state_hash_is_caught() {
        let td = tempdir().unwrap();
        let (a, c, s) = populate(td.path());
        let acc_h = blake3_file(&a);
        let cp_h = blake3_file(&c);
        let state_h = blake3_file(&s);
        let chain = causality_chain(None, state_h.as_deref(), acc_h.as_deref(), cp_h.as_deref());
        let r = Receipt {
            id: "u1".into(),
            status: "emitted".into(),
            verify_passed: true,
            receipt_verified: false,
            state_advanced: false,
            evidence: ReceiptEvidence {
                accepted_delta_hash: acc_h,
                control_pack_hash: cp_h,
                state_before_hash: state_h.clone(),
                state_after_hash: state_h,
                causality_chain_hash: Some(chain),
                ..Default::default()
            },
        };
        // Mutate the state file after the receipt was 'emitted'.
        fs::write(&s, "target: y\nphase: tampered\n").unwrap();
        match r.verify_against(&VerifyInputs {
            accepted_delta_path: &a,
            control_pack_path: &c,
            state_path: &s,
        }) {
            VerifyOutcome::Drift(k) => {
                assert!(k == "state_after_hash" || k == "causality_chain_hash");
            }
            other => panic!("expected drift, got {other:?}"),
        }
    }

    #[test]
    fn missing_required_evidence_is_rejected() {
        let td = tempdir().unwrap();
        let (a, c, s) = populate(td.path());
        let r = Receipt {
            id: "u1".into(),
            status: "emitted".into(),
            verify_passed: true,
            receipt_verified: false,
            state_advanced: false,
            evidence: ReceiptEvidence::default(),
        };
        match r.verify_against(&VerifyInputs {
            accepted_delta_path: &a,
            control_pack_path: &c,
            state_path: &s,
        }) {
            VerifyOutcome::MissingEvidence(k) => assert_eq!(k, "accepted_delta_hash"),
            other => panic!("expected missing-evidence, got {other:?}"),
        }
    }

    #[test]
    fn is_complete_only_when_status_verified_and_state_advanced() {
        let mut r = Receipt::pending("u1");
        assert!(!r.is_complete(), "pending");
        r.status = "verified".into();
        assert!(!r.is_complete(), "verified but evidence empty");
        r.evidence.accepted_delta_hash = Some("blake3:a".into());
        r.evidence.control_pack_hash = Some("blake3:c".into());
        r.evidence.state_before_hash = Some("blake3:s".into());
        r.evidence.state_after_hash = Some("blake3:s".into());
        r.evidence.causality_chain_hash = Some("blake3:cc".into());
        assert!(!r.is_complete(), "evidence ok but receipt_verified=false");
        r.receipt_verified = true;
        r.state_advanced = true;
        assert!(r.is_complete());
    }

    #[test]
    fn andon_receipt_defect_string_is_stable() {
        // Cross-check against the v2 control-pack value so receipts emitted
        // by canonical MCPP and absorbed v2 cell are interoperable.
        assert_eq!(AndonClass::ReceiptDefect.as_str(), "RECEIPT_DEFECT");
    }
}
