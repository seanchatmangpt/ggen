//! Cryptographic transition logging for the admission kernel — the
//! reverse-80/20 §5 receipt half of the fleet receipt law
//! (`A = μ(O*)`, `R = receipt(A)`).
//!
//! Every `admit_work_order` outcome — admitted OR refused — becomes an
//! [`AdmissionTransition`] chained into a [`TransitionJournal`] by blake3:
//!
//! ```text
//! hash_n = blake3(prev_hash_n ‖ canonical-JSON body_n)
//! ```
//!
//! The body is the semantic payload (work_order_digest, decision,
//! blast_radius_digest) as key-sorted canonical JSON; the genesis link
//! chains off the all-zeros prev_hash. [`TransitionJournal::verify_chain`]
//! detects a tamper at any depth: a mutated body breaks the recomputed
//! hash (Tampered); a mutated `prev_hash` breaks the linkage (BrokenLink);
//! a mutated `hash` breaks recomputation (Tampered). Each error carries
//! the index of the first broken link, so depth is located, not just
//! flagged.
//!
//! IO-free. The journal is an append-only in-memory Vec; durability is a
//! later slice. `#![forbid(unsafe_code)]` is inherited from the crate root.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest as _, Sha256};
use std::fmt;

use crate::admit::{AdmittedWorkOrder, Refusal};
use crate::work_order::hex;

/// The all-zeros genesis prev_hash (blake3 string form, 64 hex chars).
pub const GENESIS_PREV_HASH: &str =
    "0000000000000000000000000000000000000000000000000000000000000000";

/// One recorded admission outcome, sealed into the blake3 chain.
///
/// `hash = blake3(prev_hash ‖ canonical-JSON body)`; the body covers the
/// semantic fields, so any body mutation is detected by recomputation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdmissionTransition {
    /// Digest of the work order the decision bound to. For admitted
    /// orders: the admission snapshot digest (`sha256:…`). For refusals:
    /// `refusal:sha256:…` over the refusal text (the payload never
    /// deserialized, so the snapshot digest does not exist — the two
    /// digest classes are never conflated).
    pub work_order_digest: String,
    /// The admission decision.
    pub decision: Decision,
    /// Digest of the blast-radius set, when the caller supplies one.
    pub blast_radius_digest: Option<String>,
    /// Chain field: previous transition's hash (genesis: all-zeros).
    pub prev_hash: String,
    /// Chain field: blake3(prev_hash ‖ canonical-JSON body).
    pub hash: String,
}

/// Admitted or Refused — the legal-transition / illegal-denial boundary
/// from the blueprint.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Decision {
    /// Order admitted; carries the stamped authority (always `NONE` in
    /// this kernel — admission never creates authority).
    Admitted {
        authority: String,
    },
    /// Order refused; carries the typed refusal's Display text as detail.
    Refused {
        detail: String,
    },
}

impl fmt::Display for Decision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Admitted { authority } => write!(f, "Admitted(authority={authority})"),
            Self::Refused { detail } => write!(f, "Refused({detail})"),
        }
    }
}

/// The semantic body of a transition: every field except the chain fields,
/// as key-sorted canonical JSON text.
fn body_of(
    work_order_digest: &str,
    decision: &Decision,
    blast_radius_digest: Option<&str>,
) -> String {
    let mut m = serde_json::Map::new();
    m.insert(
        "blast_radius_digest".to_string(),
        match blast_radius_digest {
            Some(d) => Value::String(d.to_string()),
            None => Value::Null,
        },
    );
    m.insert(
        "decision".to_string(),
        serde_json::to_value(decision).expect("Decision is serializable"),
    );
    m.insert(
        "work_order_digest".to_string(),
        Value::String(work_order_digest.to_string()),
    );
    // serde_json::Map is a BTreeMap by default (sorted keys) and
    // canonical() re-sorts recursively — determinism is doubly pinned.
    crate::work_order::canonical(&Value::Object(m)).to_string()
}

impl AdmissionTransition {
    /// Recompute `hash` from this record's own fields.
    fn recompute_hash(&self) -> String {
        let body = body_of(
            &self.work_order_digest,
            &self.decision,
            self.blast_radius_digest.as_deref(),
        );
        let mut hasher = blake3::Hasher::new();
        hasher.update(self.prev_hash.as_bytes());
        hasher.update(body.as_bytes());
        hasher.finalize().to_string()
    }

    /// The chain-ready record with `hash` computed over `prev_hash` ‖ body.
    fn sealed(self) -> Self {
        Self {
            hash: self.recompute_hash(),
            ..self
        }
    }
}

/// Append-only journal of admission transitions with blake3 chain
/// verification.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TransitionJournal {
    /// Public so courts can construct tamper scenarios (Chicago: mutate
    /// the real record, re-verify, assert the typed error). Append via
    /// `record`/`append` only — the verify law never accepts a hand-built
    /// chain that does not recompute.
    pub transitions: Vec<AdmissionTransition>,
}

/// Typed chain failure, carrying the index of the first broken link:
/// BrokenLink (prev_hash mismatch) vs Tampered (body-vs-hash mismatch).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ChainError {
    #[error("chain broken at link {index}: {detail}")]
    BrokenLink { index: usize, detail: String },
    #[error("tamper detected at link {index}: {detail}")]
    Tampered { index: usize, detail: String },
}

impl TransitionJournal {
    pub fn new() -> Self {
        Self::default()
    }

    /// Current chain head (genesis prev_hash when empty).
    pub fn head(&self) -> String {
        self.transitions
            .last()
            .map(|t| t.hash.clone())
            .unwrap_or_else(|| GENESIS_PREV_HASH.to_string())
    }

    /// Number of recorded transitions.
    pub fn len(&self) -> usize {
        self.transitions.len()
    }

    /// True when no transitions are recorded.
    pub fn is_empty(&self) -> bool {
        self.transitions.is_empty()
    }

    /// Snapshot accessor.
    pub fn transitions(&self) -> &[AdmissionTransition] {
        &self.transitions
    }

    /// The bridge required by the blueprint: convert one
    /// `admit_work_order` result into a sealed [`AdmissionTransition`]
    /// chained off the current head. An `Ok(admitted)` becomes
    /// `Decision::Admitted` carrying the stamped authority; an `Err`
    /// becomes `Decision::Refused` carrying the refusal's Display text.
    /// Returns the appended transition's hash.
    pub fn record(
        &mut self,
        result: &Result<AdmittedWorkOrder, Refusal>,
        blast_radius_digest: Option<String>,
    ) -> String {
        self.append(
            work_order_digest_of(result),
            decision_of(result),
            blast_radius_digest,
        )
    }

    /// Append a pre-built transition: `prev_hash` is overwritten with the
    /// current head, then `hash` is computed. For callers that already
    /// hold the semantic fields directly. Returns the sealed hash.
    pub fn append(
        &mut self,
        work_order_digest: String,
        decision: Decision,
        blast_radius_digest: Option<String>,
    ) -> String {
        let prev_hash = self.head();
        let t = AdmissionTransition {
            work_order_digest,
            decision,
            blast_radius_digest,
            prev_hash,
            hash: String::new(),
        }
        .sealed();
        self.transitions.push(t.clone());
        t.hash
    }

    /// Verify the whole chain: linkage (each `prev_hash` equals the prior
    /// hash; genesis chains off all-zeros) AND each record's own hash
    /// recomputation. `Ok(())` iff fully intact.
    pub fn verify_chain(&self) -> Result<(), ChainError> {
        let mut expected_prev = GENESIS_PREV_HASH.to_string();
        for (index, t) in self.transitions.iter().enumerate() {
            if t.prev_hash != expected_prev {
                return Err(ChainError::BrokenLink {
                    index,
                    detail: format!(
                        "prev_hash {} does not match prior hash {}",
                        t.prev_hash, expected_prev
                    ),
                });
            }
            let recomputed = t.recompute_hash();
            if recomputed != t.hash {
                return Err(ChainError::Tampered {
                    index,
                    detail: format!("stored {} != recomputed {}", t.hash, recomputed),
                });
            }
            expected_prev = t.hash.clone();
        }
        Ok(())
    }

    /// Convenience for court wiring: refused-decision count.
    pub fn refused_count(&self) -> usize {
        self.transitions
            .iter()
            .filter(|t| matches!(t.decision, Decision::Refused { .. }))
            .count()
    }

    /// Convenience for court wiring: admitted-decision count.
    pub fn admitted_count(&self) -> usize {
        self.transitions
            .iter()
            .filter(|t| matches!(t.decision, Decision::Admitted { .. }))
            .count()
    }
}

/// Bridge helper: the digest bound into the transition. Admitted orders
/// carry the admission snapshot digest; refusals carry
/// `refusal:sha256:<hex>` over the refusal Display text, so the two digest
/// classes are never conflated.
fn work_order_digest_of(result: &Result<AdmittedWorkOrder, Refusal>) -> String {
    match result {
        Ok(admitted) => admitted.work_order_digest.clone(),
        Err(refusal) => {
            let mut hasher = Sha256::new();
            hasher.update(refusal.to_string().as_bytes());
            format!("refusal:sha256:{}", hex(&hasher.finalize()))
        }
    }
}

/// Bridge helper: the decision payload.
fn decision_of(result: &Result<AdmittedWorkOrder, Refusal>) -> Decision {
    match result {
        Ok(admitted) => Decision::Admitted {
            authority: admitted.authority.clone(),
        },
        Err(refusal) => Decision::Refused {
            detail: refusal.to_string(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admit::admit_work_order;
    use serde_json::json;

    fn valid_order_json() -> Value {
        json!({
            "identity": "X1",
            "title": "t",
            "description": "d",
            "subject": "src/x1.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "standing": "UNKNOWN",
            "evidence_ceiling": "UNVERIFIED",
            "promotion_rule": "courts",
            "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "origin_authority": "https://example.test/auth/X1",
            "projections": ["jira"],
            "required_courts": ["c1"],
            "required_evidence": ["e1"],
            "acceptance": ["a1"],
            "falsifiers": ["f1"]
        })
    }

    #[test]
    fn chain_builds_and_verifies() {
        let good = admit_work_order(&valid_order_json());
        assert!(good.is_ok());
        let refused: Result<AdmittedWorkOrder, Refusal> =
            admit_work_order(&json!({ "identity": "X2" }));
        assert!(refused.is_err());

        let mut j = TransitionJournal::new();
        let h1 = j.record(&good, Some("sha256:bbbb".into()));
        let h2 = j.record(&refused, None);
        assert_eq!(j.len(), 2);
        assert_eq!(j.head(), h2);
        assert_eq!(j.refused_count(), 1);
        assert_eq!(j.admitted_count(), 1);
        assert_eq!(j.verify_chain(), Ok(()));
        // linkage is real: first record's hash IS the second's prev_hash
        assert_eq!(j.transitions()[1].prev_hash, h1);
        assert_ne!(h1, h2);
        assert_eq!(j.transitions()[0].prev_hash, GENESIS_PREV_HASH);
    }

    #[test]
    fn mid_chain_tamper_detected_at_depth() {
        let good = admit_work_order(&valid_order_json());
        let mut j = TransitionJournal::new();
        j.record(&good, None);
        j.record(&good, None);
        j.record(&good, None);
        assert_eq!(j.verify_chain(), Ok(()));

        // Mutate the body of the MIDDLE transition.
        let mut tampered = j.clone();
        tampered.transitions[1].work_order_digest = "sha256:dead".into();
        match tampered.verify_chain() {
            Err(ChainError::Tampered { index: 1, .. }) => {}
            other => panic!("expected Tampered at 1, got {other:?}"),
        }

        // Mutate a hash field: recomputation breaks at that index first.
        let mut tampered_hash = j.clone();
        tampered_hash.transitions[1].hash = "0".repeat(64);
        match tampered_hash.verify_chain() {
            Err(ChainError::Tampered { index: 1, .. }) => {}
            other => panic!("expected Tampered at 1, got {other:?}"),
        }

        // Mutate prev_hash only: linkage breaks there, downstream hashes
        // still recompute fine — BrokenLink, not Tampered.
        let mut broken_link = j.clone();
        broken_link.transitions[2].prev_hash = GENESIS_PREV_HASH.to_string();
        match broken_link.verify_chain() {
            Err(ChainError::BrokenLink { index: 2, .. }) => {}
            other => panic!("expected BrokenLink at 2, got {other:?}"),
        }
    }

    #[test]
    fn refusal_transitions_are_logged() {
        let refused: Result<AdmittedWorkOrder, Refusal> =
            admit_work_order(&json!({ "identity": "X2" }));
        let refusal_text = refused.as_ref().unwrap_err().to_string();
        assert!(refusal_text.starts_with("Deserialization"));

        let mut j = TransitionJournal::new();
        j.record(&refused, None);
        assert_eq!(j.len(), 1);
        assert_eq!(j.refused_count(), 1);
        assert_eq!(j.admitted_count(), 0);
        let t = &j.transitions()[0];
        assert!(
            matches!(t.decision, Decision::Refused { detail: ref d } if d == &refusal_text),
            "refusal detail must carry the refusal Display text"
        );
        // refusal digest class is distinct from the order-digest class
        assert!(t.work_order_digest.starts_with("refusal:sha256:"));
        assert_eq!(j.verify_chain(), Ok(()));
    }

    #[test]
    fn determinism() {
        let good = admit_work_order(&valid_order_json());
        let refused: Result<AdmittedWorkOrder, Refusal> =
            admit_work_order(&json!({ "identity": "X2" }));
        let build = || {
            let mut j = TransitionJournal::new();
            j.record(&good, Some("sha256:cc".into()));
            j.record(&refused, None);
            j.record(&good, None);
            j
        };
        let a = build();
        let b = build();
        assert_eq!(a, b);
        // Same body + same prev_hash ⇒ same hash (blake3 deterministic).
        assert_eq!(a.transitions()[0].hash, b.transitions()[0].hash);
        assert_eq!(a.transitions()[2].hash, b.transitions()[2].hash);
    }
}
