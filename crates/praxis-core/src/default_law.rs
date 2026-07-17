//! Default `Judge`/`Admit` implementation for `serde_json::Value` payloads.
//!
//! This module provides the first concrete policy for the `Law` type
//! parameter: `DefaultLaw`, a zero-sized marker whose obligations are
//! evaluated directly against fields embedded in the payload JSON. It exists
//! so `LawObject<Payload, S, Law>` has at least one working, testable
//! `Judge`/`Admit` pair rather than only the trait definitions.

use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    law::{Admit, Andon, Judge, LawObject, Obligation},
    lifecycle::{Admitted, Raw, Validated},
    refusal::RefusalScenario,
};

/// A minimal, dependency-free Judge/Admit policy for JSON payloads.
///
/// Obligations are checked against fields embedded directly in the payload
/// JSON:
/// - [`Obligation::BlockingConstraint`] is always unmet — there is no way to
///   satisfy it via payload content alone; it can only be lifted via an
///   explicit `Andon::Overridden`.
/// - [`Obligation::EvidenceRequired`] is met iff the payload has an
///   `"evidence"` array containing `evidence_type` as a string element.
/// - [`Obligation::Precondition`] is met iff the payload has a
///   `"satisfied_predicates"` array containing `predicate_id` as a string
///   element. Note: `params_hash` is recorded on the obligation but not
///   re-derived or checked here — this is a deliberate limitation of this
///   default policy, not a general guarantee about precondition parameters.
///
/// Any unmet obligation causes `judge` to return the raw object back with
/// `andon` set to `Andon::Halted { unmet, at }`.
#[derive(Debug, Clone, Copy, Default)]
pub struct DefaultLaw;

/// Current time in milliseconds since the UNIX epoch, used for `Andon::Halted::at`.
fn now_ms() -> Result<u64, String> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .map_err(|e| format!("system time error: {e}"))
}

/// True iff `payload[key]` is a JSON array containing the string `needle`.
fn array_contains_str(payload: &serde_json::Value, key: &str, needle: &str) -> bool {
    payload
        .get(key)
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().any(|item| item.as_str() == Some(needle)))
        .unwrap_or(false)
}

/// Evaluate a single obligation against the payload JSON.
fn obligation_met(payload: &serde_json::Value, obligation: &Obligation) -> bool {
    match obligation {
        Obligation::BlockingConstraint { .. } => false,
        Obligation::EvidenceRequired { evidence_type } => {
            array_contains_str(payload, "evidence", evidence_type)
        }
        Obligation::Precondition { predicate_id, .. } => {
            array_contains_str(payload, "satisfied_predicates", predicate_id)
        }
    }
}

impl Judge for DefaultLaw {
    type Payload = serde_json::Value;
    type Law = DefaultLaw;
    type Error = String;

    fn judge(
        raw: LawObject<Self::Payload, Raw, Self::Law>,
    ) -> Result<
        LawObject<Self::Payload, Validated, Self::Law>,
        LawObject<Self::Payload, Raw, Self::Law>,
    > {
        let unmet: Vec<Obligation> = raw
            .obligations()
            .iter()
            .filter(|ob| !obligation_met(raw.payload(), ob))
            .cloned()
            .collect();

        if unmet.is_empty() {
            Ok(raw.transition())
        } else {
            let at_ms = match now_ms() {
                Ok(t) => t,
                Err(e) => {
                    // Propagate time failure as a halt reason rather than silently defaulting to 0
                    let mut halted = raw;
                    halted.andon = Andon::Halted {
                        unmet: vec![Obligation::BlockingConstraint { reason: e }],
                        refusals: vec![RefusalScenario::WatchdogDrained],
                        at: 0,
                    };
                    return Err(halted);
                }
            };

            let refusals: Vec<RefusalScenario> = unmet.iter().map(RefusalScenario::from).collect();
            let mut halted = raw;
            halted.andon = Andon::Halted {
                unmet,
                refusals,
                at: at_ms,
            };
            Err(halted)
        }
    }
}

impl Admit for DefaultLaw {
    type Payload = serde_json::Value;
    type Law = DefaultLaw;
    type Witness = ();

    fn admit(
        validated: LawObject<Self::Payload, Validated, Self::Law>,
    ) -> Result<LawObject<Self::Payload, Admitted, Self::Law>, Andon> {
        match validated.andon() {
            Andon::Green | Andon::Overridden { .. } => Ok(validated.transition()),
            Andon::Halted { .. } => {
                let andon = validated.andon().clone();
                Err(andon)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use crate::law::ReceiptMeta;

    // `LawObject` intentionally does not derive `Debug` (its phantom stage/law
    // markers are not all `Debug`), so tests unwrap `Result`s via `match`
    // instead of `.expect()`/`.expect_err()`.

    fn no_obligations() -> LawObject<serde_json::Value, Raw, DefaultLaw> {
        LawObject::<serde_json::Value, Raw, DefaultLaw>::new(json!({"id": 1}), vec![])
    }

    #[test]
    fn judge_succeeds_with_no_obligations() {
        let raw = no_obligations();
        match DefaultLaw::judge(raw) {
            Ok(validated) => assert!(matches!(validated.andon(), Andon::Green)),
            Err(_) => panic!("no obligations should always validate"),
        }
    }

    #[test]
    fn judge_respects_evidence_required_met() {
        let payload = json!({"evidence": ["lab_report"]});
        let raw: LawObject<serde_json::Value, Raw, DefaultLaw> =
            LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
                payload,
                vec![Obligation::EvidenceRequired {
                    evidence_type: "lab_report".to_string(),
                }],
            );
        match DefaultLaw::judge(raw) {
            Ok(validated) => assert!(matches!(validated.andon(), Andon::Green)),
            Err(_) => panic!("evidence present should validate"),
        }
    }

    #[test]
    fn judge_respects_evidence_required_unmet() {
        let payload = json!({"evidence": ["other"]});
        let raw: LawObject<serde_json::Value, Raw, DefaultLaw> =
            LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
                payload,
                vec![Obligation::EvidenceRequired {
                    evidence_type: "lab_report".to_string(),
                }],
            );
        match DefaultLaw::judge(raw) {
            Ok(_) => panic!("missing evidence should halt"),
            Err(halted) => match halted.andon() {
                Andon::Halted { unmet, .. } => assert_eq!(unmet.len(), 1),
                _ => panic!("expected Halted"),
            },
        }
    }

    #[test]
    fn judge_respects_precondition_met() {
        let payload = json!({"satisfied_predicates": ["p1"]});
        let raw: LawObject<serde_json::Value, Raw, DefaultLaw> =
            LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
                payload,
                vec![Obligation::Precondition {
                    predicate_id: "p1".to_string(),
                    params_hash: [0u8; 32],
                }],
            );
        match DefaultLaw::judge(raw) {
            Ok(validated) => assert!(matches!(validated.andon(), Andon::Green)),
            Err(_) => panic!("satisfied predicate should validate"),
        }
    }

    #[test]
    fn judge_respects_precondition_unmet() {
        let payload = json!({"satisfied_predicates": []});
        let raw: LawObject<serde_json::Value, Raw, DefaultLaw> =
            LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
                payload,
                vec![Obligation::Precondition {
                    predicate_id: "p1".to_string(),
                    params_hash: [0u8; 32],
                }],
            );
        match DefaultLaw::judge(raw) {
            Ok(_) => panic!("unsatisfied predicate should halt"),
            Err(halted) => match halted.andon() {
                Andon::Halted { unmet, .. } => assert_eq!(unmet.len(), 1),
                _ => panic!("expected Halted"),
            },
        }
    }

    #[test]
    fn judge_always_halts_on_blocking_constraint() {
        let payload = json!({"evidence": [], "satisfied_predicates": []});
        let raw: LawObject<serde_json::Value, Raw, DefaultLaw> =
            LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
                payload,
                vec![Obligation::BlockingConstraint {
                    reason: "manual review required".to_string(),
                }],
            );
        match DefaultLaw::judge(raw) {
            Ok(_) => panic!("blocking constraint can never be met"),
            Err(halted) => match halted.andon() {
                Andon::Halted { unmet, .. } => assert_eq!(unmet.len(), 1),
                _ => panic!("expected Halted"),
            },
        }
    }

    #[test]
    fn admit_succeeds_on_green_andon() {
        // `receipt()` below signs the chain hash when `signed` is enabled,
        // which needs a `PRAXIS_SIGNING_KEY`; see `signing::test_support`.
        #[cfg(feature = "signed")]
        let _guard = crate::signing::test_support::with_test_signing_key();

        let raw = no_obligations();
        let validated = match DefaultLaw::judge(raw) {
            Ok(v) => v,
            Err(_) => panic!("no obligations should validate"),
        };
        let admitted = match DefaultLaw::admit(validated) {
            Ok(a) => a,
            Err(_) => panic!("green andon should admit"),
        };
        assert!(matches!(admitted.andon(), Andon::Green));

        // Sanity: the admitted object can flow into receipt() using the
        // fixed payload-binding path from Task 2.
        let receipted = match admitted.receipt(&[0u8; 32], ReceiptMeta::default()) {
            Ok(r) => r,
            Err(_) => panic!("receipt should succeed"),
        };
        assert!(receipted.chain_hash().is_some());
    }
}
