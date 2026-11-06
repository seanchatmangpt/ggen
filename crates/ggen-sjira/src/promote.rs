//! Promotion calculus — port of `promote/4` (semantic_jira.ex lines
//! 603-700, plus the check helpers at lines 700-760).
//!
//! `promote(order, target, evidence)` judges an already-admitted order's
//! promotion evidence and returns a [`PromotionIntent`] — a pure
//! `standing_transition_intent` value whose `transition_digest` is
//! `work_order::digest_of` over the canonical intent (the Elixir
//! `digest/1`, i.e. `digest_exact/1` ≡ `digest_of`).
//!
//! Port scope (disclosed divergences, per the igniter moduledoc law):
//! - the Elixir `promote/4` admits internally and resolves the origin
//!   through `Authority.require_origin/2` (opts-carried); this port takes a
//!   pre-admitted [`AdmittedWorkOrder`] and is pure/IO-free — callers
//!   compose `admit_work_order` and the authority gate themselves
//!   (descriptor/reconcile slices do exactly that);
//! - the `dependencies` check is vacuously true in this port: the core
//!   fifteen-field `WorkOrder` struct carries no `dependencies` field (the
//!   igniter's normalized map defaults it to `[]` when absent);
//! - the igniter's failed-check list maps 1:1 to typed refusals except two
//!   checks the roadmap's refusal list did not name: an unwitnessed
//!   acceptance item refuses `AcceptanceUnwitnessed` and
//!   `inherited_standing == true` refuses `InheritedStanding` (the
//!   no-inherited-crown law) — both added as typed variants rather than
//!   folded into another variant's payload.
//!
//! `#![forbid(unsafe_code)]` crate-wide. IO-free; no unwrap on external
//! input; typed refusals fail closed.

use serde_json::{Map, Value};

use crate::admit::AdmittedWorkOrder;
use crate::work_order::digest_of;

/// Typed promotion refusal (`{:error, {:refused_promotion, reason}}`
/// family, semantic_jira.ex 603-760).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum PromoteRefusal {
    #[error(
        "UnknownStanding: target {target:?} is not a legal standing name — remediation: \
         promote to one of UNKNOWN, PARTIAL_ALIVE, ALIVE, BLOCKED, BUILD_BROKEN, \
         UNSUPPORTED, or REFUSED(<reason>)"
    )]
    UnknownStanding { target: String },

    #[error(
        "InvalidTarget: target {target:?} — remediation: admission owns UNKNOWN; \
         promotion targets a standing strictly above UNKNOWN"
    )]
    InvalidTarget { target: String },

    #[error(
        "UnwitnessedCourt: required court {court:?} has no passing witness in \
         court_results — remediation: run the court and present its passing result"
    )]
    UnwitnessedCourt { court: String },

    #[error(
        "AcceptanceUnwitnessed: acceptance item {name:?} is not true in \
         acceptance_results — remediation: run the acceptance check and present \
         its result"
    )]
    AcceptanceUnwitnessed { name: String },

    #[error(
        "EvidenceClassRefused: required evidence class {class:?} is missing from \
         evidence_types — remediation: present the class (required: {required:?})"
    )]
    EvidenceClassRefused { class: String, required: String },

    #[error(
        "EvidenceCeilingExceeded: evidence ceiling {declared:?} does not satisfy the \
         order's declared ceiling {required:?} for an ALIVE promotion — remediation: \
         raise the order's evidence ceiling or present observed_execution"
    )]
    EvidenceCeilingExceeded { declared: String, required: String },

    #[error(
        "FalsifierUnsurvived: falsifier {falsifier:?} did not survive — remediation: \
         every required falsifier must return survived"
    )]
    FalsifierUnsurvived { falsifier: String },

    #[error(
        "ReplayIdentityMismatch — remediation: replay evidence must carry \
         replay_passed=true and replay_identity equal to the order's replay_identity"
    )]
    ReplayIdentityMismatch,

    #[error(
        "InheritedStanding — remediation: standing never inherits; the evidence must \
         not set inherited_standing=true (no-inherited-crown law)"
    )]
    InheritedStanding,
}

/// The promotion calculus's output: a `standing_transition_intent` with a
/// recomputed `transition_digest` (`digest_exact` over the intent).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PromotionIntent {
    /// The intent map; `transition_digest` is `digest_of` over the same
    /// map with its digest key removed (self-referential exclusion law).
    pub intent: Value,
    pub transition_digest: String,
}

/// The promotion calculus (reverse-80/20: pure admission judging
/// courts/evidence/ceiling/replay). Checks run in the Elixir
/// `promotion_checks/4` order; the first failure is the typed refusal
/// (the igniter returns the failed-name list; this port fails on the first).
pub fn promote(
    order: &AdmittedWorkOrder, target: &str, evidence: &Value,
) -> Result<PromotionIntent, PromoteRefusal> {
    // standing/1 (line ~1390): the target must be a legal standing name,
    // and admission owns UNKNOWN — promotion targets above it.
    if target == "UNKNOWN" {
        return Err(PromoteRefusal::UnknownStanding {
            target: target.to_string(),
        });
    }
    if crate::work_order::Standing::parse(target).is_err() {
        return Err(PromoteRefusal::InvalidTarget {
            target: target.to_string(),
        });
    }

    let ev = |key: &str| evidence.get(key);

    // exact_subject? — evidence binds to this exact work-order snapshot.
    let subject_exact = ev("work_order_digest")
        == Some(&Value::String(order.work_order_digest.clone()))
        && ev("subject") == Some(&Value::String(order.work_order.subject.clone()))
        && ev("repository") == Some(&Value::String(order.work_order.repository.clone()))
        && ev("base_sha") == Some(&Value::String(order.work_order.base_sha.clone()));
    if !subject_exact {
        return Err(PromoteRefusal::ReplayIdentityMismatch);
    }

    // courts_satisfied? (line ~716): every required court has a passing
    // witness under court_results.
    for court in &order.work_order.required_courts {
        let passed = evidence
            .get("court_results")
            .and_then(|c| c.get(court))
            .and_then(|c| c.get("passed"))
            == Some(&Value::Bool(true));
        if !passed {
            return Err(PromoteRefusal::UnwitnessedCourt {
                court: court.clone(),
            });
        }
    }

    // subset?(required_evidence, evidence_types) (line ~712).
    let types = evidence
        .get("evidence_types")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    for class in &order.work_order.required_evidence {
        if !types.iter().any(|t| t == class) {
            return Err(PromoteRefusal::EvidenceClassRefused {
                class: class.clone(),
                required: types
                    .iter()
                    .filter_map(|t| t.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
            });
        }
    }

    // acceptance_satisfied? (line ~722): every acceptance item true.
    for name in &order.work_order.acceptance {
        let ok = evidence.get("acceptance_results").and_then(|a| a.get(name))
            == Some(&Value::Bool(true));
        if !ok {
            return Err(PromoteRefusal::AcceptanceUnwitnessed { name: name.clone() });
        }
    }

    // falsifiers_satisfied? (line ~726): every falsifier in
    // falsifier_results survived ("survived" | true).
    for f in &order.work_order.falsifiers {
        let survived = evidence
            .get("falsifier_results")
            .and_then(|r| r.get(f))
            .map(|v| v == &Value::Bool(true) || v.as_str() == Some("survived"))
            .unwrap_or(false);
        if !survived {
            return Err(PromoteRefusal::FalsifierUnsurvived {
                falsifier: f.clone(),
            });
        }
    }

    // replay_satisfied? (line ~720): replay_required ⇒ replay_passed &&
    // identity match.
    let replay_required = evidence.get("replay_required") == Some(&Value::Bool(true));
    if replay_required {
        let passed = evidence.get("replay_passed") == Some(&Value::Bool(true))
            && ev("replay_identity")
                == Some(&Value::String(order.work_order.replay_identity.clone()));
        if !passed {
            return Err(PromoteRefusal::ReplayIdentityMismatch);
        }
    }

    // ceiling_satisfied? (line ~724): ALIVE requires the evidence ceiling
    // to equal the order's declared ceiling plus observed execution.
    if target == "ALIVE" {
        let ev_ceiling = ev("evidence_ceiling")
            .and_then(|v| v.as_str())
            .unwrap_or("");
        let observed = evidence.get("observed_execution") == Some(&Value::Bool(true));
        if ev_ceiling != order.work_order.evidence_ceiling || !observed {
            return Err(PromoteRefusal::EvidenceCeilingExceeded {
                declared: ev_ceiling.to_string(),
                required: order.work_order.evidence_ceiling.clone(),
            });
        }
    }

    // no_inherited_crown (line ~748): standing never inherits.
    if evidence.get("inherited_standing") == Some(&Value::Bool(true)) {
        return Err(PromoteRefusal::InheritedStanding);
    }

    build_intent(order, target)
}

/// Assembles the `standing_transition_intent` (Elixir `promotion_result/3`,
/// all-checks-passed arm) with the recomputed `transition_digest`.
fn build_intent(
    order: &AdmittedWorkOrder, target: &str,
) -> Result<PromotionIntent, PromoteRefusal> {
    let mut m = Map::new();
    m.insert("authority".to_string(), Value::String("NONE".to_string()));
    m.insert(
        "checks".to_string(),
        Value::Object(Map::new()), // every check true; the map is carried
    );
    m.insert(
        "from".to_string(),
        Value::String(order.work_order.standing.as_str()),
    );
    m.insert(
        "kind".to_string(),
        Value::String("standing_transition_intent".to_string()),
    );
    m.insert("to".to_string(), Value::String(target.to_string()));
    m.insert(
        "work_order_digest".to_string(),
        Value::String(order.work_order_digest.clone()),
    );
    let intent_no_digest = Value::Object(m);
    let transition_digest = digest_of(&intent_no_digest);

    let mut with_digest = match intent_no_digest.clone() {
        Value::Object(mut m2) => {
            m2.insert(
                "transition_digest".to_string(),
                Value::String(transition_digest.clone()),
            );
            Value::Object(m2)
        }
        _ => unreachable!("intent is an object"),
    };
    let _ = &mut with_digest;
    Ok(PromotionIntent {
        intent: with_digest,
        transition_digest,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admit::admit_work_order;
    use serde_json::json;

    fn order_json() -> Value {
        json!({
            "identity": "X1",
            "title": "t",
            "description": "d",
            "subject": "src/x1.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "standing": "UNKNOWN",
            "evidence_ceiling": "VERIFIED",
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

    fn admitted() -> crate::admit::AdmittedWorkOrder {
        admit_work_order(&order_json()).expect("fixture order admits")
    }

    fn evidence() -> Value {
        json!({
            "work_order_digest": admitted().work_order_digest,
            "subject": "src/x1.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "court_results": {"c1": {"passed": true}},
            "evidence_types": ["e1"],
            "acceptance_results": {"a1": true},
            "falsifier_results": {"f1": "survived"},
            "evidence_ceiling": "VERIFIED",
            "observed_execution": true
        })
    }

    #[test]
    fn happy_path_builds_digest_bound_intent() {
        let intent = promote(&admitted(), "PARTIAL_ALIVE", &evidence()).unwrap();
        assert_eq!(intent.intent["kind"], "standing_transition_intent");
        assert_eq!(intent.intent["to"], "PARTIAL_ALIVE");
        assert_eq!(intent.intent["from"], "UNKNOWN");
        assert_eq!(intent.intent["authority"], "NONE");
        assert_eq!(intent.intent["transition_digest"], intent.transition_digest);
        // the digest recomputes over the intent minus its digest key
        let mut no_digest = intent.intent.clone();
        no_digest
            .as_object_mut()
            .unwrap()
            .remove("transition_digest");
        assert_eq!(digest_of(&no_digest), intent.transition_digest);
    }

    #[test]
    fn unknown_target_standing_refuses() {
        assert_eq!(
            promote(&admitted(), "UNKNOWN", &evidence()),
            Err(PromoteRefusal::UnknownStanding {
                target: "UNKNOWN".to_string()
            })
        );
        assert_eq!(
            promote(&admitted(), "NONSENSE", &evidence()),
            Err(PromoteRefusal::InvalidTarget {
                target: "NONSENSE".to_string()
            })
        );
    }

    #[test]
    fn unwitnessed_court_refuses() {
        let mut ev = evidence();
        ev["court_results"]["c1"]["passed"] = json!(false);
        assert_eq!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev),
            Err(PromoteRefusal::UnwitnessedCourt {
                court: "c1".to_string()
            })
        );
    }

    #[test]
    fn evidence_class_and_acceptance_and_falsifier_refuse() {
        let mut ev = evidence();
        ev["evidence_types"] = json!([]);
        assert!(matches!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev),
            Err(PromoteRefusal::EvidenceClassRefused { .. })
        ));

        let mut ev2 = evidence();
        ev2["acceptance_results"]["a1"] = json!(false);
        assert_eq!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev2),
            Err(PromoteRefusal::AcceptanceUnwitnessed {
                name: "a1".to_string()
            })
        );

        let mut ev3 = evidence();
        ev3["falsifier_results"]["f1"] = json!("died");
        assert_eq!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev3),
            Err(PromoteRefusal::FalsifierUnsurvived {
                falsifier: "f1".to_string()
            })
        );
    }

    #[test]
    fn alive_requires_ceiling_match_and_observed_execution() {
        let mut ev = evidence();
        ev["evidence_ceiling"] = json!("UNVERIFIED");
        assert_eq!(
            promote(&admitted(), "ALIVE", &ev),
            Err(PromoteRefusal::EvidenceCeilingExceeded {
                declared: "UNVERIFIED".to_string(),
                required: "VERIFIED".to_string(),
            })
        );
        let mut ev2 = evidence();
        ev2["observed_execution"] = json!(false);
        assert!(matches!(
            promote(&admitted(), "ALIVE", &ev2),
            Err(PromoteRefusal::EvidenceCeilingExceeded { .. })
        ));
        // PARTIAL_ALIVE does not trip the ceiling gate.
        assert!(promote(&admitted(), "PARTIAL_ALIVE", &ev).is_ok());
    }

    #[test]
    fn replay_gate_and_inherited_crown_refuse() {
        let mut ev = evidence();
        ev["replay_required"] = json!(true);
        assert_eq!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev),
            Err(PromoteRefusal::ReplayIdentityMismatch)
        );
        let mut ev2 = evidence();
        ev2["inherited_standing"] = json!(true);
        assert_eq!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev2),
            Err(PromoteRefusal::InheritedStanding)
        );
    }

    #[test]
    fn subject_binding_refuses_on_any_mismatch() {
        let mut ev = evidence();
        ev["subject"] = json!("src/other.rs");
        assert!(matches!(
            promote(&admitted(), "PARTIAL_ALIVE", &ev),
            Err(PromoteRefusal::ReplayIdentityMismatch)
        ));
    }
}
