//! Praxis-native refusal taxonomy: category buckets + concrete scenarios.
//!
//! # Prior art
//!
//! The 8-bucket [`RefusalCategory`] classification and the overall shape of
//! [`RefusalScenario`] are a **design port** of `stpnt::refusal::{RefusalCategory,
//! RefusalScenario}` (see `/Users/sac/stpnt/src/refusal.rs`). stpnt is *not* a
//! dependency of praxis: its `Cargo.toml` has no `license` field, which is a
//! real blocker for depending on it. Only the taxonomy pattern (category
//! buckets + a closed enum of concrete refusal scenarios, each mapping to
//! exactly one bucket) is ported here, reimplemented from scratch against
//! praxis's own [`crate::law::Obligation`] and `bcinr_powl_receipt`'s
//! `DenialPolarity` lanes rather than stpnt's stewardship domain.
//!
//! # What maps to what
//!
//! - [`Obligation`] (the 3 kinds unmet during `Judge`) → [`RefusalScenario`]
//!   via [`From<&Obligation>`].
//! - `DenialPolarity`'s 7 non-`ADMITTED` lanes → [`RefusalScenario`] via
//!   [`scenario_for_denial_lane`], with [`denial_lane`] as the inverse.
//! - Kernel (`prolog8::Kernel::query`) and andon-ring outcomes → the
//!   `Kernel*`/`AndonInvariantViolated` variants, added in later steps of the
//!   admission-enrichment lane.
//!
//! Every mapping in this module is total: [`RefusalScenario::category`] and
//! [`denial_lane`] match every variant with no wildcard arm, so adding a new
//! `RefusalScenario` variant without updating both functions is a compile
//! error. [`scenario_for_denial_lane`] cannot be a similarly exhaustive
//! `match` because `DenialPolarity` is an external newtype over `u64` (an
//! open 2^64 value space, not a closed enum) — it is instead an `if`/`else`
//! chain over the 8 known constants, ending in `None` for any other word
//! (e.g. a composed multi-lane value), which is covered by a dedicated test
//! rather than the type system.

use bcinr_powl_receipt::denial::DenialPolarity;
use serde::{Deserialize, Serialize};

use crate::law::Obligation;

/// 8-bucket refusal classification. Design ported from stpnt (see module
/// docs) as prior art; stpnt is not a dependency.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RefusalCategory {
    /// Caller/subject identity or ID-format problem.
    Identity,
    /// Resource capacity or quota limit reached.
    Capacity,
    /// Graph/topology (structural) violation.
    Topology,
    /// Temporal or deadline constraint violated.
    Temporal,
    /// Lifecycle / state-machine position violation.
    Lifecycle,
    /// Authorization or credential failure.
    Authorization,
    /// Event-level prerequisite (evidence, precondition) missing.
    Prerequisites,
    /// Reserved for future enforcement.
    Reserved,
}

impl std::fmt::Display for RefusalCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Identity => "identity",
            Self::Capacity => "capacity",
            Self::Topology => "topology",
            Self::Temporal => "temporal",
            Self::Lifecycle => "lifecycle",
            Self::Authorization => "authorization",
            Self::Prerequisites => "prerequisites",
            Self::Reserved => "reserved",
        };
        f.write_str(s)
    }
}

/// Praxis-native refusal scenarios: obligation-driven halts, the 7
/// non-`ADMITTED` `DenialPolarity` lanes, and (added by later steps of the
/// admission-enrichment lane) prolog8 Kernel and andon-ring denials.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RefusalScenario {
    // ── Obligation-driven (from `Obligation`, via `judge`) ──────────────
    /// An [`Obligation::BlockingConstraint`] was in force (never satisfiable
    /// by payload content alone).
    BlockingConstraint {
        /// The obligation's stated reason.
        reason: String,
    },
    /// An [`Obligation::EvidenceRequired`] was not satisfied.
    MissingEvidence {
        /// The evidence type that was required but absent.
        evidence_type: String,
    },
    /// An [`Obligation::Precondition`] was not satisfied.
    UnsatisfiedPrecondition {
        /// The predicate id that was required but not in `satisfied_predicates`.
        predicate_id: String,
    },

    // ── DenialPolarity lanes (bcinr_powl_receipt, non-ADMITTED) ─────────
    /// `DenialPolarity::WATCHDOG_DRAINED`.
    WatchdogDrained,
    /// `DenialPolarity::PRECONDITION_FAILED`.
    PreconditionFailed,
    /// `DenialPolarity::SLA_BREACH`.
    SlaBreach,
    /// `DenialPolarity::AUTHORIZATION_DENIED`.
    AuthorizationDenied,
    /// `DenialPolarity::RESOURCE_EXHAUSTED`.
    ResourceExhausted,
    /// `DenialPolarity::OBJECT_LIFECYCLE_VIOLATION`.
    ObjectLifecycleViolation,
    /// `DenialPolarity::CONFORMANCE_GATE_FAILED`.
    ConformanceGateFailed,

    // ── Kernel / andon lanes ─────────────────────────────────────────────
    /// `prolog8::Kernel::query` returned `QueryResult::Denied`: the query
    /// was admissible but the kernel found no supporting derivation.
    KernelDenied {
        /// The raw `PredicateId` (as `u32`) of the denied query atom.
        pred_id: u32,
    },
    /// `prolog8::Kernel::query` returned `QueryResult::Invalid`: the query,
    /// facts, or rule failed prolog8 admission before execution.
    KernelInvalid {
        /// `Display` text of the prolog8 `RejectionCode`.
        rejection: String,
    },
    /// The optional lsp-max andon second gate ring fired a blocking,
    /// non-admission-allowed event.
    AndonInvariantViolated {
        /// The `AndonInvariant`/`AndonEvent` id that fired.
        invariant_id: String,
    },
}

impl RefusalScenario {
    /// The 8-bucket category this scenario belongs to. Exhaustive: every
    /// variant is matched explicitly, no wildcard arm.
    #[must_use]
    pub fn category(&self) -> RefusalCategory {
        match self {
            Self::BlockingConstraint { .. } => RefusalCategory::Lifecycle,
            Self::MissingEvidence { .. } => RefusalCategory::Prerequisites,
            Self::UnsatisfiedPrecondition { .. } => RefusalCategory::Prerequisites,
            Self::WatchdogDrained => RefusalCategory::Temporal,
            Self::PreconditionFailed => RefusalCategory::Prerequisites,
            Self::SlaBreach => RefusalCategory::Temporal,
            Self::AuthorizationDenied => RefusalCategory::Authorization,
            Self::ResourceExhausted => RefusalCategory::Capacity,
            Self::ObjectLifecycleViolation => RefusalCategory::Lifecycle,
            Self::ConformanceGateFailed => RefusalCategory::Topology,
            Self::KernelDenied { .. } => RefusalCategory::Authorization,
            Self::KernelInvalid { .. } => RefusalCategory::Identity,
            Self::AndonInvariantViolated { .. } => RefusalCategory::Topology,
        }
    }
}

/// Convert an unmet [`Obligation`] into the matching [`RefusalScenario`].
/// Exhaustive: every `Obligation` variant is matched explicitly.
impl From<&Obligation> for RefusalScenario {
    fn from(obligation: &Obligation) -> Self {
        match obligation {
            Obligation::BlockingConstraint { reason } => RefusalScenario::BlockingConstraint {
                reason: reason.clone(),
            },
            Obligation::EvidenceRequired { evidence_type } => RefusalScenario::MissingEvidence {
                evidence_type: evidence_type.clone(),
            },
            Obligation::Precondition { predicate_id, .. } => {
                RefusalScenario::UnsatisfiedPrecondition {
                    predicate_id: predicate_id.clone(),
                }
            }
        }
    }
}

/// Map a single `DenialPolarity` lane constant to the `RefusalScenario` that
/// fires it, or `None` for `ADMITTED` (or any word that isn't one of the 7
/// known single-lane constants — see module docs for why this can't be a
/// wildcard-free `match`).
#[must_use]
pub fn scenario_for_denial_lane(lane: DenialPolarity) -> Option<RefusalScenario> {
    if lane == DenialPolarity::ADMITTED {
        None
    } else if lane == DenialPolarity::WATCHDOG_DRAINED {
        Some(RefusalScenario::WatchdogDrained)
    } else if lane == DenialPolarity::PRECONDITION_FAILED {
        Some(RefusalScenario::PreconditionFailed)
    } else if lane == DenialPolarity::SLA_BREACH {
        Some(RefusalScenario::SlaBreach)
    } else if lane == DenialPolarity::AUTHORIZATION_DENIED {
        Some(RefusalScenario::AuthorizationDenied)
    } else if lane == DenialPolarity::RESOURCE_EXHAUSTED {
        Some(RefusalScenario::ResourceExhausted)
    } else if lane == DenialPolarity::OBJECT_LIFECYCLE_VIOLATION {
        Some(RefusalScenario::ObjectLifecycleViolation)
    } else if lane == DenialPolarity::CONFORMANCE_GATE_FAILED {
        Some(RefusalScenario::ConformanceGateFailed)
    } else {
        None
    }
}

/// Inverse of [`scenario_for_denial_lane`]: which `DenialPolarity` lane a
/// scenario composes into a receipt's denial word. Exhaustive: every
/// `RefusalScenario` variant is matched explicitly, no wildcard arm.
///
/// Obligation-driven scenarios (which have no native bcinr lane of their
/// own) map onto the closest-fit lane: `BlockingConstraint` and
/// `ObjectLifecycleViolation` both represent "progress is blocked by
/// lifecycle state" → `OBJECT_LIFECYCLE_VIOLATION`; `MissingEvidence` and
/// `UnsatisfiedPrecondition` both represent "a declared precondition wasn't
/// met" → `PRECONDITION_FAILED`. The prolog8/andon lanes (which also have no
/// native bcinr lane) all compose into `CONFORMANCE_GATE_FAILED`, since they
/// are, structurally, a process-conformance gate rejecting the step.
#[must_use]
pub fn denial_lane(scenario: &RefusalScenario) -> DenialPolarity {
    match scenario {
        RefusalScenario::BlockingConstraint { .. } => DenialPolarity::OBJECT_LIFECYCLE_VIOLATION,
        RefusalScenario::MissingEvidence { .. } => DenialPolarity::PRECONDITION_FAILED,
        RefusalScenario::UnsatisfiedPrecondition { .. } => DenialPolarity::PRECONDITION_FAILED,
        RefusalScenario::WatchdogDrained => DenialPolarity::WATCHDOG_DRAINED,
        RefusalScenario::PreconditionFailed => DenialPolarity::PRECONDITION_FAILED,
        RefusalScenario::SlaBreach => DenialPolarity::SLA_BREACH,
        RefusalScenario::AuthorizationDenied => DenialPolarity::AUTHORIZATION_DENIED,
        RefusalScenario::ResourceExhausted => DenialPolarity::RESOURCE_EXHAUSTED,
        RefusalScenario::ObjectLifecycleViolation => DenialPolarity::OBJECT_LIFECYCLE_VIOLATION,
        RefusalScenario::ConformanceGateFailed => DenialPolarity::CONFORMANCE_GATE_FAILED,
        RefusalScenario::KernelDenied { .. } => DenialPolarity::CONFORMANCE_GATE_FAILED,
        RefusalScenario::KernelInvalid { .. } => DenialPolarity::CONFORMANCE_GATE_FAILED,
        RefusalScenario::AndonInvariantViolated { .. } => DenialPolarity::CONFORMANCE_GATE_FAILED,
    }
}

/// Fold a set of refusal scenarios into a single composed `DenialPolarity`
/// word (OR of each scenario's [`denial_lane`]). Empty input composes to
/// `ADMITTED` (the identity element of `compose`).
#[must_use]
pub fn compose_denials<'a>(
    scenarios: impl IntoIterator<Item = &'a RefusalScenario>,
) -> DenialPolarity {
    scenarios
        .into_iter()
        .map(denial_lane)
        .fold(DenialPolarity::ADMITTED, DenialPolarity::compose)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The 3 `Obligation` kinds each map to exactly one `RefusalCategory`
    /// via `RefusalScenario::from`.
    #[test]
    fn category_mapping_is_total_over_obligations() {
        let cases: &[(Obligation, RefusalCategory)] = &[
            (
                Obligation::BlockingConstraint {
                    reason: "x".to_string(),
                },
                RefusalCategory::Lifecycle,
            ),
            (
                Obligation::EvidenceRequired {
                    evidence_type: "lab".to_string(),
                },
                RefusalCategory::Prerequisites,
            ),
            (
                Obligation::Precondition {
                    predicate_id: "p1".to_string(),
                    params_hash: [0u8; 32],
                },
                RefusalCategory::Prerequisites,
            ),
        ];
        for (obligation, expected) in cases {
            let scenario = RefusalScenario::from(obligation);
            assert_eq!(scenario.category(), *expected, "obligation: {obligation:?}");
        }
    }

    /// All 7 non-ADMITTED `DenialPolarity` lanes map to `Some` with the
    /// expected category, `ADMITTED` maps to `None`, and the mapping
    /// round-trips through `denial_lane`.
    #[test]
    fn category_mapping_is_total_over_denial_lanes() {
        let cases: &[(DenialPolarity, RefusalCategory)] = &[
            (DenialPolarity::WATCHDOG_DRAINED, RefusalCategory::Temporal),
            (
                DenialPolarity::PRECONDITION_FAILED,
                RefusalCategory::Prerequisites,
            ),
            (DenialPolarity::SLA_BREACH, RefusalCategory::Temporal),
            (
                DenialPolarity::AUTHORIZATION_DENIED,
                RefusalCategory::Authorization,
            ),
            (
                DenialPolarity::RESOURCE_EXHAUSTED,
                RefusalCategory::Capacity,
            ),
            (
                DenialPolarity::OBJECT_LIFECYCLE_VIOLATION,
                RefusalCategory::Lifecycle,
            ),
            (
                DenialPolarity::CONFORMANCE_GATE_FAILED,
                RefusalCategory::Topology,
            ),
        ];
        for (lane, expected_category) in cases {
            let scenario = scenario_for_denial_lane(*lane)
                .unwrap_or_else(|| panic!("lane {lane:?} should map to Some(_)"));
            assert_eq!(scenario.category(), *expected_category, "lane: {lane:?}");
            assert_eq!(
                denial_lane(&scenario),
                *lane,
                "denial_lane(scenario_for_denial_lane(lane)) should round-trip to lane"
            );
        }

        assert!(scenario_for_denial_lane(DenialPolarity::ADMITTED).is_none());
    }

    #[test]
    fn kernel_and_andon_scenarios_have_expected_categories() {
        assert_eq!(
            RefusalScenario::KernelDenied { pred_id: 1 }.category(),
            RefusalCategory::Authorization
        );
        assert_eq!(
            RefusalScenario::KernelInvalid {
                rejection: "x".to_string()
            }
            .category(),
            RefusalCategory::Identity
        );
        assert_eq!(
            RefusalScenario::AndonInvariantViolated {
                invariant_id: "x".to_string()
            }
            .category(),
            RefusalCategory::Topology
        );
    }

    #[test]
    fn compose_denials_ors_lanes() {
        let scenarios = vec![
            RefusalScenario::PreconditionFailed,
            RefusalScenario::WatchdogDrained,
        ];
        let composed = compose_denials(&scenarios);
        assert_eq!(
            composed.0,
            DenialPolarity::PRECONDITION_FAILED.0 | DenialPolarity::WATCHDOG_DRAINED.0
        );

        let empty: Vec<RefusalScenario> = vec![];
        assert_eq!(compose_denials(&empty), DenialPolarity::ADMITTED);
    }
}
