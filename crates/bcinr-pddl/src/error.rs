//! `bcinr-pddl`'s error/outcome types.
//!
//! `PlannerOutcome<T>` used to be defined locally here as a lossy four-way
//! enum (`Found(T) | Exhausted | Bounded(usize) | Unsupported(String) |
//! Inconsistent`) that collapsed every non-`Found` case to a bare unit or a
//! single primitive by the time `into_result()` ran. That type has been
//! **removed** in favor of re-exporting [`bcinr_mfw_ir::PlannerOutcome`],
//! which keeps a full witness on every non-`Found` variant
//! (`ExhaustionWitness`, `BoundHit`, `UnsupportedFeature`,
//! `InconsistencyWitness`) — this is the Phase 2 reconciliation called out in
//! `bcinr-mfw-ir/src/outcome.rs`'s module doc comment. There is exactly one
//! `PlannerOutcome<T>` in this workspace now, not two parallel types.
//!
//! `Pddl8Error` stays the closed, PDDL-pipeline-shaped error enum it always
//! was, plus one new variant, [`Pddl8Error::PlanningFailed`], that wraps
//! [`bcinr_mfw_ir::PlannerFailure`] without stringifying or discarding it —
//! this is what lets every existing `.into_result()?` call site (which
//! expects `Result<T, Pddl8Error>`) keep working via `From`/`?` while
//! actually preserving the witness, instead of collapsing every failure onto
//! `NoAdmittedPlan` the way the old local `PlannerOutcome` did.
pub use bcinr_mfw_ir::PlannerOutcome;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pddl8Error {
    /// PDDL text could not be parsed.
    ParseError(String),
    /// A structural bound was exceeded (arity, body atoms, variables, depth).
    ///
    /// `limit` is `usize`, matching `got` and every bound constant that
    /// feeds it (`PDDL8_MAX_GROUND` = 4096, plus any caller-supplied
    /// `max_ground: Option<usize>`) — it was previously `u8`, which silently
    /// truncated any bound above 255 (`4096usize as u8 == 0`, so a domain
    /// that actually exceeded `PDDL8_MAX_GROUND` reported `limit: 0` instead
    /// of the real bound). A refusal's witness must report the bound that
    /// was actually hit.
    BoundExceeded {
        what: &'static str,
        limit: usize,
        got: usize,
    },
    /// An action schema references an unknown predicate or type.
    UnknownPredicate(String),
    /// Grounding produced zero applicable actions — plan search space is empty.
    EmptyGrounding,
    /// The planner exhausted bounded search without reaching the goal.
    NoAdmittedPlan,
    /// Prolog8 admission kernel rejected a rule at load time.
    AdmissionLoadError(String),
    /// An op fired but Prolog8 denied it at runtime.
    StepDenied { op_index: u8, reason: String },
    /// Goal was not reached after executing all admitted steps.
    GoalNotReached,
    /// Receipt chain integrity failure.
    ReceiptIntegrity(String),
    /// case_id contains disallowed characters or is out of the 1-64 char range.
    InvalidCaseId(String),
    /// A bounded search/analysis stage (`PlannerOutcome<T>`) did not produce
    /// `Found` — carries the *full* typed witness
    /// (`bcinr_mfw_ir::PlannerFailure`: `Exhausted`/`Bounded`/`Unsupported`/
    /// `Inconsistent`, each with its witness fields intact) instead of
    /// collapsing it to `NoAdmittedPlan` the way this crate's old local
    /// `PlannerOutcome::into_result` did. New call sites that want a
    /// `Pddl8Error` from a `PlannerOutcome<T>` should prefer this variant
    /// (via `?`, see the `From` impl below) over hand-mapping to
    /// `NoAdmittedPlan`.
    PlanningFailed(bcinr_mfw_ir::PlannerFailure),
}

impl std::fmt::Display for Pddl8Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError(s) => write!(f, "PDDL parse error: {s}"),
            Self::BoundExceeded { what, limit, got } => {
                write!(f, "PDDL8 bound exceeded: {what} limit={limit} got={got}")
            }
            Self::UnknownPredicate(p) => write!(f, "unknown predicate: {p}"),
            Self::EmptyGrounding => write!(f, "grounding produced no applicable actions"),
            Self::NoAdmittedPlan => write!(f, "bounded plan search exhausted without goal"),
            Self::AdmissionLoadError(s) => write!(f, "Prolog8 admission load error: {s}"),
            Self::StepDenied { op_index, reason } => write!(f, "step {op_index} denied: {reason}"),
            Self::GoalNotReached => write!(f, "goal not reached after plan execution"),
            Self::ReceiptIntegrity(s) => write!(f, "receipt integrity failure: {s}"),
            Self::InvalidCaseId(s) => write!(f, "invalid case_id: {s}"),
            Self::PlanningFailed(w) => write!(f, "planning stage did not find a value: {w}"),
        }
    }
}

impl std::error::Error for Pddl8Error {}

/// Lets every existing `planner_outcome.into_result()?` call site (functions
/// returning `Result<T, Pddl8Error>`) keep compiling unchanged after
/// `PlannerOutcome<T>::into_result` started returning
/// `Result<T, bcinr_mfw_ir::PlannerFailure>` instead of `Result<T,
/// Pddl8Error>` directly — the `?` operator finds this impl automatically.
/// No information is discarded: the whole `PlannerFailure` witness is
/// carried inside `Pddl8Error::PlanningFailed`.
impl From<bcinr_mfw_ir::PlannerFailure> for Pddl8Error {
    fn from(f: bcinr_mfw_ir::PlannerFailure) -> Self {
        Pddl8Error::PlanningFailed(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bcinr_mfw_ir::{
        BoundHit, BoundKind, Digest, ExhaustionWitness, PlannerFailure, SearchProfileId,
    };

    #[test]
    fn planner_failure_converts_via_from_and_preserves_witness() {
        let witness = ExhaustionWitness {
            search_profile: SearchProfileId(7),
            explored_states: 12,
            frontier_empty: true,
            digest: Digest::hash(b"x"),
        };
        let err: Pddl8Error = PlannerFailure::Exhausted(witness.clone()).into();
        match err {
            Pddl8Error::PlanningFailed(PlannerFailure::Exhausted(w)) => assert_eq!(w, witness),
            other => panic!("expected PlanningFailed(Exhausted(_)), got {other:?}"),
        }
    }

    #[test]
    fn into_result_question_mark_compiles_and_preserves_bound_hit() {
        fn inner() -> Result<u32, Pddl8Error> {
            let outcome: PlannerOutcome<u32> = PlannerOutcome::Bounded(BoundHit {
                kind: BoundKind::PlanDepth,
                limit: 3,
                observed: 4,
            });
            let v = outcome.into_result()?;
            Ok(v)
        }
        let err = inner().unwrap_err();
        assert!(matches!(
            err,
            Pddl8Error::PlanningFailed(PlannerFailure::Bounded(BoundHit {
                limit: 3,
                observed: 4,
                ..
            }))
        ));
    }

    #[test]
    fn display_does_not_panic_for_planning_failed() {
        let err = Pddl8Error::PlanningFailed(PlannerFailure::Unsupported(
            bcinr_mfw_ir::UnsupportedFeature {
                feature_name: "existential-preconditions".into(),
                context: "test".into(),
            },
        ));
        let _ = format!("{err}");
    }
}
