//! Parent-Child Closure Law (PRD v26.7.11 §9, PROJ-759).
//!
//! PRD §9: "Every recursive socket SHALL declare its closure law." A
//! *recursive socket* is a `powl2_decompose::WorkflowSocketId` — the same
//! addressing scheme `powl_projection.rs` already uses to name an external
//! cut's region — and its *children* are that socket's direct structural
//! children in a `powl2_decompose::ParentChildClosure` (e.g. the branches of
//! a `PartialOrder`/`Choice` composite, or an `ExternalCut`'s single
//! `region`). This module never invents its own child-identity scheme; it
//! builds directly on `powl2_decompose`'s existing parent-child machinery.
//!
//! Closure belongs to POWL/GraphLaw (PRD §9: "Closure SHALL belong to
//! POWL/GraphLaw. ... Arazzo SHALL transport the outer execution but SHALL
//! NOT define the authoritative closure law"), so this module lives in
//! `praxis-graphlaw`'s chatman lane, not in any Arazzo-transport crate.
//!
//! ## Scope: PROJ-759 / PROJ-772 / PROJ-773 / PROJ-774
//!
//! PRD §9 names six initial closure types; all six are real, evaluated
//! [`ClosureLaw`] variants. `all_required` and `quorum(q)` (PROJ-772) use
//! the PRD's own explicit formulas verbatim. The remaining four
//! (PROJ-773) — `any_sufficient`, `ordered_subset`, `policy_decides`,
//! `first_conformant` — have no formula given by the PRD (it names them,
//! nothing more); each uses the most direct, honestly-documented
//! formalization consistent with the two given formulas, spelled out on
//! its own [`ClosureLaw`] variant doc comment: `any_sufficient` is the
//! existential dual of `all_required`; `ordered_subset` is `all_required`
//! scoped to a declared non-empty subset of `C(W)` (order is preserved in
//! the declaration for downstream consumers, not re-checked against
//! historical admission timing — this module tracks current per-child
//! *state*, never a *history*); `policy_decides` genuinely delegates
//! `Close(W)` to an out-of-band authority decision recorded via
//! [`RecursiveSocketClosure::record_policy_decision`] (never auto-derived
//! from child state, and legitimately `Ok(false)` until a decision is
//! recorded); `first_conformant`'s `Close(W)` boolean necessarily
//! coincides with `any_sufficient`'s (once ≥1 child conforms, both hold —
//! that's mathematics, not laziness), so the genuinely distinguishing
//! behavior it adds is [`RecursiveSocketClosure::first_conformant_child`],
//! naming *which* child's consequence is authoritative by canonical
//! (`Ord`) order.
//!
//! PRD §9 line 525 ("A child completion signal SHALL be treated as
//! observation until admitted") is captured by the three-state
//! [`ChildCompletionState`] (`Open`/`Observed`/`Admitted`) and enforced by
//! [`RecursiveSocketClosure::require_terminal_admitted`]: an `Observed`
//! child never satisfies `TerminalAdmitted(c)`. PROJ-774 owns the full
//! *admission gate* that promotes `Observed` to `Admitted`:
//! [`RecursiveSocketClosure::promote_observed_to_admitted`] requires real
//! [`crate::shacl::ValidationReport`] conformance evidence (SHACL Core:
//! `conforms` iff zero results) and refuses with
//! [`Refusal::ChildConformanceRefused`] when that evidence does not
//! conform — it never promotes on say-so alone. Scoping that evidence to
//! the correct child (e.g. running SHACL validation over the subgraph the
//! child's returned consequence actually touches) is the caller's
//! responsibility; this module gates on the report it is handed, exactly
//! as [`crate::shacl::report::Validator::validate`] itself computes
//! `conforms`.

use std::collections::{BTreeMap, BTreeSet};

use powl2_decompose::{ParentChildClosure, WorkflowSocketId};

use super::abi::{OperatorId, Refusal};
use crate::shacl::ValidationReport;

/// Runtime completion state of one child under a recursive socket's
/// declared closure law (PRD §9 line 525).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChildCompletionState {
    /// No completion signal has been observed for this child yet.
    Open,
    /// A completion signal was observed but not yet admitted (PRD §9 line
    /// 525). Never satisfies `TerminalAdmitted(c)`.
    Observed,
    /// The child's terminal state has been admitted — `TerminalAdmitted(c)`
    /// holds.
    Admitted,
}

impl ChildCompletionState {
    /// `TerminalAdmitted(c)` per PRD §9's `all_required`/`quorum(q)`
    /// formulas: true only for [`ChildCompletionState::Admitted`].
    ///
    /// # Complexity
    /// O(1).
    #[must_use]
    pub fn is_terminal_admitted(self) -> bool {
        matches!(self, ChildCompletionState::Admitted)
    }
}

/// One of the PRD §9 "initial closure types" a recursive socket may
/// declare. See the module doc comment for this module's chosen
/// formalization of the four kinds the PRD names without a formula.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClosureLaw {
    /// `Close(W) iff forall c in C(W), TerminalAdmitted(c)` (PRD §9's own
    /// formula, verbatim).
    AllRequired,
    /// `Close(W) iff |{c in C(W): TerminalAdmitted(c)}| >= q` (PRD §9's own
    /// formula, verbatim).
    Quorum(u32),
    /// `Close(W) iff exists c in C(W), TerminalAdmitted(c)` — the
    /// existential dual of `all_required`'s universal formula; PRD §9
    /// names `any_sufficient` without a formula, and this is the direct
    /// reading of "any... sufficient".
    AnySufficient,
    /// `Close(W) iff forall c in S, TerminalAdmitted(c)`, for a
    /// caller-declared non-empty ordered sequence `S` that is a genuine
    /// subset of `C(W)` (validated at [`RecursiveSocketClosure::declare`]
    /// time: non-empty, no duplicates, every entry a real direct child).
    /// `S`'s declared order is preserved for downstream consumers (e.g.
    /// hook scheduling) but the closure predicate itself is a pure
    /// current-state membership check, consistent with this module's
    /// no-history design — it does not re-derive or enforce that children
    /// were admitted in that order.
    OrderedSubset(Vec<WorkflowSocketId>),
    /// `Close(W) iff` a policy decision has been recorded via
    /// [`RecursiveSocketClosure::record_policy_decision`] and its verdict
    /// is `true`. Unlike every other kind, this one is never derived from
    /// child completion state alone — PRD §9 names `policy_decides`
    /// precisely because closure is delegated to an out-of-band authority.
    /// Before any decision is recorded, `Close(W)` is legitimately
    /// `Ok(false)` (an open, not-yet-decided parent), never silently
    /// `true`.
    PolicyDecides,
    /// `Close(W) iff exists c in C(W), TerminalAdmitted(c)` — mathematically
    /// identical to `any_sufficient`'s boolean (once at least one child
    /// conforms, both hold; there is no way for these two predicates to
    /// disagree). The real, non-fabricated distinguishing behavior
    /// `first_conformant` provides is
    /// [`RecursiveSocketClosure::first_conformant_child`]: which child's
    /// consequence is authoritative, resolved deterministically by
    /// canonical (`Ord`) declared order, never by wall-clock arrival.
    FirstConformant,
}

impl ClosureLaw {
    /// The PRD's own lower-snake-case name for this closure kind.
    ///
    /// # Complexity
    /// O(1).
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            ClosureLaw::AllRequired => "all_required",
            ClosureLaw::Quorum(_) => "quorum",
            ClosureLaw::AnySufficient => "any_sufficient",
            ClosureLaw::OrderedSubset(_) => "ordered_subset",
            ClosureLaw::PolicyDecides => "policy_decides",
            ClosureLaw::FirstConformant => "first_conformant",
        }
    }
}

/// An out-of-band decision recorded for a [`ClosureLaw::PolicyDecides`]
/// closure (PRD §9): who decided, and what they decided. Later recorded
/// decisions supersede earlier ones (last-recorded governs) — this module
/// does not adjudicate between concurrent conflicting policy authorities;
/// that arbitration is out of this foundation's scope.
#[derive(Debug, Clone, PartialEq, Eq)]
struct PolicyDecision {
    authority: OperatorId,
    verdict: bool,
}

/// The declared closure law for one recursive socket, plus the current
/// per-child completion state (PRD §9). Children are sourced from a
/// `powl2_decompose::ParentChildClosure` at declaration time; there is no
/// way to add or remove a child afterward — a recursive socket's child set
/// is fixed by the admitted POWL structure it was declared over.
///
/// # Determinism
/// `children` is a `BTreeMap` keyed by `WorkflowSocketId` (itself `Ord` over
/// a plain index sequence); every method here iterates it in that fixed
/// sorted order, never a `HashMap`. No wall clock, no randomness.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecursiveSocketClosure {
    socket: WorkflowSocketId,
    law: ClosureLaw,
    children: BTreeMap<WorkflowSocketId, ChildCompletionState>,
    /// Set only for [`ClosureLaw::PolicyDecides`] via
    /// [`Self::record_policy_decision`]; `None` means "not yet decided",
    /// a legitimate open state, never treated as an implicit `true`.
    policy_decision: Option<PolicyDecision>,
}

impl RecursiveSocketClosure {
    /// Declares a closure law over `socket`'s direct structural children in
    /// `closure` (PRD §9: "every recursive socket SHALL declare its closure
    /// law"). Every child starts `Open`.
    ///
    /// # Errors
    /// - [`Refusal::ClosureLawNoChildren`] if `socket` has zero direct
    ///   children in `closure`.
    /// - [`Refusal::ClosureLawQuorumOutOfRange`] if `law` is `Quorum(q)`
    ///   and `q` is zero or exceeds the child count.
    /// - [`Refusal::ClosureLawOrderedSubsetInvalid`] if `law` is
    ///   `OrderedSubset(subset)` and `subset` is empty, contains a
    ///   duplicate, or names a `WorkflowSocketId` that is not one of
    ///   `socket`'s direct declared children.
    ///
    /// # Complexity
    /// O(log n) for the `children_of` lookup plus O(c log c) to build the
    /// per-child state map, where c is the direct child count and n is the
    /// total node count of the source POWL model; `OrderedSubset(subset)`
    /// additionally costs O(k log k) for its declare-time validation,
    /// k = `subset.len()`.
    pub fn declare(
        closure: &ParentChildClosure,
        socket: WorkflowSocketId,
        law: ClosureLaw,
    ) -> Result<Self, Refusal> {
        let child_set = closure.children_of(&socket);
        if child_set.is_empty() {
            return Err(Refusal::ClosureLawNoChildren(format!(
                "recursive socket {socket} declares a {} closure law but has zero direct \
                 children in the supplied parent-child closure",
                law.name()
            )));
        }
        match &law {
            ClosureLaw::Quorum(q) => {
                if *q == 0 || (*q as usize) > child_set.len() {
                    return Err(Refusal::ClosureLawQuorumOutOfRange(format!(
                        "recursive socket {socket} declares quorum({q}) over {} children; q \
                         must be in 1..={}",
                        child_set.len(),
                        child_set.len()
                    )));
                }
            }
            ClosureLaw::OrderedSubset(subset) => {
                if subset.is_empty() {
                    return Err(Refusal::ClosureLawOrderedSubsetInvalid(format!(
                        "recursive socket {socket} declares ordered_subset with zero required \
                         children; an empty ordered subset would close vacuously without any \
                         admission"
                    )));
                }
                // O(k log k): one BTreeSet insert/contains per declared entry.
                let mut seen: BTreeSet<&WorkflowSocketId> = BTreeSet::new();
                for c in subset {
                    if !child_set.contains(c) {
                        return Err(Refusal::ClosureLawOrderedSubsetInvalid(format!(
                            "recursive socket {socket} declares ordered_subset containing {c}, \
                             which is not one of its {} direct declared children",
                            child_set.len()
                        )));
                    }
                    if !seen.insert(c) {
                        return Err(Refusal::ClosureLawOrderedSubsetInvalid(format!(
                            "recursive socket {socket} declares ordered_subset with duplicate \
                             entry {c}"
                        )));
                    }
                }
            }
            ClosureLaw::AllRequired
            | ClosureLaw::AnySufficient
            | ClosureLaw::PolicyDecides
            | ClosureLaw::FirstConformant => {}
        }
        // O(c log c): one BTreeMap insert per direct child.
        let children = child_set
            .into_iter()
            .map(|c| (c, ChildCompletionState::Open))
            .collect();
        Ok(RecursiveSocketClosure {
            socket,
            law,
            children,
            policy_decision: None,
        })
    }

    /// The recursive socket this closure law is declared over.
    #[must_use]
    pub fn socket(&self) -> &WorkflowSocketId {
        &self.socket
    }

    /// The declared closure law.
    #[must_use]
    pub fn law(&self) -> &ClosureLaw {
        &self.law
    }

    /// Read-only view of every declared child's current completion state,
    /// in canonical (`WorkflowSocketId`-sorted) order.
    #[must_use]
    pub fn children(&self) -> &BTreeMap<WorkflowSocketId, ChildCompletionState> {
        &self.children
    }

    /// Records an observed (not-yet-admitted) completion signal for
    /// `child` (PRD §9 line 525). Idempotent and never a downgrade: an
    /// already-`Observed` or already-`Admitted` child is left unchanged
    /// (PRD §19.7: "the second return SHALL not duplicate consequence or
    /// advance the workflow twice").
    ///
    /// # Errors
    /// [`Refusal::ClosureLawUnknownChild`] if `child` was not part of the
    /// declared closure.
    ///
    /// # Complexity
    /// O(log c), c = declared child count.
    pub fn observe(&mut self, child: &WorkflowSocketId) -> Result<(), Refusal> {
        let state = self.child_state_mut(child)?;
        if *state == ChildCompletionState::Open {
            *state = ChildCompletionState::Observed;
        }
        Ok(())
    }

    /// Admits `child`'s terminal state — `TerminalAdmitted(c)` now holds
    /// (PRD §9). Idempotent: admitting an already-`Admitted` child is a
    /// no-op (PRD §19.7, duplicate result).
    ///
    /// # Errors
    /// [`Refusal::ClosureLawUnknownChild`] if `child` was not part of the
    /// declared closure.
    ///
    /// # Complexity
    /// O(log c).
    pub fn admit(&mut self, child: &WorkflowSocketId) -> Result<(), Refusal> {
        let state = self.child_state_mut(child)?;
        *state = ChildCompletionState::Admitted;
        Ok(())
    }

    /// Asserts `child` is `TerminalAdmitted` (PRD §9 line 525: "A child
    /// completion signal SHALL be treated as observation until admitted").
    ///
    /// # Errors
    /// - [`Refusal::ChildCompletionUnadmitted`] if `child` is `Observed` or
    ///   still `Open`.
    /// - [`Refusal::ClosureLawUnknownChild`] if `child` was not declared.
    ///
    /// # Complexity
    /// O(log c).
    pub fn require_terminal_admitted(&self, child: &WorkflowSocketId) -> Result<(), Refusal> {
        match self.children.get(child) {
            Some(ChildCompletionState::Admitted) => Ok(()),
            Some(state) => Err(Refusal::ChildCompletionUnadmitted(format!(
                "child {child} of recursive socket {} is {state:?}, not admitted; a completion \
                 signal is an observation until admitted (PRD §9 line 525)",
                self.socket
            ))),
            None => Err(self.unknown_child(child)),
        }
    }

    /// Promotes `child` from `Observed` to `Admitted` using real SHACL
    /// conformance evidence (PROJ-774; PRD §9 line 525's "observation
    /// until admitted" — this is the gate that actually performs that
    /// promotion, not merely models the state it promotes between).
    /// `evidence` is a real [`crate::shacl::ValidationReport`]
    /// (`crate::shacl::report::Validator::validate`'s own return type);
    /// promotion happens iff `evidence.conforms`. Idempotent for an
    /// already-`Admitted` child (PRD §19.7): re-promoting is a no-op and
    /// does not re-inspect `evidence`.
    ///
    /// Scoping `evidence` to `child`'s own returned consequence (e.g.
    /// running SHACL validation over the correct subgraph) is the
    /// caller's responsibility; this gate inspects the report it is
    /// handed and nothing more.
    ///
    /// # Errors
    /// - [`Refusal::ClosureLawUnknownChild`] if `child` was not declared.
    /// - [`Refusal::ChildCompletionUnadmitted`] if `child` is still `Open`
    ///   — there is no completion signal yet to promote; call
    ///   [`Self::observe`] first.
    /// - [`Refusal::ChildConformanceRefused`] if `child` is `Observed` and
    ///   `evidence.conforms` is `false` — the child stays `Observed`,
    ///   never silently promoted on failing evidence.
    ///
    /// # Complexity
    /// O(log c) for the child lookup; O(1) beyond that (the evidence is
    /// pre-computed by the caller — this function only inspects
    /// `evidence.conforms`/`evidence.results.len()`).
    pub fn promote_observed_to_admitted(
        &mut self,
        child: &WorkflowSocketId,
        evidence: &ValidationReport,
    ) -> Result<(), Refusal> {
        let socket = self.socket.clone();
        let state = self.child_state_mut(child)?;
        match *state {
            ChildCompletionState::Admitted => Ok(()),
            ChildCompletionState::Open => Err(Refusal::ChildCompletionUnadmitted(format!(
                "child {child} of recursive socket {socket} has no completion signal yet \
                 (still Open); call observe() before requesting promotion (PRD §9 line 525)"
            ))),
            ChildCompletionState::Observed => {
                if evidence.conforms {
                    *state = ChildCompletionState::Admitted;
                    Ok(())
                } else {
                    Err(Refusal::ChildConformanceRefused(format!(
                        "child {child} of recursive socket {socket} failed SHACL conformance \
                         ({} validation result(s) present); observation is not promoted to \
                         admission (PRD §9 line 525)",
                        evidence.results.len()
                    )))
                }
            }
        }
    }

    /// Records the [`ClosureLaw::PolicyDecides`] out-of-band decision for
    /// this recursive socket (PRD §9). Later calls overwrite the prior
    /// decision (last-recorded governs — see [`PolicyDecision`]).
    ///
    /// # Errors
    /// - [`Refusal::ClosureLawPolicyNotDeclared`] if this socket's
    ///   declared law is not `PolicyDecides` — a verdict has no closure
    ///   meaning under any other law.
    /// - [`Refusal::ValidationFailed`] if `authority` is empty — a policy
    ///   decision must be attributable, never anonymous.
    ///
    /// # Complexity
    /// O(1).
    pub fn record_policy_decision(
        &mut self,
        authority: OperatorId,
        verdict: bool,
    ) -> Result<(), Refusal> {
        if !matches!(self.law, ClosureLaw::PolicyDecides) {
            return Err(Refusal::ClosureLawPolicyNotDeclared(format!(
                "recursive socket {} declares a {} closure law, not policy_decides; a policy \
                 decision has no closure meaning here",
                self.socket,
                self.law.name()
            )));
        }
        if authority.as_str().is_empty() {
            return Err(Refusal::ValidationFailed(format!(
                "recursive socket {} policy decision requires a non-empty declared authority",
                self.socket
            )));
        }
        self.policy_decision = Some(PolicyDecision { authority, verdict });
        Ok(())
    }

    /// The currently recorded [`ClosureLaw::PolicyDecides`] decision, if
    /// any: `(authority, verdict)`.
    ///
    /// # Complexity
    /// O(1).
    #[must_use]
    pub fn policy_decision(&self) -> Option<(&OperatorId, bool)> {
        self.policy_decision
            .as_ref()
            .map(|d| (&d.authority, d.verdict))
    }

    /// The canonically-first (declared, `WorkflowSocketId`-`Ord`) child
    /// that is currently `TerminalAdmitted`, or `None` if none has been
    /// admitted yet. See the module doc comment and
    /// [`ClosureLaw::FirstConformant`] for why this accessor — not the
    /// `is_closed` boolean — is `first_conformant`'s real distinguishing
    /// behavior.
    ///
    /// # Complexity
    /// O(c) worst case — a linear scan of the `BTreeMap` in canonical
    /// order, short-circuiting at the first admitted child.
    #[must_use]
    pub fn first_conformant_child(&self) -> Option<&WorkflowSocketId> {
        self.children
            .iter()
            .find(|(_, s)| s.is_terminal_admitted())
            .map(|(k, _)| k)
    }

    /// Evaluates `Close(W)` under the declared law (PRD §9). An
    /// unsatisfied law returns `Ok(false)` — that is a legitimate open
    /// state (PRD §19.4: "shall leave the parent open"), not a refusal.
    /// See the module doc comment for the formalization this module uses
    /// for the four PRD-named kinds given without an explicit formula.
    ///
    /// # Complexity
    /// O(c) over the declared children for `all_required`/`quorum`/
    /// `any_sufficient`/`first_conformant`; O(k log c) for
    /// `ordered_subset` (k = declared subset length); O(1) for
    /// `policy_decides`.
    pub fn is_closed(&self) -> Result<bool, Refusal> {
        match &self.law {
            ClosureLaw::AllRequired => {
                // O(c): short-circuits on the first non-admitted child.
                Ok(self.children.values().all(|s| s.is_terminal_admitted()))
            }
            ClosureLaw::Quorum(q) => {
                // O(c): counts every admitted child (no short-circuit,
                // since the count itself is the answer).
                let admitted = self
                    .children
                    .values()
                    .filter(|s| s.is_terminal_admitted())
                    .count();
                Ok(admitted as u32 >= *q)
            }
            ClosureLaw::AnySufficient | ClosureLaw::FirstConformant => {
                // O(c): short-circuits on the first admitted child.
                Ok(self.children.values().any(|s| s.is_terminal_admitted()))
            }
            ClosureLaw::OrderedSubset(subset) => {
                // O(k log c): one BTreeMap lookup per declared entry.
                Ok(subset.iter().all(|c| {
                    self.children
                        .get(c)
                        .is_some_and(|s| s.is_terminal_admitted())
                }))
            }
            ClosureLaw::PolicyDecides => {
                Ok(self.policy_decision.as_ref().is_some_and(|d| d.verdict))
            }
        }
    }

    /// Attempts to close the parent workflow at this recursive socket.
    ///
    /// # Errors
    /// [`Refusal::ParentClosureUnsatisfied`] when [`Self::is_closed`]
    /// evaluates to `false`.
    ///
    /// # Complexity
    /// O(c), delegates to [`Self::is_closed`].
    pub fn close(&self) -> Result<(), Refusal> {
        if self.is_closed()? {
            Ok(())
        } else {
            Err(Refusal::ParentClosureUnsatisfied(format!(
                "recursive socket {} does not satisfy its declared {} closure law yet",
                self.socket,
                self.law.name()
            )))
        }
    }

    /// Mutable lookup shared by [`Self::observe`]/[`Self::admit`]/
    /// [`Self::promote_observed_to_admitted`].
    ///
    /// # Complexity
    /// O(log c).
    fn child_state_mut(
        &mut self,
        child: &WorkflowSocketId,
    ) -> Result<&mut ChildCompletionState, Refusal> {
        let socket = self.socket.clone();
        self.children.get_mut(child).ok_or_else(|| {
            Refusal::ClosureLawUnknownChild(format!(
                "recursive socket {socket} has no declared child {child}"
            ))
        })
    }

    /// Builds the [`Refusal::ClosureLawUnknownChild`] context for `child`.
    ///
    /// # Complexity
    /// O(1).
    fn unknown_child(&self, child: &WorkflowSocketId) -> Refusal {
        Refusal::ClosureLawUnknownChild(format!(
            "recursive socket {} has no declared child {child}",
            self.socket
        ))
    }
}

#[cfg(test)]
#[path = "closure_test.rs"]
mod tests;
