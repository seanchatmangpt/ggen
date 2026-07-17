//! Tests for [`super::RecursiveSocketClosure`] (PRD v26.7.11 §9, PROJ-759).

use std::collections::BTreeSet;

use powl2_decompose::{ParentChildClosure, Powl, SocketKind, SocketPath, WorkflowSocketId};

use super::*;
use crate::shacl::ValidationResult;
use crate::term::Term;

/// A `ValidationReport` with zero results — real SHACL Core conformance
/// (`conforms` iff `results.is_empty()`, matching
/// `crate::shacl::report::Validator::validate`'s own invariant).
fn conforming_evidence() -> ValidationReport {
    ValidationReport {
        conforms: true,
        results: Vec::new(),
    }
}

/// A `ValidationReport` carrying one real violation result — genuinely
/// non-conforming evidence, not a fabricated always-fail stub.
fn nonconforming_evidence() -> ValidationReport {
    let focus = Term::parse("<urn:test:focus-node>".to_string());
    let constraint =
        Term::parse("<http://www.w3.org/ns/shacl#MinCountConstraintComponent>".to_string());
    let shape = Term::parse("<urn:test:shape>".to_string());
    let severity = Term::parse("<http://www.w3.org/ns/shacl#Violation>".to_string());
    ValidationReport {
        conforms: false,
        results: vec![ValidationResult {
            focus_node: focus,
            result_path: None,
            value: None,
            source_constraint_component: constraint,
            source_shape: shape,
            severity,
            message: Some("test-injected violation".to_string()),
        }],
    }
}

/// A root `PartialOrder` over `n` leaves (`a`, `b`, `c`, ... by index),
/// no ordering constraints — concurrent for closure-law purposes, since
/// this module only cares about the parent-child structure, not the
/// execution order among children.
fn root_partial_order_over(n: usize) -> Powl {
    let children = (0..n)
        .map(|i| Powl::Leaf(Some(format!("leaf-{i}"))))
        .collect();
    Powl::PartialOrder {
        children,
        order: BTreeSet::new(),
    }
}

fn root_socket() -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root(),
        kind: SocketKind::PartialOrder,
    }
}

fn leaf_socket(i: usize) -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root().child(i),
        kind: SocketKind::Leaf,
    }
}

// ---------------------------------------------------------------------------
// PRD §19.4 — Remote Child Closure ("all_required" leaves an unsatisfied
// parent open).
// ---------------------------------------------------------------------------

#[test]
fn all_required_leaves_parent_open_until_every_child_is_admitted() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;

    // One admitted child, one merely observed: PRD §19.4 says this SHALL
    // leave the parent open.
    rsc.admit(&leaf_socket(0))?;
    rsc.observe(&leaf_socket(1))?;
    assert!(!rsc.is_closed()?);
    assert!(matches!(
        rsc.close(),
        Err(Refusal::ParentClosureUnsatisfied(_))
    ));

    // Admitting the second child satisfies all_required.
    rsc.admit(&leaf_socket(1))?;
    assert!(rsc.is_closed()?);
    rsc.close()?;
    Ok(())
}

#[test]
fn all_required_with_all_children_still_open_is_not_closed() -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    assert!(!rsc.is_closed()?);
    Ok(())
}

// ---------------------------------------------------------------------------
// PRD §19.5 — Quorum Closure.
// ---------------------------------------------------------------------------

#[test]
fn quorum_two_of_three_closes_even_though_the_third_stays_open() -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(2))?;

    rsc.admit(&leaf_socket(0))?;
    rsc.admit(&leaf_socket(1))?;
    // leaf 2 is left Open entirely.
    assert!(rsc.is_closed()?);
    rsc.close()?;
    Ok(())
}

#[test]
fn quorum_two_of_three_not_closed_with_only_one_admitted() -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(2))?;
    rsc.admit(&leaf_socket(0))?;
    rsc.observe(&leaf_socket(1))?; // observed, not admitted -- must not count
    assert!(!rsc.is_closed()?);
    Ok(())
}

// ---------------------------------------------------------------------------
// Declaration-time validation.
// ---------------------------------------------------------------------------

#[test]
fn declare_refuses_zero_children() {
    let model = Powl::Leaf(Some("solo".to_string()));
    let pcc = ParentChildClosure::from_model(&model);
    let leaf = WorkflowSocketId {
        path: SocketPath::root(),
        kind: SocketKind::Leaf,
    };
    let result = RecursiveSocketClosure::declare(&pcc, leaf, ClosureLaw::AllRequired);
    assert!(matches!(result, Err(Refusal::ClosureLawNoChildren(_))));
}

#[test]
fn declare_refuses_quorum_zero() {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let result = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(0));
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawQuorumOutOfRange(_))
    ));
}

#[test]
fn declare_refuses_quorum_exceeding_child_count() {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let result = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(3));
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawQuorumOutOfRange(_))
    ));
}

#[test]
fn declare_accepts_quorum_equal_to_child_count() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(2))?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Unknown-child refusals.
// ---------------------------------------------------------------------------

#[test]
fn observe_refuses_unknown_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let stranger = WorkflowSocketId {
        path: SocketPath::root().child(99),
        kind: SocketKind::Leaf,
    };
    assert!(matches!(
        rsc.observe(&stranger),
        Err(Refusal::ClosureLawUnknownChild(_))
    ));
    Ok(())
}

#[test]
fn admit_refuses_unknown_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let stranger = WorkflowSocketId {
        path: SocketPath::root().child(99),
        kind: SocketKind::Leaf,
    };
    assert!(matches!(
        rsc.admit(&stranger),
        Err(Refusal::ClosureLawUnknownChild(_))
    ));
    Ok(())
}

#[test]
fn require_terminal_admitted_refuses_unknown_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let stranger = WorkflowSocketId {
        path: SocketPath::root().child(99),
        kind: SocketKind::Leaf,
    };
    assert!(matches!(
        rsc.require_terminal_admitted(&stranger),
        Err(Refusal::ClosureLawUnknownChild(_))
    ));
    Ok(())
}

// ---------------------------------------------------------------------------
// PRD §9 line 525 — "observation until admitted".
// ---------------------------------------------------------------------------

#[test]
fn require_terminal_admitted_refuses_a_merely_observed_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.observe(&leaf_socket(0))?;
    assert!(matches!(
        rsc.require_terminal_admitted(&leaf_socket(0)),
        Err(Refusal::ChildCompletionUnadmitted(_))
    ));
    Ok(())
}

#[test]
fn require_terminal_admitted_refuses_a_still_open_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    assert!(matches!(
        rsc.require_terminal_admitted(&leaf_socket(0)),
        Err(Refusal::ChildCompletionUnadmitted(_))
    ));
    Ok(())
}

#[test]
fn require_terminal_admitted_accepts_an_admitted_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.admit(&leaf_socket(0))?;
    rsc.require_terminal_admitted(&leaf_socket(0))?;
    Ok(())
}

// ---------------------------------------------------------------------------
// PRD §19.7 — Duplicate Result (idempotent signals, never a downgrade).
// ---------------------------------------------------------------------------

#[test]
fn admit_is_idempotent_and_observe_never_downgrades_an_admitted_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;

    rsc.admit(&leaf_socket(0))?;
    // A second admit of the same child (duplicate result) is a no-op.
    rsc.admit(&leaf_socket(0))?;
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Admitted)
    );

    // A late "observe" signal after admission must not regress the state.
    rsc.observe(&leaf_socket(0))?;
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Admitted)
    );
    Ok(())
}

#[test]
fn observe_is_idempotent_for_an_already_observed_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.observe(&leaf_socket(0))?;
    rsc.observe(&leaf_socket(0))?;
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Observed)
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// PROJ-773 — `any_sufficient`: `Close(W) iff exists c in C(W),
// TerminalAdmitted(c)`.
// ---------------------------------------------------------------------------

#[test]
fn any_sufficient_closes_as_soon_as_one_child_is_admitted() -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AnySufficient)?;
    assert!(!rsc.is_closed()?);
    rsc.admit(&leaf_socket(1))?;
    assert!(rsc.is_closed()?);
    rsc.close()?;
    Ok(())
}

#[test]
fn any_sufficient_not_closed_when_only_observed_not_admitted() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AnySufficient)?;
    rsc.observe(&leaf_socket(0))?;
    assert!(!rsc.is_closed()?);
    assert!(matches!(
        rsc.close(),
        Err(Refusal::ParentClosureUnsatisfied(_))
    ));
    Ok(())
}

// ---------------------------------------------------------------------------
// PROJ-773 — `ordered_subset`: `Close(W) iff forall c in S,
// TerminalAdmitted(c)` for a declared non-empty subset `S` of `C(W)`.
// ---------------------------------------------------------------------------

#[test]
fn ordered_subset_closes_once_its_declared_children_are_admitted_even_if_others_stay_open(
) -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let subset = vec![leaf_socket(0), leaf_socket(2)];
    let mut rsc =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::OrderedSubset(subset))?;
    rsc.admit(&leaf_socket(0))?;
    assert!(!rsc.is_closed()?, "leaf 2 not admitted yet");
    // leaf 1 is entirely outside the declared subset and stays Open forever.
    rsc.admit(&leaf_socket(2))?;
    assert!(rsc.is_closed()?);
    rsc.close()?;
    Ok(())
}

#[test]
fn ordered_subset_not_closed_while_a_declared_member_is_merely_observed() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let subset = vec![leaf_socket(0), leaf_socket(1)];
    let mut rsc =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::OrderedSubset(subset))?;
    rsc.admit(&leaf_socket(0))?;
    rsc.observe(&leaf_socket(1))?;
    assert!(!rsc.is_closed()?);
    Ok(())
}

#[test]
fn declare_refuses_empty_ordered_subset() {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let result =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::OrderedSubset(vec![]));
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawOrderedSubsetInvalid(_))
    ));
}

#[test]
fn declare_refuses_ordered_subset_with_duplicate_entry() {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let subset = vec![leaf_socket(0), leaf_socket(0)];
    let result =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::OrderedSubset(subset));
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawOrderedSubsetInvalid(_))
    ));
}

#[test]
fn declare_refuses_ordered_subset_naming_a_non_child() {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let stranger = WorkflowSocketId {
        path: SocketPath::root().child(99),
        kind: SocketKind::Leaf,
    };
    let subset = vec![leaf_socket(0), stranger];
    let result =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::OrderedSubset(subset));
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawOrderedSubsetInvalid(_))
    ));
}

// ---------------------------------------------------------------------------
// PROJ-773 — `policy_decides`: `Close(W)` delegated to an out-of-band
// authority decision, never derived from child completion state.
// ---------------------------------------------------------------------------

#[test]
fn policy_decides_stays_open_until_a_decision_is_recorded() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    // Even a fully-admitted child does not close a policy_decides socket:
    // closure is delegated, never auto-derived.
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::PolicyDecides)?;
    rsc.admit(&leaf_socket(0))?;
    assert!(!rsc.is_closed()?);
    assert!(rsc.policy_decision().is_none());
    Ok(())
}

#[test]
fn policy_decides_closes_once_authority_records_an_affirmative_verdict() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::PolicyDecides)?;
    rsc.record_policy_decision(OperatorId::new("urn:operator:policy-desk"), true)?;
    assert!(rsc.is_closed()?);
    rsc.close()?;
    assert_eq!(
        rsc.policy_decision().map(|(a, v)| (a.as_str(), v)),
        Some(("urn:operator:policy-desk", true))
    );
    Ok(())
}

#[test]
fn policy_decides_stays_open_on_a_negative_verdict() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::PolicyDecides)?;
    rsc.record_policy_decision(OperatorId::new("urn:operator:policy-desk"), false)?;
    assert!(!rsc.is_closed()?);
    Ok(())
}

#[test]
fn record_policy_decision_refuses_when_law_is_not_policy_decides() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let result = rsc.record_policy_decision(OperatorId::new("urn:operator:policy-desk"), true);
    assert!(matches!(
        result,
        Err(Refusal::ClosureLawPolicyNotDeclared(_))
    ));
    Ok(())
}

#[test]
fn record_policy_decision_refuses_empty_authority() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::PolicyDecides)?;
    let result = rsc.record_policy_decision(OperatorId::new(""), true);
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
    Ok(())
}

#[test]
fn record_policy_decision_last_recorded_governs() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::PolicyDecides)?;
    rsc.record_policy_decision(OperatorId::new("urn:operator:first"), true)?;
    assert!(rsc.is_closed()?);
    rsc.record_policy_decision(OperatorId::new("urn:operator:second"), false)?;
    assert!(!rsc.is_closed()?);
    Ok(())
}

// ---------------------------------------------------------------------------
// PROJ-773 — `first_conformant`: `Close(W)` boolean necessarily coincides
// with `any_sufficient`'s; the real distinguishing behavior is
// `first_conformant_child` naming *which* child, by canonical order.
// ---------------------------------------------------------------------------

#[test]
fn first_conformant_closes_like_any_sufficient_once_one_child_conforms() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::FirstConformant)?;
    assert!(!rsc.is_closed()?);
    assert!(rsc.first_conformant_child().is_none());
    rsc.admit(&leaf_socket(1))?;
    assert!(rsc.is_closed()?);
    rsc.close()?;
    Ok(())
}

#[test]
fn first_conformant_not_closed_when_only_observed_not_admitted() -> Result<(), Refusal> {
    let model = root_partial_order_over(2);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::FirstConformant)?;
    rsc.observe(&leaf_socket(0))?;
    assert!(!rsc.is_closed()?);
    assert!(rsc.first_conformant_child().is_none());
    assert!(matches!(
        rsc.close(),
        Err(Refusal::ParentClosureUnsatisfied(_))
    ));
    Ok(())
}

#[test]
fn first_conformant_child_resolves_ties_by_canonical_order() -> Result<(), Refusal> {
    let model = root_partial_order_over(3);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc =
        RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::FirstConformant)?;
    // Admit leaf 2 first (in call order), then leaf 0 — canonical order
    // (WorkflowSocketId Ord, not admission call order) must still pick
    // leaf 0.
    rsc.admit(&leaf_socket(2))?;
    rsc.admit(&leaf_socket(0))?;
    assert_eq!(rsc.first_conformant_child(), Some(&leaf_socket(0)));
    Ok(())
}

// ---------------------------------------------------------------------------
// PROJ-774 — `promote_observed_to_admitted`: real SHACL conformance
// evidence gates the Observed -> Admitted promotion.
// ---------------------------------------------------------------------------

#[test]
fn promote_observed_to_admitted_promotes_on_conforming_evidence() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.observe(&leaf_socket(0))?;
    rsc.promote_observed_to_admitted(&leaf_socket(0), &conforming_evidence())?;
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Admitted)
    );
    rsc.require_terminal_admitted(&leaf_socket(0))?;
    Ok(())
}

#[test]
fn promote_observed_to_admitted_refuses_on_nonconforming_evidence_and_leaves_child_observed(
) -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.observe(&leaf_socket(0))?;
    let result = rsc.promote_observed_to_admitted(&leaf_socket(0), &nonconforming_evidence());
    assert!(matches!(result, Err(Refusal::ChildConformanceRefused(_))));
    // The child stays Observed — never silently promoted on failing evidence.
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Observed)
    );
    assert!(matches!(
        rsc.require_terminal_admitted(&leaf_socket(0)),
        Err(Refusal::ChildCompletionUnadmitted(_))
    ));
    Ok(())
}

#[test]
fn promote_observed_to_admitted_refuses_a_still_open_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let result = rsc.promote_observed_to_admitted(&leaf_socket(0), &conforming_evidence());
    assert!(matches!(result, Err(Refusal::ChildCompletionUnadmitted(_))));
    Ok(())
}

#[test]
fn promote_observed_to_admitted_refuses_unknown_child() -> Result<(), Refusal> {
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let stranger = WorkflowSocketId {
        path: SocketPath::root().child(99),
        kind: SocketKind::Leaf,
    };
    let result = rsc.promote_observed_to_admitted(&stranger, &conforming_evidence());
    assert!(matches!(result, Err(Refusal::ClosureLawUnknownChild(_))));
    Ok(())
}

#[test]
fn promote_observed_to_admitted_is_idempotent_for_an_already_admitted_child() -> Result<(), Refusal>
{
    let model = root_partial_order_over(1);
    let pcc = ParentChildClosure::from_model(&model);
    let mut rsc = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    rsc.admit(&leaf_socket(0))?;
    // Even with non-conforming evidence, an already-Admitted child is
    // never demoted -- promotion is a no-op, not a re-check.
    rsc.promote_observed_to_admitted(&leaf_socket(0), &nonconforming_evidence())?;
    assert_eq!(
        rsc.children().get(&leaf_socket(0)).copied(),
        Some(ChildCompletionState::Admitted)
    );
    Ok(())
}

#[test]
fn closure_law_name_matches_prd_snake_case_vocabulary() {
    assert_eq!(ClosureLaw::AllRequired.name(), "all_required");
    assert_eq!(ClosureLaw::Quorum(1).name(), "quorum");
    assert_eq!(ClosureLaw::AnySufficient.name(), "any_sufficient");
    assert_eq!(
        ClosureLaw::OrderedSubset(vec![leaf_socket(0)]).name(),
        "ordered_subset"
    );
    assert_eq!(ClosureLaw::PolicyDecides.name(), "policy_decides");
    assert_eq!(ClosureLaw::FirstConformant.name(), "first_conformant");
}

// ---------------------------------------------------------------------------
// Determinism: identical declarations produce identical canonical child
// ordering (BTreeMap keyed by Ord `WorkflowSocketId`, never a `HashMap`).
// ---------------------------------------------------------------------------

#[test]
fn declared_child_order_is_deterministic_across_runs() -> Result<(), Refusal> {
    let model = root_partial_order_over(4);
    let pcc = ParentChildClosure::from_model(&model);
    let a = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let b = RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AllRequired)?;
    let keys_a: Vec<&WorkflowSocketId> = a.children().keys().collect();
    let keys_b: Vec<&WorkflowSocketId> = b.children().keys().collect();
    assert_eq!(keys_a, keys_b);
    // And it is exactly the sorted order (paths child(0)..child(3)).
    let expected: Vec<WorkflowSocketId> = (0..4).map(leaf_socket).collect();
    let expected_refs: Vec<&WorkflowSocketId> = expected.iter().collect();
    assert_eq!(keys_a, expected_refs);
    Ok(())
}
