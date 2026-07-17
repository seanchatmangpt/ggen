//! Refusal-taxonomy governance tests.
//!
//! Ground truth: `src/chatman/abi.rs:301-537` (the `Refusal` enum, 46
//! variants, including PROJ-SEC-04's `StageSealMismatch` and
//! `UnlawfulActuation`, and PROJ-786/787's closure-law/external-cut/N3
//! catalog-completeness variants added in a later session than the
//! original 31-variant count this file was authored against) and
//! `src/chatman/abi.rs:574-621` (`ALL_REFUSAL_NAMES: [&str; 46]`,
//! declaration order) and `src/chatman/abi.rs:630-681` (`Refusal::name()`,
//! exhaustive match).
//!
//! `chicago_tdd_tools::prelude` (verified this session in
//! `/Users/sac/chicago-tdd-tools/src/lib.rs:262-320`) exports no
//! `assert_subset!`/`assert_superset!` macro — `grep -rn "assert_subset\|assert_superset"
//! /Users/sac/chicago-tdd-tools/src` returns no matches anywhere in that
//! crate. Substituted with plain `assert!`/`assert_eq!` over `BTreeSet`, with
//! a comment at each substitution site marking it as such.

use std::collections::BTreeSet;

use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId,
    Refusal, ALL_REFUSAL_NAMES,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::{Dialect, ProfileGates};
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:refusal-governance:snapshot";
const PROFILE_IRI: &str = "profile:refusal-governance-test";

/// Governed list, hand-transcribed from `abi.rs:301-537` by reading the enum
/// declaration independently (not copied from `ALL_REFUSAL_NAMES` itself, so
/// this test actually cross-checks two independent readings of the source,
/// not the constant against itself). Extended to 46 entries when
/// `ALL_REFUSAL_NAMES` grew from 31 to 46 (PROJ-786/787 catalog-completeness
/// wiring): the 15 added entries below were already real, constructed,
/// end-to-end-tested `Refusal` variants (N3 cost/builtin/direct-actuation,
/// POWL external-cut, and closure-law/child-completion/parent-closure) —
/// only their presence in this governance list and `ALL_REFUSAL_NAMES` was
/// behind.
const GOVERNED_REFUSAL_NAMES: &[&str] = &[
    "ValidationFailed",
    "PlanInfeasible",
    "TraceUnlawful",
    "HookUnpermitted",
    "MissingReceipt",
    "SnapshotNotFound",
    "BoundaryRequestMissingReceipt",
    "Triple8UniverseOverflow",
    "TermNotInTriple8Universe",
    "ProfileSymbolTableMismatch",
    "ProjectionHashMismatch",
    "WarmPathRequired",
    "AdmissionTableMismatch",
    "HookPatternNotAdmitted",
    "OcelEventNotAdmitted",
    "LeastExpressiveRouteViolation",
    "UnsupportedDialect",
    "N3UnavailableByProfile",
    "N3ActuationRefused",
    "N3CostBoundExceeded",
    "N3BuiltinRefused",
    "N3DirectActuationRefused",
    "RouteDecisionMismatch",
    "GraphSnapshotMismatch",
    "ProfileHashMismatch",
    "AgentOverrideDenied",
    "WitnessNotAuthority",
    "BreedUnpermitted",
    "NondeterministicOperatorRequiresReceipt",
    "ProcessReceiptShadowType",
    "DuplicateCanonicalTapeType",
    "TripleTermInSnapshot",
    "StageSealMismatch",
    "UnlawfulActuation",
    "PowlRegionNotAdmitted",
    "ExternalCutUndeclared",
    "ExternalCutTypeMismatch",
    "ExternalCutAuthorityMismatch",
    "ClosureLawNoChildren",
    "ClosureLawQuorumOutOfRange",
    "ClosureLawUnknownChild",
    "ClosureLawOrderedSubsetInvalid",
    "ClosureLawPolicyNotDeclared",
    "ChildConformanceRefused",
    "ChildCompletionUnadmitted",
    "ParentClosureUnsatisfied",
];

/// `ALL_REFUSAL_NAMES` is a hard-coded `[&str; 46]` array
/// (`abi.rs:574`); this asserts the exact count read from the source.
#[test]
fn all_refusal_names_has_exactly_forty_six_entries() {
    assert_eq_msg!(
        ALL_REFUSAL_NAMES.len(),
        46,
        "abi.rs:574 declares ALL_REFUSAL_NAMES: [&str; 46]; count drifted"
    );
    assert_eq_msg!(
        GOVERNED_REFUSAL_NAMES.len(),
        46,
        "independently-transcribed governed list must also have 46 entries"
    );
}

/// Set-equality between the independently-transcribed governed list and the
/// crate's own `ALL_REFUSAL_NAMES` constant.
///
/// SUBSTITUTION NOTE: `chicago_tdd_tools` has no `assert_subset!`/
/// `assert_superset!` macro (verified this session; see module doc). This
/// uses plain `BTreeSet` equality via `assert_eq_msg!` instead, which is
/// strictly stronger than a subset check in both directions.
#[test]
fn governed_list_is_set_equal_to_all_refusal_names() {
    // Substitution for assert_subset!/assert_superset! (do not exist in
    // chicago_tdd_tools): plain BTreeSet built from each side, compared with
    // assert_eq_msg!.
    let governed: BTreeSet<&str> = GOVERNED_REFUSAL_NAMES.iter().copied().collect();
    let actual: BTreeSet<&str> = ALL_REFUSAL_NAMES.iter().copied().collect();
    assert_eq_msg!(
        governed,
        actual,
        "governed refusal-name list must be exactly ALL_REFUSAL_NAMES (abi.rs:574-621), \
         no more, no fewer"
    );
}

/// Every name in `ALL_REFUSAL_NAMES` is unique (no accidental duplicate
/// variant registration).
#[test]
fn all_refusal_names_are_unique() {
    let as_set: BTreeSet<&str> = ALL_REFUSAL_NAMES.iter().copied().collect();
    assert_eq_msg!(
        as_set.len(),
        ALL_REFUSAL_NAMES.len(),
        "ALL_REFUSAL_NAMES must contain no duplicates"
    );
}

// ─────────────────────────────────────────────────────────────────────────
// Engine corpus: provoke real Refusal variants and assert the exact name.
// ─────────────────────────────────────────────────────────────────────────

fn test_profile() -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(
        profile_id,
        vec![
            "<urn:chatman:refusal-governance:t0>".to_string(),
            "<urn:chatman:refusal-governance:t1>".to_string(),
        ],
    )?;
    Ok(EngineProfile {
        gates,
        symbol_table,
        admission: AdmissionSpec {
            constraint_names: vec!["c0".to_string()],
            required_mask: 0,
            forbidden_mask: 0,
            set_on_admit: 0,
            clear_on_admit: 0,
        },
        breed_permits: Vec::new(),
    })
}

fn envelope() -> InvocationEnvelope {
    InvocationEnvelope {
        invocation_id: InvocationId::new("inv-refusal-governance-1"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-refusal-governance-1"),
        input_handles: InputHandles::default(),
    }
}

const SNAPSHOT_TTL_MISSING_PDDL: &str = r#"
@prefix ex: <http://example.org/> .
ex:a ex:knows ex:b .
"#;

const SNAPSHOT_TTL_DUPLICATE_FIRE: &str = r#"
@prefix ex: <http://example.org/> .
@prefix ceng: <urn:chatman:engine#> .
ex:world ceng:pddlDomain """
(define (domain chatman-min)
  (:requirements :strips)
  (:predicates (ready ?x) (done ?x))
  (:action finish
    :parameters (?x)
    :precondition (and (ready ?x))
    :effect (and (done ?x) (not (ready ?x)))))
""" .
ex:world ceng:pddlProblem """
(define (problem chatman-min-p)
  (:domain chatman-min)
  (:objects a)
  (:init (ready a))
  (:goal (done a))
)
""" .
ex:world ceng:ocelLog """{"run_id":1,"sealed":true,"objects":[{"id":"case-1","otype":"case"}],"events":[{"id":"e1","activity":"finish(a)","op_index":0,"at_ns":1,"objects":["case-1"]},{"id":"e2","activity":"finish(a)","op_index":0,"at_ns":2,"objects":["case-1"]}]}""" .
"#;

/// Distinct refusal #1: `SnapshotNotFound` — S1 with no snapshot loaded.
#[test]
fn provokes_snapshot_not_found() -> Result<(), Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    let result = engine.admit_transition(envelope());
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "SnapshotNotFound",
                "unloaded snapshot must refuse exactly SnapshotNotFound, not merely is_err()"
            );
            Ok(())
        }
        Ok(_) => panic!("expected SnapshotNotFound, got Ok"),
    }
}

/// Distinct refusal #2: `TripleTermInSnapshot` — RDF 1.2 quoted-triple token
/// at the `load_snapshot` boundary (`engine.rs:524-529`).
#[test]
fn provokes_triple_term_in_snapshot() -> Result<(), Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    let result = engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        "<< <urn:s> <urn:p> <urn:o> >> <urn:q> <urn:r> .",
    );
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "TripleTermInSnapshot",
                "wrong refusal variant name"
            );
            Ok(())
        }
        Ok(()) => panic!("expected TripleTermInSnapshot, got Ok"),
    }
}

/// Distinct refusal #3: `UnsupportedDialect` — profile gates that never
/// enable OWL RL, over a snapshot that requires it (`engine.rs:875-882`,
/// `router.rs::decide`).
#[test]
fn provokes_unsupported_dialect() -> Result<(), Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let hot_only = ProfileGates::new(profile_id.clone(), Dialect::Triple8Pattern.mask_bit(), 0, 8)?;
    let base = test_profile()?;
    let profile = EngineProfile {
        gates: hot_only,
        symbol_table: base.symbol_table,
        admission: base.admission,
        breed_permits: base.breed_permits,
    };
    let mut engine = ChatmanEngine::in_memory(profile)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        SNAPSHOT_TTL_MISSING_PDDL,
    )?;
    let result = engine.admit_transition(envelope());
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "UnsupportedDialect",
                "wrong refusal variant name"
            );
            Ok(())
        }
        Ok(_) => panic!("expected UnsupportedDialect, got Ok"),
    }
}

/// Distinct refusal #4: `PlanInfeasible` — snapshot with graph data but no
/// PDDL domain/problem literals (S3, `engine.rs` `PlanInfeasible` mapping via
/// `Pddl8Error::EmptyGrounding`/`NoAdmittedPlan`).
#[test]
fn provokes_plan_infeasible() -> Result<(), Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        SNAPSHOT_TTL_MISSING_PDDL,
    )?;
    let result = engine.admit_transition(envelope());
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "PlanInfeasible",
                "wrong refusal variant name"
            );
            Ok(())
        }
        Ok(_) => panic!("expected PlanInfeasible, got Ok"),
    }
}

/// Distinct refusal #5: `TraceUnlawful` — OCEL trace fires the same plan op
/// twice (S4 conformance check).
#[test]
fn provokes_trace_unlawful() -> Result<(), Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        SNAPSHOT_TTL_DUPLICATE_FIRE,
    )?;
    let result = engine.admit_transition(envelope());
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "TraceUnlawful",
                "wrong refusal variant name"
            );
            Ok(())
        }
        Ok(_) => panic!("expected TraceUnlawful, got Ok"),
    }
}

/// Distinct refusal #6 (bonus, beyond the required 4-5): `ProfileHashMismatch`
/// — envelope names a different profile than the engine runs.
#[test]
fn provokes_profile_hash_mismatch() -> Result<(), Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        SNAPSHOT_TTL_MISSING_PDDL,
    )?;
    let mut env = envelope();
    env.profile_id = ProfileId::new("profile:someone-else");
    let result = engine.admit_transition(env);
    match result {
        Err(refusal) => {
            assert_eq_msg!(
                refusal.name(),
                "ProfileHashMismatch",
                "wrong refusal variant name"
            );
            Ok(())
        }
        Ok(_) => panic!("expected ProfileHashMismatch, got Ok"),
    }
}
