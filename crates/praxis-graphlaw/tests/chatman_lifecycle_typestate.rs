//! Typestate/lifecycle tests for the chatman engine's S1-S6 admission loop.
//!
//! **Capability note (ModelChecker)**: `chicago_tdd_tools::testing::state_machine`
//! exposes `StateMachine<S: State>`, `Transition<From, To>`, `ScheduleGenerator`,
//! and `ModelChecker`. A probe this session found that
//! `ScheduleGenerator::generate` returns a hardcoded schedule set (it does not
//! enumerate reachable states from the real `ChatmanEngine`/`AdmittedTransition`
//! types), so `ModelChecker::check_invariant` run against it would not exercise
//! real engine coverage — it would only prove the invariant holds against the
//! framework's fixed synthetic schedule. That capability is therefore
//! **not used** as the primary coverage mechanism here (would be MOCKED
//! coverage dressed as real coverage). Instead:
//!
//! 1. The engine's real admission lifecycle (`in_memory` -> `load_snapshot`
//!    -> `admit_transition` -> `actuate`) is exercised directly against the
//!    public API, proving the actual typestate transitions.
//! 2. `AdmittedTransition`'s "only constructible via `admit_transition`"
//!    guarantee is proven by the type signature itself: every field
//!    (`envelope`, `receipt`, `boundary_requests`) is private to the `engine`
//!    module, and the struct has no `pub fn new`/public constructor anywhere
//!    in `crates/praxis-graphlaw/src/chatman/engine.rs` (grepped: the only
//!    place `AdmittedTransition { ... }` is constructed is inside
//!    `admit_transition`'s body). A `trybuild` compile-fail doctest would be
//!    the stronger proof of this, but `trybuild` is not wired as a
//!    dev-dependency in this crate's `Cargo.toml` (checked: absent) — adding
//!    it is out of scope for this task (test-file-only changes), so this is
//!    documented as a gap rather than faked with a `#[should_panic]` runtime
//!    test, which would prove nothing about compile-time construction.

use chicago_tdd_tools::assert_matches;
use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:lifecycle-test";
const PROFILE_IRI: &str = "profile:lifecycle-test";

const SNAPSHOT_TTL: &str = r#"
@prefix ex: <http://example.org/> .
@prefix ceng: <urn:chatman:engine#> .

ex:alice a ex:Employee .

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
ex:world ceng:ocelLog """{"run_id":1,"sealed":true,"objects":[{"id":"case-1","otype":"case"}],"events":[{"id":"e1","activity":"finish(a)","op_index":0,"at_ns":1,"objects":["case-1"]}]}""" .
"#;

fn build_profile() -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(
        profile_id,
        vec![
            "<urn:chatman:t0>".to_string(),
            "<urn:chatman:t1>".to_string(),
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
        invocation_id: InvocationId::new("inv-lifecycle"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-lifecycle"),
        input_handles: InputHandles::default(),
    }
}

// ---- Real lifecycle transitions, exercised directly ------------------------

test!(unloaded_engine_cannot_admit_a_transition, {
    // Arrange: constructed but no snapshot loaded — the "pre-load" state.
    let mut engine = ChatmanEngine::in_memory(build_profile()?)?;

    // Act: attempting admit_transition before load_snapshot.
    let result = engine.admit_transition(envelope());

    // Assert: SnapshotNotFound proves the typestate boundary — admission
    // cannot proceed from the pre-load state.
    assert_matches!(result, Err(Refusal::SnapshotNotFound(_)));
    Ok::<(), Refusal>(())
});

test!(loaded_engine_admits_and_yields_admitted_transition, {
    // Arrange: pre-load -> loaded.
    let mut engine = ChatmanEngine::in_memory(build_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;

    // Act: loaded -> admitted.
    let transition = engine.admit_transition(envelope())?;

    // Assert: the admitted transition carries a sealed, self-consistent
    // receipt (recompute_root over the nine carried digests matches the
    // carried root) — proving S6 sealed correctly.
    assert_eq!(
        transition.receipt().recompute_root(),
        transition.receipt().receipt_root
    );
    // The envelope accessor returns exactly what was submitted.
    assert_eq!(
        transition.envelope().invocation_id.as_str(),
        "inv-lifecycle"
    );
    Ok::<(), Refusal>(())
});

test!(admitted_transition_can_be_actuated_exactly_once_by_value, {
    // Arrange: admitted -> ready-to-actuate.
    let mut engine = ChatmanEngine::in_memory(build_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    let transition = engine.admit_transition(envelope())?;
    let root_before = transition.receipt().receipt_root.clone();

    // Act: admitted -> actuated. `actuate` consumes the transition by value
    // (typestate: `fn actuate(&mut self, transition: AdmittedTransition)`),
    // so the type system itself forbids actuating the same transition twice
    // — there is no second `transition` binding to pass. This is proven at
    // compile time by the function signature, not merely by a runtime check;
    // the runtime assertion below is the observable half of that guarantee.
    let record = engine.actuate(transition)?;

    // Assert: the post graph name is derived from the receipt root that
    // existed before actuation (identity carried through the transition).
    assert!(
        record.post_graph.contains(&root_before.0),
        "post graph {} must be scoped by the receipt root {}",
        record.post_graph,
        root_before.0
    );
    Ok::<(), Refusal>(())
});

test!(
    profile_hash_mismatch_refuses_admission_from_a_foreign_envelope,
    {
        // Arrange: an engine running under PROFILE_IRI, but an envelope naming a
        // different profile — a lifecycle transition that must never be admitted
        // regardless of snapshot/load state.
        let mut engine = ChatmanEngine::in_memory(build_profile()?)?;
        engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
        let foreign_envelope = InvocationEnvelope {
            invocation_id: InvocationId::new("inv-foreign"),
            snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
            profile_id: ProfileId::new("profile:not-this-engine"),
            operator_id: OperatorId::new("op-foreign"),
            input_handles: InputHandles::default(),
        };

        // Act
        let result = engine.admit_transition(foreign_envelope);

        // Assert
        assert_matches!(result, Err(Refusal::ProfileHashMismatch(_)));
        Ok::<(), Refusal>(())
    }
);

// ---- AdmittedTransition construction is compile-time-gated -----------------
//
// See the module doc for why this is a documented gap rather than a
// trybuild compile-fail test: trybuild is not wired in this crate's
// Cargo.toml, and this task is scoped to tests/ only. The proof that stands
// in its place: `AdmittedTransition`'s three fields (`envelope`, `receipt`,
// `boundary_requests`) carry no `pub` visibility modifier in
// `crates/praxis-graphlaw/src/chatman/engine.rs`, and grepping that file for
// `AdmittedTransition {` shows exactly one construction site, inside
// `ChatmanEngine::admit_transition`. Any attempt from this file to write
// `AdmittedTransition { envelope: ..., receipt: ..., boundary_requests: ... }`
// is therefore a compile error (E0451: field is private) — which is not
// something a runtime `#[test]` can observe without failing the build, so we
// state it here as a structural fact instead of asserting it at runtime.
