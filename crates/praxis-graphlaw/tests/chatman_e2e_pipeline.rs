//! End-to-end S1->S6 pipeline determinism test for the Chatman engine.
//!
//! This file drives ONE real fixture continuously through the full
//! `ChatmanEngine::admit_transition` pipeline (S1 snapshot canonicalization
//! through S6 receipt sealing) with five independently constructed engine
//! instances, and asserts the resulting `EngineProcessReceipt::receipt_root`
//! is byte-identical across all five runs. This is the single continuous
//! go/no-go signal for "does the pipeline complete end-to-end with a
//! stable, byte-identical receipt across repeated runs" — the existing
//! `chatman_snapshot_semantics.rs::s6_root_recomputes_over_pinned_digests_deterministically`
//! test only compares 2 runs; this test widens that to 5 runs in one place
//! and is scoped exclusively to the receipt-root determinism question (not
//! S1 canonical-N-Quads shape, not S4 orchestrated-plan shape, which are
//! covered elsewhere).
//!
//! The fixture (Turtle text, profile, envelope) is reused verbatim from
//! `chatman_snapshot_semantics.rs` rather than inventing new RDF.

use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:e2e-pipeline-test";
const PROFILE_IRI: &str = "profile:e2e-pipeline-test";

/// Reused fixture shape from `chatman_snapshot_semantics.rs::FIXED_TURTLE`:
/// deliberately unsorted subject/predicate order, one RDFS fact (S2 OWL RL
/// closure), one trivial PDDL world (S3 planning), one conforming OCEL trace
/// (S4 admission) — exercises the full S1->S6 pipeline on a single fixture.
const FIXED_TURTLE: &str = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ceng: <urn:chatman:engine#> .

ex:bob ex:knows ex:alice .
ex:Employee rdfs:subClassOf ex:Person .
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
        invocation_id: InvocationId::new("inv-e2e-pipeline"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-e2e-pipeline"),
        input_handles: InputHandles::default(),
    }
}

test!(
    s1_through_s6_pipeline_is_byte_identical_across_five_independent_runs,
    {
        // Arrange + Act: five independently constructed engines, each driven
        // through the full S1(snapshot)->S6(receipt) admission pipeline over
        // the same fixed Turtle fixture.
        let mut roots = Vec::with_capacity(5);
        for run in 0..5 {
            let profile = build_profile()?;
            let mut engine = ChatmanEngine::in_memory(profile)?;
            engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), FIXED_TURTLE)?;
            let transition = engine.admit_transition(envelope())?;
            let receipt = transition.receipt();

            // Each run's receipt must be internally self-consistent: the
            // nine constitutional digests recompute to the carried root
            // (S6 sealing law), not merely equal to each other by coincidence.
            assert_eq!(
                receipt.recompute_root(),
                receipt.receipt_root,
                "run {run}: recompute_root() must match the carried receipt_root"
            );

            roots.push(receipt.receipt_root.clone());
        }

        // Assert: all five runs produced the exact same receipt root.
        for (run, root) in roots.iter().enumerate().skip(1) {
            assert_eq!(
                root, &roots[0],
                "run {run}'s receipt_root diverged from run 0's — pipeline is not deterministic"
            );
        }

        Ok::<(), Refusal>(())
    }
);
