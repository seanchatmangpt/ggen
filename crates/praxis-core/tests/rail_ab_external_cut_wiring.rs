//! PROJ-796: real end-to-end proof that
//! `ChatmanEngine::admit_transition_with_external_cut`
//! (`praxis_graphlaw::chatman::engine`) reaches the real Rail A/B pipeline
//! (`ChatmanRailAbCompiler`, this crate) through the `ExternalCutCompiler`
//! seam -- not a hand-built fixture bypassing the wiring, and not merely a
//! unit test of `ArazzoProjectionReceipt::project_and_compile` in
//! isolation.
//!
//! Three scenarios:
//! 1. A POWL region with **no** declared `ExternalCut` produces a receipt
//!    byte-identical (all ten fields) to a plain `admit_transition` call
//!    (PRD.md sec.19.1, "local POWL remains local").
//! 2. A POWL region **with** a declared `ExternalCut` produces digests #1-9
//!    and the root byte-identical to the plain run, plus a real digest #10
//!    that independently recomputes to the same value the sealed engine
//!    produced -- proving the wiring actually reaches the real Tera
//!    render / AIR compile, not an opaque or asserted value.
//! 3. The same input run twice through the new path yields byte-identical
//!    receipts (this repo's determinism invariant).

use std::collections::BTreeSet;

use powl2_decompose::Powl;
use praxis_core::arazzo::{ArazzoProjectionReceipt, ChatmanRailAbCompiler};
use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;
use wasm4pm_compat::hash::blake3_combined;

const SNAPSHOT_IRI: &str = "urn:chatman:rail-ab-wiring:test";
const PROFILE_IRI: &str = "profile:rail-ab-wiring-test";

/// A tiny lawful world (PDDL domain/problem + one conforming OCEL event) so
/// S1-S6 admission succeeds; the external-cut wiring under test is layered
/// on top of a real admission, not a mocked one.
const SNAPSHOT_TTL: &str = r#"
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
ex:world ceng:ocelLog """{"run_id":1,"sealed":true,"objects":[{"id":"case-1","otype":"case"}],"events":[{"id":"e1","activity":"finish(a)","op_index":0,"at_ns":1,"objects":["case-1"]}]}""" .
"#;

fn test_profile() -> Result<EngineProfile, Refusal> {
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
        invocation_id: InvocationId::new("inv-rail-ab-1"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-1"),
        input_handles: InputHandles::default(),
    }
}

fn engine_with_snapshot() -> Result<ChatmanEngine, Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    Ok(engine)
}

/// A `PartialOrder` of a plain leaf followed by a declared `ExternalCut`
/// whose region is one leaf activity -- mirrors
/// `praxis_graphlaw::chatman::powl_projection::tests::model_with_external_cut`
/// / `praxis_core::arazzo::tests::model_with_external_cut`.
fn model_with_external_cut() -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some("intake".to_string())),
            Powl::ExternalCut {
                region: Box::new(Powl::Leaf(Some("remote_settle".to_string()))),
                projection: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                renderer: "arazzo_projection.tera".to_string(),
            },
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

/// The same shape, but with no `ExternalCut` anywhere -- the common case
/// PRD.md sec.19.1 requires to stay local.
fn model_without_cut() -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some("intake".to_string())),
            Powl::Leaf(Some("settle".to_string())),
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

/// Independently recomputes `chatman::engine`'s private digest #10 formula
/// (`external_cut_digest`) over the real materials
/// `ArazzoProjectionReceipt::project_and_compile` produced -- proving the
/// sealed engine's `external_cut` digest is a real, recomputable function
/// of real Rail A/B output, not an opaque or asserted value. The tag string
/// and field order are pinned to `chatman/engine/external-cut/v1` /
/// `crate::chatman::engine::EXTERNAL_CUT_DIGEST_TAG` (private to that
/// crate, mirrored here deliberately rather than exposed, the same
/// discipline `StageSeal`'s own tag constants follow).
fn expected_external_cut_digest(
    root_element_id: &str,
    receipt: &ArazzoProjectionReceipt,
) -> String {
    blake3_combined(&[
        "chatman/engine/external-cut/v1",
        root_element_id,
        &receipt.source_powl_digest_hex,
        &receipt.sparql_projection_digest_hex,
        &receipt.tera_template_digest_hex,
        &receipt.arazzo_digest_hex,
        &receipt.compiler_version,
        &receipt.air_digest_hex,
    ])
}

#[test]
fn no_external_cut_keeps_s1_s6_byte_identical_to_plain_admit_transition(
) -> Result<(), Box<dyn std::error::Error>> {
    let plain = engine_with_snapshot()?.admit_transition(envelope())?;

    let compiler = ChatmanRailAbCompiler::default();
    let via_helper = engine_with_snapshot()?.admit_transition_with_external_cut(
        envelope(),
        &model_without_cut(),
        &compiler,
    )?;

    assert_eq!(
        plain.receipt(),
        via_helper.receipt(),
        "a POWL region with no declared ExternalCut must produce a receipt \
         byte-identical to a plain admit_transition run"
    );
    assert!(via_helper.receipt().external_cut.is_none());
    Ok(())
}

#[test]
fn external_cut_admission_runs_the_real_rail_ab_pipeline_and_reflects_its_content(
) -> Result<(), Box<dyn std::error::Error>> {
    let model = model_with_external_cut();
    let compiler = ChatmanRailAbCompiler::default();

    let plain = engine_with_snapshot()?.admit_transition(envelope())?;
    let with_cut = engine_with_snapshot()?.admit_transition_with_external_cut(
        envelope(),
        &model,
        &compiler,
    )?;

    // Digests #1-#9 and the root are byte-identical to the plain run: the
    // Rail A/B pipeline only adds digest #10, it never perturbs the rest.
    assert_eq!(
        plain.receipt().graph_snapshot,
        with_cut.receipt().graph_snapshot
    );
    assert_eq!(plain.receipt().profile, with_cut.receipt().profile);
    assert_eq!(
        plain.receipt().symbol_table,
        with_cut.receipt().symbol_table
    );
    assert_eq!(plain.receipt().projection, with_cut.receipt().projection);
    assert_eq!(
        plain.receipt().admission_table,
        with_cut.receipt().admission_table
    );
    assert_eq!(
        plain.receipt().route_decision,
        with_cut.receipt().route_decision
    );
    assert_eq!(plain.receipt().tape, with_cut.receipt().tape);
    assert_eq!(plain.receipt().hook_event, with_cut.receipt().hook_event);
    assert_eq!(
        plain.receipt().engine_version,
        with_cut.receipt().engine_version
    );
    assert_eq!(
        plain.receipt().receipt_root,
        with_cut.receipt().receipt_root
    );
    assert_eq!(
        with_cut.receipt().recompute_root(),
        with_cut.receipt().receipt_root
    );

    // Digest #10 is real: present, and equal to an independent
    // recomputation over the real Rail A/B materials this test produces by
    // calling the same production entry point
    // (`ArazzoProjectionReceipt::project_and_compile`) with the identical
    // inputs `ChatmanEngine::admit_transition_with_external_cut` derives
    // internally.
    let external_cut = with_cut
        .receipt()
        .external_cut
        .clone()
        .expect("a declared ExternalCut must populate digest #10");

    let base_iri = SNAPSHOT_IRI;
    let root_element_id = format!("{base_iri}/n0");
    let invocation_id = envelope().invocation_id.to_string();
    let workflow_id = format!("chatman-external-cut/{invocation_id}");
    let title = format!("Chatman Rail A/B external cut for invocation {invocation_id}");

    let artifact = ArazzoProjectionReceipt::project_and_compile(
        &model,
        base_iri,
        Some(base_iri),
        &workflow_id,
        &title,
        compiler.compiler_version,
    )?;
    let expected = expected_external_cut_digest(&root_element_id, &artifact.receipt);
    assert_eq!(
        external_cut.0, expected,
        "the sealed engine's digest #10 must equal an independent recomputation \
         over the real Rail A/B materials"
    );

    // The manufactured Arazzo document really contains both step kinds
    // this fixture declares -- not a hand-built fixture standing in for
    // real Rail A output.
    assert!(artifact.arazzo_document.contains("intake"));
    assert!(artifact.arazzo_document.contains("x-powl-external-cut"));
    assert!(!artifact.air_wasm.is_empty());
    Ok(())
}

#[test]
fn external_cut_admission_is_deterministic_across_two_independent_runs(
) -> Result<(), Box<dyn std::error::Error>> {
    let model = model_with_external_cut();
    let compiler = ChatmanRailAbCompiler::default();

    let run_a = engine_with_snapshot()?.admit_transition_with_external_cut(
        envelope(),
        &model,
        &compiler,
    )?;
    let run_b = engine_with_snapshot()?.admit_transition_with_external_cut(
        envelope(),
        &model,
        &compiler,
    )?;

    assert_eq!(
        run_a.receipt(),
        run_b.receipt(),
        "identical input through the external-cut path twice must yield a \
         byte-identical receipt (all ten fields)"
    );
    assert!(run_a.receipt().external_cut.is_some());
    Ok(())
}
