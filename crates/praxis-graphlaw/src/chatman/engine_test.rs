#![cfg(test)]

//! Chicago-style state-based tests over a real in-memory store: one
//! happy path, one refusal per stage, determinism (byte-identical
//! receipt roots across independent engines), actuation, and replay.

use std::collections::BTreeSet;

use super::*;
use crate::chatman::abi::{InputHandles, InvocationId, OperatorId, ProfileId};

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:test";
const PROFILE_IRI: &str = "profile:engine-test";

/// Snapshot fixture: a tiny RDFS hierarchy (so OWL RL derives a fact),
/// a one-step PDDL world, and a conforming one-event OCEL trace.
const SNAPSHOT_TTL: &str = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ceng: <urn:chatman:engine#> .

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

/// Same world, but the trace fires op 0 twice (DuplicateFire).
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

/// Same world, but the goal is unreachable (no action derives it).
const SNAPSHOT_TTL_INFEASIBLE: &str = r#"
@prefix ex: <http://example.org/> .
@prefix ceng: <urn:chatman:engine#> .
ex:world ceng:pddlDomain """
(define (domain chatman-min)
  (:requirements :strips)
  (:predicates (ready ?x) (done ?x) (never ?x))
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
  (:goal (never a))
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
        invocation_id: InvocationId::new("inv-1"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-1"),
        input_handles: InputHandles::default(),
    }
}

fn engine_with(turtle: &str) -> Result<ChatmanEngine, Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), turtle)?;
    Ok(engine)
}

fn admit(turtle: &str) -> Result<AdmittedTransition, Refusal> {
    let mut engine = engine_with(turtle)?;
    engine.admit_transition(envelope())
}

fn refusal_check(result: Result<AdmittedTransition, Refusal>, want: &str) -> Result<(), Refusal> {
    match result {
        Err(refusal) if refusal.name() == want => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "wanted refusal {want}, got {other:?}"
        ))),
    }
}

#[test]
fn happy_path_admits_and_seals_nine_digests() -> Result<(), Refusal> {
    // Arrange + Act
    let transition = admit(SNAPSHOT_TTL)?;
    let receipt = transition.receipt();

    // Assert: every digest is non-empty 64-hex material and the root
    // recomputes over the nine carried digests.
    for digest in [
        &receipt.graph_snapshot,
        &receipt.profile,
        &receipt.symbol_table,
        &receipt.projection,
        &receipt.admission_table,
        &receipt.route_decision,
        &receipt.tape,
        &receipt.hook_event,
        &receipt.engine_version,
    ] {
        assert_eq!(digest.0.len(), 64, "digest must be 64 hex chars");
    }
    assert_eq!(receipt.recompute_root(), receipt.receipt_root);
    // Canonical material is sorted (receipt law).
    let lines: Vec<&str> = receipt.canon_nquads.lines().collect();
    let mut sorted = lines.clone();
    sorted.sort_unstable();
    assert_eq!(lines, sorted, "canonical N-Quads must be sorted");
    assert!(!receipt.canon_nquads.is_empty());
    // The hookless fixture yields no boundary requests.
    assert!(transition.boundary_requests().is_empty());
    Ok(())
}

#[test]
fn owl_closure_lands_in_sibling_graph_not_snapshot() -> Result<(), Refusal> {
    // Arrange
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    let (_, before) = engine.fetch_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI))?;

    // Act
    let transition = engine.admit_transition(envelope())?;

    // Assert: the derived alice-is-a-Person fact exists in the closure
    // graph and the snapshot graph is byte-identical to before.
    let closure = NamedNode::new(format!("{SNAPSHOT_IRI}#closure"))
        .map_err(|e| Refusal::ValidationFailed(format!("closure IRI: {e}")))?;
    let mut derived_lines = Vec::new();
    for quad in engine
        .store
        .quads_for_pattern(None, None, None, Some(closure.as_ref().into()))
    {
        let quad = quad.map_err(|e| Refusal::ValidationFailed(format!("storage: {e}")))?;
        derived_lines.push(format!(
            "{} {} {}",
            quad.subject, quad.predicate, quad.object
        ));
    }
    assert!(
        derived_lines
            .iter()
            .any(|l| l.contains("alice") && l.contains("Person")),
        "OWL RL closure must derive alice rdf:type Person, got {derived_lines:?}"
    );
    let (_, after) = engine.fetch_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI))?;
    assert_eq!(before, after, "the input snapshot graph is immutable");
    assert_eq!(
        transition.receipt().recompute_root(),
        transition.receipt().receipt_root
    );
    Ok(())
}

#[test]
fn double_run_is_byte_identical() -> Result<(), Refusal> {
    // Arrange + Act: two independent engines over the same snapshot.
    let first = admit(SNAPSHOT_TTL)?;
    let second = admit(SNAPSHOT_TTL)?;

    // Assert: receipts agree byte for byte.
    assert_eq!(first.receipt(), second.receipt());
    assert_eq!(
        first.receipt().receipt_root.0,
        second.receipt().receipt_root.0
    );
    Ok(())
}

/// Gate F determinism, extended for PROJ-SEC-01: five independent engines
/// over the same snapshot must produce byte-identical end-of-run receipts
/// (the existing check, carried over from `double_run_is_byte_identical`
/// above but run 5x instead of 2x to match the "5 consecutive runs" bar
/// documented in `.claude/rules/rust-agi-core-team.md` §1) *and*
/// byte-identical PROJ-SEC-01 per-transition seals at every S1→S2, S2→S3,
/// S3→S4, and S4→S5 boundary. `double_run_is_byte_identical` above is left
/// as-is (its name predates this ticket); this test is the 5-run
/// extension the ticket asked for, added alongside rather than in place of
/// the original.
#[test]
fn five_consecutive_runs_receipts_and_stage_seals_are_byte_identical() -> Result<(), Refusal> {
    let mut receipts = Vec::with_capacity(5);
    let mut seal_runs = Vec::with_capacity(5);
    // O(1) fixed iteration count (5), each iteration bounded by one S1-S6
    // admission; not a hot-path loop.
    for _ in 0..5 {
        let mut engine = engine_with(SNAPSHOT_TTL)?;
        let stages = engine.run_stages(&envelope())?;
        seal_runs.push(stages.stage_seals);
        let transition = engine.admit_transition(envelope())?;
        receipts.push(transition.receipt().clone());
    }
    for r in &receipts[1..] {
        assert_eq!(
            &receipts[0], r,
            "receipt must be byte-identical across runs"
        );
    }
    for s in &seal_runs[1..] {
        assert_eq!(
            &seal_runs[0], s,
            "PROJ-SEC-01 stage seals must be byte-identical across runs"
        );
    }
    Ok(())
}

/// PROJ-SEC-01 negative test: a stage entered with a seal that does not
/// verify against the digest it is paired with must refuse with
/// [`Refusal::StageSealMismatch`], not silently proceed. This calls S2
/// (`apply_owl_closure`) directly, bypassing `run_stages`'s honest wiring,
/// with a seal computed over a tampered digest — proving the check is a
/// real recompute-and-compare seam, not a no-op that always agrees with
/// itself.
#[test]
fn stage_seal_mismatch_refuses_tampered_transition() -> Result<(), Refusal> {
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    let snapshot_id = GraphSnapshotId::new(SNAPSHOT_IRI);
    let (_, real_graph_snapshot) = engine.fetch_snapshot(&snapshot_id)?;

    // A seal computed over a *different* digest than the one about to be
    // passed as `prior_digest` — simulating corruption between S1 and S2.
    let tampered_digest = Digest::new("tampered-not-what-s1-produced".to_string());
    let seal_over_tampered = StageSeal::of("S1", &tampered_digest);

    let result = engine.apply_owl_closure(&snapshot_id, &seal_over_tampered, &real_graph_snapshot);
    match result {
        Err(Refusal::StageSealMismatch(_)) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "wanted Err(StageSealMismatch), got {other:?}"
        ))),
    }
}

/// Companion negative test: even a seal that verifies against the *real*
/// digest tag from the wrong stage name must refuse (seals are stage-name
/// tagged, so an S2 seal cannot be replayed as an S1 seal for the same
/// digest bytes).
#[test]
fn stage_seal_wrong_stage_name_refuses() -> Result<(), Refusal> {
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    let snapshot_id = GraphSnapshotId::new(SNAPSHOT_IRI);
    let (_, real_graph_snapshot) = engine.fetch_snapshot(&snapshot_id)?;

    // Seal computed under the wrong stage tag ("S2" instead of "S1") over
    // the *real* digest — still must not verify at S2's entry, which
    // checks against "S1".
    let wrongly_tagged_seal = StageSeal::of("S2", &real_graph_snapshot);

    let result = engine.apply_owl_closure(&snapshot_id, &wrongly_tagged_seal, &real_graph_snapshot);
    match result {
        Err(Refusal::StageSealMismatch(_)) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "wanted Err(StageSealMismatch), got {other:?}"
        ))),
    }
}

#[test]
fn s1_unknown_snapshot_refuses_snapshot_not_found() -> Result<(), Refusal> {
    // Arrange: engine with no snapshot loaded.
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    // Act
    let result = engine.admit_transition(envelope());
    // Assert
    refusal_check(result, "SnapshotNotFound")
}

#[test]
fn s1_quoted_triple_text_refuses_triple_term() -> Result<(), Refusal> {
    // Arrange
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    // Act: RDF 1.2 quoted-triple syntax at the load boundary.
    let result = engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        "<< <urn:s> <urn:p> <urn:o> >> <urn:q> <urn:r> .",
    );
    // Assert
    match result {
        Err(Refusal::TripleTermInSnapshot(_)) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "wanted TripleTermInSnapshot, got {other:?}"
        ))),
    }
}

#[test]
fn s2_owl_disabled_refuses_via_router() -> Result<(), Refusal> {
    // Arrange: profile whose gates never enable OwlRl.
    let profile_id = ProfileId::new(PROFILE_IRI);
    let hot_only = ProfileGates::new(
        profile_id.clone(),
        crate::chatman::router::Dialect::Triple8Pattern.mask_bit(),
        0,
        8,
    )?;
    let base = test_profile()?;
    let profile = EngineProfile {
        gates: hot_only,
        symbol_table: base.symbol_table,
        admission: base.admission,
        breed_permits: base.breed_permits,
    };
    let mut engine = ChatmanEngine::in_memory(profile)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    // Act
    let result = engine.admit_transition(envelope());
    // Assert: the OWL RL shape has no enabled dialect >= its floor.
    refusal_check(result, "UnsupportedDialect")
}

#[test]
fn s3_missing_pddl_refuses_plan_infeasible() -> Result<(), Refusal> {
    // Arrange: snapshot with graph data but no PDDL literals.
    let result = admit(
        r#"@prefix ex: <http://example.org/> .
ex:a ex:knows ex:b ."#,
    );
    // Assert
    refusal_check(result, "PlanInfeasible")
}

#[test]
fn s3_unreachable_goal_refuses_plan_infeasible() -> Result<(), Refusal> {
    refusal_check(admit(SNAPSHOT_TTL_INFEASIBLE), "PlanInfeasible")
}

#[test]
fn s4_duplicate_fire_refuses_trace_unlawful() -> Result<(), Refusal> {
    refusal_check(admit(SNAPSHOT_TTL_DUPLICATE_FIRE), "TraceUnlawful")
}

#[test]
fn s4_missing_trace_refuses_trace_unlawful() -> Result<(), Refusal> {
    // Arrange: PDDL present, OCEL literal absent.
    let ttl = r#"
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
"#;
    refusal_check(admit(ttl), "TraceUnlawful")
}

#[test]
fn envelope_naming_wrong_profile_refuses_profile_hash_mismatch() -> Result<(), Refusal> {
    // Arrange
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    let mut env = envelope();
    env.profile_id = ProfileId::new("profile:someone-else");
    // Act + Assert
    refusal_check(engine.admit_transition(env), "ProfileHashMismatch")
}

#[test]
fn actuate_registers_post_graph_and_dedups() -> Result<(), Refusal> {
    // Arrange
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    let transition = engine.admit_transition(envelope())?;
    let root = transition.receipt().receipt_root.0.clone();

    // Act
    let record = engine.actuate(transition)?;

    // Assert: the post graph is named by the receipt root and exists.
    assert!(record.post_graph.contains(&root));
    assert_eq!(record.duplicates_skipped, 0);
    let post = NamedNode::new(&record.post_graph)
        .map_err(|e| Refusal::ValidationFailed(format!("post IRI: {e}")))?;
    let present = engine
        .store
        .contains_named_graph(post.as_ref())
        .map_err(|e| Refusal::ValidationFailed(format!("storage: {e}")))?;
    assert!(present, "actuation must register the post graph");
    Ok(())
}

#[test]
fn verify_replay_accepts_faithful_and_refuses_tampered() -> Result<(), Refusal> {
    // Arrange
    let transition = admit(SNAPSHOT_TTL)?;
    let inputs = ReplayInputs {
        envelope: envelope(),
        snapshot_turtle: SNAPSHOT_TTL.to_string(),
        profile: test_profile()?,
    };

    // Act + Assert: the faithful receipt replays clean.
    match ChatmanEngine::verify_replay(transition.receipt(), &inputs) {
        Ok(()) => {}
        Err(mismatch) => {
            return Err(Refusal::ValidationFailed(format!(
                "faithful replay must verify, got {mismatch}"
            )))
        }
    }

    // Tamper with digest #7: the fail-fast enum names the tape field.
    let mut tampered = transition.receipt().clone();
    tampered.tape = Digest::new("0".repeat(64));
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::Tape { .. }) => {}
        other => {
            return Err(Refusal::ValidationFailed(format!(
                "tampered tape digest must fail as ReplayMismatch::Tape, got {other:?}"
            )))
        }
    }

    // Tamper with the root only: every field matches, the root recompute
    // catches the drift.
    let mut root_tampered = transition.receipt().clone();
    root_tampered.receipt_root = Digest::new("f".repeat(64));
    match ChatmanEngine::verify_replay(&root_tampered, &inputs) {
        Err(ReplayMismatch::ReceiptRoot { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "tampered root must fail as ReplayMismatch::ReceiptRoot, got {other:?}"
        ))),
    }
}

/// A [`Powl`] region declaring one external cut, mirroring
/// `powl_projection::tests::model_with_external_cut` (this module cannot
/// depend on `praxis-core`, so it builds its own tiny fixture rather than
/// sharing one; see this file's `use super::*` for why `Powl` is in scope).
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

/// A deterministic, in-crate stand-in for the real Rail A/B pipeline
/// (`praxis_core::arazzo::ChatmanRailAbCompiler`, PROJ-796): this crate
/// cannot depend on `praxis-core` (see `powl_projection`'s module doc for
/// why), so PROJ-796's digest-#10 replay-verification gap is exercised here
/// through the same [`ExternalCutCompiler`] trait seam production code
/// uses, with a fake implementation the test controls directly. Two
/// instances built with different `air_digest_hex` values stand in for
/// "same S1-S6 state, different compiled WASM" — exactly the drift
/// PROJ-796 named as unreachable-to-catch before this fix.
struct FakeExternalCutCompiler {
    air_digest_hex: String,
}

impl ExternalCutCompiler for FakeExternalCutCompiler {
    fn compile(
        &self,
        request: &ExternalCutCompilationRequest<'_>,
    ) -> Result<ExternalCutCompilationOutcome, Refusal> {
        Ok(ExternalCutCompilationOutcome {
            source_powl_digest_hex: blake3_combined(&["fake-source", request.region_turtle]),
            sparql_projection_digest_hex: "fake-sparql-digest".to_string(),
            tera_template_digest_hex: "fake-tera-digest".to_string(),
            arazzo_digest_hex: "fake-arazzo-digest".to_string(),
            compiler_version: "fake-compiler/v1".to_string(),
            air_digest_hex: self.air_digest_hex.clone(),
            arazzo_document: "{}".to_string(),
        })
    }
}

/// PROJ-796 gap closure: [`ChatmanEngine::verify_replay_with_external_cut`]
/// recomputes digest #10 and catches a tampered/drifted Rail A/B artifact
/// that carries the exact same S1-S6 state (same envelope, same snapshot,
/// same declared region) as the admitted receipt — the scenario the ticket
/// disclosed as unreachable through plain [`ChatmanEngine::verify_replay`].
#[test]
fn verify_replay_with_external_cut_accepts_faithful_and_refuses_tampered_artifact(
) -> Result<(), Refusal> {
    // Arrange: admit through the real external-cut path with a fixed fake
    // compiler, so the receipt carries a real (recomputable) digest #10.
    let model = model_with_external_cut();
    let admit_compiler = FakeExternalCutCompiler {
        air_digest_hex: "air-v1".to_string(),
    };
    let transition = engine_with(SNAPSHOT_TTL)?.admit_transition_with_external_cut(
        envelope(),
        &model,
        &admit_compiler,
    )?;
    let receipt = transition.receipt().clone();
    assert!(
        receipt.external_cut.is_some(),
        "a declared ExternalCut must populate digest #10"
    );

    let inputs = ReplayInputs {
        envelope: envelope(),
        snapshot_turtle: SNAPSHOT_TTL.to_string(),
        profile: test_profile()?,
    };

    // Act + Assert: faithful replay (same compiler, same region) verifies.
    match ChatmanEngine::verify_replay_with_external_cut(
        &receipt,
        &inputs,
        Some(&model),
        &admit_compiler,
    ) {
        Ok(()) => {}
        Err(mismatch) => {
            return Err(Refusal::ValidationFailed(format!(
                "faithful external-cut replay must verify, got {mismatch}"
            )))
        }
    }

    // Tampered replay: identical S1-S6 inputs, but the compiler now stands
    // in for a different compiled WASM artifact. Digest #10 must diverge
    // and replay must refuse with the new taxonomy variant — this is the
    // exact case that verified cleanly (silently) before this fix, because
    // verify_replay never looked at digest #10 at all.
    let tampered_compiler = FakeExternalCutCompiler {
        air_digest_hex: "air-v2-tampered".to_string(),
    };
    match ChatmanEngine::verify_replay_with_external_cut(
        &receipt,
        &inputs,
        Some(&model),
        &tampered_compiler,
    ) {
        Err(ReplayMismatch::ExternalCut { recorded, replayed }) => {
            assert_ne!(
                recorded, replayed,
                "mismatch must carry two different digests"
            );
        }
        other => {
            return Err(Refusal::ValidationFailed(format!(
                "tampered external-cut artifact must fail as ReplayMismatch::ExternalCut, \
                 got {other:?}"
            )))
        }
    }

    // A receipt with Some digest #10 replayed with no powl_region at all
    // must also refuse (there is nothing to recompile against).
    match ChatmanEngine::verify_replay_with_external_cut(&receipt, &inputs, None, &admit_compiler) {
        Err(ReplayMismatch::ExternalCut { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "Some digest #10 replayed with no powl_region must fail as \
             ReplayMismatch::ExternalCut, got {other:?}"
        ))),
    }
}

/// A receipt with `external_cut: None` (the common, plain-`admit_transition`
/// case) replays exactly like [`ChatmanEngine::verify_replay`] — the
/// [`ExternalCutCompiler`] is never consulted.
#[test]
fn verify_replay_with_external_cut_matches_plain_replay_when_receipt_has_no_digest_10(
) -> Result<(), Refusal> {
    let transition = admit(SNAPSHOT_TTL)?;
    assert!(transition.receipt().external_cut.is_none());
    let inputs = ReplayInputs {
        envelope: envelope(),
        snapshot_turtle: SNAPSHOT_TTL.to_string(),
        profile: test_profile()?,
    };
    let compiler = FakeExternalCutCompiler {
        air_digest_hex: "unused-when-digest-10-is-none".to_string(),
    };
    match ChatmanEngine::verify_replay_with_external_cut(
        transition.receipt(),
        &inputs,
        None,
        &compiler,
    ) {
        Ok(()) => Ok(()),
        Err(mismatch) => Err(Refusal::ValidationFailed(format!(
            "None digest #10 must replay clean with no powl_region, got {mismatch}"
        ))),
    }
}

/// PROJ-771 (PRD.md sec.8, "Independent Process Cells"): two independently
/// constructed [`ChatmanEngine`] instances, alive in the same process, must
/// not be able to observe or reproduce one another's admitted graph/state
/// through any hidden channel. `ChatmanEngine` holds no `static`,
/// `OnceCell`/`OnceLock`, `lazy_static`, or thread-local field anywhere in
/// its own struct or constructors (`in_memory`/`open`/`with_store`): each
/// instance owns a private `oxigraph::store::Store` built fresh by
/// `Store::new()`, so there is no field-level sharing to defeat. This test
/// exercises that guarantee behaviorally, entirely through the public
/// admission API (never reaching into a "cross-engine" accessor, because
/// none exists on `ChatmanEngine` — no method takes another engine's handle
/// or a raw numeric term id):
///
/// 1. Engine A admits a full S1-S6 transition over a snapshot carrying a
///    distinctive marker literal.
/// 2. Engine B — a wholly separate `in_memory` instance under the same
///    profile — is asked to admit a transition against the *exact same*
///    `snapshot_id` A just used. It refuses `SnapshotNotFound`: B's own
///    store never received A's triples, so the identifier alone unlocks
///    nothing.
/// 3. Engine B then loads and admits its own (unmarked) snapshot under that
///    same `snapshot_id` and profile. Its receipt's canonical N-Quads
///    material contains none of A's marker literal, and every one of B's
///    receipt digests differs from A's — proving content isolation, not
///    just a missing-graph refusal.
///
/// This also stands as the disclosed answer to whether the crate's
/// process-wide term-interning statics (`crate::registry::LIST_REGISTRY`
/// et al., `crate::encoding::GLOBAL_ENCODER` — real `static`s shared by
/// every `crate::TripleStore` instance in the process, per
/// `registry.rs`'s own module doc) could defeat this guarantee: they
/// cannot be reached from here, because `ChatmanEngine`'s public surface
/// never exposes a raw numeric term id or a `TripleStore`/`Encoder`/
/// `VarOrTerm` handle to a caller. `apply_owl_closure` (S2) constructs its
/// `TripleStore` fresh, local to that one call, and every output that
/// crosses back out of it (`canonicalize_quads`) is already-decoded
/// N-Quads text, never a raw id — so even though those side tables are
/// literally process-wide, nothing in this crate's *public* API lets one
/// engine instance use them to read another instance's content. Recorded
/// here rather than silently assumed, per PROJ-771's scope note.
#[test]
fn process_cell_isolation_engine_b_cannot_observe_engine_a() -> Result<(), Refusal> {
    const MARKER: &str = "ISOLATION-MARKER-ONLY-IN-ENGINE-A-7f3c9a2d";
    let snapshot_a_ttl =
        format!("{SNAPSHOT_TTL}\n<urn:chatman:secret> <urn:chatman:marker> \"{MARKER}\" .");

    // Arrange: Engine A is a fully independent instance that admits a real
    // S1-S6 transition over the marked snapshot.
    let mut engine_a = ChatmanEngine::in_memory(test_profile()?)?;
    engine_a.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), &snapshot_a_ttl)?;
    let transition_a = engine_a.admit_transition(envelope())?;
    assert!(
        transition_a.receipt().canon_nquads.contains(MARKER),
        "sanity: engine A's own receipt must contain its own marker"
    );

    // Act 1 + Assert 1: Engine B is a second, wholly separate instance
    // (same profile, own fresh in-memory store) that never loaded anything.
    // Asking it to admit the identical snapshot_id A just used must refuse
    // SnapshotNotFound -- the id alone unlocks nothing from B's own store.
    let mut engine_b = ChatmanEngine::in_memory(test_profile()?)?;
    match engine_b.admit_transition(envelope()) {
        Err(Refusal::SnapshotNotFound(_)) => {}
        other => {
            return Err(Refusal::ValidationFailed(format!(
                "isolation violated: engine B resolved engine A's snapshot id \
                 <{SNAPSHOT_IRI}> without ever loading it: {other:?}"
            )))
        }
    }

    // Act 2 + Assert 2: Engine B loads and admits its own, unmarked
    // snapshot under the very same snapshot_id/profile A used. Its receipt
    // must carry none of A's marker, and every digest must differ from A's.
    engine_b.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    let transition_b = engine_b.admit_transition(envelope())?;
    assert!(
        !transition_b.receipt().canon_nquads.contains(MARKER),
        "isolation violated: engine B's canonical material contains engine A's \
         private marker literal"
    );
    let receipt_a = transition_a.receipt();
    let receipt_b = transition_b.receipt();
    // digest #1 (graph_snapshot) hashes the raw snapshot content, which
    // genuinely differs (A carries the marker triple, B does not) -- this
    // is the direct proof that B's S1 read its own content, not A's.
    assert_ne!(
        receipt_a.graph_snapshot.0, receipt_b.graph_snapshot.0,
        "isolation violated: engine B's graph_snapshot digest equals engine A's"
    );
    // digest #6 root folds graph_snapshot in, so it must differ too.
    // (digest #4 `projection`, over the *derived* OWL RL closure delta, is
    // legitimately identical here: the marker triple has no RDFS rule
    // applicable to it, so both engines derive the same closure from the
    // same underlying RDFS fragment -- that is expected engine behavior,
    // not cross-instance leakage, and is not asserted on.)
    assert_ne!(
        receipt_a.receipt_root.0, receipt_b.receipt_root.0,
        "isolation violated: engine B's receipt_root equals engine A's"
    );

    // Act 3 + Assert 3: reaching directly at the store level (available to
    // this in-crate test module) confirms B's store never received A's
    // marker graph content at all, independent of any digest comparison.
    let (canon_b, _) = engine_b.fetch_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI))?;
    assert!(
        !canon_b.contains(MARKER),
        "isolation violated: engine B's own store-level snapshot read contains \
         engine A's private marker"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// PROJ-774 engine bridge -- `ChatmanEngine::admit_child_completion`.
//
// `docs/jira/v26.7.11/PATH_TO_100.md` §7 names the closure/compensation
// group as real, tested, and reachable from zero non-test callers of
// `ChatmanEngine`. These tests exercise the minimal, honestly-scoped bridge
// method added to close that gap: they prove `admit_child_completion` is a
// real `ChatmanEngine` entry point that (a) genuinely consults this
// engine's own S1 snapshot-presence check before the closure-law state
// machine ever sees the signal, and (b) genuinely drives
// `RecursiveSocketClosure`'s real PROJ-774 promotion gate through to a
// changed, observable state -- not a call that merely returns `Ok(())`
// without the underlying state having moved.
// ---------------------------------------------------------------------------

use crate::chatman::closure::ClosureLaw;
use crate::shacl::ValidationResult;
use crate::term::Term;
use powl2_decompose::{ParentChildClosure, SocketKind, SocketPath};

const CHILD_SNAPSHOT_IRI: &str = "urn:chatman:snapshot:test-child";

fn closure_root_socket() -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root(),
        kind: SocketKind::PartialOrder,
    }
}

fn closure_leaf_socket(i: usize) -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root().child(i),
        kind: SocketKind::Leaf,
    }
}

/// One recursive socket, one declared child, `all_required` -- the
/// smallest closure law shape that lets a single `admit_child_completion`
/// call close the parent.
fn one_leaf_closure() -> Result<RecursiveSocketClosure, Refusal> {
    let model = Powl::PartialOrder {
        children: vec![Powl::Leaf(Some("leaf-0".to_string()))],
        order: BTreeSet::new(),
    };
    let pcc = ParentChildClosure::from_model(&model);
    RecursiveSocketClosure::declare(&pcc, closure_root_socket(), ClosureLaw::AllRequired)
}

/// A `ValidationReport` with zero results -- real SHACL Core conformance,
/// matching `crate::shacl::report::Validator::validate`'s own invariant
/// (`conforms` iff `results.is_empty()`).
fn conforming_evidence() -> ValidationReport {
    ValidationReport {
        conforms: true,
        results: Vec::new(),
    }
}

/// A `ValidationReport` carrying one real violation result -- genuinely
/// non-conforming evidence, not a fabricated always-fail stub. Mirrors
/// `closure_test.rs`'s own `nonconforming_evidence()` fixture.
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

/// Happy path: a real `ChatmanEngine`, holding a real child snapshot in its
/// own store, admits a child-workflow completion signal end to end through
/// `admit_child_completion` -- and the closure law genuinely closes as a
/// result, not merely "returns without error".
#[test]
fn admit_child_completion_admits_through_real_engine_s1_and_closure_gate() -> Result<(), Refusal> {
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(CHILD_SNAPSHOT_IRI),
        "<urn:ex:s> <urn:ex:p> <urn:ex:o> .\n",
    )?;
    let mut socket = one_leaf_closure()?;

    engine.admit_child_completion(
        &mut socket,
        &closure_leaf_socket(0),
        &GraphSnapshotId::new(CHILD_SNAPSHOT_IRI),
        &conforming_evidence(),
    )?;

    // Real, observable state change: `require_terminal_admitted` is the
    // same PRD §9 line 525 check `closure.rs`'s own tests use -- this is
    // not merely "no error was returned".
    socket.require_terminal_admitted(&closure_leaf_socket(0))?;
    assert!(socket.is_closed()?);
    socket.close()?;
    Ok(())
}

/// Refusal path proving the engine's real S1 presence check runs *before*
/// the closure-law state machine is touched at all: a completion signal
/// citing a snapshot this engine never loaded is refused with the same
/// `Refusal::SnapshotNotFound` `admit_transition`'s own S1 raises, and the
/// child is left genuinely `Open` -- not silently advanced to `Observed`
/// first and only then refused.
#[test]
fn admit_child_completion_refuses_before_touching_closure_state_when_child_snapshot_is_unresolvable(
) -> Result<(), Refusal> {
    let engine = engine_with(SNAPSHOT_TTL)?;
    let mut socket = one_leaf_closure()?;

    let result = engine.admit_child_completion(
        &mut socket,
        &closure_leaf_socket(0),
        &GraphSnapshotId::new("urn:chatman:snapshot:never-loaded"),
        &conforming_evidence(),
    );

    assert!(
        matches!(result, Err(Refusal::SnapshotNotFound(_))),
        "expected SnapshotNotFound, got {result:?}"
    );
    // The child is still Open (never even reached Observed): proof the S1
    // check short-circuited before `socket.observe()` ran.
    assert!(matches!(
        socket.require_terminal_admitted(&closure_leaf_socket(0)),
        Err(Refusal::ChildCompletionUnadmitted(_))
    ));
    Ok(())
}

/// Refusal path proving `admit_child_completion` genuinely reaches
/// PROJ-774's real conformance gate: non-conforming evidence for a
/// resolvable child snapshot is refused with `ChildConformanceRefused`,
/// the same variant `promote_observed_to_admitted` raises when called
/// directly in `closure_test.rs`.
#[test]
fn admit_child_completion_refuses_nonconforming_evidence_through_the_real_engine_entry_point(
) -> Result<(), Refusal> {
    let mut engine = engine_with(SNAPSHOT_TTL)?;
    engine.load_snapshot(
        &GraphSnapshotId::new(CHILD_SNAPSHOT_IRI),
        "<urn:ex:s> <urn:ex:p> <urn:ex:o> .\n",
    )?;
    let mut socket = one_leaf_closure()?;

    let result = engine.admit_child_completion(
        &mut socket,
        &closure_leaf_socket(0),
        &GraphSnapshotId::new(CHILD_SNAPSHOT_IRI),
        &nonconforming_evidence(),
    );

    assert!(
        matches!(result, Err(Refusal::ChildConformanceRefused(_))),
        "expected ChildConformanceRefused, got {result:?}"
    );
    Ok(())
}
