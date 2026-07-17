//! Chicago-TDD coverage of chatman receipt integrity, using
//! `chicago_tdd_tools::observability::receipt` (`Blake3ChainValidator`,
//! `ReceiptChainBuilder`) and the real chatman engine
//! (`praxis_graphlaw::chatman::engine`).
//!
//! Two independent things are exercised here:
//!
//! 1. A synthetic 3-entry BLAKE3 chain built with `ReceiptChainBuilder`,
//!    validated with `Blake3ChainValidator`, then tampered and reconfirmed
//!    as detected — this is the generic chain-replay law chicago-tdd-tools
//!    ships, applied to a fixture chain (not chatman-specific storage).
//! 2. `ChatmanEngine::admit_transition` + `ChatmanEngine::verify_replay`
//!    over the real nine constitutional-order digests
//!    (`EngineProcessReceipt`), perturbing each of the nine fields in turn
//!    and confirming `verify_replay` returns the `ReplayMismatch` variant
//!    specific to that field.

use chicago_tdd_tools::observability::receipt::{Blake3ChainValidator, ReceiptChainBuilder};

use praxis_graphlaw::chatman::abi::{
    Digest, GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId,
    Refusal,
};
use praxis_graphlaw::chatman::engine::{
    AdmissionSpec, ChatmanEngine, EngineProfile, ReplayInputs, ReplayMismatch,
};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

// ─── Part 1: synthetic BLAKE3 chain (chicago-tdd-tools generic chain law) ──

#[test]
fn synthetic_three_entry_chain_validates_ok() {
    let entries = ReceiptChainBuilder::new()
        .add_entry(1, 0b111, 0)
        .add_entry(2, 0b011, 0)
        .add_entry(3, 0b001, 1)
        .build();

    assert_eq!(entries.len(), 3);
    Blake3ChainValidator::assert_chain_valid(&entries);
    Blake3ChainValidator::assert_tamper_evident(&entries);
}

#[test]
fn tampered_chain_entry_is_detected() {
    let mut entries = ReceiptChainBuilder::new()
        .add_entry(1, 0b111, 0)
        .add_entry(2, 0b011, 0)
        .add_entry(3, 0b001, 1)
        .build();

    // Flip a byte in the middle entry's op_trace field. This changes its
    // content_bytes without recomputing its stored_hash or the downstream
    // prev_hash link, so validation must fail starting at index 1.
    entries[1].op_trace_le[0] ^= 0xFF;

    let result = Blake3ChainValidator::validate_chain(&entries);
    assert!(
        result.is_err(),
        "tampering with entry content must be detected by chain validation"
    );
}

#[test]
fn tampered_chain_hash_field_is_detected() {
    let mut entries = ReceiptChainBuilder::new()
        .add_entry(1, 0b111, 0)
        .add_entry(2, 0b011, 0)
        .add_entry(3, 0b001, 1)
        .build();

    // Flip a byte directly in the stored chain_hash of the last entry: the
    // stored hash no longer matches BLAKE3(prev_hash || content_bytes).
    entries[2].chain_hash[0] ^= 0xFF;

    let result = Blake3ChainValidator::validate_chain(&entries);
    assert!(
        result.is_err(),
        "tampering with a stored chain_hash must be detected by chain validation"
    );
}

// ─── Part 2: chatman engine verify_replay over the nine constitutional ────
// ─── digests ────────────────────────────────────────────────────────────

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:receipts-chain-test";
const PROFILE_IRI: &str = "profile:receipts-chain-test";

/// A minimal snapshot: one RDFS fact (drives S2 OWL RL closure), one
/// trivial PDDL world (drives S3 planning), one conforming OCEL trace
/// (drives S4 admission).
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
        invocation_id: InvocationId::new("inv-receipts-chain"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-receipts-chain"),
        input_handles: InputHandles::default(),
    }
}

fn admitted_transition() -> Result<praxis_graphlaw::chatman::engine::AdmittedTransition, Refusal> {
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    engine.admit_transition(envelope())
}

fn replay_inputs() -> Result<ReplayInputs, Refusal> {
    Ok(ReplayInputs {
        envelope: envelope(),
        snapshot_turtle: SNAPSHOT_TTL.to_string(),
        profile: test_profile()?,
    })
}

/// A perturbed digest guaranteed to differ from any real recorded digest:
/// 64 hex zero characters is not a value any of our BLAKE3 digests will
/// ever produce for non-empty inputs in this fixture.
fn wrong_digest() -> Digest {
    Digest::new("0".repeat(64))
}

#[test]
fn verify_replay_accepts_faithful_receipt() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let receipt = transition.receipt();

    // All nine digests are present and well-shaped.
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
        assert_eq!(
            digest.0.len(),
            64,
            "each constitutional digest is 64 hex chars"
        );
    }

    let inputs = replay_inputs()?;
    match ChatmanEngine::verify_replay(receipt, &inputs) {
        Ok(()) => Ok(()),
        Err(mismatch) => Err(Refusal::ValidationFailed(format!(
            "faithful replay must verify, got {mismatch}"
        ))),
    }
}

#[test]
fn verify_replay_field_1_graph_snapshot_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.graph_snapshot = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::GraphSnapshot { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::GraphSnapshot, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_2_profile_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.profile = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::Profile { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::Profile, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_3_symbol_table_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.symbol_table = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::SymbolTable { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::SymbolTable, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_4_projection_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.projection = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::Projection { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::Projection, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_5_admission_table_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.admission_table = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::AdmissionTable { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::AdmissionTable, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_6_route_decision_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.route_decision = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::RouteDecision { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::RouteDecision, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_7_tape_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.tape = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::Tape { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::Tape, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_8_hook_event_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.hook_event = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::HookEvent { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::HookEvent, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_field_9_engine_version_mismatch() -> Result<(), Refusal> {
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.engine_version = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::EngineVersion { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::EngineVersion, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_receipt_root_mismatch() -> Result<(), Refusal> {
    // The tenth check: every one of the nine fields matches, but the root
    // itself was tampered — recompute_root() must catch the drift.
    let transition = admitted_transition()?;
    let inputs = replay_inputs()?;
    let mut tampered = transition.receipt().clone();
    tampered.receipt_root = wrong_digest();
    match ChatmanEngine::verify_replay(&tampered, &inputs) {
        Err(ReplayMismatch::ReceiptRoot { .. }) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::ReceiptRoot, got {other:?}"
        ))),
    }
}

#[test]
fn verify_replay_refused_when_snapshot_turtle_is_malformed() -> Result<(), Refusal> {
    // Not a digest-drift case: the replay run itself must fail to even
    // reach S1-S5, because `ChatmanEngine::load_snapshot`'s Turtle parser
    // refuses `snapshot_turtle` outright. `verify_replay` must surface that
    // as `ReplayMismatch::ReplayRefused`, not panic, not silently treat it
    // as some other mismatch variant, and not report a spurious verified
    // `Ok(())`.
    let transition = admitted_transition()?;
    let receipt = transition.receipt();

    let mut inputs = replay_inputs()?;
    inputs.snapshot_turtle = "this is not @@@ valid [[ turtle syntax at all".to_string();

    match ChatmanEngine::verify_replay(receipt, &inputs) {
        Err(ReplayMismatch::ReplayRefused(_)) => Ok(()),
        other => Err(Refusal::ValidationFailed(format!(
            "expected ReplayMismatch::ReplayRefused, got {other:?}"
        ))),
    }
}
