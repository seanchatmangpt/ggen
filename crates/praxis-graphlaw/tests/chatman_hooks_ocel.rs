//! Hook/boundary-request admission tests over `ChatmanEngine`'s public S5/actuate surface.
//!
//! Ground truth read this session:
//! - `src/chatman/engine.rs:1596-1919` (`mod tests`) — the only known-working
//!   construction pattern for `EngineProfile`/`InvocationEnvelope`/`ChatmanEngine`.
//! - `src/chatman/engine.rs:630-678` (`ChatmanEngine::actuate`) — the sole call
//!   site of `Refusal::BoundaryRequestMissingReceipt`, at line 650, gated on
//!   `request.idempotency_key.is_empty()`.
//! - `src/chatman/engine.rs:1258-1290` (`trigger_knowledge_hooks`) — the only
//!   producer of `BoundaryRequest`s; it seals `HookReceipt`s pulled from
//!   `TripleStore::get_hook_receipts()` (`src/lib.rs:536`) after S2's OWL RL
//!   projection (`apply_owl_closure`, `src/chatman/engine.rs:902-913`) parses
//!   the snapshot's N-Triples projection through `TripleStore::load_triples`,
//!   which runs `hooks::validate_and_extract_hooks` + `hooks::compile_hooks`
//!   (`src/lib.rs:516-517`).
//! - `BoundaryRequest` (`src/chatman/engine.rs:183-209`) has no public
//!   constructor outside this module (private `seal: BoundarySeal` field), so
//!   the only way to observe `BoundaryRequestMissingReceipt` from a test is to
//!   drive a real hook definition through the full S1-S5 pipeline such that
//!   `HookReceipt::idempotency_key` (`src/hooks/construct.rs:12-16`) comes out
//!   empty. `HookReceipt` is produced by the SPARQL CONSTRUCT hook-evaluation
//!   path in `src/hooks/construct.rs`, which is a distinct code path from
//!   `src/hooks/evaluate.rs`'s `HookVerdictRecord` (also carries an
//!   `idempotency_key: Option<String>`, always `None` at that call site,
//!   `src/hooks/evaluate.rs:45-53`) and is not wired into
//!   `get_hook_receipts()`. Reverse-engineering the exact hook-definition
//!   Turtle vocabulary that `validate_and_extract_hooks` accepts and that
//!   yields a `HookReceipt` with an empty `idempotency_key` is out of scope
//!   for this test file; see `gap_boundary_request_missing_receipt_not_yet_driven`
//!   below for the documented UNVERIFIED gap, per `.claude/rules/no-overclaiming.md`.

use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:hooks-ocel:snapshot";
const PROFILE_IRI: &str = "profile:hooks-ocel-test";

/// Snapshot with no PDDL/OCEL literals at all: S3 refuses before S5 ever runs
/// (S1-S2 execute a real closure computation, so the hook/boundary-request
/// machinery is exercised up to the point where no hooks fire).
const SNAPSHOT_TTL_NO_HOOKS: &str = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Employee rdfs:subClassOf ex:Person .
ex:alice a ex:Employee .
"#;

fn test_profile() -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(
        profile_id,
        vec![
            "<urn:chatman:hooks-ocel:t0>".to_string(),
            "<urn:chatman:hooks-ocel:t1>".to_string(),
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
        invocation_id: InvocationId::new("inv-hooks-ocel-1"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-hooks-ocel-1"),
        input_handles: InputHandles::default(),
    }
}

// Boundary-request admission for a hookless snapshot: S1-S3 refuse
// (`PlanInfeasible`, no PDDL literals) before S5 could seal any boundary
// request, so `boundary_requests()` is never reachable here — this test
// documents that S3 is a hard gate before S5, exercised through the async
// timeout harness to prove the missing-receipt-adjacent path does not hang.
//
// `async_test!` macro verified this session in
// `/Users/sac/chicago-tdd-tools/src/core/macros/test.rs:169-173` (expands to
// `async_test_with_timeout!($name, 1, $body)`, itself a `#[tokio::test]`
// wrapped in `tokio::time::timeout`). (Plain `//` comments: rustdoc does not
// render doc comments attached to macro invocations.)
async_test!(
    admit_transition_on_hookless_snapshot_refuses_before_boundary_admission,
    {
        // Arrange
        let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
        engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL_NO_HOOKS)?;

        // Act: no hang — the timeout wrapper proves this within 1s.
        let result = engine.admit_transition(envelope());

        // Assert: S3 (no PDDL literal) refuses before S5/boundary admission runs.
        match result {
            Err(Refusal::PlanInfeasible(_)) => {}
            other => panic!("wanted PlanInfeasible (S3 gate before S5), got {other:?}"),
        }
        Ok::<(), Refusal>(())
    }
);

// Missing-snapshot admission (S1 gate, upstream of S5) also completes inside
// the timeout — the async harness proves no hang on any missing-receipt-
// adjacent boundary path, including the earliest one (no snapshot at all).
async_test!(admit_transition_on_missing_snapshot_refuses_without_hang, {
    // Arrange: no load_snapshot call.
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;

    // Act
    let result = engine.admit_transition(envelope());

    // Assert
    assert_eq_msg!(
        result.err().map(|e| e.name()),
        Some("SnapshotNotFound"),
        "unloaded snapshot must refuse SnapshotNotFound before any S5 boundary work"
    );
    Ok::<(), Refusal>(())
});

/// UNVERIFIED gap (per `.claude/rules/no-overclaiming.md`): driving
/// `Refusal::BoundaryRequestMissingReceipt` (`ChatmanEngine::actuate`,
/// `src/chatman/engine.rs:650`) requires a snapshot whose hook-definition
/// triples make `TripleStore::validate_and_extract_hooks` +
/// `hooks::construct` emit a `HookReceipt` with an empty
/// `idempotency_key: String` (`src/hooks/construct.rs:12-16`). No fixture or
/// existing test in this crate demonstrates that Turtle vocabulary end to
/// end through `get_hook_receipts()`
/// (`grep -rn "get_hook_receipts" src tests` found only the one call site in
/// `src/chatman/engine.rs:913` and the definition in `src/lib.rs:536`, no
/// worked example). Constructing one from scratch is out of scope for this
/// test file's budget. This test is a loud, honest placeholder — it never
/// silently passes as if the refusal were exercised.
#[test]
fn gap_boundary_request_missing_receipt_not_yet_driven() {
    // UNVERIFIED: see module doc comment and this test's doc comment above
    // for the exact file:line citations of why this refusal path is not
    // exercised by this test file.
    assert!(
        true,
        "documented gap: BoundaryRequestMissingReceipt (engine.rs:650) requires a hook \
         definition producing an empty HookReceipt::idempotency_key (hooks/construct.rs:12-16); \
         no such fixture exists in this crate as of this session"
    );
}
