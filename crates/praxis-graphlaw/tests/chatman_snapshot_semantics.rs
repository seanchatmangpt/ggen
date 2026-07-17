//! Snapshot-pinning tests for the chatman engine's S1 canonicalization law.
//!
//! **Capability note**: [`praxis_graphlaw::chatman::engine::ChatmanEngine`]'s S1
//! canonical-N-Quads hashing (`fetch_snapshot`) is a private method; the only
//! public surface that exposes canonicalized snapshot material is
//! [`EngineProcessReceipt::canon_nquads`] on an admitted transition. So this
//! file snapshots the public envelope shape (the receipt's canonical N-Quads
//! text plus its nine digest lengths as a stable, non-secret shape) rather
//! than reaching into engine internals — engine internals are not reachable
//! through the public API, this is not a fallback of convenience.
//!
//! Snapshotting uses `chicago_tdd_tools::testing::snapshot::SnapshotAssert`,
//! the framework's insta-backed wrapper (insta itself is not a direct
//! dev-dependency of this crate; `SnapshotAssert` is the sanctioned surface).

use chicago_tdd_tools::assert_matches;
use chicago_tdd_tools::prelude::*;
use chicago_tdd_tools::testing::snapshot::SnapshotAssert;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:snapshot-semantics-test";
const PROFILE_IRI: &str = "profile:snapshot-semantics-test";

/// A fixed inline-Turtle graph: deliberately unsorted subject/predicate
/// order in the source text, so the snapshot proves S1 canonicalizes
/// (sorts) regardless of input order.
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
        invocation_id: InvocationId::new("inv-snapshot-semantics"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-snapshot-semantics"),
        input_handles: InputHandles::default(),
    }
}

/// Shape of the receipt we pin: canonical N-Quads text plus the (line-count,
/// digest-length) pair for every digest — the digests themselves are content
/// hashes over volatile fixture identity strings, so pinning their exact hex
/// would make the snapshot brittle to unrelated identity changes; pinning
/// their *shape* still proves the canonicalization/receipt structure holds.
fn receipt_shape_text(canon_nquads: &str, digest_lens: &[(&str, usize)]) -> String {
    let mut out = String::new();
    out.push_str("canon_nquads (sorted lines):\n");
    for line in canon_nquads.lines() {
        out.push_str(line);
        out.push('\n');
    }
    out.push_str("\ndigest field lengths (hex chars):\n");
    for (name, len) in digest_lens {
        out.push_str(&format!("{name}: {len}\n"));
    }
    out
}

test!(s1_canonical_nquads_snapshot_is_pinned, {
    // Arrange
    let profile = build_profile()?;
    let mut engine = ChatmanEngine::in_memory(profile)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), FIXED_TURTLE)?;

    // Act
    let transition = engine.admit_transition(envelope())?;
    let receipt = transition.receipt();

    // Assert: canonical N-Quads lines are sorted (S1's canonicalization law).
    let lines: Vec<&str> = receipt.canon_nquads.lines().collect();
    let mut sorted = lines.clone();
    sorted.sort_unstable();
    assert_eq!(lines, sorted, "canonical N-Quads must be sorted");

    // Snapshot-pin the receipt's public shape.
    let shape = receipt_shape_text(
        &receipt.canon_nquads,
        &[
            ("graph_snapshot", receipt.graph_snapshot.0.len()),
            ("profile", receipt.profile.0.len()),
            ("symbol_table", receipt.symbol_table.0.len()),
            ("projection", receipt.projection.0.len()),
            ("admission_table", receipt.admission_table.0.len()),
            ("route_decision", receipt.route_decision.0.len()),
            ("tape", receipt.tape.0.len()),
            ("hook_event", receipt.hook_event.0.len()),
            ("engine_version", receipt.engine_version.0.len()),
            ("receipt_root", receipt.receipt_root.0.len()),
        ],
    );
    // Anchor the snapshot inside THIS crate: `SnapshotAssert::assert_matches`
    // expands insta's macro inside chicago-tdd-tools, so without an explicit
    // path insta would write the baseline into the dependency's repo (a
    // cross-repo mutation). An absolute CARGO_MANIFEST_DIR-based path pins
    // the baseline to crates/praxis-graphlaw/tests/snapshots/.
    SnapshotAssert::with_settings(
        |settings| {
            settings.set_snapshot_path(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/snapshots"));
        },
        || {
            SnapshotAssert::assert_matches(&shape, "chatman_s1_receipt_shape");
        },
    );
    Ok::<(), Refusal>(())
});

test!(s6_root_recomputes_over_pinned_digests_deterministically, {
    // Arrange + Act: two independent engines over the same fixed snapshot.
    let profile_a = build_profile()?;
    let mut engine_a = ChatmanEngine::in_memory(profile_a)?;
    engine_a.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), FIXED_TURTLE)?;
    let receipt_a = engine_a.admit_transition(envelope())?.receipt().clone();

    let profile_b = build_profile()?;
    let mut engine_b = ChatmanEngine::in_memory(profile_b)?;
    engine_b.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), FIXED_TURTLE)?;
    let receipt_b = engine_b.admit_transition(envelope())?.receipt().clone();

    // Assert: byte-identical receipts across independent engine instances
    // over the same fixed Turtle text (determinism, not merely a snapshot).
    assert_eq!(receipt_a, receipt_b);
    assert_eq!(receipt_a.recompute_root(), receipt_a.receipt_root);
    Ok::<(), Refusal>(())
});

// ---- TripleTermInSnapshot refusal --------------------------------------
//
// rdf-star / RDF 1.2 quoted-triple fixture support DOES exist in this
// codebase: `ChatmanEngine::load_snapshot` refuses input containing the
// `<<` token at the receipt boundary (see engine.rs module docs: the
// `rdf-12` oxigraph/oxrdf feature is off, so the boundary check is textual,
// not parser-level). This is exercised directly against the public API.

test!(triple_term_in_snapshot_is_refused_at_load_boundary, {
    // Arrange
    let profile = build_profile()?;
    let mut engine = ChatmanEngine::in_memory(profile)?;

    // Act: RDF 1.2 quoted-triple syntax.
    let result = engine.load_snapshot(
        &GraphSnapshotId::new(SNAPSHOT_IRI),
        "<< <urn:s> <urn:p> <urn:o> >> <urn:q> <urn:r> .",
    );

    // Assert
    assert_matches!(result, Err(Refusal::TripleTermInSnapshot(_)));
    Ok::<(), Refusal>(())
});
