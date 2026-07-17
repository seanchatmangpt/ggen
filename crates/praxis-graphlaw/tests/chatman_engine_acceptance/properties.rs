//! Hand-written anti-hardcode properties for the chatman engine surface.
//!
//! Eight properties, each driven by a pinned deterministic RNG: proptest's
//! ChaCha `TestRng` seeded from a BLAKE3 digest of the property's name, so
//! every run explores byte-identical case sequences (determinism invariant).
//!
//! Three properties run today against the landed `chatman::abi` surface.
//! Five are `#[ignore = "CENG: engine lands in next phase"]`: the lane sketch
//! said to use `chatman::{router, triple8, admission8}` directly, but those
//! modules are one-line doc-comment stubs today (populated by the CE-* lanes),
//! so compilation against them is impossible; those properties are written
//! against the `harness::run_fixture` surface instead and, if un-ignored
//! before the engine lands, fail loudly with the harness's "engine not wired"
//! `ValidationFailed` — they can never pass vacuously.

use std::path::PathBuf;

use proptest::prelude::*;
use proptest::test_runner::{Config, TestRng, TestRunner};

use praxis_graphlaw::chatman::abi::{
    InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Receipt, Refusal,
    ALL_REFUSAL_NAMES,
};

use crate::harness;
use praxis_graphlaw::chatman::abi::GraphSnapshotId;

/// Cases per property. Small enough for the fast inner loop, large enough to
/// defeat hardcoded outputs.
const CASES: u32 = 64;

/// Builds a deterministic ChaCha-backed proptest runner. The 32-byte seed is
/// the BLAKE3 digest of the property tag, so distinct properties explore
/// distinct but reproducible sequences.
fn runner_for(tag: &str) -> TestRunner {
    let seed = *blake3::hash(tag.as_bytes()).as_bytes();
    let rng = TestRng::from_seed(proptest::test_runner::RngAlgorithm::ChaCha, &seed);
    TestRunner::new_with_rng(
        Config {
            cases: CASES,
            failure_persistence: None,
            ..Config::default()
        },
        rng,
    )
}

/// Strategy: a non-empty, sorted, deduplicated N-Quads-shaped line set joined
/// with newlines (canonical receipt material).
fn canonical_nquads() -> impl Strategy<Value = String> {
    proptest::collection::btree_set("<urn:s:[a-z]{1,8}> <urn:p> \"[a-z0-9]{1,12}\" \\.", 1..12)
        .prop_map(|lines| lines.into_iter().collect::<Vec<_>>().join("\n"))
}

// ─────────────────────────────────────────────────────────────────────────────
// Live properties (chatman::abi is landed)
// ─────────────────────────────────────────────────────────────────────────────

/// distinct-inputs-distinct-receipts: distinct canonical material yields
/// distinct BLAKE3 digests; identical material yields identical digests.
/// Defeats any hardcoded/constant receipt digest.
#[test]
fn prop_distinct_inputs_distinct_receipts() {
    let mut runner = runner_for("chatman:prop:distinct-inputs-distinct-receipts");
    runner
        .run(&(canonical_nquads(), canonical_nquads()), |(a, b)| {
            let ra = Receipt::from_canonical_nquads("urn:sub", "urn:wit", "replay", &a)
                .expect("sorted non-empty material must be admitted");
            let rb = Receipt::from_canonical_nquads("urn:sub", "urn:wit", "replay", &b)
                .expect("sorted non-empty material must be admitted");
            let ra2 = Receipt::from_canonical_nquads("urn:sub", "urn:wit", "replay", &a)
                .expect("sorted non-empty material must be admitted");
            prop_assert_eq!(
                ra.envelope.digest.as_inner(),
                ra2.envelope.digest.as_inner(),
                "same material, same digest"
            );
            if a != b {
                prop_assert_ne!(
                    ra.envelope.digest.as_inner(),
                    rb.envelope.digest.as_inner(),
                    "distinct material must yield distinct digests"
                );
            }
            ra.verify().expect("freshly computed receipt must verify");
            rb.verify().expect("freshly computed receipt must verify");
            Ok(())
        })
        .expect("property distinct-inputs-distinct-receipts");
}

/// replay-input-specific: the envelope hash is a function of the input —
/// permuting handle order leaves it fixed (order is not semantic), while
/// changing any identity field or handle set changes it. Defeats a replay
/// path keyed on anything other than the actual input.
#[test]
fn prop_replay_input_specific() {
    let mut runner = runner_for("chatman:prop:replay-input-specific");
    let handles = proptest::collection::btree_set("[a-z0-9]{1,10}", 0..8)
        .prop_map(|s| s.into_iter().collect::<Vec<_>>());
    let strategy = ("[a-z0-9]{1,12}", "[a-z0-9]{1,12}", handles.clone(), handles);
    runner
        .run(&strategy, |(inv, inv_other, nodes, events)| {
            let envelope = InvocationEnvelope {
                invocation_id: InvocationId::new(inv.clone()),
                snapshot_id: GraphSnapshotId::new("snap-1"),
                profile_id: ProfileId::new("profile-1"),
                operator_id: OperatorId::new("op-1"),
                input_handles: InputHandles {
                    nodes: nodes.clone(),
                    events: events.clone(),
                    plan_steps: vec![],
                },
            };
            let base = envelope.envelope_hash();

            // Permutation invariance: reversed handle order, same hash.
            let mut permuted = envelope.clone();
            permuted.input_handles.nodes.reverse();
            permuted.input_handles.events.reverse();
            prop_assert_eq!(
                &base,
                &permuted.envelope_hash(),
                "handle order is not semantic"
            );

            // Field sensitivity: a different invocation id, different hash.
            if inv_other != inv {
                let mut other = envelope.clone();
                other.invocation_id = InvocationId::new(inv_other.clone());
                prop_assert_ne!(
                    &base,
                    &other.envelope_hash(),
                    "invocation id must bind the hash"
                );
            }

            // Handle-set sensitivity: adding a fresh node changes the hash.
            let mut grown = envelope.clone();
            grown.input_handles.nodes.push(format!("zzz-fresh-{inv}"));
            prop_assert_ne!(
                &base,
                &grown.envelope_hash(),
                "handle sets must bind the hash"
            );
            Ok(())
        })
        .expect("property replay-input-specific");
}

/// refusals-specific: every refusal variant reports its own schema-contract
/// name, carries its context verbatim in the message, and survives a serde
/// round trip. Defeats a catch-all error path that collapses variants.
#[test]
fn prop_refusals_specific() {
    let mut runner = runner_for("chatman:prop:refusals-specific");
    let strategy = (0usize..ALL_REFUSAL_NAMES.len(), "[a-z0-9 ]{1,24}");
    runner
        .run(&strategy, |(index, context)| {
            let refusal = make_refusal(index, &context);
            prop_assert_eq!(
                refusal.name(),
                ALL_REFUSAL_NAMES[index],
                "name() must match the declaration-order contract"
            );
            prop_assert!(
                refusal.to_string().contains(&context),
                "message must carry the concrete offender context"
            );
            let json = serde_json::to_string(&refusal).expect("refusal serializes");
            let back: Refusal = serde_json::from_str(&json).expect("refusal deserializes");
            prop_assert_eq!(back, refusal, "serde round trip preserves the variant");
            Ok(())
        })
        .expect("property refusals-specific");
}

/// Constructs the `index`-th refusal variant (declaration order, matching
/// `ALL_REFUSAL_NAMES`) with the given context.
fn make_refusal(index: usize, ctx: &str) -> Refusal {
    let c = ctx.to_string();
    match index {
        0 => Refusal::ValidationFailed(c),
        1 => Refusal::PlanInfeasible(c),
        2 => Refusal::TraceUnlawful(c),
        3 => Refusal::HookUnpermitted(c),
        4 => Refusal::MissingReceipt(c),
        5 => Refusal::SnapshotNotFound(c),
        6 => Refusal::BoundaryRequestMissingReceipt(c),
        7 => Refusal::Triple8UniverseOverflow(c),
        8 => Refusal::TermNotInTriple8Universe(c),
        9 => Refusal::ProfileSymbolTableMismatch(c),
        10 => Refusal::ProjectionHashMismatch(c),
        11 => Refusal::WarmPathRequired(c),
        12 => Refusal::AdmissionTableMismatch(c),
        13 => Refusal::HookPatternNotAdmitted(c),
        14 => Refusal::OcelEventNotAdmitted(c),
        15 => Refusal::LeastExpressiveRouteViolation(c),
        16 => Refusal::UnsupportedDialect(c),
        17 => Refusal::N3UnavailableByProfile(c),
        18 => Refusal::N3ActuationRefused(c),
        19 => Refusal::N3CostBoundExceeded(c),
        20 => Refusal::N3BuiltinRefused(c),
        21 => Refusal::N3DirectActuationRefused(c),
        22 => Refusal::RouteDecisionMismatch(c),
        23 => Refusal::GraphSnapshotMismatch(c),
        24 => Refusal::ProfileHashMismatch(c),
        25 => Refusal::AgentOverrideDenied(c),
        26 => Refusal::WitnessNotAuthority(c),
        27 => Refusal::BreedUnpermitted(c),
        28 => Refusal::NondeterministicOperatorRequiresReceipt(c),
        29 => Refusal::ProcessReceiptShadowType(c),
        30 => Refusal::DuplicateCanonicalTapeType(c),
        31 => Refusal::TripleTermInSnapshot(c),
        32 => Refusal::StageSealMismatch(c),
        33 => Refusal::UnlawfulActuation(c),
        34 => Refusal::PowlRegionNotAdmitted(c),
        35 => Refusal::ExternalCutUndeclared(c),
        36 => Refusal::ExternalCutTypeMismatch(c),
        37 => Refusal::ExternalCutAuthorityMismatch(c),
        38 => Refusal::ClosureLawNoChildren(c),
        39 => Refusal::ClosureLawQuorumOutOfRange(c),
        40 => Refusal::ClosureLawUnknownChild(c),
        41 => Refusal::ClosureLawOrderedSubsetInvalid(c),
        42 => Refusal::ClosureLawPolicyNotDeclared(c),
        43 => Refusal::ChildConformanceRefused(c),
        44 => Refusal::ChildCompletionUnadmitted(c),
        45 => Refusal::ParentClosureUnsatisfied(c),
        _ => unreachable!("index bounded by ALL_REFUSAL_NAMES.len() in the strategy"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Engine-surface properties (ignored until chatman::engine lands)
// ─────────────────────────────────────────────────────────────────────────────

/// Writes one scenario JSON into a per-test scratch dir under `target/` and
/// returns its path (deterministic location, no temp-dir randomness).
fn write_scratch_scenario(name: &str, scenario: &serde_json::Value) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/chatman-properties-scratch");
    std::fs::create_dir_all(&dir).expect("scratch dir creatable");
    let path = dir.join(name);
    std::fs::write(
        &path,
        serde_json::to_string_pretty(scenario).expect("scenario serializes"),
    )
    .expect("scratch scenario writable");
    path
}

/// route-depends-on-shape: two routing scenarios that differ only in the
/// admitted-dialect set must not produce the same route; the chosen route is
/// the least expressive sufficient dialect. Written against the harness
/// dispatch surface because `chatman::router` is a doc-stub today.
#[test]
#[ignore = "CENG: engine lands in next phase; chatman::router is a doc-stub module today"]
fn prop_route_depends_on_shape() {
    let base = serde_json::json!({
        "case": "prop-route-shape-a",
        "kind": "acceptance",
        "seed": 11,
        "expected_behavior": "router picks the least expressive sufficient dialect",
        "input": {
            "profile_id": "profile-1",
            "requested_dialect": "shacl",
            "admitted_dialects": ["datalog", "shacl"]
        }
    });
    let narrowed = serde_json::json!({
        "case": "prop-route-shape-b",
        "kind": "acceptance",
        "seed": 11,
        "expected_behavior": "router picks the least expressive sufficient dialect",
        "input": {
            "profile_id": "profile-1",
            "requested_dialect": "shacl",
            "admitted_dialects": ["shacl", "shex", "n3"]
        }
    });
    let a = harness::run_fixture(&write_scratch_scenario("route_shape_a.json", &base))
        .expect("acceptance routing scenario must be admitted");
    let b = harness::run_fixture(&write_scratch_scenario("route_shape_b.json", &narrowed))
        .expect("acceptance routing scenario must be admitted");
    assert_ne!(
        a.detail, b.detail,
        "route must depend on the admitted-dialect shape, not on a constant"
    );
}

/// overflow-changes-outcome: an 8-bit-bounded universe admits exactly its
/// capacity; one term past the bound (the 256-vs-257 boundary in u8 space,
/// projected onto the schema's 8-term universe plus overflow_terms) flips the
/// outcome to `Triple8UniverseOverflow`. Written against the harness surface
/// because `chatman::triple8` is a doc-stub today.
#[test]
#[ignore = "CENG: engine lands in next phase; chatman::triple8 is a doc-stub module today"]
fn prop_overflow_changes_outcome() {
    let universe: Vec<String> = (0..8).map(|i| format!("t{i}")).collect();
    let at_capacity = serde_json::json!({
        "case": "prop-triple8-at-capacity",
        "kind": "acceptance",
        "seed": 256,
        "expected_behavior": "a full universe at capacity is admitted",
        "input": { "universe": universe, "terms": ["t0", "t7"] }
    });
    let past_capacity = serde_json::json!({
        "case": "prop-triple8-past-capacity",
        "kind": "falsification",
        "seed": 257,
        "mutation": "one term past the bounded capacity",
        "expected_refusal": "Triple8UniverseOverflow",
        "input": { "universe": universe, "terms": ["t0"], "overflow_terms": ["t8-overflow"] }
    });
    harness::run_fixture(&write_scratch_scenario(
        "triple8_at_capacity.json",
        &at_capacity,
    ))
    .expect("universe at capacity must be admitted");
    let refusal = harness::run_fixture(&write_scratch_scenario(
        "triple8_past_capacity.json",
        &past_capacity,
    ))
    .expect_err("one term past capacity must be refused");
    assert_eq!(refusal.name(), "Triple8UniverseOverflow");
}

/// masks-match-arithmetic-truth: admission decisions derived from the table's
/// bit masks must equal the arithmetic truth of its (pattern, admitted) u8
/// triples — a carried table hash that disagrees with the recomputed sorted
/// entries is refused. Written against the harness surface because
/// `chatman::admission8` is a doc-stub today.
#[test]
#[ignore = "CENG: engine lands in next phase; chatman::admission8 is a doc-stub module today"]
fn prop_masks_match_arithmetic_truth() {
    let tampered = serde_json::json!({
        "case": "prop-admission-mask-tamper",
        "kind": "falsification",
        "seed": 42,
        "mutation": "carried table hash disagrees with recomputed sorted entries",
        "expected_refusal": "AdmissionTableMismatch",
        "input": {
            "table_hash": "0000000000000000000000000000000000000000000000000000000000000000",
            "entries": [
                { "pattern": "hook:on-add", "admitted": true },
                { "pattern": "hook:on-remove", "admitted": false }
            ]
        }
    });
    let refusal = harness::run_fixture(&write_scratch_scenario(
        "admission_mask_tamper.json",
        &tampered,
    ))
    .expect_err("tampered admission table identity must be refused");
    assert_eq!(refusal.name(), "AdmissionTableMismatch");
}

/// actuation-requires-receipt: a replay whose envelope covers a
/// nondeterministic actuation but carries no covering receipt is refused.
#[test]
#[ignore = "CENG: engine lands in next phase; chatman::engine is a doc-stub module today"]
fn prop_actuation_requires_receipt() {
    let uncovered = serde_json::json!({
        "case": "prop-actuation-no-receipt",
        "kind": "falsification",
        "seed": 7,
        "mutation": "covering receipt list emptied",
        "expected_refusal": "NondeterministicOperatorRequiresReceipt",
        "input": {
            "envelope": {
                "invocation_id": "inv-1",
                "snapshot_id": "snap-1",
                "profile_id": "profile-1",
                "operator_id": "op-1",
                "input_handles": { "nodes": ["n1"], "events": ["e1"], "plan_steps": [] }
            },
            "receipts": []
        }
    });
    let refusal = harness::run_fixture(&write_scratch_scenario(
        "actuation_no_receipt.json",
        &uncovered,
    ))
    .expect_err("actuation without a covering receipt must be refused");
    assert_eq!(refusal.name(), "NondeterministicOperatorRequiresReceipt");
}

/// agent-cannot-override: an agent requesting an override of an operator
/// decision without authority is refused with `AgentOverrideDenied`.
#[test]
#[ignore = "CENG: engine lands in next phase; chatman::engine is a doc-stub module today"]
fn prop_agent_cannot_override() {
    let overriding = serde_json::json!({
        "case": "prop-agent-override",
        "kind": "falsification",
        "seed": 3,
        "mutation": "agent requests operator override without authority",
        "expected_refusal": "AgentOverrideDenied",
        "input": {
            "agent_id": "agent-1",
            "operator_id": "op-1",
            "override_requested": true
        }
    });
    let refusal = harness::run_fixture(&write_scratch_scenario("agent_override.json", &overriding))
        .expect_err("agent override without authority must be refused");
    assert_eq!(refusal.name(), "AgentOverrideDenied");
}
