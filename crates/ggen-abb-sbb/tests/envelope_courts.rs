//! Envelope falsifier courts (wave-1 L03 plan, lane A03) — Chicago school:
//! real kernel receipts, real digests, state assertions only. Each court names
//! the falsifier it can kill.
//!
//! RED-BY-COMPILE (correct TDD, RESOLUTIONS.md sequencing: "A10/A03/A17 courts
//! independent of engine land (RED-first is correct)"). This file pins the
//! agreed seam for A02/A07 to satisfy:
//!
//! - `ggen_abb_sbb::Envelope` — bounded-resource envelope binding an ontology
//!   hash, a graph digest, a `CapTable`, and the manufacture `Receipt`.
//!   `Envelope::cap_table(&self) -> &CapTable` is the accessor the courts read.
//! - `ggen_abb_sbb::CapTable` — declared cap id -> u64 bound map.
//! - `ggen_abb_sbb::envelope_digest(&Envelope) -> String` — canonical digest;
//!   every envelope field (including every cap entry) must move it.
//! - `ggen_abb_sbb::envelope_court(&Envelope) -> ReplayVerdict` — the court:
//!   `ReplayVerdict::{Pass, VacuousGate{..}, Refused{..}}` with `is_pass()`
//!   and `exit_code()` (Pass=0, VacuousGate=1, Refused=2). `VacuousGate` is
//!   the typed exit-1 verdict for a court that consumed an undeclared cap
//!   (RESOLUTIONS.md refusal registry: `VACUOUS_COURT` family).

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_abb_sbb::*;
use std::collections::BTreeMap;

const FIXTURE: &str = include_str!("../fixtures/ea-graph.json");

fn gen() -> Generator {
    Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.26".into(),
    }
}

fn graph() -> EaGraph {
    parse_graph(FIXTURE).expect("fixture admits")
}

/// The nominal receipt for the fixture graph, as the kernel itself produces it.
fn nominal_receipt() -> ManufactureReceipt {
    let g = graph();
    let ad = admit(
        &g,
        &Request {
            abb: "abb:event-ingest".into(),
            sbb: "sbb:ingest-0000".into(),
            requested_authority: Authority::Construct,
            expected_graph_digest: Some(g.digest()),
            // A07's generation lease fields (FreshEpochLease): generation 0
            // admits against any baseline — the pre-lease default.
            artifact_baseline_generation: 0,
            receipt_generation: 0,
        },
    )
    .expect("fixture SBB admits");
    manufacture(&ad, &gen())
        .expect("fixture SBB manufactures")
        .receipt
}

fn cap_table(entries: &[(&str, u64)]) -> CapTable {
    CapTable {
        caps: entries
            .iter()
            .map(|(id, max)| (id.to_string(), *max))
            .collect::<BTreeMap<_, _>>(),
    }
}

/// Nominal envelope: ontology hash bound, graph digest bound, one declared cap
/// per artifact the receipt actually produced, receipt embedded.
fn nominal_envelope() -> Envelope {
    let g = graph();
    let receipt = nominal_receipt();
    let caps = cap_table(&[(
        "cap:event-ingest.max_artifacts",
        receipt.outputs.len() as u64,
    )]);
    Envelope {
        ontology_hash: g.digest(),
        graph_digest: g.digest(),
        cap_table: caps,
        receipt,
    }
}

// --- Court 1: nominal cap table / receipt PASS --------------------------------------

/// Falsifies "the envelope court never passes": a self-consistent envelope
/// (declared caps cover exactly what the receipt consumed, digests bound) must
/// yield a PASS verdict with exit code 0 — and the digest must be stable
/// across recomputation (receipt-chain discipline).
#[test]
fn nominal_cap_table_envelope_passes_with_exit_0() {
    let env = nominal_envelope();
    let d1 = envelope_digest(&env);
    let d2 = envelope_digest(&env);
    assert_ne!(d1, String::new(), "envelope digest must be bound");
    assert_eq!(d1, d2, "envelope digest must be deterministic");

    let verdict = envelope_court(&env);
    assert!(
        verdict.is_pass(),
        "nominal envelope must PASS, got {verdict:?}"
    );
    assert_eq!(verdict.exit_code(), 0);

    // The court saw the real cap table accessor, not a copy the test faked.
    assert_eq!(
        env.cap_table().caps.get("cap:event-ingest.max_artifacts"),
        Some(&3),
        "fixture receipt produces 3 outputs; the declared cap must bind them"
    );
}

// --- Court 2: cap-raise x10 mutant ---------------------------------------------------

/// Falsifies "the envelope digest is decorative": raising every declared cap
/// x10 MUST change the envelope digest (the caps are inside the digest input)
/// and MUST flip the verdict off PASS (the raised caps no longer match the
/// receipt-bound contract ceiling). A court that passes both the original and
/// the mutant is vacuous.
#[test]
fn cap_raise_x10_mutant_changes_envelope_digest_and_flips_verdict() {
    let base = nominal_envelope();
    let base_digest = envelope_digest(&base);
    assert!(envelope_court(&base).is_pass());

    let mut mutant = base.clone();
    let raised: Vec<(String, u64)> = mutant
        .cap_table()
        .caps
        .iter()
        .map(|(k, v)| (k.clone(), v.saturating_mul(10)))
        .collect();
    for (k, v) in raised {
        mutant.cap_table_mut().caps.insert(k, v);
    }

    assert_ne!(
        envelope_digest(&mutant),
        base_digest,
        "x10 cap raise left the envelope digest unchanged — caps are not digest-bound"
    );
    let verdict = envelope_court(&mutant);
    assert!(
        !verdict.is_pass(),
        "x10-raised cap table must not PASS: {verdict:?}"
    );
    assert_ne!(
        verdict.exit_code(),
        0,
        "flipped verdict must carry a non-zero exit code"
    );
}

// --- Court 3: undeclared cap is typed VacuousGate (exit 1) ---------------------------

/// Falsifies "an undeclared cap slips through as a pass (or as a panic)": an
/// envelope whose receipt consumes a cap id absent from the cap table must
/// yield the TYPED `VacuousGate` verdict — not `Pass`, not a generic
/// `Refused` — carrying the undeclared cap id, with exit-1 semantics
/// (RESOLUTIONS.md: exit 1 = VACUOUS GATE, loud, never silent).
#[test]
fn undeclared_cap_produces_typed_vacuous_gate_verdict() {
    let mut env = nominal_envelope();
    // The receipt consumed cap:event-ingest, but the envelope "forgets" to
    // declare it — the exact vacuity shape the court exists to kill.
    env.cap_table_mut().caps.clear();

    let verdict = envelope_court(&env);
    let ReplayVerdict::VacuousGate {
        ref undeclared,
        ref detail,
    } = verdict
    else {
        panic!("expected typed VacuousGate, got {verdict:?}");
    };
    assert_eq!(
        undeclared.as_deref(),
        Some("cap:event-ingest"),
        "the verdict must name the undeclared cap"
    );
    assert!(!detail.is_empty(), "VacuousGate carries a detail string");
    assert!(!verdict.is_pass());
    assert_eq!(
        verdict.exit_code(),
        1,
        "VACUOUS GATE is exit-1 semantics, distinct from Refused (2)"
    );

    // The digest still moved: the undeclared-cap envelope is a different
    // subject, not a digest-colliding twin of the nominal one.
    assert_ne!(envelope_digest(&env), envelope_digest(&nominal_envelope()));
}

// --- Meta-falsifier: anti-vacuity of the envelope digest -----------------------------

/// Falsifies "the envelope digest ignores its own fields": every single-field
/// mutation of a valid envelope (ontology hash, graph digest, each declared
/// cap) must change `envelope_digest`. A field whose mutation the digest
/// survives is not bound by the receipt chain.
#[test]
fn every_single_field_mutation_of_the_envelope_changes_its_digest() {
    let base = nominal_envelope();
    let base_digest = envelope_digest(&base);

    let mutants: [fn(&mut Envelope); 5] = [
        |e| e.ontology_hash.push('x'),
        |e| e.graph_digest.push('x'),
        |e| {
            let k = e.cap_table().caps.keys().next().unwrap().clone();
            let v = e.cap_table_mut().caps.get_mut(&k).unwrap();
            *v += 1;
        },
        |e| {
            e.cap_table_mut()
                .caps
                .insert("cap:extra-undeclared".into(), 1);
        },
        |e| e.receipt.receipt_digest.push('x'),
    ];
    for (i, m) in mutants.iter().enumerate() {
        let mut env = nominal_envelope();
        m(&mut env);
        assert_ne!(
            envelope_digest(&env),
            base_digest,
            "mutant {i} left the envelope digest unchanged — field is unbound"
        );
    }
}
