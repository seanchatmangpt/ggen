//! Chicago-TDD e2e proof for `packs/standing-ladder-pack` — real filesystem,
//! real `sync()`, real assertions on real file content, no mocks. Same
//! four-part pattern as `process_intelligence_rag_pack_e2e.rs` /
//! `domain_capability_pack_e2e.rs`: one positive worked-instance test plus
//! three sabotage tests proving the "no skipped rungs, no empty evidence,
//! no missing chain" gate is real and enforced, not merely documented.

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn standing_ladder_pack_admits_the_real_worked_instance_chain() {
    // 1. SCAFFOLD: the pack's own ontology.ttl already carries the real worked
    //    instance (dcp:sregym-run-kubectl's real 6-transition chain to ADMITTED)
    //    -- no extra consumer facts needed.
    let (_dir, project) = scaffold_pack(&packs_dir().join("standing-ladder-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let doc = read(&project, "docs/standing-ladder/audit-trail.md");
    assert!(
        doc.contains("standing-ladder#sregym-run-kubectl-claim"),
        "audit-trail doc must list the real worked claim:\n{doc}"
    );
    assert!(
        doc.contains("domain-capability#sregym-run-kubectl"),
        "audit-trail doc must cite the real tracked fact by IRI:\n{doc}"
    );
    assert!(
        doc.contains("ADMITTED"),
        "audit-trail doc must echo the real current standing:\n{doc}"
    );
    assert!(
        doc.contains(
            "OBSERVED: directly read from ~/gymact/src/gymact/gyms/sregym.py's \
             SREGYM_CAPABILITIES tuple"
        ),
        "audit-trail doc must echo the real, specific evidenceRef text for the \
         first transition, not a placeholder:\n{doc}"
    );
    assert!(
        doc.contains(
            "ADMITTED: gates/010_no_skipped_states.rq (this pack) ran clean against \
             the full 6-transition chain"
        ),
        "audit-trail doc must echo the real, specific evidenceRef text for the \
         final transition, not a placeholder:\n{doc}"
    );
    // All 6 real transitions must be present, in order.
    for (order, from, to) in [
        (1, "UNKNOWN", "OBSERVED"),
        (2, "OBSERVED", "VALIDATED"),
        (3, "VALIDATED", "DERIVED"),
        (4, "DERIVED", "CANDIDATE"),
        (5, "CANDIDATE", "EXPERIMENTALLY_SUPPORTED"),
        (6, "EXPERIMENTALLY_SUPPORTED", "ADMITTED"),
    ] {
        let row = format!("| {order} | {from} | {to} |");
        assert!(
            doc.contains(&row),
            "audit-trail doc must contain the real transition row {row}:\n{doc}"
        );
    }

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn standing_ladder_pack_gate_refuses_a_skipped_rung() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("standing-ladder-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a claim whose only transition jumps straight from OBSERVED
    // to ADMITTED (skipping VALIDATED/DERIVED/CANDIDATE/EXPERIMENTALLY_SUPPORTED)
    // must be refused -- the transition never fills any of the required
    // intermediate rungs, so gates/010_no_skipped_states.rq reports the first
    // missing rung and refuses admission.
    assert_gate_refuses(
        &project,
        "@prefix stl: <http://seanchatmangpt.github.io/packs/standing-ladder#> .\n\
         stl:sabotage-skip-claim a stl:StandingClaim ;\n\
         \x20\x20\x20\x20stl:aboutFact stl:sabotage-skip-fact ;\n\
         \x20\x20\x20\x20stl:hasStanding stl:ADMITTED .\n\
         stl:sabotage-skip-t1 a stl:StandingTransition ;\n\
         \x20\x20\x20\x20stl:aboutClaim stl:sabotage-skip-claim ;\n\
         \x20\x20\x20\x20stl:fromState stl:UNKNOWN ; stl:toState stl:OBSERVED ;\n\
         \x20\x20\x20\x20stl:transitionOrder 1 ;\n\
         \x20\x20\x20\x20stl:evidenceRef \"a real observation\" .\n\
         stl:sabotage-skip-t2 a stl:StandingTransition ;\n\
         \x20\x20\x20\x20stl:aboutClaim stl:sabotage-skip-claim ;\n\
         \x20\x20\x20\x20stl:fromState stl:OBSERVED ; stl:toState stl:ADMITTED ;\n\
         \x20\x20\x20\x20stl:transitionOrder 2 ;\n\
         \x20\x20\x20\x20stl:evidenceRef \"a transition that skips 4 rungs\" .\n",
        "010_no_skipped_states",
    );
}

#[test]
fn standing_ladder_pack_gate_refuses_empty_evidence_ref() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("standing-ladder-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a single-rung claim (UNKNOWN -> OBSERVED) whose only
    // transition carries an EMPTY stl:evidenceRef must be refused -- the
    // transition exists and is exactly one rung, but the empty evidence
    // string fails the STRLEN(?ev) > 0 filter, so it does not count as real
    // evidence and the required rung is reported missing.
    assert_gate_refuses(
        &project,
        "@prefix stl: <http://seanchatmangpt.github.io/packs/standing-ladder#> .\n\
         stl:sabotage-empty-claim a stl:StandingClaim ;\n\
         \x20\x20\x20\x20stl:aboutFact stl:sabotage-empty-fact ;\n\
         \x20\x20\x20\x20stl:hasStanding stl:OBSERVED .\n\
         stl:sabotage-empty-t1 a stl:StandingTransition ;\n\
         \x20\x20\x20\x20stl:aboutClaim stl:sabotage-empty-claim ;\n\
         \x20\x20\x20\x20stl:fromState stl:UNKNOWN ; stl:toState stl:OBSERVED ;\n\
         \x20\x20\x20\x20stl:transitionOrder 1 ;\n\
         \x20\x20\x20\x20stl:evidenceRef \"\" .\n",
        "010_no_skipped_states",
    );
}

#[test]
fn standing_ladder_pack_gate_refuses_a_standing_with_no_transition_chain_at_all() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("standing-ladder-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a claim declared hasStanding=ADMITTED with ZERO
    // stl:StandingTransition individuals anywhere -- the strongest form of
    // an unsupported claim (self-certified standing, no evidence at all)
    // must be refused.
    assert_gate_refuses(
        &project,
        "@prefix stl: <http://seanchatmangpt.github.io/packs/standing-ladder#> .\n\
         stl:sabotage-no-chain-claim a stl:StandingClaim ;\n\
         \x20\x20\x20\x20stl:aboutFact stl:sabotage-no-chain-fact ;\n\
         \x20\x20\x20\x20stl:hasStanding stl:ADMITTED .\n",
        "010_no_skipped_states",
    );
}
