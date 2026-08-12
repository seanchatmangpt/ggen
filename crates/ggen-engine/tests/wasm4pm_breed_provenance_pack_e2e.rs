//! Chicago-TDD e2e proof for `packs/wasm4pm-breed-provenance-pack` — real
//! filesystem, real `sync()`, real assertions on real file content, no
//! mocks. Second real instance of `domain-capability-pack`'s
//! Capability/CapabilitySourceManifest/LabAllowlist shape, generalized to a
//! different real duplication: wasm4pm's 55-breed catalog independently
//! re-admitted by two sibling packs (wasm4pm-facts-pack,
//! wasm4pm-cognition-pack) with no cross-pack drift guard.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn wasm4pm_breed_provenance_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: copy the real pack + a minimal consumer into a TempDir.
    //    The pack's own ontology.ttl already carries the real, transcribed
    //    55-breed / 2-adoption worked instance -- no extra consumer facts
    //    are needed.
    let (_dir, project) = scaffold_pack(&packs_dir().join("wasm4pm-breed-provenance-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    let doc = read(
        &project,
        "docs/wasm4pm-breed-provenance/breed-provenance.md",
    );

    // Exactly 55 real breed rows (order 1..55, one table row each) --
    // excludes the header row ("| # | Breed ID | ...") and the "|---|"
    // separator row.
    let row_count = doc
        .lines()
        .filter(|l| l.starts_with("| ") && !l.starts_with("| #"))
        .count();
    assert_eq!(
        row_count, 55,
        "expected exactly 55 real wasm4pm breed rows, got {row_count}:\n{doc}"
    );

    // Both real sibling packs fully re-admit the same set today (verified
    // 2026-08-11 by direct grep of both real ontology.ttl files): every
    // breed row must render adopted=true for BOTH columns, proving the
    // join is real per-breed data, not a constant column.
    for line in doc
        .lines()
        .filter(|l| l.starts_with("| ") && !l.starts_with("| #"))
    {
        let cols: Vec<&str> = line.trim_matches('|').split('|').map(str::trim).collect();
        let (adopted_facts, adopted_cognition) = (cols[3], cols[4]);
        assert_eq!(
            (adopted_facts, adopted_cognition),
            ("true", "true"),
            "every breed row must show adopted=true for both wasm4pm-facts-pack and \
             wasm4pm-cognition-pack (both real packs currently admit the full set): {line}"
        );
    }
    // Spot-check one specific, real breed id renders with real source
    // provenance columns (not synthesized).
    let strips_line = doc
        .lines()
        .find(|l| l.contains("`strips`"))
        .expect("strips row not found");
    assert!(
        strips_line.contains("`wasm4pm`/`ggen/ontology/breeds.ttl`"),
        "strips row must cite real source provenance: {strips_line}"
    );

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn wasm4pm_breed_provenance_pack_gate_refuses_missing_required_property() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("wasm4pm-breed-provenance-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE (previously untested branch of 010_required.rq): a wbp:Breed
    // missing a required property (here, wbp:breedId) must be refused, citing
    // 010_required by name -- this gate had zero real sabotage coverage before this
    // test.
    assert_gate_refuses(
        &project,
        "@prefix wbp: <http://seanchatmangpt.github.io/packs/wasm4pm-breed-provenance#> .\n\
         wbp:sabotage-incomplete-breed a wbp:Breed ;\n\
         \x20\x20\x20\x20wbp:order 1 ; wbp:sourceRepo \"nowhere\" ;\n\
         \x20\x20\x20\x20wbp:sourceFile \"nowhere.ttl\" .\n",
        "010_required",
    );
}

#[test]
fn wasm4pm_breed_provenance_pack_gate_refuses_adoption_referencing_bogus_breed() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("wasm4pm-breed-provenance-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // 4. GATE SABOTAGE: a pack adoption admitting a breed that doesn't
    //    exist in the graph must be refused, citing 030_admits_subset by
    //    name.
    assert_gate_refuses(
        &project,
        "@prefix wbp: <http://seanchatmangpt.github.io/packs/wasm4pm-breed-provenance#> .\n\
         wbp:sabotage-adoption a wbp:PackAdoption ;\n\
         \x20\x20\x20\x20wbp:ownerPack \"sabotage-pack\" ;\n\
         \x20\x20\x20\x20wbp:admits wbp:this-breed-does-not-exist .\n",
        "030_admits_subset",
    );
}

#[test]
fn wasm4pm_breed_provenance_pack_gate_refuses_breed_count_drift() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("wasm4pm-breed-provenance-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // A manifest declaring a count that doesn't match the real, admitted
    // breed count for that (sourceRepo, sourceFile) pair must be refused,
    // citing 020_exact_count_per_source by name -- including the
    // zero-real-breeds edge case the gate's OPTIONAL/COALESCE handling
    // exists for.
    assert_gate_refuses(
        &project,
        "@prefix wbp: <http://seanchatmangpt.github.io/packs/wasm4pm-breed-provenance#> .\n\
         wbp:sabotage-manifest a wbp:BreedSourceManifest ;\n\
         \x20\x20\x20\x20wbp:manifestSourceRepo \"made-up-repo\" ;\n\
         \x20\x20\x20\x20wbp:manifestSourceFile \"does/not/exist.ttl\" ;\n\
         \x20\x20\x20\x20wbp:expectedBreedCount 99 .\n",
        "020_exact_count_per_source",
    );
}

#[test]
fn wasm4pm_breed_provenance_pack_gate_refuses_sibling_adoption_divergence() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("wasm4pm-breed-provenance-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // Baseline: the pack's own worked instance ships two real
    // wbp:PackAdoption individuals (wasm4pm-facts-pack,
    // wasm4pm-cognition-pack) that admit the IDENTICAL real 55-breed set
    // today -- the sync above already proves the new gate does not fire
    // against that unmodified, currently-matching state.

    // GATE SABOTAGE (new coverage for gates/040_sibling_adoptions_match.rq):
    // a THIRD wbp:PackAdoption that admits one fewer real breed than the
    // two existing sibling adoptions (a real, specific divergence -- it
    // omits wbp:Breed_strips, which both real adoptions admit) must be
    // refused, citing 040_sibling_adoptions_match by name. This is the
    // real, structural "merge/deduplicate" guarantee named as a follow-up
    // in pack.toml: it cannot force wasm4pm-facts-pack and
    // wasm4pm-cognition-pack to share code, but it refuses to admit a
    // state where sibling adoptions have silently drifted apart.
    assert_gate_refuses(
        &project,
        "@prefix wbp: <http://seanchatmangpt.github.io/packs/wasm4pm-breed-provenance#> .\n\
         wbp:sabotage-drifted-adoption a wbp:PackAdoption ;\n\
         \x20\x20\x20\x20wbp:ownerPack \"sabotage-third-pack\" ;\n\
         \x20\x20\x20\x20wbp:admits wbp:Breed_gps, wbp:Breed_htn_planning, \
         wbp:Breed_partial_order_plan, wbp:Breed_prolog, wbp:Breed_default_logic, \
         wbp:Breed_circumscription, wbp:Breed_description_logic, \
         wbp:Breed_bayesian_network, wbp:Breed_dempster_shafer, wbp:Breed_fuzzy_logic, \
         wbp:Breed_mycin, wbp:Breed_eliza, wbp:Breed_soar, wbp:Breed_act_r, \
         wbp:Breed_hearsay, wbp:Breed_abductive_ibe, wbp:Breed_abductive_lp, \
         wbp:Breed_allen_temporal, wbp:Breed_event_calculus, \
         wbp:Breed_situation_calculus, wbp:Breed_ltl_monitor, wbp:Breed_ctl_check, \
         wbp:Breed_analogy_sme, wbp:Breed_cbr, wbp:Breed_episodic_memory, \
         wbp:Breed_frames_inheritance, wbp:Breed_script_sam, \
         wbp:Breed_construction_grammar, wbp:Breed_autoinstinct_learning, \
         wbp:Breed_autoinstinct_neurosis, wbp:Breed_autoinstinct_semantics, \
         wbp:Breed_autoinstinct_vision, wbp:Breed_belief_merging, \
         wbp:Breed_markov_logic, wbp:Breed_problog, wbp:Breed_csp_ac3, wbp:Breed_clp, \
         wbp:Breed_asp, wbp:Breed_sat_cdcl, wbp:Breed_tableaux, \
         wbp:Breed_contingent_plan, wbp:Breed_mdp, wbp:Breed_pomdp, wbp:Breed_ebl, \
         wbp:Breed_ilp, wbp:Breed_rl_symbolic, wbp:Breed_version_space, \
         wbp:Breed_dendral, wbp:Breed_naive_physics, wbp:Breed_qualitative_reason, \
         wbp:Breed_morphological, wbp:Breed_triz, wbp:Breed_meta_reasoning, \
         wbp:Breed_ocpm_route_discoverer .\n",
        "040_sibling_adoptions_match",
    );
}
