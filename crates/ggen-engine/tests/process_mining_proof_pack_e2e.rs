//! Chicago-TDD e2e proof for `packs/process-mining-proof-pack` — real filesystem, real
//! `sync()`, real assertions on real generated file content, no mocks. Mirrors
//! `process_intelligence_rag_pack_e2e.rs`'s structure exactly.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn process_mining_proof_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: the pack's own ontology.ttl already carries the real worked instance
    //    (pmp:fortune5-k8s-planner-powl-ocel-proof) -- no extra consumer facts needed.
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-mining-proof-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let test_py = read(
        &project,
        "tests/fortune5_k8s_planner_powl_ocel_proof_test.py",
    );
    assert!(
        test_py.contains("PLANNER = \"autofde_lab.utils.load_registered_solver('Astar')\""),
        "generated test must echo the real declared planner:\n{test_py}"
    );
    assert!(
        test_py.contains(
            "OCEL_REPLAY_ENGINE = \"autofde_lab.ocel.powl_replay.replay_structural_fires\""
        ),
        "generated test must echo the real declared OCEL replay engine:\n{test_py}"
    );
    assert!(
        test_py.contains(
            "CONFORMANCE_CHECKER = \"autofde_lab.powl.conformance.check_ocel_conformance\""
        ),
        "generated test must echo the real declared conformance checker:\n{test_py}"
    );
    assert!(
        test_py.contains("def test_every_declared_stage_function_resolves_to_a_real_callable"),
        "generated test must define the real, importable-and-callable proof:\n{test_py}"
    );

    let doc = read(&project, "docs/process-mining-proof/pipelines.md");
    assert!(
        doc.contains("fortune5_k8s_planner_powl_ocel"),
        "reference doc must list the real worked pipeline:\n{doc}"
    );
    assert!(
        doc.contains("autofde_lab.ocel.powl_replay.plan_lines_to_powl_node"),
        "reference doc must cite the real POWL projector by name:\n{doc}"
    );

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn process_mining_proof_pack_gate_refuses_incomplete_pipeline() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-mining-proof-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // 4. GATE SABOTAGE: a pmp:ProofPipeline naming a planner and domain but missing
    //    powlProjector/ocelReplayEngine/conformanceChecker must be refused -- a
    //    pipeline description this incomplete would generate a test that imports
    //    functions that were never declared.
    assert_gate_refuses(
        &project,
        "@prefix pmp: <http://seanchatmangpt.github.io/packs/process-mining-proof#> .\n\
         pmp:sabotage-incomplete-pipeline a pmp:ProofPipeline ;\n\
         \x20\x20\x20\x20pmp:pipelineName \"sabotage\" ;\n\
         \x20\x20\x20\x20pmp:planner \"some.planner\" ;\n\
         \x20\x20\x20\x20pmp:domain \"some/domain\" .\n",
        "010_pipeline_grounded_or_refused",
    );
}
