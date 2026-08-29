//! Chicago-TDD e2e proof for `packs/fde20-revops-pack`.
//! The test exercises the real ggen sync path, validates generated artifacts,
//! proves idempotency, and sabotages the economic, topology, and BRCE gates.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn fde20_revops_pack_generates_receipted_pipeline_and_is_idempotent() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("fde20-revops-pack"));

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");

    let contract = read(&project, "revops/fde20/OPERATING_CONTRACT.md");
    assert!(contract.contains("$20/hr FDE RevOps"), "{contract}");
    assert!(contract.contains("USD 20.00/hour"), "{contract}");
    assert!(contract.contains("exception-handling-only"), "{contract}");
    assert!(contract.contains("zero unreceipted actuation") || contract.contains("every DO transition must pass BRCE"), "{contract}");

    let challenger = read(&project, "revops/fde20/CHALLENGER_MOTION.md");
    assert!(challenger.contains("The expensive part of an FDE is not typing code"), "{challenger}");
    assert!(challenger.contains("Start with one revenue workflow"), "{challenger}");

    let pipeline = read(&project, "revops/fde20/pipeline.json");
    assert!(pipeline.contains("\"rate\": 20.00"), "{pipeline}");
    assert!(pipeline.contains("\"operation\": \"DO\""), "{pipeline}");
    assert!(pipeline.contains("\"authority_boundary\": \"BRCE\""), "{pipeline}");
    assert!(pipeline.contains("\"receipt_type\": \"delivery-receipt\""), "{pipeline}");

    assert_idempotent(&project);
}

#[test]
fn fde20_revops_gate_refuses_price_drift() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("fde20-revops-pack"));

    assert_gate_refuses(
        &project,
        "@prefix fde20: <http://seanchatmangpt.github.io/packs/fde20-revops#> .\n\
         @prefix dcterms: <http://purl.org/dc/terms/> .\n\
         fde20:SabotageOffer a fde20:Offer ;\n\
         \x20\x20dcterms:title \"price drift\" ; dcterms:description \"sabotage\" ;\n\
         \x20\x20fde20:hourlyRate 21.00 ; fde20:currency \"USD\" ; fde20:billingUnit \"hour\" ;\n\
         \x20\x20fde20:deterministicFirst true ; fde20:llmRole \"exception-handling-only\" ;\n\
         \x20\x20fde20:brceRequired true ; fde20:receiptRequired true ;\n\
         \x20\x20fde20:hasStage fde20:SignalSelect ; fde20:hasChallengerMotion fde20:DefaultChallengerMotion .\n",
        "010_economic_contract",
    );
}

#[test]
fn fde20_revops_gate_refuses_skipped_stage_edge() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("fde20-revops-pack"));

    assert_gate_refuses(
        &project,
        "@prefix fde20: <http://seanchatmangpt.github.io/packs/fde20-revops#> .\n\
         @prefix dcterms: <http://purl.org/dc/terms/> .\n\
         @prefix skos: <http://www.w3.org/2004/02/skos/core#> .\n\
         fde20:SabotageOffer2 a fde20:Offer ; dcterms:title \"topology\" ; dcterms:description \"sabotage\" ;\n\
         \x20\x20fde20:hourlyRate 20.00 ; fde20:currency \"USD\" ; fde20:billingUnit \"hour\" ;\n\
         \x20\x20fde20:deterministicFirst true ; fde20:llmRole \"exception-handling-only\" ;\n\
         \x20\x20fde20:brceRequired true ; fde20:receiptRequired true ;\n\
         \x20\x20fde20:hasChallengerMotion fde20:DefaultChallengerMotion ;\n\
         \x20\x20fde20:hasStage fde20:SabotageStageA, fde20:SabotageStageB .\n\
         fde20:SabotageStageA a fde20:RevOpsStage ; skos:prefLabel \"A\" ;\n\
         \x20\x20fde20:stageOrder 1 ; fde20:stageSlug \"sabotage-a\" ; fde20:operationKind \"SELECT\" ;\n\
         \x20\x20fde20:objective \"a\" ; fde20:entryCriterion \"a\" ; fde20:exitCriterion \"a\" ;\n\
         \x20\x20fde20:standingCeiling \"OBSERVED\" ; fde20:requiresReceipt false ; fde20:nextStage fde20:SabotageStageB .\n\
         fde20:SabotageStageB a fde20:RevOpsStage ; skos:prefLabel \"B\" ;\n\
         \x20\x20fde20:stageOrder 3 ; fde20:stageSlug \"sabotage-b\" ; fde20:operationKind \"SELECT\" ;\n\
         \x20\x20fde20:objective \"b\" ; fde20:entryCriterion \"b\" ; fde20:exitCriterion \"b\" ;\n\
         \x20\x20fde20:standingCeiling \"OBSERVED\" ; fde20:requiresReceipt false .\n",
        "020_process_topology",
    );
}

#[test]
fn fde20_revops_gate_refuses_unreceipted_do() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("fde20-revops-pack"));

    assert_gate_refuses(
        &project,
        "@prefix fde20: <http://seanchatmangpt.github.io/packs/fde20-revops#> .\n\
         @prefix skos: <http://www.w3.org/2004/02/skos/core#> .\n\
         fde20:SabotageDo a fde20:RevOpsStage ; skos:prefLabel \"unreceipted do\" ;\n\
         \x20\x20fde20:stageOrder 200 ; fde20:stageSlug \"unreceipted-do\" ; fde20:operationKind \"DO\" ;\n\
         \x20\x20fde20:objective \"actuate without receipt\" ; fde20:entryCriterion \"none\" ;\n\
         \x20\x20fde20:exitCriterion \"none\" ; fde20:standingCeiling \"ALIVE\" ;\n\
         \x20\x20fde20:requiresReceipt false .\n",
        "030_brce_authority",
    );
}
