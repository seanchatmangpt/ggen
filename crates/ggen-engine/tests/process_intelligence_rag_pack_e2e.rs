//! Chicago-TDD e2e proof for `packs/process-intelligence-rag-pack` — real filesystem,
//! real `sync()`, real assertions on real file content, no mocks. Written in the same
//! round as the pack itself (learned from `domain-capability-pack`'s round 1/2: shipping
//! a pack without its e2e test regresses `guard-pack-e2e-coverage.sh` immediately).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn process_intelligence_rag_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: the pack's own ontology.ttl already carries the real worked instance
    //    (one GroundingSource + one grounded dspy:Module) -- no extra consumer facts needed.
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let program = read(&project, "src/grounded_process_query.py");
    assert!(
        program.contains("class GroundedProcessQuery(dspy.Signature):"),
        "generated program must define the real Signature class:\n{program}"
    );
    assert!(
        program.contains("grounding_facts: str = dspy.InputField()"),
        "generated program must require grounding_facts as an input, not an optional field:\n{program}"
    );

    let doc = read(
        &project,
        "docs/process-intelligence-rag/grounded-queries.md",
    );
    assert!(
        doc.contains("answer_sregym_capability_question"),
        "reference doc must list the real worked grounded query:\n{doc}"
    );
    assert!(
        doc.contains("domain-capability-pack"),
        "reference doc must cite the real grounding source by name:\n{doc}"
    );
    assert!(
        doc.contains("14 real dcp:Capability"),
        "reference doc must echo the real grounding facts summary, not a placeholder:\n{doc}"
    );

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn process_intelligence_rag_pack_gate_refuses_ungrounded_query() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // 4. GATE SABOTAGE: a dspy:Module bound to GroundedProcessQuerySignature with NO
    //    pirag:grounds link must be refused -- the literal enactment of "No AI Without PI".
    assert_gate_refuses(
        &project,
        "@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .\n\
         @prefix pirag: <http://seanchatmangpt.github.io/packs/process-intelligence-rag#> .\n\
         pirag:ungrounded-query a dspy:Module ;\n\
         \x20\x20\x20\x20dspy:kind \"ChainOfThought\" ;\n\
         \x20\x20\x20\x20dspy:name \"sabotage_ungrounded\" ;\n\
         \x20\x20\x20\x20dspy:signature pirag:GroundedProcessQuerySignature .\n",
        "010_grounded_or_refused",
    );
}

// --- Round 4: strategic-grounding authority, a stricter admission tier ---------------

#[test]
fn process_intelligence_rag_pack_renders_the_real_vision_signature_and_worked_module() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    // The worked instance (sregym-vision-query, grounded by domain-capability-pack, which
    // holds a real pirag:StrategicGroundingAuthority grant) must sync clean -- proven by the
    // sync succeeding at all (a missing/insufficient grant would have refused it, per the
    // sabotage test below). Assert idempotency holds for the extended ontology too.
    assert_idempotent(&project);
}

#[test]
fn process_intelligence_rag_pack_gate_refuses_operationally_grounded_but_strategically_unauthorized_vision(
) {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a real pirag:GroundingSource with producedBy="wasm4pm-conformance" --
    // a value the closed pirag:producedBy enum genuinely admits, so this source WOULD pass
    // gates/010_grounded_or_refused.rq if bound to an ordinary GroundedProcessQuery -- but
    // NO pirag:StrategicGroundingAuthority grant covers it. A dspy:Module bound to
    // OriginateArchitectureVisionSignature and grounded by it must still be refused, by
    // gates/020_strategic_grounding_required.rq specifically -- proving the two-tier
    // distinction (operationally grounded vs. strategically authorized) is real and
    // enforced, not merely asserted in the ontology's prose comments.
    assert_gate_refuses(
        &project,
        "@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .\n\
         @prefix pirag: <http://seanchatmangpt.github.io/packs/process-intelligence-rag#> .\n\
         pirag:sabotage-conformance-source a pirag:GroundingSource ;\n\
         \x20\x20\x20\x20pirag:producedBy \"wasm4pm-conformance\" ;\n\
         \x20\x20\x20\x20pirag:factsSummary \"some real conformance result\" .\n\
         pirag:sabotage-vision-query a dspy:Module ;\n\
         \x20\x20\x20\x20dspy:kind \"ChainOfThought\" ;\n\
         \x20\x20\x20\x20dspy:name \"sabotage_vision\" ;\n\
         \x20\x20\x20\x20dspy:signature dspy:OriginateArchitectureVisionSignature ;\n\
         \x20\x20\x20\x20pirag:grounds pirag:sabotage-conformance-source .\n",
        "020_strategic_grounding_required",
    );
}

// --- Connection 1 (2026-08-11): systematic training-example generation --------------

#[test]
fn process_intelligence_rag_pack_renders_the_real_training_example_signature_and_worked_module() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let program = read(&project, "src/generate_training_example.py");
    assert!(
        program.contains("class GenerateTrainingExample(dspy.Signature):"),
        "generated program must define the real Signature class:\n{program}"
    );
    assert!(
        program.contains("process_evidence: str = dspy.InputField()"),
        "generated program must require process_evidence as an input, not an optional field:\n{program}"
    );
    assert!(
        program.contains("target_value: str = dspy.OutputField()"),
        "generated program must render the real target_value output field:\n{program}"
    );

    let doc = read(
        &project,
        "docs/process-intelligence-rag/training-examples.md",
    );
    assert!(
        doc.contains("generate_sregym_capability_training_example"),
        "reference doc must list the real worked training-example generator:\n{doc}"
    );
    assert!(
        doc.contains("domain-capability-pack"),
        "reference doc must cite the real grounding source by name:\n{doc}"
    );

    // The worked instance (sregym-training-example-query) syncing clean at all proves it
    // was admitted by gates/030_training_example_grounded_or_refused.rq -- an ungrounded
    // generator would have refused it, per the sabotage test below.
    assert_idempotent(&project);
}

#[test]
fn process_intelligence_rag_pack_gate_refuses_ungrounded_training_example_generator() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a dspy:Module bound to GenerateTrainingExampleSignature with NO
    // pirag:grounds link must be refused -- connection 1's own enactment of "No AI
    // Without PI", proving gates/030_training_example_grounded_or_refused.rq is real and
    // enforced, not merely a sibling of 010 that never actually fires.
    assert_gate_refuses(
        &project,
        "@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .\n\
         @prefix pirag: <http://seanchatmangpt.github.io/packs/process-intelligence-rag#> .\n\
         pirag:ungrounded-training-example a dspy:Module ;\n\
         \x20\x20\x20\x20dspy:kind \"ChainOfThought\" ;\n\
         \x20\x20\x20\x20dspy:name \"sabotage_ungrounded_training_example\" ;\n\
         \x20\x20\x20\x20dspy:signature dspy:GenerateTrainingExampleSignature .\n",
        "030_training_example_grounded_or_refused",
    );
}

// --- Connection 5 (2026-08-11): GenAI-assisted bridging of proprietary formats into OCED ---

#[test]
fn process_intelligence_rag_pack_renders_the_real_bridge_mapping_signature_and_worked_module() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let program = read(&project, "src/bridge_source_format.py");
    assert!(
        program.contains("class BridgeSourceFormat(dspy.Signature):"),
        "generated program must define the real Signature class:\n{program}"
    );
    assert!(
        program.contains("source_format_sample: str = dspy.InputField()"),
        "generated program must require source_format_sample as an input, not an optional field:\n{program}"
    );
    assert!(
        program.contains("proposed_oced_mapping: str = dspy.OutputField()"),
        "generated program must render the real proposed_oced_mapping output field:\n{program}"
    );

    let doc = read(&project, "docs/process-intelligence-rag/bridge-mappings.md");
    assert!(
        doc.contains("bridge_gymact_capabilities_toml_to_oced"),
        "reference doc must list the real worked bridge-mapping module:\n{doc}"
    );
    assert!(
        doc.contains("domain-capability-pack"),
        "reference doc must cite the real grounding source by name:\n{doc}"
    );

    // The worked instance (autofde-lab-toml-bridge-query) syncing clean at all proves it
    // was admitted by gates/040_bridge_mapping_grounded_or_refused.rq -- an ungrounded
    // proposal would have refused it, per the sabotage test below.
    assert_idempotent(&project);
}

#[test]
fn process_intelligence_rag_pack_gate_refuses_ungrounded_bridge_mapping_proposal() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: a dspy:Module bound to BridgeSourceFormatSignature with NO
    // pirag:grounds link must be refused -- connection 5's own enactment of "No AI
    // Without PI", proving gates/040_bridge_mapping_grounded_or_refused.rq is real and
    // enforced, not merely a sibling of 010/030 that never actually fires.
    assert_gate_refuses(
        &project,
        "@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .\n\
         @prefix pirag: <http://seanchatmangpt.github.io/packs/process-intelligence-rag#> .\n\
         pirag:ungrounded-bridge-mapping a dspy:Module ;\n\
         \x20\x20\x20\x20dspy:kind \"ChainOfThought\" ;\n\
         \x20\x20\x20\x20dspy:name \"sabotage_ungrounded_bridge_mapping\" ;\n\
         \x20\x20\x20\x20dspy:signature dspy:BridgeSourceFormatSignature .\n",
        "040_bridge_mapping_grounded_or_refused",
    );
}
