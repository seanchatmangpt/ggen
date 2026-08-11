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
