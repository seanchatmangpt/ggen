//! Real end-to-end regression coverage for `packs/dspy-pack`. Uses the real
//! `sync()` pipeline (`tests/support::scaffold_pack_with_ontology`, no
//! mocks), a real temp consumer project wired to the pack by relative path,
//! and asserts on real generated Python file content on disk.
//!
//! Each fixture below is a real, on-disk `examples/<name>/ontology.ttl` (the
//! repo's standard reference-fixture convention, matching e.g.
//! `examples/fortune5-architecture` -- one directory per consumer project
//! under the top-level `examples/`, referencing its pack via
//! `path = "../../packs/<pack-name>"`, NOT nested inside the pack directory
//! itself) that a background-agent verification pass (2026-08-10) already proved
//! generates code that constructs real `dspy` 3.1.3 objects (Signatures,
//! Modules, Optimizers, Pydantic-typed fields) without exception — see each
//! example directory's own comments for that session's findings, including
//! three real upstream `dspy` defects (`SignatureOptimizer`/`AvatarOptimizer`/
//! `BetterTogether`) documented as caveats in `ontology.ttl`, not hidden.
//!
//! This test does NOT re-run Python — that would make `cargo test` depend on
//! a local `dspy` install, which the rest of this workspace's test suite
//! never assumes. It re-runs the real `ggen sync` pipeline (ontology load,
//! SPARQL extraction, Tera rendering, the pack's own `gates/010_admission.rq`
//! admission gate) and asserts on the real generated Python source text, so a
//! future edit to `ontology.ttl`/`gates/010_admission.rq`/
//! `templates/dspy_*.tmpl` that breaks generation for any of these fixtures
//! fails `cargo test`/`just pre-commit` immediately instead of silently
//! drifting until the next manual verification pass.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use support::{read, scaffold_pack_with_ontology};

fn pack_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../packs")
        .join("dspy-pack")
}

/// Sync `fixture_domain_ttl` against a fresh copy of `packs/dspy-pack` and
/// return the real generated `src/dspy_program.py` content.
fn sync_and_read_program(fixture_domain_ttl: &str) -> String {
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), fixture_domain_ttl);
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect("dspy-pack fixture must sync cleanly through the real admission gate");
    read(&project, "src/dspy_program.py")
}

/// Same as [`sync_and_read_program`] but also returns `src/dspy_optimize.py`
/// — for fixtures that declare `dspy:Optimizer` individuals.
fn sync_and_read_both(fixture_domain_ttl: &str) -> (String, String) {
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), fixture_domain_ttl);
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect("dspy-pack fixture must sync cleanly through the real admission gate");
    (
        read(&project, "src/dspy_program.py"),
        read(&project, "src/dspy_optimize.py"),
    )
}

#[test]
fn mcp_react_fixture_generates_real_tool_discovery_agent() {
    let ttl = include_str!("../../../examples/gepa-trusted-monitor/ontology.ttl");
    let (program, optimize) = sync_and_read_both(ttl);
    assert!(program.contains("class PredictSuspicionScore(dspy.Signature):"));
    assert!(program.contains("suspicion_score: int = dspy.OutputField("));
    assert!(
        program.contains("ge=0, le=100"),
        "real ge/le bounds must render: {program}"
    );
    assert!(optimize.contains("dspy.GEPA("));
    assert!(optimize.contains("max_full_evals=2"));
    // dspy:LMConfig regression guard: reflection_lm must render as a real dspy.LM(...) call
    // with its real kwargs (temperature/max_tokens), not a bare model-id string -- this is
    // the tutorial's own exact real code (dspy.LM(model="gpt-5", temperature=1.0,
    // max_tokens=32000, ...)), confirmed against real dspy 3.1.3 construction.
    assert!(
        optimize
            .contains(r#"reflection_lm=dspy.LM("openai/gpt-5", temperature=1, max_tokens=32000),"#),
        "LMConfig must render real dspy.LM kwargs, not a bare string:\n{optimize}"
    );
}

#[test]
fn gepa_facility_support_analyzer_renders_literal_and_list_literal_fields() {
    let ttl = include_str!("../../../examples/gepa-facility-support-analyzer/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("from typing import List, Literal"));
    assert!(program.contains("Literal[\"low\", \"medium\", \"high\"]"));
    assert!(
        program.contains("List[Literal["),
        "categories field must render List[Literal[...]]: {program}"
    );
}

#[test]
fn all_modules_coverage_renders_all_nine_admitted_module_kinds() {
    let ttl = include_str!("../../../examples/all-modules-coverage/ontology.ttl");
    let program = sync_and_read_program(ttl);
    for expected in [
        "dspy.ChainOfThought(AnswerQuestion)",
        "dspy.MultiChainComparison(AnswerQuestion, M=4, temperature=0.5)",
        "dspy.ProgramOfThought(AnswerQuestion, max_iters=5)",
        "dspy.RLM(AnswerQuestion,",
        "dspy.CodeAct(AnswerQuestion, tools=[lookup_price]",
        "dspy.BestOfN(module=base_module,",
        "dspy.Refine(module=base_module,",
    ] {
        assert!(
            program.contains(expected),
            "missing `{expected}` in:\n{program}"
        );
    }
}

#[test]
fn bootstrap_family_optimizers_render_real_constructor_and_compile_kwargs() {
    let ttl = include_str!("../../../examples/optimizers-bootstrap-family/ontology.ttl");
    let (_program, optimize) = sync_and_read_both(ttl);
    for expected in [
        "dspy.BootstrapFewShot(",
        "dspy.BootstrapFewShotWithRandomSearch(",
        "dspy.BootstrapFewShotWithOptuna(",
        "dspy.BootstrapFinetune(",
        "multitask=False,",
    ] {
        assert!(
            optimize.contains(expected),
            "missing `{expected}` in:\n{optimize}"
        );
    }
    // Regression guard for the real bug a 2026-08-10 verification agent found:
    // a boolean optimizer property must render Python's `True`/`False`, never
    // Tera's lowercase `true`/`false` (invalid Python — `NameError` at import).
    assert!(
        !optimize.contains("multitask=false,"),
        "lowercase Tera boolean leaked into generated Python:\n{optimize}"
    );
    assert!(
        !optimize.contains("multitask=true,"),
        "lowercase Tera boolean leaked into generated Python:\n{optimize}"
    );
}

#[test]
fn prompt_optimization_family_uses_correct_per_kind_compile_signature() {
    let ttl = include_str!("../../../examples/optimizers-prompt-family/ontology.ttl");
    let (_program, optimize) = sync_and_read_both(ttl);
    assert!(optimize.contains("dspy.COPRO("));
    assert!(optimize.contains("dspy.SignatureOptimizer("));
    assert!(optimize.contains("dspy.MIPROv2("));
    // Real compile() shapes verified against dspy 3.1.3 by inspect.signature():
    // SignatureOptimizer takes `devset=`, not `trainset=`; COPRO/SignatureOptimizer
    // both require the `eval_kwargs` dict (rendered as a placeholder).
    assert!(
        optimize.contains("devset=devset, eval_kwargs={}"),
        "SignatureOptimizer must compile with devset=, not trainset=:\n{optimize}"
    );
    assert!(
        optimize.contains("trainset=trainset, eval_kwargs={}"),
        "COPRO must compile with trainset=+eval_kwargs=:\n{optimize}"
    );
    // dspy:LMConfig regression guard: prompt_model/task_model must render real dspy.LM kwargs
    // (a shared LMConfig individual reused across COPRO/SignatureOptimizer, and a distinct one
    // for MIPROv2's task_model), not bare model-id strings.
    assert!(
        optimize.contains(r#"dspy.LM("openai/gpt-4o-mini", temperature=0.7, max_tokens=4000)"#),
        "promptModel must render real dspy.LM kwargs:\n{optimize}"
    );
    assert!(
        optimize.contains(r#"dspy.LM("openai/gpt-4o", max_tokens=8000, num_retries=5)"#),
        "taskModel must render real dspy.LM kwargs:\n{optimize}"
    );
}

#[test]
fn misc_family_optimizers_include_bettertogether_wrapping_another_optimizer() {
    let ttl = include_str!("../../../examples/optimizers-misc-family/ontology.ttl");
    let (_program, optimize) = sync_and_read_both(ttl);
    assert!(optimize.contains("dspy.AvatarOptimizer("));
    assert!(optimize.contains("dspy.InferRules("));
    assert!(optimize.contains("dspy.SIMBA("));
    assert!(optimize.contains("dspy.LabeledFewShot("));
    assert!(optimize.contains("dspy.BetterTogether("));
    // BetterTogether must reference another already-constructed `<name>_optimizer`
    // variable (the module-wrapping mechanism), not a bare Module.
    assert!(
        optimize.contains("prompt_optimizer=avatar_opt_optimizer,"),
        "BetterTogether must wrap the real `_optimizer` variable of another Optimizer:\n{optimize}"
    );
    // Regression guard for the real gate bug a 2026-08-10 verification agent
    // found: dspy.AvatarOptimizer's real `lower_bound` default is 0 (a
    // legitimate, non-positive value) — the gate must accept it.
    assert!(
        optimize.contains("lower_bound=0,"),
        "lower_bound=0 (real dspy default) must be accepted by the gate:\n{optimize}"
    );
}

#[test]
fn pydantic_model_fields_render_real_basemodel_classes_both_directions() {
    let ttl = include_str!("../../../examples/pydantic-model-fields/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("import pydantic"));
    assert!(
        program.contains("class Address(pydantic.BaseModel):"),
        "OutputField-direction model missing:\n{program}"
    );
    assert!(
        program.contains("class ContactInfo(pydantic.BaseModel):"),
        "InputField-direction model missing:\n{program}"
    );
}

#[test]
fn rag_pipeline_fixture_generates_real_retrieve_then_reason_module() {
    let ttl = include_str!("../../../examples/rag-pipeline/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(
        program.contains("class Rag(dspy.Module):"),
        "Pipeline module must render as a real dspy.Module subclass:\n{program}"
    );
    assert!(program.contains("self.retrieve = dspy.Retrieve(k=3)"));
    assert!(program.contains("self.generate_answer = dspy.ChainOfThought(AnswerQuestion)"));
    assert!(
        program.contains("retrieve_result = self.retrieve(question)"),
        "Retrieve step must be called positionally with its PipelineInput-bound arg:\n{program}"
    );
    assert!(
        program.contains(
            "generate_answer_result = self.generate_answer(context=retrieve_result.passages, question=question)"
        ),
        "downstream step must resolve its StepOutput/PipelineInput ArgBindings:\n{program}"
    );
    assert!(program.contains("return dspy.Prediction(answer=generate_answer_result.answer)"));
    assert!(
        program.contains("rag = Rag()"),
        "Pipeline module must also render a top-level instance, matching every other module kind:\n{program}"
    );
}

#[test]
fn multi_hop_qa_fixture_generates_real_accumulate_context_loop() {
    let ttl = include_str!("../../../examples/multi-hop-qa/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("class MultiHop(dspy.Module):"));
    assert!(program.contains("self.max_hops = 2"));
    assert!(program.contains("self.generate_query = dspy.ChainOfThought(GenerateSearchQuery)"));
    assert!(program.contains("self.retrieve = dspy.Retrieve(k=2)"));
    assert!(program.contains("self.generate_answer = dspy.ChainOfThought(GenerateAnswer)"));
    assert!(program.contains("for _hop in range(self.max_hops):"));
    assert!(program.contains("context = list(set(context) | set(passages))"));
    assert!(program.contains("multi_hop = MultiHop()"));
}

#[test]
fn knn_parallel_ensemble_family_renders_all_four_previously_excluded_kinds() {
    let ttl = include_str!("../../../examples/knn-parallel-ensemble-family/ontology.ttl");
    let (program, optimize) = sync_and_read_both(ttl);
    assert!(
        program.contains(
            r#"knn_lookup = dspy.KNN(k=3, trainset=knn_trainset, vectorizer=dspy.Embedder("text-embedding-3-small", batch_size=100, caching=True))"#
        ),
        "KNN module must render real dspy.KNN + dspy.Embedder construction:\n{program}"
    );
    assert!(
        program.contains("par = dspy.Parallel(num_threads=4, )"),
        "Parallel module must render real dspy.Parallel construction:\n{program}"
    );
    assert!(program.contains("par_pairs = ["));
    assert!(program.contains("(predictor1, None)"));
    assert!(program.contains("(predictor2, None)"));
    assert!(
        optimize.contains("ens_opt_optimizer = dspy.Ensemble(reduce_fn=ensemble_reduce, size=2, )"),
        "Ensemble optimizer must render real dspy.Ensemble construction:\n{optimize}"
    );
    assert!(
        optimize.contains("ens_opt = ens_opt_optimizer.compile([predictor1, predictor2])"),
        "Ensemble compile() must take a program list, no metric/trainset:\n{optimize}"
    );
    assert!(
        optimize.contains(
            r#"vectorizer=dspy.Embedder("text-embedding-3-small", batch_size=100, caching=True)"#
        ),
        "KNNFewShot optimizer must render real dspy.Embedder construction:\n{optimize}"
    );
    assert!(optimize.contains("knn_fewshot_opt_optimizer = dspy.KNNFewShot("));
    assert!(
        optimize.contains("knn_fewshot_opt = knn_fewshot_opt_optimizer.compile(predictor1)"),
        "KNNFewShot compile() must take just the target Module:\n{optimize}"
    );
}

#[test]
fn playground_basic_qa_renders_bare_predict() {
    let ttl = include_str!("../../../examples/dspy-basic-qa/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("class AnswerQuestion(dspy.Signature):"));
    assert!(program.contains("qa = dspy.Predict(AnswerQuestion)"));
}

#[test]
fn playground_chain_of_thought_math_renders_float_output() {
    let ttl = include_str!("../../../examples/dspy-chain-of-thought-math/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("answer: float = dspy.OutputField("));
    assert!(program.contains("solve_math = dspy.ChainOfThought(SolveMathProblem)"));
}

#[test]
fn playground_classification_renders_literal_output() {
    let ttl = include_str!("../../../examples/dspy-classification/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains(r#"sentiment: Literal["positive", "negative", "neutral"]"#));
    assert!(program.contains("classify_sentiment = dspy.Predict(ClassifySentiment)"));
}

#[test]
fn playground_summarization_renders_chain_of_thought() {
    let ttl = include_str!("../../../examples/dspy-summarization/ontology.ttl");
    let program = sync_and_read_program(ttl);
    assert!(program.contains("summarize = dspy.ChainOfThought(SummarizeDocument)"));
}

// --- Round 5: SHACL-shape-driven Signature derivation (ported from the real ttl2dspy.py
// lineage found this session across ~/cns/~/bytestar/~/ggen-mcp) --------------------------

const SHACL_CAPABILITY_FIXTURE: &str = r#"
@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .
@prefix dcp:  <http://seanchatmangpt.github.io/packs/domain-capability#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .

dspy:capability-shacl-signature a dspy:Signature ;
    dspy:className "CapabilitySignature" ;
    dcterms:title "CapabilitySignature" ;
    dcterms:description "Explain a capability given its slug and consequence, SHACL-derived." ;
    dspy:derivedFromShaclShape dspy:capability-node-shape .

dspy:capability-node-shape a sh:NodeShape ;
    sh:targetClass dcp:Capability ;
    sh:property dspy:slug-prop , dspy:consequence-prop , dspy:explanation-prop .

dspy:slug-prop a sh:PropertyShape ;
    sh:path dcp:slug ; sh:datatype xsd:string .

dspy:consequence-prop a sh:PropertyShape ;
    sh:path dcp:consequence ; sh:datatype xsd:string .

dspy:explanation-prop a sh:PropertyShape ;
    sh:path <http://seanchatmangpt.github.io/packs/domain-capability#explanation> ;
    sh:datatype xsd:string ;
    dspy:isOutputField true .
"#;

fn sync_and_read_shacl_signatures(fixture_domain_ttl: &str) -> String {
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), fixture_domain_ttl);
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect("dspy-pack SHACL fixture must sync cleanly through the real admission gates");
    read(&project, "src/dspy_shacl_signatures.py")
}

#[test]
fn shacl_derived_signature_renders_real_inputs_and_output_from_real_property_shapes() {
    // Reuses domain-capability-pack's real dcp:Capability class (round 1/2 this session) as
    // the SHACL shape's real target -- this IS "running ttl2dspy's capability so it feeds the
    // dspy back into ggen": the SHACL->Signature transpilation happens inside a real `ggen
    // sync` call, not an external script.
    let program = sync_and_read_shacl_signatures(SHACL_CAPABILITY_FIXTURE);
    assert!(
        program.contains("class CapabilitySignature(dspy.Signature):"),
        "must render the real signature class:\n{program}"
    );
    assert!(
        program.contains("slug: str = dspy.InputField("),
        "must render slug as an input, mapped from xsd:string:\n{program}"
    );
    assert!(
        program.contains("consequence: str = dspy.InputField("),
        "must render consequence as an input:\n{program}"
    );
    assert!(
        program.contains("explanation: str = dspy.OutputField("),
        "must render explanation as the real dspy:isOutputField=true output, not an input:\n{program}"
    );
}

#[test]
fn shacl_signature_gate_refuses_invalid_python_identifier() {
    let ttl = r#"
@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
dspy:sig a dspy:Signature ; dspy:className "BadId" ; dcterms:title "t" ; dcterms:description "d" ;
    dspy:derivedFromShaclShape dspy:shape .
dspy:shape a sh:NodeShape ; sh:property dspy:p1 , dspy:out1 .
dspy:p1 a sh:PropertyShape ; sh:path <http://example.org/9bad-name> ; sh:datatype xsd:string .
dspy:out1 a sh:PropertyShape ; sh:path <http://example.org/ok> ; sh:datatype xsd:string ; dspy:isOutputField true .
"#;
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), ttl);
    let err = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect_err("a SHACL path local name that isn't a valid Python identifier must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains("020_shacl_signature_admission"),
        "refusal must cite the gate by name: {msg}"
    );
}

#[test]
fn shacl_signature_gate_refuses_zero_output_fields() {
    let ttl = r#"
@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
dspy:sig a dspy:Signature ; dspy:className "NoOut" ; dcterms:title "t" ; dcterms:description "d" ;
    dspy:derivedFromShaclShape dspy:shape .
dspy:shape a sh:NodeShape ; sh:property dspy:p1 .
dspy:p1 a sh:PropertyShape ; sh:path <http://example.org/only_input> ; sh:datatype xsd:string .
"#;
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), ttl);
    let err = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect_err("a SHACL shape with zero dspy:isOutputField=true properties must refuse, not fabricate a default output field");
    let msg = format!("{err}");
    assert!(
        msg.contains("020_shacl_signature_admission"),
        "refusal must cite the gate by name: {msg}"
    );
}

#[test]
fn shacl_signature_gate_refuses_unmapped_datatype() {
    let ttl = r#"
@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
dspy:sig a dspy:Signature ; dspy:className "BadType" ; dcterms:title "t" ; dcterms:description "d" ;
    dspy:derivedFromShaclShape dspy:shape .
dspy:shape a sh:NodeShape ; sh:property dspy:p1 , dspy:out1 .
dspy:p1 a sh:PropertyShape ; sh:path <http://example.org/weird> ; sh:datatype xsd:anyURI .
dspy:out1 a sh:PropertyShape ; sh:path <http://example.org/ok> ; sh:datatype xsd:string ; dspy:isOutputField true .
"#;
    let (_dir, project) = scaffold_pack_with_ontology(&pack_dir(), ttl);
    let err = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect_err("an sh:datatype outside the 4 admitted XSD types must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains("020_shacl_signature_admission"),
        "refusal must cite the gate by name: {msg}"
    );
}
