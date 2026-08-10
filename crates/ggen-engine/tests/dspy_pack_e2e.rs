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
