#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    dead_code,
    unused_imports,
    clippy::all
)]

//! Regression witness: custom `[[validation.rules]]` SPARQL ASK rules are EXECUTED
//! and an Error-severity violation ABORTS generation before any file is written.
//!
//! Why this test exists (the membership≠identity trap, at the engine level):
//! `ValidationConfig.rules` is parsed into the manifest schema, and a fully-built
//! `RuleExecutor` (validation/sparql_rules.rs) exists — but for a long time NOTHING
//! in the pipeline called it, so a colliding manifest rendered anyway. The field was
//! *present* (member of "well-formed config") without being *enforced* (identity with
//! a real check). That defect compiles, parses, and reports green. The only witness
//! that catches a regression back to that state is a behavioral test that drives the
//! pipeline and asserts the rule actually blocks.
//!
//! The wiring lives at `codegen/pipeline.rs::execute_validation_rules` (called from
//! `run()` between inference and generation). This test pins it so a future pipeline
//! refactor cannot silently un-wire it without turning this suite red.
//!
//! Engine ASK convention (sparql_rules.rs): true = VALID, false = VIOLATION. So a
//! collision rule is authored as `ASK { FILTER NOT EXISTS { <collision pattern> } }`
//! (true when no collision). The query must use INLINE full IRIs — a leading PREFIX
//! trips the executor's ASK/SELECT-prefix check.

use ggen_core::codegen::pipeline::GenerationPipeline;
use ggen_core::manifest::types::{ValidationRule, ValidationSeverity};
use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use std::collections::BTreeMap;
use std::path::PathBuf;
use tempfile::TempDir;

const CNV: &str = "http://clap-noun-verb.io/ontology#";

/// Ontology with one verb carrying two args. `collide=true` gives two flags that
/// sanitize (kebab->snake) to the same field (`dry-run` and `dry_run`); `collide=false`
/// gives distinct fields (`dry-run` and `output`).
fn ontology(collide: bool) -> String {
    let second_flag = if collide { "dry_run" } else { "output" };
    format!(
        r#"@prefix cnv: <{CNV}> .
@prefix ex: <http://ex#> .

ex:v a cnv:Verb ;
    cnv:hasVerbName "go" ;
    cnv:hasArguments ex:a1, ex:a2 .
ex:a1 cnv:hasArgumentName "dry-run" .
ex:a2 cnv:hasArgumentName "{second_flag}" .
"#
    )
}

/// The field-collision validation rule (Error severity). Inline IRIs, NOT-EXISTS
/// polarity (true = no collision = valid).
fn collision_rule() -> ValidationRule {
    ValidationRule {
        name: "no-flag-field-collision".to_string(),
        description: "Two CLI flags on one verb must not sanitize to the same Rust field."
            .to_string(),
        ask: format!(
            r#"ASK {{ FILTER NOT EXISTS {{ ?a1 <{CNV}hasArgumentName> ?f1 . ?a2 <{CNV}hasArgumentName> ?f2 . FILTER(?a1 != ?a2) FILTER(REPLACE(?f1, "-", "_") = REPLACE(?f2, "-", "_")) }} }}"#
        ),
        severity: ValidationSeverity::Error,
    }
}

/// Build a manifest whose validation rules and ontology filename are parameterized.
fn manifest(rules: Vec<ValidationRule>) -> GgenManifest {
    GgenManifest {
        project: ProjectConfig {
            name: "val-ask-test".to_string(),
            version: "0.0.0".to_string(),
            description: Some("Witness that validation ASK rules are enforced.".to_string()),
            authors: None,
            license: None,
            repository: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: Vec::new(),
            base_iri: None,
            prefixes: BTreeMap::new(),
            standard_only: None,
        },
        inference: InferenceConfig::default(),
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "verbs".to_string(),
                query: QuerySource::Inline {
                    inline: format!(
                        "SELECT ?n WHERE {{ ?v a <{CNV}Verb> ; <{CNV}hasVerbName> ?n }} ORDER BY ?n"
                    ),
                },
                template: TemplateSource::Inline {
                    inline: "// verb {{ n }}\n".to_string(),
                },
                output_file: "v_{{ n }}.rs".to_string(),
                skip_empty: false,
                mode: GenerationMode::Overwrite,
                when: None,
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("out"),
            enable_llm: false,
            llm_provider: None,
            llm_model: None,
        },
        validation: ValidationConfig {
            rules,
            ..ValidationConfig::default()
        },
        packs: vec![],
        sync: None,
        rdf: None,
        templates: None,
        output: None,
        ai: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: None,
        logging: None,
        telemetry: None,
        features: None,
        env: None,
    }
}

/// Surgical: a colliding ontology + Error collision rule => execute_validation_rules
/// returns Err naming GGEN-VALIDATION.
#[test]
fn validation_ask_blocks_on_collision() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), ontology(true)).expect("write ontology");

    let mut pipeline =
        GenerationPipeline::new(manifest(vec![collision_rule()]), dir.path().to_path_buf());
    pipeline.load_ontology().expect("load ontology");

    let result = pipeline.execute_validation_rules();
    assert!(
        result.is_err(),
        "colliding ontology must fail the Error-severity collision rule"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("GGEN-VALIDATION"),
        "error must name GGEN-VALIDATION, got: {msg}"
    );
}

/// Teeth control: the SAME rule must PASS on a non-colliding ontology — proving the
/// failure above is the rule firing on the collision, not a rule that always fails.
#[test]
fn validation_ask_passes_when_no_collision() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), ontology(false)).expect("write ontology");

    let mut pipeline =
        GenerationPipeline::new(manifest(vec![collision_rule()]), dir.path().to_path_buf());
    pipeline.load_ontology().expect("load ontology");

    pipeline
        .execute_validation_rules()
        .expect("non-colliding ontology must pass the collision rule");
}

// ---------------------------------------------------------------------------
// Capability x witness: a SECOND authoritative invariant, "no orphan verb".
// Each new enforced invariant is a new type-identical seam (a `cnv:Verb` that is
// well-formed but unreachable from any noun) — admitted only with its own
// blocks/passes witness. Now that the engine executes ASKs, the maximalist move is
// to cross-multiply more ontology-shape invariants; each one lands with its teeth.
// ---------------------------------------------------------------------------

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// Ontology with a noun and two verbs. `orphan=true` leaves `v2` a `cnv:Verb` that
/// no noun's `hasVerbs` references; `orphan=false` links both.
fn orphan_ontology(orphan: bool) -> String {
    let verbs = if orphan { "ex:v1" } else { "ex:v1, ex:v2" };
    format!(
        r#"@prefix cnv: <{CNV}> .
@prefix ex: <http://ex#> .

ex:n a cnv:Noun ; cnv:hasNounName "n" ; cnv:hasVerbs {verbs} .
ex:v1 a cnv:Verb ; cnv:hasVerbName "v1" .
ex:v2 a cnv:Verb ; cnv:hasVerbName "v2" .
"#
    )
}

/// "No orphan verb": every `cnv:Verb` must be the object of some `cnv:hasVerbs`.
/// NOT-EXISTS polarity (true = no orphan = valid). Inline IRIs.
fn no_orphan_verb_rule() -> ValidationRule {
    ValidationRule {
        name: "no-orphan-verb".to_string(),
        description: "Every cnv:Verb must be reachable from a noun via cnv:hasVerbs.".to_string(),
        ask: format!(
            r#"ASK {{ FILTER NOT EXISTS {{ ?v <{RDF_TYPE}> <{CNV}Verb> . FILTER NOT EXISTS {{ ?n <{CNV}hasVerbs> ?v }} }} }}"#
        ),
        severity: ValidationSeverity::Error,
    }
}

/// An orphan verb fails the invariant.
#[test]
fn orphan_verb_blocks() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), orphan_ontology(true)).expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![no_orphan_verb_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    let result = pipeline.execute_validation_rules();
    assert!(
        result.is_err(),
        "an orphan cnv:Verb must fail the no-orphan-verb rule"
    );
    assert!(
        result.unwrap_err().to_string().contains("GGEN-VALIDATION"),
        "error must name GGEN-VALIDATION"
    );
}

/// Teeth control: the SAME rule passes when every verb is linked.
#[test]
fn no_orphan_verb_passes_when_all_linked() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), orphan_ontology(false))
        .expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![no_orphan_verb_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    pipeline
        .execute_validation_rules()
        .expect("a fully-linked ontology must pass the no-orphan-verb rule");
}

// ---------------------------------------------------------------------------
// Third authoritative invariant: "no orphan argument". A `cnv:Argument` that no
// verb's `hasArguments` references is well-formed but unreachable — a dangling
// declaration that would never surface in a rendered CLI. Same maximalist move:
// a new ontology-shape invariant, admitted with its own blocks/passes witness.
// ---------------------------------------------------------------------------

/// Ontology: a verb with one referenced arg. `orphan=true` adds a second arg that
/// no `hasArguments` references; `orphan=false` references both.
fn orphan_arg_ontology(orphan: bool) -> String {
    let args = if orphan { "ex:a1" } else { "ex:a1, ex:a2" };
    format!(
        r#"@prefix cnv: <{CNV}> .
@prefix ex: <http://ex#> .

ex:v a cnv:Verb ; cnv:hasVerbName "go" ; cnv:hasArguments {args} .
ex:a1 cnv:hasArgumentName "first" .
ex:a2 cnv:hasArgumentName "second" .
"#
    )
}

/// "No orphan argument": every node with `cnv:hasArgumentName` must be the object of
/// some `cnv:hasArguments`. NOT-EXISTS polarity (true = no orphan = valid).
fn no_orphan_arg_rule() -> ValidationRule {
    ValidationRule {
        name: "no-orphan-argument".to_string(),
        description: "Every cnv:Argument must be referenced by a verb via cnv:hasArguments."
            .to_string(),
        ask: format!(
            r#"ASK {{ FILTER NOT EXISTS {{ ?a <{CNV}hasArgumentName> ?n . FILTER NOT EXISTS {{ ?v <{CNV}hasArguments> ?a }} }} }}"#
        ),
        severity: ValidationSeverity::Error,
    }
}

/// An unreferenced argument fails the invariant.
#[test]
fn orphan_argument_blocks() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), orphan_arg_ontology(true))
        .expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![no_orphan_arg_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    let result = pipeline.execute_validation_rules();
    assert!(
        result.is_err(),
        "an unreferenced cnv:Argument must fail the rule"
    );
    assert!(result.unwrap_err().to_string().contains("GGEN-VALIDATION"));
}

/// Teeth control: passes when every argument is referenced.
#[test]
fn no_orphan_argument_passes_when_all_referenced() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), orphan_arg_ontology(false))
        .expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![no_orphan_arg_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    pipeline
        .execute_validation_rules()
        .expect("fully-referenced arguments must pass the rule");
}

// ---------------------------------------------------------------------------
// Fourth invariant: "every argument is named". An arg referenced via hasArguments
// but lacking cnv:hasArgumentName renders a nameless parameter (`: String,`) — a
// hard compile error in the generated wrapper. A real render-breaker, admitted
// with its own blocks/passes witness.
// ---------------------------------------------------------------------------

/// `named=false` gives the verb a second referenced arg with no hasArgumentName.
fn unnamed_arg_ontology(named: bool) -> String {
    let a2 = if named {
        r#"ex:a2 cnv:hasArgumentName "second" ."#
    } else {
        "ex:a2 a cnv:Argument ." // referenced but unnamed
    };
    format!(
        r#"@prefix cnv: <{CNV}> .
@prefix ex: <http://ex#> .

ex:v a cnv:Verb ; cnv:hasVerbName "go" ; cnv:hasArguments ex:a1, ex:a2 .
ex:a1 cnv:hasArgumentName "first" .
{a2}
"#
    )
}

/// "Every argument is named": every node referenced via hasArguments must carry a
/// hasArgumentName. NOT-EXISTS polarity (true = all named = valid).
fn every_arg_named_rule() -> ValidationRule {
    ValidationRule {
        name: "every-argument-named".to_string(),
        description: "Every argument referenced by a verb must declare a cnv:hasArgumentName."
            .to_string(),
        ask: format!(
            r#"ASK {{ FILTER NOT EXISTS {{ ?v <{CNV}hasArguments> ?a . FILTER NOT EXISTS {{ ?a <{CNV}hasArgumentName> ?n }} }} }}"#
        ),
        severity: ValidationSeverity::Error,
    }
}

/// An unnamed referenced argument fails the invariant.
#[test]
fn unnamed_argument_blocks() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), unnamed_arg_ontology(false))
        .expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![every_arg_named_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    let result = pipeline.execute_validation_rules();
    assert!(
        result.is_err(),
        "an unnamed argument must fail the every-argument-named rule"
    );
    assert!(result.unwrap_err().to_string().contains("GGEN-VALIDATION"));
}

/// Teeth control: passes when every argument is named.
#[test]
fn every_argument_named_passes_when_all_named() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), unnamed_arg_ontology(true))
        .expect("write ontology");

    let mut pipeline = GenerationPipeline::new(
        manifest(vec![every_arg_named_rule()]),
        dir.path().to_path_buf(),
    );
    pipeline.load_ontology().expect("load ontology");

    pipeline
        .execute_validation_rules()
        .expect("fully-named arguments must pass the rule");
}

/// Regression guard: `run()` must invoke validation BEFORE generation, so a colliding
/// ontology aborts with no files written. This is the test that turns red if a future
/// refactor un-wires `execute_validation_rules` from the pipeline (the original bug).
#[test]
fn run_enforces_validation_before_generation() {
    let dir = TempDir::new().expect("TempDir");
    std::fs::write(dir.path().join("ontology.ttl"), ontology(true)).expect("write ontology");

    let mut pipeline =
        GenerationPipeline::new(manifest(vec![collision_rule()]), dir.path().to_path_buf());

    let result = pipeline.run();
    assert!(
        result.is_err(),
        "run() must abort on an Error-severity validation violation"
    );

    // No generated files: generation must not have run.
    let out_dir = dir.path().join("out");
    if out_dir.exists() {
        let rendered: Vec<_> = std::fs::read_dir(&out_dir)
            .expect("read out dir")
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map(|x| x == "rs").unwrap_or(false))
            .collect();
        assert!(
            rendered.is_empty(),
            "no .rs files may be written when validation aborts; found {} ",
            rendered.len()
        );
    }
}
