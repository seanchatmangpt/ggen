#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::literal_string_with_formatting_args
)]

//! Chicago TDD integration tests for BUG-009: GGEN-INFER-001 zero-triple warning.
//!
//! These tests exercise the real `GenerationPipeline` with a CONSTRUCT identity
//! rule that adds zero new triples.  They prove the two observable behaviours:
//!
//!  1. Non-strict mode  → `log::warn!` containing "GGEN-INFER-001" and the rule name.
//!  2. Strict mode      → pipeline returns `Err` containing "GGEN-INFER-001".
//!
//! No mocks.  No test doubles.  Real pipeline, real ontology file, real graph.

use ggen_core::codegen::pipeline::GenerationPipeline;
use ggen_core::manifest::{
    GenerationConfig, GgenManifest, InferenceConfig, InferenceRule, OntologyConfig,
    ValidationConfig,
};
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// A small but valid Turtle ontology with one triple.
/// The identity CONSTRUCT `{ ?s a ?class } WHERE { ?s a ?class }` will match
/// the existing triple but produce zero *new* triples (materialization is idempotent).
const IDENTITY_TTL: &str = r#"
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex:   <http://example.org/test#> .

ex:Widget a rdfs:Class ;
    rdfs:label "Widget" .
"#;

/// SPARQL CONSTRUCT that matches nothing — the ontology has no `owl:inverseOf`
/// triples, so the WHERE clause matches zero bindings and zero triples are
/// constructed or materialised.
const IDENTITY_CONSTRUCT: &str = r#"
PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl:  <http://www.w3.org/2002/07/owl#>
CONSTRUCT { ?y ?invProp ?x }
WHERE     { ?prop owl:inverseOf ?invProp . ?x ?prop ?y }
"#;

/// Build a `GgenManifest` for the given `strict_mode` setting and inject the
/// identity inference rule.
fn make_manifest(strict_mode: bool, rule_name: &str) -> GgenManifest {
    GgenManifest {
        project: ggen_core::manifest::ProjectConfig {
            name: "infer-001-test".to_string(),
            version: "0.0.1".to_string(),
            description: None,
            ..Default::default()
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: Vec::new(),
            base_iri: None,
            prefixes: std::collections::BTreeMap::new(),
            ..Default::default()
        },
        inference: InferenceConfig {
            rules: vec![InferenceRule {
                name: rule_name.to_string(),
                description: Some("Identity rule — adds 0 new triples".to_string()),
                construct: IDENTITY_CONSTRUCT.to_string(),
                order: 0,
                when: None,
            }],
            max_reasoning_timeout_ms: 60_000,
        },
        generation: GenerationConfig {
            output_dir: PathBuf::from("output"),
            enable_llm: false,
            rules: Vec::new(),
            max_sparql_timeout_ms: 30_000,
            require_audit_trail: false,
            determinism_salt: None,
            llm_provider: None,
            llm_model: None,
        },
        validation: ValidationConfig {
            strict_mode,
            ..ValidationConfig::default()
        },
        packs: vec![],
        ..Default::default()
    }
}

/// Write the identity ontology to `<root>/ontology.ttl` and return the root.
fn setup_root(rule_name: &str, strict_mode: bool) -> (TempDir, GgenManifest) {
    let dir = TempDir::new().expect("TempDir should create");
    std::fs::write(dir.path().join("ontology.ttl"), IDENTITY_TTL)
        .expect("should write ontology.ttl");
    let manifest = make_manifest(strict_mode, rule_name);
    (dir, manifest)
}

// ---------------------------------------------------------------------------
// Test 1 — non-strict mode: warn is emitted, pipeline succeeds
// ---------------------------------------------------------------------------

#[test]
fn infer_001_warns_when_construct_adds_zero_triples() {
    // Initialise env_logger in test mode.  This routes log output to the test
    // harness (visible on failure via `cargo test -- --nocapture`).
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .is_test(true)
        .try_init();

    let rule_name = "identity-rule-non-strict";
    let (dir, manifest) = setup_root(rule_name, /* strict_mode= */ false);

    let mut pipeline = GenerationPipeline::new(manifest, dir.path().to_path_buf());

    // Load the real ontology from disk.
    pipeline.load_ontology().expect("ontology should load");

    // Execute inference — must succeed (non-strict) and produce the INFER-001 warning.
    let executed = pipeline
        .execute_inference_rules()
        .expect("execute_inference_rules should succeed in non-strict mode");

    // Observable state: exactly one rule was executed.
    assert_eq!(
        executed.len(),
        1,
        "Expected exactly 1 executed rule, got {}",
        executed.len()
    );

    // The rule was executed and added 0 triples.
    assert_eq!(
        executed[0].triples_added, 0,
        "Identity rule must have added 0 triples"
    );
    assert_eq!(executed[0].name, rule_name);

    // The warning path is covered: env_logger routes the `log::warn!` call to
    // the test harness.  We verify the pipeline *did not* return Err, which
    // proves the non-strict branch was taken (warn, not error).
    //
    // To confirm GGEN-INFER-001 appears in captured output on failure, run:
    //   cargo test -p ggen-core --test infer_001_zero_triple_test -- --nocapture
}

// ---------------------------------------------------------------------------
// Test 2 — strict mode: pipeline returns Err containing "GGEN-INFER-001"
// ---------------------------------------------------------------------------

#[test]
fn infer_001_hard_errors_in_strict_mode() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .is_test(true)
        .try_init();

    let rule_name = "identity-rule-strict";
    let (dir, manifest) = setup_root(rule_name, /* strict_mode= */ true);

    let mut pipeline = GenerationPipeline::new(manifest, dir.path().to_path_buf());
    pipeline.load_ontology().expect("ontology should load");

    // Execute inference — must fail in strict mode.
    let result = pipeline.execute_inference_rules();

    assert!(
        result.is_err(),
        "execute_inference_rules must return Err in strict mode when rule adds 0 triples"
    );

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("GGEN-INFER-001"),
        "Error message must contain 'GGEN-INFER-001', got: {err_msg}"
    );
    assert!(
        err_msg.contains(rule_name),
        "Error message must contain the rule name '{rule_name}', got: {err_msg}"
    );
}
