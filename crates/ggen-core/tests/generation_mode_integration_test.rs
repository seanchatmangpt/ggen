#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Chicago TDD integration tests for BUG-004 and BUG-009.
//!
//! BUG-004: GenerationMode::Create must not overwrite existing files.
//! BUG-009: GenerationMode::Overwrite must replace existing file content.
//!
//! Real filesystem (TempDir), real pipeline execution, real file I/O.
//! No mocks. No test doubles.

use ggen_core::codegen::pipeline::GenerationPipeline;
use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use std::collections::BTreeMap;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const MINIMAL_TTL: &str = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/test#> .

ex:Widget a rdfs:Class ;
    rdfs:label "Widget" .
"#;

/// Build a minimal manifest with a single generation rule.
fn build_manifest(output_dir: &str, mode: GenerationMode, output_file: &str) -> GgenManifest {
    GgenManifest {
        project: ProjectConfig {
            name: "mode-test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
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
                name: "test-rule".to_string(),
                query: QuerySource::Inline {
                    inline: "SELECT ?class WHERE { ?class a rdfs:Class } ORDER BY ?class"
                        .to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "GENERATED: {{ class }}\n".to_string(),
                },
                output_file: output_file.to_string(),
                skip_empty: false,
                mode,
                when: None,
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from(output_dir),
            enable_llm: false,
            llm_provider: None,
            llm_model: None,
        },
        validation: ValidationConfig::default(),
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

// ---------------------------------------------------------------------------
// Create mode: skip silently when output file already exists
// ---------------------------------------------------------------------------

/// When output file exists and mode = Create, pipeline must succeed and leave
/// the existing file untouched (bootstrap-scaffold semantics: write-once,
/// hand-owned thereafter).
#[test]
fn test_create_mode_skips_when_existing() {
    let dir = TempDir::new().expect("TempDir");

    std::fs::write(dir.path().join("ontology.ttl"), MINIMAL_TTL).expect("write ontology");

    let out_dir = dir.path().join("out");
    std::fs::create_dir_all(&out_dir).expect("create out dir");
    let output_path = out_dir.join("result.txt");
    let sentinel = "SENTINEL — DO NOT OVERWRITE\n";
    std::fs::write(&output_path, sentinel).expect("write sentinel");

    let manifest = build_manifest("out", GenerationMode::Create, "result.txt");
    let mut pipeline = GenerationPipeline::new(manifest, dir.path().to_path_buf());

    pipeline.load_ontology().expect("load ontology");
    let result = pipeline.execute_generation_rules();

    // Create mode: must succeed (not error) when file already exists.
    assert!(result.is_ok(), "Create mode must succeed when file exists, got: {:?}", result.err());

    // Sentinel must be intact — file was not overwritten.
    let actual = std::fs::read_to_string(&output_path).expect("read output");
    assert_eq!(actual, sentinel, "Create mode must not change content of existing file");
}

// ---------------------------------------------------------------------------
// BUG-009: Overwrite mode must replace existing file content
// ---------------------------------------------------------------------------

/// BUG-009: When output file exists and mode = Overwrite, pipeline must replace
/// the file content with freshly generated content.
#[test]
fn test_overwrite_mode_replaces_existing() {
    let dir = TempDir::new().expect("TempDir");

    std::fs::write(dir.path().join("ontology.ttl"), MINIMAL_TTL).expect("write ontology");

    // Pre-create the output file with stale content.
    let out_dir = dir.path().join("out");
    std::fs::create_dir_all(&out_dir).expect("create out dir");
    let output_path = out_dir.join("result.txt");
    std::fs::write(&output_path, "STALE CONTENT\n").expect("write stale");

    let manifest = build_manifest("out", GenerationMode::Overwrite, "result.txt");
    let mut pipeline = GenerationPipeline::new(manifest, dir.path().to_path_buf());

    pipeline.load_ontology().expect("load ontology");
    let generated = pipeline
        .execute_generation_rules()
        .expect("pipeline must not error in Overwrite mode");

    // Overwrite mode: at least one file must have been generated.
    assert!(
        !generated.is_empty(),
        "Overwrite mode must generate (replace) existing file, got empty list"
    );

    // Real filesystem state: content must be the generated output, not the stale text.
    let actual = std::fs::read_to_string(&output_path).expect("read output");
    assert!(
        !actual.contains("STALE CONTENT"),
        "Overwrite mode must have replaced stale content; file still contains 'STALE CONTENT'"
    );
    assert!(
        actual.contains("GENERATED"),
        "Overwrite mode must produce generated content; got: {actual}"
    );
}
