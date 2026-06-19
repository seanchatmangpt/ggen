#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    dead_code,
    unused_imports
)]

//! Chicago TDD integration test for BUG-002: frontmatter wiring in ggen sync pipeline.
//!
//! Proves that:
//! 1. The raw `---\n...\n---` YAML frontmatter is NOT emitted into the generated file.
//! 2. The `to:` frontmatter field overrides the ggen.toml `output_file` path.
//! 3. A `to:` field containing a Tera variable (e.g. `{{ name }}`) resolves per-row.
//! 4. A template WITHOUT frontmatter still uses the manifest `output_file` (regression).
//!
//! No mocks. Real TempDir, real ontology, real SPARQL BIND query, real pipeline run.

use ggen_core::codegen::pipeline::GenerationPipeline;
use ggen_core::manifest::{
    GenerationMode, GenerationRule, GgenManifest, OntologyConfig, QuerySource, TemplateSource,
};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

fn minimal_manifest(temp_dir: &TempDir) -> GgenManifest {
    GgenManifest {
        project: ggen_core::manifest::ProjectConfig {
            name: "frontmatter-test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            ..Default::default()
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: Vec::new(),
            base_iri: None,
            prefixes: BTreeMap::new(),
            ..Default::default()
        },
        inference: ggen_core::manifest::InferenceConfig {
            rules: Vec::new(),
            max_reasoning_timeout_ms: 60_000,
        },
        generation: ggen_core::manifest::GenerationConfig {
            output_dir: PathBuf::from("output"),
            enable_llm: false,
            rules: Vec::new(),
            max_sparql_timeout_ms: 30_000,
            require_audit_trail: false,
            determinism_salt: None,
            llm_provider: None,
            llm_model: None,
        },
        validation: ggen_core::manifest::ValidationConfig::default(),
        packs: vec![],
        ..Default::default()
    }
}

/// A single-triple ontology — just enough for Oxigraph to load without error.
fn minimal_ontology() -> &'static str {
    r#"
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex:   <http://example.org/test#> .

ex:Widget a rdfs:Class ;
    rdfs:label "Widget" .
"#
}

// ---------------------------------------------------------------------------
// Test 1 — frontmatter `to:` with Tera variable resolves per-row
// ---------------------------------------------------------------------------

/// BUG-002 primary proof:
/// Template has frontmatter `to: {{ name }}.rs`.
/// SPARQL BIND returns `?name = "widget"`.
/// Pipeline must:
///   - write `output/widget.rs` (frontmatter-resolved path)
///   - NOT write `output/declared_output.rs` (manifest output_file)
///   - NOT include any `---` YAML block in the emitted file
///   - emit `fn widget() {}` (body rendered with row context)
#[test]
fn test_frontmatter_to_with_tera_variable_resolves_per_row() {
    let temp_dir = TempDir::new().expect("TempDir");
    let mut manifest = minimal_manifest(&temp_dir);

    // Write ontology
    fs::write(temp_dir.path().join("ontology.ttl"), minimal_ontology()).expect("write ontology");

    // Template: frontmatter declares `to:` with a Tera variable.
    // The value must be quoted in YAML so the `{{` braces don't confuse the
    // YAML parser before Tera renders them.
    let template_with_frontmatter = "---\nto: \"{{ name }}.rs\"\n---\nfn {{ name }}() {}\n";

    // SPARQL returns one row: ?name = "widget"
    let rule = GenerationRule {
        name: "widget-rule".to_string(),
        query: QuerySource::Inline {
            inline: r#"SELECT ?name WHERE { BIND("widget" AS ?name) }"#.to_string(),
        },
        template: TemplateSource::Inline {
            inline: template_with_frontmatter.to_string(),
        },
        output_file: "declared_output.rs".to_string(), // should be overridden by frontmatter
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();
    assert!(result.is_ok(), "pipeline.run() failed: {:?}", result.err());

    // ── ASSERT 1: frontmatter-resolved file exists ──────────────────────────
    let resolved_path = temp_dir.path().join("output").join("widget.rs");
    assert!(
        resolved_path.exists(),
        "Expected output/widget.rs to exist (frontmatter `to:` override), but it does not.\n\
         Files in output/: {:?}",
        fs::read_dir(temp_dir.path().join("output")).ok().map(|d| d
            .filter_map(|e| e.ok())
            .map(|e| e.file_name())
            .collect::<Vec<_>>())
    );

    // ── ASSERT 2: manifest-declared output_file does NOT exist ───────────────
    let declared_path = temp_dir.path().join("output").join("declared_output.rs");
    assert!(
        !declared_path.exists(),
        "declared_output.rs should NOT exist — frontmatter `to:` must override manifest output_file"
    );

    // ── ASSERT 3: generated file contains the rendered body ──────────────────
    let content = fs::read_to_string(&resolved_path).expect("read widget.rs");
    assert!(
        content.contains("fn widget()"),
        "Generated file must contain `fn widget()`, got:\n{content}"
    );

    // ── ASSERT 4: raw YAML frontmatter block is NOT in the output ────────────
    assert!(
        !content.contains("---"),
        "Generated file must NOT contain raw frontmatter `---`, got:\n{content}"
    );
    assert!(
        !content.contains("to:"),
        "Generated file must NOT contain the raw `to:` YAML key, got:\n{content}"
    );
}

// ---------------------------------------------------------------------------
// Test 2 — regression: no frontmatter → manifest output_file is used
// ---------------------------------------------------------------------------

/// Regression guard: a template that has NO frontmatter must still write to
/// the manifest-declared `output_file`, not some derived path.
#[test]
fn test_no_frontmatter_uses_manifest_output_file() {
    let temp_dir = TempDir::new().expect("TempDir");
    let mut manifest = minimal_manifest(&temp_dir);

    fs::write(temp_dir.path().join("ontology.ttl"), minimal_ontology()).expect("write ontology");

    // Plain template, no frontmatter
    let plain_template = "fn plain() {}\n";

    let rule = GenerationRule {
        name: "plain-rule".to_string(),
        query: QuerySource::Inline {
            inline: r#"SELECT ?name WHERE { BIND("plain" AS ?name) }"#.to_string(),
        },
        template: TemplateSource::Inline {
            inline: plain_template.to_string(),
        },
        output_file: "plain_output.rs".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();
    assert!(result.is_ok(), "pipeline.run() failed: {:?}", result.err());

    // Manifest-declared path must exist
    let expected = temp_dir.path().join("output").join("plain_output.rs");
    assert!(
        expected.exists(),
        "output/plain_output.rs should exist for a template with no frontmatter"
    );

    let content = fs::read_to_string(&expected).expect("read plain_output.rs");
    assert!(
        content.contains("fn plain()"),
        "Content should contain `fn plain()`, got:\n{content}"
    );

    // No raw frontmatter delimiters
    assert!(
        !content.contains("---"),
        "Plain template output must not contain `---`, got:\n{content}"
    );
}

// ---------------------------------------------------------------------------
// Test 3 — static frontmatter `to:` (no Tera var) overrides output_file
// ---------------------------------------------------------------------------

/// Proves that even a static (non-variable) `to:` in frontmatter overrides
/// the manifest `output_file`. Uses the static branch of the pipeline.
#[test]
fn test_static_frontmatter_to_overrides_output_file() {
    let temp_dir = TempDir::new().expect("TempDir");
    let mut manifest = minimal_manifest(&temp_dir);

    fs::write(temp_dir.path().join("ontology.ttl"), minimal_ontology()).expect("write ontology");

    // Frontmatter `to:` is a literal path, no Tera variable
    let template_static_front = r#"---
to: static_override.rs
---
fn static_fn() {}
"#;

    let rule = GenerationRule {
        name: "static-front-rule".to_string(),
        query: QuerySource::Inline {
            // Empty result set → exercises static (non-per-row) code path
            inline: r#"SELECT ?dummy WHERE { BIND("x" AS ?dummy) }"#.to_string(),
        },
        template: TemplateSource::Inline {
            inline: template_static_front.to_string(),
        },
        output_file: "should_not_appear.rs".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();
    assert!(result.is_ok(), "pipeline.run() failed: {:?}", result.err());

    // The overridden file must exist
    let overridden = temp_dir.path().join("output").join("static_override.rs");
    // The manifest-declared file must NOT exist
    let declared = temp_dir.path().join("output").join("should_not_appear.rs");

    // Note: the static path is used when there is exactly one row. If the
    // pipeline uses the per-row branch for any non-empty result set, the
    // per-row `to:` resolution also applies. Either way, `static_override.rs`
    // must appear and `should_not_appear.rs` must not.
    assert!(
        overridden.exists(),
        "output/static_override.rs should exist; frontmatter `to:` must override manifest.\n\
         Files in output/: {:?}",
        fs::read_dir(temp_dir.path().join("output")).ok().map(|d| d
            .filter_map(|e| e.ok())
            .map(|e| e.file_name())
            .collect::<Vec<_>>())
    );

    assert!(
        !declared.exists(),
        "output/should_not_appear.rs must NOT exist — frontmatter `to:` must have overridden it"
    );

    let content = fs::read_to_string(&overridden).expect("read static_override.rs");
    assert!(
        !content.contains("---"),
        "No raw YAML block in output: {content}"
    );
    assert!(
        content.contains("fn static_fn()"),
        "Body rendered: {content}"
    );
}
