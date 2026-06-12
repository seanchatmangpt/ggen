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

//! Chicago TDD integration tests for BUG-001 and BUG-006.
//!
//! BUG-001: `parse_and_validate` wires parse + validate together (real filesystem)
//! BUG-006: E0013 fires for SELECT without ORDER BY (strict_mode enforcement)

use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig, OntologyConfig,
    ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use ggen_core::manifest::{ManifestParser, ManifestValidator};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn minimal_ttl() -> &'static str {
    r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/test#> .

ex:Thing a rdfs:Class ;
    rdfs:label "Thing" .
"#
}

/// Build a minimal valid manifest struct pointing at `source` inside `base_path`.
fn minimal_manifest(source: &str, strict_mode: bool) -> GgenManifest {
    GgenManifest {
        project: ProjectConfig {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from(source),
            imports: Vec::new(),
            base_iri: None,
            prefixes: BTreeMap::new(),
            standard_only: None,
        },
        inference: InferenceConfig::default(),
        generation: GenerationConfig {
            rules: Vec::new(),
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("."),
            enable_llm: false,
            llm_provider: None,
            llm_model: None,
        },
        validation: ValidationConfig {
            strict_mode,
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

fn generation_rule_with_inline_query(query: &str) -> GenerationRule {
    GenerationRule {
        name: "test-rule".to_string(),
        query: QuerySource::Inline {
            inline: query.to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{ name }}\n".to_string(),
        },
        output_file: "out.txt".to_string(),
        skip_empty: false,
        mode: GenerationMode::Overwrite,
        when: None,
    }
}

// ---------------------------------------------------------------------------
// BUG-001: parse_and_validate wires parse + validate together
// ---------------------------------------------------------------------------

/// BUG-001: Missing import file must cause parse_and_validate to return Err.
/// Real filesystem: TempDir, real .toml, real import reference that doesn't exist.
#[test]
fn test_parse_and_validate_rejects_missing_import() {
    let dir = TempDir::new().expect("TempDir");

    // Write a tiny valid ontology so the source check passes.
    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    // Write ggen.toml that imports a file that does NOT exist.
    let toml = r#"
[project]
name = "test"
version = "0.1.0"

[ontology]
source = "ontology.ttl"
imports = ["missing.ttl"]

[generation]
rules = []
"#;
    let toml_path = dir.path().join("ggen.toml");
    std::fs::write(&toml_path, toml).expect("write ggen.toml");

    let result = ManifestParser::parse_and_validate(&toml_path);

    assert!(result.is_err(), "parse_and_validate must reject missing import");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("import") || msg.contains("missing") || msg.contains("not found"),
        "error must mention missing import, got: {msg}"
    );
}

/// BUG-001: A fully valid ggen.toml (source exists, no missing imports) must succeed.
#[test]
fn test_parse_and_validate_accepts_valid_manifest() {
    let dir = TempDir::new().expect("TempDir");

    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    let toml = r#"
[project]
name = "test"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
rules = []
"#;
    let toml_path = dir.path().join("ggen.toml");
    std::fs::write(&toml_path, toml).expect("write ggen.toml");

    let result = ManifestParser::parse_and_validate(&toml_path);

    assert!(result.is_ok(), "parse_and_validate must accept valid manifest, got: {:?}", result);
}

// ---------------------------------------------------------------------------
// BUG-006: E0013 — SELECT without ORDER BY
// ---------------------------------------------------------------------------

/// BUG-006: strict_mode = true + inline SELECT without ORDER BY → Err containing "E0013".
#[test]
fn test_e0013_fires_for_select_without_order_by_strict_mode() {
    let dir = TempDir::new().expect("TempDir");
    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    let mut manifest = minimal_manifest("ontology.ttl", true /* strict_mode */);
    manifest.generation.rules = vec![generation_rule_with_inline_query(
        "SELECT ?name WHERE { ?name a <http://www.w3.org/2000/01/rdf-schema#Class> }",
    )];

    let result = ManifestValidator::new(&manifest, dir.path()).validate();

    assert!(result.is_err(), "E0013 must fire in strict_mode without ORDER BY");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("E0013"),
        "error must contain E0013 code, got: {msg}"
    );
}

/// BUG-006: strict_mode = false + inline SELECT without ORDER BY → Ok (only a warning).
#[test]
fn test_e0013_is_warning_in_non_strict_mode() {
    let dir = TempDir::new().expect("TempDir");
    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    let mut manifest = minimal_manifest("ontology.ttl", false /* strict_mode */);
    manifest.generation.rules = vec![generation_rule_with_inline_query(
        "SELECT ?name WHERE { ?name a <http://www.w3.org/2000/01/rdf-schema#Class> }",
    )];

    let result = ManifestValidator::new(&manifest, dir.path()).validate();

    assert!(
        result.is_ok(),
        "non-strict mode must not reject SELECT without ORDER BY, got: {:?}",
        result
    );
}

/// BUG-006: SELECT that includes ORDER BY must pass even with strict_mode = true.
#[test]
fn test_e0013_not_fired_for_select_with_order_by() {
    let dir = TempDir::new().expect("TempDir");
    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    let mut manifest = minimal_manifest("ontology.ttl", true /* strict_mode */);
    manifest.generation.rules = vec![generation_rule_with_inline_query(
        "SELECT ?name WHERE { ?name a <http://www.w3.org/2000/01/rdf-schema#Class> } ORDER BY ?name",
    )];

    let result = ManifestValidator::new(&manifest, dir.path()).validate();

    assert!(
        result.is_ok(),
        "SELECT with ORDER BY must not trigger E0013, got: {:?}",
        result
    );
}

/// BUG-006: QuerySource::Pack skips E0013 check entirely — must pass strict_mode.
#[test]
fn test_e0013_skipped_for_pack_query_source() {
    let dir = TempDir::new().expect("TempDir");
    let ttl_path = dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, minimal_ttl()).expect("write ontology");

    let mut manifest = minimal_manifest("ontology.ttl", true /* strict_mode */);
    // Declare the pack so E0014 (undeclared pack) does not fire before E0013.
    manifest.packs = vec![ggen_core::manifest::types::PackRef {
        name: "some-pack".to_string(),
        registry: "local".to_string(),
        path: None,
        version: None,
    }];
    manifest.generation.rules = vec![GenerationRule {
        name: "pack-rule".to_string(),
        query: QuerySource::Pack {
            pack: "some-pack".to_string(),
            output: "queries".to_string(),
            file: PathBuf::from("select.rq"),
        },
        template: TemplateSource::Inline {
            inline: "{{ name }}\n".to_string(),
        },
        output_file: "out.txt".to_string(),
        skip_empty: false,
        mode: GenerationMode::Overwrite,
        when: None,
    }];

    let result = ManifestValidator::new(&manifest, dir.path()).validate();

    assert!(
        result.is_ok(),
        "Pack query source must skip E0013 check (runtime-only resolution), got: {:?}",
        result
    );
}
