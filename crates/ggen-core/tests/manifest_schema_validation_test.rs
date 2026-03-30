//! Negative tests for ggen.toml manifest parsing and validation.
//!
//! These tests verify that ManifestParser and ManifestValidator correctly
//! reject malformed, missing, or semantically invalid manifests. Each test
//! targets a specific failure mode and asserts on the error message content.

use ggen_core::manifest::{ManifestParser, ManifestValidator};
use std::fs;
use std::path::Path;

// ---------------------------------------------------------------------------
// Parsing tests (ManifestParser::parse / parse_str)
// ---------------------------------------------------------------------------

#[test]
fn parse_empty_string_returns_error() {
    let result = ManifestParser::parse_str("");
    assert!(result.is_err(), "Empty string should fail to parse");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("TOML parse error") || msg.contains("missing field"),
        "Error message should mention TOML parsing or missing field, got: {msg}"
    );
}

#[test]
fn parse_missing_project_section_returns_error() {
    let toml = r#"
[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_err(),
        "Manifest without [project] section should fail to parse"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("TOML parse error") || msg.contains("missing field"),
        "Error should mention missing field, got: {msg}"
    );
}

#[test]
fn parse_missing_project_name_returns_error() {
    let toml = r#"
[project]
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_err(),
        "Manifest with missing project.name should fail to parse"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("name"),
        "Error message should mention 'name', got: {msg}"
    );
}

#[test]
fn parse_missing_project_version_returns_error() {
    let toml = r#"
[project]
name = "test-project"

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_err(),
        "Manifest with missing project.version should fail to parse"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("version"),
        "Error message should mention 'version', got: {msg}"
    );
}

#[test]
fn parse_missing_ontology_section_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[generation]
rules = []
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_err(),
        "Manifest without [ontology] section should fail to parse"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("TOML parse error") || msg.contains("missing field"),
        "Error should mention missing field, got: {msg}"
    );
}

#[test]
fn parse_missing_generation_section_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_err(),
        "Manifest without [generation] section should fail to parse"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("TOML parse error") || msg.contains("missing field"),
        "Error should mention missing field, got: {msg}"
    );
}

#[test]
fn parse_valid_minimal_manifest_succeeds() {
    let toml = r#"
[project]
name = "minimal-project"
version = "0.1.0"

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(
        result.is_ok(),
        "Valid minimal manifest should parse successfully"
    );
    let manifest = result.unwrap();
    assert_eq!(manifest.project.name, "minimal-project");
    assert_eq!(manifest.project.version, "0.1.0");
    assert_eq!(manifest.generation.rules.len(), 0);
}

#[test]
fn parse_invalid_toml_syntax_returns_error() {
    let toml = r#"
[project
name = "test"  <-- missing closing bracket
"#;
    let result = ManifestParser::parse_str(toml);
    assert!(result.is_err(), "Invalid TOML syntax should fail to parse");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("TOML parse error"),
        "Error should mention TOML parse error, got: {msg}"
    );
}

#[test]
fn parse_nonexistent_file_returns_error() {
    let result = ManifestParser::parse(Path::new("/tmp/ggen_test_nonexistent_file_918273.toml"));
    assert!(
        result.is_err(),
        "Parsing a nonexistent file should return an error"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("Failed to read manifest"),
        "Error message should mention failed read, got: {msg}"
    );
}

// ---------------------------------------------------------------------------
// Validation tests (ManifestValidator::validate)
// ---------------------------------------------------------------------------

/// Helper: write a TOML string to a temp file and return (TempDir, file path).
fn write_toml_to_tempdir(content: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempfile::tempdir().expect("Failed to create temp dir");
    let file_path = dir.path().join("ggen.toml");
    fs::write(&file_path, content).expect("Failed to write ggen.toml");
    (dir, file_path)
}

#[test]
fn validate_empty_project_name_returns_error() {
    let toml = r#"
[project]
name = ""
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(result.is_err(), "Empty project.name should fail validation");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("project.name cannot be empty"),
        "Error should mention empty project.name, got: {msg}"
    );
}

#[test]
fn validate_empty_project_version_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = ""

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Empty project.version should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("project.version cannot be empty"),
        "Error should mention empty project.version, got: {msg}"
    );
}

#[test]
fn validate_nonexistent_ontology_source_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "nonexistent_ontology.ttl"

[generation]
rules = []
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Nonexistent ontology source should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("Ontology source not found"),
        "Error should mention missing ontology source, got: {msg}"
    );
}

#[test]
fn validate_nonexistent_ontology_import_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"
imports = ["missing_import.ttl"]

[generation]
rules = []
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    // Create the main ontology file so source validation passes
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Nonexistent ontology import should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("Ontology import not found"),
        "Error should mention missing ontology import, got: {msg}"
    );
}

#[test]
fn validate_generation_rule_with_nonexistent_query_file_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "gen-rule"
query = { file = "queries/missing_query.rq" }
template = { inline = "hello {{ name }}" }
output_file = "src/generated/test.rs"
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Nonexistent query file should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("Query file not found"),
        "Error should mention missing query file, got: {msg}"
    );
    assert!(
        msg.contains("gen-rule"),
        "Error should mention the rule name, got: {msg}"
    );
}

#[test]
fn validate_generation_rule_with_nonexistent_template_file_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "tpl-rule"
query = { inline = "SELECT ?s WHERE { ?s ?p ?o }" }
template = { file = "templates/missing_template.tera" }
output_file = "src/generated/test.rs"
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Nonexistent template file should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("Template file not found"),
        "Error should mention missing template file, got: {msg}"
    );
    assert!(
        msg.contains("tpl-rule"),
        "Error should mention the rule name, got: {msg}"
    );
}

#[test]
fn validate_generation_rule_with_empty_output_file_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "no-output"
query = { inline = "SELECT ?s WHERE { ?s ?p ?o }" }
template = { inline = "hello" }
output_file = ""
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(result.is_err(), "Empty output_file should fail validation");
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("output_file cannot be empty"),
        "Error should mention empty output_file, got: {msg}"
    );
}

#[test]
fn validate_nonexistent_shacl_file_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[generation]
rules = []

[validation]
shacl = ["shapes/missing_shape.ttl"]
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Nonexistent SHACL file should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("SHACL shape file not found"),
        "Error should mention missing SHACL file, got: {msg}"
    );
}

#[test]
fn validate_inference_rule_with_empty_name_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[generation]
rules = []

[[inference.rules]]
name = ""
construct = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Empty inference rule name should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("inference.rules[].name cannot be empty"),
        "Error should mention empty rule name, got: {msg}"
    );
}

#[test]
fn validate_inference_rule_with_empty_construct_returns_error() {
    let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[generation]
rules = []

[[inference.rules]]
name = "empty-construct"
construct = ""
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    fs::write(dir.path().join("ontology.ttl"), "").expect("Failed to write ontology.ttl");
    let manifest = ManifestParser::parse(&path).unwrap();
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();
    assert!(
        result.is_err(),
        "Empty construct query should fail validation"
    );
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("construct cannot be empty"),
        "Error should mention empty construct, got: {msg}"
    );
}

#[test]
fn validate_valid_manifest_with_all_sections_succeeds() {
    let toml = r#"
[project]
name = "full-project"
version = "2.0.0"
description = "A complete manifest"

[ontology]
source = "ontology.ttl"
imports = ["extra.ttl"]
base_iri = "http://example.org/"

[ontology.prefixes]
ex = "http://example.org#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

[[inference.rules]]
name = "infer-labels"
description = "Infer rdfs:label from skos:prefLabel"
construct = "CONSTRUCT { ?s rdfs:label ?label } WHERE { ?s skos:prefLabel ?label } ORDER BY ?s"
order = 1

[[generation.rules]]
name = "gen-structs"
query = { inline = "SELECT ?name WHERE { ?name a ex:Entity }" }
template = { inline = "struct {{ name }} {}" }
output_file = "src/generated/{{ name }}.rs"
skip_empty = true
mode = "Overwrite"

[generation]
max_sparql_timeout_ms = 10000
require_audit_trail = true
output_dir = "src/generated"

[validation]
validate_syntax = true
no_unsafe = true
"#;
    let (dir, path) = write_toml_to_tempdir(toml);
    // Create the referenced ontology files
    fs::write(dir.path().join("ontology.ttl"), "").expect("write ontology.ttl");
    fs::write(dir.path().join("extra.ttl"), "").expect("write extra.ttl");

    let manifest = ManifestParser::parse(&path).expect("Should parse valid manifest");
    let validator = ManifestValidator::new(&manifest, dir.path());
    let result = validator.validate();

    assert!(
        result.is_ok(),
        "Fully valid manifest with all sections should pass validation, got error: {}",
        result.unwrap_err()
    );

    // Spot-check parsed values
    assert_eq!(manifest.project.name, "full-project");
    assert_eq!(manifest.project.version, "2.0.0");
    assert_eq!(manifest.ontology.imports.len(), 1);
    assert_eq!(manifest.inference.rules.len(), 1);
    assert_eq!(manifest.inference.rules[0].name, "infer-labels");
    assert_eq!(manifest.generation.rules.len(), 1);
    assert_eq!(manifest.generation.rules[0].name, "gen-structs");
    assert!(manifest.generation.require_audit_trail);
    assert!(manifest.validation.validate_syntax);
    assert!(manifest.validation.no_unsafe);
}
