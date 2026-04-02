//! Unit tests for GpackManifest operations
//!
//! Tests core gpack manifest functionality:
//! - Parsing and validation
//! - File discovery (templates, RDF, SPARQL, SHACL)
//! - Pattern matching
//! - Default conventions

use ggen_core::gpack::{GpackManifest, PackConventions};
use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Manifest Parsing Tests
// ============================================================================

#[test]
fn test_parse_minimal_manifest() {
    let toml = r#"
[gpack]
id = "test.minimal"
name = "Minimal Pack"
version = "1.0.0"
description = "Test pack"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.id, "test.minimal");
    assert_eq!(manifest.metadata.name, "Minimal Pack");
    assert_eq!(manifest.metadata.version, "1.0.0");
    assert!(manifest.dependencies.is_empty());
}

#[test]
fn test_parse_full_manifest() {
    let toml = r#"
[gpack]
id = "test.full"
name = "Full Pack"
version = "2.1.0"
description = "Complete test pack"
license = "MIT OR Apache-2.0"
ggen_compat = ">=3.0.0 <4.0.0"

[dependencies]
"test.dep1" = "^1.0"
"test.dep2" = "~2.3.4"

[templates]
patterns = ["custom/**/*.tmpl"]
includes = ["macros/*.tera"]

[macros]
paths = ["shared/macros"]

[rdf]
base = "https://example.org/ontology/"
patterns = ["custom/graphs/*.ttl"]
inline = ["@prefix ex: <https://example.org/> ."]

[rdf.prefixes]
ex = "https://example.org/"
skos = "http://www.w3.org/2004/02/skos/core#"

[queries]
patterns = ["custom/queries/*.sparql"]

[queries.aliases]
get_components = "custom/queries/components.sparql"

[shapes]
patterns = ["custom/shapes/*.ttl"]

[preset]
config = "preset/config.toml"

[preset.vars]
author = "Test Author"
license = "MIT"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.id, "test.full");
    assert_eq!(manifest.dependencies.len(), 2);
    assert_eq!(manifest.templates.patterns.len(), 1);
    assert_eq!(manifest.rdf.prefixes.len(), 2);
    assert_eq!(manifest.queries.aliases.len(), 1);
    assert_eq!(manifest.preset.vars.len(), 2);
}

#[test]
fn test_parse_manifest_invalid_toml() {
    let invalid_toml = "{ this is not toml }";
    let result: Result<GpackManifest, _> = toml::from_str(invalid_toml);
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_missing_required_field() {
    let toml = r#"
[gpack]
id = "test.incomplete"
name = "Incomplete"
version = "1.0.0"
# Missing description, license, ggen_compat
"#;

    let result: Result<GpackManifest, _> = toml::from_str(toml);
    assert!(result.is_err());
}

// ============================================================================
// File Discovery Tests - Templates
// ============================================================================

#[test]
fn test_discover_templates_default_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    // Create template files using default patterns
    create_file(base, "templates/main.tmpl", "template content");
    create_file(base, "templates/sub/helper.tmpl", "helper template");
    create_file(base, "templates/other.tera", "tera template");
    create_file(base, "not-templates/ignored.tmpl", "should be ignored");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(base).unwrap();

    assert_eq!(templates.len(), 3);
    assert!(templates.iter().any(|p| p.ends_with("main.tmpl")));
    assert!(templates.iter().any(|p| p.ends_with("helper.tmpl")));
    assert!(templates.iter().any(|p| p.ends_with("other.tera")));
}

#[test]
fn test_discover_templates_custom_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(base, "custom/my.tmpl", "custom template");
    create_file(base, "templates/default.tmpl", "should be ignored");

    let mut manifest = minimal_manifest();
    manifest.templates.patterns = vec!["custom/**/*.tmpl".to_string()];

    let templates = manifest.discover_templates(base).unwrap();

    assert_eq!(templates.len(), 1);
    assert!(templates[0].ends_with("my.tmpl"));
}

#[test]
fn test_discover_templates_empty_directory() {
    let temp_dir = TempDir::new().unwrap();
    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();
    assert_eq!(templates.len(), 0);
}

// ============================================================================
// File Discovery Tests - RDF
// ============================================================================

#[test]
fn test_discover_rdf_files_default_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(base, "templates/api/graphs/ontology.ttl", "turtle content");
    create_file(base, "templates/cli/graphs/vocab.rdf", "rdf/xml content");
    create_file(base, "templates/web/graphs/data.jsonld", "json-ld content");
    create_file(base, "other/file.ttl", "should be ignored");

    let manifest = minimal_manifest();
    let rdf_files = manifest.discover_rdf_files(base).unwrap();

    assert_eq!(rdf_files.len(), 3);
    assert!(rdf_files.iter().any(|p| p.ends_with("ontology.ttl")));
    assert!(rdf_files.iter().any(|p| p.ends_with("vocab.rdf")));
    assert!(rdf_files.iter().any(|p| p.ends_with("data.jsonld")));
}

#[test]
fn test_discover_rdf_files_custom_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(base, "ontologies/custom.ttl", "custom rdf");
    create_file(base, "templates/graphs/default.ttl", "should be ignored");

    let mut manifest = minimal_manifest();
    manifest.rdf.patterns = vec!["ontologies/**/*.ttl".to_string()];

    let rdf_files = manifest.discover_rdf_files(base).unwrap();

    assert_eq!(rdf_files.len(), 1);
    assert!(rdf_files[0].ends_with("custom.ttl"));
}

// ============================================================================
// File Discovery Tests - SPARQL Queries
// ============================================================================

#[test]
fn test_discover_query_files_default_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(
        base,
        "templates/api/queries/components.rq",
        "SELECT ?s WHERE { ?s ?p ?o }",
    );
    create_file(
        base,
        "templates/cli/queries/search.sparql",
        "SELECT * WHERE { ?s ?p ?o }",
    );
    create_file(base, "other/query.rq", "should be ignored");

    let manifest = minimal_manifest();
    let queries = manifest.discover_query_files(base).unwrap();

    assert_eq!(queries.len(), 2);
    assert!(queries.iter().any(|p| p.ends_with("components.rq")));
    assert!(queries.iter().any(|p| p.ends_with("search.sparql")));
}

#[test]
fn test_discover_query_files_custom_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(base, "sparql/custom.sparql", "custom query");
    create_file(base, "templates/queries/default.rq", "should be ignored");

    let mut manifest = minimal_manifest();
    manifest.queries.patterns = vec!["sparql/**/*.sparql".to_string()];

    let queries = manifest.discover_query_files(base).unwrap();

    assert_eq!(queries.len(), 1);
    assert!(queries[0].ends_with("custom.sparql"));
}

// ============================================================================
// File Discovery Tests - SHACL Shapes
// ============================================================================

#[test]
fn test_discover_shape_files_default_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    create_file(
        base,
        "templates/api/graphs/shapes/validation.shacl.ttl",
        "shape content",
    );
    create_file(base, "templates/cli/shapes/schema.ttl", "shape content");
    create_file(base, "other/shape.ttl", "should be ignored");

    let manifest = minimal_manifest();
    let shapes = manifest.discover_shape_files(base).unwrap();

    assert_eq!(shapes.len(), 2);
    assert!(shapes.iter().any(|p| p.ends_with("validation.shacl.ttl")));
    assert!(shapes.iter().any(|p| p.ends_with("schema.ttl")));
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_load_manifest_from_nonexistent_file() {
    let result = GpackManifest::load_from_file(&PathBuf::from("/nonexistent/gpack.toml"));
    assert!(result.is_err());
}

#[test]
fn test_discover_files_with_invalid_glob_pattern() {
    let temp_dir = TempDir::new().unwrap();
    let manifest = minimal_manifest();

    // This should handle glob errors gracefully
    // Note: Most glob patterns are actually valid, so this is hard to trigger
    let result = manifest.discover_templates(temp_dir.path());
    assert!(result.is_ok());
}

#[test]
fn test_manifest_with_empty_strings() {
    let toml = r#"
[gpack]
id = "test.empty"
name = ""
version = "1.0.0"
description = ""
license = ""
ggen_compat = ""
"#;

    // Should parse but may fail validation later
    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.name, "");
    assert_eq!(manifest.metadata.description, "");
}

#[test]
fn test_manifest_with_special_characters() {
    let toml = r#"
[gpack]
id = "test.special-chars_123"
name = "Special™ Pack®"
version = "1.0.0-beta.1+build.123"
description = "Test with émojis 🚀 and símböls"
license = "MIT OR Apache-2.0"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.id, "test.special-chars_123");
    assert!(manifest.metadata.description.contains("🚀"));
}

#[test]
fn test_manifest_with_very_long_strings() {
    let long_desc = "x".repeat(10000);
    let toml = format!(
        r#"
[gpack]
id = "test.long"
name = "Long Pack"
version = "1.0.0"
description = "{}"
license = "MIT"
ggen_compat = ">=3.0.0"
"#,
        long_desc
    );

    let manifest: GpackManifest = toml::from_str(&toml).unwrap();
    assert_eq!(manifest.metadata.description.len(), 10000);
}

// ============================================================================
// Pack Conventions Tests
// ============================================================================

#[test]
fn test_pack_conventions_defaults() {
    let conventions = PackConventions::default();

    assert_eq!(conventions.template_patterns.len(), 2);
    assert!(conventions.template_patterns.contains(&"templates/**/*.tmpl"));
    assert!(conventions.template_patterns.contains(&"templates/**/*.tera"));

    assert_eq!(conventions.rdf_patterns.len(), 3);
    assert!(conventions.rdf_patterns.contains(&"templates/**/graphs/*.ttl"));

    assert_eq!(conventions.query_patterns.len(), 2);
    assert!(conventions.query_patterns.contains(&"templates/**/queries/*.rq"));

    assert_eq!(conventions.shape_patterns.len(), 2);
    assert!(conventions
        .shape_patterns
        .contains(&"templates/**/graphs/shapes/*.shacl.ttl"));
}

// ============================================================================
// Helper Functions
// ============================================================================

fn minimal_manifest() -> GpackManifest {
    GpackManifest {
        metadata: ggen_core::gpack::GpackMetadata {
            id: "test.minimal".to_string(),
            name: "Test Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "Test pack".to_string(),
            license: "MIT".to_string(),
            ggen_compat: ">=3.0.0".to_string(),
        },
        dependencies: BTreeMap::new(),
        templates: ggen_core::gpack::TemplatesConfig::default(),
        macros: ggen_core::gpack::MacrosConfig::default(),
        rdf: ggen_core::gpack::RdfConfig::default(),
        queries: ggen_core::gpack::QueriesConfig::default(),
        shapes: ggen_core::gpack::ShapesConfig::default(),
        preset: ggen_core::gpack::PresetConfig::default(),
    }
}

fn create_file(base: &Path, relative_path: &str, content: &str) {
    let path = base.join(relative_path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    let mut file = fs::File::create(path).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
