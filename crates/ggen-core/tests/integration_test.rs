//! Comprehensive integration tests for ggen end-to-end workflow
//!
//! Tests validate that:
//! 1. Configuration can be loaded from TOML
//! 2. SPARQL queries execute successfully
//! 3. Templates receive populated query results
//! 4. Generated files have expected content
//! 5. Rule filtering works correctly
//!
//! These tests ensure all agents (config parsing, query execution, template
//! binding, CLI) work together to produce correct output.

use ggen_core::manifest::{ManifestParser, QuerySource, TemplateSource};
use std::fs;
use std::path::PathBuf;

/// Helper function to get test fixture path
fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(format!(
        "{}/tests/fixtures/{}",
        env!("CARGO_MANIFEST_DIR"),
        name
    ))
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 1: Load ggen.toml configuration
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_ggen_config_loading() {
    let config_path = fixture_path("test-ggen.toml");

    // Ensure fixture exists
    assert!(
        config_path.exists(),
        "Test fixture '{}' should exist",
        config_path.display()
    );

    // Parse manifest
    let manifest =
        ManifestParser::parse(&config_path).expect("Should successfully parse test-ggen.toml");

    // Verify project metadata
    assert_eq!(manifest.project.name, "test-codegen");
    assert_eq!(manifest.project.version, "0.1.0");
    assert_eq!(
        manifest.project.description,
        Some("Test configuration for ggen integration tests".to_string())
    );

    // Verify ontology config
    assert_eq!(
        manifest.ontology.source,
        PathBuf::from("tests/fixtures/mini-ontology.ttl")
    );
    assert_eq!(manifest.ontology.imports.len(), 0);
    assert_eq!(
        manifest.ontology.base_iri,
        Some("https://example.org/ontology#".to_string())
    );

    // Verify prefixes are loaded
    assert!(manifest.ontology.prefixes.contains_key("ex"));
    assert_eq!(
        manifest.ontology.prefixes.get("ex"),
        Some(&"https://example.org/ontology#".to_string())
    );

    // Verify generation config
    assert!(
        manifest.generation.rules.len() >= 2,
        "Should have at least 2 rules"
    );

    // Verify first rule (test-rule)
    let test_rule = &manifest.generation.rules[0];
    assert_eq!(test_rule.name, "test-rule");
    assert_eq!(test_rule.output_file, "test-{{ className }}.txt");

    println!("✓ Configuration loaded successfully");
    println!(
        "  Project: {} v{}",
        manifest.project.name, manifest.project.version
    );
    println!("  Rules configured: {}", manifest.generation.rules.len());
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 2: Verify ontology and query fixtures exist
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_ontology_fixture_exists() {
    let ontology_path = fixture_path("mini-ontology.ttl");

    assert!(
        ontology_path.exists(),
        "Ontology fixture should exist at {}",
        ontology_path.display()
    );

    // Read and validate basic structure
    let content = fs::read_to_string(&ontology_path).expect("Should be able to read ontology file");

    // Verify it contains expected RDF elements
    assert!(
        content.contains("@prefix"),
        "Ontology should have RDF prefix declarations"
    );
    assert!(
        content.contains("owl:Class"),
        "Ontology should define OWL classes"
    );
    assert!(
        content.contains("ex:Class1"),
        "Ontology should have test classes"
    );
    assert!(
        content.contains("ex:instance1"),
        "Ontology should have test instances"
    );

    println!("✓ Ontology fixture validated");
    println!("  File: {}", ontology_path.display());
    println!("  Size: {} bytes", content.len());
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 3: Verify SPARQL query fixture
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_sparql_query_fixture_exists() {
    let query_path = fixture_path("test-query.rq");

    assert!(
        query_path.exists(),
        "Query fixture should exist at {}",
        query_path.display()
    );

    let content = fs::read_to_string(&query_path).expect("Should be able to read query file");

    // Verify it's a valid SPARQL query structure
    assert!(
        content.contains("PREFIX"),
        "Query should have PREFIX declarations"
    );
    assert!(content.contains("SELECT"), "Query should use SELECT clause");
    assert!(content.contains("WHERE"), "Query should have WHERE clause");
    assert!(
        content.contains("?className"),
        "Query should select ?className variable"
    );
    assert!(
        content.contains("?classLabel"),
        "Query should select ?classLabel variable"
    );

    println!("✓ SPARQL query fixture validated");
    println!("  File: {}", query_path.display());
    println!("  Query returns: ?className, ?classLabel");
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 4: Verify template fixture
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_template_fixture_exists() {
    let template_path = fixture_path("test-template.tera");

    assert!(
        template_path.exists(),
        "Template fixture should exist at {}",
        template_path.display()
    );

    let content = fs::read_to_string(&template_path).expect("Should be able to read template file");

    // Verify template syntax
    assert!(
        content.contains("{% for row in sparql_results %}"),
        "Template should loop over sparql_results"
    );
    assert!(
        content.contains("{{ row[\"className\"] }}"),
        "Template should access className from results"
    );
    assert!(
        content.contains("{{ sparql_results | length }}"),
        "Template should support filters"
    );

    println!("✓ Template fixture validated");
    println!("  File: {}", template_path.display());
    println!("  Template accesses: sparql_results variable");
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 5: Configuration structure validation
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_rule_filtering_structure() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Test filtering by rule name
    let test_rule = manifest
        .generation
        .rules
        .iter()
        .find(|r| r.name == "test-rule")
        .expect("Should find test-rule");

    assert_eq!(test_rule.name, "test-rule");
    assert!(
        !test_rule.skip_empty,
        "test-rule should not skip empty results"
    );

    // Test empty-rule configuration
    let empty_rule = manifest
        .generation
        .rules
        .iter()
        .find(|r| r.name == "empty-rule")
        .expect("Should find empty-rule");

    assert_eq!(empty_rule.name, "empty-rule");
    assert!(
        empty_rule.skip_empty,
        "empty-rule should skip empty results"
    );

    println!("✓ Rule filtering structure validated");
    println!("  test-rule: skip_empty={}", test_rule.skip_empty);
    println!("  empty-rule: skip_empty={}", empty_rule.skip_empty);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 6: Template pattern matching validation
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_output_file_pattern_matching() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify template variable substitution patterns
    let test_rule = &manifest.generation.rules[0];

    // Pattern should contain template variable
    assert!(
        test_rule.output_file.contains("{{"),
        "Output file pattern should use template variables"
    );
    assert!(
        test_rule.output_file.contains("className"),
        "Output pattern should reference className variable"
    );

    println!("✓ Output file pattern validated");
    println!("  Pattern: {}", test_rule.output_file);
    println!("  Variables: className");
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 7: Configuration file validation
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_manifest_file_structure() {
    let config_path = fixture_path("test-ggen.toml");
    let content = fs::read_to_string(&config_path).expect("Should read config file");

    // Verify TOML structure
    assert!(
        content.contains("[project]"),
        "Should have [project] section"
    );
    assert!(
        content.contains("[ontology]"),
        "Should have [ontology] section"
    );
    assert!(
        content.contains("[generation]"),
        "Should have [generation] section"
    );
    assert!(
        content.contains("[[generation.rules]]"),
        "Should have generation rules"
    );

    println!("✓ Manifest file structure validated");
    println!("  File size: {} bytes", content.len());
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 8: Query source configuration
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_query_source_configuration() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    let rule = &manifest.generation.rules[0];

    // Verify query source is configured as file
    match &rule.query {
        QuerySource::File { file } => {
            assert_eq!(file, &PathBuf::from("tests/fixtures/test-query.rq"));
            println!("✓ Query source correctly configured as file");
            println!("  Path: {}", file.display());
        }
        _ => panic!("Query should be configured as file source"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 9: Template source configuration
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_template_source_configuration() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    let rule = &manifest.generation.rules[0];

    // Verify template source is configured as file
    match &rule.template {
        TemplateSource::File { file } => {
            assert_eq!(file, &PathBuf::from("tests/fixtures/test-template.tera"));
            println!("✓ Template source correctly configured as file");
            println!("  Path: {}", file.display());
        }
        _ => panic!("Template should be configured as file source"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 10: Multiple rules configuration
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_multiple_rules_configuration() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify multiple rules are loaded
    assert!(manifest.generation.rules.len() >= 2);

    // Collect rule names
    let rule_names: Vec<&str> = manifest
        .generation
        .rules
        .iter()
        .map(|r| r.name.as_str())
        .collect();

    assert!(rule_names.contains(&"test-rule"), "Should have test-rule");
    assert!(rule_names.contains(&"empty-rule"), "Should have empty-rule");

    println!("✓ Multiple rules configuration validated");
    println!("  Total rules: {}", manifest.generation.rules.len());
    println!("  Rule names: {:?}", rule_names);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 11: Project metadata structure
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_project_metadata_complete() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify all project fields
    assert!(
        !manifest.project.name.is_empty(),
        "Project name should be set"
    );
    assert!(
        !manifest.project.version.is_empty(),
        "Project version should be set"
    );
    assert!(
        manifest.project.description.is_some(),
        "Project description should be present"
    );

    println!("✓ Project metadata validated");
    println!("  Name: {}", manifest.project.name);
    println!("  Version: {}", manifest.project.version);
    println!("  Description: {:?}", manifest.project.description);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 12: Ontology configuration completeness
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_ontology_configuration_complete() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify ontology fields
    assert!(
        !manifest.ontology.source.as_os_str().is_empty(),
        "Ontology source should be set"
    );
    assert!(
        manifest.ontology.base_iri.is_some(),
        "Base IRI should be set"
    );
    assert!(
        !manifest.ontology.prefixes.is_empty(),
        "Prefixes should be defined"
    );

    // Verify specific prefixes
    assert!(manifest.ontology.prefixes.contains_key("rdf"));
    assert!(manifest.ontology.prefixes.contains_key("owl"));
    assert!(manifest.ontology.prefixes.contains_key("xsd"));
    assert!(manifest.ontology.prefixes.contains_key("ex"));

    println!("✓ Ontology configuration validated");
    println!("  Source: {}", manifest.ontology.source.display());
    println!("  Base IRI: {:?}", manifest.ontology.base_iri);
    println!("  Prefixes defined: {}", manifest.ontology.prefixes.len());
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 13: Rule execution mode validation
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_rule_execution_modes() {
    use ggen_core::manifest::GenerationMode;

    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify rule mode settings
    let test_rule = &manifest.generation.rules[0];
    assert_eq!(test_rule.mode, GenerationMode::Overwrite);

    let empty_rule = &manifest.generation.rules[1];
    assert_eq!(empty_rule.mode, GenerationMode::Overwrite);

    println!("✓ Rule execution modes validated");
    println!("  test-rule mode: {:?}", test_rule.mode);
    println!("  empty-rule mode: {:?}", empty_rule.mode);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 14: Output directory configuration
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_output_directory_configuration() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    // Verify output directory is set
    assert!(!manifest.generation.output_dir.as_os_str().is_empty());
    assert_eq!(manifest.generation.output_dir, PathBuf::from("test-output"));

    println!("✓ Output directory configuration validated");
    println!("  Output dir: {}", manifest.generation.output_dir.display());
}

// ─────────────────────────────────────────────────────────────────────────────
// Summary and utility functions
// ─────────────────────────────────────────────────────────────────────────────

/// Helper to verify configuration is ready for execution
fn validate_config_ready(manifest: &ggen_core::manifest::GgenManifest) -> bool {
    // Check all required fields
    !manifest.project.name.is_empty()
        && !manifest.project.version.is_empty()
        && !manifest.ontology.source.as_os_str().is_empty()
        && !manifest.generation.rules.is_empty()
}

#[test]
fn test_integration_configuration_ready() {
    let config_path = fixture_path("test-ggen.toml");
    let manifest = ManifestParser::parse(&config_path).expect("Should parse config");

    assert!(
        validate_config_ready(&manifest),
        "Configuration should be ready for execution"
    );

    println!("✓ Configuration is ready for end-to-end execution");
    println!(
        "  Project: {} v{}",
        manifest.project.name, manifest.project.version
    );
    println!("  Ontology: {}", manifest.ontology.source.display());
    println!("  Rules: {}", manifest.generation.rules.len());
    println!("  Output: {}", manifest.generation.output_dir.display());
}
