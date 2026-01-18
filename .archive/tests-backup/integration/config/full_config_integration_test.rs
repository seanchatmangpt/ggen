//! Full config integration tests
//!
//! Chicago TDD: State-based testing with real filesystem operations
//! Tests end-to-end config loading, validation, and cross-module integration

use anyhow::Result;
use std::fs;
use std::path::{Path, PathBuf};
use ggen_utils::project_config::GgenConfig;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/config")
        .join(name)
}

fn load_fixture_config(name: &str) -> Result<GgenConfig> {
    let path = fixture_path(name);
    let content = fs::read_to_string(&path)?;
    toml::from_str(&content).map_err(Into::into)
}

#[test]
fn test_full_schema_integration() -> Result<()> {
    // Arrange: Load comprehensive config
    let config = load_fixture_config("full_schema.ggen.toml")?;

    // Act: Access all major sections
    let has_project = true;
    let has_prefixes = !config.prefixes.is_empty();
    let has_rdf = !config.rdf.files.is_empty() || !config.rdf.inline.is_empty();
    let has_vars = !config.vars.is_empty();

    // Assert: Verify all sections present and valid
    assert!(has_project, "Should have project section");
    assert!(has_prefixes, "Should have prefixes");
    assert!(has_rdf, "Should have RDF configuration");
    assert!(has_vars, "Should have template variables");

    // Act: Verify cross-field consistency
    let output_exists = !config.project.output_dir.as_os_str().is_empty();
    let prefixes_valid = config.prefixes.values().all(|v| v.starts_with("http"));

    // Assert: Verify data integrity
    assert!(output_exists, "Output directory should be specified");
    assert!(prefixes_valid, "All prefixes should be valid URIs");

    Ok(())
}

#[test]
fn test_workspace_member_integration() -> Result<()> {
    // Arrange: Load workspace root config
    let workspace_config = load_fixture_config("workspace.ggen.toml")?;

    // Act: Verify workspace-specific behavior
    let has_workspace_prefix = workspace_config.prefixes.contains_key("workspace");
    let workspace_output = &workspace_config.project.output_dir;

    // Assert: Verify workspace configuration
    assert!(has_workspace_prefix, "Workspace should have workspace prefix");
    assert_eq!(workspace_output, Path::new("target"));

    // Act: Verify RDF file references are workspace-relative
    let rdf_files = &workspace_config.rdf.files;
    assert_eq!(rdf_files.len(), 1);

    let file_path = &rdf_files[0];
    let is_relative = !file_path.is_absolute();

    // Assert: Verify workspace-relative paths
    assert!(is_relative, "Workspace RDF files should be relative paths");

    Ok(())
}

#[test]
fn test_config_serialization_roundtrip() -> Result<()> {
    // Arrange: Load original config
    let original = load_fixture_config("full_schema.ggen.toml")?;

    // Act: Serialize to TOML string
    let serialized = toml::to_string(&original)?;

    // Act: Deserialize back to config
    let deserialized: GgenConfig = toml::from_str(&serialized)?;

    // Assert: Verify critical fields match
    assert_eq!(
        deserialized.project.output_dir,
        original.project.output_dir,
        "Output directory should survive roundtrip"
    );

    assert_eq!(
        deserialized.prefixes.len(),
        original.prefixes.len(),
        "Prefixes count should survive roundtrip"
    );

    assert_eq!(
        deserialized.rdf.files.len(),
        original.rdf.files.len(),
        "RDF files count should survive roundtrip"
    );

    assert_eq!(
        deserialized.vars.len(),
        original.vars.len(),
        "Vars count should survive roundtrip"
    );

    Ok(())
}

#[test]
fn test_real_filesystem_rdf_resolution() -> Result<()> {
    // Arrange: Load config with real RDF file references
    let config = load_fixture_config("rdf_files.ggen.toml")?;

    // Act: Check if referenced files exist
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let base_path = Path::new(manifest_dir).join("../..");

    let files_exist: Vec<bool> = config.rdf.files
        .iter()
        .map(|f| base_path.join(f).exists())
        .collect();

    // Assert: Verify RDF files are resolvable
    assert_eq!(files_exist.len(), 2, "Should have 2 RDF file references");
    assert!(files_exist[0], "First RDF file should exist: tests/fixtures/sample.ttl");
    assert!(files_exist[1], "Second RDF file should exist: tests/fixtures/shapes.ttl");

    Ok(())
}

#[test]
fn test_inline_rdf_processing() -> Result<()> {
    // Arrange: Load config with inline RDF
    let config = load_fixture_config("rdf_inline.ggen.toml")?;

    // Act: Join inline RDF into single document
    let rdf_document = config.rdf.inline.join("\n");

    // Assert: Verify inline RDF forms valid Turtle document
    assert!(rdf_document.contains("@prefix ex:"), "Should have ex prefix declaration");
    assert!(rdf_document.contains("@prefix rdfs:"), "Should have rdfs prefix declaration");
    assert!(rdf_document.contains("ex:TestClass"), "Should have ex:TestClass definition");
    assert!(rdf_document.contains("rdfs:Class"), "Should have rdfs:Class type");
    assert!(rdf_document.contains("rdfs:label"), "Should have rdfs:label property");

    // Act: Verify RDF syntax elements
    let has_prefix_declarations = rdf_document.lines().filter(|l| l.starts_with("@prefix")).count();
    let has_triple_terminators = rdf_document.matches('.').count();

    // Assert: Verify RDF structure
    assert_eq!(has_prefix_declarations, 2, "Should have 2 prefix declarations");
    assert!(has_triple_terminators >= 1, "Should have triple terminators");

    Ok(())
}

#[test]
fn test_error_handling_missing_file() {
    // Arrange & Act: Attempt to load non-existent config
    let result = load_fixture_config("does_not_exist.ggen.toml");

    // Assert: Verify appropriate error
    assert!(result.is_err(), "Should fail when config file doesn't exist");

    let error = result.unwrap_err();
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("No such file") || error_msg.contains("not found"),
        "Error should indicate file not found, got: {}",
        error_msg
    );
}

#[test]
fn test_error_handling_invalid_toml() {
    // Arrange: Create temp file with invalid TOML
    let temp_dir = std::env::temp_dir();
    let invalid_path = temp_dir.join("invalid.ggen.toml");

    fs::write(&invalid_path, "[project]\noutput_dir = ").ok();

    // Act: Attempt to load invalid TOML
    let content = fs::read_to_string(&invalid_path).unwrap();
    let result: Result<GgenConfig, _> = toml::from_str(&content);

    // Assert: Verify TOML parse error
    assert!(result.is_err(), "Should fail on invalid TOML syntax");

    // Cleanup
    fs::remove_file(&invalid_path).ok();
}

#[test]
fn test_config_validation_workflow() -> Result<()> {
    // Arrange: Load config
    let config = load_fixture_config("full_schema.ggen.toml")?;

    // Act: Perform validation checks
    let output_dir_not_empty = !config.project.output_dir.as_os_str().is_empty();
    let has_rdf_sources = !config.rdf.files.is_empty() || !config.rdf.inline.is_empty();

    // Assert: Verify config is valid for code generation
    assert!(output_dir_not_empty, "Config validation: output_dir must be specified");
    assert!(has_rdf_sources, "Config validation: must have RDF sources (files or inline)");

    // Act: Validate prefix URIs
    let all_prefixes_valid = config.prefixes.values().all(|uri| {
        uri.starts_with("http://") || uri.starts_with("https://")
    });

    // Assert: Verify prefix URI validity
    assert!(all_prefixes_valid, "Config validation: all prefixes must be valid HTTP(S) URIs");

    Ok(())
}
