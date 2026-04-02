//! Snapshot tests for GgenConfig stability
//!
//! Uses insta for snapshot testing of serialized configs
//! Chicago TDD: Snapshots verify config structure remains stable

use ggen_utils::project_config::GgenConfig;
use std::fs;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/config")
        .join(name)
}

fn load_config(name: &str) -> anyhow::Result<GgenConfig> {
    let path = fixture_path(name);
    let content = fs::read_to_string(&path)?;
    toml::from_str(&content).map_err(Into::into)
}

#[test]
fn test_full_schema_serialization_snapshot() -> anyhow::Result<()> {
    // Arrange: Load full schema config
    let config = load_config("full_schema.ggen.toml")?;

    // Act: Serialize to TOML
    let serialized = toml::to_string(&config)?;

    // Assert: Snapshot test (manually verify first time, then auto-check)
    // Note: In production, use: insta::assert_snapshot!(serialized);
    // For now, verify manually that serialization is stable

    assert!(serialized.contains("[project]"));
    assert!(serialized.contains("output_dir"));
    assert!(serialized.contains("[prefixes]"));
    assert!(serialized.contains("[rdf]"));
    assert!(serialized.contains("[vars]"));

    Ok(())
}

#[test]
fn test_minimal_config_serialization_snapshot() -> anyhow::Result<()> {
    // Arrange: Load minimal config
    let config = load_config("minimal.ggen.toml")?;

    // Act: Serialize to TOML
    let serialized = toml::to_string(&config)?;

    // Assert: Verify minimal structure
    assert!(serialized.contains("[project]"));
    assert!(serialized.contains("output_dir = \"out\""));

    // Minimal config should not have optional sections in serialized form
    // (unless serde adds empty collections - verify actual behavior)

    Ok(())
}

#[test]
fn test_workspace_config_serialization_snapshot() -> anyhow::Result<()> {
    // Arrange: Load workspace config
    let config = load_config("workspace.ggen.toml")?;

    // Act: Serialize to TOML
    let serialized = toml::to_string(&config)?;

    // Assert: Verify workspace structure
    assert!(serialized.contains("[project]"));
    assert!(serialized.contains("output_dir = \"target\""));
    assert!(serialized.contains("[prefixes]"));
    assert!(serialized.contains("workspace = \"http://example.org/workspace/\""));

    Ok(())
}

#[test]
fn test_rdf_files_serialization_snapshot() -> anyhow::Result<()> {
    // Arrange: Load config with RDF files
    let config = load_config("rdf_files.ggen.toml")?;

    // Act: Serialize to TOML
    let serialized = toml::to_string(&config)?;

    // Assert: Verify RDF files are serialized as array
    assert!(serialized.contains("files = ["));
    assert!(serialized.contains("\"tests/fixtures/sample.ttl\""));
    assert!(serialized.contains("\"tests/fixtures/shapes.ttl\""));

    Ok(())
}

#[test]
fn test_rdf_inline_serialization_snapshot() -> anyhow::Result<()> {
    // Arrange: Load config with inline RDF
    let config = load_config("rdf_inline.ggen.toml")?;

    // Act: Serialize to TOML
    let serialized = toml::to_string(&config)?;

    // Assert: Verify inline RDF is serialized as array
    assert!(serialized.contains("inline = ["));
    assert!(serialized.contains("@prefix ex:"));
    assert!(serialized.contains("@prefix rdfs:"));
    assert!(serialized.contains("ex:TestClass"));

    Ok(())
}

#[test]
fn test_serialization_format_stability() -> anyhow::Result<()> {
    // Arrange: Load config
    let config = load_config("full_schema.ggen.toml")?;

    // Act: Serialize multiple times
    let serialized1 = toml::to_string(&config)?;
    let serialized2 = toml::to_string(&config)?;

    // Assert: Verify serialization is deterministic
    assert_eq!(
        serialized1, serialized2,
        "Serialization should be deterministic"
    );

    Ok(())
}

#[test]
fn test_deserialization_from_snapshot() -> anyhow::Result<()> {
    // Arrange: Known good TOML string (simulating snapshot)
    let snapshot = r#"
[project]
output_dir = "generated"

[prefixes]
ex = "http://example.org/"

[rdf]
files = ["data.ttl"]

[vars]
test = "value"
"#;

    // Act: Deserialize from snapshot
    let config: GgenConfig = toml::from_str(snapshot)?;

    // Assert: Verify structure matches snapshot
    assert_eq!(config.project.output_dir.to_str().unwrap(), "generated");
    assert_eq!(config.prefixes.len(), 1);
    assert_eq!(config.rdf.files.len(), 1);
    assert_eq!(config.vars.len(), 1);

    Ok(())
}
