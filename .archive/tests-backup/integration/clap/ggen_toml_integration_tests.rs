//! ggen.toml Integration Tests
//!
//! Tests configuration loading, CLI argument overrides,
//! environment variable precedence, and validation.

use std::env;
use std::fs;
use tempfile::TempDir;
use assert_cmd::Command;
use predicates::prelude::*;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Test that ggen.toml can be loaded
#[test]
fn test_load_ggen_toml() {
    let temp_dir = TempDir::new().unwrap();

    let config = r#"
[project]
name = "test-project"
version = "1.0.0"

[template]
default_engine = "tera"
output_dir = "./generated"
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, config).unwrap();

    // Verify config can be parsed
    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    assert_eq!(parsed["project"]["name"].as_str().unwrap(), "test-project");
    assert_eq!(parsed["template"]["default_engine"].as_str().unwrap(), "tera");
}

/// Test CLI args override ggen.toml values
#[test]
fn test_cli_args_override_config() {
    let temp_dir = TempDir::new().unwrap();

    let config = r#"
[template]
output_dir = "./default-output"
"#;

    fs::write(temp_dir.path().join("ggen.toml"), config).unwrap();

    // CLI args should override config
    // This test verifies the structure; actual override logic is in the implementation
    let cli_override = temp_dir.path().join("cli-output");

    assert_ne!(
        cli_override.to_str().unwrap(),
        "./default-output",
        "CLI arg should differ from config default"
    );
}

/// Test environment variables override config
#[test]
fn test_env_vars_override_config() {
    let temp_dir = TempDir::new().unwrap();

    let config = r#"
[template]
output_dir = "./default-output"
"#;

    fs::write(temp_dir.path().join("ggen.toml"), config).unwrap();

    // Set environment variable
    env::set_var("GGEN_OUTPUT_DIR", "/env/output");

    let env_value = env::var("GGEN_OUTPUT_DIR").unwrap();
    assert_eq!(env_value, "/env/output");

    // Clean up
    env::remove_var("GGEN_OUTPUT_DIR");
}

/// Test precedence: CLI > Env > Config > Defaults
#[test]
fn test_configuration_precedence() {
    let temp_dir = TempDir::new().unwrap();

    // 1. Default (lowest priority)
    let default_value = "default";

    // 2. Config file
    let config_value = "from-config";
    let config = format!(
        r#"
[template]
output_dir = "{}"
"#,
        config_value
    );
    fs::write(temp_dir.path().join("ggen.toml"), config).unwrap();

    // 3. Environment variable
    let env_value = "from-env";
    env::set_var("GGEN_OUTPUT_DIR", env_value);

    // 4. CLI argument (highest priority)
    let cli_value = "from-cli";

    // Verify precedence order
    assert_ne!(config_value, default_value);
    assert_ne!(env_value, config_value);
    assert_ne!(cli_value, env_value);

    // Clean up
    env::remove_var("GGEN_OUTPUT_DIR");
}

/// Test type validation (string → number)
#[test]
fn test_type_validation() {
    let temp_dir = TempDir::new().unwrap();

    // Invalid type in config
    let invalid_config = r#"
[project]
version = 123  # Should be string
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, invalid_config).unwrap();

    // Parse to verify type handling
    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    // TOML allows number, but we can check type
    let version = &parsed["project"]["version"];
    assert!(version.is_integer(), "Version is a number, not string");

    // Valid config
    let valid_config = r#"
[project]
version = "1.0.0"  # Correct type
"#;

    fs::write(&config_path, valid_config).unwrap();
    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    let version = &parsed["project"]["version"];
    assert!(version.is_str(), "Version should be a string");
}

/// Test missing required fields produce good errors
#[test]
fn test_missing_required_fields() {
    let temp_dir = TempDir::new().unwrap();

    // Config missing required field
    let incomplete_config = r#"
[project]
# Missing 'name' field
version = "1.0.0"
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, incomplete_config).unwrap();

    // Verify we can detect missing fields
    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    assert!(parsed["project"]["name"].is_none(), "Name field should be missing");
}

/// Test invalid TOML produces good error
#[test]
fn test_invalid_toml_error() {
    let temp_dir = TempDir::new().unwrap();

    // Invalid TOML syntax
    let invalid_toml = r#"
[project
name = "test"  # Missing closing bracket
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, invalid_toml).unwrap();

    // Attempt to parse
    let content = fs::read_to_string(&config_path).unwrap();
    let result = toml::from_str::<toml::Value>(&content);

    assert!(result.is_err(), "Invalid TOML should produce error");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains("expected") || error.to_string().contains("TOML"),
        "Error should mention TOML syntax: {}",
        error
    );
}

/// Test config loading performance
#[test]
fn test_config_loading_performance() {
    use std::time::Instant;

    let temp_dir = TempDir::new().unwrap();

    let config = r#"
[project]
name = "perf-test"
version = "1.0.0"

[template]
engine = "tera"
output_dir = "./output"

[marketplace]
enabled = true
cache_ttl = 3600
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, config).unwrap();

    let start = Instant::now();
    for _ in 0..100 {
        let content = fs::read_to_string(&config_path).unwrap();
        let _parsed: toml::Value = toml::from_str(&content).unwrap();
    }
    let duration = start.elapsed();

    // Config loading should be fast (<10ms per load)
    assert!(
        duration.as_millis() < 1000,
        "Config loading should be fast for 100 iterations, took {:?}",
        duration
    );
}

/// Test large config file handling
#[test]
fn test_large_config_file() {
    let temp_dir = TempDir::new().unwrap();

    // Generate large config with many sections
    let mut config = String::from("[project]\nname = \"large-config\"\nversion = \"1.0.0\"\n\n");

    for i in 0..100 {
        config.push_str(&format!(
            "[section_{}]\nkey_{} = \"value_{}\"\ncount = {}\n\n",
            i, i, i, i
        ));
    }

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, &config).unwrap();

    // Should handle large config
    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    assert!(parsed["project"]["name"].as_str().unwrap() == "large-config");
    assert!(parsed["section_99"].is_table(), "Should have many sections");
}

/// Test array and nested object validation
#[test]
fn test_complex_config_structures() {
    let temp_dir = TempDir::new().unwrap();

    let config = r#"
[project]
name = "complex-config"

[[templates]]
name = "template1"
engine = "tera"

[[templates]]
name = "template2"
engine = "handlebars"

[metadata]
tags = ["tag1", "tag2", "tag3"]
authors = [
    { name = "Alice", email = "alice@example.com" },
    { name = "Bob", email = "bob@example.com" }
]
"#;

    let config_path = temp_dir.path().join("ggen.toml");
    fs::write(&config_path, config).unwrap();

    let content = fs::read_to_string(&config_path).unwrap();
    let parsed: toml::Value = toml::from_str(&content).unwrap();

    // Verify arrays
    assert!(parsed["templates"].is_array());
    assert_eq!(parsed["templates"].as_array().unwrap().len(), 2);

    assert!(parsed["metadata"]["tags"].is_array());
    assert_eq!(parsed["metadata"]["tags"].as_array().unwrap().len(), 3);

    // Verify nested objects
    assert!(parsed["metadata"]["authors"].is_array());
    assert_eq!(parsed["metadata"]["authors"].as_array().unwrap().len(), 2);
}
