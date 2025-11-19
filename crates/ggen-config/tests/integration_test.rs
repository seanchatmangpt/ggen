//! Integration tests for ggen-config

use ggen_config::{ConfigError, ConfigLoader, ConfigValidator};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_load_microservices_config() {
    // This tests loading a real ggen.toml from the examples
    let config_path = "../../examples/microservices-architecture/ggen.toml";

    // Try to load the config
    if std::path::Path::new(config_path).exists() {
        let result = ConfigLoader::from_file(config_path);
        assert!(
            result.is_ok(),
            "Failed to load microservices config: {:?}",
            result.err()
        );

        let config = result.unwrap();

        // Verify project info
        assert_eq!(config.project.name, "microservices-architecture");
        assert_eq!(config.project.version, "1.0.0");

        // Verify AI config
        assert!(config.ai.is_some());
        let ai = config.ai.as_ref().unwrap();
        assert_eq!(ai.provider, "ollama");
        assert_eq!(ai.model, "qwen2.5-coder");

        // Verify templates config
        assert!(config.templates.is_some());

        // Validate the config
        let validation_result = ConfigValidator::validate(&config);
        assert!(
            validation_result.is_ok(),
            "Validation failed: {:?}",
            validation_result.err()
        );
    }
}

#[test]
fn test_load_advanced_rust_config() {
    let config_path = "../../marketplace/packages/advanced-rust-project/ggen.toml";

    if std::path::Path::new(config_path).exists() {
        let result = ConfigLoader::from_file(config_path);
        assert!(
            result.is_ok(),
            "Failed to load advanced-rust config: {:?}",
            result.err()
        );

        let config = result.unwrap();

        // Verify comprehensive config sections
        assert_eq!(config.project.name, "advanced-rust-project");
        assert!(config.ai.is_some());
        assert!(config.rdf.is_some());
        assert!(config.security.is_some());
        assert!(config.logging.is_some());

        // Validate
        assert!(ConfigValidator::validate(&config).is_ok());
    }
}

#[test]
fn test_create_and_load_config() {
    let temp_dir = TempDir::new().unwrap();
    let config_path = temp_dir.path().join("ggen.toml");

    // Create a minimal config
    let config_content = r#"
[project]
name = "test-project"
version = "1.0.0"
description = "A test project"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true

[security]
path_traversal_protection = true
shell_injection_protection = true
"#;

    fs::write(&config_path, config_content).unwrap();

    // Load the config
    let config = ConfigLoader::from_file(&config_path).unwrap();

    assert_eq!(config.project.name, "test-project");
    assert_eq!(config.project.version, "1.0.0");

    let ai = config.ai.as_ref().unwrap();
    assert_eq!(ai.provider, "openai");
    assert_eq!(ai.model, "gpt-4");

    // Validate
    assert!(ConfigValidator::validate(&config).is_ok());
}

#[test]
fn test_environment_overrides() {
    let temp_dir = TempDir::new().unwrap();
    let config_path = temp_dir.path().join("ggen.toml");

    let config_content = r#"
[project]
name = "env-test"
version = "1.0.0"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7

[logging]
level = "info"

[env.development]
"ai.model" = "gpt-3.5-turbo"
"ai.temperature" = 0.9
"logging.level" = "debug"

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"#;

    fs::write(&config_path, config_content).unwrap();

    // Load base config
    let loader = ConfigLoader::new(&config_path).unwrap();
    let base_config = loader.load().unwrap();
    let base_ai = base_config.ai.as_ref().unwrap();
    assert_eq!(base_ai.model, "gpt-4");
    assert!((base_ai.temperature - 0.7).abs() < f32::EPSILON);

    // Load with development environment
    let dev_config = loader.load_with_env("development").unwrap();
    let dev_ai = dev_config.ai.as_ref().unwrap();
    assert_eq!(dev_ai.model, "gpt-3.5-turbo");
    assert!((dev_ai.temperature - 0.9).abs() < f32::EPSILON);
    assert_eq!(dev_config.logging.as_ref().unwrap().level, "debug");

    // Load with production environment
    let prod_config = loader.load_with_env("production").unwrap();
    let prod_ai = prod_config.ai.as_ref().unwrap();
    // Model should stay as base since not overridden in production
    assert_eq!(prod_ai.model, "gpt-4");
    assert!((prod_ai.temperature - 0.3).abs() < f32::EPSILON);
    assert_eq!(prod_config.logging.as_ref().unwrap().level, "warn");
}

#[test]
fn test_validation_failures() {
    // Invalid version
    let invalid_version = r#"
[project]
name = "test"
version = "invalid"
"#;

    let config = ConfigLoader::from_str(invalid_version).unwrap();
    assert!(ConfigValidator::validate(&config).is_err());

    // Invalid AI temperature
    let invalid_temp = r#"
[project]
name = "test"
version = "1.0.0"

[ai]
provider = "openai"
model = "gpt-4"
temperature = 1.5
"#;

    let config = ConfigLoader::from_str(invalid_temp).unwrap();
    let result = ConfigValidator::validate(&config);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("temperature"));

    // Invalid log level
    let invalid_log = r#"
[project]
name = "test"
version = "1.0.0"

[logging]
level = "invalid"
format = "json"
"#;

    let config = ConfigLoader::from_str(invalid_log).unwrap();
    assert!(ConfigValidator::validate(&config).is_err());
}

#[test]
fn test_file_not_found() {
    let result = ConfigLoader::from_file("/nonexistent/path/ggen.toml");
    assert!(result.is_err());
    match result.unwrap_err() {
        ConfigError::FileNotFound(_) => {}
        other => panic!("Expected FileNotFound, got {:?}", other),
    }
}

#[test]
fn test_invalid_toml() {
    let invalid_toml = r#"
[project
name = "test"
version = "1.0.0"
"#;

    let result = ConfigLoader::from_str(invalid_toml);
    assert!(result.is_err());
    match result.unwrap_err() {
        ConfigError::TomlParse(_) => {}
        other => panic!("Expected TomlParse, got {:?}", other),
    }
}

#[test]
fn test_minimal_valid_config() {
    let minimal = r#"
[project]
name = "minimal"
version = "0.1.0"
"#;

    let config = ConfigLoader::from_str(minimal).unwrap();
    assert_eq!(config.project.name, "minimal");
    assert!(ConfigValidator::validate(&config).is_ok());
}

#[test]
fn test_default_values_populated() {
    let toml = r#"
[project]
name = "defaults"
version = "1.0.0"

[ai]
provider = "ollama"
model = "llama2"
"#;

    let config = ConfigLoader::from_str(toml).unwrap();
    let ai = config.ai.as_ref().unwrap();

    // Verify default values are set
    assert_eq!(ai.temperature, 0.7);
    assert_eq!(ai.max_tokens, 2000);
    assert_eq!(ai.timeout, 30);
}
