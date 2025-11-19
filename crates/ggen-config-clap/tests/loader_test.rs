//! Tests for config-clap loader

use ggen_config_clap::loader::{expand_env_vars, load_ggen_config};
use std::env;
use std::fs::File;
use std::io::Write;
use tempfile::tempdir;

#[test]
fn test_expand_env_vars_brace() {
    env::set_var("TEST_VAR", "value");
    let result = expand_env_vars("Path: ${TEST_VAR}/file");
    assert_eq!(result, "Path: value/file");
    env::remove_var("TEST_VAR");
}

#[test]
fn test_expand_env_vars_simple() {
    env::set_var("USER", "testuser");
    let result = expand_env_vars("Home: /home/$USER");
    assert_eq!(result, "Home: /home/testuser");
}

#[test]
fn test_load_valid_config() {
    let dir = tempdir().expect("Failed to create temp dir");
    let config_path = dir.path().join("ggen.toml");

    let mut file = File::create(&config_path).expect("Failed to create file");
    writeln!(
        file,
        r#"
[lifecycle]
templates_dir = "templates"
output_dir = "output"
    "#
    )
    .expect("Failed to write");

    let result = load_ggen_config(&config_path);
    assert!(result.is_ok());

    let config = result.expect("Should load config");
    // Verify lifecycle section exists
    assert!(config.lifecycle.is_some());
}

#[test]
fn test_load_missing_config() {
    let result = load_ggen_config("/nonexistent/ggen.toml");
    assert!(result.is_err());
}

#[test]
fn test_expand_multiple_vars() {
    env::set_var("VAR1", "value1");
    env::set_var("VAR2", "value2");

    let result = expand_env_vars("${VAR1} and ${VAR2}");
    assert_eq!(result, "value1 and value2");

    env::remove_var("VAR1");
    env::remove_var("VAR2");
}
