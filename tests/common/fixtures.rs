//! Test fixtures for integration tests
//!
//! Provides standard test data, templates, and configurations used across the test suite.

use ggen_core::lifecycle::{Context, Make, Phase, PhaseBuilder, Project};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;

/// Creates a sample Phase for lifecycle testing
#[allow(dead_code)]
pub fn sample_phase_init() -> Phase {
    PhaseBuilder::new("init")
        .description("Initialize project".to_string())
        .command("echo Initializing".to_string())
        .build()
        .expect("Failed to build init phase")
        .phase()
        .clone()
}

/// Creates a sample Phase for build testing
#[allow(dead_code)]
pub fn sample_phase_build() -> Phase {
    PhaseBuilder::new("build")
        .description("Build project".to_string())
        .command("echo Building".to_string())
        .build()
        .expect("Failed to build build phase")
        .phase()
        .clone()
}

/// Creates a sample Phase for test execution
#[allow(dead_code)]
pub fn sample_phase_test() -> Phase {
    PhaseBuilder::new("test")
        .description("Run tests".to_string())
        .command("echo Testing".to_string())
        .build()
        .expect("Failed to build test phase")
        .phase()
        .clone()
}

/// Creates a complete Make configuration for testing
#[allow(dead_code)]
pub fn sample_make() -> Make {
    let mut lifecycle = BTreeMap::new();
    lifecycle.insert("init".to_string(), sample_phase_init());
    lifecycle.insert("build".to_string(), sample_phase_build());
    lifecycle.insert("test".to_string(), sample_phase_test());

    Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: Some("Test project description".to_string()),
        },
        workspace: None,
        lifecycle,
        hooks: None,
    }
}

/// Creates a Context for lifecycle testing
#[allow(dead_code)]
pub fn sample_context(temp_dir: &TempDir) -> Context {
    let make = Arc::new(sample_make());
    let root = temp_dir.path().to_path_buf();
    let state_path = root.join(".ggen").join("state.json");

    Context::new(root, make, state_path, vec![])
}

/// Creates a sample Package for marketplace testing
/// Note: This function is commented out as ggen_marketplace is not available in root workspace
/// To use this, add ggen-marketplace to dev-dependencies
#[allow(dead_code)]
pub fn sample_package() -> String {
    // TODO: Implement with proper ggen_marketplace dependency
    "test-package".to_string()
}

/// Creates a temporary directory for test isolation
pub fn create_temp_dir() -> TempDir {
    TempDir::new().expect("Failed to create temp directory")
}

/// Creates sample template variables
pub fn sample_template_vars() -> BTreeMap<String, String> {
    let mut vars = BTreeMap::new();
    vars.insert("project_name".to_string(), "TestProject".to_string());
    vars.insert("author".to_string(), "Test Author".to_string());
    vars.insert("version".to_string(), "1.0.0".to_string());
    vars
}

/// Creates a simple template content string
pub fn sample_template_content() -> String {
    r#"# {{ project_name }}

Author: {{ author }}
Version: {{ version }}

This is a test template.
"#
    .to_string()
}

/// Creates a sample make.toml content
pub fn sample_make_toml() -> String {
    r#"[project]
name = "test-project"
version = "1.0.0"
description = "Test project"

[lifecycle.init]
description = "Initialize"
commands = ["echo Initializing"]

[lifecycle.build]
description = "Build"
commands = ["echo Building"]

[lifecycle.test]
description = "Test"
commands = ["echo Testing"]
"#
    .to_string()
}

/// Creates a sample Gpack manifest content
pub fn sample_gpack_manifest() -> String {
    r#"{
  "name": "test-gpack",
  "version": "1.0.0",
  "description": "Test gpack for integration tests",
  "author": "Test Author",
  "license": "MIT",
  "templates": [
    {
      "name": "basic",
      "path": "templates/basic.tmpl",
      "description": "Basic template"
    }
  ]
}
"#
    .to_string()
}

/// Helper to create a test cache directory path
pub fn test_cache_path() -> PathBuf {
    let temp = create_temp_dir();
    temp.path().join(".cache")
}
