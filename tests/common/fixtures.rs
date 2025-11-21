//! Test fixtures for integration tests
//!
//! Provides standard test data, templates, and configurations used across the test suite.

use ggen_core::lifecycle::{Context, Make, Phase, PhaseBuilder, Project};
use ggen_marketplace::models::{Package, PackageId, Version};
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
        #[allow(clippy::expect_used)]
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
        #[allow(clippy::expect_used)]
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
        #[allow(clippy::expect_used)]
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
pub fn sample_package() -> Package {
    use ggen_marketplace::models::{ContentId, HashAlgorithm, PackageStats};

    let mut metadata = ggen_marketplace::models::PackageMetadata::default();
    metadata.title = "Test Package".to_string();
    metadata.description = "A test package for integration tests".to_string();
    metadata.license = "MIT".to_string();

    Package {
        id: PackageId::new("test-namespace", "test-package"),
        version: Version::new(1, 0, 0),
        metadata,
        content_id: ContentId::new("test-content-hash", HashAlgorithm::Sha256),
        dependencies: vec![],
        stats: PackageStats::default(),
        created_at: chrono::Utc::now(),
        updated_at: chrono::Utc::now(),
    }
}

/// Creates a temporary directory for test isolation
pub fn create_temp_dir() -> TempDir {
    #[allow(clippy::expect_used)]
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
