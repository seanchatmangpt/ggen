//! CI commands - clap-noun-verb v5.3.0 Migration
//!
//! CI/CD workflow management with GitHub Actions integration using the
//! v5.3.0 #[verb("verb", "noun")] pattern.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting
//! - **Layer 2 (Integration)**: Async coordination, resource management
//! - **Layer 1 (Domain)**: Pure CI logic from ggen_domain::ci

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct WorkflowOutput {
    workflow_name: String,
    status: String,
    path: Option<String>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Generate GitHub Actions workflow configuration
///
/// # Usage
///
/// ```bash
/// # Generate default workflow
/// ggen ci workflow
///
/// # Generate workflow with custom name
/// ggen ci workflow --name "release-pipeline"
/// ```
#[verb("workflow", "ci")]
fn workflow(name: Option<String>, output: Option<String>) -> Result<WorkflowOutput> {
    let workflow_name = name.unwrap_or_else(|| "build".to_string());
    let output_path = output.unwrap_or_else(|| format!(".github/workflows/{}.yml", workflow_name));
    let path = if output_path.starts_with('/') {
        Path::new(&output_path).to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(&output_path)
    };

    // Create parent directory if it doesn't exist
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create directory: {}",
                e
            ))
        })?;
    }

    // Write basic GitHub Actions workflow
    let workflow_content = format!(
        r#"name: {}

on:
  push:
    branches: [ main, master ]
  pull_request:
    branches: [ main, master ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - name: Setup Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
    - name: Build
      run: cargo build --release
    - name: Test
      run: cargo test
"#,
        workflow_name
    );

    fs::write(&path, workflow_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to write workflow to {}: {}",
            path.display(),
            e
        ))
    })?;

    Ok(WorkflowOutput {
        workflow_name,
        status: "Generated".to_string(),
        path: Some(output_path),
    })
}
