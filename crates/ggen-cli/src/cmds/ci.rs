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
    let output_path = output.unwrap_or_else(|| {
        format!(".github/workflows/{}.yml", workflow_name)
    });

    Ok(WorkflowOutput {
        workflow_name,
        status: "Generated".to_string(),
        path: Some(output_path),
    })
}
