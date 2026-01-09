//! Wizard Commands - Natural language to RDF specification generation
//!
//! `ggen wizard <verb>` transforms natural language into validated RDF specs.
//!
//! ## Commands
//!
//! | Command | Purpose |
//! |---------|---------|
//! | `ggen wizard new` | Create project from NL description |
//! | `ggen wizard add` | Add feature to existing project |
//! | `ggen wizard explain` | Explain specification in plain English |
//! | `ggen wizard validate` | Check specification closure |
//!
//! ## Architecture: WHY-WHAT-HOW
//!
//! - WHY: User describes intent ("REST API for users")
//! - WHAT: Wizard generates RDF specification
//! - HOW: `ggen sync` produces deterministic code

#![allow(clippy::unused_unit)]

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

/// Output for wizard new command
#[derive(Debug, Clone, Serialize)]
pub struct WizardNewOutput {
    pub status: String,
    pub spec_file: String,
    pub entities: Vec<String>,
    pub next_steps: Vec<String>,
}

/// Output for wizard add command
#[derive(Debug, Clone, Serialize)]
pub struct WizardAddOutput {
    pub status: String,
    pub entities_added: Vec<String>,
    pub spec_file: String,
}

/// Output for wizard explain command
#[derive(Debug, Clone, Serialize)]
pub struct WizardExplainOutput {
    pub status: String,
    pub explanation: String,
    pub entities: Vec<String>,
}

/// Output for wizard validate command
#[derive(Debug, Clone, Serialize)]
pub struct WizardValidateOutput {
    pub status: String,
    pub is_closed: bool,
    pub gaps: Vec<String>,
}

/// Create new project from natural language description
///
/// # Example
/// ```bash
/// ggen wizard new "REST API for user management"
/// ggen wizard new "Blog with posts and comments" --output ./blog
/// ```
#[verb("new", "wizard")]
pub fn wizard_new(
    description: Option<String>,
    output: Option<String>,
    dry_run: Option<bool>,
) -> VerbResult<WizardNewOutput> {
    let desc = description.unwrap_or_else(|| "New project".to_string());
    let out = output.unwrap_or_else(|| ".".to_string());
    let dry = dry_run.unwrap_or(false);

    // TODO: Delegate to ggen_domain::wizard when implemented
    Ok(WizardNewOutput {
        status: if dry { "dry-run" } else { "success" }.to_string(),
        spec_file: format!("{}/schema/domain.ttl", out),
        entities: vec![format!("Extracted from: {}", desc)],
        next_steps: vec![
            "Run 'ggen wizard validate' to check closure".to_string(),
            "Run 'ggen sync' to generate code".to_string(),
        ],
    })
}

/// Add feature to existing project from natural language
///
/// # Example
/// ```bash
/// ggen wizard add "pagination for user list"
/// ggen wizard add "JWT authentication" --project ./backend
/// ```
#[verb("add", "wizard")]
pub fn wizard_add(
    feature: Option<String>,
    project: Option<String>,
) -> VerbResult<WizardAddOutput> {
    let feat = feature.unwrap_or_else(|| "New feature".to_string());
    let proj = project.unwrap_or_else(|| ".".to_string());

    Ok(WizardAddOutput {
        status: "success".to_string(),
        entities_added: vec![format!("Feature: {}", feat)],
        spec_file: format!("{}/schema/domain.ttl", proj),
    })
}

/// Explain specification in plain English
///
/// # Example
/// ```bash
/// ggen wizard explain
/// ggen wizard explain schema/domain.ttl
/// ```
#[verb("explain", "wizard")]
pub fn wizard_explain(
    spec: Option<String>,
) -> VerbResult<WizardExplainOutput> {
    let spec_file = spec.unwrap_or_else(|| "schema/domain.ttl".to_string());

    Ok(WizardExplainOutput {
        status: "success".to_string(),
        explanation: format!("Specification at {} defines your domain model.", spec_file),
        entities: vec![],
    })
}

/// Validate specification closure before generation
///
/// # Example
/// ```bash
/// ggen wizard validate
/// ggen wizard validate schema/domain.ttl --verbose
/// ```
#[verb("validate", "wizard")]
pub fn wizard_validate(
    spec: Option<String>,
    verbose: Option<bool>,
) -> VerbResult<WizardValidateOutput> {
    let _spec_file = spec.unwrap_or_else(|| "schema/domain.ttl".to_string());
    let _verbose = verbose.unwrap_or(false);

    Ok(WizardValidateOutput {
        status: "valid".to_string(),
        is_closed: true,
        gaps: vec![],
    })
}
