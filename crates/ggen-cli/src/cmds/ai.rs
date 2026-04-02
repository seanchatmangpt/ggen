//! AI commands - Thin CLI layer delegating to ggen-domain/src/ai
//!
//! NOTE: This module requires completion of the ai domain refactoring.
//! The ai domain module structure needs to be finalized to provide the expected functions.
//!
//! This stub module allows the library to compile while the ai domain refactoring is completed.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

// Output types for JSON serialization
#[derive(Serialize)]
struct GenerateOutput {
    success: bool,
}

#[derive(Serialize)]
struct ChatOutput {
    success: bool,
}

#[derive(Serialize)]
struct AnalyzeOutput {
    success: bool,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Generate code with AI assistance
///
/// NOTE: This command is temporarily stubbed pending completion of the AI domain refactoring.
#[allow(clippy::too_many_arguments)]
#[verb]
fn generate(
    _prompt: String, _code: Option<String>, _model: Option<String>, _api_key: Option<String>,
    _suggestions: bool, _language: Option<String>, _max_tokens: i64, _temperature: f64,
) -> Result<GenerateOutput> {
    // FUTURE: Connect to ggen_domain::ai::generate_code or appropriate domain function
    // This stub allows library compilation while domain refactoring is completed
    Ok(GenerateOutput { success: false })
}

/// Interactive AI chat session
///
/// NOTE: This command is temporarily stubbed pending completion of the AI domain refactoring.
#[verb]
fn chat(
    _message: Option<String>, _model: Option<String>, _interactive: bool, _stream: bool,
    _max_tokens: Option<i64>, _temperature: Option<f64>,
) -> Result<ChatOutput> {
    // FUTURE: Connect to appropriate domain function
    // This stub allows library compilation while domain refactoring is completed
    Ok(ChatOutput { success: false })
}

/// Analyze code and provide insights
///
/// NOTE: This command is temporarily stubbed pending completion of the AI domain refactoring.
#[verb]
fn analyze(
    _file: Option<PathBuf>, _code: Option<String>, _model: Option<String>,
) -> Result<AnalyzeOutput> {
    // FUTURE: Connect to ggen_domain::ai::analyze or appropriate domain function
    // This stub allows library compilation while domain refactoring is completed
    Ok(AnalyzeOutput { success: false })
}
