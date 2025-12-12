//! AI commands - Thin CLI layer delegating to ggen-domain/src/ai
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async coordination, resource management
//! - **Layer 1 (Domain)**: Pure business logic from ggen_domain::ai
//!
//! All verbs use explicit `#[verb("verb", "ai")]` registration for clap-noun-verb v5.3.0.

// Standard library imports
use std::path::PathBuf;

// External crate imports
use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use serde_json::json;

// Local crate imports
use crate::cmds::helpers::{
    execute_async_op, log_operation, DEFAULT_MAX_TOKENS, DEFAULT_TEMPERATURE,
};
use ggen_domain::ai::execute;

// Logging uses shared helpers::log_operation

// ============================================================================
// Output Types (Layer 3: CLI - Structured JSON responses)
// ============================================================================

/// Generation output with detailed result data
#[derive(Debug, Serialize, Clone)]
struct GenerateOutput {
    success: bool,
    analysis: Option<String>,
    suggestions: Option<Vec<String>>,
    model_used: Option<String>,
    processing_time_ms: Option<u64>,
}

/// Chat output for interactive sessions
#[derive(Debug, Serialize, Clone)]
struct ChatOutput {
    success: bool,
    message: Option<String>,
    model_used: Option<String>,
}

/// Analysis output with insights
#[derive(Debug, Serialize, Clone)]
struct AnalyzeOutput {
    success: bool,
    analysis: Option<String>,
    file_analyzed: Option<String>,
}

// ============================================================================
// Verb Functions (Layer 3: CLI - Input validation + output formatting)
// ============================================================================

/// Generate code with AI assistance
///
/// Calls ggen_domain::ai::generate_code with builder pattern.
/// Layer 3: Validates input and formats output for CLI.
/// Layer 2: ggen_domain coordinates async execution.
/// Layer 1: Pure domain logic in ggen_domain::ai.
#[allow(clippy::too_many_arguments)]
#[verb("generate", "ai")]
fn generate(
    prompt: String,
    code: Option<String>,
    model: Option<String>,
    _api_key: Option<String>, // FUTURE: Use for LLM provider authentication
    suggestions: bool,
    _language: Option<String>, // FUTURE: Use for language-specific generation
    max_tokens: Option<i64>,
    temperature: Option<f64>,
) -> Result<GenerateOutput> {
    // Apply safe defaults so callers are not forced to pass tuning flags
    let max_tokens = max_tokens.unwrap_or(DEFAULT_MAX_TOKENS);
    let temperature = temperature.unwrap_or(DEFAULT_TEMPERATURE);

    // #region agent log
    log_operation(
        "ai::generate:entry",
        "generate called",
        json!({
            "prompt_len": prompt.len(),
            "has_code": code.is_some(),
            "model": model.as_ref(),
            "suggestions": suggestions,
            "max_tokens": max_tokens,
            "temperature": temperature,
        }),
    );
    // #endregion

    // Layer 3: Input validation
    if prompt.is_empty() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Prompt cannot be empty",
        ));
    }

    // Layer 2: Call execute layer (async coordination)
    let result = execute_async_op("generate", async {
        execute::execute_generate(&prompt, code.as_deref(), model.as_deref(), suggestions)
            .await
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Generation failed: {}", e))
            })
    })?;

    // Layer 3: Format output for CLI
    Ok(GenerateOutput {
        success: true,
        analysis: Some(result.analysis),
        suggestions: Some(result.suggestions),
        model_used: Some(result.model_used),
        processing_time_ms: Some(result.processing_time_ms),
    })
}

/// Interactive AI chat session
///
/// FUTURE: Implement full chat interface with streaming and conversation history.
/// Currently returns placeholder response.
#[verb("chat", "ai")]
fn chat(
    message: Option<String>,
    model: Option<String>,
    interactive: bool, // FUTURE: Support interactive mode
    stream: bool,      // FUTURE: Support streaming responses
    max_tokens: Option<i64>,
    temperature: Option<f64>,
) -> Result<ChatOutput> {
    // #region agent log
    log_operation(
        "ai::chat:entry",
        "chat called",
        json!({
            "has_message": message.is_some(),
            "message_len": message.as_ref().map(|m| m.len()),
            "model": model.as_ref(),
            "interactive": interactive,
            "stream": stream,
        }),
    );
    // #endregion

    // Layer 3: Input validation
    let msg = message.unwrap_or_else(|| "Hello".to_string());

    if msg.is_empty() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Message cannot be empty",
        ));
    }

    // Surface optional tuning parameters and avoid unused warnings
    let _ = (
        max_tokens.unwrap_or(DEFAULT_MAX_TOKENS),
        temperature.unwrap_or(DEFAULT_TEMPERATURE),
    );

    // Layer 2: Call execute layer (async coordination)
    let result = execute_async_op("chat", async {
        execute::execute_chat(&msg, model.as_deref())
            .await
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Chat failed: {}", e))
            })
    })?;

    // #region agent log
    log_operation(
        "ai::chat:result",
        "chat result received",
        json!({
            "response_preview": result.response.chars().take(80).collect::<String>(),
            "model_used": result.model_used.clone(),
        }),
    );
    // #endregion

    // Layer 3: Format output for CLI
    Ok(ChatOutput {
        success: true,
        message: Some(result.response),
        model_used: Some(result.model_used),
    })
}

/// Analyze code and provide insights
///
/// Analyzes either a code snippet or a project directory using the AI domain layer.
/// Layer 3: Validates input and handles file I/O.
/// Layer 2: ggen_domain::ai::analyze handles async analysis.
/// Layer 1: Pure analysis logic in analyze.rs.
#[verb("analyze", "ai")]
fn analyze(
    file: Option<PathBuf>,
    code: Option<String>,
    _model: Option<String>, // FUTURE: Use model selection for analysis
) -> Result<AnalyzeOutput> {
    // #region agent log
    log_operation(
        "ai::analyze:entry",
        "analyze called",
        json!({
            "has_file": file.is_some(),
            "file": file.as_ref().map(|p| p.display().to_string()),
            "code_len": code.as_ref().map(|c| c.len()),
        }),
    );
    // #endregion

    // Layer 3: Input validation - must have either file or code
    if file.is_none() && code.is_none() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Must provide either --file or --code to analyze",
        ));
    }

    // Layer 2: Call execute layer (async coordination)
    let result = execute_async_op("analyze", async {
        execute::execute_analyze(code.as_deref(), file.as_deref())
            .await
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Analysis failed: {}", e))
            })
    })?;

    // #region agent log
    log_operation(
        "ai::analyze:result",
        "analyze result received",
        json!({
            "analysis_preview": result.analysis.chars().take(80).collect::<String>(),
            "file_analyzed": result.file_analyzed.clone(),
        }),
    );
    // #endregion

    // Layer 3: Format output for CLI
    Ok(AnalyzeOutput {
        success: true,
        analysis: Some(result.analysis),
        file_analyzed: result.file_analyzed,
    })
}

// ============================================================================
// Unit Tests (Chicago TDD - State-based verification)
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_with_prompt_only() {
        let result = generate(
            "write a hello world program".to_string(),
            None,
            None,
            None,
            false,
            None,
            Some(-1),
            Some(0.0),
        );

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.analysis.is_some());
    }

    #[test]
    fn test_generate_with_all_options() {
        let result = generate(
            "optimize this code".to_string(),
            Some("fn main() {}".to_string()),
            Some("gpt-4".to_string()),
            Some("test-key".to_string()),
            true,
            Some("rust".to_string()),
            Some(256),
            Some(0.7),
        );

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.suggestions.is_some());
        assert_eq!(output.model_used, Some("gpt-4".to_string()));
    }

    #[test]
    fn test_generate_empty_prompt_fails() {
        let result = generate(
            "".to_string(),
            None,
            None,
            None,
            false,
            None,
            Some(-1),
            Some(0.0),
        );

        assert!(result.is_err());
    }

    #[test]
    fn test_analyze_code_snippet() {
        let result = analyze(None, Some("fn main() {}".to_string()), None);

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.analysis.is_some());
        assert!(output.file_analyzed.is_none());
    }

    #[test]
    fn test_analyze_project_directory() {
        let result = analyze(Some(PathBuf::from(".")), None, None);

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.file_analyzed.is_some());
    }

    #[test]
    fn test_analyze_nonexistent_path_fails() {
        let result = analyze(Some(PathBuf::from("/nonexistent/path")), None, None);

        assert!(result.is_err());
    }

    #[test]
    fn test_analyze_no_input_fails() {
        let result = analyze(None, None, None);

        assert!(result.is_err());
    }

    #[test]
    fn test_analyze_empty_code_fails() {
        let result = analyze(None, Some("".to_string()), None);

        assert!(result.is_err());
    }

    #[test]
    fn test_chat_with_message() {
        let result = chat(
            Some("hello assistant".to_string()),
            Some("gpt-4".to_string()),
            false,
            false,
            Some(256),
            Some(0.7),
        );

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.message.is_some());
        assert_eq!(output.model_used, Some("gpt-4".to_string()));
    }

    #[test]
    fn test_chat_default_message() {
        let result = chat(None, None, true, true, None, None);

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.message.is_some());
    }

    #[test]
    fn test_chat_empty_message_fails() {
        let result = chat(Some("".to_string()), None, false, false, None, None);

        assert!(result.is_err());
    }
}
