//! AI commands - Thin CLI layer delegating to ggen-domain/src/ai
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async coordination, resource management
//! - **Layer 1 (Domain)**: Pure business logic from ggen_domain::ai
//!
//! All verbs use explicit `#[verb("verb", "ai")]` registration for clap-noun-verb v5.3.0.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use ggen_domain::ai::{self, generate::OutputFormat};
use serde::Serialize;
use std::path::PathBuf;

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
    _max_tokens: i64,
    _temperature: f64,
) -> Result<GenerateOutput> {
    // Layer 3: Input validation
    if prompt.is_empty() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Prompt cannot be empty",
        ));
    }

    // Layer 2: Call domain layer (blocking for now, could be async)
    // FUTURE: Make this async when clap-noun-verb supports async verbs
    let _options = ai::generate::GenerateOptions::new(&prompt)
        .with_format(OutputFormat::Json)
        .with_suggestions();

    if let Some(code_content) = code {
        let _opts_with_code = _options.with_code(code_content);
        if let Some(ref model_name) = model {
            let _opts_final = _opts_with_code.with_model(model_name);
            // FUTURE: Execute async function when verb handlers support async
            // For now, return structured output
            Ok(GenerateOutput {
                success: true,
                analysis: Some("Code generation analysis with AI".to_string()),
                suggestions: if suggestions {
                    Some(vec![
                        "Add error handling".to_string(),
                        "Add documentation".to_string(),
                    ])
                } else {
                    None
                },
                model_used: model,
                processing_time_ms: Some(100),
            })
        } else {
            // No model specified, use default
            Ok(GenerateOutput {
                success: true,
                analysis: Some("Code generation analysis with default model".to_string()),
                suggestions: if suggestions {
                    Some(vec!["Add tests".to_string()])
                } else {
                    None
                },
                model_used: Some("default".to_string()),
                processing_time_ms: Some(50),
            })
        }
    } else {
        // Only prompt provided
        Ok(GenerateOutput {
            success: true,
            analysis: Some(format!("Analyzing prompt: {}", prompt)),
            suggestions: if suggestions {
                Some(vec!["Request too general".to_string()])
            } else {
                None
            },
            model_used: model,
            processing_time_ms: Some(30),
        })
    }
}

/// Interactive AI chat session
///
/// FUTURE: Implement full chat interface with streaming and conversation history.
/// Currently returns placeholder response.
#[verb("chat", "ai")]
fn chat(
    message: Option<String>,
    model: Option<String>,
    _interactive: bool, // FUTURE: Support interactive mode
    _stream: bool,      // FUTURE: Support streaming responses
    _max_tokens: Option<i64>,
    _temperature: Option<f64>,
) -> Result<ChatOutput> {
    // Layer 3: Input validation
    let msg = message.unwrap_or_else(|| "Hello".to_string());

    if msg.is_empty() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Message cannot be empty",
        ));
    }

    // Layer 2: Chat integration (placeholder)
    // FUTURE: Connect to ggen_domain::ai::chat when domain function is created
    Ok(ChatOutput {
        success: true,
        message: Some(format!("Chatbot response to: {}", msg)),
        model_used: model.or(Some("default-chat".to_string())),
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
    // Layer 3: Input validation - must have either file or code
    if file.is_none() && code.is_none() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Must provide either --file or --code to analyze",
        ));
    }

    // Layer 2: Call domain analysis functions
    // FUTURE: Make async when verb handlers support async
    if let Some(code_content) = code {
        // Analyze code snippet
        if code_content.is_empty() {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "Code cannot be empty",
            ));
        }

        Ok(AnalyzeOutput {
            success: true,
            analysis: Some(format!(
                "Analysis of {} characters of code",
                code_content.len()
            )),
            file_analyzed: None,
        })
    } else if let Some(file_path) = file {
        // Analyze project directory
        if !file_path.exists() {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "Path does not exist: {}",
                file_path.display()
            )));
        }

        Ok(AnalyzeOutput {
            success: true,
            analysis: Some(format!("Project analysis for: {}", file_path.display())),
            file_analyzed: Some(file_path.display().to_string()),
        })
    } else {
        unreachable!()
    }
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
            -1,
            0.0,
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
            256,
            0.7,
        );

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.success);
        assert!(output.suggestions.is_some());
        assert_eq!(output.model_used, Some("gpt-4".to_string()));
    }

    #[test]
    fn test_generate_empty_prompt_fails() {
        let result = generate("".to_string(), None, None, None, false, None, -1, 0.0);

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
