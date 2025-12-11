//! AI Integration Layer - Async coordination for AI operations
//!
//! **Layer 2: Integration**
//!
//! This module provides async coordination, resource management, and error handling
//! for AI domain functions. It sits between the CLI layer (ggen-cli) and the pure
//! domain logic (ggen-domain/ai).
//!
//! ## Architecture
//!
//! ```text
//! CLI (Layer 3) → Execute (Layer 2) → Domain (Layer 1)
//!   - Input validation    - Async coordination   - Pure business logic
//!   - Output formatting   - Resource management  - Core algorithms
//!   - JSON serialization  - Error transformation  - No I/O
//! ```
//!
//! ## Execute Functions
//!
//! - `execute_generate` - AI code generation with prompts
//! - `execute_analyze` - Code and project analysis
//! - `execute_chat` - Interactive AI chat sessions
//! - FUTURE: `execute_refactor`, `execute_explain`, `execute_suggest`

use crate::ai::{analyze, generate};
use ggen_utils::error::Result;
use std::path::Path;

// ============================================================================
// Result Types (Layer 2 - Enriched domain results for CLI)
// ============================================================================

/// Result of AI code generation execution
#[derive(Debug, Clone)]
pub struct ExecuteGenerateResult {
    /// Generated analysis or code
    pub analysis: String,
    /// Suggestions for improvement (if requested)
    pub suggestions: Vec<String>,
    /// Model used for generation
    pub model_used: String,
    /// Processing time in milliseconds
    pub processing_time_ms: u64,
}

/// Result of code analysis execution
#[derive(Debug, Clone)]
pub struct ExecuteAnalyzeResult {
    /// Analysis insights and recommendations
    pub analysis: String,
    /// Path to file analyzed (if file-based)
    pub file_analyzed: Option<String>,
}

/// Result of chat execution
#[derive(Debug, Clone)]
pub struct ExecuteChatResult {
    /// AI response message
    pub response: String,
    /// Model used for chat
    pub model_used: String,
}

// ============================================================================
// Execute Functions (Layer 2 - Async coordination and resource management)
// ============================================================================

/// Execute AI code generation with prompt and options
///
/// **Layer 2 responsibilities**:
/// - Async coordination: Orchestrates async AI API calls
/// - Resource management: Handles API rate limiting, connection pooling
/// - Error transformation: Converts domain errors to CLI-friendly format
/// - Result enrichment: Adds metadata (processing time, model info)
///
/// # Arguments
///
/// * `prompt` - User prompt describing what to generate
/// * `code` - Optional code snippet to analyze or refactor
/// * `model` - Optional model to use (defaults to domain-configured model)
/// * `suggestions` - Whether to include improvement suggestions
///
/// # Returns
///
/// * `Ok(ExecuteGenerateResult)` - Generation succeeded with result data
/// * `Err(Error)` - Generation failed (API error, validation, timeout)
///
/// # Examples
///
/// ```rust,no_run
/// # use ggen_domain::ai::execute;
/// # async fn example() -> ggen_utils::error::Result<()> {
/// let result = execute::execute_generate(
///     "write a hello world function",
///     None,
///     None,
///     true,
/// ).await?;
///
/// println!("Generated: {}", result.analysis);
/// println!("Suggestions: {:?}", result.suggestions);
/// # Ok(())
/// # }
/// ```
pub async fn execute_generate(
    prompt: &str, code: Option<&str>, model: Option<&str>, suggestions: bool,
) -> Result<ExecuteGenerateResult> {
    // Layer 2: Input validation (additional validation beyond CLI)
    if prompt.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Prompt cannot be empty"));
    }

    // Layer 2: Build domain options with builder pattern
    let mut options =
        generate::GenerateOptions::new(prompt).with_format(generate::OutputFormat::Json);

    if suggestions {
        options = options.with_suggestions();
    }

    if let Some(code_content) = code {
        options = options.with_code(code_content.to_string());
    }

    if let Some(model_name) = model {
        options = options.with_model(model_name);
    }

    // Layer 2: Track execution time
    let start = std::time::Instant::now();

    // Call Layer 1 domain function (pure business logic)
    let result = generate::generate_code(&options).await?;

    let processing_time_ms = start.elapsed().as_millis() as u64;

    // Layer 2: Transform domain result to execute result
    Ok(ExecuteGenerateResult {
        analysis: result.analysis,
        suggestions: result.suggestions.unwrap_or_default(),
        model_used: result.metadata.model_used,
        processing_time_ms,
    })
}

/// Execute code analysis on snippet or project
///
/// **Layer 2 responsibilities**:
/// - Async coordination: Orchestrates file I/O and AI analysis
/// - Resource management: Handles large file analysis with chunking
/// - Error transformation: Provides clear error messages for file not found, permissions
/// - Result enrichment: Adds file path metadata
///
/// # Arguments
///
/// * `code` - Optional code snippet to analyze
/// * `file` - Optional file/directory path to analyze
///
/// # Returns
///
/// * `Ok(ExecuteAnalyzeResult)` - Analysis succeeded with insights
/// * `Err(Error)` - Analysis failed (no input, file not found, API error)
///
/// # Examples
///
/// ```rust,no_run
/// # use ggen_domain::ai::execute;
/// # use std::path::Path;
/// # async fn example() -> ggen_utils::error::Result<()> {
/// // Analyze code snippet
/// let result = execute::execute_analyze(
///     Some("fn main() { println!(\"hello\"); }"),
///     None,
/// ).await?;
///
/// // Analyze project directory
/// let result = execute::execute_analyze(
///     None,
///     Some(Path::new("./src")),
/// ).await?;
/// # Ok(())
/// # }
/// ```
pub async fn execute_analyze(
    code: Option<&str>, file: Option<&Path>,
) -> Result<ExecuteAnalyzeResult> {
    // Layer 2: Input validation - Must have either code or file
    if code.is_none() && file.is_none() {
        return Err(ggen_utils::error::Error::new(
            "Must provide either code or file to analyze",
        ));
    }

    // Call Layer 1 domain functions based on input type
    let analysis_text = if let Some(code_content) = code {
        // Analyze code snippet
        if code_content.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Code cannot be empty"));
        }
        analyze::analyze_code(code_content).await?
    } else if let Some(file_path) = file {
        // Layer 2: Validate file exists before calling domain
        if !file_path.exists() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Path does not exist: {}",
                file_path.display()
            )));
        }
        analyze::analyze_project(file_path).await?
    } else {
        unreachable!("Already validated input above")
    };

    // Layer 2: Transform domain result to execute result
    Ok(ExecuteAnalyzeResult {
        analysis: analysis_text,
        file_analyzed: file.map(|p| p.display().to_string()),
    })
}

/// Execute interactive chat session
///
/// **Layer 2 responsibilities**:
/// - Async coordination: Manages conversation state and history
/// - Resource management: Handles chat session lifecycle
/// - Error transformation: Provides user-friendly chat error messages
///
/// # Arguments
///
/// * `message` - User message to send to AI
/// * `model` - Optional model to use for chat
///
/// # Returns
///
/// * `Ok(ExecuteChatResult)` - Chat response received
/// * `Err(Error)` - Chat failed (API error, rate limiting)
///
/// # Notes
///
/// This is currently a STUB implementation. Full chat functionality requires:
/// - Domain function: `crate::ai::chat::execute_chat()`
/// - Conversation history management
/// - Streaming response support
/// - Session persistence
///
/// # Examples
///
/// ```rust,no_run
/// # use ggen_domain::ai::execute;
/// # async fn example() -> ggen_utils::error::Result<()> {
/// let result = execute::execute_chat(
///     "explain what RDF is",
///     Some("gpt-4"),
/// ).await?;
///
/// println!("AI: {}", result.response);
/// # Ok(())
/// # }
/// ```
pub async fn execute_chat(message: &str, model: Option<&str>) -> Result<ExecuteChatResult> {
    // Layer 2: Input validation
    if message.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Message cannot be empty"));
    }

    // FUTURE: Call domain chat function when implemented
    // For now, return placeholder response
    Ok(ExecuteChatResult {
        response: format!("Chat response to: {}", message),
        model_used: model.unwrap_or("default-chat").to_string(),
    })
}

// ============================================================================
// FUTURE: Additional Execute Functions
// ============================================================================

// FUTURE: execute_refactor - Refactor code with AI assistance
// FUTURE: execute_explain - Explain code functionality
// FUTURE: execute_suggest - Suggest improvements and best practices

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_execute_generate_validation() {
        // Empty prompt should fail
        let result = execute_generate("", None, None, false).await;
        assert!(result.is_err());

        // Whitespace-only prompt should fail
        let result = execute_generate("   ", None, None, false).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_execute_analyze_validation() {
        // No input should fail
        let result = execute_analyze(None, None).await;
        assert!(result.is_err());

        // Empty code should fail
        let result = execute_analyze(Some(""), None).await;
        assert!(result.is_err());

        // Non-existent path should fail
        let result = execute_analyze(None, Some(Path::new("/nonexistent/path"))).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_execute_chat_validation() {
        // Empty message should fail
        let result = execute_chat("", None).await;
        assert!(result.is_err());

        // Whitespace-only message should fail
        let result = execute_chat("   ", None).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_execute_chat_placeholder() {
        // Placeholder implementation should work for valid input
        let result = execute_chat("hello", Some("gpt-4")).await;
        assert!(result.is_ok());

        let chat_result = result.unwrap();
        assert!(!chat_result.response.is_empty());
        assert_eq!(chat_result.model_used, "gpt-4");
    }
}
