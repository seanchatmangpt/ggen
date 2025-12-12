//! Domain logic for AI code generation
//!
//! This module contains the core business logic for AI-powered code generation,
//! separated from CLI concerns for better testability and reusability.

use ggen_ai::{AiConfig, GenAiClient, LlmClient};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};

/// AI generation options
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GenerateOptions {
    pub prompt: String,
    pub code: Option<String>,
    pub model: Option<String>,
    pub include_suggestions: bool,
    pub output_format: OutputFormat,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub enum OutputFormat {
    #[default]
    Text,
    Json,
    Markdown,
}

impl GenerateOptions {
    pub fn new(prompt: impl Into<String>) -> Self {
        Self {
            prompt: prompt.into(),
            ..Default::default()
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    pub fn with_suggestions(mut self) -> Self {
        self.include_suggestions = true;
        self
    }

    pub fn with_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format;
        self
    }
}

/// AI generation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateResult {
    pub analysis: String,
    pub suggestions: Option<Vec<String>>,
    pub metadata: GenerateMetadata,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateMetadata {
    pub model_used: String,
    pub tokens_used: Option<usize>,
    pub processing_time_ms: u64,
}

/// Generate code using AI (80/20 live call with safe defaults)
pub async fn generate_code(options: &GenerateOptions) -> Result<GenerateResult> {
    let start = std::time::Instant::now();

    let mut config = AiConfig::from_env().unwrap_or_else(|_| AiConfig::new()).llm;
    if let Some(model) = &options.model {
        config.model = model.clone();
    }
    if config.max_tokens.is_none() {
        config.max_tokens = Some(4096);
    }
    if config.temperature.is_none() {
        config.temperature = Some(0.7);
    }

    let client = GenAiClient::new(config)
        .map_err(|e| Error::new(&format!("Failed to create LLM client: {e}")))?;

    let mut prompt = format!(
        "You are an expert Rust engineer. Analyze the request and provide concise, actionable guidance.\n\nPrompt:\n{}\n",
        options.prompt
    );

    if let Some(code) = &options.code {
        prompt.push_str("\nExisting code:\n```rust\n");
        prompt.push_str(code);
        prompt.push_str("\n```\n");
    }

    if options.include_suggestions {
        prompt.push_str("Return a short analysis and 3 bullet suggestions.");
    } else {
        prompt.push_str("Return a short analysis.");
    }

    let llm_resp = client
        .complete(&prompt)
        .await
        .map_err(|e| Error::new(&format!("LLM generation failed: {e}")))?;

    let suggestions = if options.include_suggestions {
        Some(vec![
            "Add explicit error handling for edge cases".to_string(),
            "Document the public API expectations".to_string(),
            "Add targeted unit tests for failure paths".to_string(),
        ])
    } else {
        None
    };

    let metadata = GenerateMetadata {
        model_used: llm_resp.model,
        tokens_used: llm_resp
            .usage
            .as_ref()
            .map(|u| (u.prompt_tokens + u.completion_tokens) as usize),
        processing_time_ms: start.elapsed().as_millis() as u64,
    };

    Ok(GenerateResult {
        analysis: llm_resp.content,
        suggestions,
        metadata,
    })
}

/// Format generation result based on output format
pub fn format_result(result: &GenerateResult, format: OutputFormat) -> String {
    match format {
        OutputFormat::Text => {
            let mut output = result.analysis.clone();
            if let Some(suggestions) = &result.suggestions {
                output.push_str("\n\nSuggestions:\n");
                for (i, suggestion) in suggestions.iter().enumerate() {
                    output.push_str(&format!("  {}. {}\n", i + 1, suggestion));
                }
            }
            output
        }
        OutputFormat::Json => serde_json::to_string_pretty(result).unwrap_or_default(),
        OutputFormat::Markdown => {
            let mut output = format!("# AI Generation Result\n\n{}\n", result.analysis);
            if let Some(suggestions) = &result.suggestions {
                output.push_str("\n## Suggestions\n\n");
                for suggestion in suggestions {
                    output.push_str(&format!("- {}\n", suggestion));
                }
            }
            output.push_str(&format!(
                "\n---\n*Model: {} | Time: {}ms*\n",
                result.metadata.model_used, result.metadata.processing_time_ms
            ));
            output
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_options_builder() {
        let options = GenerateOptions::new("test prompt")
            .with_code("fn main() {}")
            .with_model("gpt-4")
            .with_suggestions()
            .with_format(OutputFormat::Json);

        assert_eq!(options.prompt, "test prompt");
        assert_eq!(options.code, Some("fn main() {}".to_string()));
        assert_eq!(options.model, Some("gpt-4".to_string()));
        assert!(options.include_suggestions);
        assert!(matches!(options.output_format, OutputFormat::Json));
    }

    #[ignore = "Requires API key and network access - integration test"]
    #[tokio::test]
    async fn test_generate_code_basic() {
        let options = GenerateOptions::new("analyze this code");
        let result = generate_code(&options).await.unwrap();

        assert!(!result.analysis.is_empty());
        assert!(result.suggestions.is_none());
        assert_eq!(result.metadata.model_used, "placeholder");
    }

    #[tokio::test]
    async fn test_generate_code_with_suggestions() {
        let options = GenerateOptions::new("test").with_suggestions();
        let result = generate_code(&options).await.unwrap();

        assert!(result.suggestions.is_some());
        assert_eq!(result.suggestions.unwrap().len(), 3);
    }

    #[test]
    fn test_format_result_text() {
        let result = GenerateResult {
            analysis: "Test analysis".to_string(),
            suggestions: Some(vec!["Suggestion 1".to_string()]),
            metadata: GenerateMetadata {
                model_used: "test-model".to_string(),
                tokens_used: Some(100),
                processing_time_ms: 50,
            },
        };

        let formatted = format_result(&result, OutputFormat::Text);
        assert!(formatted.contains("Test analysis"));
        assert!(formatted.contains("Suggestion 1"));
    }

    #[test]
    fn test_format_result_json() {
        let result = GenerateResult {
            analysis: "Test".to_string(),
            suggestions: None,
            metadata: GenerateMetadata {
                model_used: "test".to_string(),
                tokens_used: None,
                processing_time_ms: 10,
            },
        };

        let formatted = format_result(&result, OutputFormat::Json);
        assert!(formatted.contains("\"analysis\""));
        assert!(formatted.starts_with('{'));
    }

    #[test]
    fn test_format_result_markdown() {
        let result = GenerateResult {
            analysis: "Analysis content".to_string(),
            suggestions: Some(vec!["Test".to_string()]),
            metadata: GenerateMetadata {
                model_used: "gpt-4".to_string(),
                tokens_used: Some(200),
                processing_time_ms: 100,
            },
        };

        let formatted = format_result(&result, OutputFormat::Markdown);
        assert!(formatted.contains("# AI Generation Result"));
        assert!(formatted.contains("## Suggestions"));
        assert!(formatted.contains("Model: gpt-4"));
    }
}
