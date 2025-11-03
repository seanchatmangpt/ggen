//! Domain logic for AI code generation
//!
//! This module contains the core business logic for AI-powered code generation,
//! separated from CLI concerns for better testability and reusability.

use ggen_utils::error::Result;
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

/// Generate code using AI
///
/// This is a placeholder implementation for Phase 1.
/// Phase 2 will implement actual AI integration (OpenAI, Anthropic, local models).
pub async fn generate_code(options: &GenerateOptions) -> Result<GenerateResult> {
    let start = std::time::Instant::now();

    // Placeholder: Simulate AI analysis
    let analysis = format!(
        "Analysis of prompt: '{}'\n\nThis is a placeholder AI generation result. \
         Phase 2 will integrate real AI models (OpenAI, Anthropic, or local LLMs).",
        options.prompt
    );

    let suggestions = if options.include_suggestions {
        Some(vec![
            "Suggestion 1: Add error handling".to_string(),
            "Suggestion 2: Add documentation".to_string(),
            "Suggestion 3: Add unit tests".to_string(),
        ])
    } else {
        None
    };

    let metadata = GenerateMetadata {
        model_used: options.model.clone().unwrap_or_else(|| "placeholder".to_string()),
        tokens_used: Some(100),
        processing_time_ms: start.elapsed().as_millis() as u64,
    };

    Ok(GenerateResult {
        analysis,
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
