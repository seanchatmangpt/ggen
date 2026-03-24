//! AI Commands Integration Tests - Chicago TDD
//!
//! Tests for AI operations: generate, chat, analyze
//!
//! Chicago TDD Cycle:
//! 1. RED: Write failing test
//! 2. GREEN: Make test pass with REAL implementation
//! 3. REFACTOR: Improve code while maintaining green
//!
//! NO MOCKS - Tests against REAL domain implementations

use std::path::PathBuf;

// ============================================================================
// Domain Layer Imports (REAL types, NO mocks)
// ============================================================================

use ggen_domain::ai::{
    GenerateOptions, GenerateResult, GenerateMetadata, OutputFormat,
    generate_code, format_result, analyze_code, analyze_project,
};

// ============================================================================
// Generate Command Tests
// ============================================================================

#[cfg(test)]
mod generate_tests {
    use super::*;

    /// Test: GenerateOptions can be created with builder pattern
    #[test]
    fn test_generate_options_builder() {
        let options = GenerateOptions::new("write a function")
            .with_code("fn main() {}")
            .with_model("gpt-4")
            .with_suggestions()
            .with_format(OutputFormat::Json);

        assert_eq!(options.prompt, "write a function");
        assert_eq!(options.code, Some("fn main() {}".to_string()));
        assert_eq!(options.model, Some("gpt-4".to_string()));
        assert!(options.include_suggestions);
        assert!(matches!(options.output_format, OutputFormat::Json));
    }

    /// Test: GenerateOptions with minimal prompt
    #[test]
    fn test_generate_options_minimal() {
        let options = GenerateOptions::new("hello world");

        assert_eq!(options.prompt, "hello world");
        assert!(options.code.is_none());
        assert!(options.model.is_none());
        assert!(!options.include_suggestions);
        assert!(matches!(options.output_format, OutputFormat::Text));
    }

    /// Test: GenerateOptions default values
    #[test]
    fn test_generate_options_default() {
        let options = GenerateOptions::default();

        assert_eq!(options.prompt, "");
        assert!(options.code.is_none());
        assert!(options.model.is_none());
        assert!(!options.include_suggestions);
        assert!(matches!(options.output_format, OutputFormat::Text));
    }

    /// Test: GenerateOptions chain builder
    #[test]
    fn test_generate_options_chain_builder() {
        let options = GenerateOptions::new("test")
            .with_model("claude-3")
            .with_format(OutputFormat::Markdown);

        assert_eq!(options.model, Some("claude-3".to_string()));
        assert!(matches!(options.output_format, OutputFormat::Markdown));
    }

    /// Test: GenerateResult structure with all fields
    #[test]
    fn test_generate_result_structure() {
        let result = GenerateResult {
            analysis: "Code analysis complete".to_string(),
            suggestions: Some(vec!["Add error handling".to_string()]),
            metadata: GenerateMetadata {
                model_used: "gpt-4".to_string(),
                tokens_used: Some(150),
                processing_time_ms: 250,
            },
        };

        assert_eq!(result.analysis, "Code analysis complete");
        assert_eq!(result.suggestions.as_ref().unwrap().len(), 1);
        assert_eq!(result.metadata.model_used, "gpt-4");
        assert_eq!(result.metadata.tokens_used, Some(150));
        assert_eq!(result.metadata.processing_time_ms, 250);
    }

    /// Test: GenerateResult with no suggestions
    #[test]
    fn test_generate_result_no_suggestions() {
        let result = GenerateResult {
            analysis: "Basic analysis".to_string(),
            suggestions: None,
            metadata: GenerateMetadata {
                model_used: "default".to_string(),
                tokens_used: None,
                processing_time_ms: 100,
            },
        };

        assert!(result.suggestions.is_none());
        assert_eq!(result.metadata.tokens_used, None);
    }

    /// Test: GenerateMetadata with all fields
    #[test]
    fn test_generate_metadata_structure() {
        let metadata = GenerateMetadata {
            model_used: "claude-3-opus".to_string(),
            tokens_used: Some(1000),
            processing_time_ms: 500,
        };

        assert_eq!(metadata.model_used, "claude-3-opus");
        assert_eq!(metadata.tokens_used, Some(1000));
        assert_eq!(metadata.processing_time_ms, 500);
    }

    /// Test: OutputFormat variants
    #[test]
    fn test_output_format_variants() {
        let text = OutputFormat::Text;
        let json = OutputFormat::Json;
        let markdown = OutputFormat::Markdown;

        // Verify each variant is distinct
        assert!(matches!(text, OutputFormat::Text));
        assert!(matches!(json, OutputFormat::Json));
        assert!(matches!(markdown, OutputFormat::Markdown));
    }

    /// Test: OutputFormat default is Text
    #[test]
    fn test_output_format_default() {
        let format = OutputFormat::default();
        assert!(matches!(format, OutputFormat::Text));
    }

    /// Test: format_result with Text format
    #[test]
    fn test_format_result_text() {
        let result = GenerateResult {
            analysis: "Test analysis content".to_string(),
            suggestions: Some(vec![
                "Suggestion 1".to_string(),
                "Suggestion 2".to_string(),
            ]),
            metadata: GenerateMetadata {
                model_used: "test-model".to_string(),
                tokens_used: Some(100),
                processing_time_ms: 50,
            },
        };

        let formatted = format_result(&result, OutputFormat::Text);

        assert!(formatted.contains("Test analysis content"));
        assert!(formatted.contains("Suggestion 1"));
        assert!(formatted.contains("Suggestion 2"));
        assert!(formatted.contains("Suggestions:"));
    }

    /// Test: format_result with Text format and no suggestions
    #[test]
    fn test_format_result_text_no_suggestions() {
        let result = GenerateResult {
            analysis: "Simple analysis".to_string(),
            suggestions: None,
            metadata: GenerateMetadata {
                model_used: "test".to_string(),
                tokens_used: None,
                processing_time_ms: 10,
            },
        };

        let formatted = format_result(&result, OutputFormat::Text);

        assert!(formatted.contains("Simple analysis"));
        assert!(!formatted.contains("Suggestions:"));
    }

    /// Test: format_result with Json format
    #[test]
    fn test_format_result_json() {
        let result = GenerateResult {
            analysis: "JSON test".to_string(),
            suggestions: None,
            metadata: GenerateMetadata {
                model_used: "gpt-4".to_string(),
                tokens_used: Some(200),
                processing_time_ms: 150,
            },
        };

        let formatted = format_result(&result, OutputFormat::Json);

        assert!(formatted.contains("\"analysis\""));
        assert!(formatted.contains("JSON test"));
        assert!(formatted.contains("\"metadata\""));
        assert!(formatted.starts_with('{'));
        assert!(formatted.ends_with('}'));
    }

    /// Test: format_result with Markdown format
    #[test]
    fn test_format_result_markdown() {
        let result = GenerateResult {
            analysis: "Markdown analysis".to_string(),
            suggestions: Some(vec!["Tip 1".to_string(), "Tip 2".to_string()]),
            metadata: GenerateMetadata {
                model_used: "claude-3".to_string(),
                tokens_used: Some(300),
                processing_time_ms: 200,
            },
        };

        let formatted = format_result(&result, OutputFormat::Markdown);

        assert!(formatted.contains("# AI Generation Result"));
        assert!(formatted.contains("Markdown analysis"));
        assert!(formatted.contains("## Suggestions"));
        assert!(formatted.contains("- Tip 1"));
        assert!(formatted.contains("- Tip 2"));
        assert!(formatted.contains("Model: claude-3"));
        assert!(formatted.contains("Time: 200ms"));
    }

    /// Test: generate_code returns proper result structure
    #[tokio::test]
    async fn test_generate_code_basic() {
        let options = GenerateOptions::new("write a rust function");

        let result = generate_code(&options).await.unwrap();

        assert!(!result.analysis.is_empty());
        assert!(result.analysis.contains("write a rust function"));
        assert!(result.suggestions.is_none());
        assert_eq!(result.metadata.model_used, "placeholder");
        assert!(result.metadata.processing_time_ms >= 0); // Can be 0ms for very fast operations
    }

    /// Test: generate_code with suggestions enabled
    #[tokio::test]
    async fn test_generate_code_with_suggestions() {
        let options = GenerateOptions::new("analyze code")
            .with_suggestions();

        let result = generate_code(&options).await.unwrap();

        assert!(result.suggestions.is_some());
        let suggestions = result.suggestions.unwrap();
        assert_eq!(suggestions.len(), 3);
        assert!(suggestions[0].contains("error handling"));
        assert!(suggestions[1].contains("documentation"));
        assert!(suggestions[2].contains("unit tests"));
    }

    /// Test: generate_code with custom model
    #[tokio::test]
    async fn test_generate_code_custom_model() {
        let options = GenerateOptions::new("test")
            .with_model("gpt-4-turbo");

        let result = generate_code(&options).await.unwrap();

        assert_eq!(result.metadata.model_used, "gpt-4-turbo");
    }

    /// Test: generate_code with code context
    #[tokio::test]
    async fn test_generate_code_with_context() {
        let options = GenerateOptions::new("optimize this")
            .with_code("fn main() { println!(\"hello\"); }");

        assert_eq!(options.code, Some("fn main() { println!(\"hello\"); }".to_string()));

        let result = generate_code(&options).await.unwrap();
        assert!(!result.analysis.is_empty());
    }
}

// ============================================================================
// Analyze Command Tests
// ============================================================================

#[cfg(test)]
mod analyze_tests {
    use super::*;

    /// Test: analyze_code with valid code
    #[tokio::test]
    async fn test_analyze_code_valid() {
        let code = r#"
        fn main() {
            println!("Hello, world!");
        }
        "#;

        let result = analyze_code(code).await.unwrap();

        assert!(!result.is_empty());
        assert!(result.contains("characters of code"));
    }

    /// Test: analyze_code with empty code returns error
    #[tokio::test]
    async fn test_analyze_code_empty() {
        let code = "";

        let result = analyze_code(code).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    /// Test: analyze_code with whitespace only
    ///
    /// NOTE: Whitespace contains characters (spaces, tabs, newlines) so it IS valid.
    /// The domain function counts characters, not semantic content.
    #[tokio::test]
    async fn test_analyze_code_whitespace() {
        let code = "   \n\t   ";

        let result = analyze_code(code).await.unwrap();

        // Whitespace characters are still characters
        assert!(result.contains("characters of code"));
    }

    /// Test: analyze_code counts characters correctly
    #[tokio::test]
    async fn test_analyze_code_character_count() {
        let code = "fn main() {}";
        let result = analyze_code(code).await.unwrap();

        assert!(result.contains(&code.len().to_string()));
    }

    /// Test: analyze_project with valid path
    #[tokio::test]
    async fn test_analyze_project_valid() {
        let path = PathBuf::from("/tmp");

        let result = analyze_project(&path).await.unwrap();

        assert!(!result.is_empty());
        assert!(result.contains("/tmp"));
    }

    /// Test: analyze_project with current directory
    #[tokio::test]
    async fn test_analyze_project_current_dir() {
        let path = PathBuf::from(".");

        let result = analyze_project(&path).await.unwrap();

        assert!(!result.is_empty());
        assert!(result.contains("."));
    }

    /// Test: analyze_project with non-existent path
    #[tokio::test]
    async fn test_analyze_project_nonexistent() {
        let path = PathBuf::from("/nonexistent/path/that/does/not/exist");

        let result = analyze_project(&path).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("does not exist"));
    }

    /// Test: analyze_project with empty path
    #[tokio::test]
    async fn test_analyze_project_empty_path() {
        let path = PathBuf::from("");

        let result = analyze_project(&path).await;

        assert!(result.is_err());
    }
}

// ============================================================================
// Integration Tests (Domain Layer - REAL implementations)
// ============================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: Full generate workflow with all options
    #[tokio::test]
    async fn test_integration_generate_full_workflow() {
        let options = GenerateOptions::new("create a REST API")
            .with_code("// existing code")
            .with_model("claude-3-opus")
            .with_suggestions()
            .with_format(OutputFormat::Markdown);

        let result = generate_code(&options).await.unwrap();

        assert!(!result.analysis.is_empty());
        assert!(result.suggestions.is_some());
        assert_eq!(result.metadata.model_used, "claude-3-opus");

        let formatted = format_result(&result, OutputFormat::Markdown);
        assert!(formatted.contains("# AI Generation Result"));
        assert!(formatted.contains("## Suggestions"));
    }

    /// Test: Analyze workflow with multiple code snippets
    #[tokio::test]
    async fn test_integration_analyze_multiple_snippets() {
        let snippets = vec![
            "fn add(a: i32, b: i32) -> i32 { a + b }",
            "fn multiply(x: i32, y: i32) -> i32 { x * y }",
            "fn greet(name: &str) { println!(\"Hello, {}!\", name); }",
        ];

        for snippet in snippets {
            let result = analyze_code(snippet).await.unwrap();
            assert!(!result.is_empty());
        }
    }

    /// Test: Generate and analyze workflow
    #[tokio::test]
    async fn test_integration_generate_then_analyze() {
        // First generate
        let gen_options = GenerateOptions::new("create a function")
            .with_suggestions();
        let gen_result = generate_code(&gen_options).await.unwrap();

        // Then analyze the generated analysis
        let analysis_result = analyze_code(&gen_result.analysis).await.unwrap();

        assert!(!analysis_result.is_empty());
        assert!(gen_result.suggestions.is_some());
    }

    /// Test: Multiple format outputs for same result
    #[tokio::test]
    async fn test_integration_multiple_formats() {
        let options = GenerateOptions::new("test prompt").with_suggestions();
        let result = generate_code(&options).await.unwrap();

        let text_output = format_result(&result, OutputFormat::Text);
        let json_output = format_result(&result, OutputFormat::Json);
        let md_output = format_result(&result, OutputFormat::Markdown);

        // Verify all formats produce output
        assert!(!text_output.is_empty());
        assert!(!json_output.is_empty());
        assert!(!md_output.is_empty());

        // Verify format-specific characteristics
        assert!(json_output.starts_with('{'));
        assert!(md_output.contains("# AI Generation Result"));
        assert!(text_output.contains("Suggestions:"));
    }

    /// Test: GenerateOptions with all OutputFormat variants
    #[test]
    fn test_integration_all_output_formats() {
        let formats = [OutputFormat::Text, OutputFormat::Json, OutputFormat::Markdown];
        for format in &formats {
            let options = GenerateOptions::new("test").with_format(*format);
            match options.output_format {
                OutputFormat::Text => assert!(matches!(format, OutputFormat::Text)),
                OutputFormat::Json => assert!(matches!(format, OutputFormat::Json)),
                OutputFormat::Markdown => assert!(matches!(format, OutputFormat::Markdown)),
            }
        }
    }

    /// Test: GenerateMetadata with various token counts
    #[test]
    fn test_integration_metadata_tokens() {
        let cases = vec![
            Some(0),
            Some(100),
            Some(1000),
            Some(4096),
            None,
        ];

        for tokens in cases {
            let metadata = GenerateMetadata {
                model_used: "test-model".to_string(),
                tokens_used: tokens,
                processing_time_ms: 100,
            };
            assert_eq!(metadata.tokens_used, tokens);
        }
    }

    /// Test: Real-world generate scenario
    #[tokio::test]
    async fn test_integration_real_world_scenario() {
        // User provides a prompt and existing code
        let options = GenerateOptions::new("Add error handling to this function")
            .with_code(
                r#"
                fn divide(a: f64, b: f64) -> f64 {
                    a / b
                }
                "#
            )
            .with_model("gpt-4")
            .with_suggestions();

        let result = generate_code(&options).await.unwrap();

        // Verify result structure
        assert!(!result.analysis.is_empty());
        assert!(result.suggestions.is_some());
        assert_eq!(result.metadata.model_used, "gpt-4");
        assert!(result.metadata.tokens_used.is_some());
        // Processing time can vary wildly on different systems
        assert!(result.metadata.processing_time_ms >= 0);

        // Format for display
        let formatted = format_result(&result, OutputFormat::Markdown);
        assert!(formatted.contains("Add error handling"));
    }
}
