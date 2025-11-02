//! Sync CLI wrapper for AI generate command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic following the v2.0 architecture pattern.

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Arguments for the AI generate command
#[derive(Args, Debug, Clone)]
pub struct GenerateArgs {
    /// Code or prompt to analyze
    pub prompt: String,

    /// Optional file to analyze
    #[arg(short, long)]
    pub file: Option<PathBuf>,

    /// Output format (text, json, markdown)
    #[arg(short, long, default_value = "text")]
    pub format: String,

    /// Include code suggestions
    #[arg(short, long)]
    pub suggestions: bool,

    /// AI model to use
    #[arg(short, long)]
    pub model: Option<String>,
}

/// Execute the AI generate command (sync wrapper)
pub fn run(args: &GenerateArgs) -> Result<()> {
    crate::runtime::execute(async {
        // Build options
        let mut options = crate::domain::ai::GenerateOptions::new(&args.prompt);

        if let Some(ref file) = args.file {
            let code = std::fs::read_to_string(file).map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to read file: {}", e))
            })?;
            options = options.with_code(code);
        }

        if let Some(ref model) = args.model {
            options = options.with_model(model);
        }

        if args.suggestions {
            options = options.with_suggestions();
        }

        options = options.with_format(match args.format.as_str() {
            "json" => crate::domain::ai::OutputFormat::Json,
            "markdown" => crate::domain::ai::OutputFormat::Markdown,
            _ => crate::domain::ai::OutputFormat::Text,
        });

        // Generate
        let result = crate::domain::ai::generate_code(&options).await?;

        // Format and display
        let formatted = crate::domain::ai::format_result(&result, options.output_format);
        println!("{}", formatted);

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_args_defaults() {
        let args = GenerateArgs {
            prompt: "analyze this code".to_string(),
            file: None,
            format: "text".to_string(),
            suggestions: false,
            model: None,
        };
        assert_eq!(args.format, "text");
        assert!(!args.suggestions);
        assert!(args.model.is_none());
    }

    #[test]
    fn test_generate_args_with_file() {
        let args = GenerateArgs {
            prompt: "analyze".to_string(),
            file: Some(PathBuf::from("code.rs")),
            format: "json".to_string(),
            suggestions: true,
            model: Some("gpt-4".to_string()),
        };
        assert!(args.file.is_some());
        assert!(args.suggestions);
        assert_eq!(args.model, Some("gpt-4".to_string()));
    }
}
