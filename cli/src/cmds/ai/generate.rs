//! Generate templates using AI

use anyhow;
use clap::Args;
use ggen_ai::client::LlmClient;
use ggen_ai::{client::GenAiClient, LlmConfig, MockClient, TemplateGenerator};
use ggen_utils::error::Result;
use std::fs;
use std::sync::Arc;

#[derive(Debug, Args)]
pub struct GenerateArgs {
    /// Description of what to generate
    #[arg(short, long)]
    pub description: String,

    /// Examples or requirements
    #[arg(short, long)]
    pub examples: Vec<String>,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Enable iterative validation and improvement
    #[arg(long)]
    pub validate: bool,

    /// Maximum iterations for validation
    #[arg(long, default_value = "3")]
    pub max_iterations: usize,

    /// Use mock client for testing
    #[arg(long)]
    pub mock: bool,

    /// LLM provider to use
    #[arg(long, default_value = "mock")]
    pub llm_provider: String,

    /// Model name to use
    #[arg(long)]
    pub model: Option<String>,

    /// Temperature for generation
    #[arg(long)]
    pub temperature: Option<f32>,

    /// Maximum tokens to generate
    #[arg(long)]
    pub max_tokens: Option<u32>,
}

pub async fn run(args: &GenerateArgs) -> Result<()> {
    println!("🔧 Generating template with AI...");

    // Use global config for proper provider detection
    let global_config = ggen_ai::get_global_config();

    let client: Arc<dyn LlmClient> = if args.mock || args.llm_provider == "mock" {
        println!("ℹ️  Using mock client for testing");
        Arc::new(MockClient::with_response("Generated template content"))
    } else {
        println!("ℹ️  Using {} provider", global_config.provider_name());

        // Create client with proper configuration
        if let Some(model) = &args.model {
            // Use custom model if specified
            let llm_config = LlmConfig {
                model: model.clone(),
                max_tokens: args.max_tokens,
                temperature: args.temperature,
                top_p: Some(0.9),
                stop: None,
                extra: std::collections::HashMap::new(),
            };
            Arc::new(
                GenAiClient::new(llm_config)
                    .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?,
            )
        } else {
            // Use contextual client with auto-detection
            global_config
                .create_contextual_client()
                .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?
        }
    };

    let generator = TemplateGenerator::with_client(client);

    let template = if args.validate {
        println!(
            "🔍 Enabling iterative validation with max {} iterations",
            args.max_iterations
        );

        // Create validator
        let validator = ggen_ai::TemplateValidator::new();

        // Generate and validate iteratively with proper error handling
        let mut current_template = generator
            .generate_template(
                &args.description,
                args.examples.iter().map(|s| s.as_str()).collect(),
            )
            .await
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to generate initial template: {}",
                    e
                ))
            })?;

        for iteration in 0..args.max_iterations {
            println!(
                "📊 Iteration {}/{}: Validating...",
                iteration + 1,
                args.max_iterations
            );

            let result = validator
                .validate_template(&current_template)
                .await
                .map_err(|e| ggen_utils::error::Error::new(&format!("Validation failed: {}", e)))?;

            println!("  ℹ️  Issues found: {}", result.issues.len());

            if result.valid && result.issues.is_empty() {
                println!("✅ Template validation passed!");
                break;
            }

            if iteration < args.max_iterations - 1 {
                println!("🔄 Improving template based on feedback...");

                // Generate improved version based on issues
                let improvement_context = format!(
                    "{}\n\nPrevious issues:\n{}",
                    args.description,
                    result
                        .issues
                        .iter()
                        .map(|i| format!("- {}", i.message))
                        .collect::<Vec<_>>()
                        .join("\n")
                );

                current_template = generator
                    .generate_template(
                        &improvement_context,
                        args.examples.iter().map(|s| s.as_str()).collect(),
                    )
                    .await
                    .map_err(|e| {
                        ggen_utils::error::Error::new(&format!("Failed to improve template: {}", e))
                    })?;
            } else {
                println!(
                    "⚠️  Max iterations reached with {} remaining issues",
                    result.issues.len()
                );
            }
        }

        current_template
    } else {
        generator
            .generate_template(
                &args.description,
                args.examples.iter().map(|s| s.as_str()).collect(),
            )
            .await
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Template generation failed: {}", e))
            })?
    };

    println!("✅ Template generated successfully!");

    if let Some(output_path) = &args.output {
        fs::write(
            output_path,
            format!("{:?}\n---\n{}", template.front, template.body),
        )?;
        println!("Saved to: {}", output_path);
    } else {
        println!("Generated template:");
        println!("{:?}", template.front);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}
