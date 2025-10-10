//! Generate templates using AI

use clap::Args;
use ggen_utils::error::Result;
use ggen_ai::{TemplateGenerator};
use ggen_ai::providers::{OllamaClient, OpenAIClient, AnthropicClient, MockClient};
use ggen_ai::client::LlmClient;
use ggen_ai::config::{OllamaConfig, OpenAIConfig, AnthropicConfig};
use std::fs;
use anyhow;

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

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,

    /// Enable iterative validation and improvement
    #[arg(long)]
    pub validate: bool,

    /// Maximum iterations for validation
    #[arg(long, default_value = "3")]
    pub max_iterations: usize,
}

pub async fn run(args: &GenerateArgs) -> Result<()> {
    println!("Generating template with AI...");

    // Initialize the AI client
    let client: Box<dyn LlmClient> = if args.openai {
        println!("Using OpenAI client");
        Box::new(OpenAIClient::new(OpenAIConfig::new("dummy-key"))
            .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?)
    } else if args.anthropic {
        println!("Using Anthropic client");
        Box::new(AnthropicClient::new(AnthropicConfig::new("dummy-key"))
            .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?)
    } else if args.ollama {
        println!("Using Ollama client");
        let mut config = OllamaConfig::new();
        config.default_model = Some("qwen3-coder:30b".to_string());
        Box::new(OllamaClient::new(config)
            .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?)
    } else {
        println!("Using mock client for demonstration");
        Box::new(MockClient::new(vec!["Generated template content".to_string()]))
    };

    let generator = TemplateGenerator::new(client);

    let template = if args.validate {
        println!("🔍 Enabling iterative validation with max {} iterations", args.max_iterations);
        
        // Create validator
        let validator = ggen_ai::TemplateValidator::new();
        
        // Generate and validate iteratively
        let mut current_template = generator.generate_template(
            &args.description,
            args.examples.iter().map(|s| s.as_str()).collect()
        ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
        
        for iteration in 0..args.max_iterations {
            println!("Iteration {}/{}: Validating...", iteration + 1, args.max_iterations);
            
            let result = validator.validate_template(&current_template)
                .await
                .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
            
            println!("  Quality score: {:.2}", result.quality_score);
            println!("  Issues found: {}", result.issues.len());
            
            if result.is_valid && result.quality_score >= 0.8 {
                println!("✅ Template meets quality threshold!");
                break;
            }
            
            if iteration < args.max_iterations - 1 {
                println!("🔄 Improving template based on feedback...");
                // Generate improved version based on issues
                let improvement_context = format!(
                    "{}\n\nPrevious issues:\n{}",
                    args.description,
                    result.issues.iter()
                        .map(|i| format!("- {}", i.description))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
                
                current_template = generator.generate_template(
                    &improvement_context,
                    args.examples.iter().map(|s| s.as_str()).collect()
                ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
            }
        }
        
        current_template
    } else {
        generator.generate_template(
            &args.description,
            args.examples.iter().map(|s| s.as_str()).collect()
        ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?
    };

    println!("Template generated successfully!");

    if let Some(output_path) = &args.output {
        fs::write(output_path, format!("{:?}\n---\n{}", template.front, template.body))?;
        println!("Saved to: {}", output_path);
    } else {
        println!("Generated template:");
        println!("{:?}", template.front);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}
