//! Generate frontmatter using AI

use anyhow;
use clap::Args;
use ggen_ai::TemplateGenerator;
use ggen_utils::error::Result;
use std::fs;

#[derive(Debug, Args)]
pub struct FrontmatterArgs {
    /// Description of the frontmatter to generate
    #[arg(short, long)]
    pub description: String,

    /// Examples or requirements
    #[arg(short, long)]
    pub examples: Vec<String>,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Use mock client for testing
    #[arg(long)]
    pub mock: bool,

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

pub async fn run(args: &FrontmatterArgs) -> Result<()> {
    println!("📋 Generating frontmatter with AI...");
    println!("Description: {}", args.description);

    // Use global config for proper provider detection
    let global_config = ggen_ai::get_global_config();

    use ggen_ai::client::{GenAiClient, LlmClient};
    use ggen_ai::{LlmConfig, MockClient};
    use std::sync::Arc;

    let client: Arc<dyn LlmClient> = if args.mock {
        println!("ℹ️  Using mock client for testing");
        Arc::new(MockClient::with_response("Generated frontmatter content"))
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

    // Generate template with frontmatter
    let template = generator
        .generate_template(
            &args.description,
            args.examples.iter().map(|s| s.as_str()).collect(),
        )
        .await
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("✅ Frontmatter generated successfully!");

    if let Some(output_path) = &args.output {
        // Write frontmatter in YAML format
        let frontmatter_yaml = serde_yaml::to_string(&template.front).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e))
        })?;

        fs::write(
            output_path,
            format!("{}\n---\n{}", frontmatter_yaml, template.body),
        )?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated frontmatter:");
        let frontmatter_yaml = serde_yaml::to_string(&template.front).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e))
        })?;
        println!("{}", frontmatter_yaml);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}
