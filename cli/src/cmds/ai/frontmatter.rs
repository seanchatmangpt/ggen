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
}

pub async fn run(args: &FrontmatterArgs) -> Result<()> {
    println!("📋 Generating frontmatter with AI...");
    println!("Description: {}", args.description);

    // Use global config to auto-detect and create appropriate client
    let global_config = ggen_ai::get_global_config();
    let client = global_config.create_contextual_client()
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
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
