//! Generate template from existing source file

use anyhow;
use clap::Args;
use ggen_ai::{MockClient, TemplateGenerator};
use ggen_utils::error::Result;
use std::fs;
use std::sync::Arc;

use super::config::AiConfigArgs;

#[derive(Debug, Args)]
pub struct FromSourceArgs {
    /// Source file to analyze
    #[arg(short, long)]
    pub source_file: String,

    /// Output template file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Target language for the generated template
    #[arg(short, long, default_value = "rust")]
    pub language: String,

    /// Target framework
    #[arg(short, long)]
    pub framework: Option<String>,

    /// Include RDF metadata in the template
    #[arg(long)]
    pub include_rdf: bool,

    /// Base IRI for RDF metadata
    #[arg(long)]
    pub base_iri: Option<String>,

    /// Extract variables from the source code
    #[arg(long)]
    pub extract_variables: bool,

    /// AI model to use (e.g., gpt-4, claude-3-sonnet, qwen3-coder:30b)
    /// Defaults to environment variable GGEN_DEFAULT_MODEL or provider default
    #[arg(short, long)]
    pub model: Option<String>,

    /// Maximum tokens to generate
    #[arg(long, default_value = "4096")]
    pub max_tokens: u32,

    /// Temperature for sampling (0.0 to 2.0)
    #[arg(long, default_value = "0.7")]
    pub temperature: f32,

    /// Top-p for nucleus sampling (0.0 to 1.0)
    #[arg(long, default_value = "0.9")]
    pub top_p: f32,

    /// Stop sequences (comma-separated)
    #[arg(long)]
    pub stop: Option<String>,

    /// Provider-specific API key (overrides environment variables)
    #[arg(long)]
    pub api_key: Option<String>,

    /// Custom endpoint URL (for self-hosted models)
    #[arg(long)]
    pub endpoint: Option<String>,

    /// Use mock client for testing
    #[arg(long)]
    pub mock: bool,
}

pub async fn run(args: &FromSourceArgs) -> Result<()> {
    println!("📂 Generating template from source file...");
    println!("Source file: {}", args.source_file);
    println!("Language: {}", args.language);

    if let Some(framework) = &args.framework {
        println!("Framework: {}", framework);
    }

    println!("Include RDF: {}", args.include_rdf);
    println!("Extract variables: {}", args.extract_variables);

    let model_display = args.model.clone().unwrap_or_else(|| {
        ggen_ai::get_global_config()
            .get_default_config()
            .map(|c| c.model.clone())
            .unwrap_or_else(|| "default".to_string())
    });
    println!("AI Model: {}", model_display);

    // Validate AI configuration
    let ai_config = AiConfigArgs {
        model: args.model.clone(),
        max_tokens: args.max_tokens,
        temperature: args.temperature,
        top_p: args.top_p,
        stop: args.stop.clone(),
        api_key: args.api_key.clone(),
        endpoint: args.endpoint.clone(),
    };

    ai_config.validate()?;
    println!("Provider: {}", ai_config.provider_name());

    // Read the source file
    let source_content = fs::read_to_string(&args.source_file).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read source file: {}", e))
    })?;

    println!("📖 Read {} bytes from source file", source_content.len());

    let global_config = ggen_ai::get_global_config();

    let client = if args.mock || cfg!(test) || std::env::var("GGEN_TEST_MODE").is_ok() {
        // Use mock client for testing
        println!("ℹ️  Using mock client for testing");
        Arc::new(MockClient::with_response(
            "Generated template from source analysis",
        )) as Arc<dyn ggen_ai::client::LlmClient>
    } else {
        // Use contextual client (auto-detects provider)
        global_config
            .create_contextual_client()
            .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?
    };

    let generator = TemplateGenerator::with_client(client);

    // Generate template description based on source analysis
    let analysis_description = format!(
        "Analyze this {} source code and generate a ggen template that can recreate similar code structures:\n\n```{}\n{}\n```\n\nRequirements:\n- Extract key patterns and structures\n- Identify variables that should be templated\n- Generate appropriate frontmatter with variables\n- Create a reusable template",
        args.language,
        args.language,
        source_content
    );

    let mut examples = vec![
        format!("Source language: {}", args.language),
        "Extract common patterns".to_string(),
        "Identify templatable variables".to_string(),
    ];

    if let Some(framework) = &args.framework {
        examples.push(format!("Framework: {}", framework));
    }

    if args.extract_variables {
        examples.push("Extract variables from source code".to_string());
    }

    if args.include_rdf {
        examples.push("Include RDF metadata in frontmatter".to_string());
    }

    // Generate the template
    let template = generator
        .generate_template(
            &analysis_description,
            examples.iter().map(|s| s.as_str()).collect(),
        )
        .await
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("✅ Template generated successfully!");

    // Determine output path
    let output_path = args.output.as_ref().map(|s| s.as_str()).unwrap_or_else(|| {
            let source_name = std::path::Path::new(&args.source_file)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("generated");
            format!("{}.tmpl", source_name).leak()
        });

        // Write the template
        fs::write(
            output_path,
            format!("{:?}\n---\n{}", template.front, template.body),
        )?;
        println!("📁 Saved template to: {}", output_path);

        // Generate analysis report
        let analysis_report = format!(
            r#"# Source Analysis Report
    Source file: {}
    Language: {}
    Framework: {}
    Generated at: {}
    Generated by: ggen-ai

    ## Analysis Summary
    - Source file size: {} bytes
    - Language: {}
    - Template generated: {}
    - Variables extracted: {}
    - RDF metadata included: {}

    ## Generated Template
    The template has been saved to: {}

    ## Usage
    To use the generated template:
    1. Review the template structure
    2. Customize variables as needed
    3. Use with ggen: `ggen gen '{}' --var key=value`

    ## Next Steps
    - Review the generated template
    - Test with sample data
    - Customize for your specific needs
    - Add additional variables as needed
    "#,
            args.source_file,
            args.language,
            args.framework.as_deref().unwrap_or("None"),
            chrono::Utc::now().to_rfc3339(),
            source_content.len(),
            args.language,
            output_path,
            args.extract_variables,
            args.include_rdf,
            output_path,
            output_path
        );

        let report_path = format!("{}_analysis.md", output_path.replace(".tmpl", ""));
        fs::write(&report_path, analysis_report)?;
        println!("📋 Generated analysis report: {}", report_path);

        println!("✅ Source analysis completed successfully!");
        println!("📋 Summary:");
        println!("   • Source file: {}", args.source_file);
        println!("   • Language: {}", args.language);
        println!("   • Template: {}", output_path);
        println!("   • Analysis report: {}", report_path);

        Ok(())
}
