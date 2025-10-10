//! AI-powered template generation commands

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
use anyhow;
use ggen_ai::{TemplateGenerator, SparqlGenerator, OntologyGenerator};
use ggen_ai::providers::{OpenAIClient, AnthropicClient, OllamaClient, MockClient};
use ggen_ai::client::LlmClient;
use ggen_ai::config::{OpenAIConfig, AnthropicConfig, OllamaConfig};
use std::fs;

#[derive(Debug, Args)]
pub struct AiArgs {
    #[command(subcommand)]
    pub command: AiCommand,
}

#[derive(Debug, Subcommand)]
pub enum AiCommand {
    /// Generate templates using AI
    Generate(GenerateArgs),
    /// Run the AI template demo
    Demo,
    /// Start the MCP server
    Server(ServerArgs),
    /// List available AI models
    Models(ModelsArgs),
    /// Validate templates
    Validate(ValidateArgs),
}

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
}

#[derive(Debug, Args)]
pub struct ServerArgs {
    /// Port to run the server on
    #[arg(short, long, default_value = "3000")]
    pub port: u16,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

#[derive(Debug, Args)]
pub struct ModelsArgs {
    /// Specific adapter to list models for
    #[arg(short, long)]
    pub adapter: Option<String>,
}

#[derive(Debug, Args)]
pub struct ValidateArgs {
    /// Template file to validate
    #[arg(short, long)]
    pub template: String,

    /// Strict validation mode
    #[arg(long)]
    pub strict: bool,
}

pub async fn run(args: AiArgs) -> Result<()> {
    match args.command {
        AiCommand::Generate(args) => run_generate(&args).await,
        AiCommand::Demo => run_demo().await,
        AiCommand::Server(args) => run_server(&args).await,
        AiCommand::Models(args) => run_models(&args).await,
        AiCommand::Validate(args) => run_validate(&args).await,
    }
}

async fn run_generate(args: &GenerateArgs) -> Result<()> {
    println!("Generating template with AI...");

    // Initialize the AI client
    let client: Box<dyn LlmClient> = if args.openai {
        println!("Using OpenAI client");
        Box::new(OpenAIClient::new(OpenAIConfig::new("dummy-key")).expect("Failed to create OpenAI client"))
    } else if args.anthropic {
        println!("Using Anthropic client");
        Box::new(AnthropicClient::new(AnthropicConfig::new("dummy-key")).expect("Failed to create Anthropic client"))
    } else if args.ollama {
        println!("Using Ollama client");
        let mut config = OllamaConfig::new();
        config.default_model = Some("qwen3-coder:30b".to_string());
        Box::new(OllamaClient::new(config).expect("Failed to create Ollama client"))
    } else {
        println!("Using mock client for demonstration");
        Box::new(MockClient::new(vec!["Generated template content".to_string()]))
    };

    let generator = TemplateGenerator::new(client);

    let template = generator.generate_template(
        &args.description,
        args.examples.iter().map(|s| s.as_str()).collect()
    ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

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

async fn run_demo() -> Result<()> {
    println!("Running AI template generation demo...");

    // Run the demo example
    let demo_result = std::process::Command::new("cargo")
        .args(&["run", "--example", "iterative_template_improvement"])
        .current_dir("/Users/sac/ggen")
        .output();

    match demo_result {
        Ok(output) => {
            if output.status.success() {
                println!("Demo completed successfully!");
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                println!("Demo failed:");
                println!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            println!("Failed to run demo: {}", e);
        }
    }

    Ok(())
}

async fn run_server(args: &ServerArgs) -> Result<()> {
    println!("Starting ggen-ai MCP server...");
    println!("Port: {}", args.port);

    if args.openai {
        println!("Using OpenAI client");
    } else if args.anthropic {
        println!("Using Anthropic client");
    } else if args.ollama {
        println!("Using Ollama client");
    } else {
        println!("No client specified, using default");
    }

    // For now, just print instructions
    println!("\nTo start the actual MCP server, run:");
    println!("cargo run --bin ggen-ai-mcp");

    if args.openai {
        println!("OPENAI_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.anthropic {
        println!("ANTHROPIC_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.ollama {
        println!("USE_OLLAMA=true cargo run --bin ggen-ai-mcp");
    }

    Ok(())
}

async fn run_models(args: &ModelsArgs) -> Result<()> {
    println!("Listing available AI models...");

    use genai::adapter::AdapterKind;
    use genai::Client;

    let client = Client::default();

    let adapters = if let Some(adapter_name) = &args.adapter {
        match adapter_name.as_str() {
            "openai" => vec![AdapterKind::OpenAI],
            "ollama" => vec![AdapterKind::Ollama],
            "anthropic" => vec![AdapterKind::Anthropic],
            "gemini" => vec![AdapterKind::Gemini],
            "groq" => vec![AdapterKind::Groq],
            "cohere" => vec![AdapterKind::Cohere],
            _ => {
                println!("Unknown adapter: {}. Available: openai, ollama, anthropic, gemini, groq, cohere", adapter_name);
                return Ok(());
            }
        }
    } else {
        vec![
            AdapterKind::OpenAI,
            AdapterKind::Ollama,
            AdapterKind::Anthropic,
            AdapterKind::Gemini,
            AdapterKind::Groq,
            AdapterKind::Cohere,
        ]
    };

    for adapter in adapters {
        println!("{}", format!("--- Models for {:?}", adapter));
        match client.all_model_names(adapter).await {
            Ok(models) => {
                for model in models {
                    println!("  - {}", model);
                }
            }
            Err(e) => {
                println!("  Error fetching models: {}", e);
            }
        }
    }

    Ok(())
}

async fn run_validate(args: &ValidateArgs) -> Result<()> {
    println!("Validating template: {}", args.template);

    // Load and parse template
    let content = fs::read_to_string(&args.template)?;
    let template = ggen_core::Template::parse(&content)?;

    // Create validator
    let validator = ggen_ai::TemplateValidator::new();

    // Validate template
    let result = validator.validate_template(&template).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("Validation Results:");
    println!("  Valid: {}", result.is_valid);
    println!("  Quality Score: {:.2}", result.quality_score);
    println!("  Issues Found: {}", result.issues.len());

    for issue in &result.issues {
        let severity = match issue.severity {
            ggen_ai::generators::validator::Severity::Error => "ERROR",
            ggen_ai::generators::validator::Severity::Warning => "WARNING",
            ggen_ai::generators::validator::Severity::Info => "INFO",
        };
        println!("  {}: {}", severity, issue.description);
    }

    if !result.suggestions.is_empty() {
        println!("\nSuggestions for improvement:");
        for suggestion in &result.suggestions {
            println!("  - {}", suggestion);
        }
    }

    Ok(())
}
