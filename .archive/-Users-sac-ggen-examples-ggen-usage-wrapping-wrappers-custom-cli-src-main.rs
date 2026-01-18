//! Custom CLI wrapper for ggen
//!
//! A feature-rich command-line interface demonstrating how to wrap ggen
//! with custom commands, interactive prompts, and enhanced user experience.

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use colored::*;
use dialoguer::{Confirm, Input, MultiSelect, Select};
use ggen_ai::{GenAiClient, LlmConfig, LlmProvider, TemplateGenerator};
use ggen_core::{GenContext, Generator, Template};
use indicatif::{ProgressBar, ProgressStyle};
use prettytable::{Cell, Row, Table};
use std::collections::HashMap;
use std::path::PathBuf;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

#[derive(Parser)]
#[command(name = "ggencli")]
#[command(about = "Custom CLI wrapper for ggen code generation", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Configuration file
    #[arg(short, long, global = true)]
    config: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate code from a template
    Generate {
        /// Template file path
        #[arg(short, long)]
        template: PathBuf,

        /// Output directory
        #[arg(short, long)]
        output: PathBuf,

        /// Context variables (key=value)
        #[arg(short = 'v', long = "var")]
        variables: Vec<String>,

        /// Context file (JSON/YAML)
        #[arg(short = 'f', long)]
        context_file: Option<PathBuf>,
    },

    /// Interactive generation mode
    Interactive,

    /// AI-powered template generation
    Ai {
        /// Template description
        #[arg(short, long)]
        description: String,

        /// Additional requirements
        #[arg(short, long)]
        requirements: Vec<String>,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Validate a template
    Validate {
        /// Template file path
        template: PathBuf,

        /// Strict mode
        #[arg(short, long)]
        strict: bool,
    },

    /// List available templates
    List {
        /// Show detailed information
        #[arg(short, long)]
        detailed: bool,
    },

    /// Initialize a new project
    Init {
        /// Project name
        #[arg(short, long)]
        name: String,

        /// Project type
        #[arg(short = 't', long)]
        project_type: Option<String>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let log_level = if cli.verbose { Level::DEBUG } else { Level::INFO };
    let subscriber = FmtSubscriber::builder()
        .with_max_level(log_level)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    // Print banner
    print_banner();

    // Execute command
    match cli.command {
        Commands::Generate {
            template,
            output,
            variables,
            context_file,
        } => {
            generate_command(template, output, variables, context_file).await?;
        }
        Commands::Interactive => {
            interactive_mode().await?;
        }
        Commands::Ai {
            description,
            requirements,
            output,
        } => {
            ai_command(description, requirements, output).await?;
        }
        Commands::Validate { template, strict } => {
            validate_command(template, strict).await?;
        }
        Commands::List { detailed } => {
            list_command(detailed).await?;
        }
        Commands::Init { name, project_type } => {
            init_command(name, project_type).await?;
        }
    }

    Ok(())
}

/// Print welcome banner
fn print_banner() {
    println!("{}", "╔═══════════════════════════════════════╗".bright_cyan());
    println!(
        "{}",
        "║   ggen Custom CLI - Code Generator   ║".bright_cyan()
    );
    println!("{}", "╚═══════════════════════════════════════╝".bright_cyan());
    println!();
}

/// Generate code from template
async fn generate_command(
    template_path: PathBuf,
    output_path: PathBuf,
    variables: Vec<String>,
    context_file: Option<PathBuf>,
) -> Result<()> {
    println!("{}", "Starting generation...".bright_green().bold());

    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );

    // Load template
    pb.set_message("Loading template...");
    let template = Template::from_file(&template_path)
        .context("Failed to load template")?;

    println!("✓ Template loaded: {}", template.metadata.name.bright_yellow());

    // Create context
    pb.set_message("Building context...");
    let mut context = GenContext::new();

    // Add variables from command line
    for var in variables {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() == 2 {
            context.insert(parts[0], parts[1]);
        }
    }

    // Load context from file if provided
    if let Some(ctx_file) = context_file {
        let content = std::fs::read_to_string(&ctx_file)?;
        let vars: HashMap<String, String> = if ctx_file.extension().unwrap_or_default() == "json" {
            serde_json::from_str(&content)?
        } else {
            serde_yaml::from_str(&content)?
        };

        for (key, value) in vars {
            context.insert(&key, &value);
        }
    }

    // Generate
    pb.set_message("Generating code...");
    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    // Write output
    pb.set_message("Writing output...");
    std::fs::create_dir_all(&output_path)?;
    let output_file = output_path.join(format!("{}.generated", template.metadata.name));
    std::fs::write(&output_file, output)?;

    pb.finish_with_message("✓ Generation complete!".to_string());

    println!("\n{}", "✓ Code generated successfully!".bright_green().bold());
    println!("  Output: {}", output_file.display().to_string().bright_yellow());

    Ok(())
}

/// Interactive generation mode
async fn interactive_mode() -> Result<()> {
    println!("{}", "Interactive Generation Mode".bright_blue().bold());
    println!();

    // Select template source
    let template_source = Select::new()
        .with_prompt("Template source")
        .items(&["Load from file", "Enter inline", "Generate with AI"])
        .default(0)
        .interact()?;

    let template = match template_source {
        0 => {
            // Load from file
            let path: String = Input::new()
                .with_prompt("Template file path")
                .interact_text()?;
            Template::from_file(path)?
        }
        1 => {
            // Enter inline
            println!("Enter template content (end with Ctrl+D):");
            let mut content = String::new();
            std::io::Read::read_to_string(&mut std::io::stdin(), &mut content)?;
            Template::from_str(&content)?
        }
        2 => {
            // AI generation
            let description: String = Input::new()
                .with_prompt("Describe what you want to generate")
                .interact_text()?;

            let add_requirements = Confirm::new()
                .with_prompt("Add specific requirements?")
                .default(false)
                .interact()?;

            let requirements = if add_requirements {
                let req_input: String = Input::new()
                    .with_prompt("Enter requirements (comma-separated)")
                    .interact_text()?;
                req_input.split(',').map(|s| s.trim().to_string()).collect()
            } else {
                Vec::new()
            };

            generate_with_ai(&description, requirements).await?
        }
        _ => unreachable!(),
    };

    // Collect context variables
    let mut context = GenContext::new();
    println!("\n{}", "Context Variables:".bright_blue());

    loop {
        let add_var = Confirm::new()
            .with_prompt("Add a variable?")
            .default(true)
            .interact()?;

        if !add_var {
            break;
        }

        let key: String = Input::new().with_prompt("Variable name").interact_text()?;

        let value: String = Input::new().with_prompt("Variable value").interact_text()?;

        context.insert(&key, &value);
        println!("  ✓ Added: {} = {}", key.bright_yellow(), value.bright_cyan());
    }

    // Generate
    println!("\n{}", "Generating...".bright_green());
    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    // Display output
    println!("\n{}", "Generated Output:".bright_blue().bold());
    println!("{}", "─".repeat(80).bright_black());
    println!("{}", output);
    println!("{}", "─".repeat(80).bright_black());

    // Save output
    let save = Confirm::new()
        .with_prompt("Save to file?")
        .default(true)
        .interact()?;

    if save {
        let output_path: String = Input::new()
            .with_prompt("Output file path")
            .default("output.txt".to_string())
            .interact_text()?;

        std::fs::write(&output_path, output)?;
        println!("✓ Saved to: {}", output_path.bright_yellow());
    }

    Ok(())
}

/// AI-powered template generation
async fn ai_command(
    description: String,
    requirements: Vec<String>,
    output: Option<PathBuf>,
) -> Result<()> {
    println!("{}", "AI Template Generation".bright_green().bold());
    println!("Description: {}", description.bright_yellow());
    if !requirements.is_empty() {
        println!("Requirements:");
        for req in &requirements {
            println!("  - {}", req);
        }
    }
    println!();

    let template = generate_with_ai(&description, requirements).await?;

    // Display template
    println!("\n{}", "Generated Template:".bright_blue().bold());
    println!("{}", "─".repeat(80).bright_black());
    println!("{}", template.content);
    println!("{}", "─".repeat(80).bright_black());

    // Save if output specified
    if let Some(path) = output {
        std::fs::write(&path, &template.content)?;
        println!("\n✓ Template saved to: {}", path.display().to_string().bright_yellow());
    }

    Ok(())
}

/// Helper to generate template with AI
async fn generate_with_ai(description: &str, requirements: Vec<String>) -> Result<Template> {
    let api_key = std::env::var("OPENAI_API_KEY")
        .or_else(|_| std::env::var("ANTHROPIC_API_KEY"))
        .context("No API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY")?;

    let config = LlmConfig {
        provider: LlmProvider::OpenAI,
        model: "gpt-4o".to_string(),
        api_key,
        ..Default::default()
    };

    let client = GenAiClient::with_config(config)?;
    let generator = TemplateGenerator::new(Box::new(client));

    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );
    pb.set_message("Generating template with AI...");

    let template = generator
        .generate_template(description, requirements.iter().map(|s| s.as_str()).collect())
        .await?;

    pb.finish_with_message("✓ Template generated!".to_string());

    Ok(template)
}

/// Validate a template
async fn validate_command(template_path: PathBuf, strict: bool) -> Result<()> {
    println!("{}", "Template Validation".bright_blue().bold());

    let template = Template::from_file(&template_path)?;

    println!("✓ Template parsed successfully");
    println!("  Name: {}", template.metadata.name.bright_yellow());
    if let Some(desc) = &template.metadata.description {
        println!("  Description: {}", desc);
    }

    // Additional validation in strict mode
    if strict {
        println!("\n{}", "Running strict validation...".bright_yellow());

        let mut issues = Vec::new();

        // Check metadata
        if template.metadata.description.is_none() {
            issues.push("Missing description");
        }
        if template.metadata.version.is_none() {
            issues.push("Missing version");
        }

        // Check content
        if template.content.trim().is_empty() {
            issues.push("Empty template content");
        }

        if issues.is_empty() {
            println!("✓ {}", "No issues found".bright_green());
        } else {
            println!("{}", "Issues found:".bright_red());
            for issue in issues {
                println!("  ✗ {}", issue);
            }
        }
    }

    Ok(())
}

/// List available templates
async fn list_command(detailed: bool) -> Result<()> {
    println!("{}", "Available Templates".bright_blue().bold());

    // In a real implementation, this would scan a templates directory
    let sample_templates = vec![
        ("rust-project", "Rust project structure", "1.0.0"),
        ("react-component", "React component", "1.2.0"),
        ("api-endpoint", "REST API endpoint", "1.1.0"),
    ];

    if detailed {
        let mut table = Table::new();
        table.add_row(Row::new(vec![
            Cell::new("Name").style_spec("Fb"),
            Cell::new("Description").style_spec("Fb"),
            Cell::new("Version").style_spec("Fb"),
        ]));

        for (name, desc, version) in sample_templates {
            table.add_row(Row::new(vec![
                Cell::new(name),
                Cell::new(desc),
                Cell::new(version),
            ]));
        }

        table.printstd();
    } else {
        for (name, _, _) in sample_templates {
            println!("  • {}", name.bright_yellow());
        }
    }

    Ok(())
}

/// Initialize a new project
async fn init_command(name: String, project_type: Option<String>) -> Result<()> {
    println!("{}", "Project Initialization".bright_green().bold());
    println!("Project name: {}", name.bright_yellow());

    let proj_type = if let Some(t) = project_type {
        t
    } else {
        let types = vec!["Rust library", "Rust binary", "Web app", "CLI tool"];
        let selection = Select::new()
            .with_prompt("Project type")
            .items(&types)
            .default(0)
            .interact()?;
        types[selection].to_string()
    };

    println!("Project type: {}", proj_type.bright_cyan());

    // Create project structure
    let project_dir = PathBuf::from(&name);
    std::fs::create_dir_all(&project_dir)?;
    std::fs::create_dir_all(project_dir.join("src"))?;

    println!("\n✓ {}", "Project initialized!".bright_green().bold());
    println!("  Directory: {}", project_dir.display());

    Ok(())
}
