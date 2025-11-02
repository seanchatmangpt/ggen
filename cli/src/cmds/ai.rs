//! AI commands - clap-noun-verb auto-discovery
//!
//! This module defines the Args structs and command routing for AI operations,
//! delegating business logic to the domain layer.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;

/// AI command arguments
#[derive(Debug, Args)]
pub struct AiArgs {
    #[command(subcommand)]
    pub command: AiCommand,
}

/// AI subcommands
#[derive(Debug, Subcommand)]
pub enum AiCommand {
    /// Generate code with AI assistance
    Generate(GenerateArgs),
    /// Interactive AI chat session
    Chat(ChatArgs),
    /// Analyze code with AI insights
    Analyze(AnalyzeArgs),
}

/// Generate command arguments
#[derive(Debug, clap::Args)]
pub struct GenerateArgs {
    /// Prompt for code generation
    pub prompt: String,

    /// Existing code to base generation on
    #[arg(short = 'c', long)]
    pub code: Option<String>,

    /// AI model to use
    #[arg(short = 'm', long)]
    pub model: Option<String>,

    /// Include suggestions
    #[arg(short = 's', long)]
    pub suggestions: bool,

    /// Output format (text, json, markdown)
    #[arg(short = 'f', long, default_value = "text")]
    pub format: String,
}

/// Chat command arguments
#[derive(Debug, clap::Args)]
pub struct ChatArgs {
    /// Initial message or query
    pub message: Option<String>,

    /// AI model to use for chat
    #[arg(short = 'm', long)]
    pub model: Option<String>,

    /// Enable interactive mode
    #[arg(short = 'i', long)]
    pub interactive: bool,
}

/// Analyze command arguments
#[derive(Debug, clap::Args)]
pub struct AnalyzeArgs {
    /// Code to analyze (or file path with --file)
    pub code: Option<String>,

    /// Read code from file
    #[arg(short = 'f', long)]
    pub file: Option<std::path::PathBuf>,

    /// Project path to analyze
    #[arg(short = 'p', long)]
    pub project: Option<std::path::PathBuf>,

    /// AI model to use
    #[arg(short = 'm', long)]
    pub model: Option<String>,

    /// Output format (text, json, markdown)
    #[arg(long, default_value = "text")]
    pub format: String,
}

impl AiArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            AiCommand::Generate(args) => run_generate(args),
            AiCommand::Chat(args) => run_chat(args),
            AiCommand::Analyze(args) => run_analyze(args),
        }
    }
}

/// Run generate command
fn run_generate(args: &GenerateArgs) -> Result<()> {
    use crate::domain::ai;

    // Parse output format
    let format = match args.format.to_lowercase().as_str() {
        "json" => ai::OutputFormat::Json,
        "markdown" | "md" => ai::OutputFormat::Markdown,
        _ => ai::OutputFormat::Text,
    };

    // Build options
    let mut options = ai::GenerateOptions::new(&args.prompt).with_format(format);

    if let Some(code) = &args.code {
        options = options.with_code(code);
    }

    if let Some(model) = &args.model {
        options = options.with_model(model);
    }

    if args.suggestions {
        options = options.with_suggestions();
    }

    // Execute generation (sync wrapper for async domain logic)
    // Note: This is a sync command that calls async domain logic
    // The main runtime is already active, so we can't create a new one
    let result = tokio::task::block_in_place(|| {
        tokio::runtime::Handle::current().block_on(ai::generate_code(&options))
    })?;

    // Format and print output
    let output = ai::format_result(&result, format);
    println!("{}", output);

    Ok(())
}

/// Run chat command
fn run_chat(args: &ChatArgs) -> Result<()> {
    if args.interactive {
        println!("🤖 AI Chat - Interactive Mode");
        println!("Type 'exit' or 'quit' to end session\n");

        // Placeholder for interactive chat loop
        println!("🚧 Interactive chat coming in Phase 2");
        println!("Model: {}", args.model.as_deref().unwrap_or("default"));
    } else if let Some(message) = &args.message {
        println!("🤖 AI Chat Response:");
        println!("\n{}", message);
        println!("\n🚧 AI chat integration coming in Phase 2");
    } else {
        println!("Error: Provide a message or use --interactive for chat session");
    }

    Ok(())
}

/// Run analyze command
fn run_analyze(args: &AnalyzeArgs) -> Result<()> {
    use crate::domain::ai;

    // Determine what to analyze
    let code = if let Some(code_str) = &args.code {
        code_str.clone()
    } else if let Some(file_path) = &args.file {
        std::fs::read_to_string(file_path).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read file: {}", e))
        })?
    } else if args.project.is_some() {
        // Project analysis (sync wrapper for async domain logic)
        let project_path = args.project.as_ref().unwrap();
        let result = tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(ai::analyze_project(project_path))
        })?;
        println!("📊 Project Analysis:");
        println!("{}", result);
        return Ok(());
    } else {
        return Err(ggen_utils::error::Error::new(
            "Provide code, --file, or --project to analyze"
        ));
    };

    // Code analysis (sync wrapper for async domain logic)
    let result = tokio::task::block_in_place(|| {
        tokio::runtime::Handle::current().block_on(ai::analyze_code(&code))
    })?;
    println!("📊 Code Analysis:");
    println!("{}", result);

    Ok(())
}
