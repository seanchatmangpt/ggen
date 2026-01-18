use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::*;
use std::path::PathBuf;
use tracing::{info, warn};

mod architect;
mod generator;
mod validator;
mod deployer;

use architect::Architect;
use generator::Generator;
use validator::Validator;
use deployer::Deployer;

#[derive(Parser)]
#[command(name = "fullstack-generator")]
#[command(about = "AI-powered full-stack blog platform generator", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose logging
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Design the system architecture using AI
    Design {
        /// Project name
        #[arg(short, long)]
        name: String,

        /// Target platform (web, mobile, desktop)
        #[arg(short, long, default_value = "web")]
        platform: String,

        /// Output directory
        #[arg(short, long, default_value = "./design")]
        output: PathBuf,
    },

    /// Generate code from architecture
    Generate {
        /// Design file
        #[arg(short, long)]
        design: PathBuf,

        /// Output directory
        #[arg(short, long, default_value = "./generated")]
        output: PathBuf,

        /// Enable caching
        #[arg(short, long)]
        cache: bool,
    },

    /// Validate generated code using SPARQL
    Validate {
        /// Generated code directory
        #[arg(short, long)]
        input: PathBuf,

        /// Schema directory
        #[arg(short, long, default_value = "./schemas")]
        schemas: PathBuf,
    },

    /// Deploy the generated application
    Deploy {
        /// Generated code directory
        #[arg(short, long)]
        input: PathBuf,

        /// Deployment target (local, docker, k8s)
        #[arg(short, long, default_value = "local")]
        target: String,
    },

    /// Run complete lifecycle (design → generate → validate → deploy)
    Full {
        /// Project name
        #[arg(short, long)]
        name: String,

        /// Platform
        #[arg(short = 'p', long, default_value = "web")]
        platform: String,

        /// Output directory
        #[arg(short, long, default_value = "./output")]
        output: PathBuf,

        /// Deployment target
        #[arg(short, long, default_value = "local")]
        target: String,

        /// Enable caching
        #[arg(short, long)]
        cache: bool,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing
    let filter = if cli.verbose {
        "debug"
    } else {
        "info"
    };
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .init();

    match cli.command {
        Commands::Design { name, platform, output } => {
            run_design(name, platform, output).await?;
        }
        Commands::Generate { design, output, cache } => {
            run_generate(design, output, cache).await?;
        }
        Commands::Validate { input, schemas } => {
            run_validate(input, schemas).await?;
        }
        Commands::Deploy { input, target } => {
            run_deploy(input, target).await?;
        }
        Commands::Full { name, platform, output, target, cache } => {
            run_full_lifecycle(name, platform, output, target, cache).await?;
        }
    }

    Ok(())
}

async fn run_design(name: String, platform: String, output: PathBuf) -> Result<()> {
    println!("{}", "🏗️  Designing system architecture...".bright_blue().bold());

    let architect = Architect::new()?;
    let design = architect.design_system(&name, &platform).await?;

    std::fs::create_dir_all(&output)?;
    let design_path = output.join("architecture.json");
    std::fs::write(&design_path, serde_json::to_string_pretty(&design)?)?;

    println!("{}", "✅ Architecture design complete!".bright_green().bold());
    println!("   Design saved to: {}", design_path.display());

    Ok(())
}

async fn run_generate(design: PathBuf, output: PathBuf, cache: bool) -> Result<()> {
    println!("{}", "⚙️  Generating code from architecture...".bright_blue().bold());

    let generator = Generator::new(cache)?;
    let design_json = std::fs::read_to_string(&design)?;
    let design: serde_json::Value = serde_json::from_str(&design_json)?;

    let files = generator.generate(&design, &output).await?;

    println!("{}", "✅ Code generation complete!".bright_green().bold());
    println!("   Generated {} files in: {}", files, output.display());

    Ok(())
}

async fn run_validate(input: PathBuf, schemas: PathBuf) -> Result<()> {
    println!("{}", "🔍 Validating generated code...".bright_blue().bold());

    let validator = Validator::new(schemas)?;
    let results = validator.validate(&input).await?;

    if results.is_valid() {
        println!("{}", "✅ All validations passed!".bright_green().bold());
    } else {
        println!("{}", "⚠️  Validation issues found:".bright_yellow().bold());
        for issue in results.issues() {
            println!("   • {}", issue);
        }
    }

    Ok(())
}

async fn run_deploy(input: PathBuf, target: String) -> Result<()> {
    println!("{}", format!("🚀 Deploying to {}...", target).bright_blue().bold());

    let deployer = Deployer::new()?;
    let deployment = deployer.deploy(&input, &target).await?;

    println!("{}", "✅ Deployment complete!".bright_green().bold());
    println!("   Access your application at: {}", deployment.url());

    Ok(())
}

async fn run_full_lifecycle(
    name: String,
    platform: String,
    output: PathBuf,
    target: String,
    cache: bool,
) -> Result<()> {
    println!("{}", "🎯 Running full lifecycle...".bright_magenta().bold());
    println!();

    // Phase 1: Design
    let design_dir = output.join("design");
    run_design(name.clone(), platform, design_dir.clone()).await?;
    println!();

    // Phase 2: Generate
    let gen_dir = output.join("generated");
    let design_file = design_dir.join("architecture.json");
    run_generate(design_file, gen_dir.clone(), cache).await?;
    println!();

    // Phase 3: Validate
    let schemas_dir = PathBuf::from("./schemas");
    run_validate(gen_dir.clone(), schemas_dir).await?;
    println!();

    // Phase 4: Deploy
    run_deploy(gen_dir, target).await?;
    println!();

    println!("{}", "🎉 Full lifecycle complete!".bright_green().bold());
    println!("   Project: {}", name.bright_cyan());
    println!("   Output: {}", output.display());

    Ok(())
}
