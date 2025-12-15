//! Complete Lifecycle System Demonstration
//!
//! Demonstrates all lifecycle features:
//! - Phase execution (init, setup, build, test, deploy)
//! - Before/after hooks
//! - State persistence
//! - Caching
//! - Workspace parallelism
//! - Error handling and recovery

use clap::{Parser, Subcommand};
use colored::*;
use ggen_core::lifecycle::{
    load_make, load_state, run_phase, run_pipeline, save_state, Context, LifecycleState, Make,
    Phase,
};
use indicatif::{ProgressBar, ProgressStyle};
use std::path::PathBuf;
use tracing::{info, warn};

#[derive(Parser)]
#[command(name = "lifecycle-demo")]
#[command(about = "Complete lifecycle system demonstration", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Path to make.toml file
    #[arg(short, long, default_value = "make.toml")]
    config: PathBuf,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Dry run mode
    #[arg(short = 'n', long)]
    dry_run: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a specific phase
    Run {
        /// Phase name (init, setup, build, test, deploy, etc.)
        phase: String,

        /// Skip cache
        #[arg(long)]
        no_cache: bool,
    },

    /// Run complete pipeline
    Pipeline {
        /// Phases to run (defaults to all)
        phases: Vec<String>,

        /// Stop on first error
        #[arg(long)]
        fail_fast: bool,
    },

    /// Show lifecycle status
    Status,

    /// Clear cached state
    Clean,

    /// Validate make.toml configuration
    Validate,

    /// Show available phases
    Phases,

    /// Generate example make.toml
    Init {
        /// Project name
        name: String,

        /// Template type (rust, python, fullstack)
        #[arg(short, long, default_value = "rust")]
        template: String,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("lifecycle_demo={}", level))
        .init();

    // Execute command
    match cli.command {
        Commands::Run { phase, no_cache } => {
            run_phase_command(&cli.config, &phase, no_cache, cli.dry_run).await?
        }
        Commands::Pipeline { phases, fail_fast } => {
            run_pipeline_command(&cli.config, phases, fail_fast, cli.dry_run).await?
        }
        Commands::Status => show_status(&cli.config)?,
        Commands::Clean => clean_state(&cli.config)?,
        Commands::Validate => validate_config(&cli.config)?,
        Commands::Phases => show_phases(&cli.config)?,
        Commands::Init { name, template } => init_project(&name, &template)?,
    }

    Ok(())
}

async fn run_phase_command(
    config_path: &PathBuf,
    phase_name: &str,
    no_cache: bool,
    dry_run: bool,
) -> anyhow::Result<()> {
    info!("Running phase: {}", phase_name);

    let make = load_make(config_path)?;
    let mut ctx = create_context(config_path, dry_run)?;

    if no_cache {
        ctx.skip_cache = true;
    }

    let pb = create_progress_bar(&format!("Executing {}", phase_name));

    match run_phase(&make, phase_name, &ctx).await {
        Ok(_) => {
            pb.finish_with_message(format!("✓ {} completed", phase_name).green().to_string());
            println!("{}", format!("Phase '{}' completed successfully", phase_name).green());
        }
        Err(e) => {
            pb.finish_with_message(format!("✗ {} failed", phase_name).red().to_string());
            eprintln!("{}", format!("Phase '{}' failed: {}", phase_name, e).red());
            return Err(e.into());
        }
    }

    Ok(())
}

async fn run_pipeline_command(
    config_path: &PathBuf,
    phases: Vec<String>,
    fail_fast: bool,
    dry_run: bool,
) -> anyhow::Result<()> {
    info!("Running pipeline");

    let make = load_make(config_path)?;
    let ctx = create_context(config_path, dry_run)?;

    let phases_to_run = if phases.is_empty() {
        vec!["init", "setup", "build", "test"]
            .iter()
            .map(|s| s.to_string())
            .collect()
    } else {
        phases
    };

    println!("{}", format!("Running pipeline: {:?}", phases_to_run).cyan());
    println!();

    for phase in &phases_to_run {
        let pb = create_progress_bar(&format!("Executing {}", phase));

        match run_phase(&make, phase, &ctx).await {
            Ok(_) => {
                pb.finish_with_message(format!("✓ {} completed", phase).green().to_string());
                println!();
            }
            Err(e) => {
                pb.finish_with_message(format!("✗ {} failed", phase).red().to_string());
                eprintln!("{}", format!("Phase '{}' failed: {}", phase, e).red());
                if fail_fast {
                    return Err(e.into());
                }
                println!();
            }
        }
    }

    println!("{}", "Pipeline completed".green().bold());
    Ok(())
}

fn show_status(config_path: &PathBuf) -> anyhow::Result<()> {
    let make = load_make(config_path)?;
    let state = load_state().unwrap_or_default();

    println!("{}", "Lifecycle Status".cyan().bold());
    println!("{}", "═".repeat(50));

    println!("\n{}", "Project:".yellow().bold());
    println!("  Name: {}", make.project.name);
    println!("  Version: {}", make.project.version.as_deref().unwrap_or("N/A"));

    if let Some(workspace) = &make.workspace {
        println!("\n{}", "Workspace:".yellow().bold());
        println!("  Members: {}", workspace.members.len());
        for member in &workspace.members {
            println!("    - {}", member);
        }
    }

    println!("\n{}", "Phases:".yellow().bold());
    for (name, phase) in &make.phases {
        let status = if state.completed_phases.contains(name) {
            "✓".green()
        } else {
            "○".normal()
        };
        println!("  {} {}", status, name);
        if !phase.commands.is_empty() {
            println!("      Commands: {}", phase.commands.len());
        }
    }

    Ok(())
}

fn clean_state(_config_path: &PathBuf) -> anyhow::Result<()> {
    println!("{}", "Cleaning cached state...".yellow());

    let default_state = LifecycleState::default();
    save_state(&default_state)?;

    println!("{}", "✓ State cleaned".green());
    Ok(())
}

fn validate_config(config_path: &PathBuf) -> anyhow::Result<()> {
    println!("{}", "Validating configuration...".yellow());

    match load_make(config_path) {
        Ok(make) => {
            println!("{}", "✓ Configuration is valid".green());
            println!("\nProject: {}", make.project.name);
            println!("Phases: {}", make.phases.len());
            if let Some(workspace) = make.workspace {
                println!("Workspace members: {}", workspace.members.len());
            }
        }
        Err(e) => {
            eprintln!("{}", format!("✗ Configuration is invalid: {}", e).red());
            return Err(e.into());
        }
    }

    Ok(())
}

fn show_phases(config_path: &PathBuf) -> anyhow::Result<()> {
    let make = load_make(config_path)?;

    println!("{}", "Available Phases".cyan().bold());
    println!("{}", "═".repeat(50));

    for (name, phase) in &make.phases {
        println!("\n{}", format!("{}:", name).yellow().bold());
        if let Some(desc) = &phase.description {
            println!("  Description: {}", desc);
        }
        println!("  Commands: {}", phase.commands.len());

        if let Some(hooks) = &phase.hooks {
            if !hooks.before.is_empty() {
                println!("  Before hooks: {}", hooks.before.len());
            }
            if !hooks.after.is_empty() {
                println!("  After hooks: {}", hooks.after.len());
            }
        }

        if !phase.depends_on.is_empty() {
            println!("  Dependencies: {:?}", phase.depends_on);
        }
    }

    Ok(())
}

fn init_project(name: &str, template: &str) -> anyhow::Result<()> {
    println!("{}", format!("Initializing project: {}", name).cyan());

    let make_toml = match template {
        "rust" => generate_rust_make(name),
        "python" => generate_python_make(name),
        "fullstack" => generate_fullstack_make(name),
        _ => {
            eprintln!("{}", format!("Unknown template: {}", template).red());
            return Err(anyhow::anyhow!("Unknown template"));
        }
    };

    std::fs::write("make.toml", make_toml)?;
    println!("{}", "✓ make.toml created".green());

    Ok(())
}

fn create_context(config_path: &PathBuf, dry_run: bool) -> anyhow::Result<Context> {
    Ok(Context {
        project_root: config_path.parent().unwrap_or(std::path::Path::new(".")).to_path_buf(),
        dry_run,
        skip_cache: false,
        env: std::collections::HashMap::new(),
    })
}

fn create_progress_bar(message: &str) -> ProgressBar {
    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.cyan} {msg}")
            .unwrap(),
    );
    pb.set_message(message.to_string());
    pb.enable_steady_tick(std::time::Duration::from_millis(100));
    pb
}

fn generate_rust_make(name: &str) -> String {
    format!(
        r#"[project]
name = "{}"
version = "0.1.0"

[phases.init]
description = "Initialize project"
commands = ["cargo init --name {}"]

[phases.setup]
description = "Install dependencies"
commands = ["cargo fetch"]
depends_on = ["init"]

[phases.build]
description = "Build project"
commands = ["cargo build --release"]
depends_on = ["setup"]

[phases.test]
description = "Run tests"
commands = ["cargo test"]
depends_on = ["build"]

[phases.deploy]
description = "Deploy application"
commands = ["cargo install --path ."]
depends_on = ["test"]
"#,
        name, name
    )
}

fn generate_python_make(name: &str) -> String {
    format!(
        r#"[project]
name = "{}"
version = "0.1.0"

[phases.init]
description = "Initialize project"
commands = ["python -m venv venv"]

[phases.setup]
description = "Install dependencies"
commands = ["./venv/bin/pip install -r requirements.txt"]
depends_on = ["init"]

[phases.build]
description = "Build project"
commands = ["./venv/bin/python -m build"]
depends_on = ["setup"]

[phases.test]
description = "Run tests"
commands = ["./venv/bin/pytest"]
depends_on = ["setup"]

[phases.deploy]
description = "Deploy application"
commands = ["./venv/bin/python -m twine upload dist/*"]
depends_on = ["test", "build"]
"#,
        name
    )
}

fn generate_fullstack_make(name: &str) -> String {
    format!(
        r#"[project]
name = "{}"
version = "0.1.0"

[workspace]
members = ["backend", "frontend"]
parallel = true

[phases.init]
description = "Initialize all services"
commands = [
    "cargo init backend",
    "npm create vite@latest frontend -- --template react-ts"
]

[phases.setup]
description = "Install dependencies"
commands = [
    "cd backend && cargo fetch",
    "cd frontend && npm install"
]
depends_on = ["init"]

[phases.build]
description = "Build all services"
commands = [
    "cd backend && cargo build --release",
    "cd frontend && npm run build"
]
depends_on = ["setup"]

[phases.test]
description = "Test all services"
commands = [
    "cd backend && cargo test",
    "cd frontend && npm test"
]
depends_on = ["build"]

[phases.deploy]
description = "Deploy all services"
commands = [
    "cd backend && cargo install --path .",
    "cd frontend && npm run deploy"
]
depends_on = ["test"]
"#,
        name
    )
}
