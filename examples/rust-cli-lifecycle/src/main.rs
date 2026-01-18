//! TaskMgr - Example Rust CLI with ggen Lifecycle System
//!
//! This demonstrates a complete noun-verb CLI structure managed
//! through the ggen lifecycle system.

use clap::{Parser, Subcommand};
use colored::*;

mod commands;

#[derive(Parser, Debug)]
#[command(
    name = "taskmgr",
    version,
    about = "Task manager CLI - Built with ggen lifecycle",
    long_about = "A complete example of noun-verb CLI architecture\nmanaged by ggen's universal lifecycle system"
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Task management commands
    #[command(name = "task", about = "Manage tasks")]
    Task {
        #[command(subcommand)]
        action: TaskAction,
    },

    /// Project management commands
    #[command(name = "project", about = "Manage projects")]
    Project {
        #[command(subcommand)]
        action: ProjectAction,
    },

    /// Status and reporting commands
    #[command(name = "status", about = "Show status")]
    Status {
        /// Show detailed status
        #[arg(short, long)]
        detailed: bool,
    },

    /// Configuration commands
    #[command(name = "config", about = "Manage configuration")]
    Config {
        #[command(subcommand)]
        action: ConfigAction,
    },
}

#[derive(Subcommand, Debug)]
enum TaskAction {
    /// Create a new task
    #[command(name = "create")]
    Create {
        /// Task description
        description: String,

        /// Priority (low, medium, high)
        #[arg(short, long, default_value = "medium")]
        priority: String,
    },

    /// List all tasks
    #[command(name = "list")]
    List {
        /// Filter by status
        #[arg(short, long)]
        status: Option<String>,
    },

    /// Complete a task
    #[command(name = "complete")]
    Complete {
        /// Task ID
        id: u32,
    },

    /// Delete a task
    #[command(name = "delete")]
    Delete {
        /// Task ID
        id: u32,
    },
}

#[derive(Subcommand, Debug)]
enum ProjectAction {
    /// Create a new project
    #[command(name = "create")]
    Create {
        /// Project name
        name: String,
    },

    /// List all projects
    #[command(name = "list")]
    List,

    /// Show project details
    #[command(name = "show")]
    Show {
        /// Project name
        name: String,
    },
}

#[derive(Subcommand, Debug)]
enum ConfigAction {
    /// Show current configuration
    #[command(name = "show")]
    Show,

    /// Set a configuration value
    #[command(name = "set")]
    Set {
        /// Config key
        key: String,

        /// Config value
        value: String,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Show lifecycle banner
    println!("{}", "━".repeat(60).bright_blue());
    println!(
        "{} {}",
        "TaskMgr".bright_yellow().bold(),
        "v0.1.0".bright_black()
    );
    println!("{}", "Built with ggen lifecycle system".bright_black());
    println!("{}", "━".repeat(60).bright_blue());
    println!();

    match cli.command {
        Commands::Task { action } => {
            commands::task::handle(action, cli.verbose).await?;
        }
        Commands::Project { action } => {
            commands::project::handle(action, cli.verbose).await?;
        }
        Commands::Status { detailed } => {
            commands::status::show(detailed).await?;
        }
        Commands::Config { action } => {
            commands::config::handle(action, cli.verbose).await?;
        }
    }

    Ok(())
}
