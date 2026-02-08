//! MCP+ Board Report CLI
//!
//! Command-line interface for MCP+ operations:
//! - Kill switch control (global, family, capability, epoch)
//! - Evidence bundle verification (text-blind)
//! - Key epoch rotation
//! - Disaster recovery drills
//! - Quarterly drill orchestration
//!
//! Core axiom: A = μ(O), μ ∘ μ = μ, hash(A) = hash(μ(O)), O ⊨ Σ

mod commands;
mod models;

use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::Colorize;

#[derive(Parser)]
#[command(name = "mcp")]
#[command(author = "MCP+ Team")]
#[command(version = "0.1.0")]
#[command(about = "MCP+ Board Report CLI - Sealed operating contract management")]
#[command(long_about = r#"
MCP+ Board Report CLI

Enterprise-grade CLI for managing sealed operating contracts with:
- Cryptographic verification
- Deterministic refusals
- Text-blind evidence bundles
- Kill switch authority

Core axiom: A = μ(O), μ ∘ μ = μ, hash(A) = hash(μ(O)), O ⊨ Σ
"#)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Base directory for state and evidence
    #[arg(long, global = true, default_value = ".")]
    base_dir: String,
}

#[derive(Subcommand)]
enum Commands {
    /// Kill switch operations
    #[command(subcommand)]
    Kill(KillCommands),

    /// Evidence bundle operations
    #[command(subcommand)]
    Bundle(BundleCommands),

    /// Key epoch operations
    #[command(subcommand)]
    Epoch(EpochCommands),

    /// Drill operations
    #[command(subcommand)]
    Drill(DrillCommands),

    /// Show system status
    Status,
}

#[derive(Subcommand)]
enum KillCommands {
    /// Disable all MCP+ operations globally
    Global,

    /// Disable a specific contract family
    Family {
        /// Family ID to disable
        id: String,
    },

    /// Disable a specific capability
    Capability {
        /// Capability ID to disable
        id: String,
    },

    /// Revoke a signing key epoch
    Epoch {
        /// Epoch ID to revoke
        id: String,
    },

    /// Reset all kill switches (requires confirmation)
    Reset {
        /// Skip confirmation prompt
        #[arg(long)]
        force: bool,
    },
}

#[derive(Subcommand)]
enum BundleCommands {
    /// Verify an evidence bundle (text-blind)
    Verify {
        /// Path to bundle file
        path: String,
    },

    /// Create a test bundle for verification demo
    CreateTest,

    /// Generate a new evidence bundle from receipts
    Generate {
        /// Contract family
        #[arg(long)]
        family: String,

        /// Output path
        #[arg(long, short)]
        output: Option<String>,
    },
}

#[derive(Subcommand)]
enum EpochCommands {
    /// Rotate to a new signing key epoch
    Rotate {
        /// Duration in days (default: 90)
        #[arg(long, default_value = "90")]
        duration_days: i64,

        /// Dry run - don't actually rotate
        #[arg(long)]
        dry_run: bool,
    },

    /// List all epochs
    List,

    /// Show current epoch status
    Status,
}

#[derive(Subcommand)]
enum DrillCommands {
    /// Run kill switch drill
    KillSwitch {
        /// Drill type (global, family, capability, epoch, all)
        #[arg(default_value = "all")]
        drill_type: String,
    },

    /// Run disaster recovery drill
    Dr,

    /// Run full quarterly drill suite
    Quarterly,
}

fn print_header(title: &str) {
    println!();
    println!("{}", "╔══════════════════════════════════════════════════════════════╗".blue());
    println!("{}", format!("║  {:^58}  ║", title).blue());
    println!("{}", "╚══════════════════════════════════════════════════════════════╝".blue());
    println!();
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Ensure directories exist
    let base = std::path::PathBuf::from(&cli.base_dir);
    let state_dir = base.join("state");
    let evidence_dir = base.join("evidence");
    let logs_dir = base.join("logs");

    std::fs::create_dir_all(&state_dir)?;
    std::fs::create_dir_all(&evidence_dir)?;
    std::fs::create_dir_all(&logs_dir)?;

    let ctx = commands::Context {
        base_dir: base,
        state_dir,
        evidence_dir,
        logs_dir,
    };

    match cli.command {
        Commands::Kill(cmd) => match cmd {
            KillCommands::Global => {
                print_header("GLOBAL KILL SWITCH");
                commands::kill::disable_global(&ctx)?;
            }
            KillCommands::Family { id } => {
                print_header("FAMILY KILL SWITCH");
                commands::kill::disable_family(&ctx, &id)?;
            }
            KillCommands::Capability { id } => {
                print_header("CAPABILITY KILL SWITCH");
                commands::kill::disable_capability(&ctx, &id)?;
            }
            KillCommands::Epoch { id } => {
                print_header("EPOCH REVOCATION");
                commands::kill::revoke_epoch(&ctx, &id)?;
            }
            KillCommands::Reset { force } => {
                print_header("RESET KILL SWITCHES");
                commands::kill::reset(&ctx, force)?;
            }
        },

        Commands::Bundle(cmd) => match cmd {
            BundleCommands::Verify { path } => {
                print_header("EVIDENCE BUNDLE VERIFICATION");
                commands::bundle::verify(&ctx, &path)?;
            }
            BundleCommands::CreateTest => {
                print_header("CREATE TEST BUNDLE");
                commands::bundle::create_test(&ctx)?;
            }
            BundleCommands::Generate { family, output } => {
                print_header("GENERATE BUNDLE");
                commands::bundle::generate(&ctx, &family, output.as_deref())?;
            }
        },

        Commands::Epoch(cmd) => match cmd {
            EpochCommands::Rotate { duration_days, dry_run } => {
                print_header("EPOCH ROTATION");
                commands::epoch::rotate(&ctx, duration_days, dry_run)?;
            }
            EpochCommands::List => {
                print_header("EPOCH LIST");
                commands::epoch::list(&ctx)?;
            }
            EpochCommands::Status => {
                print_header("EPOCH STATUS");
                commands::epoch::status(&ctx)?;
            }
        },

        Commands::Drill(cmd) => match cmd {
            DrillCommands::KillSwitch { drill_type } => {
                print_header("KILL SWITCH DRILL");
                commands::drill::kill_switch(&ctx, &drill_type)?;
            }
            DrillCommands::Dr => {
                print_header("DISASTER RECOVERY DRILL");
                commands::drill::disaster_recovery(&ctx)?;
            }
            DrillCommands::Quarterly => {
                print_header("QUARTERLY DRILL SUITE");
                commands::drill::quarterly(&ctx)?;
            }
        },

        Commands::Status => {
            print_header("MCP+ SYSTEM STATUS");
            commands::status::show(&ctx)?;
        }
    }

    Ok(())
}
