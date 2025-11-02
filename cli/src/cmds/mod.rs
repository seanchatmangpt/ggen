//! Command Router Module - clap-noun-verb v3.2.0 Auto-Discovery
//!
//! This module provides the entry point for clap-noun-verb auto-discovery.
//! It registers all noun commands with their associated verbs for the CLI.
//!
//! ## Architecture
//! ```
//! cmds (router) → commands (sync wrappers) → runtime → domain (async logic)
//! ```

pub mod template;

use clap::Parser;
use ggen_utils::error::Result;

/// Main CLI structure for ggen
#[derive(Parser, Debug)]
#[command(name = "ggen")]
#[command(author, version, about = "Graph-based code generation framework", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

/// All available noun commands
#[derive(clap::Subcommand, Debug)]
pub enum Commands {
    /// Template operations
    Template(crate::cmds::template::TemplateArgs),

    // Additional nouns will be added here as they're implemented:
    // Graph(crate::cmds::graph::GraphArgs),
    // Ai(crate::cmds::ai::AiArgs),
    // Marketplace(crate::cmds::marketplace::MarketplaceArgs),
    // Project(crate::cmds::project::ProjectArgs),
    // Utils(crate::cmds::utils::UtilsArgs),
}

impl Cli {
    /// Execute the CLI command
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            Commands::Template(args) => args.execute(),
            // Add other command handlers here
        }
    }
}

/// Setup and run the command router
pub fn run_cli() -> Result<()> {
    let cli = Cli::parse();
    cli.execute()
}
