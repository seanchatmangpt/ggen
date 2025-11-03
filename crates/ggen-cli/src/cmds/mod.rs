//! Command Router Module - clap-noun-verb v3.3.0 Auto-Discovery
//!
//! This module provides the entry point for clap-noun-verb auto-discovery.
//! It registers all noun commands with their associated verbs for the CLI.
//!
//! ## Architecture
//! ```
//! cmds (router) → commands (sync wrappers) → runtime → domain (async logic)
//! ```

pub mod ai;
pub mod graph;
pub mod hook;
pub mod marketplace;
pub mod project;
pub mod template;
pub mod utils;

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
    /// Template operations (generate, lint, list, etc.)
    Template(crate::cmds::template::TemplateArgs),

    /// AI-powered code generation and analysis
    Ai(crate::cmds::ai::AiArgs),

    /// Graph operations (load, query, export, visualize)
    Graph(crate::cmds::graph::GraphArgs),

    /// Marketplace operations (search, install, list, publish, update)
    Marketplace(crate::cmds::marketplace::MarketplaceArgs),

    /// Project operations (new, gen, apply, plan, init)
    Project(crate::cmds::project::ProjectArgs),

    /// Hook management (create, list, remove, monitor)
    #[command(subcommand)]
    Hook(crate::cmds::hook::HookCmd),

    /// Utility operations (doctor, env)
    #[command(subcommand)]
    Utils(crate::cmds::utils::UtilsCmd),
}

impl Cli {
    /// Execute the CLI command
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            Commands::Template(args) => args.execute(),
            Commands::Ai(args) => args.execute(),
            Commands::Graph(args) => args.execute(),
            Commands::Marketplace(args) => args.execute(),
            Commands::Project(args) => args.execute(),
            Commands::Hook(cmd) => cmd.execute(),
            Commands::Utils(cmd) => cmd.execute(),
        }
    }
}

/// Setup and run the command router
pub fn run_cli() -> Result<()> {
    let cli = Cli::parse();
    cli.execute()
}
