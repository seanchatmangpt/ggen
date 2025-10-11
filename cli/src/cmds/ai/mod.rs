//! AI-powered template generation commands

use clap::{Args, Subcommand};
use ggen_utils::error::Result;

pub mod config;
pub mod generate;
pub mod sparql;
pub mod graph;
pub mod demo;
pub mod server;
pub mod frontmatter;
pub mod models;
pub mod validate;
pub mod project;
pub mod from_source;

#[derive(Debug, Args)]
pub struct AiArgs {
    #[command(subcommand)]
    pub command: AiCommand,
}

#[derive(Debug, Subcommand)]
pub enum AiCommand {
    /// Generate templates using AI
    Generate(generate::GenerateArgs),
    /// Generate SPARQL queries using AI
    Sparql(sparql::SparqlArgs),
    /// Generate RDF graphs using AI
    Graph(graph::GraphArgs),
    /// Run the AI template demo
    Demo,
    /// Start the MCP server
    Server(server::ServerArgs),
    /// Generate frontmatter using AI
    Frontmatter(frontmatter::FrontmatterArgs),
    /// List available AI models
    Models(models::ModelsArgs),
    /// Validate templates
    Validate(validate::ValidateArgs),
    /// Generate complete template projects
    Project(project::ProjectArgs),
    /// Generate template from existing source file
    FromSource(from_source::FromSourceArgs),
}

pub async fn run(args: &AiArgs) -> Result<()> {
    match &args.command {
        AiCommand::Generate(args) => generate::run(args).await,
        AiCommand::Sparql(args) => sparql::run(args).await,
        AiCommand::Graph(args) => graph::run(args).await,
        AiCommand::Demo => demo::run().await,
        AiCommand::Server(args) => server::run(args).await,
        AiCommand::Frontmatter(args) => frontmatter::run(args).await,
        AiCommand::Models(args) => models::run(args).await,
        AiCommand::Validate(args) => validate::run(args).await,
        AiCommand::Project(args) => project::run(args).await,
        AiCommand::FromSource(args) => from_source::run(args).await,
    }
}

