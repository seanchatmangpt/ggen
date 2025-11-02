//! Graph commands - clap-noun-verb auto-discovery
//!
//! This module defines the command-line interface for graph operations,
//! delegating to domain layer functions.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;

use crate::domain::graph;

/// Graph command arguments
#[derive(Debug, Args)]
pub struct GraphArgs {
    #[command(subcommand)]
    pub command: GraphCommand,
}

/// Graph subcommands
#[derive(Debug, Subcommand)]
pub enum GraphCommand {
    /// Load RDF data into graph
    Load(graph::load::LoadArgs),

    /// Query graph with SPARQL
    Query(graph::query::QueryArgs),

    /// Export graph to file
    Export(graph::export::ExportArgs),

    /// Visualize graph
    Visualize(graph::visualize::VisualizeArgs),
}

impl GraphArgs {
    /// Execute the graph command
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            GraphCommand::Load(args) => graph::load::run(args),
            GraphCommand::Query(args) => graph::query::run(args),
            GraphCommand::Export(args) => graph::export::run(args),
            GraphCommand::Visualize(args) => graph::visualize::run(args),
        }
    }
}
