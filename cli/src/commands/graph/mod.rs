//! Graph commands - CLI layer for RDF operations
//!
//! Three-layer architecture:
//! - This layer: CLI argument parsing and user interaction
//! - Domain layer: Business logic (cli/src/domain/graph)
//! - Core layer: RDF infrastructure (ggen-core)

pub mod export;
pub mod load;
pub mod query;
pub mod visualize;

use clap::Subcommand;
use ggen_utils::error::Result;

#[derive(clap::Args, Debug)]
pub struct GraphCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Execute SPARQL query against RDF graph
    Query(query::QueryArgs),
    /// Load RDF data into graph
    Load(load::LoadArgs),
    /// Export RDF graph to file
    Export(export::ExportArgs),
    /// Visualize RDF graph
    Visualize(visualize::VisualizeArgs),
}

impl GraphCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Query(args) => query::run(args).await,
            Verb::Load(args) => load::run(args).await,
            Verb::Export(args) => export::run(args).await,
            Verb::Visualize(args) => visualize::run(args),
        }
    }
}
