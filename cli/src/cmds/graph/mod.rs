use clap::Subcommand;
use ggen_utils::error::Result;

pub mod export;
pub mod load;
pub mod query;
pub mod stats;
pub mod validate;

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
    /// Export graph to file
    Export(export::ExportArgs),
    /// Validate graph against SHACL shapes
    Validate(validate::ValidateArgs),
    /// Show graph statistics
    Stats(stats::StatsArgs),
}

impl GraphCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Query(args) => query::run(args).await,
            Verb::Load(args) => load::run(args).await,
            Verb::Export(args) => export::run(args).await,
            Verb::Validate(args) => validate::run(args).await,
            Verb::Stats(args) => stats::run(args).await,
        }
    }
}
