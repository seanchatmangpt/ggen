use clap::Subcommand;
use ggen_utils::error::Result;

pub mod diff;
pub mod export;
pub mod load;
pub mod query;
pub mod snapshot;
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
    /// Export RDF graph
    Export(export::ExportArgs),
    /// Validate graph against SHACL shapes
    Validate(validate::ValidateArgs),
    /// Show graph statistics
    Stats(stats::StatsArgs),
    /// Compare two RDF graphs and show differences
    Diff(diff::DiffArgs),
    /// Manage graph snapshots for delta-driven projection
    Snapshot(snapshot::SnapshotArgs),
}

impl GraphCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Query(args) => query::run(args).await,
            Verb::Load(args) => load::run(args).await,
            Verb::Export(args) => export::run(args).await,
            Verb::Validate(args) => validate::run(args).await,
            Verb::Stats(args) => stats::run(args).await,
            Verb::Diff(args) => diff::run(args).await,
            Verb::Snapshot(args) => snapshot::run(args).await,
        }
    }
}
