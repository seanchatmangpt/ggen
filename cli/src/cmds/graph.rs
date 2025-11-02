//! Graph commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::graph;

#[derive(Debug, Subcommand)]
pub enum GraphCmd {
    /// Load RDF graph
    Load(graph::load::LoadArgs),
    /// Query RDF graph
    Query(graph::query::QueryArgs),
    /// Export RDF graph
    Export(graph::export::ExportArgs),
    /// Visualize RDF graph
    Visualize(graph::visualize::VisualizeArgs),
}

impl GraphCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Load(args) => graph::load::run(args),
            Self::Query(args) => graph::query::run(args),
            Self::Export(args) => graph::export::run(args),
            Self::Visualize(args) => graph::visualize::run(args),
        }
    }
}
