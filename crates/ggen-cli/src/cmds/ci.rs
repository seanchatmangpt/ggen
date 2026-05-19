//! CI commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_core::utils::error::Result;

use ggen_core::domain::ci;

#[derive(Debug, Subcommand)]
pub enum CiCmd {
    /// Generate CI workflow
    Workflow(ci::workflow::WorkflowArgs),
}

impl CiCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Workflow(args) => ci::workflow::run(args),
        }
    }
}
