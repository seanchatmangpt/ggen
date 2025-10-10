use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod apply;
pub mod diff;
pub mod gen;
pub mod plan;

#[derive(Args, Debug)]
pub struct ProjectCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Generate artifacts from a template directly
    Gen(gen::GenArgs),

    /// Create a machine-readable plan of changes without applying them (dry-run)
    Plan(plan::PlanArgs),

    /// Apply a previously generated plan to the filesystem
    Apply(apply::ApplyArgs),

    /// Show a unified diff of what a generation would change
    Diff(diff::DiffArgs),
}

impl ProjectCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Gen(args) => gen::run(args).await,
            Verb::Plan(args) => plan::run(args).await,
            Verb::Apply(args) => apply::run(args).await,
            Verb::Diff(args) => diff::run(args).await,
        }
    }
}
