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
    ///
    /// Examples:
    ///   ggen project gen "rust-cli-template" --var name=myapp
    ///   ggen project gen "web-api.tmpl" --dry-run
    ///   ggen project gen "template.tmpl" --seed 12345
    Gen(gen::GenArgs),

    /// Create a machine-readable plan of changes without applying them (dry-run)
    ///
    /// Examples:
    ///   ggen project plan "rust-cli-template" --var name=myapp
    ///   ggen project plan "template.tmpl" --format yaml
    Plan(plan::PlanArgs),

    /// Apply a previously generated plan to the filesystem
    ///
    /// Examples:
    ///   ggen project apply plan.json
    ///   ggen project apply plan.yaml --dry-run
    Apply(apply::ApplyArgs),

    /// Show a unified diff of what a generation would change
    ///
    /// Examples:
    ///   ggen project diff "rust-cli-template" --var name=myapp
    ///   ggen project diff "template.tmpl" --var version=1.0.0
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
