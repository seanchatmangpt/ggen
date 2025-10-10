use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod pages;
pub mod release;
pub mod trigger;
pub mod workflow;

#[derive(Args, Debug)]
pub struct CiCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Manage GitHub Pages deployment
    Pages(pages::PagesArgs),

    /// Run release workflows locally with act
    Release(release::ReleaseArgs),

    /// Manage GitHub Actions workflows
    Workflow(workflow::WorkflowArgs),

    /// Trigger CI/CD workflows manually
    Trigger(trigger::TriggerArgs),
}

impl CiCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Pages(args) => pages::run(args).await,
            Verb::Release(args) => release::run(args).await,
            Verb::Workflow(args) => workflow::run(args).await,
            Verb::Trigger(args) => trigger::run(args).await,
        }
    }
}
