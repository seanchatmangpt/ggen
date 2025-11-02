//! Project commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::project;

#[derive(Debug, Subcommand)]
pub enum ProjectCmd {
    /// Create a new project
    New(project::new::NewArgs),
    /// Generate project plan
    Plan(project::plan::PlanArgs),
    /// Generate code from plan
    Gen(project::gen::GenArgs),
    /// Apply code changes
    Apply(project::apply::ApplyArgs),
}

impl ProjectCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::New(args) => project::new::run(args),
            Self::Plan(args) => project::plan::run(args),
            Self::Gen(args) => project::gen::run(args),
            Self::Apply(args) => project::apply::run(args),
        }
    }
}
