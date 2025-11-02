//! AI commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::ai;

#[derive(Debug, Subcommand)]
pub enum AiCmd {
    /// Generate code with AI
    Generate(ai::generate::GenerateArgs),
}

impl AiCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Generate(args) => ai::generate::run(args),
        }
    }
}
