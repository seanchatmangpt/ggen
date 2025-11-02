//! AI commands - CLI layer for AI-powered operations

pub mod generate;

use clap::Subcommand;
use ggen_utils::error::Result;

#[derive(clap::Args, Debug)]
pub struct AiCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Generate code using AI
    Generate(generate::GenerateArgs),
}

impl AiCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Generate(args) => generate::run(args),
        }
    }
}
