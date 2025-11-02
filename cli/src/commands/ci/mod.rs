//! CI commands - CLI layer for continuous integration operations

pub mod validate;

use clap::Subcommand;
use ggen_utils::error::Result;

#[derive(clap::Args, Debug)]
pub struct CiCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Validate CI/CD workflow files
    Validate(validate::ValidateArgs),
}

impl CiCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Validate(args) => validate::run(args),
        }
    }
}
