use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
// pub mod completion; // COMMENTED OUT: Command line completion code
pub mod init;

#[derive(Args, Debug)]
pub struct ShellCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    // /// Generate shell completion scripts // COMMENTED OUT: Command line completion code
    // Completion(completion::CompletionArgs),

    /// Initialize shell integration
    Init(init::InitArgs),
}

impl ShellCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            // Verb::Completion(args) => completion::run(args).await, // COMMENTED OUT: Command line completion code
            Verb::Init(args) => init::run(args).await,
        }
    }
}
