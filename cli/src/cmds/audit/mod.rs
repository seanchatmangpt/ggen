use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod hazard;
pub mod performance;
pub mod security;

#[derive(Args, Debug)]
pub struct AuditCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Check for hazardous patterns and anti-patterns
    Hazard(hazard::HazardArgs),

    /// Perform security vulnerability scans
    Security(security::SecurityArgs),

    /// Analyze performance characteristics
    Performance(performance::PerformanceArgs),
}

impl AuditCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Hazard(args) => hazard::run(args).await,
            Verb::Security(args) => security::run(args).await,
            Verb::Performance(args) => performance::run(args).await,
        }
    }
}
