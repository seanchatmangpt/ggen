//! Hook listing - CLI wrapper
//!
//! Wraps async domain logic for listing hooks in a synchronous CLI context.

use clap::Args;
use ggen_utils::error::Result;

/// Arguments for listing hooks
#[derive(Debug, Args)]
pub struct ListArgs {
    /// Show detailed hook information
    #[arg(short = 'v', long)]
    pub verbose: bool,

    /// Filter by trigger pattern
    #[arg(short = 'f', long)]
    pub filter: Option<String>,
}

/// Run hook list command (placeholder implementation)
pub fn run(_args: &ListArgs) -> Result<()> {
    println!("📭 No hooks configured");
    println!("\nCreate a hook with:");
    println!("  ggen hook create --trigger file:change --action 'echo Changed'");
    Ok(())
}
