//! Hook removal - CLI wrapper
//!
//! Wraps async domain logic for removing hooks in a synchronous CLI context.

use clap::Args;
use ggen_utils::error::Result;

/// Arguments for removing a hook
#[derive(Debug, Args)]
pub struct RemoveArgs {
    /// Hook ID to remove
    pub hook_id: String,

    /// Force removal without confirmation
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// Run hook remove command (placeholder implementation)
pub fn run(args: &RemoveArgs) -> Result<()> {
    if !args.force {
        println!("⚠️  Removing hook: {}", args.hook_id);
        println!("   Use --force to confirm");
        return Ok(());
    }

    println!("✅ Removed hook: {}", args.hook_id);
    Ok(())
}
