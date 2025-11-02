//! Plan application CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct ApplyArgs {
    /// Path to the plan file to apply
    pub plan_file: String,

    /// Apply changes without prompting for confirmation
    #[arg(short = 'y', long = "yes")]
    pub auto_confirm: bool,

    /// Perform dry-run to show what would be applied
    #[arg(long)]
    pub dry_run: bool,
}

pub async fn run(args: &ApplyArgs) -> Result<()> {
    println!("🔧 Applying generation plan...");

    // Delegate to domain layer using sync wrapper
    let result = tokio::task::spawn_blocking({
        let args = args.clone();
        move || crate::domain::project::apply::apply_plan(&args)
    })
    .await
    .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Task join error: {}", e)))??;

    if args.dry_run {
        println!("🔍 Dry run completed - no changes applied");
        println!("   Would apply {} operations", result.operations_count);
    } else {
        println!("✅ Generation plan applied successfully");
        println!("   Operations applied: {}", result.operations_count);
    }

    Ok(())
}
