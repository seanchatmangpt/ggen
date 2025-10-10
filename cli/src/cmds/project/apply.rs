use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
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

/// Main entry point for `ggen project apply`
pub async fn run(args: &ApplyArgs) -> Result<()> {
    println!("🚧 Placeholder: project apply");
    println!("  Plan file: {}", args.plan_file);
    println!("  Auto confirm: {}", args.auto_confirm);
    println!("  Dry run: {}", args.dry_run);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_apply_args_parsing() {
        let args = ApplyArgs {
            plan_file: "changes.plan".to_string(),
            auto_confirm: true,
            dry_run: false,
        };

        assert_eq!(args.plan_file, "changes.plan");
        assert!(args.auto_confirm);
    }
}
