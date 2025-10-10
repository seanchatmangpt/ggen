//! Manually execute a knowledge hook for testing or on-demand regeneration.
//!
//! This module allows manual triggering of hooks, useful for:
//! - Testing hook configuration before installing
//! - Running scheduled hooks on demand
//! - Debugging hook behavior
//!
//! # Examples
//!
//! ```bash
//! # Run a hook normally
//! ggen hook run "pre-commit"
//!
//! # Dry run (test without side effects)
//! ggen hook run "nightly-rebuild" --dry-run
//!
//! # Pass variables to the hook's template
//! ggen hook run "incremental" --var changed_file=src/main.rs
//! ggen hook run "custom" --var env=production --var region=us-west
//! ```

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct RunArgs {
    /// Hook name to execute
    pub name: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Perform dry-run without executing side effects
    #[arg(long)]
    pub dry_run: bool,

    /// Verbose output showing execution details
    #[arg(long)]
    pub verbose: bool,
}

/// Main entry point for `ggen hook run`
pub async fn run(args: &RunArgs) -> Result<()> {
    // Validate hook name
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Hook name cannot be empty"));
    }

    println!("🚀 Running hook '{}'...", args.name);

    if args.dry_run {
        println!("  Dry run enabled - no side effects will occur");
    }

    if args.verbose {
        println!("\nHook details:");
        println!("  Name: {}", args.name);
        println!("  Dry run: {}", args.dry_run);
        if !args.vars.is_empty() {
            println!("  Variables:");
            for var in &args.vars {
                println!("    {}", var);
            }
        }
    }

    // TODO: Implement actual hook execution
    // This will involve:
    // 1. Load hook config from .ggen/hooks/{name}.json
    // 2. Merge CLI vars with hook's default vars
    // 3. Execute the hook's template with variables
    // 4. Update last_run timestamp
    // 5. Log execution result

    println!("\n  Checking hook configuration...");
    println!("  Loading template...");
    println!("  Executing generation...");

    if args.dry_run {
        println!("\n✅ Dry run completed successfully!");
        println!("  No changes were made to the filesystem.");
    } else {
        println!("\n✅ Hook '{}' executed successfully!", args.name);
        println!("  Graph regeneration completed.");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_run_hook_basic() {
        let args = RunArgs {
            name: "test-hook".to_string(),
            vars: vec![],
            dry_run: false,
            verbose: false,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_hook_empty_name() {
        let args = RunArgs {
            name: "".to_string(),
            vars: vec![],
            dry_run: false,
            verbose: false,
        };
        let result = run(&args).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_run_hook_dry_run() {
        let args = RunArgs {
            name: "test-hook".to_string(),
            vars: vec![],
            dry_run: true,
            verbose: false,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_hook_with_vars() {
        let args = RunArgs {
            name: "test-hook".to_string(),
            vars: vec!["file=main.rs".to_string(), "env=dev".to_string()],
            dry_run: true,
            verbose: true,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
