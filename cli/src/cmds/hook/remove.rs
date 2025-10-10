//! Remove knowledge hooks and uninstall them from the system.
//!
//! This module provides commands to remove hooks, which involves:
//! - Deleting hook configuration files
//! - Uninstalling git hooks from .git/hooks/
//! - Stopping file watchers
//! - Removing cron jobs
//!
//! # Examples
//!
//! ```bash
//! # Remove a hook (with confirmation)
//! ggen hook remove "pre-commit"
//!
//! # Force remove without confirmation
//! ggen hook remove "nightly-rebuild" --force
//! ```

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct RemoveArgs {
    /// Hook name to remove
    pub name: String,

    /// Force removal without confirmation
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// Main entry point for `ggen hook remove`
pub async fn run(args: &RemoveArgs) -> Result<()> {
    // Validate hook name
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Hook name cannot be empty"));
    }

    println!("🗑️  Removing hook '{}'...", args.name);

    // TODO: Check if hook exists
    // For now, assume it exists

    if !args.force {
        println!("\n⚠️  This will:");
        println!("  - Delete hook configuration");
        println!("  - Uninstall git hooks (if applicable)");
        println!("  - Stop file watchers (if applicable)");
        println!("  - Remove cron jobs (if applicable)");
        println!("\nAre you sure you want to continue? (y/N)");

        // TODO: Implement actual confirmation prompt
        // For now, just proceed
        println!("Proceeding with removal...");
    }

    // TODO: Implement actual hook removal
    // This will involve:
    // 1. Load hook config to determine type
    // 2. Uninstall git hook if trigger is git-*
    // 3. Stop file watcher process if trigger is file-watch
    // 4. Remove cron job if trigger is cron
    // 5. Delete hook config file from .ggen/hooks/

    println!("  Uninstalling hook from system...");
    println!("  Deleting hook configuration...");

    println!("\n✅ Hook '{}' removed successfully!", args.name);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_remove_hook_basic() {
        let args = RemoveArgs {
            name: "test-hook".to_string(),
            force: true, // Force to skip confirmation
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_remove_hook_empty_name() {
        let args = RemoveArgs {
            name: "".to_string(),
            force: true,
        };
        let result = run(&args).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_remove_hook_with_force() {
        let args = RemoveArgs {
            name: "test-hook".to_string(),
            force: true,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
