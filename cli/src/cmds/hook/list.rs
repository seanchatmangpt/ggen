////! List knowledge hooks with filtering and JSON output.
//!
//! This module provides commands to list all configured hooks,
//! with filters for active, disabled, or all hooks.
//!
//! # Examples
//!
//! ```bash
//! # List all hooks
//! ggen hook list
//!
//! # List only active hooks
//! ggen hook list --active
//!
//! # List only disabled hooks
//! ggen hook list --disabled
//!
//! # Output as JSON
//! ggen hook list --json
//! ```

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

#[derive(Args, Debug)]
pub struct ListArgs {
    /// Show only active hooks
    #[arg(long)]
    pub active: bool,

    /// Show only disabled hooks
    #[arg(long)]
    pub disabled: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookListItem {
    pub name: String,
    pub trigger: String,
    pub template: String,
    pub enabled: bool,
    pub last_run: Option<String>,
}

/// Main entry point for `ggen hook list`
pub async fn run(args: &ListArgs) -> Result<()> {
    // Validate mutually exclusive flags
    if args.active && args.disabled {
        return Err(ggen_utils::error::Error::new(
            "Cannot use both --active and --disabled flags together",
        ));
    }

    // TODO: Load hooks from .ggen/hooks/ directory
    // For now, return a mock list for demonstration

    let hooks = vec![
        HookListItem {
            name: "pre-commit".to_string(),
            trigger: "git-pre-commit".to_string(),
            template: "graph-gen.tmpl".to_string(),
            enabled: true,
            last_run: Some("2025-10-09T10:30:00Z".to_string()),
        },
        HookListItem {
            name: "post-merge".to_string(),
            trigger: "git-post-merge".to_string(),
            template: "sync-graph.tmpl".to_string(),
            enabled: true,
            last_run: Some("2025-10-08T14:20:00Z".to_string()),
        },
        HookListItem {
            name: "nightly-rebuild".to_string(),
            trigger: "cron".to_string(),
            template: "full-rebuild.tmpl".to_string(),
            enabled: false,
            last_run: None,
        },
    ];

    // Filter based on flags
    let filtered_hooks: Vec<&HookListItem> = hooks
        .iter()
        .filter(|hook| {
            if args.active {
                hook.enabled
            } else if args.disabled {
                !hook.enabled
            } else {
                true // Show all
            }
        })
        .collect();

    if args.json {
        let json = serde_json::to_string_pretty(&filtered_hooks).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("JSON serialization failed: {}", e))
        })?;
        println!("{}", json);
        return Ok(());
    }

    // Human-readable output
    println!("📋 Knowledge Hooks:");
    println!();

    if filtered_hooks.is_empty() {
        println!("  No hooks found.");
        if args.active {
            println!("  Try running without --active to see all hooks.");
        } else if args.disabled {
            println!("  Try running without --disabled to see all hooks.");
        }
        return Ok(());
    }

    for hook in filtered_hooks {
        let status_icon = if hook.enabled { "✅" } else { "⏸️" };
        println!("{} {}", status_icon, hook.name);
        println!("   Trigger: {}", hook.trigger);
        println!("   Template: {}", hook.template);
        if let Some(last_run) = &hook.last_run {
            println!("   Last run: {}", last_run);
        } else {
            println!("   Last run: never");
        }
        println!();
    }

    let total = hooks.len();
    let active_count = hooks.iter().filter(|h| h.enabled).count();
    let disabled_count = total - active_count;

    println!(
        "Total: {} ({} active, {} disabled)",
        total, active_count, disabled_count
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_all_hooks() {
        let args = ListArgs {
            active: false,
            disabled: false,
            json: false,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_mutually_exclusive_flags() {
        let args = ListArgs {
            active: true,
            disabled: true,
            json: false,
        };
        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Cannot use both --active and --disabled"));
    }

    #[tokio::test]
    async fn test_list_json_output() {
        let args = ListArgs {
            active: false,
            disabled: false,
            json: true,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
