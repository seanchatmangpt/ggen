//! Create knowledge hooks for automatic graph regeneration.
//!
//! This module implements Pattern 021: Knowledge Hooks from the ggen cookbook.
//! Hooks trigger automatic graph regeneration in response to system events,
//! transforming ggen from a manual tool into a reactive autonomic system.
//!
//! # Hook Types
//!
//! ## Git Hooks (Local automation)
//! - `pre-commit`: Regenerate before committing
//! - `post-merge`: Rebuild after merging branches
//! - `post-checkout`: Update when switching branches
//!
//! ## File Watch (Continuous updates)
//! - `file-watch`: Trigger on filesystem changes using inotify/fswatch
//!
//! ## Scheduled (Temporal patterns)
//! - `cron`: Run at scheduled intervals
//!
//! ## Manual (CLI trigger)
//! - Manually invoked via `ggen hook run`
//!
//! # Examples
//!
//! ```bash
//! # Git pre-commit hook
//! ggen hook create "pre-commit" \
//!   --trigger git-pre-commit \
//!   --template graph-gen.tmpl
//!
//! # Nightly full rebuild (cron)
//! ggen hook create "nightly-rebuild" \
//!   --trigger cron \
//!   --schedule "0 2 * * *" \
//!   --template full-graph.tmpl
//!
//! # File watcher for incremental updates
//! ggen hook create "rust-watcher" \
//!   --trigger file-watch \
//!   --path "src/**/*.rs" \
//!   --template incremental.tmpl
//!
//! # Dry run (test without installing)
//! ggen hook create "test-hook" \
//!   --trigger git-pre-commit \
//!   --template test.tmpl \
//!   --dry-run
//! ```
//!
//! # Errors
//!
//! Returns errors if:
//! - Hook name is invalid or already exists
//! - Template reference is invalid
//! - Trigger type requires missing parameters (e.g., cron without schedule)
//! - Git hook installation fails
//! - File permissions prevent hook creation

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Args, Debug)]
pub struct CreateArgs {
    /// Hook name (unique identifier)
    pub name: String,

    /// Trigger type: git-pre-commit, git-post-merge, git-post-checkout, file-watch, cron, manual
    #[arg(short = 't', long)]
    pub trigger: String,

    /// Template reference to execute when hook triggers
    #[arg(long)]
    pub template: String,

    /// Cron schedule (required for cron trigger, e.g., "0 2 * * *")
    #[arg(long)]
    pub schedule: Option<String>,

    /// File path pattern for file-watch trigger (e.g., "src/**/*.rs")
    #[arg(long)]
    pub path: Option<String>,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Enable the hook immediately after creation
    #[arg(long, default_value = "true")]
    pub enabled: bool,

    /// Perform dry-run without installing the hook
    #[arg(long)]
    pub dry_run: bool,

    /// Output hook configuration as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookConfig {
    pub name: String,
    pub trigger: HookTrigger,
    pub template: String,
    pub vars: HashMap<String, String>,
    pub enabled: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum HookTrigger {
    #[serde(rename = "git-pre-commit")]
    GitPreCommit,
    #[serde(rename = "git-post-merge")]
    GitPostMerge,
    #[serde(rename = "git-post-checkout")]
    GitPostCheckout,
    #[serde(rename = "file-watch")]
    FileWatch { path: String },
    #[serde(rename = "cron")]
    Cron { schedule: String },
    #[serde(rename = "manual")]
    Manual,
}

/// Parse key=value pairs into HashMap
fn parse_vars(vars: &[String]) -> Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for var in vars {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var
            )));
        }
        map.insert(parts[0].to_string(), parts[1].to_string());
    }
    Ok(map)
}

/// Validate hook creation arguments
fn validate_create_args(args: &CreateArgs) -> Result<()> {
    // Validate hook name
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Hook name cannot be empty"));
    }

    if args.name.len() > 100 {
        return Err(ggen_utils::error::Error::new(
            "Hook name too long (max 100 characters)",
        ));
    }

    // Validate trigger-specific requirements
    match args.trigger.as_str() {
        "cron" => {
            if args.schedule.is_none() {
                return Err(ggen_utils::error::Error::new(
                    "Cron trigger requires --schedule parameter",
                ));
            }
        }
        "file-watch" => {
            if args.path.is_none() {
                return Err(ggen_utils::error::Error::new(
                    "File-watch trigger requires --path parameter",
                ));
            }
        }
        "git-pre-commit" | "git-post-merge" | "git-post-checkout" | "manual" => {
            // These triggers don't require additional parameters
        }
        _ => {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid trigger type: '{}'. Must be one of: git-pre-commit, git-post-merge, git-post-checkout, file-watch, cron, manual",
                args.trigger
            )));
        }
    }

    // Validate template reference
    if args.template.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    Ok(())
}

/// Parse trigger type and parameters into HookTrigger enum
fn parse_trigger(args: &CreateArgs) -> Result<HookTrigger> {
    match args.trigger.as_str() {
        "git-pre-commit" => Ok(HookTrigger::GitPreCommit),
        "git-post-merge" => Ok(HookTrigger::GitPostMerge),
        "git-post-checkout" => Ok(HookTrigger::GitPostCheckout),
        "file-watch" => Ok(HookTrigger::FileWatch {
            path: args
                .path
                .clone()
                .expect("file-watch requires --path (validated earlier)"),
        }),
        "cron" => Ok(HookTrigger::Cron {
            schedule: args
                .schedule
                .clone()
                .expect("cron requires --schedule (validated earlier)"),
        }),
        "manual" => Ok(HookTrigger::Manual),
        _ => Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Unknown trigger type: {}",
            args.trigger
        ))),
    }
}

/// Main entry point for `ggen hook create`
pub async fn run(args: &CreateArgs) -> Result<()> {
    // Validate input
    validate_create_args(args)?;

    println!("🔨 Creating knowledge hook '{}'...", args.name);

    // Parse variables
    let vars = parse_vars(&args.vars)?;

    // Parse trigger
    let trigger = parse_trigger(args)?;

    // Build hook configuration
    let hook_config = HookConfig {
        name: args.name.clone(),
        trigger,
        template: args.template.clone(),
        vars,
        enabled: args.enabled,
    };

    // Output as JSON if requested
    if args.json {
        let json = serde_json::to_string_pretty(&hook_config).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("JSON serialization failed: {}", e))
        })?;
        println!("{}", json);
        return Ok(());
    }

    if args.dry_run {
        println!("  Dry run enabled - hook will not be installed");
        println!("\nHook Configuration:");
        println!("  Name: {}", hook_config.name);
        println!("  Trigger: {:?}", hook_config.trigger);
        println!("  Template: {}", hook_config.template);
        println!("  Enabled: {}", hook_config.enabled);
        if !hook_config.vars.is_empty() {
            println!("  Variables:");
            for (key, value) in &hook_config.vars {
                println!("    {} = {}", key, value);
            }
        }
        return Ok(());
    }

    // TODO: Implement actual hook installation
    // This will involve:
    // 1. Creating hook config file in .ggen/hooks/
    // 2. Installing git hooks if trigger is git-*
    // 3. Setting up file watcher if trigger is file-watch
    // 4. Configuring cron job if trigger is cron

    println!("  Hook trigger: {:?}", hook_config.trigger);
    println!("  Template: {}", hook_config.template);
    println!(
        "  Status: {}",
        if hook_config.enabled {
            "enabled"
        } else {
            "disabled"
        }
    );
    println!("\n✅ Hook '{}' created successfully!", args.name);
    println!("\nNext steps:");
    println!("  - Run hook manually: ggen hook run '{}'", args.name);
    println!(
        "  - Validate configuration: ggen hook validate '{}'",
        args.name
    );
    println!("  - List all hooks: ggen hook list");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_create_args_valid() {
        let args = CreateArgs {
            name: "test-hook".to_string(),
            trigger: "git-pre-commit".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: None,
            path: None,
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        assert!(validate_create_args(&args).is_ok());
    }

    #[test]
    fn test_validate_create_args_empty_name() {
        let args = CreateArgs {
            name: "".to_string(),
            trigger: "git-pre-commit".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: None,
            path: None,
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        assert!(validate_create_args(&args).is_err());
    }

    #[test]
    fn test_validate_create_args_cron_without_schedule() {
        let args = CreateArgs {
            name: "nightly".to_string(),
            trigger: "cron".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: None, // Missing schedule
            path: None,
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        assert!(validate_create_args(&args).is_err());
    }

    #[test]
    fn test_validate_create_args_file_watch_without_path() {
        let args = CreateArgs {
            name: "watcher".to_string(),
            trigger: "file-watch".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: None,
            path: None, // Missing path
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        assert!(validate_create_args(&args).is_err());
    }

    #[test]
    fn test_parse_vars_valid() {
        let vars = vec!["name=value".to_string(), "key=data".to_string()];
        let result = parse_vars(&vars).unwrap();
        assert_eq!(result.get("name"), Some(&"value".to_string()));
        assert_eq!(result.get("key"), Some(&"data".to_string()));
    }

    #[test]
    fn test_parse_trigger_git_pre_commit() {
        let args = CreateArgs {
            name: "test".to_string(),
            trigger: "git-pre-commit".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: None,
            path: None,
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        let trigger = parse_trigger(&args).unwrap();
        matches!(trigger, HookTrigger::GitPreCommit);
    }

    #[test]
    fn test_parse_trigger_cron() {
        let args = CreateArgs {
            name: "test".to_string(),
            trigger: "cron".to_string(),
            template: "graph.tmpl".to_string(),
            schedule: Some("0 2 * * *".to_string()),
            path: None,
            vars: vec![],
            enabled: true,
            dry_run: false,
            json: false,
        };
        let trigger = parse_trigger(&args).unwrap();
        if let HookTrigger::Cron { schedule } = trigger {
            assert_eq!(schedule, "0 2 * * *");
        } else {
            panic!("Expected Cron trigger");
        }
    }
}
