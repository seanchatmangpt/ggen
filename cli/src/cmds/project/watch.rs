//! Continuous generation mode with file watching.
//!
//! This module implements Pattern 022: DELTA-DRIVEN by providing a watch
//! mode that monitors template files for changes and automatically regenerates
//! outputs. This enables rapid development iteration with live reloading.
//!
//! # Examples
//!
//! ```bash
//! ggen project watch "*.tmpl" --target src/
//! ggen project watch "templates/" --target output/ --recursive
//! ggen project watch "api.tmpl" --target api/ --debounce 1000 --var env=dev
//! ```
//!
//! # Errors
//!
//! Returns errors if watch setup fails, template paths are invalid,
//! or generation errors occur during watch cycles.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path, PathBuf};

#[derive(Args, Debug)]
pub struct WatchArgs {
    /// Template path or pattern to watch
    pub pattern: String,

    /// Target directory for generated output
    #[arg(long, short = 't')]
    pub target: PathBuf,

    /// Variables to pass to templates (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Watch directories recursively
    #[arg(long, short = 'r')]
    pub recursive: bool,

    /// Debounce delay in milliseconds (default: 500ms)
    #[arg(long, default_value = "500")]
    pub debounce: u64,

    /// Clear target directory on each regeneration
    #[arg(long)]
    pub clear: bool,

    /// Show verbose output
    #[arg(long)]
    pub verbose: bool,

    /// Perform dry-run without writing files
    #[arg(long)]
    pub dry_run: bool,

    /// Poll interval in milliseconds for fallback polling
    #[arg(long, default_value = "1000")]
    pub poll_interval: u64,
}

/// Validate path to prevent directory traversal attacks
fn validate_path(path: &Path) -> Result<()> {
    if path.components().any(|c| matches!(c, Component::ParentDir)) {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }
    Ok(())
}

/// Parse key=value pairs into HashMap
fn parse_vars(vars: &[String]) -> Result<std::collections::HashMap<String, String>> {
    let mut map = std::collections::HashMap::new();
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

/// Validate watch configuration
fn validate_watch_config(args: &WatchArgs) -> Result<()> {
    if args.pattern.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Watch pattern cannot be empty",
        ));
    }

    if args.debounce == 0 {
        return Err(ggen_utils::error::Error::new(
            "Debounce delay must be greater than 0",
        ));
    }

    if args.debounce > 10000 {
        return Err(ggen_utils::error::Error::new(
            "Debounce delay too large (max 10000ms)",
        ));
    }

    if args.poll_interval == 0 {
        return Err(ggen_utils::error::Error::new(
            "Poll interval must be greater than 0",
        ));
    }

    Ok(())
}

/// Main entry point for `ggen project watch`
pub async fn run(args: &WatchArgs) -> Result<()> {
    // Validate inputs
    validate_path(&args.target)?;
    validate_watch_config(args)?;
    let _vars = parse_vars(&args.vars)?;

    println!("👁️  Starting watch mode...");
    println!("📁 Watching: {}", args.pattern);
    println!("🎯 Target: {}", args.target.display());

    if args.dry_run {
        println!("🔍 Dry run mode - no files will be written");
    }

    // Create target directory if it doesn't exist
    if !args.target.exists() && !args.dry_run {
        std::fs::create_dir_all(&args.target).map_err(ggen_utils::error::Error::from)?;
    }

    // Build command for cargo make watch
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-watch"]);
    cmd.arg("--pattern").arg(&args.pattern);
    cmd.arg("--target").arg(&args.target);
    cmd.arg("--debounce").arg(args.debounce.to_string());
    cmd.arg("--poll-interval")
        .arg(args.poll_interval.to_string());

    for var in &args.vars {
        cmd.arg("--var").arg(var);
    }

    if args.recursive {
        cmd.arg("--recursive");
    }

    if args.clear {
        cmd.arg("--clear");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.dry_run {
        cmd.arg("--dry-run");
    }

    println!("⏳ Press Ctrl+C to stop watching...\n");

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Watch mode failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_vars_valid() {
        let vars = vec!["env=dev".to_string(), "port=3000".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(result.get("env"), Some(&"dev".to_string()));
        assert_eq!(result.get("port"), Some(&"3000".to_string()));
    }

    #[test]
    fn test_parse_vars_invalid() {
        let vars = vec!["invalid".to_string()];
        let result = parse_vars(&vars);

        assert!(result.is_err());
    }

    #[test]
    fn test_validate_watch_config_valid() {
        let args = WatchArgs {
            pattern: "*.tmpl".to_string(),
            target: PathBuf::from("output"),
            vars: vec![],
            recursive: false,
            debounce: 500,
            clear: false,
            verbose: false,
            dry_run: false,
            poll_interval: 1000,
        };

        assert!(validate_watch_config(&args).is_ok());
    }

    #[test]
    fn test_validate_watch_config_empty_pattern() {
        let args = WatchArgs {
            pattern: "".to_string(),
            target: PathBuf::from("output"),
            vars: vec![],
            recursive: false,
            debounce: 500,
            clear: false,
            verbose: false,
            dry_run: false,
            poll_interval: 1000,
        };

        let result = validate_watch_config(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Watch pattern cannot be empty"));
    }

    #[test]
    fn test_validate_watch_config_zero_debounce() {
        let args = WatchArgs {
            pattern: "*.tmpl".to_string(),
            target: PathBuf::from("output"),
            vars: vec![],
            recursive: false,
            debounce: 0,
            clear: false,
            verbose: false,
            dry_run: false,
            poll_interval: 1000,
        };

        let result = validate_watch_config(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Debounce delay must be greater than 0"));
    }

    #[test]
    fn test_validate_path_safe() {
        let path = Path::new("output/generated");
        assert!(validate_path(path).is_ok());
    }

    #[test]
    fn test_validate_path_traversal() {
        let path = Path::new("../../etc/passwd");
        assert!(validate_path(path).is_err());
    }
}
