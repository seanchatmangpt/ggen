//! Add freeze blocks to generated files for immutability.
//!
//! This module implements Pattern 015: IMMUTABILITY FIRST by adding freeze
//! markers to generated code sections that should not be modified by
//! subsequent generations. Frozen blocks are preserved during regeneration.
//!
//! # Examples
//!
//! ```bash
//! ggen project freeze "src/main.rs" --blocks "impl,business_logic"
//! ggen project freeze "src/" --blocks "custom" --recursive
//! ggen project freeze "app.ts" --blocks "config,routes" --dry-run
//! ```
//!
//! # Errors
//!
//! Returns errors if file paths are invalid, blocks cannot be identified,
//! or file write operations fail.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path, PathBuf};

#[derive(Args, Debug)]
pub struct FreezeArgs {
    /// File or directory path to add freeze blocks to
    pub path: PathBuf,

    /// Block identifiers to freeze (comma-separated)
    #[arg(long, short = 'b')]
    pub blocks: String,

    /// Process directories recursively
    #[arg(long, short = 'r')]
    pub recursive: bool,

    /// File pattern for recursive processing (e.g., "*.rs")
    #[arg(long, default_value = "*")]
    pub pattern: String,

    /// Perform dry-run without modifying files
    #[arg(long)]
    pub dry_run: bool,

    /// Show verbose output
    #[arg(long)]
    pub verbose: bool,

    /// Output results in JSON format
    #[arg(long)]
    pub json: bool,
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

/// Validate block identifiers
fn validate_blocks(blocks: &str) -> Result<Vec<String>> {
    if blocks.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Block identifiers cannot be empty",
        ));
    }

    let block_list: Vec<String> = blocks
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if block_list.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "At least one valid block identifier required",
        ));
    }

    Ok(block_list)
}

/// Main entry point for `ggen project freeze`
pub async fn run(args: &FreezeArgs) -> Result<()> {
    // Validate inputs
    validate_path(&args.path)?;
    let block_list = validate_blocks(&args.blocks)?;

    println!("🧊 Adding freeze blocks to files...");

    if args.dry_run {
        println!("🔍 Dry run mode - no files will be modified");
    }

    // Check if path exists
    if !args.path.exists() {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Path not found: {}",
            args.path.display()
        )));
    }

    // Build command for cargo make freeze
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-freeze"]);
    cmd.arg("--path").arg(&args.path);
    cmd.arg("--blocks").arg(&args.blocks);

    if args.recursive {
        cmd.arg("--recursive");
    }

    if !args.pattern.is_empty() && args.pattern != "*" {
        cmd.arg("--pattern").arg(&args.pattern);
    }

    if args.dry_run {
        cmd.arg("--dry-run");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.json {
        cmd.arg("--json");
    }

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Freeze operation failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    if args.json {
        println!("{}", stdout);
    } else {
        println!("{}", stdout);
        if !args.dry_run {
            println!("✅ Freeze blocks added for: {}", block_list.join(", "));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_blocks_valid() {
        let result = validate_blocks("impl,business_logic,config").unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "impl");
        assert_eq!(result[1], "business_logic");
        assert_eq!(result[2], "config");
    }

    #[test]
    fn test_validate_blocks_empty() {
        let result = validate_blocks("");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_blocks_whitespace() {
        let result = validate_blocks("  impl  , config  ").unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], "impl");
        assert_eq!(result[1], "config");
    }

    #[test]
    fn test_validate_path_safe() {
        let path = Path::new("src/main.rs");
        assert!(validate_path(path).is_ok());
    }

    #[test]
    fn test_validate_path_traversal() {
        let path = Path::new("../etc/passwd");
        assert!(validate_path(path).is_err());
    }

    #[tokio::test]
    async fn test_freeze_args_validation() {
        let args = FreezeArgs {
            path: PathBuf::from("nonexistent.rs"),
            blocks: "impl".to_string(),
            recursive: false,
            pattern: "*".to_string(),
            dry_run: false,
            verbose: false,
            json: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Path not found"));
    }
}
