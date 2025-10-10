//! Idempotent code injection into existing files.
//!
//! This module implements Pattern 091: IDEMPOTENT INJECTION by providing
//! safe, repeatable code insertion at specific anchor points. Injections
//! are tracked to ensure they only occur once, making generation idempotent.
//!
//! # Examples
//!
//! ```bash
//! ggen project inject "src/mod.rs" --anchor "// IMPORTS" --content "use foo;"
//! ggen project inject "main.rs" --anchor "fn main()" --content "println!(\"hello\");" --after
//! ggen project inject "config.toml" --anchor "[dependencies]" --content "serde = \"1.0\"" --dry-run
//! ```
//!
//! # Errors
//!
//! Returns errors if file paths are invalid, anchor points are not found,
//! or file write operations fail.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path, PathBuf};

#[derive(Args, Debug)]
pub struct InjectArgs {
    /// File path to inject content into
    pub path: PathBuf,

    /// Anchor point (line content or regex pattern) where content will be injected
    #[arg(long, short = 'a')]
    pub anchor: String,

    /// Content to inject
    #[arg(long, short = 'c')]
    pub content: String,

    /// Inject after the anchor (default: before)
    #[arg(long)]
    pub after: bool,

    /// Use regex for anchor matching
    #[arg(long)]
    pub regex: bool,

    /// Perform dry-run without modifying files
    #[arg(long)]
    pub dry_run: bool,

    /// Show verbose output
    #[arg(long)]
    pub verbose: bool,

    /// Output results in JSON format
    #[arg(long)]
    pub json: bool,

    /// Force injection even if content already exists
    #[arg(long, short = 'f')]
    pub force: bool,
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

/// Validate injection inputs
fn validate_injection_input(args: &InjectArgs) -> Result<()> {
    if args.anchor.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Anchor point cannot be empty",
        ));
    }

    if args.content.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Injection content cannot be empty",
        ));
    }

    if args.anchor.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Anchor point too long (max 1000 characters)",
        ));
    }

    if args.content.len() > 10000 {
        return Err(ggen_utils::error::Error::new(
            "Injection content too long (max 10000 characters)",
        ));
    }

    Ok(())
}

/// Main entry point for `ggen project inject`
pub async fn run(args: &InjectArgs) -> Result<()> {
    // Validate inputs
    validate_path(&args.path)?;
    validate_injection_input(args)?;

    println!("💉 Injecting code into file...");

    if args.dry_run {
        println!("🔍 Dry run mode - no files will be modified");
    }

    // Check if file exists
    if !args.path.exists() {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "File not found: {}",
            args.path.display()
        )));
    }

    // Build command for cargo make inject
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-inject"]);
    cmd.arg("--path").arg(&args.path);
    cmd.arg("--anchor").arg(&args.anchor);
    cmd.arg("--content").arg(&args.content);

    if args.after {
        cmd.arg("--after");
    }

    if args.regex {
        cmd.arg("--regex");
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

    if args.force {
        cmd.arg("--force");
    }

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Injection failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    if args.json {
        println!("{}", stdout);
    } else {
        println!("{}", stdout);
        if !args.dry_run {
            println!("✅ Content injected successfully");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_injection_input_valid() {
        let args = InjectArgs {
            path: PathBuf::from("test.rs"),
            anchor: "// IMPORTS".to_string(),
            content: "use foo;".to_string(),
            after: false,
            regex: false,
            dry_run: false,
            verbose: false,
            json: false,
            force: false,
        };

        assert!(validate_injection_input(&args).is_ok());
    }

    #[test]
    fn test_validate_injection_input_empty_anchor() {
        let args = InjectArgs {
            path: PathBuf::from("test.rs"),
            anchor: "".to_string(),
            content: "use foo;".to_string(),
            after: false,
            regex: false,
            dry_run: false,
            verbose: false,
            json: false,
            force: false,
        };

        let result = validate_injection_input(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Anchor point cannot be empty"));
    }

    #[test]
    fn test_validate_injection_input_empty_content() {
        let args = InjectArgs {
            path: PathBuf::from("test.rs"),
            anchor: "// IMPORTS".to_string(),
            content: "".to_string(),
            after: false,
            regex: false,
            dry_run: false,
            verbose: false,
            json: false,
            force: false,
        };

        let result = validate_injection_input(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Injection content cannot be empty"));
    }

    #[test]
    fn test_validate_path_safe() {
        let path = Path::new("src/main.rs");
        assert!(validate_path(path).is_ok());
    }

    #[test]
    fn test_validate_path_traversal() {
        let path = Path::new("../../etc/passwd");
        assert!(validate_path(path).is_err());
    }

    #[tokio::test]
    async fn test_inject_args_file_not_found() {
        let args = InjectArgs {
            path: PathBuf::from("nonexistent.rs"),
            anchor: "// ANCHOR".to_string(),
            content: "test".to_string(),
            after: false,
            regex: false,
            dry_run: false,
            verbose: false,
            json: false,
            force: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("File not found"));
    }
}
