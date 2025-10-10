//! Golden file snapshot testing for templates.
//!
//! This module provides snapshot testing capabilities to verify that template
//! generation produces expected output. It implements Pattern 009: PROJECT PLAN
//! by enabling regression testing through golden file comparisons.
//!
//! # Examples
//!
//! ```bash
//! ggen project test "template.tmpl" --golden expected/
//! ggen project test "rust-cli.tmpl" --golden golden/ --update
//! ggen project test "*.tmpl" --golden snapshots/ --json
//! ```
//!
//! # Errors
//!
//! Returns errors if template rendering fails, golden files are missing,
//! or snapshot comparisons detect unexpected differences.

use clap::Args;
use ggen_utils::error::Result;
use std::fs;
use std::path::{Component, Path, PathBuf};

#[derive(Args, Debug)]
pub struct TestArgs {
    /// Template reference (e.g., "template.tmpl" or "gpack:path/to.tmpl")
    pub template_ref: String,

    /// Directory containing golden/expected files
    #[arg(long, short = 'g')]
    pub golden: PathBuf,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Update golden files instead of comparing
    #[arg(long, short = 'u')]
    pub update: bool,

    /// Show detailed diff when tests fail
    #[arg(long)]
    pub verbose: bool,

    /// Output results in JSON format
    #[arg(long)]
    pub json: bool,

    /// Perform dry-run without writing updates
    #[arg(long)]
    pub dry_run: bool,
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

/// Main entry point for `ggen project test`
pub async fn run(args: &TestArgs) -> Result<()> {
    // Validate inputs
    if args.template_ref.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    validate_path(&args.golden)?;
    let _vars = parse_vars(&args.vars)?;

    println!("🧪 Running golden file snapshot tests...");

    if args.update {
        println!("📝 Update mode: Golden files will be updated");
    } else {
        println!("🔍 Compare mode: Checking against golden files");
    }

    // Ensure golden directory exists
    if !args.golden.exists() && !args.update {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Golden directory not found: {}",
            args.golden.display()
        )));
    }

    if args.update && !args.dry_run {
        fs::create_dir_all(&args.golden).map_err(ggen_utils::error::Error::from)?;
    }

    // Build command for cargo make test
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-test"]);
    cmd.arg("--template").arg(&args.template_ref);
    cmd.arg("--golden").arg(&args.golden);

    for var in &args.vars {
        cmd.arg("--var").arg(var);
    }

    if args.update {
        cmd.arg("--update");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.dry_run {
        cmd.arg("--dry-run");
    }

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Snapshot test failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    if args.json {
        println!("{}", stdout);
    } else {
        println!("{}", stdout);
        if args.update {
            println!("✅ Golden files updated successfully");
        } else {
            println!("✅ All snapshot tests passed");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_vars_valid() {
        let vars = vec!["name=test".to_string(), "version=1.0".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(result.get("name"), Some(&"test".to_string()));
        assert_eq!(result.get("version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_parse_vars_invalid() {
        let vars = vec!["invalid".to_string()];
        let result = parse_vars(&vars);

        assert!(result.is_err());
    }

    #[test]
    fn test_validate_path_safe() {
        let path = Path::new("golden/snapshots");
        assert!(validate_path(path).is_ok());
    }

    #[test]
    fn test_validate_path_traversal() {
        let path = Path::new("../etc/passwd");
        assert!(validate_path(path).is_err());
    }

    #[tokio::test]
    async fn test_test_args_validation() {
        let args = TestArgs {
            template_ref: "".to_string(),
            golden: PathBuf::from("golden"),
            vars: vec![],
            update: false,
            verbose: false,
            json: false,
            dry_run: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Template reference cannot be empty"));
    }
}
