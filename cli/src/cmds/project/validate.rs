//! Validate plans and generated output.
//!
//! This module provides validation capabilities for generation plans and
//! output files, ensuring they meet schema requirements, contain valid
//! syntax, and follow project conventions.
//!
//! # Examples
//!
//! ```bash
//! ggen project validate plan.json
//! ggen project validate generated/ --recursive
//! ggen project validate output.rs --schema rust-file.schema.json
//! ggen project validate plan.yaml --verbose --json
//! ```
//!
//! # Errors
//!
//! Returns errors if validation fails, files are malformed, or schema
//! constraints are violated.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path, PathBuf};

#[derive(Args, Debug)]
pub struct ValidateArgs {
    /// Path to plan file or directory to validate
    pub path: PathBuf,

    /// Schema file for validation (optional)
    #[arg(long, short = 's')]
    pub schema: Option<PathBuf>,

    /// Validate directories recursively
    #[arg(long, short = 'r')]
    pub recursive: bool,

    /// File pattern for recursive validation (e.g., "*.rs")
    #[arg(long, default_value = "*")]
    pub pattern: String,

    /// Strict validation mode (fail on warnings)
    #[arg(long)]
    pub strict: bool,

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

/// Detect file type from extension
fn detect_validation_type(path: &Path) -> Result<String> {
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");

    let validation_type = match extension {
        "json" => "json-plan",
        "yaml" | "yml" => "yaml-plan",
        "toml" => "toml-plan",
        "rs" => "rust-file",
        "ts" | "tsx" => "typescript-file",
        "js" | "jsx" => "javascript-file",
        "py" => "python-file",
        _ => "generic",
    };

    Ok(validation_type.to_string())
}

/// Main entry point for `ggen project validate`
pub async fn run(args: &ValidateArgs) -> Result<()> {
    // Validate inputs
    validate_path(&args.path)?;

    if let Some(schema_path) = &args.schema {
        validate_path(schema_path)?;
        if !schema_path.exists() {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Schema file not found: {}",
                schema_path.display()
            )));
        }
    }

    println!("✅ Validating files...");

    // Check if path exists
    if !args.path.exists() {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Path not found: {}",
            args.path.display()
        )));
    }

    // Detect validation type
    let validation_type = detect_validation_type(&args.path)?;

    if args.verbose {
        println!("📋 Validation type: {}", validation_type);
    }

    // Build command for cargo make validate
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-validate"]);
    cmd.arg("--path").arg(&args.path);
    cmd.arg("--type").arg(&validation_type);

    if let Some(schema_path) = &args.schema {
        cmd.arg("--schema").arg(schema_path);
    }

    if args.recursive {
        cmd.arg("--recursive");
    }

    if !args.pattern.is_empty() && args.pattern != "*" {
        cmd.arg("--pattern").arg(&args.pattern);
    }

    if args.strict {
        cmd.arg("--strict");
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
            "Validation failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    if args.json {
        println!("{}", stdout);
    } else {
        println!("{}", stdout);
        println!("✅ Validation completed successfully");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_validation_type_json() {
        let path = Path::new("plan.json");
        let result = detect_validation_type(path).unwrap();
        assert_eq!(result, "json-plan");
    }

    #[test]
    fn test_detect_validation_type_yaml() {
        let path = Path::new("config.yaml");
        let result = detect_validation_type(path).unwrap();
        assert_eq!(result, "yaml-plan");
    }

    #[test]
    fn test_detect_validation_type_rust() {
        let path = Path::new("main.rs");
        let result = detect_validation_type(path).unwrap();
        assert_eq!(result, "rust-file");
    }

    #[test]
    fn test_detect_validation_type_generic() {
        let path = Path::new("README.md");
        let result = detect_validation_type(path).unwrap();
        assert_eq!(result, "generic");
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
    async fn test_validate_args_path_not_found() {
        let args = ValidateArgs {
            path: PathBuf::from("nonexistent.json"),
            schema: None,
            recursive: false,
            pattern: "*".to_string(),
            strict: false,
            verbose: false,
            json: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Path not found"));
    }
}
