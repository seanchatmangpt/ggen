//! Security vulnerability scanning and analysis.
//!
//! This module provides functionality to scan codebases for security vulnerabilities,
//! analyze dependencies for known CVEs, and perform security audits. It integrates with
//! cargo-make to perform comprehensive security analysis.
//!
//! # Examples
//!
//! ```bash
//! ggen audit security scan --path ./src --verbose
//! ggen audit security cve --package "serde" --version "1.0.0"
//! ggen audit security audit --all
//! ```
//!
//! # Errors
//!
//! Returns errors if the underlying cargo-make commands fail or if
//! the specified paths don't exist.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct SecurityArgs {
    #[command(subcommand)]
    pub action: SecurityAction,
}

#[derive(Subcommand, Debug)]
pub enum SecurityAction {
    /// Scan for security vulnerabilities
    Scan(ScanArgs),

    /// Check dependencies for known vulnerabilities
    Dependencies(DependenciesArgs),

    /// Audit configuration files
    Config(ConfigArgs),
}

#[derive(Args, Debug)]
pub struct ScanArgs {
    /// Directory to scan [default: current directory]
    #[arg(long, default_value = ".")]
    pub path: String,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Show detailed information
    #[arg(long)]
    pub verbose: bool,

    /// Fix automatically where possible
    #[arg(long)]
    pub fix: bool,
}

#[derive(Args, Debug)]
pub struct DependenciesArgs {
    /// Check only direct dependencies
    #[arg(long)]
    pub direct_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Update vulnerable dependencies
    #[arg(long)]
    pub update: bool,
}

#[derive(Args, Debug)]
pub struct ConfigArgs {
    /// Configuration file to audit
    #[arg(long)]
    pub file: Option<String>,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Fix configuration issues
    #[arg(long)]
    pub fix: bool,
}

pub async fn run(args: &SecurityArgs) -> Result<()> {
    match &args.action {
        SecurityAction::Scan(scan_args) => scan_security(scan_args).await,
        SecurityAction::Dependencies(deps_args) => check_dependencies(deps_args).await,
        SecurityAction::Config(config_args) => audit_config(config_args).await,
    }
}

/// Validate and sanitize path input
fn validate_path(path: &str) -> Result<()> {
    // Validate path is not empty
    if path.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Path cannot be empty"));
    }

    // Validate path length
    if path.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Path too long (max 1000 characters)",
        ));
    }

    // Basic path traversal protection
    if path.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: path cannot contain '..'",
        ));
    }

    Ok(())
}

/// Validate and sanitize file path input (if provided)
fn validate_file_path(file: &Option<String>) -> Result<()> {
    if let Some(file) = file {
        // Validate file path is not empty
        if file.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("File path cannot be empty"));
        }

        // Validate file path length
        if file.len() > 1000 {
            return Err(ggen_utils::error::Error::new(
                "File path too long (max 1000 characters)",
            ));
        }

        // Basic path traversal protection
        if file.contains("..") {
            return Err(ggen_utils::error::Error::new(
                "Path traversal detected: file path cannot contain '..'",
            ));
        }

        // Validate file path format (basic pattern check)
        if !file.chars().all(|c| {
            c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\'
        }) {
            return Err(ggen_utils::error::Error::new(
                "Invalid file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
            ));
        }
    }

    Ok(())
}

async fn scan_security(args: &ScanArgs) -> Result<()> {
    // Validate input
    validate_path(&args.path)?;

    println!("🔒 Scanning for security vulnerabilities");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit"]);

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.fix {
        cmd.arg("--fix");
    }

    cmd.arg("--path").arg(&args.path);

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Security scan failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn check_dependencies(args: &DependenciesArgs) -> Result<()> {
    println!("📦 Checking dependencies for security vulnerabilities");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit-deps"]);

    if args.direct_only {
        cmd.arg("--direct-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.update {
        cmd.arg("--update");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Dependency security check failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn audit_config(args: &ConfigArgs) -> Result<()> {
    // Validate input
    validate_file_path(&args.file)?;

    println!("⚙️ Auditing configuration files for security issues");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit-config"]);

    if let Some(file) = &args.file {
        cmd.arg("--file").arg(file);
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.fix {
        cmd.arg("--fix");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Configuration audit failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}
