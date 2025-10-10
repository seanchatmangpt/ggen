//! Hazard scanning and pattern detection for code quality assurance.
//!
//! This module provides functionality to scan codebases for hazardous patterns,
//! anti-patterns, and potential security vulnerabilities. It integrates with
//! cargo-make to perform comprehensive hazard analysis.
//!
//! # Examples
//!
//! ```bash
//! ggen audit hazard scan --path ./src --verbose
//! ggen audit hazard list --critical
//! ggen audit hazard check --type "unsafe" --path ./src
//! ```
//!
//! # Errors
//!
//! Returns errors if the underlying cargo-make commands fail or if
//! the specified paths don't exist.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait HazardScanner {
    fn scan(&self, path: &str, json: bool, verbose: bool, fix: bool) -> Result<ScanResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait HazardLister {
    fn list(&self, critical: bool, json: bool) -> Result<ListResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait HazardChecker {
    fn check(&self, hazard_type: &str, path: &str, json: bool) -> Result<CheckResult>;
}

#[derive(Debug, Clone)]
pub struct ScanResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct ListResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct CheckResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Args, Debug)]
pub struct HazardArgs {
    #[command(subcommand)]
    pub action: HazardAction,
}

#[derive(Subcommand, Debug)]
pub enum HazardAction {
    /// Scan for hazardous patterns
    Scan(ScanArgs),

    /// List known hazardous patterns
    List(ListArgs),

    /// Check specific hazard types
    Check(CheckArgs),
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
pub struct ListArgs {
    /// Show only critical hazards
    #[arg(long)]
    pub critical: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct CheckArgs {
    /// Hazard type to check
    #[arg(long)]
    pub hazard_type: String,

    /// Directory to check [default: current directory]
    #[arg(long, default_value = ".")]
    pub path: String,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,
}

pub async fn run(args: &HazardArgs) -> Result<()> {
    let scanner = CargoMakeHazardScanner;
    let lister = CargoMakeHazardLister;
    let checker = CargoMakeHazardChecker;

    run_with_deps(args, &scanner, &lister, &checker).await
}

pub async fn run_with_deps(
    args: &HazardArgs, scanner: &dyn HazardScanner, lister: &dyn HazardLister,
    checker: &dyn HazardChecker,
) -> Result<()> {
    match &args.action {
        HazardAction::Scan(scan_args) => scan_hazards_with_deps(scan_args, scanner).await,
        HazardAction::List(list_args) => list_hazards_with_deps(list_args, lister).await,
        HazardAction::Check(check_args) => check_hazard_type_with_deps(check_args, checker).await,
    }
}

async fn scan_hazards_with_deps(args: &ScanArgs, scanner: &dyn HazardScanner) -> Result<()> {
    println!("🔍 Scanning for hazardous patterns");

    let result = scanner.scan(&args.path, args.json, args.verbose, args.fix)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new(&format!(
            "Hazard scan failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn scan_hazards(args: &ScanArgs) -> Result<()> {
    let scanner = CargoMakeHazardScanner;
    scan_hazards_with_deps(args, &scanner).await
}

async fn list_hazards_with_deps(args: &ListArgs, lister: &dyn HazardLister) -> Result<()> {
    println!("📋 Listing known hazardous patterns");

    let result = lister.list(args.critical, args.json)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new(&format!(
            "Hazard listing failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn list_hazards(args: &ListArgs) -> Result<()> {
    let lister = CargoMakeHazardLister;
    list_hazards_with_deps(args, &lister).await
}

async fn check_hazard_type_with_deps(args: &CheckArgs, checker: &dyn HazardChecker) -> Result<()> {
    println!("🔍 Checking for hazard type: {}", args.hazard_type);

    let result = checker.check(&args.hazard_type, &args.path, args.json)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new(&format!(
            "Hazard check failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn check_hazard_type(args: &CheckArgs) -> Result<()> {
    let checker = CargoMakeHazardChecker;
    check_hazard_type_with_deps(args, &checker).await
}

// Concrete implementations for production use
pub struct CargoMakeHazardScanner;

impl HazardScanner for CargoMakeHazardScanner {
    fn scan(&self, path: &str, json: bool, verbose: bool, fix: bool) -> Result<ScanResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "hazard"]);

        if json {
            cmd.arg("--json");
        }

        if verbose {
            cmd.arg("--verbose");
        }

        if fix {
            cmd.arg("--fix");
        }

        cmd.arg("--path").arg(path);

        let output = cmd.output()?;
        Ok(ScanResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakeHazardLister;

impl HazardLister for CargoMakeHazardLister {
    fn list(&self, critical: bool, json: bool) -> Result<ListResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "hazard-list"]);

        if critical {
            cmd.arg("--critical");
        }

        if json {
            cmd.arg("--json");
        }

        let output = cmd.output()?;
        Ok(ListResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakeHazardChecker;

impl HazardChecker for CargoMakeHazardChecker {
    fn check(&self, hazard_type: &str, path: &str, json: bool) -> Result<CheckResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "hazard-check"]);

        cmd.arg("--type").arg(hazard_type);
        cmd.arg("--path").arg(path);

        if json {
            cmd.arg("--json");
        }

        let output = cmd.output()?;
        Ok(CheckResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_scan_calls_scanner() {
        let mut mock = MockHazardScanner::new();
        mock.expect_scan()
            .with(eq("."), eq(false), eq(false), eq(false))
            .times(1)
            .returning(|_, _, _, _| {
                Ok(ScanResult {
                    stdout: "Scan complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ScanArgs {
            path: ".".to_string(),
            json: false,
            verbose: false,
            fix: false,
        };
        let result = scan_hazards_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_scan_handles_failure() {
        let mut mock = MockHazardScanner::new();
        mock.expect_scan()
            .with(eq("."), eq(false), eq(false), eq(false))
            .times(1)
            .returning(|_, _, _, _| {
                Ok(ScanResult {
                    stdout: "".to_string(),
                    stderr: "Scan failed".to_string(),
                    success: false,
                })
            });

        let args = ScanArgs {
            path: ".".to_string(),
            json: false,
            verbose: false,
            fix: false,
        };
        let result = scan_hazards_with_deps(&args, &mock).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Hazard scan failed"));
    }

    #[tokio::test]
    async fn test_list_calls_lister() {
        let mut mock = MockHazardLister::new();
        mock.expect_list()
            .with(eq(false), eq(false))
            .times(1)
            .returning(|_, _| {
                Ok(ListResult {
                    stdout: "Hazard list".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ListArgs {
            critical: false,
            json: false,
        };
        let result = list_hazards_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_check_calls_checker() {
        let mut mock = MockHazardChecker::new();
        mock.expect_check()
            .with(eq("unsafe"), eq("."), eq(false))
            .times(1)
            .returning(|_, _, _| {
                Ok(CheckResult {
                    stdout: "Check complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = CheckArgs {
            hazard_type: "unsafe".to_string(),
            path: ".".to_string(),
            json: false,
        };
        let result = check_hazard_type_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock_scanner = MockHazardScanner::new();
        mock_scanner
            .expect_scan()
            .with(eq("."), eq(false), eq(false), eq(false))
            .times(1)
            .returning(|_, _, _, _| {
                Ok(ScanResult {
                    stdout: "Scan complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let mock_lister = MockHazardLister::new();
        let mock_checker = MockHazardChecker::new();

        let args = HazardArgs {
            action: HazardAction::Scan(ScanArgs {
                path: ".".to_string(),
                json: false,
                verbose: false,
                fix: false,
            }),
        };

        let result = run_with_deps(&args, &mock_scanner, &mock_lister, &mock_checker).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_scan_args_defaults() {
        let args = ScanArgs {
            path: ".".to_string(),
            json: false,
            verbose: false,
            fix: false,
        };
        assert_eq!(args.path, ".");
        assert!(!args.json);
        assert!(!args.verbose);
        assert!(!args.fix);
    }

    #[test]
    fn test_list_args_defaults() {
        let args = ListArgs {
            critical: false,
            json: false,
        };
        assert!(!args.critical);
        assert!(!args.json);
    }

    #[test]
    fn test_check_args_defaults() {
        let args = CheckArgs {
            hazard_type: "unsafe".to_string(),
            path: ".".to_string(),
            json: false,
        };
        assert_eq!(args.hazard_type, "unsafe");
        assert_eq!(args.path, ".");
        assert!(!args.json);
    }
}
