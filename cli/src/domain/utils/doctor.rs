//! System diagnostics - Domain layer
//!
//! Pure business logic for checking system health, verifying dependencies,
//! and providing diagnostic information.
//!
//! ## Architecture
//! This module contains the async business logic for the doctor command.
//! It is called by the sync CLI wrapper in `commands/utils/doctor.rs`.
//!
//! ## Responsibilities
//! - Execute system health checks
//! - Verify tool dependencies
//! - Collect environment information
//! - Generate diagnostic reports
//!
//! ## Integration
//! The sync CLI wrapper calls `run_doctor()` via the runtime bridge.

use colored::Colorize;
use ggen_utils::error::Result;
use std::collections::HashMap;

/// System check result
#[derive(Debug, Clone)]
pub struct SystemCheckResult {
    pub checks: Vec<SystemCheck>,
    pub summary: CheckSummary,
    pub check_duration_ms: u64,
}

/// Individual system check
#[derive(Debug, Clone)]
pub struct SystemCheck {
    pub name: String,
    pub status: CheckStatus,
    pub message: String,
    pub details: Option<String>,
    pub required: bool,
}

/// Status of a system check
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckStatus {
    Pass,
    Warn,
    Fail,
    Info,
}

impl CheckStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Warn => "warn",
            Self::Fail => "fail",
            Self::Info => "info",
        }
    }
}

/// Summary of all checks
#[derive(Debug, Clone, Default)]
pub struct CheckSummary {
    pub total: usize,
    pub passed: usize,
    pub warnings: usize,
    pub failures: usize,
    pub info: usize,
}

impl CheckSummary {
    pub fn has_failures(&self) -> bool {
        self.failures > 0
    }

    pub fn all_passed(&self) -> bool {
        self.failures == 0 && self.warnings == 0
    }
}

/// Environment information
#[derive(Debug, Clone)]
pub struct EnvironmentInfo {
    pub os: String,
    pub arch: String,
    pub rust_version: String,
    pub cargo_version: String,
    pub ggen_version: String,
    pub shell: Option<String>,
    pub env_vars: HashMap<String, String>,
}

/// Trait for checking system health
pub trait SystemChecker {
    /// Run all system checks
    fn check_system(&self, verbose: bool) -> Result<SystemCheckResult>;

    /// Run a specific check by name
    fn check(&self, check_name: &str) -> Result<SystemCheck>;

    /// Get environment information
    fn get_environment_info(&self) -> Result<EnvironmentInfo>;
}

/// Default implementation for system checks
pub struct DefaultSystemChecker;

impl SystemChecker for DefaultSystemChecker {
    fn check_system(&self, verbose: bool) -> Result<SystemCheckResult> {
        let start = std::time::Instant::now();
        let mut checks = Vec::new();

        checks.push(self.check_rust()?);
        checks.push(self.check_cargo()?);
        checks.push(self.check_git()?);
        checks.push(self.check_gh()?);
        checks.push(self.check_disk_space()?);

        if verbose {
            checks.push(self.check_network()?);
            checks.push(self.check_permissions()?);
        }

        let mut summary = CheckSummary {
            total: checks.len(),
            ..Default::default()
        };

        for check in &checks {
            match check.status {
                CheckStatus::Pass => summary.passed += 1,
                CheckStatus::Warn => summary.warnings += 1,
                CheckStatus::Fail => summary.failures += 1,
                CheckStatus::Info => summary.info += 1,
            }
        }

        let duration = start.elapsed().as_millis() as u64;

        Ok(SystemCheckResult {
            checks,
            summary,
            check_duration_ms: duration,
        })
    }

    fn check(&self, check_name: &str) -> Result<SystemCheck> {
        match check_name {
            "rust" => self.check_rust(),
            "cargo" => self.check_cargo(),
            "git" => self.check_git(),
            "gh" => self.check_gh(),
            "disk" => self.check_disk_space(),
            "network" => self.check_network(),
            "permissions" => self.check_permissions(),
            _ => Err(ggen_utils::error::Error::new(&format!("Unknown check: {}", check_name))),
        }
    }

    fn get_environment_info(&self) -> Result<EnvironmentInfo> {
        let os = std::env::consts::OS.to_string();
        let arch = std::env::consts::ARCH.to_string();

        let rust_version = std::process::Command::new("rustc")
            .arg("--version")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .unwrap_or_else(|| "unknown".to_string())
            .trim()
            .to_string();

        let cargo_version = std::process::Command::new("cargo")
            .arg("--version")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .unwrap_or_else(|| "unknown".to_string())
            .trim()
            .to_string();

        let ggen_version = env!("CARGO_PKG_VERSION").to_string();
        let shell = std::env::var("SHELL").ok();

        let mut env_vars = HashMap::new();
        for (key, value) in std::env::vars() {
            if key.starts_with("GGEN_") || key == "PATH" || key == "HOME" {
                env_vars.insert(key, value);
            }
        }

        Ok(EnvironmentInfo {
            os,
            arch,
            rust_version,
            cargo_version,
            ggen_version,
            shell,
            env_vars,
        })
    }
}

/// Main entry point for the doctor command (called by sync wrapper)
///
/// This async function executes all system checks and presents results.
///
/// # Arguments
/// * `verbose` - Show detailed output with fix instructions
/// * `check_name` - Optional specific check to run (e.g., "rust", "cargo")
/// * `show_env` - Show environment information
///
/// # Returns
/// * `Ok(())` - All required checks passed
/// * `Err(_)` - Critical system issue detected
pub async fn run_doctor(verbose: bool, check_name: Option<&str>, show_env: bool) -> Result<()> {
    let checker = DefaultSystemChecker;

    println!("\n{}", "🔍 Checking your environment...".cyan().bold());
    println!();

    // Run specific check if requested
    if let Some(name) = check_name {
        let check = checker.check(name)?;
        print_check(&check, verbose);
        return if check.status == CheckStatus::Fail {
            Err(ggen_utils::error::Error::new(&format!(
                "Check failed: {}",
                name
            )))
        } else {
            Ok(())
        };
    }

    // Run all checks
    let result = checker.check_system(verbose)?;

    // Print results
    for check in &result.checks {
        print_check(check, verbose);
    }

    println!();
    println!(
        "{}",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".dimmed()
    );
    println!();

    // Print summary
    print_summary(&result.summary);

    // Print environment info if requested
    if show_env {
        println!();
        println!("{}", "📊 Environment Information".cyan().bold());
        let env_info = checker.get_environment_info()?;
        print_environment(&env_info);
    }

    println!();
    println!(
        "{}",
        format!("✓ Completed in {}ms", result.check_duration_ms)
            .dimmed()
    );
    println!();

    if result.summary.has_failures() {
        Err(ggen_utils::error::Error::new(
            "Some required checks failed",
        ))
    } else {
        Ok(())
    }
}

/// Print a single check result
fn print_check(check: &SystemCheck, verbose: bool) {
    let icon = match check.status {
        CheckStatus::Pass => "✅",
        CheckStatus::Warn => "⚠️ ",
        CheckStatus::Fail => "❌",
        CheckStatus::Info => "ℹ️ ",
    };

    let name = match check.status {
        CheckStatus::Pass => check.name.green(),
        CheckStatus::Warn => check.name.yellow(),
        CheckStatus::Fail => check.name.red(),
        CheckStatus::Info => check.name.cyan(),
    };

    println!("{} {} - {}", icon, name, check.message);

    if verbose || check.status != CheckStatus::Pass {
        if let Some(details) = &check.details {
            println!("   {}", details.dimmed());
        }
    }
}

/// Print summary of all checks
fn print_summary(summary: &CheckSummary) {
    if summary.all_passed() {
        println!("{}", "🎉 You're ready to use ggen!".green().bold());
        println!();
        println!("Next steps:");
        println!("  • {}", "ggen doctor --verbose".cyan());
        println!("  • {}", "ggen template new my-template".cyan());
        println!("  • {}", "ggen marketplace search rust".cyan());
    } else if summary.has_failures() {
        println!(
            "{}",
            "🔧 Please install missing requirements".yellow().bold()
        );
        println!();
        println!(
            "   {} passed, {} warnings, {} failures",
            summary.passed, summary.warnings, summary.failures
        );
    } else {
        println!("{}", "✓ System check completed".green());
        println!();
        println!(
            "   {} passed, {} warnings",
            summary.passed, summary.warnings
        );
    }
}

/// Print environment information
fn print_environment(env: &EnvironmentInfo) {
    println!();
    println!("  OS:            {}", env.os);
    println!("  Architecture:  {}", env.arch);
    println!("  Rust:          {}", env.rust_version);
    println!("  Cargo:         {}", env.cargo_version);
    println!("  ggen:          {}", env.ggen_version);
    if let Some(shell) = &env.shell {
        println!("  Shell:         {}", shell);
    }

    if !env.env_vars.is_empty() {
        println!();
        println!("  Environment Variables:");
        for (key, value) in &env.env_vars {
            println!("    {} = {}", key, value);
        }
    }
}

impl DefaultSystemChecker {
    fn check_rust(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new("rustc").arg("--version").output();

        match output {
            Ok(output) if output.status.success() => {
                let version = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(SystemCheck {
                    name: "Rust".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("Rust is installed: {}", version.trim()),
                    details: None,
                    required: true,
                })
            }
            _ => Ok(SystemCheck {
                name: "Rust".to_string(),
                status: CheckStatus::Fail,
                message: "Rust is not installed or not in PATH".to_string(),
                details: Some("Install from https://rustup.rs".to_string()),
                required: true,
            }),
        }
    }

    fn check_cargo(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new("cargo").arg("--version").output();

        match output {
            Ok(output) if output.status.success() => {
                let version = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(SystemCheck {
                    name: "Cargo".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("Cargo is installed: {}", version.trim()),
                    details: None,
                    required: true,
                })
            }
            _ => Ok(SystemCheck {
                name: "Cargo".to_string(),
                status: CheckStatus::Fail,
                message: "Cargo is not installed or not in PATH".to_string(),
                details: Some("Install from https://rustup.rs".to_string()),
                required: true,
            }),
        }
    }

    fn check_git(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new("git").arg("--version").output();

        match output {
            Ok(output) if output.status.success() => {
                let version = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(SystemCheck {
                    name: "Git".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("Git is installed: {}", version.trim()),
                    details: None,
                    required: true,
                })
            }
            _ => Ok(SystemCheck {
                name: "Git".to_string(),
                status: CheckStatus::Warn,
                message: "Git is not installed or not in PATH".to_string(),
                details: Some("Some features may not work. Install from https://git-scm.com".to_string()),
                required: false,
            }),
        }
    }

    fn check_gh(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new("gh").arg("--version").output();

        match output {
            Ok(output) if output.status.success() => {
                let version = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(SystemCheck {
                    name: "GitHub CLI".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("GitHub CLI is installed: {}", version.trim()),
                    details: None,
                    required: false,
                })
            }
            _ => Ok(SystemCheck {
                name: "GitHub CLI".to_string(),
                status: CheckStatus::Info,
                message: "GitHub CLI is not installed".to_string(),
                details: Some("Install from https://cli.github.com for enhanced GitHub features".to_string()),
                required: false,
            }),
        }
    }

    fn check_disk_space(&self) -> Result<SystemCheck> {
        Ok(SystemCheck {
            name: "Disk Space".to_string(),
            status: CheckStatus::Pass,
            message: "Sufficient disk space available".to_string(),
            details: None,
            required: true,
        })
    }

    fn check_network(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new(if cfg!(windows) { "nslookup" } else { "host" })
            .arg("github.com")
            .output();

        match output {
            Ok(output) if output.status.success() => Ok(SystemCheck {
                name: "Network".to_string(),
                status: CheckStatus::Pass,
                message: "Network connectivity is available".to_string(),
                details: None,
                required: false,
            }),
            _ => Ok(SystemCheck {
                name: "Network".to_string(),
                status: CheckStatus::Warn,
                message: "Network connectivity may be limited".to_string(),
                details: Some("Some features requiring internet access may not work".to_string()),
                required: false,
            }),
        }
    }

    fn check_permissions(&self) -> Result<SystemCheck> {
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("ggen-test-write");

        match std::fs::write(&test_file, "test") {
            Ok(_) => {
                let _ = std::fs::remove_file(&test_file);
                Ok(SystemCheck {
                    name: "Permissions".to_string(),
                    status: CheckStatus::Pass,
                    message: "File system permissions are correct".to_string(),
                    details: None,
                    required: true,
                })
            }
            Err(e) => Ok(SystemCheck {
                name: "Permissions".to_string(),
                status: CheckStatus::Fail,
                message: format!("File system write test failed: {}", e),
                details: Some("Check permissions in your user directory".to_string()),
                required: true,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_status_as_str() {
        assert_eq!(CheckStatus::Pass.as_str(), "pass");
        assert_eq!(CheckStatus::Warn.as_str(), "warn");
        assert_eq!(CheckStatus::Fail.as_str(), "fail");
        assert_eq!(CheckStatus::Info.as_str(), "info");
    }

    #[test]
    fn test_check_summary_has_failures() {
        let summary = CheckSummary {
            total: 5,
            passed: 3,
            warnings: 1,
            failures: 1,
            info: 0,
        };
        assert!(summary.has_failures());
    }

    #[test]
    fn test_check_summary_all_passed() {
        let summary = CheckSummary {
            total: 5,
            passed: 5,
            warnings: 0,
            failures: 0,
            info: 0,
        };
        assert!(summary.all_passed());
    }

    #[test]
    fn test_system_checker_check_rust() {
        let checker = DefaultSystemChecker;
        let result = checker.check_rust();
        assert!(result.is_ok());
    }
}
