//! System diagnostics - domain layer
//!
//! Pure business logic for system health checks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// System check status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CheckStatus {
    Ok,
    Warning,
    Error,
}

/// System check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    pub name: String,
    pub status: CheckStatus,
    pub message: String,
}

/// Doctor command input (pure domain type)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DoctorInput {
    /// Show detailed output with fix instructions
    pub verbose: bool,

    /// Run specific check (e.g., "rust", "cargo", "git")
    pub check: Option<String>,

    /// Show environment information
    pub env: bool,
}

/// Doctor command result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoctorResult {
    pub checks: Vec<CheckResult>,
    pub environment: Option<EnvironmentInfo>,
}

/// Environment information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentInfo {
    pub rust_version: Option<String>,
    pub cargo_version: Option<String>,
    pub git_version: Option<String>,
    pub os: String,
    pub architecture: String,
    pub home_dir: Option<String>,
}

/// Execute doctor checks (pure domain function)
pub async fn execute_doctor(input: DoctorInput) -> Result<DoctorResult> {
    use std::process::Command;

    let mut checks = Vec::new();

    // Check Rust
    if input.check.is_none() || input.check.as_deref() == Some("rust") {
        let rust_check = check_rust().await?;
        checks.push(rust_check);
    }

    // Check Cargo
    if input.check.is_none() || input.check.as_deref() == Some("cargo") {
        let cargo_check = check_cargo().await?;
        checks.push(cargo_check);
    }

    // Check Git
    if input.check.is_none() || input.check.as_deref() == Some("git") {
        let git_check = check_git().await?;
        checks.push(git_check);
    }

    // Collect environment info if requested
    let environment = if input.env {
        Some(collect_environment().await?)
    } else {
        None
    };

    Ok(DoctorResult {
        checks,
        environment,
    })
}

/// Check Rust installation
async fn check_rust() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("rustc").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Rust".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Rust".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install from https://rustup.rs".to_string(),
        }),
    }
}

/// Check Cargo installation
async fn check_cargo() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("cargo").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Cargo".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Cargo".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install Rust from https://rustup.rs".to_string(),
        }),
    }
}

/// Check Git installation
async fn check_git() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("git").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Git".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Git".to_string(),
            status: CheckStatus::Warning,
            message: "Not installed. Optional but recommended".to_string(),
        }),
    }
}

/// Collect environment information
async fn collect_environment() -> Result<EnvironmentInfo> {
    use std::process::Command;

    let rust_version = Command::new("rustc")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    let cargo_version = Command::new("cargo")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    let git_version = Command::new("git")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    Ok(EnvironmentInfo {
        rust_version,
        cargo_version,
        git_version,
        os: std::env::consts::OS.to_string(),
        architecture: std::env::consts::ARCH.to_string(),
        home_dir: dirs::home_dir().map(|p| p.to_string_lossy().to_string()),
    })
}
