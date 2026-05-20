//! Doctor Commands
//!
//! This module provides health-check and diagnostic commands for the ggen workspace.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ggen_core::domain::utils::{execute_doctor, CheckStatus, DoctorInput};
use serde::Serialize;
use std::path::Path;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
pub struct RunOutput {
    pub healthy: bool,
    pub binary_version: String,
    pub ggen_toml_found: bool,
    pub workspace_root: String,
    pub checks: Vec<CheckItem>,
    pub message: String,
}

#[derive(Serialize)]
pub struct CheckItem {
    pub name: String,
    pub passed: bool,
    pub detail: String,
    pub recovery: Option<String>,
}

#[derive(Serialize)]
pub struct CheckOutput {
    pub passed: bool,
    pub ggen_toml_found: bool,
    pub workspace_root: String,
    pub message: String,
    pub recovery: Option<String>,
}

// ============================================================================
// Domain Integration Helpers
// ============================================================================

fn workspace_checks() -> Vec<CheckItem> {
    vec![
        path_check(
            "ggen.toml",
            "ggen.toml",
            "Found ggen.toml in current directory",
            "ggen.toml not found in current directory",
        ),
        path_check(
            "Cargo.toml",
            "Cargo.toml",
            "Found Cargo.toml — likely in a Rust workspace",
            "Cargo.toml not found — not in a Rust workspace root",
        ),
        path_check(
            ".specify directory",
            ".specify",
            "Found .specify directory — RDF specs present",
            ".specify directory not found — no RDF specs",
        ),
    ]
}

fn path_check(name: &str, path: &str, found_msg: &str, missing_msg: &str) -> CheckItem {
    let found = Path::new(path).exists();
    CheckItem {
        name: name.to_string(),
        passed: found,
        detail: if found {
            found_msg.to_string()
        } else {
            missing_msg.to_string()
        },
        recovery: None,
    }
}

fn toolchain_checks() -> Result<Vec<CheckItem>> {
    let domain_result = crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: false,
        check: None,
        env: false,
    }))
    .map_err(|e| NounVerbError::execution_error(format!("tokio runtime error: {}", e)))?
    .map_err(|e| NounVerbError::execution_error(format!("doctor domain error: {}", e)))?;

    Ok(domain_result
        .checks
        .into_iter()
        .map(|c| CheckItem {
            passed: matches!(c.status, CheckStatus::Ok),
            detail: c.message,
            name: c.name,
            recovery: c.recovery,
        })
        .collect())
}

fn is_healthy(checks: &[CheckItem]) -> bool {
    let tool_ok = |name: &str| {
        checks
            .iter()
            .find(|c| c.name == name)
            .map(|c| c.passed)
            .unwrap_or(false)
    };
    tool_ok("ggen.toml") && tool_ok("Rust") && tool_ok("Cargo")
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Run a full health check: ggen.toml presence, binary version, workspace state, and toolchain
#[verb]
pub fn run() -> Result<RunOutput> {
    let cwd = std::env::current_dir()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    let ggen_toml_found = Path::new("ggen.toml").exists();
    let binary_version = env!("CARGO_PKG_VERSION").to_string();

    let mut checks = workspace_checks();
    checks.extend(toolchain_checks()?);

    let healthy = is_healthy(&checks);
    let message = if healthy {
        "All critical health checks passed".to_string()
    } else {
        "One or more health checks failed — see checks for details".to_string()
    };

    Ok(RunOutput {
        healthy,
        binary_version,
        ggen_toml_found,
        workspace_root: cwd,
        checks,
        message,
    })
}

/// Quick validation: verifies the workspace can be found and ggen.toml is present
#[verb]
pub fn check() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    let ggen_toml_found = Path::new("ggen.toml").exists();
    let (passed, message) = if ggen_toml_found {
        (true, "Quick check passed — ggen.toml found".to_string())
    } else {
        (
            false,
            "Quick check failed — ggen.toml not found in current directory".to_string(),
        )
    };
    Ok(CheckOutput {
        passed,
        ggen_toml_found,
        workspace_root: cwd,
        message,
        recovery: if ggen_toml_found {
            None
        } else {
            Some("Create ggen.toml in the workspace root".to_string())
        },
    })
}
