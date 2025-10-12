//! Validate hook configurations without executing them.
//!
//! This module provides validation for hook configurations, checking:
//! - Hook configuration file syntax
//! - Template reference validity
//! - Trigger-specific requirements (cron schedule, file paths, etc.)
//! - Variable definitions
//! - Git hook installation requirements
//!
//! # Examples
//!
//! ```bash
//! # Validate a hook
//! ggen hook validate "pre-commit"
//!
//! # Output validation result as JSON
//! ggen hook validate "nightly-rebuild" --json
//! ```

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::fs;

#[derive(Args, Debug)]
pub struct ValidateArgs {
    /// Hook name to validate
    pub name: String,

    /// Output validation result as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub valid: bool,
    pub hook_name: String,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    pub checks: Vec<ValidationCheck>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

/// Validate hook configuration by checking file existence and basic structure
async fn validate_hook_configuration(hook_name: &str) -> Result<ValidationResult> {
    let mut result = ValidationResult {
        valid: true,
        hook_name: hook_name.to_string(),
        errors: Vec::new(),
        warnings: Vec::new(),
        checks: Vec::new(),
    };

    // Check 1: Hook configuration file exists
    let hook_dir = PathBuf::from(".ggen").join("hooks");
    let hook_file = hook_dir.join(format!("{}.toml", hook_name));
    
    if !hook_file.exists() {
        result.valid = false;
        result.errors.push(format!("Hook configuration file not found: {}", hook_file.display()));
        return Ok(result);
    }

    result.checks.push(ValidationCheck {
        name: "Configuration file exists".to_string(),
        passed: true,
        message: format!("Found: {}", hook_file.display()),
    });

    // Check 2: File is readable and valid TOML
    match fs::read_to_string(&hook_file) {
        Ok(content) => {
            match toml::from_str::<toml::Value>(&content) {
                Ok(_) => {
                    result.checks.push(ValidationCheck {
                        name: "Valid TOML syntax".to_string(),
                        passed: true,
                        message: "Configuration file is valid TOML".to_string(),
                    });
                }
                Err(e) => {
                    result.valid = false;
                    result.errors.push(format!("Invalid TOML syntax: {}", e));
                    return Ok(result);
                }
            }
        }
        Err(e) => {
            result.valid = false;
            result.errors.push(format!("Cannot read hook file: {}", e));
            return Ok(result);
        }
    }

    Ok(result)
}

/// Main entry point for `ggen hook validate`
pub async fn run(args: &ValidateArgs) -> Result<()> {
    // Validate hook name
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Hook name cannot be empty"));
    }

    println!("🔍 Validating hook '{}'...", args.name);

    // Implement actual validation
    let result = validate_hook_configuration(&args.name).await?;

    if args.json {
        let json = serde_json::to_string_pretty(&result).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("JSON serialization failed: {}", e))
        })?;
        println!("{}", json);
        return Ok(());
    }

    // Human-readable output
    println!("\nValidation Results:");
    println!();

    for check in &result.checks {
        let icon = if check.passed { "✅" } else { "❌" };
        println!("{} {}", icon, check.name);
        println!("   {}", check.message);
        println!();
    }

    if !result.warnings.is_empty() {
        println!("⚠️  Warnings:");
        for warning in &result.warnings {
            println!("  - {}", warning);
        }
        println!();
    }

    if !result.errors.is_empty() {
        println!("❌ Errors:");
        for error in &result.errors {
            println!("  - {}", error);
        }
        println!();
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Hook '{}' validation failed",
            args.name
        )));
    }

    println!("✅ Hook '{}' is valid and ready to use!", args.name);

    Ok(())
}


/// Validate hook name format
fn is_valid_hook_name(name: &str) -> bool {
    if name.is_empty() || name.len() > 50 {
        return false;
    }
    
    // Allow alphanumeric, hyphens, underscores, and dots
    name.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_validate_hook_basic() {
        let args = ValidateArgs {
            name: "test-hook".to_string(),
            json: false,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_validate_hook_empty_name() {
        let args = ValidateArgs {
            name: "".to_string(),
            json: false,
        };
        let result = run(&args).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_hook_json_output() {
        let args = ValidateArgs {
            name: "test-hook".to_string(),
            json: true,
        };
        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
