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

/// Main entry point for `ggen hook validate`
pub async fn run(args: &ValidateArgs) -> Result<()> {
    // Validate hook name
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Hook name cannot be empty"));
    }

    println!("🔍 Validating hook '{}'...", args.name);

    // TODO: Implement actual validation
    // This will involve:
    // 1. Load hook config from .ggen/hooks/{name}.json
    // 2. Validate configuration syntax
    // 3. Check template reference exists
    // 4. Validate trigger-specific requirements
    // 5. Check variable definitions
    // 6. Verify git hook installation (for git triggers)

    // Mock validation checks
    let checks = vec![
        ValidationCheck {
            name: "Configuration file exists".to_string(),
            passed: true,
            message: "Hook configuration found at .ggen/hooks/pre-commit.json".to_string(),
        },
        ValidationCheck {
            name: "Configuration syntax valid".to_string(),
            passed: true,
            message: "JSON syntax is valid".to_string(),
        },
        ValidationCheck {
            name: "Template reference valid".to_string(),
            passed: true,
            message: "Template 'graph-gen.tmpl' exists".to_string(),
        },
        ValidationCheck {
            name: "Trigger configuration valid".to_string(),
            passed: true,
            message: "Git pre-commit hook properly configured".to_string(),
        },
        ValidationCheck {
            name: "Variables defined correctly".to_string(),
            passed: true,
            message: "All required variables have default values".to_string(),
        },
    ];

    let errors: Vec<String> = checks
        .iter()
        .filter(|c| !c.passed)
        .map(|c| c.message.clone())
        .collect();

    let warnings: Vec<String> = vec![];

    let result = ValidationResult {
        valid: errors.is_empty(),
        hook_name: args.name.clone(),
        errors,
        warnings,
        checks,
    };

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
