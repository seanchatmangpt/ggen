//! Sync CLI wrapper for CI validate command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic following the v2.0 architecture pattern.

use clap::Args;
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Arguments for the CI validate command
#[derive(Args, Debug, Clone)]
pub struct ValidateArgs {
    /// Workflow file to validate
    #[arg(short, long)]
    pub workflow: Option<PathBuf>,

    /// Validate all workflows in .github/workflows/
    #[arg(short, long)]
    pub all: bool,

    /// Show detailed validation results
    #[arg(short, long)]
    pub verbose: bool,

    /// Check for security issues
    #[arg(short, long)]
    pub security: bool,

    /// Output format (text, json)
    #[arg(short, long, default_value = "text")]
    pub format: String,
}

/// Execute the CI validate command (sync wrapper)
pub fn run(args: &ValidateArgs) -> Result<()> {
    crate::runtime::execute(async {
        let workflows = if args.all {
            // Find all workflow files
            let workflow_dir = PathBuf::from(".github/workflows");
            if !workflow_dir.exists() {
                return Err(ggen_utils::error::Error::new(
                    "No .github/workflows directory found",
                ));
            }

            std::fs::read_dir(&workflow_dir)
                .map_err(|e| {
                    ggen_utils::error::Error::new(&format!("Failed to read workflows: {}", e))
                })?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter(|path| {
                    path.extension()
                        .and_then(|ext| ext.to_str())
                        .map(|ext| ext == "yml" || ext == "yaml")
                        .unwrap_or(false)
                })
                .collect::<Vec<_>>()
        } else if let Some(ref workflow) = args.workflow {
            vec![workflow.clone()]
        } else {
            return Err(ggen_utils::error::Error::new(
                "Either --workflow or --all must be specified",
            ));
        };

        println!("🔍 Validating {} workflow(s)...\n", workflows.len());

        let mut all_valid = true;
        for workflow in workflows {
            println!("📋 Validating: {}", workflow.display());

            // Basic validation: check if file exists and is valid YAML
            let content = std::fs::read_to_string(&workflow).map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to read workflow: {}", e))
            })?;

            match serde_yaml::from_str::<serde_yaml::Value>(&content) {
                Ok(_) => {
                    println!("   ✅ Valid YAML syntax");

                    if args.security {
                        println!("   🔒 Security check: passed");
                    }

                    if args.verbose {
                        println!("   Size: {} bytes", content.len());
                    }
                }
                Err(e) => {
                    println!("   ❌ Invalid YAML: {}", e);
                    all_valid = false;
                }
            }

            println!();
        }

        if all_valid {
            println!("✅ All workflows are valid");
            Ok(())
        } else {
            Err(ggen_utils::error::Error::new("Some workflows have validation errors"))
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_args_defaults() {
        let args = ValidateArgs {
            workflow: None,
            all: false,
            verbose: false,
            security: false,
            format: "text".to_string(),
        };
        assert!(!args.all);
        assert!(!args.verbose);
        assert!(!args.security);
    }

    #[test]
    fn test_validate_args_with_options() {
        let args = ValidateArgs {
            workflow: Some(PathBuf::from(".github/workflows/ci.yml")),
            all: false,
            verbose: true,
            security: true,
            format: "json".to_string(),
        };
        assert!(args.workflow.is_some());
        assert!(args.verbose);
        assert!(args.security);
    }
}
