//! Sync CLI wrapper for template generate command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic following the v2.0 architecture pattern.
//!
//! ## Architecture Pattern
//! ```
//! User CLI Input → Sync Wrapper (this file) → Runtime Bridge → Async Domain Logic
//! ```

use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

/// Arguments for the template generate command
#[derive(Args, Debug, Clone)]
pub struct GenerateArgs {
    /// Path to the template file
    #[arg(short, long)]
    pub template: PathBuf,

    /// Output directory for generated files
    #[arg(short, long, default_value = ".")]
    pub output: PathBuf,

    /// Template variables in key=value format
    #[arg(short, long)]
    pub vars: Vec<String>,

    /// Force overwrite existing files
    #[arg(short, long)]
    pub force: bool,
}

/// Execute the template generate command (sync wrapper)
///
/// This function:
/// 1. Validates CLI arguments
/// 2. Spawns a Tokio runtime
/// 3. Calls the async domain function
/// 4. Handles errors and formats output
pub fn run(args: &GenerateArgs) -> Result<()> {
    // Parse variables from key=value format
    let mut variables = HashMap::new();
    for var in &args.vars {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new(&format!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var
            )));
        }
        variables.insert(parts[0].to_string(), parts[1].to_string());
    }

    // Use the runtime bridge to execute async domain logic
    crate::runtime::execute(async {
        let result = crate::domain::template::generate_file_tree(
            &args.template,
            &args.output,
            &variables,
            args.force,
        )?;

        println!("✅ Generated {} files", result.files_generated);
        for file in result.files {
            println!("  📄 {}", file.display());
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_args_defaults() {
        let args = GenerateArgs {
            template: PathBuf::from("template.tmpl"),
            output: PathBuf::from("."),
            vars: vec![],
            force: false,
        };
        assert!(!args.force);
        assert!(args.vars.is_empty());
    }

    #[test]
    fn test_generate_args_with_vars() {
        let args = GenerateArgs {
            template: PathBuf::from("template.tmpl"),
            output: PathBuf::from("./output"),
            vars: vec!["name=test".to_string(), "version=1.0".to_string()],
            force: true,
        };
        assert_eq!(args.vars.len(), 2);
        assert!(args.force);
    }
}
