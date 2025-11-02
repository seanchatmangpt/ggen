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
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;
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
    let variables = crate::domain::template::parse_variables(&args.vars)?;

    // Build options
    let options = crate::domain::template::GenerateFileOptions::new(
        args.template.clone(),
        args.output.clone(),
    )
    .with_vars(variables);

    let options = if args.force {
        options.force()
    } else {
        options
    };

    // Use the runtime bridge to execute domain logic
    crate::runtime::execute(async {
        let result = crate::domain::template::generate_file(&options)?;

        println!("✅ File generated successfully");
        println!("   Output: {}", result.output_path.display());
        println!("   Size: {} bytes", result.bytes_written);
        println!("   Variables: {}", result.variables_used);

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
