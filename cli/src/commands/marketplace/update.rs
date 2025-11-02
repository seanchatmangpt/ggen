//! Marketplace update command - CLI layer
//!
//! This module provides the CLI interface for updating marketplace packages.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// Update marketplace packages to their latest versions
///
/// # Examples
///
/// ```bash
/// ggen marketplace update
/// ggen marketplace update "rust-cli-template"
/// ggen marketplace update --all
/// ```
#[derive(Args, Debug)]
#[command(name = "update", about = "Update marketplace packages")]
pub struct UpdateArgs {
    /// Specific package to update (updates all if not specified)
    #[arg(value_name = "PACKAGE")]
    pub package: Option<String>,

    /// Update all packages
    #[arg(long)]
    pub all: bool,

    /// Dry run - check for updates without applying
    #[arg(long)]
    pub dry_run: bool,
}

/// Execute marketplace update command
pub fn run(args: &UpdateArgs) -> Result<()> {
    runtime::execute(async {
        crate::domain::marketplace::update::update_and_report(
            args.package.as_deref(),
            args.all,
            args.dry_run,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_update_args_parsing() {
        let args = UpdateArgs {
            package: Some("rust-cli".to_string()),
            all: false,
            dry_run: true,
        };

        assert_eq!(args.package, Some("rust-cli".to_string()));
        assert!(!args.all);
        assert!(args.dry_run);
    }
}
