//! Marketplace install command - CLI layer
//!
//! This module provides the CLI interface for installing marketplace packages.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// Install a package from the marketplace
///
/// # Examples
///
/// ```bash
/// ggen marketplace install "rust-cli-template"
/// ggen marketplace install "web-api@1.2.0" --force
/// ggen marketplace install "database" --dry-run
/// ```
#[derive(Args, Debug)]
#[command(name = "install", about = "Install a package from the marketplace")]
pub struct InstallArgs {
    /// Package name with optional version (e.g., "package@1.0.0")
    #[arg(value_name = "PACKAGE")]
    pub package: String,

    /// Target installation path
    #[arg(long)]
    pub target: Option<String>,

    /// Force installation even if already installed
    #[arg(long)]
    pub force: bool,

    /// Skip dependency installation
    #[arg(long = "no-deps")]
    pub no_dependencies: bool,

    /// Dry run - validate without installing
    #[arg(long)]
    pub dry_run: bool,
}

/// Execute marketplace install command
pub fn run(args: &InstallArgs) -> Result<()> {
    runtime::execute(async {
        crate::domain::marketplace::install::install_and_report(
            &args.package,
            args.target.as_deref(),
            args.force,
            !args.no_dependencies,
            args.dry_run,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_args_parsing() {
        let args = InstallArgs {
            package: "rust-cli@1.0.0".to_string(),
            target: Some("/tmp/test".to_string()),
            force: true,
            no_dependencies: false,
            dry_run: false,
        };

        assert_eq!(args.package, "rust-cli@1.0.0");
        assert_eq!(args.target, Some("/tmp/test".to_string()));
        assert!(args.force);
        assert!(!args.no_dependencies);
    }
}
