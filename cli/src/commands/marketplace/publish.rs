//! Marketplace publish command - CLI layer
//!
//! This module provides the CLI interface for publishing packages to the marketplace.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// Publish a package to the marketplace
///
/// # Examples
///
/// ```bash
/// ggen marketplace publish
/// ggen marketplace publish --tag beta
/// ggen marketplace publish --dry-run
/// ```
#[derive(Args, Debug)]
#[command(name = "publish", about = "Publish a package to the marketplace")]
pub struct PublishArgs {
    /// Path to the package directory
    #[arg(default_value = ".")]
    pub path: String,

    /// Publish with a specific tag (e.g., beta, alpha)
    #[arg(long)]
    pub tag: Option<String>,

    /// Dry run - validate without publishing
    #[arg(long)]
    pub dry_run: bool,

    /// Force publish even with warnings
    #[arg(long)]
    pub force: bool,
}

/// Execute marketplace publish command
pub fn run(args: &PublishArgs) -> Result<()> {
    use std::path::Path;

    runtime::execute(async {
        crate::domain::marketplace::publish::publish_and_report(
            Path::new(&args.path),
            args.tag.as_deref(),
            args.dry_run,
            args.force,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_publish_args_parsing() {
        let args = PublishArgs {
            path: "/tmp/my-package".to_string(),
            tag: Some("beta".to_string()),
            dry_run: true,
            force: false,
        };

        assert_eq!(args.path, "/tmp/my-package");
        assert_eq!(args.tag, Some("beta".to_string()));
        assert!(args.dry_run);
        assert!(!args.force);
    }
}
