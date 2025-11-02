//! Marketplace list command - CLI layer
//!
//! This module provides the CLI interface for listing installed packages.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// List installed marketplace packages
///
/// # Examples
///
/// ```bash
/// ggen marketplace list
/// ggen marketplace list --detailed
/// ggen marketplace list --json
/// ```
#[derive(Args, Debug)]
#[command(name = "list", about = "List installed marketplace packages")]
pub struct ListArgs {
    /// Show detailed information
    #[arg(long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

/// Execute marketplace list command
pub fn run(args: &ListArgs) -> Result<()> {
    runtime::execute(async {
        crate::domain::marketplace::list::list_and_display(args.detailed, args.json).await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_args_parsing() {
        let args = ListArgs {
            detailed: true,
            json: false,
        };

        assert!(args.detailed);
        assert!(!args.json);
    }
}
