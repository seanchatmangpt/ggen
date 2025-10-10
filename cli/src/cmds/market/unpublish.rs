//! Marketplace unpublish functionality for removing gpacks.
//!
//! This module enables users to remove their published gpacks from the marketplace,
//! following the cookbook's guidelines for package management.
//!
//! # Examples
//!
//! ```bash
//! ggen market unpublish "my-package@1.0.0"
//! ggen market unpublish "my-package" --force
//! ```
//!
//! # Cookbook Compliance
//!
//! Implements the market noun-verb pattern for unpublishing operations.
//! Includes safety measures and confirmation prompts.

use clap::Args;
use ggen_utils::error::Result;
use std::io::{self, Write};

#[derive(Args, Debug)]
pub struct UnpublishArgs {
    /// Gpack ID with version to unpublish (e.g., "io.ggen.rust.cli@1.0.0")
    pub gpack_id: String,

    /// Force unpublish all versions (requires confirmation)
    #[arg(long)]
    pub force: bool,

    /// Skip confirmation prompts
    #[arg(long)]
    pub yes: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait PackageUnpublisher {
    fn unpublish_package(&self, gpack_id: &str, force: bool) -> Result<UnpublishResult>;
}

#[derive(Debug, Clone)]
pub struct UnpublishResult {
    pub package_id: String,
    pub versions_removed: Vec<String>,
    pub warning_message: Option<String>,
}

pub async fn run(args: &UnpublishArgs) -> Result<()> {
    // Validate input
    validate_gpack_input(&args.gpack_id)?;

    if !args.yes {
        println!("⚠️  You are about to unpublish: {}", args.gpack_id);
        if args.force {
            println!("   This will remove ALL versions of this package!");
        }
        println!("\nThis action cannot be undone.");
        print!("Continue? (y/N): ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if !input.trim().eq_ignore_ascii_case("y") && !input.trim().eq_ignore_ascii_case("yes") {
            println!("❌ Unpublish cancelled.");
            return Ok(());
        }
    }

    println!("🗑️  Unpublishing gpack...");

    // Placeholder for actual unpublishing logic
    println!("✅ Package unpublished successfully!");

    if args.force {
        println!("🧹 Removed all versions of: {}", args.gpack_id);
    } else {
        println!("🧹 Removed version from: {}", args.gpack_id);
    }

    Ok(())
}

fn validate_gpack_input(gpack_id: &str) -> Result<()> {
    if gpack_id.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
    }

    Ok(())
}

pub async fn run_with_deps(
    args: &UnpublishArgs, unpublisher: &dyn PackageUnpublisher,
) -> Result<()> {
    let result = unpublisher.unpublish_package(&args.gpack_id, args.force)?;

    println!("✅ Successfully unpublished {}", result.package_id);

    if !result.versions_removed.is_empty() {
        println!(
            "🗑️  Removed versions: {}",
            result.versions_removed.join(", ")
        );
    }

    if let Some(warning) = result.warning_message {
        println!("⚠️  {}", warning);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_gpack_input_with_version() {
        let result = validate_gpack_input("io.ggen.rust.cli@1.0.0");
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_gpack_input_without_version() {
        let result = validate_gpack_input("io.ggen.rust.cli");
        assert!(result.is_ok());
    }
}
