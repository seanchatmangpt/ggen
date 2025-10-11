//! Installed gpacks listing and management.
//!
//! This module provides functionality to list installed gpacks in the current
//! project, showing their versions, sources, and metadata. It helps users
//! understand what packages are currently installed and manage their dependencies.
//!
//! # Examples
//!
//! ```bash
//! ggen market list
//! ggen market list --detailed
//! ```
//!
//! # Errors
//!
//! Returns errors if the lockfile cannot be read, the project is not properly
//! initialized, or if the listing operation fails.

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct ListArgs {
    /// Show detailed information
    #[arg(long)]
    pub detailed: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackLister {
    fn list_installed(&self) -> Result<Vec<InstalledGpack>>;
}

#[derive(Debug, Clone)]
pub struct InstalledGpack {
    pub id: String,
    pub version: String,
    pub sha256: String,
    pub source: String,
}

pub async fn run(_args: &ListArgs) -> Result<()> {
    println!("📦 Listing installed gpacks...");

    // Placeholder: In production, this would read from .ggen/lock.json
    // For now, return mock list
    println!("ℹ️  No gpacks installed yet");
    println!();
    println!("💡 Use 'ggen market search <query>' to discover packages");
    println!("💡 Use 'ggen market add <package>' to install packages");

    Ok(())
}

pub async fn run_with_deps(args: &ListArgs, lister: &dyn GpackLister) -> Result<()> {
    // Show progress for listing operation
    println!("🔍 Listing installed gpacks...");

    let gpacks = lister.list_installed()?;

    if gpacks.is_empty() {
        println!("ℹ️  No gpacks installed");
        return Ok(());
    }

    // Show progress for large result sets
    if gpacks.len() > 20 {
        println!("📊 Processing {} installed gpacks...", gpacks.len());
    }

    println!("📦 Installed Gpacks:");
    for gpack in gpacks {
        if args.detailed {
            println!("  ID: {}", gpack.id);
            println!("  Version: {}", gpack.version);
            println!("  SHA256: {}", gpack.sha256);
            println!("  Source: {}", gpack.source);
            println!();
        } else {
            println!("  {} ({})", gpack.id, gpack.version);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_displays_installed_gpacks() {
        let mut mock_lister = MockGpackLister::new();
        mock_lister.expect_list_installed().times(1).returning(|| {
            Ok(vec![InstalledGpack {
                id: "io.ggen.rust.cli".to_string(),
                version: "1.0.0".to_string(),
                sha256: "abc123".to_string(),
                source: "https://github.com/example/repo".to_string(),
            }])
        });

        let args = ListArgs { detailed: false };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_empty() {
        let mut mock_lister = MockGpackLister::new();
        mock_lister
            .expect_list_installed()
            .times(1)
            .returning(|| Ok(vec![]));

        let args = ListArgs { detailed: false };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }
}
