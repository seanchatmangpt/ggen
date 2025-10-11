//! Installed gpacks listing and management.
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should provide visibility into project dependencies by reading
//! the lockfile and presenting installed gpacks with their metadata, helping
//! developers understand and audit their dependency tree.
//!
//! ## RESPONSIBILITIES
//! 1. **Dependency Reading**: Should parse .ggen/lock.json for installed packages
//! 2. **Metadata Display**: Should show versions, sources, checksums, licenses
//! 3. **Tree View**: Should optionally display dependency relationships
//! 4. **Outdated Detection**: Should flag packages with newer versions available
//! 5. **Security Audit**: Should warn about known vulnerabilities
//!
//! ## CONSTRAINTS
//! - Must handle missing or corrupt lockfile gracefully
//! - Must support both detailed and summary views
//! - Must verify checksums match lockfile
//! - Must work offline (reads local state only)
//! - Must handle large dependency trees efficiently
//!
//! ## DEPENDENCIES
//! - Lockfile (.ggen/lock.json): Should be the source of truth
//! - `GpackLister` trait: Should be mockable for testing
//! - Filesystem: Should read package metadata
//!
//! ## ERROR HANDLING STRATEGY
//! - Missing lockfile → Helpful message explaining initialization
//! - Corrupt lockfile → Suggest regeneration command
//! - Missing packages → Flag as inconsistent state
//! - Checksum mismatch → Security warning, suggest reinstall
//!
//! ## TESTING STRATEGY
//! - Mock GpackLister for deterministic tests
//! - Test with empty, small, and large dependency lists
//! - Test detailed vs summary modes
//! - Test error handling (missing files, corrupt JSON)
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Implement actual lockfile reading (currently placeholder)
//! - [P0] Add checksum verification
//! - [P1] Implement dependency tree visualization
//! - [P1] Add outdated package detection
//! - [P2] Support license compliance checking
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
