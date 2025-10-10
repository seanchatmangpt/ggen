use anyhow::Result;
use clap::Args;
use ggen_core::{CacheManager, LockfileManager};
use ggen_utils::error::Result as GgenResult;
use std::env;

#[derive(Args, Debug)]
pub struct RemoveArgs {
    /// Gpack ID to remove
    pub gpack_id: String,

    /// Also remove from cache (frees disk space)
    #[arg(long)]
    pub prune: bool,
}

/// Validate and sanitize gpack ID input
fn validate_gpack_id(gpack_id: &str) -> GgenResult<()> {
    // Validate gpack ID is not empty
    if gpack_id.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
    }
    // Validate gpack ID length
    if gpack_id.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Gpack ID too long (max 200 characters)",
        ));
    }
    // Validate gpack ID format (basic pattern check)
    if !gpack_id
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid gpack ID format: only alphanumeric characters, dots, dashes, and underscores allowed",
        ));
    }
    Ok(())
}

pub fn run(args: &RemoveArgs) -> Result<()> {
    validate_gpack_id(&args.gpack_id)?;
    // Get current working directory
    let current_dir = env::current_dir()?;
    let lockfile_manager = LockfileManager::new(&current_dir);
    let cache_manager = CacheManager::new()?;

    // Check if pack is installed
    println!("🔍 Checking if gpack is installed...");
    if !lockfile_manager.is_installed(&args.gpack_id)? {
        println!("Gpack '{}' is not installed", args.gpack_id);
        return Ok(());
    }

    // Get pack info before removal
    let pack_entry = lockfile_manager
        .get(&args.gpack_id)?
        .expect("Pack should exist since is_installed returned true");

    // Remove from lockfile
    println!("🗑️  Removing gpack from lockfile...");
    let removed = lockfile_manager.remove(&args.gpack_id)?;
    if !removed {
        println!("Failed to remove gpack '{}' from lockfile", args.gpack_id);
        return Ok(());
    }

    // Remove from cache if requested
    if args.prune {
        println!("🧹 Removing gpack from cache...");
        if let Err(e) = cache_manager.remove(&args.gpack_id, &pack_entry.version) {
            println!("Warning: Failed to remove gpack from cache: {}", e);
        } else {
            println!("Removed gpack from cache");
        }
    }

    println!(
        "✅ Successfully removed gpack '{}' version {}",
        args.gpack_id, pack_entry.version
    );

    Ok(())
}
