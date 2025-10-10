use anyhow::Result;
use clap::Args;
use ggen_core::{CacheManager, LockfileManager};
use std::env;

#[derive(Args, Debug)]
pub struct RemoveArgs {
    /// Gpack ID to remove
    pub gpack_id: String,

    /// Also remove from cache (frees disk space)
    #[arg(long)]
    pub prune: bool,
}

pub fn run(args: &RemoveArgs) -> Result<()> {
    // Get current working directory
    let current_dir = env::current_dir()?;
    let lockfile_manager = LockfileManager::new(&current_dir);
    let cache_manager = CacheManager::new()?;

    // Check if pack is installed
    if !lockfile_manager.is_installed(&args.gpack_id)? {
        println!("Gpack '{}' is not installed", args.gpack_id);
        return Ok(());
    }

    // Get pack info before removal
    let pack_entry = lockfile_manager
        .get(&args.gpack_id)?
        .expect("Pack should exist since is_installed returned true");

    // Remove from lockfile
    let removed = lockfile_manager.remove(&args.gpack_id)?;
    if !removed {
        println!("Failed to remove gpack '{}' from lockfile", args.gpack_id);
        return Ok(());
    }

    // Remove from cache if requested
    if args.prune {
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
