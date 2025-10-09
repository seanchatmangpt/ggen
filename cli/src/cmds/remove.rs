use anyhow::Result;
use clap::Args;
use core::{CacheManager, LockfileManager};
use std::env;

#[derive(Args, Debug)]
pub struct RemoveArgs {
    /// Rpack ID to remove
    pub rpack_id: String,

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
    if !lockfile_manager.is_installed(&args.rpack_id)? {
        println!("Rpack '{}' is not installed", args.rpack_id);
        return Ok(());
    }

    // Get pack info before removal
    let pack_entry = lockfile_manager
        .get(&args.rpack_id)?
        .expect("Pack should exist since is_installed returned true");

    // Remove from lockfile
    let removed = lockfile_manager.remove(&args.rpack_id)?;
    if !removed {
        println!("Failed to remove rpack '{}' from lockfile", args.rpack_id);
        return Ok(());
    }

    // Remove from cache if requested
    if args.prune {
        if let Err(e) = cache_manager.remove(&args.rpack_id, &pack_entry.version) {
            println!("Warning: Failed to remove rpack from cache: {}", e);
        } else {
            println!("Removed rpack from cache");
        }
    }

    println!(
        "✅ Successfully removed rpack '{}' version {}",
        args.rpack_id, pack_entry.version
    );

    Ok(())
}
