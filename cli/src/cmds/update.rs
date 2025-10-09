use anyhow::{Context, Result};
use clap::Args;
use ggen_core::{CacheManager, LockfileManager, RegistryClient};
use std::env;

#[derive(Args, Debug)]
pub struct UpdateArgs {
    /// Specific rpack ID to update (if not provided, updates all)
    pub rpack_id: Option<String>,
}

pub async fn run(args: &UpdateArgs) -> Result<()> {
    // Get current working directory
    let current_dir = env::current_dir()?;
    let lockfile_manager = LockfileManager::new(&current_dir);
    let cache_manager = CacheManager::new()?;
    let registry_client = RegistryClient::new()?;

    // Get installed packs
    let installed_packs = lockfile_manager.list()?;

    if installed_packs.is_empty() {
        println!("No rpacks installed in this project");
        return Ok(());
    }

    // Filter packs to update
    let packs_to_update = if let Some(rpack_id) = &args.rpack_id {
        installed_packs
            .into_iter()
            .filter(|pack| pack.id == *rpack_id)
            .collect::<Vec<_>>()
    } else {
        installed_packs
    };

    if packs_to_update.is_empty() {
        if let Some(rpack_id) = &args.rpack_id {
            println!("Rpack '{}' is not installed", rpack_id);
        }
        return Ok(());
    }

    let mut updated_count = 0;

    for pack in packs_to_update {
        println!("Checking for updates to '{}'...", pack.id);

        // Check for updates
        match registry_client.check_updates(&pack.id, &pack.version).await {
            Ok(Some(new_pack)) => {
                println!("Found update: {} -> {}", pack.version, new_pack.version);

                // Download new version
                let _cached_pack = cache_manager
                    .ensure(&new_pack)
                    .await
                    .with_context(|| format!("Failed to download updated rpack '{}'", pack.id))?;

                // Update lockfile
                lockfile_manager.upsert(
                    &new_pack.id,
                    &new_pack.version,
                    &new_pack.sha256,
                    &new_pack.git_url,
                )?;

                println!("✅ Updated '{}' to version {}", pack.id, new_pack.version);
                updated_count += 1;
            }
            Ok(None) => {
                println!("'{}' is up to date", pack.id);
            }
            Err(e) => {
                println!("Warning: Failed to check updates for '{}': {}", pack.id, e);
            }
        }
    }

    if updated_count > 0 {
        println!("\nUpdated {} rpack(s)", updated_count);
    } else {
        println!("\nAll rpacks are up to date");
    }

    Ok(())
}
