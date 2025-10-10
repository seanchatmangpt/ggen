use anyhow::{Context, Result};
use clap::Args;
use ggen_core::{CacheManager, LockfileManager, RegistryClient};
use ggen_utils::error::Result as GgenResult;
use std::env;

#[derive(Args, Debug)]
pub struct UpdateArgs {
    /// Specific gpack ID to update (if not provided, updates all)
    pub gpack_id: Option<String>,
}

/// Validate update command input
fn validate_update_input(args: &UpdateArgs) -> GgenResult<()> {
    if let Some(ref gpack_id) = args.gpack_id {
        if gpack_id.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
        }
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
    }
    Ok(())
}

pub async fn run(args: &UpdateArgs) -> Result<()> {
    validate_update_input(args)?;
    // Get current working directory
    println!("🔍 Loading installed gpacks...");
    let current_dir = env::current_dir().map_err(|e| {
        ggen_utils::error::Error::new_fmt(format_args!("Failed to access current directory: {}", e))
    })?;
    let lockfile_manager = LockfileManager::new(&current_dir);
    let cache_manager = CacheManager::new()?;
    let registry_client = RegistryClient::new()?;

    // Get installed packs
    let installed_packs = lockfile_manager.list()?;

    if installed_packs.is_empty() {
        println!("No gpacks installed in this project");
        return Ok(());
    }

    // Filter packs to update
    let packs_to_update = if let Some(gpack_id) = &args.gpack_id {
        installed_packs
            .into_iter()
            .filter(|pack| pack.id == *gpack_id)
            .collect::<Vec<_>>()
    } else {
        installed_packs
    };

    if packs_to_update.is_empty() {
        if let Some(gpack_id) = &args.gpack_id {
            println!("Gpack '{}' is not installed", gpack_id);
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
                    .with_context(|| format!("Failed to download updated gpack '{}'", pack.id))?;

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
        println!("\nUpdated {} gpack(s)", updated_count);
    } else {
        println!("\nAll gpacks are up to date");
    }

    Ok(())
}
