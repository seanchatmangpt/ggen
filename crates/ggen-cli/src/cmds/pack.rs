//! Pack Commands (singular alias for `packs`)
//!
//! This module provides the `ggen pack` noun as an alias for `ggen packs`,
//! supporting the golden-path form: `ggen pack add <name>`.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use ggen_core::packs::lockfile::PackLockfile;
use ggen_domain::packs::install::{install_pack, InstallInput};
use ggen_domain::packs::metadata::load_pack_metadata;

#[derive(Serialize)]
struct AddOutput {
    pack_name: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct RemoveOutput {
    pack_name: String,
    status: String,
    message: String,
}

/// Add (install) a pack by name
#[verb]
fn add(pack_name: String) -> Result<AddOutput> {
    // Verify the pack exists before attempting installation
    if let Err(e) = load_pack_metadata(&pack_name) {
        return Ok(AddOutput {
            pack_name: pack_name.clone(),
            status: "not_found".to_string(),
            message: format!(
                "Pack '{}' not found in local registry: {}. \
                 Ensure marketplace/packs/{}.toml exists.",
                pack_name, e, pack_name
            ),
        });
    }

    // Run the real installation via the domain layer
    let input = InstallInput {
        pack_id: pack_name.clone(),
        target_dir: None,
        force: false,
        dry_run: false,
    };

    let install_result = crate::runtime::block_on(install_pack(&input)).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to install pack '{}': {}",
            pack_name, e
        ))
    })?;
    let output = install_result.map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to install pack '{}': {}",
            pack_name, e
        ))
    })?;
    Ok(AddOutput {
        pack_name: output.pack_id.clone(),
        status: "installed".to_string(),
        message: format!(
            "Pack '{}' ({}) installed successfully. {} package(s) recorded, {} template(s) available. Lockfile: .ggen/packs.lock",
            output.pack_name,
            output.pack_id,
            output.packages_installed.len(),
            output.templates_available.len()
        ),
    })
}

fn validate_pack_name(pack_name: &str) -> Result<()> {
    if pack_name.trim().is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Pack name must not be empty",
        ));
    }
    let valid = pack_name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.');
    if !valid {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Pack name contains invalid characters. Use alphanumeric, hyphens, underscores only.",
        ));
    }
    Ok(())
}

/// Remove an installed pack
#[verb]
fn remove(pack_name: String) -> Result<RemoveOutput> {
    validate_pack_name(&pack_name)?;

    // Step 1: Resolve lock_path
    let lock_path = std::env::current_dir()
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Cannot resolve project directory: {}",
                e
            ))
        })?
        .join(".ggen")
        .join("packs.lock");

    // Step 2: Check if lockfile exists
    if !lock_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "No packs installed: .ggen/packs.lock not found",
        ));
    }

    // Step 3: Load lockfile
    let mut lockfile = PackLockfile::from_file(&lock_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to load lockfile: {}", e))
    })?;

    // Step 4: Check if pack exists in lockfile
    if lockfile.get_pack(&pack_name).is_none() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Pack '{}' is not installed",
            pack_name
        )));
    }

    // Step 5: Compute pack_dir and remove if exists
    let pack_dir = resolve_cache_dir()?.join(&pack_name);

    if pack_dir.exists() {
        std::fs::remove_dir_all(&pack_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to remove pack directory: {}",
                e
            ))
        })?;
    }

    // Step 6: Remove from lockfile
    lockfile.remove_pack(&pack_name);

    // Step 7: Save lockfile
    lockfile.save(&lock_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to save lockfile (partial removal may have occurred): {}",
            e
        ))
    })?;

    Ok(RemoveOutput {
        pack_name: pack_name.clone(),
        status: "removed".to_string(),
        message: format!(
            "Pack '{}' removed successfully. \
             Run `ggen packs list` to see remaining installed packs.",
            pack_name
        ),
    })
}

fn resolve_cache_dir() -> Result<PathBuf> {
    std::env::var_os("GGEN_PACK_CACHE_DIR")
        .map(PathBuf::from)
        .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
        .ok_or_else(|| {
            clap_noun_verb::NounVerbError::execution_error(
                "Cannot resolve pack cache: set HOME or GGEN_PACK_CACHE_DIR",
            )
        })
}
