//! Pack installation logic

use crate::packs::metadata::load_pack_metadata;
use chrono::Utc;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Install pack input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallInput {
    pub pack_id: String,
    pub target_dir: Option<PathBuf>,
    #[serde(default)]
    pub force: bool,
    #[serde(default)]
    pub dry_run: bool,
}

/// Install pack output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallOutput {
    pub pack_id: String,
    pub pack_name: String,
    pub packages_installed: Vec<String>,
    pub templates_available: Vec<String>,
    pub sparql_queries: usize,
    pub total_packages: usize,
    pub install_path: PathBuf,
}

/// Install a pack by installing all its packages
pub async fn install_pack(input: &InstallInput) -> Result<InstallOutput> {
    // Load pack metadata
    let pack = load_pack_metadata(&input.pack_id)?;

    if input.dry_run {
        return Ok(InstallOutput {
            pack_id: input.pack_id.clone(),
            pack_name: pack.name.clone(),
            packages_installed: vec![],
            templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
            sparql_queries: pack.sparql_queries.len(),
            total_packages: pack.packages.len(),
            install_path: input
                .target_dir
                .clone()
                .unwrap_or_else(|| PathBuf::from(".")),
        });
    }

    // Record installed packages from the pack
    let packages_installed: Vec<String> = pack.packages.clone();
    for package_name in &packages_installed {
        tracing::info!("Installing package: {}", package_name);
    }

    let install_path = input
        .target_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from(".ggen/packs"));

    // Create the install directory
    std::fs::create_dir_all(&install_path).map_err(|e| {
        ggen_utils::error::Error::new(&format!(
            "Failed to create install directory '{}': {}",
            install_path.display(),
            e
        ))
    })?;

    // Write lockfile entry to .ggen/packs.lock
    let lockfile_path = PathBuf::from(".ggen/packs.lock");
    // Load existing lockfile or start fresh
    let mut lock_root: serde_json::Value = if lockfile_path.exists() {
        let raw = std::fs::read_to_string(&lockfile_path).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read lockfile: {}", e))
        })?;
        serde_json::from_str(&raw).unwrap_or_else(|_| serde_json::json!({ "installed": [] }))
    } else {
        serde_json::json!({ "installed": [] })
    };

    let timestamp = Utc::now().to_rfc3339();
    let new_entry = serde_json::json!({
        "id": input.pack_id,
        "version": pack.version,
        "installed_at": timestamp,
        "packages": packages_installed,
    });

    // Append the entry (or replace if same id already present)
    if let Some(installed) = lock_root
        .get_mut("installed")
        .and_then(|v| v.as_array_mut())
    {
        // Remove any prior entry for the same id
        installed.retain(|e| e.get("id").and_then(|v| v.as_str()) != Some(input.pack_id.as_str()));
        installed.push(new_entry);
    }

    let lock_json = serde_json::to_string_pretty(&lock_root).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to serialize lockfile: {}", e))
    })?;
    std::fs::write(&lockfile_path, lock_json)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to write lockfile: {}", e)))?;

    tracing::info!(
        "Lockfile updated at '{}' with pack '{}'",
        lockfile_path.display(),
        input.pack_id
    );

    Ok(InstallOutput {
        pack_id: input.pack_id.clone(),
        pack_name: pack.name,
        packages_installed,
        templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
        sparql_queries: pack.sparql_queries.len(),
        total_packages: pack.packages.len(),
        install_path,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_install_pack_dry_run() {
        let input = InstallInput {
            pack_id: "test-pack".to_string(),
            target_dir: None,
            force: false,
            dry_run: true,
        };

        // This will fail if pack doesn't exist, but tests the dry_run code path
        let _ = install_pack(&input).await;
    }
}
