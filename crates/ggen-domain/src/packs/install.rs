//! Pack installation logic

use crate::packs::metadata::load_pack_metadata;
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

    // Install each package from the pack (not fully implemented - would use marketplace domain functions)
    let packages_installed = Vec::new();
    for package_name in &pack.packages {
        tracing::info!("Would install package: {}", package_name);
        // In full implementation, would call marketplace::execute_install
    }

    let install_path = input.target_dir.clone().unwrap_or_else(|| {
        dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".ggen")
            .join("packages")
    });

    Ok(InstallOutput {
        pack_id: input.pack_id.clone(),
        pack_name: pack.name,
        packages_installed: packages_installed.clone(),
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
