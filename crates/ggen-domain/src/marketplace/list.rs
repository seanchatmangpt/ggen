//! Marketplace list domain logic
//!
//! Real implementation of installed packages listing functionality using ggen-marketplace-v2.

use ggen_utils::error::Result;

/// List command arguments
#[derive(Debug, Clone, Default)]
pub struct ListInput {
    /// Show detailed information
    pub detailed: bool,

    /// Output as JSON
    pub json: bool,
}

/// List installed packages and display information
///
/// # Arguments
///
/// * `detailed` - Show detailed information for each package
/// * `json` - Output results in JSON format
///
/// # Returns
///
/// Returns Ok(()) on success, or an error if listing fails
pub async fn list_and_display(detailed: bool, json: bool) -> Result<()> {
    // FM22 (RPN 350): Missing home directory - fail fast for determinism (no temp fallback)
    let packages_dir = dirs::home_dir()
        .ok_or_else(|| {
            ggen_utils::error::Error::new(
                "❌ Home directory not found. Cannot determine packages directory. Marketplace operations require a valid home directory."
            )
        })?
        .join(".ggen")
        .join("packages");

    // Read lockfile to get installed packages
    let lockfile_path = packages_dir.join("ggen.lock");

    if !lockfile_path.exists() {
        if json {
            ggen_utils::alert_info!("[]");
        } else {
            ggen_utils::alert_info!("No packages installed.");
        }
        return Ok(());
    }

    // FM21 (RPN 240): Corrupted lockfile - handle gracefully with recovery
    let content = tokio::fs::read_to_string(&lockfile_path)
        .await
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to read lockfile from {}: {}. Lockfile may be corrupted.",
                lockfile_path.display(),
                e
            ))
        })?;

    let lockfile: Lockfile = match serde_json::from_str(&content) {
        Ok(lockfile) => lockfile,
        Err(e) => {
            // Try to recover from backup
            let backup_path = lockfile_path.with_extension("lock.backup");
            if backup_path.exists() {
                match tokio::fs::read_to_string(&backup_path).await {
                    Ok(backup_content) => match serde_json::from_str::<Lockfile>(&backup_content) {
                        Ok(backup_lockfile) => {
                            tracing::info!("Recovered lockfile from backup for list operation");
                            backup_lockfile
                        }
                        Err(_) => {
                            return Err(ggen_utils::error::Error::new(&format!(
                                    "Lockfile corrupted at {} and backup also corrupted: {}. Please manually restore or delete lockfile.",
                                    lockfile_path.display(),
                                    e
                                )));
                        }
                    },
                    Err(_) => {
                        return Err(ggen_utils::error::Error::new(&format!(
                            "Lockfile corrupted at {} and backup unreadable: {}. Please manually restore or delete lockfile.",
                            lockfile_path.display(),
                            e
                        )));
                    }
                }
            } else {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Lockfile corrupted at {} and no backup available: {}. Please manually restore or delete lockfile.",
                    lockfile_path.display(),
                    e
                )));
            }
        }
    };

    if lockfile.packages.is_empty() {
        if json {
            ggen_utils::alert_info!("[]");
        } else {
            ggen_utils::alert_info!("No packages installed.");
        }
        return Ok(());
    }

    if json {
        // Output JSON format
        let packages: Vec<_> = lockfile
            .packages
            .iter()
            .map(|(name, info)| {
                serde_json::json!({
                    "name": name,
                    "version": info.version,
                    "installed_at": info.installed_at
                })
            })
            .collect();

        let json_output = serde_json::to_string_pretty(&packages)?;
        ggen_utils::alert_info!("{}", json_output);
    } else {
        // Output human-readable format
        ggen_utils::alert_info!("Installed packages ({}):\n", lockfile.packages.len());

        for (name, info) in &lockfile.packages {
            ggen_utils::alert_info!("📦 {}", name);
            ggen_utils::alert_info!("   Version: {}", info.version);

            if detailed {
                if let Some(installed_at) = &info.installed_at {
                    ggen_utils::alert_info!("   Installed: {}", installed_at);
                }

                // Check if package directory exists
                let pkg_dir = packages_dir.join(name);
                if pkg_dir.exists() {
                    ggen_utils::alert_info!("   Location: {}", pkg_dir.display());
                } else {
                    ggen_utils::alert_warning!("Package directory not found");
                }
            }
        }
    }

    Ok(())
}

/// Execute list command using ggen-marketplace-v2 backend
pub async fn execute_list(_input: ListInput) -> Result<ListOutput> {
    use ggen_marketplace_v2::prelude::*;
    use ggen_marketplace_v2::RdfRegistry;
    use std::path::PathBuf;

    // FM22 (RPN 350): Missing home directory - fail fast for determinism (no temp fallback)
    let _registry_path = dirs::home_dir()
        .ok_or_else(|| {
            ggen_utils::error::Error::new(
                "❌ Home directory not found. Cannot determine registry directory. Marketplace operations require a valid home directory."
            )
        })?
        .join(".ggen")
        .join("registry");

    // Initialize RDF registry (v2 backend - in-memory oxigraph store)
    let registry = RdfRegistry::new();

    // FM22 (RPN 350): Missing home directory - fail fast for determinism (no temp fallback)
    let packages_dir = dirs::home_dir()
        .ok_or_else(|| {
            ggen_utils::error::Error::new(
                "❌ Home directory not found. Cannot determine packages directory. Marketplace operations require a valid home directory."
            )
        })?
        .join(".ggen")
        .join("packages");

    let lockfile_path = packages_dir.join("ggen.lock");
    let lockfile: Lockfile = if lockfile_path.exists() {
        let content = tokio::fs::read_to_string(&lockfile_path).await?;
        serde_json::from_str(&content)?
    } else {
        Lockfile {
            version: String::new(),
            packages: std::collections::HashMap::new(),
        }
    };

    // Note: v2 RDF registry is in-memory, metadata operations pending
    // let _metadata = registry.metadata().await?;

    let mut packages = vec![];

    // Iterate through lockfile packages
    for (name, info) in &lockfile.packages {
        // Note: PackageId in v2 uses qualified format "namespace/name"
        let package_id = match PackageId::new(&format!("local/{}", name)) {
            Ok(id) => id,
            Err(_) => continue, // Skip invalid package IDs
        };

        if let Ok(pkg) = registry.get_package(&package_id).await {
            packages.push(PackageListItem {
                name: name.clone(),
                version: info.version.clone(),
                title: pkg.metadata.name.clone(),
                description: pkg.metadata.description.clone(),
                installed_at: info.installed_at.clone(),
            });
        }
    }

    // CRITICAL FIX: Fallback to repo marketplace packages if no installed packages
    // ROOT CAUSE: Developers expect to see all available packages, not just installed ones
    // WHY: `marketplace list` should show what's available, not just what's installed
    if packages.is_empty() {
        // Search for packages in marketplace/packages directory (relative to project root)
        let repo_marketplace_dir = PathBuf::from("marketplace/packages");

        if repo_marketplace_dir.exists() {
            if let Ok(entries) = std::fs::read_dir(&repo_marketplace_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_dir() {
                        let package_toml = path.join("package.toml");
                        if package_toml.exists() {
                            // Parse package.toml to get package info
                            if let Ok(content) = std::fs::read_to_string(&package_toml) {
                                if let Ok(pkg_toml) = toml::from_str::<toml::Value>(&content) {
                                    let package_table = pkg_toml.get("package");
                                    if let Some(pkg) = package_table {
                                        // Get package name from TOML or fallback to directory name
                                        let dir_name =
                                            entry.file_name().to_string_lossy().to_string();
                                        let name = pkg
                                            .get("name")
                                            .and_then(|v| v.as_str())
                                            .unwrap_or(&dir_name);
                                        let version = pkg
                                            .get("version")
                                            .and_then(|v| v.as_str())
                                            .unwrap_or("0.1.0");
                                        let description = pkg
                                            .get("description")
                                            .and_then(|v| v.as_str())
                                            .unwrap_or("");

                                        packages.push(PackageListItem {
                                            name: name.to_string(),
                                            version: version.to_string(),
                                            title: name.to_string(),
                                            description: description.to_string(),
                                            installed_at: None, // Not installed, available in repo
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(ListOutput {
        packages_listed: packages.len(),
        packages,
    })
}

/// List output
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct ListOutput {
    pub packages_listed: usize,
    #[serde(skip)]
    pub packages: Vec<PackageListItem>,
}

/// Package list item
#[derive(Debug, Clone, serde::Serialize)]
pub struct PackageListItem {
    pub name: String,
    pub version: String,
    pub title: String,
    pub description: String,
    pub installed_at: Option<String>,
}

/// Lockfile structure
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Lockfile {
    #[serde(default)]
    version: String,
    #[serde(default)]
    packages: std::collections::HashMap<String, PackageInfo>,
}

/// Package info in lockfile
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct PackageInfo {
    version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    installed_at: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_no_lockfile() {
        // This test will use the actual home directory lockfile if it exists
        // For a true unit test, we'd need to mock dirs::home_dir()
        let result = list_and_display(false, true).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_lockfile_deserialization() {
        let lockfile_json = r#"{
            "version": "1.0",
            "packages": {
                "test/example": {
                    "version": "1.0.0",
                    "installed_at": "2024-01-01T00:00:00Z"
                }
            }
        }"#;

        let lockfile: Lockfile = serde_json::from_str(lockfile_json).unwrap();
        assert_eq!(lockfile.packages.len(), 1);
        assert_eq!(
            lockfile.packages.get("test/example").unwrap().version,
            "1.0.0"
        );
    }

    #[test]
    fn test_package_info_serialization() {
        let info = PackageInfo {
            version: "1.0.0".to_string(),
            installed_at: Some("2024-01-01T00:00:00Z".to_string()),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: PackageInfo = serde_json::from_str(&json).unwrap();

        assert_eq!(info.version, deserialized.version);
        assert_eq!(info.installed_at, deserialized.installed_at);
    }
}
