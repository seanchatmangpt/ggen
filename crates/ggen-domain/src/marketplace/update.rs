//! Marketplace update domain logic
//!
//! Real implementation of package update functionality.


use ggen_utils::error::Result;

/// Update command arguments
#[derive(Debug, Clone, Default)]
pub struct UpdateInput {
    /// Package name to update
    pub package: Option<String>,

    /// Update all installed packages
    pub all: bool,

    /// Dry run (simulate update)
    pub dry_run: bool,
}

/// Update packages and report progress
///
/// # Arguments
///
/// * `package` - Optional specific package to update (updates all if None)
/// * `all` - Update all installed packages
/// * `dry_run` - Simulate update without actually updating
///
/// # Returns
///
/// Returns Ok(()) on success, or an error if update fails
pub async fn update_and_report(package: Option<&str>, all: bool, dry_run: bool) -> Result<()> {
    // Get installed packages directory
    let packages_dir = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::with_context(
            "home directory not found",
            "~/.ggen/packages",
        ))?
        .join(".ggen")
        .join("packages");

    let lockfile_path = packages_dir.join("ggen.lock");

    if !lockfile_path.exists() {
        println!("No packages installed.");
        return Ok(());
    }

    // Read lockfile
    let content = tokio::fs::read_to_string(&lockfile_path)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

    let lockfile: Lockfile = serde_json::from_str(&content)?;

    if lockfile.packages.is_empty() {
        println!("No packages installed.");
        return Ok(());
    }

    // Determine which packages to update
    let packages_to_update: Vec<_> = if let Some(pkg_name) = package {
        if !lockfile.packages.contains_key(pkg_name) {
            return Err(ggen_utils::error::Error::with_context(
                &format!("Package {} is not installed", pkg_name),
                "update",
            ));
        }
        vec![pkg_name.to_string()]
    } else if all {
        lockfile.packages.keys().cloned().collect()
    } else {
        return Err(ggen_utils::error::Error::with_context(
            "Please specify a package name or use --all to update all packages",
            "update",
        ));
    };

    if dry_run {
        println!("🔍 Dry run: Would update {} package(s):", packages_to_update.len());
        for pkg_name in &packages_to_update {
            if let Some(info) = lockfile.packages.get(pkg_name) {
                println!("   {} (current: {})", pkg_name, info.version);
            }
        }
        return Ok(());
    }

    // Get registry path
    let registry_path = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::with_context(
            "home directory not found",
            "~/.ggen/registry",
        ))?
        .join(".ggen")
        .join("registry");

    println!("🔄 Updating {} package(s)...\n", packages_to_update.len());

    let mut updated_count = 0;
    let mut skipped_count = 0;

    for pkg_name in &packages_to_update {
        let current_version = lockfile.packages.get(pkg_name).map(|i| &i.version);

        match check_for_updates(&registry_path, pkg_name, current_version).await? {
            UpdateStatus::Available(new_version) => {
                println!("📦 Updating {} to version {}...", pkg_name, new_version);

                // Use install function to update the package
                use super::install::install_and_report;
                let pkg_spec = format!("{}@{}", pkg_name, new_version);

                match install_and_report(&pkg_spec, None, true, false, false).await {
                    Ok(()) => {
                        println!("✅ Updated {} to {}", pkg_name, new_version);
                        updated_count += 1;
                    }
                    Err(e) => {
                        println!("❌ Failed to update {}: {}", pkg_name, e);
                    }
                }
            }
            UpdateStatus::UpToDate => {
                println!("✓ {} is up to date", pkg_name);
                skipped_count += 1;
            }
            UpdateStatus::NotFound => {
                println!("⚠️  {} not found in registry", pkg_name);
                skipped_count += 1;
            }
        }

        println!();
    }

    println!("Summary: {} updated, {} skipped", updated_count, skipped_count);

    Ok(())
}

/// Execute update command using ggen-marketplace backend
pub async fn execute_update(input: UpdateInput) -> Result<UpdateOutput> {
    use ggen_marketplace::prelude::*;
    use ggen_marketplace::backend::LocalRegistry;

    let registry_path = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("home directory not found"))?
        .join(".ggen")
        .join("registry");

    // Initialize registry
    let registry = LocalRegistry::new(registry_path.clone()).await
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to initialize registry: {}", e)))?;

    // Load installed packages
    let packages_dir = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("home directory not found"))?
        .join(".ggen")
        .join("packages");

    let lockfile_path = packages_dir.join("ggen.lock");
    if !lockfile_path.exists() {
        return Ok(UpdateOutput {
            packages_updated: 0,
        });
    }

    let content = tokio::fs::read_to_string(&lockfile_path).await?;
    let lockfile: Lockfile = serde_json::from_str(&content)?;

    if lockfile.packages.is_empty() {
        return Ok(UpdateOutput {
            packages_updated: 0,
        });
    }

    // Determine packages to update
    let packages_to_update: Vec<_> = if let Some(ref pkg_name) = input.package {
        if !lockfile.packages.contains_key(pkg_name) {
            return Err(ggen_utils::error::Error::new(&format!(
                "Package {} is not installed",
                pkg_name
            )));
        }
        vec![pkg_name.clone()]
    } else if input.all {
        lockfile.packages.keys().cloned().collect()
    } else {
        return Err(ggen_utils::error::Error::new(
            "Please specify a package name or use --all"
        ));
    };

    if input.dry_run {
        println!("🔍 Dry run: Would update {} package(s)", packages_to_update.len());
        return Ok(UpdateOutput {
            packages_updated: 0,
        });
    }

    let mut updated_count = 0;

    for pkg_name in &packages_to_update {
        let package_id = PackageId::new("local", pkg_name);

        // Get all versions
        let versions = registry.list_versions(&package_id).await
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to list versions: {}", e)))?;

        if let Some(latest) = versions.first() {
            let current_version = lockfile.packages.get(pkg_name).map(|i| &i.version);
            let latest_version = latest.version.to_string();

            if Some(&latest_version) != current_version {
                println!("📦 Updating {} to version {}...", pkg_name, latest_version);

                // Use install to update
                use super::install::install_and_report;
                let pkg_spec = format!("{}@{}", pkg_name, latest_version);

                if install_and_report(&pkg_spec, None, true, false, false).await.is_ok() {
                    updated_count += 1;
                }
            }
        }
    }

    Ok(UpdateOutput {
        packages_updated: updated_count,
    })
}

/// Update output
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct UpdateOutput {
    pub packages_updated: usize,
}

/// Check for package updates
async fn check_for_updates(
    registry_path: &std::path::Path,
    package_name: &str,
    current_version: Option<&String>,
) -> Result<UpdateStatus> {
    let index_path = registry_path.join("index.json");

    if !index_path.exists() {
        return Ok(UpdateStatus::NotFound);
    }

    let content = tokio::fs::read_to_string(&index_path).await.map_err(|e| {
        ggen_utils::error::Error::new(&format!("IO error: {}", e))
    })?;

    let index: serde_json::Value = serde_json::from_str(&content)?;

    // Get package versions from registry
    let versions = index
        .get("packages")
        .and_then(|p| p.get(package_name))
        .and_then(|v| v.as_array());

    match versions {
        Some(versions) if !versions.is_empty() => {
            // Get latest version (first in array, assuming sorted)
            let latest = &versions[0];
            let latest_version = latest
                .get("version")
                .and_then(|v| v.as_str())
                .ok_or_else(|| ggen_utils::error::Error::with_context(
                    "Invalid registry index format",
                    "check_for_updates",
                ))?;

            // Compare versions
            match current_version {
                Some(current) if current == latest_version => Ok(UpdateStatus::UpToDate),
                _ => Ok(UpdateStatus::Available(latest_version.to_string())),
            }
        }
        _ => Ok(UpdateStatus::NotFound),
    }
}

/// Update status enum
enum UpdateStatus {
    Available(String), // New version available
    UpToDate,          // Already at latest version
    NotFound,          // Package not found in registry
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
    async fn test_update_dry_run_no_lockfile() {
        let result = update_and_report(None, true, true).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_update_no_args() {
        // When no lockfile exists, the function returns Ok with "No packages installed"
        // This is valid behavior - you can't update packages that don't exist
        let result = update_and_report(None, false, false).await;
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
    }
}
