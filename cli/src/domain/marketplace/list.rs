//! Marketplace list domain logic
//!
//! Real implementation of installed packages listing functionality.

use clap::Args;
use ggen_utils::error::Result;
use serde_json;

/// List command arguments
#[derive(Debug, Args)]
pub struct ListArgs {
    /// Show detailed information
    #[arg(short = 'd', long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(short = 'j', long)]
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
    // Get installed packages directory
    let packages_dir = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("home directory not found"))?
        .join(".ggen")
        .join("packages");

    // Read lockfile to get installed packages
    let lockfile_path = packages_dir.join("ggen.lock");

    if !lockfile_path.exists() {
        if json {
            println!("[]");
        } else {
            println!("No packages installed.");
        }
        return Ok(());
    }

    let content = tokio::fs::read_to_string(&lockfile_path)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

    let lockfile: Lockfile = serde_json::from_str(&content)?;

    if lockfile.packages.is_empty() {
        if json {
            println!("[]");
        } else {
            println!("No packages installed.");
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
        println!("{}", json_output);
    } else {
        // Output human-readable format
        println!("Installed packages ({}):\n", lockfile.packages.len());

        for (name, info) in &lockfile.packages {
            println!("📦 {}", name);
            println!("   Version: {}", info.version);

            if detailed {
                if let Some(installed_at) = &info.installed_at {
                    println!("   Installed: {}", installed_at);
                }

                // Check if package directory exists
                let pkg_dir = packages_dir.join(name);
                if pkg_dir.exists() {
                    println!("   Location: {}", pkg_dir.display());
                } else {
                    println!("   ⚠️  Package directory not found");
                }
            }

            println!();
        }
    }

    Ok(())
}

/// Run list command (sync wrapper for CLI)
pub fn run(args: &ListArgs) -> Result<()> {
    crate::runtime::block_on(async { list_and_display(args.detailed, args.json).await })
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
    use tempfile::tempdir;

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
