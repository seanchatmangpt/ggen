//! Marketplace lockfile management for tracking installed packages
//!
//! The lockfile (.ggen/lock.json) serves as the source of truth for installed
//! packages, their versions, checksums, and dependencies. This enables:
//! - Deterministic builds (same lockfile = same dependencies)
//! - Security auditing (checksums verify package integrity)
//! - Dependency resolution (track transitive dependencies)
//! - Offline installs (verify cached packages)

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Lockfile structure for tracking installed packages
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Lockfile {
    /// Lockfile format version
    pub version: String,

    /// When the lockfile was last updated
    pub updated_at: String,

    /// Installed packages
    #[serde(default)]
    pub packages: HashMap<String, InstalledPackage>,
}

/// Information about an installed package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledPackage {
    /// Package name (e.g., "rig-mcp")
    pub name: String,

    /// Full package name (e.g., "rig-mcp-integration")
    pub full_name: String,

    /// Installed version
    pub version: String,

    /// SHA256 checksum of package contents
    pub checksum: String,

    /// Where the package was installed from
    pub source: String,

    /// Installation path relative to .ggen
    pub path: String,

    /// When this package was installed
    pub installed_at: String,

    /// Direct dependencies (package names)
    #[serde(default)]
    pub dependencies: Vec<String>,
}

impl Lockfile {
    /// Create a new empty lockfile
    pub fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
            updated_at: chrono::Utc::now().to_rfc3339(),
            packages: HashMap::new(),
        }
    }

    /// Load lockfile from default location (.ggen/lock.json)
    pub fn load() -> Result<Self> {
        let path = Self::default_path()?;
        Self::load_from_path(&path)
    }

    /// Load lockfile from specific path
    pub fn load_from_path(path: &Path) -> Result<Self> {
        if !path.exists() {
            // Return empty lockfile if it doesn't exist yet
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(path).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to read lockfile: {}",
                e
            ))
        })?;

        let lockfile: Lockfile = serde_json::from_str(&content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to parse lockfile JSON: {}",
                e
            ))
        })?;

        Ok(lockfile)
    }

    /// Save lockfile to default location
    pub fn save(&self) -> Result<()> {
        let path = Self::default_path()?;
        self.save_to_path(&path)
    }

    /// Save lockfile to specific path
    pub fn save_to_path(&self, path: &Path) -> Result<()> {
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ggen_utils::error::Error::new_fmt(format_args!(
                    "Failed to create .ggen directory: {}",
                    e
                ))
            })?;
        }

        let content = serde_json::to_string_pretty(self).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to serialize lockfile: {}",
                e
            ))
        })?;

        std::fs::write(path, content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to write lockfile: {}",
                e
            ))
        })?;

        Ok(())
    }

    /// Get default lockfile path (.ggen/lock.json)
    pub fn default_path() -> Result<PathBuf> {
        let current_dir = std::env::current_dir().map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to get current directory: {}",
                e
            ))
        })?;

        Ok(current_dir.join(".ggen/lock.json"))
    }

    /// Add or update a package in the lockfile
    pub fn add_package(&mut self, package: InstalledPackage) {
        self.packages.insert(package.name.clone(), package);
        self.updated_at = chrono::Utc::now().to_rfc3339();
    }

    /// Remove a package from the lockfile
    pub fn remove_package(&mut self, name: &str) -> Option<InstalledPackage> {
        self.updated_at = chrono::Utc::now().to_rfc3339();
        self.packages.remove(name)
    }

    /// Get a package by name
    pub fn get_package(&self, name: &str) -> Option<&InstalledPackage> {
        self.packages.get(name)
    }

    /// Check if a package is installed
    pub fn has_package(&self, name: &str) -> bool {
        self.packages.contains_key(name)
    }

    /// List all installed packages
    pub fn list_packages(&self) -> Vec<&InstalledPackage> {
        self.packages.values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lockfile_new() {
        let lockfile = Lockfile::new();
        assert_eq!(lockfile.version, "1.0.0");
        assert!(lockfile.packages.is_empty());
    }

    #[test]
    fn test_add_package() {
        let mut lockfile = Lockfile::new();

        let package = InstalledPackage {
            name: "test-pkg".to_string(),
            full_name: "test-package".to_string(),
            version: "1.0.0".to_string(),
            checksum: "abc123".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/test-pkg".to_string(),
            installed_at: chrono::Utc::now().to_rfc3339(),
            dependencies: vec![],
        };

        lockfile.add_package(package.clone());

        assert_eq!(lockfile.packages.len(), 1);
        assert!(lockfile.has_package("test-pkg"));

        let retrieved = lockfile.get_package("test-pkg").unwrap();
        assert_eq!(retrieved.version, "1.0.0");
    }

    #[test]
    fn test_remove_package() {
        let mut lockfile = Lockfile::new();

        let package = InstalledPackage {
            name: "test-pkg".to_string(),
            full_name: "test-package".to_string(),
            version: "1.0.0".to_string(),
            checksum: "abc123".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/test-pkg".to_string(),
            installed_at: chrono::Utc::now().to_rfc3339(),
            dependencies: vec![],
        };

        lockfile.add_package(package);
        assert_eq!(lockfile.packages.len(), 1);

        let removed = lockfile.remove_package("test-pkg");
        assert!(removed.is_some());
        assert_eq!(lockfile.packages.len(), 0);
    }

    #[test]
    fn test_list_packages() {
        let mut lockfile = Lockfile::new();

        for i in 0..3 {
            let package = InstalledPackage {
                name: format!("pkg-{}", i),
                full_name: format!("package-{}", i),
                version: "1.0.0".to_string(),
                checksum: "abc123".to_string(),
                source: "registry".to_string(),
                path: format!(".ggen/packages/pkg-{}", i),
                installed_at: chrono::Utc::now().to_rfc3339(),
                dependencies: vec![],
            };
            lockfile.add_package(package);
        }

        let packages = lockfile.list_packages();
        assert_eq!(packages.len(), 3);
    }
}
