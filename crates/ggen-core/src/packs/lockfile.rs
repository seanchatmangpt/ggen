//! Pack lockfile management for ggen v4.0
//!
//! This module provides the Pack Installation System lockfile functionality,
//! which tracks installed packs with their sources, versions, and dependencies.
//! The lockfile ensures reproducible pack installations across environments.
//!
//! ## Features
//!
//! - **Pack tracking**: Store installed pack versions with checksums
//! - **Multiple sources**: Support Registry, GitHub, and Local sources
//! - **Dependency management**: Track pack dependencies for resolution
//! - **Integrity verification**: Optional checksums for pack verification
//! - **JSON serialization**: Standard .ggen/packs.lock format
//!
//! ## Lockfile Format
//!
//! The `.ggen/packs.lock` file is a JSON file with the following structure:
//!
//! ```json
//! {
//!   "packs": {
//!     "io.ggen.rust.cli": {
//!       "version": "1.0.0",
//!       "source": {
//!         "Registry": { "url": "https://registry.ggen.io" }
//!       },
//!       "integrity": "sha256-abc123...",
//!       "installed_at": "2024-01-01T00:00:00Z",
//!       "dependencies": ["io.ggen.macros.std"]
//!     }
//!   },
//!   "updated_at": "2024-01-01T00:00:00Z",
//!   "ggen_version": "4.0.0"
//! }
//! ```
//!
//! ## Examples
//!
//! ### Creating a Pack Lockfile
//!
//! ```rust
//! use ggen_core::packs::lockfile::{PackLockfile, LockedPack, PackSource};
//! use std::collections::BTreeMap;
//! use chrono::Utc;
//!
//! let mut lockfile = PackLockfile {
//!     packs: BTreeMap::new(),
//!     updated_at: Utc::now(),
//!     ggen_version: "4.0.0".to_string(),
//! };
//!
//! let pack = LockedPack {
//!     version: "1.0.0".to_string(),
//!     source: PackSource::Registry {
//!         url: "https://registry.ggen.io".to_string()
//!     },
//!     integrity: Some("sha256-abc123".to_string()),
//!     installed_at: Utc::now(),
//!     dependencies: vec!["io.ggen.macros.std".to_string()],
//! };
//!
//! lockfile.packs.insert("io.ggen.rust.cli".to_string(), pack);
//! ```
//!
//! ### Loading from File
//!
//! ```rust,no_run
//! use ggen_core::packs::lockfile::PackLockfile;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let lockfile = PackLockfile::from_file(Path::new(".ggen/packs.lock"))?;
//! println!("Loaded {} packs", lockfile.packs.len());
//! # Ok(())
//! # }
//! ```

use chrono::{DateTime, Utc};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

/// Pack lockfile containing all installed packs
///
/// This structure represents the `.ggen/packs.lock` file, which tracks
/// all installed packs, their versions, sources, and dependencies.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackLockfile {
    /// Map of pack IDs to their locked versions
    /// Uses BTreeMap for deterministic ordering
    pub packs: BTreeMap<String, LockedPack>,

    /// When the lockfile was last updated
    pub updated_at: DateTime<Utc>,

    /// Version of ggen that created this lockfile
    pub ggen_version: String,
}

/// A locked pack with its installation metadata
///
/// Contains all information needed to reproduce a pack installation,
/// including source, version, integrity checksum, and dependencies.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LockedPack {
    /// Semantic version of the pack (e.g., "1.0.0")
    pub version: String,

    /// Source where the pack was installed from
    pub source: PackSource,

    /// Optional integrity checksum for verification (e.g., "sha256-abc123...")
    /// Format: algorithm-base64_hash
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrity: Option<String>,

    /// Timestamp when the pack was installed
    pub installed_at: DateTime<Utc>,

    /// List of pack IDs this pack depends on
    /// Empty vector if no dependencies
    #[serde(default)]
    pub dependencies: Vec<String>,
}

/// Source from which a pack was installed
///
/// Supports three types of pack sources:
/// - Registry: Official ggen registry
/// - GitHub: Direct from GitHub repository
/// - Local: Local filesystem path
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub enum PackSource {
    /// Pack installed from a registry
    Registry {
        /// Registry URL (e.g., "https://registry.ggen.io")
        url: String,
    },

    /// Pack installed from GitHub
    GitHub {
        /// GitHub organization or user (e.g., "seanchatmangpt")
        org: String,
        /// Repository name (e.g., "ggen")
        repo: String,
        /// Branch or tag (e.g., "main", "v1.0.0")
        branch: String,
    },

    /// Pack installed from local filesystem
    Local {
        /// Absolute or relative path to pack directory
        path: PathBuf,
    },
}

impl PackLockfile {
    /// Create a new empty lockfile with current timestamp
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::packs::lockfile::PackLockfile;
    ///
    /// let lockfile = PackLockfile::new("4.0.0");
    /// assert_eq!(lockfile.packs.len(), 0);
    /// assert_eq!(lockfile.ggen_version, "4.0.0");
    /// ```
    pub fn new(ggen_version: impl Into<String>) -> Self {
        Self {
            packs: BTreeMap::new(),
            updated_at: Utc::now(),
            ggen_version: ggen_version.into(),
        }
    }

    /// Load lockfile from file
    ///
    /// Reads and deserializes a lockfile from the given path.
    /// Returns an error if the file doesn't exist or contains invalid JSON.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the lockfile (typically `.ggen/packs.lock`)
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::packs::lockfile::PackLockfile;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let lockfile = PackLockfile::from_file(Path::new(".ggen/packs.lock"))?;
    /// println!("Loaded {} packs", lockfile.packs.len());
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_file(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Err(Error::new(&format!(
                "Lockfile not found at path: {}",
                path.display()
            )));
        }

        let content = fs::read_to_string(path).map_err(|e| {
            Error::with_context(
                "Failed to read lockfile",
                &format!("{}: {}", path.display(), e),
            )
        })?;

        let lockfile: PackLockfile = serde_json::from_str(&content).map_err(|e| {
            Error::with_context(
                "Failed to parse lockfile JSON",
                &format!("{}: {}", path.display(), e),
            )
        })?;

        // Validate after loading
        lockfile.validate()?;

        Ok(lockfile)
    }

    /// Save lockfile to file
    ///
    /// Serializes the lockfile to JSON and writes it to the given path.
    /// Creates parent directories if they don't exist.
    ///
    /// # Arguments
    ///
    /// * `path` - Path where the lockfile should be saved
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::packs::lockfile::PackLockfile;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let lockfile = PackLockfile::new("4.0.0");
    /// lockfile.save(Path::new(".ggen/packs.lock"))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn save(&self, path: &Path) -> Result<()> {
        // Validate before saving
        self.validate()?;

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                Error::with_context(
                    "Failed to create lockfile directory",
                    &format!("{}: {}", parent.display(), e),
                )
            })?;
        }

        let json = serde_json::to_string_pretty(self)
            .map_err(|e| Error::with_context("Failed to serialize lockfile", &e.to_string()))?;

        fs::write(path, json).map_err(|e| {
            Error::with_context(
                "Failed to write lockfile",
                &format!("{}: {}", path.display(), e),
            )
        })?;

        Ok(())
    }

    /// Get a pack by ID
    ///
    /// Returns a reference to the locked pack if it exists in the lockfile.
    ///
    /// # Arguments
    ///
    /// * `pack_id` - The pack identifier to look up
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::packs::lockfile::{PackLockfile, LockedPack, PackSource};
    /// use std::collections::BTreeMap;
    /// use chrono::Utc;
    ///
    /// let mut lockfile = PackLockfile::new("4.0.0");
    /// let pack = LockedPack {
    ///     version: "1.0.0".to_string(),
    ///     source: PackSource::Registry {
    ///         url: "https://registry.ggen.io".to_string()
    ///     },
    ///     integrity: None,
    ///     installed_at: Utc::now(),
    ///     dependencies: vec![],
    /// };
    /// lockfile.packs.insert("test.pack".to_string(), pack);
    ///
    /// assert!(lockfile.get_pack("test.pack").is_some());
    /// assert!(lockfile.get_pack("missing.pack").is_none());
    /// ```
    pub fn get_pack(&self, pack_id: &str) -> Option<&LockedPack> {
        self.packs.get(pack_id)
    }

    /// Add or update a pack in the lockfile
    ///
    /// Updates the `updated_at` timestamp when adding a pack.
    ///
    /// # Arguments
    ///
    /// * `pack_id` - Unique pack identifier
    /// * `pack` - The locked pack to add
    pub fn add_pack(&mut self, pack_id: impl Into<String>, pack: LockedPack) {
        self.packs.insert(pack_id.into(), pack);
        self.updated_at = Utc::now();
    }

    /// Remove a pack from the lockfile
    ///
    /// Returns true if the pack was removed, false if it didn't exist.
    /// Updates the `updated_at` timestamp when removing a pack.
    pub fn remove_pack(&mut self, pack_id: &str) -> bool {
        let removed = self.packs.remove(pack_id).is_some();
        if removed {
            self.updated_at = Utc::now();
        }
        removed
    }

    /// Validate the lockfile for consistency
    ///
    /// Checks:
    /// - All pack dependencies are also in the lockfile
    /// - No circular dependencies exist
    ///
    /// Returns an error if validation fails.
    pub fn validate(&self) -> Result<()> {
        // Check that all dependencies are present
        for (pack_id, pack) in &self.packs {
            for dep_id in &pack.dependencies {
                if !self.packs.contains_key(dep_id) {
                    return Err(Error::new(&format!(
                        "Pack '{}' depends on '{}' which is not in lockfile",
                        pack_id, dep_id
                    )));
                }
            }
        }

        // Check for circular dependencies
        for pack_id in self.packs.keys() {
            self.check_circular_deps(pack_id, pack_id, &mut Vec::new())?;
        }

        Ok(())
    }

    /// Recursively check for circular dependencies
    fn check_circular_deps(
        &self, start_pack: &str, current_pack: &str, visited: &mut Vec<String>,
    ) -> Result<()> {
        if visited.contains(&current_pack.to_string()) {
            if current_pack == start_pack {
                return Err(Error::new(&format!(
                    "Circular dependency detected: {} -> {}",
                    visited.join(" -> "),
                    current_pack
                )));
            }
            return Ok(());
        }

        visited.push(current_pack.to_string());

        if let Some(pack) = self.packs.get(current_pack) {
            for dep_id in &pack.dependencies {
                self.check_circular_deps(start_pack, dep_id, visited)?;
            }
        }

        visited.pop();
        Ok(())
    }
}

impl fmt::Display for PackLockfile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Pack Lockfile (ggen v{})", self.ggen_version)?;
        writeln!(
            f,
            "Updated: {}",
            self.updated_at.format("%Y-%m-%d %H:%M:%S UTC")
        )?;
        writeln!(f, "Packs: {}", self.packs.len())?;
        writeln!(f)?;

        for (pack_id, pack) in &self.packs {
            writeln!(f, "  {} @ {}", pack_id, pack.version)?;
            writeln!(f, "    Source: {}", pack.source)?;
            if let Some(integrity) = &pack.integrity {
                writeln!(f, "    Integrity: {}", integrity)?;
            }
            if !pack.dependencies.is_empty() {
                writeln!(f, "    Dependencies: {}", pack.dependencies.join(", "))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for PackSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackSource::Registry { url } => write!(f, "Registry({})", url),
            PackSource::GitHub { org, repo, branch } => {
                write!(f, "GitHub({}/{}@{})", org, repo, branch)
            }
            PackSource::Local { path } => write!(f, "Local({})", path.display()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_pack(version: &str, deps: Vec<String>) -> LockedPack {
        LockedPack {
            version: version.to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some("sha256-test".to_string()),
            installed_at: Utc::now(),
            dependencies: deps,
        }
    }

    #[test]
    fn test_new_lockfile() {
        let lockfile = PackLockfile::new("4.0.0");
        assert_eq!(lockfile.packs.len(), 0);
        assert_eq!(lockfile.ggen_version, "4.0.0");
    }

    #[test]
    fn test_add_and_get_pack() {
        let mut lockfile = PackLockfile::new("4.0.0");
        let pack = create_test_pack("1.0.0", vec![]);

        lockfile.add_pack("test.pack", pack.clone());

        let retrieved = lockfile.get_pack("test.pack");
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().version, "1.0.0");
    }

    #[test]
    fn test_remove_pack() {
        let mut lockfile = PackLockfile::new("4.0.0");
        let pack = create_test_pack("1.0.0", vec![]);

        lockfile.add_pack("test.pack", pack);
        assert!(lockfile.get_pack("test.pack").is_some());

        let removed = lockfile.remove_pack("test.pack");
        assert!(removed);
        assert!(lockfile.get_pack("test.pack").is_none());

        // Removing again should return false
        let removed_again = lockfile.remove_pack("test.pack");
        assert!(!removed_again);
    }

    #[test]
    fn test_pack_source_display() {
        let registry = PackSource::Registry {
            url: "https://registry.ggen.io".to_string(),
        };
        assert_eq!(registry.to_string(), "Registry(https://registry.ggen.io)");

        let github = PackSource::GitHub {
            org: "seanchatmangpt".to_string(),
            repo: "ggen".to_string(),
            branch: "main".to_string(),
        };
        assert_eq!(github.to_string(), "GitHub(seanchatmangpt/ggen@main)");

        let local = PackSource::Local {
            path: PathBuf::from("/tmp/pack"),
        };
        assert_eq!(local.to_string(), "Local(/tmp/pack)");
    }
}
