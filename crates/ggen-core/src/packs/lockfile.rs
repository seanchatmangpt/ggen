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
//! use crate::packs::lockfile::{PackLockfile, LockedPack, PackSource};
//! use std::collections::BTreeMap;
//! use chrono::Utc;
//!
//! let mut lockfile = PackLockfile {
//!     packs: BTreeMap::new(),
//!     updated_at: Utc::now(),
//!     ggen_version: "4.0.0".to_string(),
//!     profile: None,
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
//! use crate::packs::lockfile::PackLockfile;
//! use std::path::Path;
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let lockfile = PackLockfile::from_file(Path::new(".ggen/packs.lock"))?;
//! println!("Loaded {} packs", lockfile.packs.len());
//! # Ok(())
//! # }
//! ```

use crate::utils::error::{Error, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

/// Pack lockfile containing all installed packs
///
/// This structure represents the `.ggen/packs.lock` file, which tracks
/// all installed packs, their versions, sources, and dependencies.
/// PartialEq without Eq: updated_at (`DateTime<Utc>`) field does not implement Eq
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackLockfile {
    /// Map of pack IDs to their locked versions
    /// Uses BTreeMap for deterministic ordering
    pub packs: BTreeMap<String, LockedPack>,

    /// When the lockfile was last updated
    pub updated_at: DateTime<Utc>,

    /// Version of ggen that created this lockfile
    pub ggen_version: String,

    /// Policy profile for pack resolution (optional)
    /// If not specified, defaults to "development" profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,
}

/// A locked pack with its installation metadata
///
/// Contains all information needed to reproduce a pack installation,
/// including source, version, integrity checksum, and dependencies.
/// PartialEq without Eq: installed_at (`DateTime<Utc>`) field does not implement Eq
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
/// PartialEq without Eq: All fields (String) implement Eq
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum PackSource {
    /// Pack installed from a registry
    Registry {
        /// Registry URL (e.g., `"https://registry.ggen.io"`)
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
    /// use crate::packs::lockfile::PackLockfile;
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
            profile: None,
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
    /// use crate::packs::lockfile::PackLockfile;
    /// use std::path::Path;
    ///
    /// # fn main() -> crate::utils::error::Result<()> {
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
    /// use crate::packs::lockfile::PackLockfile;
    /// use std::path::Path;
    ///
    /// # fn main() -> crate::utils::error::Result<()> {
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
    /// use crate::packs::lockfile::{PackLockfile, LockedPack, PackSource};
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

    /// Validate per-entry lockfile invariants (coding-agent-mistakes.md §4.1).
    ///
    /// Enforces, for every entry in `packs`, the contract that prevents
    /// "contract drift" — a lockfile entry that does not faithfully describe
    /// what was actually installed:
    ///
    /// - `version` must be non-empty (trimmed) — an entry without a version is
    ///   not reproducible.
    /// - `installed_at` must be a real timestamp, never the Unix epoch / default
    ///   sentinel (`timestamp() <= 0`) — a zero timestamp signals the field was
    ///   never populated.
    /// - `integrity` must be present and, when present, match the shape
    ///   `sha256-<64 hex chars>` — an empty or malformed digest cannot be
    ///   re-verified at `sync --locked` time.
    ///
    /// Returns a descriptive `Err` on the first violation found.
    ///
    /// This check is intentionally NOT wired into `save()`/`from_file()`: those
    /// paths already round-trip entries with looser integrity values (e.g.
    /// `integrity: None` or a non-canonical digest) that existing callers and
    /// tests depend on. Callers that require the strict §4.1 contract — such as
    /// `sync --locked` proof gates — should invoke this method explicitly.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use crate::packs::lockfile::{PackLockfile, LockedPack, PackSource};
    /// use chrono::Utc;
    ///
    /// let mut lockfile = PackLockfile::new("4.0.0");
    /// lockfile.add_pack(
    ///     "io.ggen.rust.cli",
    ///     LockedPack {
    ///         version: "1.0.0".to_string(),
    ///         source: PackSource::Registry {
    ///             url: "https://registry.ggen.io".to_string(),
    ///         },
    ///         integrity: Some(format!("sha256-{}", "a".repeat(64))),
    ///         installed_at: Utc::now(),
    ///         dependencies: vec![],
    ///     },
    /// );
    /// assert!(lockfile.validate_invariants().is_ok());
    /// ```
    pub fn validate_invariants(&self) -> Result<()> {
        for (pack_id, pack) in &self.packs {
            // version: non-empty (after trimming surrounding whitespace).
            if pack.version.trim().is_empty() {
                return Err(Error::new(&format!(
                    "Lockfile invariant violation: pack '{}' has an empty version",
                    pack_id
                )));
            }

            // installed_at: must be a real timestamp, not the Unix epoch / default.
            // `DateTime::<Utc>::default()` is 1970-01-01T00:00:00Z (timestamp 0);
            // negative timestamps are pre-epoch and equally invalid.
            if pack.installed_at.timestamp() <= 0 {
                return Err(Error::new(&format!(
                    "Lockfile invariant violation: pack '{}' has a non-real installed_at \
                     timestamp ({}); expected a populated timestamp, not the Unix epoch / default",
                    pack_id, pack.installed_at
                )));
            }

            // integrity: must be present and match the shape `sha256-<64 hex chars>`.
            match &pack.integrity {
                None => {
                    return Err(Error::new(&format!(
                        "Lockfile invariant violation: pack '{}' is missing an integrity digest",
                        pack_id
                    )));
                }
                Some(integrity) => {
                    if !is_valid_sha256_integrity(integrity) {
                        return Err(Error::new(&format!(
                            "Lockfile invariant violation: pack '{}' has a malformed integrity \
                             digest '{}'; expected the shape 'sha256-<64 hex chars>'",
                            pack_id, integrity
                        )));
                    }
                }
            }
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

/// Returns `true` if `integrity` matches the canonical shape `sha256-<64 hex chars>`.
///
/// The check is performed without a regex dependency: the `sha256-` prefix is
/// stripped, then the remaining 64 characters must all be ASCII hex digits.
/// Both lowercase and uppercase hex digits are accepted.
fn is_valid_sha256_integrity(integrity: &str) -> bool {
    const PREFIX: &str = "sha256-";
    match integrity.strip_prefix(PREFIX) {
        Some(hash) => hash.len() == 64 && hash.bytes().all(|b| b.is_ascii_hexdigit()),
        None => false,
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

    // --- §4.1 lockfile invariant tests (Chicago TDD: real fs round-trip) -----

    /// A canonical 64-hex-char sha256 integrity string (`sha256-<64 hex>`).
    fn valid_integrity() -> String {
        format!("sha256-{}", "a".repeat(64))
    }

    /// Build a §4.1-valid pack: non-empty version, real timestamp, canonical
    /// integrity digest.
    fn valid_invariant_pack() -> LockedPack {
        LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some(valid_integrity()),
            installed_at: Utc::now(),
            dependencies: vec![],
        }
    }

    /// (a) A lockfile whose entries satisfy §4.1 passes `validate_invariants`,
    /// and the property survives a real save → load round-trip on disk.
    #[test]
    fn test_validate_invariants_accepts_valid_lockfile() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("create temp dir");
        let lock_path = temp_dir.path().join("packs.lock");

        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("io.ggen.rust.cli", valid_invariant_pack());

        // In-memory check.
        assert!(
            lockfile.validate_invariants().is_ok(),
            "valid lockfile should pass §4.1 invariants"
        );

        // Real filesystem round-trip, then re-check the loaded value.
        lockfile.save(&lock_path).expect("save lockfile");
        assert!(lock_path.exists(), "lockfile should exist on disk");

        let loaded = PackLockfile::from_file(&lock_path).expect("load lockfile");
        assert!(
            loaded.validate_invariants().is_ok(),
            "lockfile loaded from disk should still pass §4.1 invariants"
        );
    }

    /// (b) An entry with an empty integrity digest fails §4.1, observable after
    /// the value is persisted and reloaded from disk.
    #[test]
    fn test_validate_invariants_rejects_empty_digest() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("create temp dir");
        let lock_path = temp_dir.path().join("packs.lock");

        let mut pack = valid_invariant_pack();
        pack.integrity = Some(String::new());

        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("io.ggen.rust.cli", pack);

        // `save`/`from_file` deliberately do not enforce §4.1, so persistence
        // succeeds and the malformed value is what we re-load.
        lockfile.save(&lock_path).expect("save lockfile");
        let loaded = PackLockfile::from_file(&lock_path).expect("load lockfile");

        let result = loaded.validate_invariants();
        assert!(result.is_err(), "empty integrity digest must fail §4.1");
        let msg = result.expect_err("expected an error").to_string();
        assert!(
            msg.contains("malformed integrity"),
            "error should name the malformed integrity digest, got: {msg}"
        );
    }

    /// (c) An entry with a malformed integrity digest (wrong length / missing
    /// prefix) fails §4.1.
    #[test]
    fn test_validate_invariants_rejects_malformed_digest() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("create temp dir");

        // Wrong length (too short).
        {
            let lock_path = temp_dir.path().join("short.lock");
            let mut pack = valid_invariant_pack();
            pack.integrity = Some("sha256-abc123".to_string());
            let mut lockfile = PackLockfile::new("4.0.0");
            lockfile.add_pack("io.ggen.rust.cli", pack);
            lockfile.save(&lock_path).expect("save lockfile");
            let loaded = PackLockfile::from_file(&lock_path).expect("load lockfile");
            assert!(
                loaded.validate_invariants().is_err(),
                "too-short integrity digest must fail §4.1"
            );
        }

        // Missing `sha256-` prefix (64 hex chars but no algorithm prefix).
        {
            let lock_path = temp_dir.path().join("noprefix.lock");
            let mut pack = valid_invariant_pack();
            pack.integrity = Some("a".repeat(64));
            let mut lockfile = PackLockfile::new("4.0.0");
            lockfile.add_pack("io.ggen.rust.cli", pack);
            lockfile.save(&lock_path).expect("save lockfile");
            let loaded = PackLockfile::from_file(&lock_path).expect("load lockfile");
            assert!(
                loaded.validate_invariants().is_err(),
                "integrity digest without sha256- prefix must fail §4.1"
            );
        }

        // Non-hex characters in the 64-char body.
        {
            let mut pack = valid_invariant_pack();
            pack.integrity = Some(format!("sha256-{}", "z".repeat(64)));
            let mut lockfile = PackLockfile::new("4.0.0");
            lockfile.add_pack("io.ggen.rust.cli", pack);
            assert!(
                lockfile.validate_invariants().is_err(),
                "integrity digest with non-hex body must fail §4.1"
            );
        }
    }

    /// (d) An entry with an empty version fails §4.1, observable after a real
    /// save → load round-trip.
    #[test]
    fn test_validate_invariants_rejects_empty_version() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("create temp dir");
        let lock_path = temp_dir.path().join("packs.lock");

        let mut pack = valid_invariant_pack();
        pack.version = String::new();

        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("io.ggen.rust.cli", pack);

        lockfile.save(&lock_path).expect("save lockfile");
        let loaded = PackLockfile::from_file(&lock_path).expect("load lockfile");

        let result = loaded.validate_invariants();
        assert!(result.is_err(), "empty version must fail §4.1");
        let msg = result.expect_err("expected an error").to_string();
        assert!(
            msg.contains("empty version"),
            "error should name the empty version, got: {msg}"
        );
    }

    /// `installed_at` at the Unix epoch / default sentinel fails §4.1.
    #[test]
    fn test_validate_invariants_rejects_epoch_installed_at() {
        let mut pack = valid_invariant_pack();
        pack.installed_at = DateTime::<Utc>::default();

        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("io.ggen.rust.cli", pack);

        let result = lockfile.validate_invariants();
        assert!(
            result.is_err(),
            "epoch/default installed_at must fail §4.1"
        );
        let msg = result.expect_err("expected an error").to_string();
        assert!(
            msg.contains("installed_at"),
            "error should name installed_at, got: {msg}"
        );
    }
}
