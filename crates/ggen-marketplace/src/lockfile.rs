//! Lock file generation for deterministic installations
//!
//! Features:
//! - Generate ggen.lock after resolution
//! - Calculate SHA256 checksums
//! - Use sorted keys (BTreeMap) for determinism
//! - Include toolchain version, timestamps
//! - Implement read/write/update operations
//! - Ensure round-trip consistency

use crate::error::{Error, Result};
use crate::models::{PackageId, PackageVersion};
use crate::resolver::ResolvedDependency;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;
use tracing::{debug, info, warn};

/// Lock file format version
pub const LOCKFILE_VERSION: u32 = 1;

/// Lock file name
pub const LOCKFILE_NAME: &str = "ggen.lock";

/// Locked package entry
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedPackage {
    /// Package version
    pub version: PackageVersion,
    /// SHA256 checksum
    pub checksum: String,
    /// Download URL
    pub source: String,
    /// Direct dependencies
    pub dependencies: Vec<PackageId>,
    /// Optional features enabled
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub features: Vec<String>,
    /// Whether this is a root package
    #[serde(default)]
    pub root: bool,
}

/// Metadata about the lock file
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockfileMetadata {
    /// Lock file format version
    pub version: u32,
    /// When the lock file was generated
    pub generated_at: DateTime<Utc>,
    /// Toolchain version used
    pub toolchain_version: String,
    /// ggen version used
    pub ggen_version: String,
    /// Hash of the manifest file that generated this lock
    #[serde(skip_serializing_if = "Option::is_none")]
    pub manifest_hash: Option<String>,
}

impl Default for LockfileMetadata {
    fn default() -> Self {
        Self {
            version: LOCKFILE_VERSION,
            generated_at: Utc::now(),
            toolchain_version: env!("CARGO_PKG_RUST_VERSION", "1.75.0").to_string(),
            ggen_version: env!("CARGO_PKG_VERSION").to_string(),
            manifest_hash: None,
        }
    }
}

/// Complete lock file structure
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Lockfile {
    /// Metadata section
    pub metadata: LockfileMetadata,
    /// Locked packages (sorted by package ID for determinism)
    pub packages: BTreeMap<PackageId, LockedPackage>,
    /// Checksum of this lock file (computed, not serialized)
    #[serde(skip)]
    pub content_hash: Option<String>,
}

impl Lockfile {
    /// Create a new empty lock file
    pub fn new() -> Self {
        Self {
            metadata: LockfileMetadata::default(),
            packages: BTreeMap::new(),
            content_hash: None,
        }
    }

    /// Create lock file from resolved dependencies
    pub fn from_resolved(
        dependencies: &[ResolvedDependency],
        root_packages: &[PackageId],
    ) -> Self {
        let root_set: std::collections::HashSet<&PackageId> = root_packages.iter().collect();

        let packages: BTreeMap<PackageId, LockedPackage> = dependencies
            .iter()
            .map(|dep| {
                let locked = LockedPackage {
                    version: dep.version.clone(),
                    checksum: dep.checksum.clone(),
                    source: dep.download_url.clone(),
                    dependencies: dep.dependencies.clone(),
                    features: Vec::new(),
                    root: root_set.contains(&dep.id),
                };
                (dep.id.clone(), locked)
            })
            .collect();

        let mut lockfile = Self {
            metadata: LockfileMetadata::default(),
            packages,
            content_hash: None,
        };

        // Compute content hash
        lockfile.compute_hash();

        lockfile
    }

    /// Add or update a package in the lock file
    pub fn upsert(&mut self, id: PackageId, package: LockedPackage) {
        self.packages.insert(id, package);
        self.metadata.generated_at = Utc::now();
        self.compute_hash();
    }

    /// Remove a package from the lock file
    pub fn remove(&mut self, id: &PackageId) -> Option<LockedPackage> {
        let removed = self.packages.remove(id);
        if removed.is_some() {
            self.metadata.generated_at = Utc::now();
            self.compute_hash();
        }
        removed
    }

    /// Get a package from the lock file
    pub fn get(&self, id: &PackageId) -> Option<&LockedPackage> {
        self.packages.get(id)
    }

    /// Check if a package is locked
    pub fn contains(&self, id: &PackageId) -> bool {
        self.packages.contains_key(id)
    }

    /// Get number of locked packages
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Check if lock file is empty
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }

    /// Compute content hash for integrity verification
    fn compute_hash(&mut self) {
        // Serialize to JSON for deterministic hashing
        let content = serde_json::to_string(&self.packages).unwrap_or_default();
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        self.content_hash = Some(hex::encode(hasher.finalize()));
    }

    /// Verify content hash
    pub fn verify_hash(&self) -> bool {
        let content = serde_json::to_string(&self.packages).unwrap_or_default();
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        let computed = hex::encode(hasher.finalize());

        self.content_hash.as_ref().map_or(false, |h| h == &computed)
    }

    /// Get all root packages
    pub fn root_packages(&self) -> Vec<&PackageId> {
        self.packages
            .iter()
            .filter(|(_, pkg)| pkg.root)
            .map(|(id, _)| id)
            .collect()
    }

    /// Get all transitive dependencies
    pub fn transitive_dependencies(&self, id: &PackageId) -> Vec<PackageId> {
        let mut result = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut stack = vec![id.clone()];

        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.clone());

            if let Some(pkg) = self.packages.get(&current) {
                for dep in &pkg.dependencies {
                    if !visited.contains(dep) {
                        stack.push(dep.clone());
                        result.push(dep.clone());
                    }
                }
            }
        }

        result
    }
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

/// Lock file manager for file operations
pub struct LockfileManager {
    /// Base directory for lock file
    base_dir: PathBuf,
}

impl LockfileManager {
    /// Create a new lock file manager
    pub fn new(base_dir: impl Into<PathBuf>) -> Self {
        Self {
            base_dir: base_dir.into(),
        }
    }

    /// Get the lock file path
    pub fn lockfile_path(&self) -> PathBuf {
        self.base_dir.join(LOCKFILE_NAME)
    }

    /// Read lock file from disk
    pub fn read(&self) -> Result<Lockfile> {
        let path = self.lockfile_path();

        if !path.exists() {
            debug!(path = %path.display(), "Lock file does not exist");
            return Ok(Lockfile::new());
        }

        let file = fs::File::open(&path)?;
        let reader = BufReader::new(file);

        let mut lockfile: Lockfile = serde_json::from_reader(reader)
            .map_err(|e| Error::ConfigError(format!("Failed to parse lock file: {}", e)))?;

        // Recompute hash since it's skipped during serialization
        lockfile.compute_hash();

        debug!(
            path = %path.display(),
            packages = %lockfile.len(),
            "Read lock file"
        );

        Ok(lockfile)
    }

    /// Write lock file to disk
    pub fn write(&self, lockfile: &Lockfile) -> Result<()> {
        let path = self.lockfile_path();

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let file = fs::File::create(&path)?;
        let writer = BufWriter::new(file);

        serde_json::to_writer_pretty(writer, lockfile)
            .map_err(|e| Error::ConfigError(format!("Failed to write lock file: {}", e)))?;

        info!(
            path = %path.display(),
            packages = %lockfile.len(),
            "Wrote lock file"
        );

        Ok(())
    }

    /// Update lock file with new packages
    pub fn update(&self, dependencies: &[ResolvedDependency], root_packages: &[PackageId]) -> Result<Lockfile> {
        let mut lockfile = self.read()?;

        // Update metadata
        lockfile.metadata.generated_at = Utc::now();

        // Update packages
        let root_set: std::collections::HashSet<&PackageId> = root_packages.iter().collect();

        for dep in dependencies {
            let locked = LockedPackage {
                version: dep.version.clone(),
                checksum: dep.checksum.clone(),
                source: dep.download_url.clone(),
                dependencies: dep.dependencies.clone(),
                features: Vec::new(),
                root: root_set.contains(&dep.id),
            };
            lockfile.packages.insert(dep.id.clone(), locked);
        }

        // Recompute hash
        lockfile.compute_hash();

        // Write to disk
        self.write(&lockfile)?;

        Ok(lockfile)
    }

    /// Check if lock file exists
    pub fn exists(&self) -> bool {
        self.lockfile_path().exists()
    }

    /// Delete lock file
    pub fn delete(&self) -> Result<bool> {
        let path = self.lockfile_path();

        if path.exists() {
            fs::remove_file(&path)?;
            info!(path = %path.display(), "Deleted lock file");
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Verify lock file integrity
    pub fn verify(&self) -> Result<bool> {
        let lockfile = self.read()?;

        // Verify content hash
        if !lockfile.verify_hash() {
            warn!("Lock file content hash verification failed");
            return Ok(false);
        }

        // Verify all checksums are valid format (64 hex chars)
        for (id, pkg) in &lockfile.packages {
            if !pkg.checksum.is_empty() {
                if pkg.checksum.len() != 64 || !pkg.checksum.chars().all(|c| c.is_ascii_hexdigit()) {
                    warn!(package = %id, "Invalid checksum format");
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    /// Check if lock file is up to date with manifest
    pub fn is_up_to_date(&self, manifest_hash: &str) -> Result<bool> {
        let lockfile = self.read()?;

        Ok(lockfile.metadata.manifest_hash.as_ref() == Some(&manifest_hash.to_string()))
    }

    /// Set manifest hash in lock file
    pub fn set_manifest_hash(&self, hash: &str) -> Result<()> {
        let mut lockfile = self.read()?;
        lockfile.metadata.manifest_hash = Some(hash.to_string());
        lockfile.metadata.generated_at = Utc::now();
        self.write(&lockfile)
    }
}

/// Diff between two lock files
#[derive(Debug, Clone)]
pub struct LockfileDiff {
    /// Packages added
    pub added: Vec<(PackageId, PackageVersion)>,
    /// Packages removed
    pub removed: Vec<(PackageId, PackageVersion)>,
    /// Packages updated
    pub updated: Vec<(PackageId, PackageVersion, PackageVersion)>,
    /// Packages unchanged
    pub unchanged: usize,
}

impl LockfileDiff {
    /// Compute diff between two lock files
    pub fn diff(old: &Lockfile, new: &Lockfile) -> Self {
        let mut added = Vec::new();
        let mut removed = Vec::new();
        let mut updated = Vec::new();
        let mut unchanged = 0;

        // Check for added and updated
        for (id, new_pkg) in &new.packages {
            if let Some(old_pkg) = old.packages.get(id) {
                if old_pkg.version != new_pkg.version {
                    updated.push((id.clone(), old_pkg.version.clone(), new_pkg.version.clone()));
                } else {
                    unchanged += 1;
                }
            } else {
                added.push((id.clone(), new_pkg.version.clone()));
            }
        }

        // Check for removed
        for (id, old_pkg) in &old.packages {
            if !new.packages.contains_key(id) {
                removed.push((id.clone(), old_pkg.version.clone()));
            }
        }

        Self {
            added,
            removed,
            updated,
            unchanged,
        }
    }

    /// Check if there are any changes
    pub fn has_changes(&self) -> bool {
        !self.added.is_empty() || !self.removed.is_empty() || !self.updated.is_empty()
    }
}

impl std::fmt::Display for LockfileDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Lock file changes:")?;

        if !self.added.is_empty() {
            writeln!(f, "  Added ({}):", self.added.len())?;
            for (id, ver) in &self.added {
                writeln!(f, "    + {}@{}", id, ver)?;
            }
        }

        if !self.removed.is_empty() {
            writeln!(f, "  Removed ({}):", self.removed.len())?;
            for (id, ver) in &self.removed {
                writeln!(f, "    - {}@{}", id, ver)?;
            }
        }

        if !self.updated.is_empty() {
            writeln!(f, "  Updated ({}):", self.updated.len())?;
            for (id, old_ver, new_ver) in &self.updated {
                writeln!(f, "    ~ {}@{} -> {}", id, old_ver, new_ver)?;
            }
        }

        writeln!(f, "  Unchanged: {}", self.unchanged)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_resolved(id: &str, version: &str, deps: Vec<&str>) -> ResolvedDependency {
        ResolvedDependency {
            id: PackageId::new(id).unwrap(),
            version: PackageVersion::new(version).unwrap(),
            checksum: "a".repeat(64),
            download_url: format!("https://example.com/{}.tar.gz", id),
            dependencies: deps.into_iter()
                .map(|d| PackageId::new(d).unwrap())
                .collect(),
            depth: 0,
        }
    }

    #[test]
    fn test_lockfile_new() {
        let lockfile = Lockfile::new();

        assert!(lockfile.is_empty());
        assert_eq!(lockfile.metadata.version, LOCKFILE_VERSION);
    }

    #[test]
    fn test_lockfile_from_resolved() {
        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec!["pkg-b"]),
            create_test_resolved("pkg-b", "2.0.0", vec![]),
        ];
        let root = vec![PackageId::new("pkg-a").unwrap()];

        let lockfile = Lockfile::from_resolved(&deps, &root);

        assert_eq!(lockfile.len(), 2);
        assert!(lockfile.contains(&PackageId::new("pkg-a").unwrap()));
        assert!(lockfile.contains(&PackageId::new("pkg-b").unwrap()));

        let pkg_a = lockfile.get(&PackageId::new("pkg-a").unwrap()).unwrap();
        assert!(pkg_a.root);

        let pkg_b = lockfile.get(&PackageId::new("pkg-b").unwrap()).unwrap();
        assert!(!pkg_b.root);
    }

    #[test]
    fn test_lockfile_upsert() {
        let mut lockfile = Lockfile::new();

        let pkg = LockedPackage {
            version: PackageVersion::new("1.0.0").unwrap(),
            checksum: "a".repeat(64),
            source: "https://example.com/pkg.tar.gz".to_string(),
            dependencies: vec![],
            features: vec![],
            root: true,
        };

        lockfile.upsert(PackageId::new("test-pkg").unwrap(), pkg);

        assert_eq!(lockfile.len(), 1);
        assert!(lockfile.contains(&PackageId::new("test-pkg").unwrap()));
    }

    #[test]
    fn test_lockfile_remove() {
        let mut lockfile = Lockfile::new();

        let pkg = LockedPackage {
            version: PackageVersion::new("1.0.0").unwrap(),
            checksum: "a".repeat(64),
            source: "https://example.com/pkg.tar.gz".to_string(),
            dependencies: vec![],
            features: vec![],
            root: true,
        };

        let id = PackageId::new("test-pkg").unwrap();
        lockfile.upsert(id.clone(), pkg);
        assert_eq!(lockfile.len(), 1);

        let removed = lockfile.remove(&id);
        assert!(removed.is_some());
        assert!(lockfile.is_empty());
    }

    #[test]
    fn test_lockfile_hash_verification() {
        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
        ];

        let lockfile = Lockfile::from_resolved(&deps, &[]);

        assert!(lockfile.content_hash.is_some());
        assert!(lockfile.verify_hash());
    }

    #[test]
    fn test_lockfile_transitive_dependencies() {
        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec!["pkg-b"]),
            create_test_resolved("pkg-b", "1.0.0", vec!["pkg-c"]),
            create_test_resolved("pkg-c", "1.0.0", vec![]),
        ];

        let lockfile = Lockfile::from_resolved(&deps, &[]);

        let pkg_a_id = PackageId::new("pkg-a").unwrap();
        let transitive = lockfile.transitive_dependencies(&pkg_a_id);

        assert_eq!(transitive.len(), 2);
    }

    #[test]
    fn test_lockfile_manager_read_write() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Initially no lock file
        assert!(!manager.exists());

        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
        ];
        let lockfile = Lockfile::from_resolved(&deps, &[]);

        // Write lock file
        manager.write(&lockfile).unwrap();
        assert!(manager.exists());

        // Read lock file
        let read_lockfile = manager.read().unwrap();
        assert_eq!(read_lockfile.len(), 1);
        assert!(read_lockfile.contains(&PackageId::new("pkg-a").unwrap()));
    }

    #[test]
    fn test_lockfile_manager_update() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Initial lock file
        let deps1 = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
        ];
        let root = vec![PackageId::new("pkg-a").unwrap()];
        manager.update(&deps1, &root).unwrap();

        // Update with new package
        let deps2 = vec![
            create_test_resolved("pkg-a", "1.1.0", vec![]),
            create_test_resolved("pkg-b", "1.0.0", vec![]),
        ];
        let lockfile = manager.update(&deps2, &root).unwrap();

        assert_eq!(lockfile.len(), 2);

        // Verify version was updated
        let pkg_a = lockfile.get(&PackageId::new("pkg-a").unwrap()).unwrap();
        assert_eq!(pkg_a.version.as_str(), "1.1.0");
    }

    #[test]
    fn test_lockfile_manager_verify() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
        ];
        let lockfile = Lockfile::from_resolved(&deps, &[]);
        manager.write(&lockfile).unwrap();

        assert!(manager.verify().unwrap());
    }

    #[test]
    fn test_lockfile_diff() {
        let deps1 = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
            create_test_resolved("pkg-b", "1.0.0", vec![]),
        ];
        let old = Lockfile::from_resolved(&deps1, &[]);

        let deps2 = vec![
            create_test_resolved("pkg-a", "1.1.0", vec![]),  // Updated
            create_test_resolved("pkg-c", "1.0.0", vec![]),  // Added
            // pkg-b removed
        ];
        let new = Lockfile::from_resolved(&deps2, &[]);

        let diff = LockfileDiff::diff(&old, &new);

        assert_eq!(diff.added.len(), 1);
        assert_eq!(diff.removed.len(), 1);
        assert_eq!(diff.updated.len(), 1);
        assert!(diff.has_changes());
    }

    #[test]
    fn test_lockfile_diff_no_changes() {
        let deps = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
        ];
        let lockfile = Lockfile::from_resolved(&deps, &[]);

        let diff = LockfileDiff::diff(&lockfile, &lockfile);

        assert!(!diff.has_changes());
        assert_eq!(diff.unchanged, 1);
    }

    #[test]
    fn test_deterministic_serialization() {
        // Create two lock files with same content but different insertion order
        let deps = vec![
            create_test_resolved("pkg-c", "1.0.0", vec![]),
            create_test_resolved("pkg-a", "1.0.0", vec![]),
            create_test_resolved("pkg-b", "1.0.0", vec![]),
        ];
        let mut lockfile1 = Lockfile::from_resolved(&deps, &[]);

        let deps_reordered = vec![
            create_test_resolved("pkg-a", "1.0.0", vec![]),
            create_test_resolved("pkg-b", "1.0.0", vec![]),
            create_test_resolved("pkg-c", "1.0.0", vec![]),
        ];
        let mut lockfile2 = Lockfile::from_resolved(&deps_reordered, &[]);

        // Normalize metadata for fair comparison (timestamps differ)
        lockfile2.metadata = lockfile1.metadata.clone();

        // Serialize both
        let json1 = serde_json::to_string(&lockfile1).unwrap();
        let json2 = serde_json::to_string(&lockfile2).unwrap();

        // Should be identical due to BTreeMap sorting
        assert_eq!(json1, json2);
    }
}
