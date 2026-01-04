//! Lockfile generation and verification (T007)
//!
//! This module handles gpack.lock file format, providing deterministic
//! builds by locking dependency versions.
//!
//! ## Format
//!
//! Lock files are stored as YAML for human readability while
//! maintaining exact reproducibility.
//!
//! ## Structure
//!
//! - `version`: Lock file schema version
//! - `packages`: Map of package name to locked metadata
//! - `checksums`: Map of package@version to checksum
//! - `metadata.generated_at`: Timestamp of generation
//! - `metadata.rust_toolchain`: Rust version used

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

use super::error::{GpackError, GpackResult};

/// Current lockfile format version
pub const LOCKFILE_VERSION: u32 = 1;

/// Lock file format version string
pub const LOCKFILE_FORMAT_VERSION: &str = "1.0.0";

/// The lockfile structure (gpack.lock)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    /// Lockfile format version
    pub version: u32,
    /// Locked package entries (sorted by name for determinism)
    pub packages: BTreeMap<String, LockfileEntry>,
    /// Metadata about the lock operation
    #[serde(default)]
    pub metadata: LockfileMetadata,
}

/// A single locked package entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockfileEntry {
    /// Package name
    pub name: String,
    /// Locked version
    pub version: String,
    /// SHA-256 checksum
    pub checksum: String,
    /// Source URL
    pub source: String,
    /// Dependencies (name -> version)
    #[serde(default)]
    pub dependencies: BTreeMap<String, String>,
    /// Features enabled
    #[serde(default)]
    pub features: Vec<String>,
}

/// Lockfile metadata
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LockfileMetadata {
    /// ggen version that created this lockfile
    pub ggen_version: Option<String>,
    /// Timestamp when lockfile was created
    pub created_at: Option<String>,
    /// Hash of the manifest that generated this lockfile
    pub manifest_hash: Option<String>,
    /// Rust toolchain version used
    pub rust_toolchain: Option<String>,
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

impl Lockfile {
    /// Create a new empty lockfile
    pub fn new() -> Self {
        Self {
            version: LOCKFILE_VERSION,
            packages: BTreeMap::new(),
            metadata: LockfileMetadata::default(),
        }
    }

    /// Create a lockfile with a specific timestamp
    pub fn with_timestamp(timestamp: impl Into<String>) -> Self {
        Self {
            version: LOCKFILE_VERSION,
            packages: BTreeMap::new(),
            metadata: LockfileMetadata {
                created_at: Some(timestamp.into()),
                ..Default::default()
            },
        }
    }

    /// Parse lockfile from TOML string
    pub fn from_toml(content: &str) -> GpackResult<Self> {
        toml::from_str(content).map_err(|e| GpackError::LockfileError(e.to_string()))
    }

    /// Serialize lockfile to TOML string
    pub fn to_toml(&self) -> GpackResult<String> {
        toml::to_string_pretty(self).map_err(|e| GpackError::LockfileError(e.to_string()))
    }

    /// Parse lockfile from YAML string (T007: human-readable format)
    pub fn from_yaml(content: &str) -> GpackResult<Self> {
        serde_yaml::from_str(content).map_err(|e| GpackError::LockfileError(e.to_string()))
    }

    /// Serialize lockfile to YAML string (T007: human-readable format)
    pub fn to_yaml(&self) -> GpackResult<String> {
        serde_yaml::to_string(self).map_err(|e| GpackError::LockfileError(e.to_string()))
    }

    /// Parse lockfile from JSON string
    pub fn from_json(content: &str) -> GpackResult<Self> {
        serde_json::from_str(content).map_err(|e| GpackError::SerializationError(e.to_string()))
    }

    /// Serialize lockfile to JSON string
    pub fn to_json(&self) -> GpackResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| GpackError::SerializationError(e.to_string()))
    }

    /// Read lockfile from path
    pub fn read_from_path(path: impl AsRef<Path>) -> GpackResult<Self> {
        let content = std::fs::read_to_string(path.as_ref())?;
        Self::from_toml(&content)
    }

    /// Read lockfile from YAML file
    pub fn read_yaml_from_path(path: impl AsRef<Path>) -> GpackResult<Self> {
        let content = std::fs::read_to_string(path.as_ref())?;
        Self::from_yaml(&content)
    }

    /// Write lockfile to path (TOML format)
    pub fn write_to_path(&self, path: impl AsRef<Path>) -> GpackResult<()> {
        let content = self.to_toml()?;
        std::fs::write(path.as_ref(), content)?;
        Ok(())
    }

    /// Write lockfile to path (YAML format)
    pub fn write_yaml_to_path(&self, path: impl AsRef<Path>) -> GpackResult<()> {
        let content = self.to_yaml()?;
        std::fs::write(path.as_ref(), content)?;
        Ok(())
    }

    /// Add or update a package entry
    pub fn upsert(&mut self, entry: LockfileEntry) {
        self.packages.insert(entry.name.clone(), entry);
    }

    /// Get a package entry
    pub fn get(&self, name: &str) -> Option<&LockfileEntry> {
        self.packages.get(name)
    }

    /// Remove a package entry
    pub fn remove(&mut self, name: &str) -> Option<LockfileEntry> {
        self.packages.remove(name)
    }

    /// Check if package is locked
    pub fn is_locked(&self, name: &str) -> bool {
        self.packages.contains_key(name)
    }

    /// Get all package names
    pub fn package_names(&self) -> impl Iterator<Item = &str> {
        self.packages.keys().map(String::as_str)
    }

    /// Verify checksum for a package
    pub fn verify_checksum(&self, name: &str, checksum: &str) -> GpackResult<bool> {
        match self.get(name) {
            Some(entry) => Ok(entry.checksum == checksum),
            None => Err(GpackError::LockfileError(format!(
                "Package {} not found in lockfile",
                name
            ))),
        }
    }

    /// Calculate lockfile hash for change detection
    pub fn calculate_hash(&self) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();

        // Hash all entries in sorted order (BTreeMap maintains order)
        for (name, entry) in &self.packages {
            hasher.update(name.as_bytes());
            hasher.update(entry.version.as_bytes());
            hasher.update(entry.checksum.as_bytes());
        }

        hex::encode(hasher.finalize())
    }

    /// Check if lockfile is up to date with manifest
    pub fn is_up_to_date(&self, manifest_hash: &str) -> bool {
        self.metadata
            .manifest_hash
            .as_ref()
            .map(|h| h == manifest_hash)
            .unwrap_or(false)
    }

    /// Get number of locked packages
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Check if lockfile is empty
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }
}

impl LockfileEntry {
    /// Create a new lockfile entry
    pub fn new(name: &str, version: &str, checksum: &str, source: &str) -> Self {
        Self {
            name: name.to_string(),
            version: version.to_string(),
            checksum: checksum.to_string(),
            source: source.to_string(),
            dependencies: BTreeMap::new(),
            features: Vec::new(),
        }
    }

    /// Add a dependency
    pub fn add_dependency(&mut self, name: &str, version: &str) {
        self.dependencies
            .insert(name.to_string(), version.to_string());
    }

    /// Add a feature
    pub fn add_feature(&mut self, feature: &str) {
        self.features.push(feature.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lockfile_creation() {
        let lockfile = Lockfile::new();
        assert_eq!(lockfile.version, LOCKFILE_VERSION);
        assert!(lockfile.is_empty());
    }

    #[test]
    fn test_upsert_and_get() {
        let mut lockfile = Lockfile::new();
        let entry = LockfileEntry::new("test-pkg", "1.0.0", "abc123", "https://example.com");

        lockfile.upsert(entry);

        let retrieved = lockfile.get("test-pkg").unwrap();
        assert_eq!(retrieved.version, "1.0.0");
        assert_eq!(retrieved.checksum, "abc123");
    }

    #[test]
    fn test_toml_serialization() {
        let mut lockfile = Lockfile::new();
        lockfile.upsert(LockfileEntry::new(
            "pkg1",
            "1.0.0",
            "abc",
            "https://example.com",
        ));

        let toml = lockfile.to_toml().unwrap();
        let parsed = Lockfile::from_toml(&toml).unwrap();

        assert_eq!(parsed.len(), 1);
        assert!(parsed.is_locked("pkg1"));
    }

    #[test]
    fn test_yaml_roundtrip() {
        let mut lockfile = Lockfile::with_timestamp("2024-01-15T10:00:00Z");
        lockfile.upsert(LockfileEntry::new(
            "test-gpack",
            "1.0.0",
            "sha256:abc123",
            "https://example.com",
        ));
        lockfile.metadata.ggen_version = Some("5.0.0".to_string());

        let yaml = lockfile.to_yaml().unwrap();
        let parsed = Lockfile::from_yaml(&yaml).unwrap();

        assert_eq!(lockfile.version, parsed.version);
        assert_eq!(lockfile.len(), parsed.len());
        assert!(parsed.is_locked("test-gpack"));

        let entry = parsed.get("test-gpack").unwrap();
        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.checksum, "sha256:abc123");
    }

    #[test]
    fn test_json_roundtrip() {
        let mut lockfile = Lockfile::new();
        let mut entry = LockfileEntry::new(
            "json-test-gpack",
            "2.0.0",
            "sha256:def456",
            "https://example.com",
        );
        entry.add_dependency("dep1", "1.0.0");
        entry.add_feature("serde");
        lockfile.upsert(entry);

        let json = lockfile.to_json().unwrap();
        let parsed = Lockfile::from_json(&json).unwrap();

        assert_eq!(lockfile.len(), parsed.len());
        let entry = parsed.get("json-test-gpack").unwrap();
        assert_eq!(entry.dependencies.get("dep1"), Some(&"1.0.0".to_string()));
        assert!(entry.features.contains(&"serde".to_string()));
    }

    #[test]
    fn test_verify_checksum() {
        let mut lockfile = Lockfile::new();
        lockfile.upsert(LockfileEntry::new("pkg", "1.0.0", "abc123", "https://example.com"));

        assert!(lockfile.verify_checksum("pkg", "abc123").unwrap());
        assert!(!lockfile.verify_checksum("pkg", "wrong").unwrap());
    }

    #[test]
    fn test_calculate_hash() {
        let mut lockfile1 = Lockfile::new();
        lockfile1.upsert(LockfileEntry::new("pkg", "1.0.0", "abc", "url"));

        let mut lockfile2 = Lockfile::new();
        lockfile2.upsert(LockfileEntry::new("pkg", "1.0.0", "abc", "url"));

        // Same content should produce same hash
        assert_eq!(lockfile1.calculate_hash(), lockfile2.calculate_hash());
    }

    #[test]
    fn test_entry_dependencies() {
        let mut entry = LockfileEntry::new("pkg", "1.0.0", "abc", "url");
        entry.add_dependency("dep1", "0.1.0");
        entry.add_dependency("dep2", "0.2.0");

        assert_eq!(entry.dependencies.len(), 2);
        assert_eq!(entry.dependencies.get("dep1"), Some(&"0.1.0".to_string()));
    }

    #[test]
    fn test_lockfile_with_timestamp() {
        let lockfile = Lockfile::with_timestamp("2024-01-15T10:00:00Z");
        assert_eq!(
            lockfile.metadata.created_at,
            Some("2024-01-15T10:00:00Z".to_string())
        );
    }

    #[test]
    fn test_metadata_fields() {
        let mut lockfile = Lockfile::new();
        lockfile.metadata.ggen_version = Some("5.0.0".to_string());
        lockfile.metadata.manifest_hash = Some("sha256:abc123".to_string());

        let yaml = lockfile.to_yaml().unwrap();
        let parsed = Lockfile::from_yaml(&yaml).unwrap();

        assert_eq!(parsed.metadata.ggen_version, Some("5.0.0".to_string()));
        assert_eq!(parsed.metadata.manifest_hash, Some("sha256:abc123".to_string()));
    }
}
