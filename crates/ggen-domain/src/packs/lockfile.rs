//! Domain lockfile operations — single entry point for all lockfile reads/writes.
//!
//! Every operation that modifies `.ggen/packs.lock` goes through this module.
//! CLI verbs never touch the file directly.

use chrono::Utc;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use ggen_core::utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

/// Digest of pack content (SHA-256 hex, 64 chars).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackDigest(pub String);

impl PackDigest {
    /// Create from raw bytes (SHA-256 output).
    pub fn from_bytes(bytes: &[u8; 32]) -> Self {
        Self(hex::encode(bytes))
    }

    /// Hex string representation.
    pub fn as_hex(&self) -> &str {
        &self.0
    }

    /// Prefixed form: `sha256:<hex>`.
    pub fn prefixed(&self) -> String {
        format!("sha256:{}", self.0)
    }
}

/// A validated, domain-level view of a single lockfile entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledPackEntry {
    pub id: String,
    pub version: String,
    pub installed_at: chrono::DateTime<chrono::Utc>,
    pub digest: Option<PackDigest>,
    pub registry_source: String,
    pub trust_tier: String,
    pub dependencies: Vec<String>,
    pub install_path: String,
    pub files: Vec<String>,
}

impl InstalledPackEntry {
    /// Convert to the core `LockedPack` for serialization.
    pub fn to_locked_pack(&self) -> LockedPack {
        LockedPack {
            version: self.version.clone(),
            source: PackSource::Local {
                path: std::path::PathBuf::from(&self.install_path),
            },
            integrity: self.digest.as_ref().map(|d| d.prefixed()),
            installed_at: self.installed_at,
            dependencies: self.dependencies.clone(),
        }
    }

    /// Build from a core `LockedPack`.
    pub fn from_locked_pack(id: String, pack: &LockedPack) -> Self {
        let (registry_source, install_path) = match &pack.source {
            PackSource::Registry { url } => (url.clone(), String::new()),
            PackSource::GitHub { org, repo, branch } => {
                (format!("github:{}/{}@{}", org, repo, branch), String::new())
            }
            PackSource::Local { path } => ("local".to_string(), path.display().to_string()),
        };

        Self {
            id,
            version: pack.version.clone(),
            installed_at: pack.installed_at,
            digest: pack.integrity.as_ref().and_then(|i| {
                i.strip_prefix("sha256:").map(|h| PackDigest(h.to_string()))
            }),
            registry_source,
            trust_tier: "local".to_string(),
            dependencies: pack.dependencies.clone(),
            install_path,
            files: vec![],
        }
    }
}

// ---------------------------------------------------------------------------
// Core operations
// ---------------------------------------------------------------------------

/// Add (or replace) a pack entry in the lockfile. Atomic write via temp+rename.
///
/// This is the **single entry point** for both `pack add` and `capability enable`.
pub fn add_pack(
    lockfile_path: &Path,
    entry: &InstalledPackEntry,
) -> Result<()> {
    let mut lockfile = load_or_create(lockfile_path)?;
    lockfile.add_pack(&entry.id, entry.to_locked_pack());
    save_atomic(&lockfile, lockfile_path)
}

/// Remove a pack entry from the lockfile.
pub fn remove_pack(lockfile_path: &Path, pack_id: &str) -> Result<bool> {
    if !lockfile_path.exists() {
        return Ok(false);
    }
    let mut lockfile = PackLockfile::from_file(lockfile_path)?;
    let removed = lockfile.remove_pack(pack_id);
    if removed {
        save_atomic(&lockfile, lockfile_path)?;
    }
    Ok(removed)
}

/// List all installed packs from the lockfile.
pub fn list_packs(lockfile_path: &Path) -> Result<Vec<InstalledPackEntry>> {
    if !lockfile_path.exists() {
        return Ok(vec![]);
    }
    let lockfile = PackLockfile::from_file(lockfile_path)?;
    Ok(lockfile
        .packs
        .iter()
        .map(|(id, pack)| InstalledPackEntry::from_locked_pack(id.clone(), pack))
        .collect())
}

/// Get a single pack entry by ID.
pub fn get_pack(lockfile_path: &Path, pack_id: &str) -> Result<Option<InstalledPackEntry>> {
    if !lockfile_path.exists() {
        return Ok(None);
    }
    let lockfile = PackLockfile::from_file(lockfile_path)?;
    Ok(lockfile
        .get_pack(pack_id)
        .map(|p| InstalledPackEntry::from_locked_pack(pack_id.to_string(), p)))
}

/// Verify the integrity of a single installed pack by comparing the stored digest
/// against the actual content of the pack TOML at install time.
///
/// Returns `Ok(true)` if the pack passes integrity check or has no stored digest.
/// Returns `Ok(false)` if the digest mismatched.
pub fn verify_pack_integrity(
    lockfile_path: &Path,
    pack_id: &str,
    packs_dir: &Path,
) -> Result<bool> {
    let entry = get_pack(lockfile_path, pack_id)?;
    let entry = match entry {
        Some(e) => e,
        None => return Err(Error::new(&format!("Pack '{}' not in lockfile", pack_id))),
    };

    let digest = match &entry.digest {
        Some(d) => d,
        None => return Ok(true), // no digest stored — pass by default
    };

    let pack_toml_path = packs_dir.join(format!("{}.toml", pack_id));
    if !pack_toml_path.exists() {
        return Ok(false);
    }

    let content = std::fs::read_to_string(&pack_toml_path)?;
    let actual = compute_digest(&content);

    Ok(digest.0 == actual.0)
}

/// Compute SHA-256 digest of arbitrary content.
pub fn compute_digest(content: &str) -> PackDigest {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    let bytes: [u8; 32] = hasher.finalize().into();
    PackDigest::from_bytes(&bytes)
}

/// Verify all packs in the lockfile against their stored digests.
pub fn verify_all_packs(lockfile_path: &Path, packs_dir: &Path) -> Result<LockfileIntegrityReport> {
    let packs = list_packs(lockfile_path)?;
    let mut results = BTreeMap::new();
    let mut all_valid = true;

    for entry in &packs {
        let valid = match &entry.digest {
            Some(digest) => {
                let pack_toml_path = packs_dir.join(format!("{}.toml", entry.id));
                if !pack_toml_path.exists() {
                    all_valid = false;
                    false
                } else {
                    let content = match std::fs::read_to_string(&pack_toml_path) {
                        Ok(c) => c,
                        Err(_) => {
                            all_valid = false;
                            results.insert(entry.id.clone(), false);
                            continue;
                        }
                    };
                    let actual = compute_digest(&content);
                    let matches = digest.0 == actual.0;
                    if !matches {
                        all_valid = false;
                    }
                    matches
                }
            }
            None => true,
        };
        results.insert(entry.id.clone(), valid);
    }

    Ok(LockfileIntegrityReport {
        valid: all_valid,
        pack_results: results,
        total: packs.len(),
        passed: results.values().filter(|&&v| v).count(),
    })
}

// ---------------------------------------------------------------------------
// Output types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockfileIntegrityReport {
    pub valid: bool,
    pub total: usize,
    pub passed: usize,
    pub pack_results: BTreeMap<String, bool>,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

fn load_or_create(path: &Path) -> Result<PackLockfile> {
    if path.exists() {
        PackLockfile::from_file(path)
    } else {
        Ok(PackLockfile::new(env!("CARGO_PKG_VERSION")))
    }
}

/// Atomic write: write to temp file, then rename.
fn save_atomic(lockfile: &PackLockfile, path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let json = serde_json::to_string_pretty(lockfile)
        .map_err(|e| Error::new(&format!("Failed to serialize lockfile: {}", e)))?;

    let tmp_path = path.with_extension("lock.tmp");
    std::fs::write(&tmp_path, &json)
        .map_err(|e| Error::new(&format!("Failed to write temp lockfile: {}", e)))?;

    std::fs::rename(&tmp_path, path)
        .map_err(|e| Error::new(&format!("Failed to rename temp lockfile: {}", e)))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn make_entry(id: &str) -> InstalledPackEntry {
        InstalledPackEntry {
            id: id.to_string(),
            version: "1.0.0".to_string(),
            installed_at: Utc::now(),
            digest: Some(PackDigest("abc123".repeat(10).trim().to_string())),
            registry_source: "local".to_string(),
            trust_tier: "local".to_string(),
            dependencies: vec![],
            install_path: format!(".ggen/packs/{}", id),
            files: vec![],
        }
    }

    #[test]
    fn test_add_and_list_packs() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");

        add_pack(&lock_path, &make_entry("mcp-rust")).unwrap();
        let packs = list_packs(&lock_path).unwrap();

        assert_eq!(packs.len(), 1);
        assert_eq!(packs[0].id, "mcp-rust");
    }

    #[test]
    fn test_remove_pack() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");

        add_pack(&lock_path, &make_entry("mcp-rust")).unwrap();
        assert_eq!(list_packs(&lock_path).unwrap().len(), 1);

        let removed = remove_pack(&lock_path, "mcp-rust").unwrap();
        assert!(removed);
        assert_eq!(list_packs(&lock_path).unwrap().len(), 0);
    }

    #[test]
    fn test_get_pack() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");

        add_pack(&lock_path, &make_entry("mcp-rust")).unwrap();
        let entry = get_pack(&lock_path, "mcp-rust").unwrap();
        assert!(entry.is_some());
        assert_eq!(entry.unwrap().version, "1.0.0");

        let missing = get_pack(&lock_path, "no-such").unwrap();
        assert!(missing.is_none());
    }

    #[test]
    fn test_add_replaces_existing() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");

        add_pack(&lock_path, &make_entry("mcp-rust")).unwrap();

        let mut entry = make_entry("mcp-rust");
        entry.version = "2.0.0".to_string();
        add_pack(&lock_path, &entry).unwrap();

        let packs = list_packs(&lock_path).unwrap();
        assert_eq!(packs.len(), 1);
        assert_eq!(packs[0].version, "2.0.0");
    }

    #[test]
    fn test_compute_digest_deterministic() {
        let d1 = compute_digest("hello");
        let d2 = compute_digest("hello");
        let d3 = compute_digest("world");
        assert_eq!(d1.0, d2.0);
        assert_ne!(d1.0, d3.0);
        assert_eq!(d1.0.len(), 64); // SHA-256 hex
    }

    #[test]
    fn test_list_packs_empty_when_no_lockfile() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");
        let packs = list_packs(&lock_path).unwrap();
        assert!(packs.is_empty());
    }
}
