use std::path::Path;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Persisted record of the last successfully applied manifest for a target repo.
///
/// Stored at `<local_repo>/.ggen/last-applied.json`.  When the SHA-256 of the
/// current manifest file matches `manifest_hash`, the executor can be skipped
/// entirely — the target repo already reflects this manifest version.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestCache {
    pub manifest_hash: String,
    pub commit_sha: String,
    pub applied_at: String,
}

impl ManifestCache {
    fn cache_path(local: &Path) -> std::path::PathBuf {
        local.join(".ggen").join("last-applied.json")
    }

    /// Read the cache from `<local>/.ggen/last-applied.json`.
    /// Returns `None` if the file is absent or cannot be parsed.
    pub fn read(local: &Path) -> Option<Self> {
        let data = std::fs::read_to_string(Self::cache_path(local)).ok()?;
        serde_json::from_str(&data).ok()
    }

    /// Write the cache to `<local>/.ggen/last-applied.json`, creating the
    /// `.ggen/` directory if it does not already exist.
    pub fn write(&self, local: &Path) -> std::io::Result<()> {
        let path = Self::cache_path(local);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        std::fs::write(&path, json)
    }
}

/// Compute the SHA-256 hex digest of a file's contents.
/// Returns `None` if the file cannot be read (e.g., missing manifest).
pub fn hash_file(path: &Path) -> Option<String> {
    let bytes = std::fs::read(path).ok()?;
    let digest = Sha256::digest(&bytes);
    Some(format!("{:x}", digest))
}
