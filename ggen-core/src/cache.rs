use anyhow::{Context, Result};
use git2::{FetchOptions, RemoteCallbacks, Repository};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

use crate::registry::ResolvedPack;

/// Local cache manager for gpacks
#[derive(Debug, Clone)]
pub struct CacheManager {
    cache_dir: PathBuf,
}

/// Cached gpack information
#[derive(Debug, Clone)]
pub struct CachedPack {
    pub id: String,
    pub version: String,
    pub path: PathBuf,
    pub sha256: String,
    pub manifest: Option<crate::gpack::GpackManifest>,
}

impl CacheManager {
    /// Create a new cache manager
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .context("Failed to find cache directory")?
            .join("ggen")
            .join("gpacks");

        fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

        Ok(Self { cache_dir })
    }

    /// Create a cache manager with custom directory (for testing)
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

        Ok(Self { cache_dir })
    }

    /// Get the cache directory path
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Ensure a pack is cached locally
    pub async fn ensure(&self, resolved_pack: &ResolvedPack) -> Result<CachedPack> {
        let pack_dir = self
            .cache_dir
            .join(&resolved_pack.id)
            .join(&resolved_pack.version);

        // Check if already cached and valid
        if pack_dir.exists() {
            if let Ok(cached) = self.load_cached(&resolved_pack.id, &resolved_pack.version) {
                // Verify SHA256 if provided
                if !resolved_pack.sha256.is_empty() {
                    let actual_sha256 = self.calculate_sha256(&pack_dir)?;
                    if actual_sha256 == resolved_pack.sha256 {
                        return Ok(cached);
                    } else {
                        // SHA256 mismatch, remove and re-download
                        fs::remove_dir_all(&pack_dir)
                            .context("Failed to remove corrupted cache")?;
                    }
                } else {
                    return Ok(cached);
                }
            }
        }

        // Download the pack
        self.download_pack(resolved_pack, &pack_dir).await?;

        // Load and return the cached pack
        self.load_cached(&resolved_pack.id, &resolved_pack.version)
    }

    /// Download a pack from its git repository
    async fn download_pack(&self, resolved_pack: &ResolvedPack, pack_dir: &Path) -> Result<()> {
        // Create parent directory
        let parent_dir = pack_dir.parent()
            .ok_or_else(|| anyhow::anyhow!("Invalid pack path: no parent directory"))?;
        fs::create_dir_all(parent_dir)
            .context("Failed to create pack directory")?;

        // Clone the repository
        let mut fetch_options = FetchOptions::new();
        let mut callbacks = RemoteCallbacks::new();

        // Progress callback
        callbacks.transfer_progress(|stats| {
            if stats.received_objects() % 100 == 0 {
                log::info!("Downloaded {} objects", stats.received_objects());
            }
            true
        });

        fetch_options.remote_callbacks(callbacks);

        // Clone to temporary directory first
        let temp_dir = TempDir::new().context("Failed to create temporary directory")?;

        let repo = Repository::clone(&resolved_pack.git_url, temp_dir.path())
            .context("Failed to clone repository")?;

        // Checkout specific revision
        let object = repo
            .revparse_single(&resolved_pack.git_rev)
            .context("Failed to find revision")?;

        repo.checkout_tree(&object, None)
            .context("Failed to checkout revision")?;

        // Move to final location
        fs::rename(temp_dir.path(), pack_dir).context("Failed to move downloaded pack")?;

        Ok(())
    }

    /// Load a cached pack
    pub fn load_cached(&self, pack_id: &str, version: &str) -> Result<CachedPack> {
        let pack_dir = self.cache_dir.join(pack_id).join(version);

        if !pack_dir.exists() {
            anyhow::bail!("Pack not found in cache: {}@{}", pack_id, version);
        }

        let sha256 = self.calculate_sha256(&pack_dir)?;

        // Try to load manifest
        let manifest_path = pack_dir.join("gpack.toml");
        let manifest = if manifest_path.exists() {
            let content = fs::read_to_string(&manifest_path).context("Failed to read manifest")?;
            Some(toml::from_str(&content).context("Failed to parse manifest")?)
        } else {
            None
        };

        Ok(CachedPack {
            id: pack_id.to_string(),
            version: version.to_string(),
            path: pack_dir,
            sha256,
            manifest,
        })
    }

    /// Calculate SHA256 hash of a directory
    fn calculate_sha256(&self, dir: &Path) -> Result<String> {
        let mut hasher = Sha256::new();

        // Walk directory and hash all files
        for entry in walkdir::WalkDir::new(dir) {
            let entry = entry.context("Failed to read directory entry")?;
            let path = entry.path();

            if path.is_file() {
                let content = fs::read(path).context("Failed to read file for hashing")?;
                hasher.update(&content);
            }
        }

        Ok(format!("{:x}", hasher.finalize()))
    }

    /// List all cached packs
    pub fn list_cached(&self) -> Result<Vec<CachedPack>> {
        let mut packs = Vec::new();

        if !self.cache_dir.exists() {
            return Ok(packs);
        }

        for pack_entry in fs::read_dir(&self.cache_dir).context("Failed to read cache directory")? {
            let pack_entry = pack_entry.context("Failed to read pack entry")?;
            let pack_path = pack_entry.path();

            if pack_path.is_dir() {
                let pack_id = pack_entry.file_name().to_string_lossy().to_string();

                // Look for version directories
                for version_entry in
                    fs::read_dir(&pack_path).context("Failed to read pack directory")?
                {
                    let version_entry = version_entry.context("Failed to read version entry")?;
                    let version_path = version_entry.path();

                    if version_path.is_dir() {
                        let version = version_entry.file_name().to_string_lossy().to_string();

                        if let Ok(cached) = self.load_cached(&pack_id, &version) {
                            packs.push(cached);
                        }
                    }
                }
            }
        }

        Ok(packs)
    }

    /// Remove a cached pack
    pub fn remove(&self, pack_id: &str, version: &str) -> Result<()> {
        let pack_dir = self.cache_dir.join(pack_id).join(version);

        if pack_dir.exists() {
            fs::remove_dir_all(&pack_dir).context("Failed to remove cached pack")?;
        }

        // Remove pack directory if empty
        if let Some(pack_parent) = pack_dir.parent() {
            if pack_parent.exists() && fs::read_dir(pack_parent)?.next().is_none() {
                fs::remove_dir(pack_parent).context("Failed to remove empty pack directory")?;
            }
        }

        Ok(())
    }

    /// Clean up old versions, keeping only the latest
    pub fn cleanup_old_versions(&self) -> Result<()> {
        if !self.cache_dir.exists() {
            return Ok(());
        }

        for pack_entry in fs::read_dir(&self.cache_dir).context("Failed to read cache directory")? {
            let pack_entry = pack_entry.context("Failed to read pack entry")?;
            let pack_path = pack_entry.path();

            if pack_path.is_dir() {
                let mut versions = Vec::new();

                // Collect all versions
                for version_entry in
                    fs::read_dir(&pack_path).context("Failed to read pack directory")?
                {
                    let version_entry = version_entry.context("Failed to read version entry")?;
                    let version_path = version_entry.path();

                    if version_path.is_dir() {
                        let version_str = version_entry.file_name().to_string_lossy().to_string();

                        if let Ok(version) = semver::Version::parse(&version_str) {
                            versions.push((version, version_path));
                        }
                    }
                }

                // Sort by version and keep only the latest
                versions.sort_by(|a, b| a.0.cmp(&b.0));

                for (_, version_path) in versions.into_iter().rev().skip(1) {
                    fs::remove_dir_all(&version_path).context("Failed to remove old version")?;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_cache_manager_creation() -> Result<()> {
        let temp_dir = TempDir::new().context("Failed to create temp dir")?;
        let cache_dir = temp_dir.path().to_path_buf();

        let cache_manager = CacheManager::with_dir(cache_dir.clone())?;
        assert_eq!(cache_manager.cache_dir(), cache_dir);
        Ok(())
    }

    #[test]
    fn test_sha256_calculation() -> Result<()> {
        let temp_dir = TempDir::new().context("Failed to create temp dir")?;
        let test_dir = temp_dir.path().join("test");
        fs::create_dir_all(&test_dir).context("Failed to create test dir")?;

        // Create test files
        fs::write(test_dir.join("file1.txt"), "content1").context("Failed to write file1")?;
        fs::write(test_dir.join("file2.txt"), "content2").context("Failed to write file2")?;

        let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf())?;
        let sha256 = cache_manager.calculate_sha256(&test_dir)?;

        // Should be a valid hex string
        assert_eq!(sha256.len(), 64);
        assert!(sha256.chars().all(|c| c.is_ascii_hexdigit()));
        Ok(())
    }

    #[test]
    fn test_list_cached_empty() -> Result<()> {
        let temp_dir = TempDir::new().context("Failed to create temp dir")?;
        let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf())?;

        let cached = cache_manager.list_cached()?;
        assert!(cached.is_empty());
        Ok(())
    }
}
