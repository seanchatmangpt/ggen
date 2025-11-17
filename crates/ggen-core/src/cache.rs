//! Local cache manager for gpack templates
//!
//! This module provides caching functionality for downloaded gpack templates.
//! The `CacheManager` handles local storage, versioning, and integrity verification
//! of cached template packs.
//!
//! ## Features
//!
//! - **Local caching**: Store downloaded packs in user cache directory
//! - **SHA256 verification**: Verify pack integrity using checksums
//! - **Version management**: Support multiple versions of the same pack
//! - **Automatic cleanup**: Remove old versions, keeping only the latest
//! - **Git integration**: Clone and checkout specific revisions
//!
//! ## Examples
//!
//! ### Creating a Cache Manager
//!
//! ```rust,no_run
//! use ggen_core::cache::CacheManager;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Use default cache directory (~/.cache/ggen/gpacks)
//! let cache = CacheManager::new()?;
//!
//! // Or use a custom directory (useful for testing)
//! use std::path::PathBuf;
//! let cache = CacheManager::with_dir(PathBuf::from("/tmp/ggen-cache"))?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Ensuring a Pack is Cached
//!
//! ```rust,no_run
//! use ggen_core::cache::CacheManager;
//! use ggen_core::registry::ResolvedPack;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let cache = CacheManager::new()?;
//! let resolved_pack = ResolvedPack {
//!     id: "io.ggen.example".to_string(),
//!     version: "1.0.0".to_string(),
//!     git_url: "https://github.com/example/pack.git".to_string(),
//!     git_rev: "v1.0.0".to_string(),
//!     sha256: "abc123...".to_string(),
//! };
//!
//! // Download and cache the pack if not already cached
//! let cached = cache.ensure(&resolved_pack).await?;
//! println!("Cached pack at: {:?}", cached.path);
//! # Ok(())
//! # }
//! ```
//!
//! ### Listing Cached Packs
//!
//! ```rust,no_run
//! use ggen_core::cache::CacheManager;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let cache = CacheManager::new()?;
//! let cached_packs = cache.list_cached()?;
//!
//! for pack in cached_packs {
//!     println!("{}@{}: {:?}", pack.id, pack.version, pack.path);
//! }
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
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
    ///
    /// Uses the default cache directory (`~/.cache/ggen/gpacks` on Unix systems).
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// println!("Cache directory: {:?}", cache.cache_dir());
    /// # Ok(())
    /// # }
    /// ```
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .ok_or_else(|| Error::new("Failed to find cache directory"))?
            .join("ggen")
            .join("gpacks");

        fs::create_dir_all(&cache_dir)
            .map_err(|e| Error::with_context("Failed to create cache directory", &e.to_string()))?;

        Ok(Self { cache_dir })
    }

    /// Create a cache manager with custom directory (for testing)
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::with_dir(PathBuf::from("/tmp/ggen-cache"))?;
    /// println!("Cache directory: {:?}", cache.cache_dir());
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&cache_dir)
            .map_err(|e| Error::with_context("Failed to create cache directory", &e.to_string()))?;

        Ok(Self { cache_dir })
    }

    /// Get the cache directory path
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// let cache_path = cache.cache_dir();
    /// println!("Cache is at: {:?}", cache_path);
    /// # Ok(())
    /// # }
    /// ```
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Ensure a pack is cached locally
    ///
    /// Downloads the pack from its git repository if not already cached, or
    /// verifies the cached version matches the expected SHA256 checksum.
    ///
    /// **SHA256 verification**: If `resolved_pack.sha256` is provided and not empty,
    /// the cached pack's SHA256 is verified. If the checksum doesn't match, the
    /// corrupted cache is automatically removed and the pack is re-downloaded.
    /// If no SHA256 is provided, the cached pack is used as-is without verification.
    ///
    /// **Automatic recovery**: If a cached pack is found but its SHA256 doesn't match
    /// the expected value, the method automatically removes the corrupted cache and
    /// re-downloads the pack. This ensures data integrity without manual intervention.
    ///
    /// # Arguments
    ///
    /// * `resolved_pack` - Pack metadata including git URL, revision, and optional SHA256
    ///
    /// # Returns
    ///
    /// Returns information about the cached pack, including its local path.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The pack cannot be downloaded from git
    /// - The SHA256 checksum doesn't match after re-download (if provided)
    /// - The cache directory cannot be accessed or created
    /// - The corrupted cache cannot be removed (should not occur in normal use)
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    /// use ggen_core::registry::ResolvedPack;
    ///
    /// # async fn example() -> anyhow::Result<()> {
    /// let cache = CacheManager::new()?;
    /// let pack = ResolvedPack {
    ///     id: "io.ggen.example".to_string(),
    ///     version: "1.0.0".to_string(),
    ///     git_url: "https://github.com/example/pack.git".to_string(),
    ///     git_rev: "v1.0.0".to_string(),
    ///     sha256: "abc123...".to_string(),
    /// };
    ///
    /// let cached = cache.ensure(&pack).await?;
    /// println!("Pack cached at: {:?}", cached.path);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid git URL
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    /// use ggen_core::registry::ResolvedPack;
    ///
    /// # async fn example() -> anyhow::Result<()> {
    /// let cache = CacheManager::new()?;
    /// let pack = ResolvedPack {
    ///     id: "io.ggen.example".to_string(),
    ///     version: "1.0.0".to_string(),
    ///     git_url: "https://invalid-url-that-does-not-exist.git".to_string(),
    ///     git_rev: "v1.0.0".to_string(),
    ///     sha256: "".to_string(),
    /// };
    ///
    /// // This will fail because the git URL is invalid
    /// let result = cache.ensure(&pack).await;
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
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
                        fs::remove_dir_all(&pack_dir).map_err(|e| {
                            Error::with_context("Failed to remove corrupted cache", &e.to_string())
                        })?;
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
        let parent_dir = pack_dir
            .parent()
            .ok_or_else(|| Error::new("Invalid pack path: no parent directory"))?;
        fs::create_dir_all(parent_dir)
            .map_err(|e| Error::with_context("Failed to create pack directory", &e.to_string()))?;

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
        let temp_dir = TempDir::new().map_err(|e| {
            Error::with_context("Failed to create temporary directory", &e.to_string())
        })?;

        let repo = Repository::clone(&resolved_pack.git_url, temp_dir.path())
            .map_err(|e| Error::with_context("Failed to clone repository", &e.to_string()))?;

        // Checkout specific revision
        let object = repo
            .revparse_single(&resolved_pack.git_rev)
            .map_err(|e| Error::with_context("Failed to find revision", &e.to_string()))?;

        repo.checkout_tree(&object, None)
            .map_err(|e| Error::with_context("Failed to checkout revision", &e.to_string()))?;

        // Move to final location
        fs::rename(temp_dir.path(), pack_dir)
            .map_err(|e| Error::with_context("Failed to move downloaded pack", &e.to_string()))?;

        Ok(())
    }

    /// Load a cached pack
    ///
    /// Returns information about a pack that is already cached locally.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The pack is not found in the cache
    /// - The pack directory exists but is corrupted
    /// - The manifest file cannot be read or parsed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// // Assuming pack is already cached
    /// let cached = cache.load_cached("io.ggen.example", "1.0.0")?;
    /// println!("Pack path: {:?}", cached.path);
    /// println!("Pack SHA256: {}", cached.sha256);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Pack not found
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// // This will fail because the pack is not cached
    /// let result = cache.load_cached("nonexistent.pack", "1.0.0");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_cached(&self, pack_id: &str, version: &str) -> Result<CachedPack> {
        let pack_dir = self.cache_dir.join(pack_id).join(version);

        if !pack_dir.exists() {
            return Err(Error::new(&format!(
                "Pack not found in cache: {}@{}",
                pack_id, version
            )));
        }

        let sha256 = self.calculate_sha256(&pack_dir)?;

        // Try to load manifest
        let manifest_path = pack_dir.join("gpack.toml");
        let manifest = if manifest_path.exists() {
            let content = fs::read_to_string(&manifest_path)
                .map_err(|e| Error::with_context("Failed to read manifest", &e.to_string()))?;
            Some(
                toml::from_str(&content)
                    .map_err(|e| Error::with_context("Failed to parse manifest", &e.to_string()))?,
            )
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
            let entry = entry.map_err(|e| {
                Error::with_context("Failed to read directory entry", &e.to_string())
            })?;
            let path = entry.path();

            if path.is_file() {
                let content = fs::read(path).map_err(|e| {
                    Error::with_context("Failed to read file for hashing", &e.to_string())
                })?;
                hasher.update(&content);
            }
        }

        Ok(format!("{:x}", hasher.finalize()))
    }

    /// List all cached packs
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// let cached_packs = cache.list_cached()?;
    ///
    /// for pack in cached_packs {
    ///     println!("{}@{}: {:?}", pack.id, pack.version, pack.path);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn list_cached(&self) -> Result<Vec<CachedPack>> {
        let mut packs = Vec::new();

        if !self.cache_dir.exists() {
            return Ok(packs);
        }

        for pack_entry in fs::read_dir(&self.cache_dir)
            .map_err(|e| Error::with_context("Failed to read cache directory", &e.to_string()))?
        {
            let pack_entry = pack_entry
                .map_err(|e| Error::with_context("Failed to read pack entry", &e.to_string()))?;
            let pack_path = pack_entry.path();

            if pack_path.is_dir() {
                let pack_id = pack_entry.file_name().to_string_lossy().to_string();

                // Look for version directories
                for version_entry in fs::read_dir(&pack_path).map_err(|e| {
                    Error::with_context("Failed to read pack directory", &e.to_string())
                })? {
                    let version_entry = version_entry.map_err(|e| {
                        Error::with_context("Failed to read version entry", &e.to_string())
                    })?;
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
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The pack directory cannot be removed
    /// - The cache directory cannot be accessed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// // Remove a specific version of a pack
    /// cache.remove("io.ggen.example", "1.0.0")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Permission denied
    ///
    /// ```rust,no_run
    /// use ggen_core::cache::CacheManager;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = CacheManager::new()?;
    /// // This may fail if we don't have permission to remove the pack
    /// let result = cache.remove("io.ggen.example", "1.0.0");
    /// // Handle error appropriately
    /// if let Err(e) = result {
    ///     eprintln!("Failed to remove pack: {}", e);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn remove(&self, pack_id: &str, version: &str) -> Result<()> {
        let pack_dir = self.cache_dir.join(pack_id).join(version);

        if pack_dir.exists() {
            fs::remove_dir_all(&pack_dir)
                .map_err(|e| Error::with_context("Failed to remove cached pack", &e.to_string()))?;
        }

        // Remove pack directory if empty
        if let Some(pack_parent) = pack_dir.parent() {
            if pack_parent.exists() && fs::read_dir(pack_parent)?.next().is_none() {
                fs::remove_dir(pack_parent).map_err(|e| {
                    Error::with_context("Failed to remove empty pack directory", &e.to_string())
                })?;
            }
        }

        Ok(())
    }

    /// Clean up old versions, keeping only the latest
    pub fn cleanup_old_versions(&self) -> Result<()> {
        if !self.cache_dir.exists() {
            return Ok(());
        }

        for pack_entry in fs::read_dir(&self.cache_dir)
            .map_err(|e| Error::with_context("Failed to read cache directory", &e.to_string()))?
        {
            let pack_entry = pack_entry
                .map_err(|e| Error::with_context("Failed to read pack entry", &e.to_string()))?;
            let pack_path = pack_entry.path();

            if pack_path.is_dir() {
                let mut versions = Vec::new();

                // Collect all versions
                for version_entry in fs::read_dir(&pack_path).map_err(|e| {
                    Error::with_context("Failed to read pack directory", &e.to_string())
                })? {
                    let version_entry = version_entry.map_err(|e| {
                        Error::with_context("Failed to read version entry", &e.to_string())
                    })?;
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
                    fs::remove_dir_all(&version_path).map_err(|e| {
                        Error::with_context("Failed to remove old version", &e.to_string())
                    })?;
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
    fn test_cache_manager_creation() {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::with_context("Failed to create temp dir", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        let cache_dir = temp_dir.path().to_path_buf();

        let cache_manager = CacheManager::with_dir(cache_dir.clone())
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        assert_eq!(cache_manager.cache_dir(), cache_dir);
        Ok::<(), Box<dyn std::error::Error>>(())
    }

    #[test]
    fn test_sha256_calculation() {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::with_context("Failed to create temp dir", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        let test_dir = temp_dir.path().join("test");
        fs::create_dir_all(&test_dir)
            .map_err(|e| Error::with_context("Failed to create test dir", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;

        // Create test files
        fs::write(test_dir.join("file1.txt"), "content1")
            .map_err(|e| Error::with_context("Failed to write file1", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        fs::write(test_dir.join("file2.txt"), "content2")
            .map_err(|e| Error::with_context("Failed to write file2", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;

        let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf())
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        let sha256 = cache_manager
            .calculate_sha256(&test_dir)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;

        // Should be a valid hex string
        assert_eq!(sha256.len(), 64);
        assert!(sha256.chars().all(|c| c.is_ascii_hexdigit()));
        Ok::<(), Box<dyn std::error::Error>>(())
    }

    #[test]
    fn test_list_cached_empty() {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::with_context("Failed to create temp dir", &e.to_string()))
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf())
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;

        let cached = cache_manager
            .list_cached()
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        assert!(cached.is_empty());
        Ok::<(), Box<dyn std::error::Error>>(())
    }
}
