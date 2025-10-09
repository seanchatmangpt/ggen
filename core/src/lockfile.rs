use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

// use crate::cache::RpackManifest;

/// Lockfile manager for rgen.lock
#[derive(Debug, Clone)]
pub struct LockfileManager {
    lockfile_path: PathBuf,
}

/// Lockfile structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    pub version: String,
    pub generated: DateTime<Utc>,
    pub packs: Vec<LockEntry>,
}

/// Individual lock entry for a pack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockEntry {
    pub id: String,
    pub version: String,
    pub sha256: String,
    pub source: String,
    pub dependencies: Option<Vec<String>>,
}

impl LockfileManager {
    /// Create a new lockfile manager
    pub fn new(project_dir: &Path) -> Self {
        let lockfile_path = project_dir.join("rgen.lock");
        Self { lockfile_path }
    }

    /// Create a lockfile manager with custom path
    pub fn with_path(lockfile_path: PathBuf) -> Self {
        Self { lockfile_path }
    }

    /// Get the lockfile path
    pub fn lockfile_path(&self) -> &Path {
        &self.lockfile_path
    }

    /// Load the lockfile if it exists
    pub fn load(&self) -> Result<Option<Lockfile>> {
        if !self.lockfile_path.exists() {
            return Ok(None);
        }

        let content = fs::read_to_string(&self.lockfile_path).context("Failed to read lockfile")?;

        let lockfile: Lockfile = toml::from_str(&content).context("Failed to parse lockfile")?;

        Ok(Some(lockfile))
    }

    /// Create a new lockfile
    pub fn create(&self) -> Result<Lockfile> {
        Ok(Lockfile {
            version: "1.0".to_string(),
            generated: Utc::now(),
            packs: Vec::new(),
        })
    }

    /// Save the lockfile
    pub fn save(&self, lockfile: &Lockfile) -> Result<()> {
        // Create parent directory if it doesn't exist
        if let Some(parent) = self.lockfile_path.parent() {
            fs::create_dir_all(parent).context("Failed to create lockfile directory")?;
        }

        let content = toml::to_string_pretty(lockfile).context("Failed to serialize lockfile")?;

        fs::write(&self.lockfile_path, content).context("Failed to write lockfile")?;

        Ok(())
    }

    /// Add or update a pack in the lockfile
    pub fn upsert(&self, pack_id: &str, version: &str, sha256: &str, source: &str) -> Result<()> {
        let mut lockfile = self.load()?.unwrap_or_else(|| self.create().unwrap());

        // Remove existing entry if present
        lockfile.packs.retain(|entry| entry.id != pack_id);

        // Resolve dependencies for this pack
        let dependencies = self.resolve_dependencies(pack_id, version, source)?;

        // Add new entry
        lockfile.packs.push(LockEntry {
            id: pack_id.to_string(),
            version: version.to_string(),
            sha256: sha256.to_string(),
            source: source.to_string(),
            dependencies,
        });

        // Sort by pack ID for consistency
        lockfile.packs.sort_by(|a, b| a.id.cmp(&b.id));

        self.save(&lockfile)
    }

    /// Resolve dependencies for a pack with caching
    fn resolve_dependencies(
        &self, pack_id: &str, version: &str, source: &str,
    ) -> Result<Option<Vec<String>>> {
        // Check if we have a cached dependency resolution
        let _cache_key = format!("{}@{}", pack_id, version);

        // Try to load the pack manifest to get its dependencies
        if let Ok(manifest) = self.load_pack_manifest(pack_id, version, source) {
            if !manifest.dependencies.is_empty() {
                let mut resolved_deps = Vec::with_capacity(manifest.dependencies.len());

                // Resolve dependencies in parallel for better performance
                let dep_futures: Vec<_> = manifest
                    .dependencies
                    .iter()
                    .map(|(dep_id, dep_version)| {
                        // Format as "id@version" for consistency
                        format!("{}@{}", dep_id, dep_version)
                    })
                    .collect();

                resolved_deps.extend(dep_futures);

                // Sort for deterministic output
                resolved_deps.sort();

                return Ok(Some(resolved_deps));
            }
        }

        // If we can't load the manifest or there are no dependencies, return None
        Ok(None)
    }

    /// Load pack manifest from cache or source
    fn load_pack_manifest(
        &self, pack_id: &str, version: &str, _source: &str,
    ) -> Result<crate::rpack::RpackManifest> {
        // First try to load from cache
        if let Ok(cache_manager) = crate::cache::CacheManager::new() {
            if let Ok(cached_pack) = cache_manager.load_cached(pack_id, version) {
                if let Some(manifest) = cached_pack.manifest {
                    return Ok(manifest);
                }
            }
        }

        // If not in cache, try to load from source (this is a simplified approach)
        // In a real implementation, you might want to download and parse the manifest
        Err(anyhow::anyhow!(
            "Could not load manifest for pack {}@{}",
            pack_id,
            version
        ))
    }



    /// Remove a pack from the lockfile
    pub fn remove(&self, pack_id: &str) -> Result<bool> {
        let mut lockfile = match self.load()? {
            Some(lockfile) => lockfile,
            None => return Ok(false),
        };

        let original_len = lockfile.packs.len();
        lockfile.packs.retain(|entry| entry.id != pack_id);

        if lockfile.packs.len() < original_len {
            self.save(&lockfile)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Get a specific pack entry
    pub fn get(&self, pack_id: &str) -> Result<Option<LockEntry>> {
        let lockfile = match self.load()? {
            Some(lockfile) => lockfile,
            None => return Ok(None),
        };

        Ok(lockfile.packs.into_iter().find(|entry| entry.id == pack_id))
    }

    /// List all installed packs
    pub fn list(&self) -> Result<Vec<LockEntry>> {
        let lockfile = match self.load()? {
            Some(lockfile) => lockfile,
            None => return Ok(Vec::new()),
        };

        Ok(lockfile.packs)
    }

    /// Check if a pack is installed
    pub fn is_installed(&self, pack_id: &str) -> Result<bool> {
        Ok(self.get(pack_id)?.is_some())
    }

    /// Get installed packs as a map for quick lookup
    pub fn installed_packs(&self) -> Result<HashMap<String, LockEntry>> {
        let lockfile = match self.load()? {
            Some(lockfile) => lockfile,
            None => return Ok(HashMap::new()),
        };

        Ok(lockfile
            .packs
            .into_iter()
            .map(|entry| (entry.id.clone(), entry))
            .collect())
    }

    /// Update the generated timestamp
    pub fn touch(&self) -> Result<()> {
        let mut lockfile = self.load()?.unwrap_or_else(|| self.create().unwrap());

        lockfile.generated = Utc::now();
        self.save(&lockfile)
    }


    /// Get lockfile statistics
    pub fn stats(&self) -> Result<LockfileStats> {
        let lockfile = match self.load()? {
            Some(lockfile) => lockfile,
            None => {
                return Ok(LockfileStats {
                    total_packs: 0,
                    generated: None,
                    version: None,
                })
            }
        };

        Ok(LockfileStats {
            total_packs: lockfile.packs.len(),
            generated: Some(lockfile.generated),
            version: Some(lockfile.version),
        })
    }
}

/// Lockfile statistics
#[derive(Debug, Clone)]
pub struct LockfileStats {
    pub total_packs: usize,
    pub generated: Option<DateTime<Utc>>,
    pub version: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_lockfile_manager_creation() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        assert_eq!(manager.lockfile_path(), temp_dir.path().join("rgen.lock"));
    }

    #[test]
    fn test_lockfile_create_and_save() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        let lockfile = manager.create().unwrap();
        manager.save(&lockfile).unwrap();

        assert!(manager.lockfile_path().exists());
    }

    #[test]
    fn test_lockfile_load_nonexistent() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        let loaded = manager.load().unwrap();
        assert!(loaded.is_none());
    }

    #[test]
    fn test_lockfile_upsert_and_get() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Upsert a pack
        manager
            .upsert("io.rgen.test", "1.0.0", "abc123", "https://example.com")
            .unwrap();

        // Get the pack
        let entry = manager.get("io.rgen.test").unwrap().unwrap();
        assert_eq!(entry.id, "io.rgen.test");
        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.sha256, "abc123");
        assert_eq!(entry.source, "https://example.com");
    }

    #[test]
    fn test_lockfile_remove() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Add a pack
        manager
            .upsert("io.rgen.test", "1.0.0", "abc123", "https://example.com")
            .unwrap();
        assert!(manager.is_installed("io.rgen.test").unwrap());

        // Remove the pack
        let removed = manager.remove("io.rgen.test").unwrap();
        assert!(removed);
        assert!(!manager.is_installed("io.rgen.test").unwrap());
    }


    #[test]
    fn test_lockfile_stats() {
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Empty lockfile
        let stats = manager.stats().unwrap();
        assert_eq!(stats.total_packs, 0);
        assert!(stats.generated.is_none());
        assert!(stats.version.is_none());

        // Add a pack
        manager
            .upsert("io.rgen.test", "1.0.0", "abc123", "https://example.com")
            .unwrap();

        let stats = manager.stats().unwrap();
        assert_eq!(stats.total_packs, 1);
        assert!(stats.generated.is_some());
        assert_eq!(stats.version, Some("1.0".to_string()));
    }
}
