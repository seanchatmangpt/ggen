//! Registry infrastructure for ggen marketplace
//!
//! This module implements the package registry and cache management system.
//! It provides production-ready infrastructure for:
//! - Loading and querying package indices
//! - LRU cache management for package metadata
//! - Async filesystem operations
//! - Package version resolution

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use tokio::fs;
use tracing::{debug, info, instrument, warn};

/// Package metadata in the registry index
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackageMetadata {
    /// Package name
    pub name: String,

    /// Available versions
    pub versions: Vec<VersionMetadata>,

    /// Package description
    pub description: String,

    /// Package author
    pub author: Option<String>,

    /// Package category
    pub category: Option<String>,

    /// Package tags
    pub tags: Vec<String>,

    /// Repository URL
    pub repository: Option<String>,

    /// License
    pub license: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,
}

/// Version-specific metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct VersionMetadata {
    /// Version string (e.g., "1.0.0")
    pub version: String,

    /// Download URL for this version
    pub download_url: String,

    /// Checksum (SHA256)
    pub checksum: String,

    /// Dependencies
    pub dependencies: Vec<Dependency>,

    /// Published timestamp (RFC3339 format)
    pub published_at: String,

    /// Size in bytes
    pub size_bytes: u64,
}

/// Package dependency specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Dependency {
    /// Dependency package name
    pub name: String,

    /// Version requirement (e.g., "^1.0.0")
    pub version_req: String,

    /// Whether this is an optional dependency
    pub optional: bool,
}

/// Registry index file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryIndex {
    /// Registry version
    pub version: String,

    /// Last updated timestamp (RFC3339 format)
    pub updated_at: String,

    /// Map of package name to metadata
    pub packages: HashMap<String, PackageMetadata>,
}

impl RegistryIndex {
    /// Create a new empty registry index
    pub fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
            updated_at: chrono::Utc::now().to_rfc3339(),
            packages: HashMap::new(),
        }
    }

    /// Add or update a package in the index
    pub fn add_package(&mut self, metadata: PackageMetadata) {
        self.packages.insert(metadata.name.clone(), metadata);
        self.updated_at = chrono::Utc::now().to_rfc3339();
    }

    /// Get package metadata by name
    pub fn get_package(&self, name: &str) -> Option<&PackageMetadata> {
        self.packages.get(name)
    }

    /// List all package names
    pub fn list_packages(&self) -> Vec<String> {
        self.packages.keys().cloned().collect()
    }
}

impl Default for RegistryIndex {
    fn default() -> Self {
        Self::new()
    }
}

/// LRU cache entry
/// NOTE: Currently unused - CacheManager uses HashMap directly
/// FUTURE: May be used if LRU queue implementation changes
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct CacheEntry {
    key: String,
    value: PackageMetadata,
}

/// Cache manager with LRU eviction policy
#[derive(Debug, Clone)]
pub struct CacheManager {
    /// Maximum cache capacity (number of entries)
    capacity: usize,

    /// Cache storage (name -> metadata)
    cache: Arc<RwLock<HashMap<String, PackageMetadata>>>,

    /// LRU queue for eviction (most recent at back)
    lru_queue: Arc<RwLock<VecDeque<String>>>,
}

impl CacheManager {
    /// Create a new cache manager with specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity,
            cache: Arc::new(RwLock::new(HashMap::new())),
            lru_queue: Arc::new(RwLock::new(VecDeque::new())),
        }
    }

    /// Get package metadata from cache
    #[instrument(skip(self))]
    pub fn get(&self, name: &str) -> Option<PackageMetadata> {
        let cache = self.cache.read().ok()?;
        let metadata = cache.get(name).cloned();

        if metadata.is_some() {
            // Move to back of LRU queue (most recently used)
            if let Ok(mut queue) = self.lru_queue.write() {
                queue.retain(|k| k != name);
                queue.push_back(name.to_string());
            }
            debug!("Cache hit for package: {}", name);
        } else {
            debug!("Cache miss for package: {}", name);
        }

        metadata
    }

    /// Put package metadata into cache
    #[instrument(skip(self, metadata))]
    pub fn put(&self, name: String, metadata: PackageMetadata) {
        let mut cache = match self.cache.write() {
            Ok(c) => c,
            Err(e) => {
                warn!("Failed to acquire cache write lock: {}", e);
                return;
            }
        };

        let mut queue = match self.lru_queue.write() {
            Ok(q) => q,
            Err(e) => {
                warn!("Failed to acquire LRU queue write lock: {}", e);
                return;
            }
        };

        // Remove from queue if already exists
        queue.retain(|k| k != &name);

        // Evict least recently used if at capacity
        if cache.len() >= self.capacity && !cache.contains_key(&name) {
            if let Some(lru_key) = queue.pop_front() {
                cache.remove(&lru_key);
                debug!("Evicted LRU package: {}", lru_key);
            }
        }

        // Insert into cache and queue
        cache.insert(name.clone(), metadata);
        queue.push_back(name.clone());
        debug!("Cached package: {}", name);
    }

    /// Clear all cache entries
    pub fn clear(&self) {
        if let Ok(mut cache) = self.cache.write() {
            cache.clear();
        }
        if let Ok(mut queue) = self.lru_queue.write() {
            queue.clear();
        }
        debug!("Cache cleared");
    }

    /// Get current cache size
    pub fn size(&self) -> usize {
        self.cache.read().map(|c| c.len()).unwrap_or(0)
    }

    /// Get cache capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

/// Registry for package discovery and metadata queries
#[derive(Debug, Clone)]
pub struct Registry {
    /// Path to registry index file
    index_path: PathBuf,

    /// In-memory registry index
    index: Arc<RwLock<Option<RegistryIndex>>>,

    /// Cache manager for package metadata
    cache: CacheManager,
}

impl Registry {
    /// Create a new registry with default paths
    pub fn new() -> Result<Self> {
        let home_dir =
            dirs::home_dir().ok_or_else(|| Error::new("Failed to determine home directory"))?;

        let index_path = home_dir.join(".ggen").join("registry").join("index.json");

        Ok(Self {
            index_path,
            index: Arc::new(RwLock::new(None)),
            cache: CacheManager::new(100), // Default cache capacity: 100 packages
        })
    }

    /// Create a registry with custom index path (for testing)
    pub fn with_path(index_path: PathBuf) -> Self {
        Self {
            index_path,
            index: Arc::new(RwLock::new(None)),
            cache: CacheManager::new(100),
        }
    }

    /// Create a registry with custom cache capacity
    pub fn with_cache_capacity(capacity: usize) -> Result<Self> {
        let home_dir =
            dirs::home_dir().ok_or_else(|| Error::new("Failed to determine home directory"))?;

        let index_path = home_dir.join(".ggen").join("registry").join("index.json");

        Ok(Self {
            index_path,
            index: Arc::new(RwLock::new(None)),
            cache: CacheManager::new(capacity),
        })
    }

    /// Load registry index from filesystem with strict validation
    ///
    /// **DETERMINISTIC**: Fails fast on any error to maintain determinism
    /// - Registry file MUST exist
    /// - JSON MUST be valid
    /// - Structure MUST be correct
    /// - Corruption is detected and reported (not silently fixed)
    ///
    /// This ensures predictable behavior and early failure detection.
    #[instrument(skip(self))]
    pub async fn load(&self) -> Result<()> {
        info!("Loading registry index from: {}", self.index_path.display());

        // Create parent directories if they don't exist
        if let Some(parent) = self.index_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        // Load index file - FAIL if missing or corrupted
        let index = if self.index_path.exists() {
            // Try to load and parse the index
            let contents = fs::read_to_string(&self.index_path).await.map_err(|e| {
                Error::new(&format!(
                    "Failed to read registry index from {}: {}. Registry may be corrupted.",
                    self.index_path.display(),
                    e
                ))
            })?;

            serde_json::from_str::<RegistryIndex>(&contents).map_err(|e| {
                Error::new(&format!(
                    "Failed to parse registry index from {} - invalid JSON: {}. Registry is corrupted. Delete {} and re-sync.",
                    self.index_path.display(),
                    e,
                    self.index_path.display()
                ))
            })?
        } else {
            return Err(Error::new(&format!(
                "Registry index not found at {}. Run 'ggen marketplace sync' to download the registry.",
                self.index_path.display()
            )));
        };

        // Store in memory
        let mut guard = self
            .index
            .write()
            .map_err(|e| Error::new(&format!("Failed to acquire index write lock: {}", e)))?;
        *guard = Some(index);

        info!("Registry index loaded successfully");
        Ok(())
    }

    /// Save registry index to filesystem
    #[instrument(skip(self))]
    pub async fn save(&self) -> Result<()> {
        // Extract index data before await to avoid holding lock across await
        let index_data = {
            let guard = self
                .index
                .read()
                .map_err(|e| Error::new(&format!("Failed to acquire index read lock: {}", e)))?;

            guard
                .as_ref()
                .ok_or_else(|| Error::new("Registry index not loaded"))?
                .clone()
        };

        // Create parent directories if they don't exist
        if let Some(parent) = self.index_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        // Serialize and write to file
        let contents = serde_json::to_string_pretty(&index_data)?;
        fs::write(&self.index_path, contents).await?;

        info!("Registry index saved to: {}", self.index_path.display());
        Ok(())
    }

    /// Get package metadata by name (checks cache first, then index)
    #[instrument(skip(self))]
    pub async fn get_package(&self, name: &str) -> Result<Option<PackageMetadata>> {
        // Check cache first
        if let Some(cached) = self.cache.get(name) {
            return Ok(Some(cached));
        }

        // Load from index if not in cache
        let guard = self
            .index
            .read()
            .map_err(|e| Error::new(&format!("Failed to acquire index read lock: {}", e)))?;

        let index = guard
            .as_ref()
            .ok_or_else(|| Error::new("Registry index not loaded"))?;

        if let Some(metadata) = index.get_package(name) {
            let metadata = metadata.clone();
            // Cache the result
            self.cache.put(name.to_string(), metadata.clone());
            Ok(Some(metadata))
        } else {
            Ok(None)
        }
    }

    /// List all versions for a package
    #[instrument(skip(self))]
    pub async fn list_versions(&self, name: &str) -> Result<Vec<String>> {
        let metadata = self
            .get_package(name)
            .await?
            .ok_or_else(|| Error::new(&format!("Package not found: {}", name)))?;

        Ok(metadata
            .versions
            .iter()
            .map(|v| v.version.clone())
            .collect())
    }

    /// Get specific version metadata
    #[instrument(skip(self))]
    pub async fn get_version(&self, name: &str, version: &str) -> Result<Option<VersionMetadata>> {
        let metadata = self.get_package(name).await?;

        Ok(metadata.and_then(|m| m.versions.iter().find(|v| v.version == version).cloned()))
    }

    /// List all packages in registry
    #[instrument(skip(self))]
    pub async fn list_packages(&self) -> Result<Vec<String>> {
        let guard = self
            .index
            .read()
            .map_err(|e| Error::new(&format!("Failed to acquire index read lock: {}", e)))?;

        let index = guard
            .as_ref()
            .ok_or_else(|| Error::new("Registry index not loaded"))?;

        Ok(index.list_packages())
    }

    /// Add or update a package in the registry
    #[instrument(skip(self, metadata))]
    pub async fn add_package(&self, metadata: PackageMetadata) -> Result<()> {
        let mut guard = self
            .index
            .write()
            .map_err(|e| Error::new(&format!("Failed to acquire index write lock: {}", e)))?;

        let index = guard
            .as_mut()
            .ok_or_else(|| Error::new("Registry index not loaded"))?;

        let name = metadata.name.clone();
        index.add_package(metadata.clone());

        // Update cache
        self.cache.put(name.clone(), metadata);

        info!("Added package to registry: {}", name);
        Ok(())
    }

    /// Get the cache manager
    pub fn cache(&self) -> &CacheManager {
        &self.cache
    }

    /// Get the index path
    pub fn index_path(&self) -> &Path {
        &self.index_path
    }

    /// Validate registry index integrity with strict checks
    ///
    /// **DETERMINISTIC**: Fails fast if registry is invalid
    ///
    /// Checks for:
    /// - Registry is loaded
    /// - Packages array exists and is not empty
    /// - All package metadata is valid (name, versions, download URLs, checksums)
    /// - No invalid state
    ///
    /// Returns error on ANY validation failure - no silent degradation
    pub async fn validate(&self) -> Result<()> {
        let guard = self
            .index
            .read()
            .map_err(|e| Error::new(&format!("Failed to acquire index read lock: {}", e)))?;

        let index = guard
            .as_ref()
            .ok_or_else(|| Error::new("Registry index not loaded"))?;

        // STRICT: Empty registry is an error, not just a warning
        if index.packages.is_empty() {
            return Err(Error::new(
                "Registry index is empty. Run 'ggen marketplace sync' to download the registry."
            ));
        }

        // Validate package metadata integrity - STRICT
        for (name, metadata) in &index.packages {
            if metadata.versions.is_empty() {
                return Err(Error::new(&format!(
                    "Package {} has no versions defined - registry is corrupted",
                    name
                )));
            }

            for version in &metadata.versions {
                // STRICT: Download URL MUST exist
                if version.download_url.is_empty() {
                    return Err(Error::new(&format!(
                        "Package {}@{} has empty download URL - registry is corrupted",
                        name, version.version
                    )));
                }

                // STRICT: Checksum MUST exist
                if version.checksum.is_empty() {
                    return Err(Error::new(&format!(
                        "Package {}@{} has empty checksum - registry is corrupted",
                        name, version.version
                    )));
                }
            }
        }

        Ok(())
    }
}

impl Default for Registry {
    fn default() -> Self {
        // Default implementation returns error if home directory not found
        // This is handled by Registry::new() which returns Result
        Self::new().unwrap_or_else(|_| {
            // Fallback to temp directory if home not available
            let temp_path = std::env::temp_dir()
                .join("ggen")
                .join("registry")
                .join("index.json");
            Self::with_path(temp_path)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// Helper to create test package metadata
    fn create_test_package(name: &str, version: &str) -> PackageMetadata {
        PackageMetadata {
            name: name.to_string(),
            versions: vec![VersionMetadata {
                version: version.to_string(),
                download_url: format!("https://example.com/{}/{}.tar.gz", name, version),
                checksum: "abcd1234".to_string(),
                dependencies: vec![],
                published_at: chrono::Utc::now().to_rfc3339(),
                size_bytes: 1024,
            }],
            description: format!("Test package {}", name),
            author: Some("Test Author".to_string()),
            category: Some("testing".to_string()),
            tags: vec!["test".to_string()],
            repository: Some("https://github.com/test/repo".to_string()),
            license: Some("MIT".to_string()),
            homepage: Some("https://example.com".to_string()),
        }
    }

    #[test]
    fn test_registry_index_creation() {
        let index = RegistryIndex::new();
        assert_eq!(index.version, "1.0.0");
        assert!(index.packages.is_empty());
    }

    #[test]
    fn test_registry_index_add_package() {
        let mut index = RegistryIndex::new();
        let package = create_test_package("test-pkg", "1.0.0");

        index.add_package(package.clone());

        assert_eq!(index.packages.len(), 1);
        assert_eq!(index.get_package("test-pkg"), Some(&package));
    }

    #[test]
    fn test_cache_manager_basic_operations() {
        let cache = CacheManager::new(3);
        let pkg1 = create_test_package("pkg1", "1.0.0");
        let pkg2 = create_test_package("pkg2", "1.0.0");

        // Initially empty
        assert_eq!(cache.size(), 0);

        // Add packages
        cache.put("pkg1".to_string(), pkg1.clone());
        cache.put("pkg2".to_string(), pkg2.clone());

        assert_eq!(cache.size(), 2);

        // Get from cache
        assert_eq!(cache.get("pkg1"), Some(pkg1));
        assert_eq!(cache.get("pkg2"), Some(pkg2));
        assert_eq!(cache.get("pkg3"), None);
    }

    #[test]
    fn test_cache_manager_lru_eviction() {
        let cache = CacheManager::new(2);
        let pkg1 = create_test_package("pkg1", "1.0.0");
        let pkg2 = create_test_package("pkg2", "1.0.0");
        let pkg3 = create_test_package("pkg3", "1.0.0");

        // Fill cache to capacity
        cache.put("pkg1".to_string(), pkg1.clone());
        cache.put("pkg2".to_string(), pkg2.clone());
        assert_eq!(cache.size(), 2);

        // Access pkg1 to make it most recently used
        let _ = cache.get("pkg1");

        // Add pkg3, should evict pkg2 (least recently used)
        cache.put("pkg3".to_string(), pkg3.clone());

        assert_eq!(cache.size(), 2);
        assert_eq!(cache.get("pkg1"), Some(pkg1)); // Still in cache
        assert_eq!(cache.get("pkg2"), None); // Evicted
        assert_eq!(cache.get("pkg3"), Some(pkg3)); // Newly added
    }

    #[test]
    fn test_cache_manager_clear() {
        let cache = CacheManager::new(5);
        cache.put("pkg1".to_string(), create_test_package("pkg1", "1.0.0"));
        cache.put("pkg2".to_string(), create_test_package("pkg2", "1.0.0"));

        assert_eq!(cache.size(), 2);

        cache.clear();

        assert_eq!(cache.size(), 0);
        assert_eq!(cache.get("pkg1"), None);
    }

    #[tokio::test]
    async fn test_registry_load_and_save_real_filesystem() {
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("index.json");

        // Create registry
        let registry = Registry::with_path(index_path.clone());

        // Load (will create new since file doesn't exist)
        registry.load().await.unwrap();

        // Add a package
        let package = create_test_package("real-pkg", "2.0.0");
        registry.add_package(package.clone()).await.unwrap();

        // Save to filesystem
        registry.save().await.unwrap();

        // Verify file exists
        assert!(index_path.exists());

        // Create new registry and load from file
        let registry2 = Registry::with_path(index_path);
        registry2.load().await.unwrap();

        // Verify package is loaded
        let loaded = registry2.get_package("real-pkg").await.unwrap();
        assert_eq!(loaded, Some(package));
    }

    #[tokio::test]
    async fn test_registry_get_package_with_cache() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        registry.load().await.unwrap();

        let package = create_test_package("cached-pkg", "1.5.0");
        registry.add_package(package.clone()).await.unwrap();

        // First get - loads from index and caches
        let result1 = registry.get_package("cached-pkg").await.unwrap();
        assert_eq!(result1, Some(package.clone()));

        // Second get - should come from cache
        let result2 = registry.get_package("cached-pkg").await.unwrap();
        assert_eq!(result2, Some(package));

        // Verify it's in cache
        assert_eq!(registry.cache().size(), 1);
    }

    #[tokio::test]
    async fn test_registry_list_versions() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        registry.load().await.unwrap();

        // Create package with multiple versions
        let mut package = create_test_package("multi-ver", "1.0.0");
        package.versions.push(VersionMetadata {
            version: "1.1.0".to_string(),
            download_url: "https://example.com/multi-ver/1.1.0.tar.gz".to_string(),
            checksum: "efgh5678".to_string(),
            dependencies: vec![],
            published_at: chrono::Utc::now().to_rfc3339(),
            size_bytes: 2048,
        });
        package.versions.push(VersionMetadata {
            version: "2.0.0".to_string(),
            download_url: "https://example.com/multi-ver/2.0.0.tar.gz".to_string(),
            checksum: "ijkl9012".to_string(),
            dependencies: vec![],
            published_at: chrono::Utc::now().to_rfc3339(),
            size_bytes: 4096,
        });

        registry.add_package(package).await.unwrap();

        let versions = registry.list_versions("multi-ver").await.unwrap();
        assert_eq!(versions.len(), 3);
        assert!(versions.contains(&"1.0.0".to_string()));
        assert!(versions.contains(&"1.1.0".to_string()));
        assert!(versions.contains(&"2.0.0".to_string()));
    }

    #[tokio::test]
    async fn test_registry_get_specific_version() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        registry.load().await.unwrap();

        let package = create_test_package("versioned-pkg", "3.2.1");
        registry.add_package(package).await.unwrap();

        let version = registry
            .get_version("versioned-pkg", "3.2.1")
            .await
            .unwrap();
        assert!(version.is_some());
        assert_eq!(version.unwrap().version, "3.2.1");

        let nonexistent = registry
            .get_version("versioned-pkg", "9.9.9")
            .await
            .unwrap();
        assert!(nonexistent.is_none());
    }

    #[tokio::test]
    async fn test_registry_list_packages() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        registry.load().await.unwrap();

        registry
            .add_package(create_test_package("pkg-a", "1.0.0"))
            .await
            .unwrap();
        registry
            .add_package(create_test_package("pkg-b", "2.0.0"))
            .await
            .unwrap();
        registry
            .add_package(create_test_package("pkg-c", "3.0.0"))
            .await
            .unwrap();

        let packages = registry.list_packages().await.unwrap();
        assert_eq!(packages.len(), 3);
        assert!(packages.contains(&"pkg-a".to_string()));
        assert!(packages.contains(&"pkg-b".to_string()));
        assert!(packages.contains(&"pkg-c".to_string()));
    }

    #[tokio::test]
    async fn test_registry_persistence_across_instances() {
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("shared-index.json");

        // First registry instance
        {
            let registry1 = Registry::with_path(index_path.clone());
            registry1.load().await.unwrap();
            registry1
                .add_package(create_test_package("persistent-pkg", "1.0.0"))
                .await
                .unwrap();
            registry1.save().await.unwrap();
        }

        // Second registry instance (simulates restart)
        {
            let registry2 = Registry::with_path(index_path);
            registry2.load().await.unwrap();

            let loaded = registry2.get_package("persistent-pkg").await.unwrap();
            assert!(loaded.is_some());
            assert_eq!(loaded.unwrap().name, "persistent-pkg");
        }
    }

    #[tokio::test]
    async fn test_registry_with_dependencies() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        registry.load().await.unwrap();

        let mut package = create_test_package("dep-pkg", "1.0.0");
        package.versions[0].dependencies = vec![
            Dependency {
                name: "dep1".to_string(),
                version_req: "^1.0.0".to_string(),
                optional: false,
            },
            Dependency {
                name: "dep2".to_string(),
                version_req: ">=2.0.0".to_string(),
                optional: true,
            },
        ];

        registry.add_package(package).await.unwrap();

        let loaded = registry.get_package("dep-pkg").await.unwrap().unwrap();
        assert_eq!(loaded.versions[0].dependencies.len(), 2);
        assert_eq!(loaded.versions[0].dependencies[0].name, "dep1");
        assert!(!loaded.versions[0].dependencies[0].optional);
        assert_eq!(loaded.versions[0].dependencies[1].name, "dep2");
        assert!(loaded.versions[0].dependencies[1].optional);
    }

    #[tokio::test]
    async fn test_registry_load_corrupted_index_fails() {
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("index.json");

        // Write corrupted JSON to index file
        std::fs::write(&index_path, "{ invalid json }").unwrap();

        // DETERMINISTIC: Should FAIL, not silently fallback
        let registry = Registry::with_path(index_path);
        assert!(registry.load().await.is_err());
    }

    #[tokio::test]
    async fn test_registry_load_missing_index_fails() {
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("index.json");

        // DETERMINISTIC: Missing index should FAIL
        let registry = Registry::with_path(index_path);
        assert!(registry.load().await.is_err());
    }

    #[tokio::test]
    async fn test_registry_validate_empty_fails() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        // Create valid (but empty) registry
        let index = RegistryIndex::new();
        let mut guard = registry.index.write().unwrap();
        *guard = Some(index);
        drop(guard);

        // DETERMINISTIC: Empty registry FAILS validation
        assert!(registry.validate().await.is_err());
    }

    #[tokio::test]
    async fn test_registry_validate_success_with_valid_packages() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        // Create valid registry with packages
        let mut index = RegistryIndex::new();
        let package = create_test_package("test-pkg", "1.0.0");
        index.add_package(package);
        let mut guard = registry.index.write().unwrap();
        *guard = Some(index);
        drop(guard);

        // Should validate successfully
        assert!(registry.validate().await.is_ok());
    }

    #[tokio::test]
    async fn test_registry_validate_detects_missing_checksum() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::with_path(temp_dir.path().join("index.json"));

        // Create registry with invalid package (empty checksum)
        let mut index = RegistryIndex::new();
        let mut package = create_test_package("invalid-pkg", "1.0.0");
        package.versions[0].checksum = String::new();
        index.add_package(package);
        let mut guard = registry.index.write().unwrap();
        *guard = Some(index);
        drop(guard);

        // DETERMINISTIC: FAILS on missing checksum
        assert!(registry.validate().await.is_err());
    }
}
