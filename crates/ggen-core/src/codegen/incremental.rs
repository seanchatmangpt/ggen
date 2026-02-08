//! # Incremental Code Generator with SHA256-based JIT (Phase 4)
//!
//! This module implements incremental code generation that skips unchanged files
//! using SHA256 content hashing and specification hash comparison.
//!
//! **Performance Target**: Skip ~70-80% of files when spec unchanged
//! **SLO**: <60s for 10+ specs with incremental generation

use ggen_utils::error::{Error, Result};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

/// Represents results of content hash computation
#[derive(Debug, Clone)]
pub struct ContentHash {
    /// SHA256 hash of file content
    pub hash: String,
    /// File path that was hashed
    pub file_path: PathBuf,
}

impl ContentHash {
    /// Create new content hash
    pub fn new(hash: String, file_path: PathBuf) -> Self {
        Self { hash, file_path }
    }
}

/// Represents state of a generated file for incremental checking
#[derive(Debug, Clone)]
pub struct FileState {
    /// SHA256 hash of file content
    pub content_hash: String,
    /// SHA256 hash of the specification that generated this file
    pub spec_hash: String,
    /// File path
    pub file_path: PathBuf,
    /// Modification timestamp
    pub timestamp: u64,
}

impl FileState {
    /// Create new file state
    pub fn new(
        content_hash: String, spec_hash: String, file_path: PathBuf, timestamp: u64,
    ) -> Self {
        Self {
            content_hash,
            spec_hash,
            file_path,
            timestamp,
        }
    }

    /// Check if file needs regeneration
    pub fn needs_regeneration(&self, current_spec_hash: &str) -> bool {
        self.spec_hash != current_spec_hash
    }
}

/// Cache for file states keyed by file path
#[derive(Debug, Clone, Default)]
pub struct IncrementalCache {
    /// Map of file paths to their states
    states: HashMap<PathBuf, FileState>,
    /// Total files in cache
    cache_size: usize,
}

impl IncrementalCache {
    /// Create new empty cache
    pub fn new() -> Self {
        Self {
            states: HashMap::new(),
            cache_size: 0,
        }
    }

    /// Get file state from cache
    pub fn get(&self, path: &Path) -> Option<&FileState> {
        self.states.get(path)
    }

    /// Insert file state into cache
    pub fn insert(&mut self, path: PathBuf, state: FileState) {
        self.states.insert(path, state);
        self.cache_size = self.states.len();
    }

    /// Check if file is in cache
    pub fn contains(&self, path: &Path) -> bool {
        self.states.contains_key(path)
    }

    /// Get cache size (number of files)
    pub fn size(&self) -> usize {
        self.cache_size
    }

    /// Clear entire cache
    pub fn clear(&mut self) {
        self.states.clear();
        self.cache_size = 0;
    }
}

/// Configuration for incremental generation
#[derive(Debug, Clone)]
pub struct IncrementalConfig {
    /// Use caching (can be disabled for force regeneration)
    pub use_cache: bool,
    /// Maximum cache size in MB (0 = unlimited)
    pub max_cache_size_mb: u64,
}

impl Default for IncrementalConfig {
    fn default() -> Self {
        Self {
            use_cache: true,
            max_cache_size_mb: 0, // Unlimited
        }
    }
}

/// Results of incremental generation
#[derive(Debug, Clone)]
pub struct IncrementalResult {
    /// Total files requested
    pub total_files: usize,
    /// Files that were regenerated
    pub regenerated_files: usize,
    /// Files that were skipped (already up-to-date)
    pub skipped_files: usize,
    /// Skip percentage (skipped / total * 100)
    pub skip_percentage: f64,
    /// Files that errored
    pub error_files: usize,
}

impl IncrementalResult {
    /// Create new result
    pub fn new(
        total_files: usize, regenerated_files: usize, skipped_files: usize, error_files: usize,
    ) -> Self {
        let skip_percentage = if total_files > 0 {
            (skipped_files as f64 / total_files as f64) * 100.0
        } else {
            0.0
        };

        Self {
            total_files,
            regenerated_files,
            skipped_files,
            error_files,
            skip_percentage,
        }
    }

    /// Check if all files passed (no errors)
    pub fn all_passed(&self) -> bool {
        self.error_files == 0
    }
}

/// Incremental code generator with SHA256-based JIT
pub struct IncrementalGenerator {
    /// In-memory cache of file states
    cache: IncrementalCache,
    /// Configuration
    config: IncrementalConfig,
}

impl IncrementalGenerator {
    /// Create new incremental generator
    pub fn new(config: IncrementalConfig) -> Self {
        Self {
            cache: IncrementalCache::new(),
            config,
        }
    }

    /// Create with default configuration
    pub fn with_default_config() -> Self {
        Self::new(IncrementalConfig::default())
    }

    /// Compute SHA256 hash of file content
    pub fn compute_content_hash(file_path: &Path) -> Result<String> {
        let mut file = fs::File::open(file_path).map_err(|e| {
            Error::io_error(format!(
                "Failed to open file '{}': {}",
                file_path.display(),
                e
            ))
        })?;

        let mut hasher = Sha256::new();
        let mut buffer = [0; 8192]; // 8KB buffer for streaming

        loop {
            let bytes_read = file.read(&mut buffer).map_err(|e| {
                Error::io_error(format!(
                    "Failed to read file '{}': {}",
                    file_path.display(),
                    e
                ))
            })?;

            if bytes_read == 0 {
                break;
            }

            hasher.update(&buffer[..bytes_read]);
        }

        let hash = hasher.finalize();
        Ok(format!("{:x}", hash))
    }

    /// Compute SHA256 hash of string (e.g., specification)
    pub fn compute_string_hash(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        let hash = hasher.finalize();
        format!("{:x}", hash)
    }

    /// Check if file should be regenerated
    pub fn should_regenerate(&self, file_path: &Path, current_spec_hash: &str) -> Result<bool> {
        if !self.config.use_cache {
            // Force regeneration if caching disabled
            return Ok(true);
        }

        // If file not in cache, regenerate
        let Some(cached_state) = self.cache.get(file_path) else {
            return Ok(true);
        };

        // If spec hash changed, regenerate
        if cached_state.needs_regeneration(current_spec_hash) {
            return Ok(true);
        }

        // Check if file still exists and content matches cache
        if !file_path.exists() {
            return Ok(true); // File deleted, regenerate
        }

        let current_content_hash = Self::compute_content_hash(file_path)?;
        Ok(current_content_hash != cached_state.content_hash)
    }

    /// Store generated file state in cache
    pub fn store_file_state(&mut self, file_path: PathBuf, spec_hash: &str) -> Result<()> {
        let content_hash = Self::compute_content_hash(&file_path)?;
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|e| Error::io_error(format!("Failed to get timestamp: {}", e)))?
            .as_secs();

        let file_state = FileState::new(
            content_hash,
            spec_hash.to_string(),
            file_path.clone(),
            timestamp,
        );

        self.cache.insert(file_path, file_state);
        Ok(())
    }

    /// Process a batch of files with incremental checking
    /// Returns which files need regeneration
    pub fn check_files(&self, files: &[PathBuf], spec_hash: &str) -> Result<Vec<bool>> {
        let mut results = Vec::with_capacity(files.len());

        for file_path in files {
            let needs_regen = self.should_regenerate(file_path, spec_hash)?;
            results.push(needs_regen);
        }

        Ok(results)
    }

    /// Get cache size (number of cached files)
    pub fn cache_size(&self) -> usize {
        self.cache.size()
    }

    /// Clear the cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Get reference to cache (read-only)
    pub fn get_cache(&self) -> &IncrementalCache {
        &self.cache
    }
}

impl Default for IncrementalGenerator {
    fn default() -> Self {
        Self {
            cache: IncrementalCache::default(),
            config: IncrementalConfig::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_compute_content_hash() -> Result<()> {
        let dir = tempdir().map_err(|e| Error::io_error(format!("{}", e)))?;
        let file_path = dir.path().join("test.txt");

        fs::write(&file_path, b"hello world").map_err(|e| Error::io_error(format!("{}", e)))?;

        let hash1 = IncrementalGenerator::compute_content_hash(&file_path)?;
        let hash2 = IncrementalGenerator::compute_content_hash(&file_path)?;

        // Same content should produce same hash (deterministic)
        assert_eq!(hash1, hash2);
        assert!(!hash1.is_empty());

        Ok(())
    }

    #[test]
    fn test_compute_string_hash() {
        let hash1 = IncrementalGenerator::compute_string_hash("test content");
        let hash2 = IncrementalGenerator::compute_string_hash("test content");
        let hash3 = IncrementalGenerator::compute_string_hash("different content");

        // Same content produces same hash
        assert_eq!(hash1, hash2);
        // Different content produces different hash
        assert_ne!(hash1, hash3);
    }

    #[test]
    fn test_incremental_cache_operations() -> Result<()> {
        let mut cache = IncrementalCache::new();
        let path = PathBuf::from("test.rs");

        assert!(!cache.contains(&path));
        assert_eq!(cache.size(), 0);

        let state = FileState::new(
            "abc123".to_string(),
            "def456".to_string(),
            path.clone(),
            12345,
        );

        cache.insert(path.clone(), state);
        assert!(cache.contains(&path));
        assert_eq!(cache.size(), 1);

        let retrieved = cache.get(&path);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().content_hash, "abc123");

        cache.clear();
        assert!(!cache.contains(&path));
        assert_eq!(cache.size(), 0);

        Ok(())
    }

    #[test]
    fn test_should_regenerate_force_mode() -> Result<()> {
        let config = IncrementalConfig {
            use_cache: false, // Force regeneration
            max_cache_size_mb: 0,
        };
        let gen = IncrementalGenerator::new(config);

        let dir = tempdir().map_err(|e| Error::io_error(format!("{}", e)))?;
        let file_path = dir.path().join("test.rs");
        fs::write(&file_path, b"code").map_err(|e| Error::io_error(format!("{}", e)))?;

        // Force mode always regenerates
        assert!(gen.should_regenerate(&file_path, "any_hash")?);

        Ok(())
    }

    #[test]
    fn test_incremental_result_percentage() {
        let result = IncrementalResult::new(100, 20, 80, 0);
        assert_eq!(result.skip_percentage, 80.0);
        assert!(result.all_passed());

        let result = IncrementalResult::new(100, 100, 0, 5);
        assert_eq!(result.skip_percentage, 0.0);
        assert!(!result.all_passed());

        let result = IncrementalResult::new(0, 0, 0, 0);
        assert_eq!(result.skip_percentage, 0.0);
        assert!(result.all_passed());
    }

    #[test]
    fn test_file_state_needs_regeneration() {
        let state = FileState::new(
            "content123".to_string(),
            "spec456".to_string(),
            PathBuf::from("test.rs"),
            12345,
        );

        // Same spec hash → no regeneration needed
        assert!(!state.needs_regeneration("spec456"));

        // Different spec hash → regeneration needed
        assert!(state.needs_regeneration("spec789"));
    }
}
