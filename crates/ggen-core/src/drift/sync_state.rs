//! Sync State Tracking
//!
//! Stores SHA256 hashes and timestamps of ontology files, manifests,
//! and inference rules after each sync operation.

use chrono::{DateTime, Utc};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// State of a single file tracked for drift detection
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FileHashState {
    /// SHA256 hash of file content
    pub hash: String,

    /// Last modification timestamp (ISO 8601)
    pub timestamp: String,

    /// File size in bytes
    pub size_bytes: u64,
}

impl FileHashState {
    /// Create new file hash state
    pub fn new(hash: String, timestamp: DateTime<Utc>, size_bytes: u64) -> Self {
        Self {
            hash,
            timestamp: timestamp.to_rfc3339(),
            size_bytes,
        }
    }

    /// Get timestamp as DateTime
    pub fn timestamp_as_datetime(&self) -> Result<DateTime<Utc>> {
        DateTime::parse_from_rfc3339(&self.timestamp)
            .map(|dt| dt.with_timezone(&Utc))
            .map_err(|e| Error::new(&format!("Failed to parse timestamp: {}", e)))
    }
}

/// Complete sync state tracking all relevant files
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyncState {
    /// Version of the state file format
    pub version: String,

    /// When this state was created (ISO 8601)
    pub created_at: String,

    /// Main ontology file
    pub ontology: FileHashState,

    /// Manifest file (ggen.toml)
    pub manifest: FileHashState,

    /// Inference rules (tracked by rule name)
    #[serde(default)]
    pub inference_rules: HashMap<String, String>,

    /// Additional imported ontologies
    #[serde(default)]
    pub imports: HashMap<String, FileHashState>,

    /// Number of files synced in last run
    #[serde(default)]
    pub files_synced: usize,

    /// Duration of last sync in milliseconds
    #[serde(default)]
    pub sync_duration_ms: u64,
}

impl SyncState {
    /// Current version of the state file format
    pub const VERSION: &'static str = "1.0.0";

    /// Create new sync state
    pub fn new(ontology: FileHashState, manifest: FileHashState) -> Self {
        Self {
            version: Self::VERSION.to_string(),
            created_at: Utc::now().to_rfc3339(),
            ontology,
            manifest,
            inference_rules: HashMap::new(),
            imports: HashMap::new(),
            files_synced: 0,
            sync_duration_ms: 0,
        }
    }

    /// Load sync state from JSON file
    pub fn load(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path).map_err(|e| {
            Error::new(&format!(
                "Failed to read sync state from {}: {}",
                path.display(),
                e
            ))
        })?;

        serde_json::from_str(&content).map_err(|e| {
            Error::new(&format!(
                "Failed to parse sync state from {}: {}",
                path.display(),
                e
            ))
        })
    }

    /// Save sync state to JSON file
    pub fn save(&self, path: &Path) -> Result<()> {
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!(
                    "Failed to create directory {}: {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        let content = serde_json::to_string_pretty(self).map_err(|e| {
            Error::new(&format!("Failed to serialize sync state: {}", e))
        })?;

        fs::write(path, content).map_err(|e| {
            Error::new(&format!(
                "Failed to write sync state to {}: {}",
                path.display(),
                e
            ))
        })
    }

    /// Add inference rule hash
    pub fn add_inference_rule(&mut self, name: String, hash: String) {
        self.inference_rules.insert(name, hash);
    }

    /// Add imported ontology
    pub fn add_import(&mut self, path: String, state: FileHashState) {
        self.imports.insert(path, state);
    }

    /// Set sync metadata
    pub fn set_sync_metadata(&mut self, files_synced: usize, duration_ms: u64) {
        self.files_synced = files_synced;
        self.sync_duration_ms = duration_ms;
    }

    /// Get age since last sync
    pub fn age_since_sync(&self) -> Result<chrono::Duration> {
        let created = DateTime::parse_from_rfc3339(&self.created_at)
            .map(|dt| dt.with_timezone(&Utc))
            .map_err(|e| Error::new(&format!("Failed to parse created_at: {}", e)))?;

        Ok(Utc::now().signed_duration_since(created))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_file_hash_state_creation() {
        let timestamp = Utc::now();
        let state = FileHashState::new("abc123".to_string(), timestamp, 1024);

        assert_eq!(state.hash, "abc123");
        assert_eq!(state.size_bytes, 1024);
        assert!(state.timestamp_as_datetime().is_ok());
    }

    #[test]
    fn test_sync_state_creation() {
        let ontology = FileHashState::new("hash1".to_string(), Utc::now(), 100);
        let manifest = FileHashState::new("hash2".to_string(), Utc::now(), 200);

        let state = SyncState::new(ontology.clone(), manifest.clone());

        assert_eq!(state.version, SyncState::VERSION);
        assert_eq!(state.ontology.hash, "hash1");
        assert_eq!(state.manifest.hash, "hash2");
        assert!(state.inference_rules.is_empty());
        assert!(state.imports.is_empty());
    }

    #[test]
    fn test_sync_state_save_load() -> Result<()> {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::new(&format!("Failed to create temp dir: {}", e)))?;
        let state_path = temp_dir.path().join("sync-state.json");

        let ontology = FileHashState::new("hash1".to_string(), Utc::now(), 100);
        let manifest = FileHashState::new("hash2".to_string(), Utc::now(), 200);

        let mut state = SyncState::new(ontology, manifest);
        state.add_inference_rule("rule1".to_string(), "rulehash1".to_string());
        state.set_sync_metadata(5, 1234);

        // Save
        state.save(&state_path)?;

        // Load
        let loaded = SyncState::load(&state_path)?;

        assert_eq!(loaded.version, state.version);
        assert_eq!(loaded.ontology.hash, state.ontology.hash);
        assert_eq!(loaded.manifest.hash, state.manifest.hash);
        assert_eq!(loaded.inference_rules.len(), 1);
        assert_eq!(loaded.files_synced, 5);
        assert_eq!(loaded.sync_duration_ms, 1234);

        Ok(())
    }

    #[test]
    fn test_add_inference_rule() {
        let ontology = FileHashState::new("hash1".to_string(), Utc::now(), 100);
        let manifest = FileHashState::new("hash2".to_string(), Utc::now(), 200);

        let mut state = SyncState::new(ontology, manifest);
        state.add_inference_rule("rule1".to_string(), "hash1".to_string());
        state.add_inference_rule("rule2".to_string(), "hash2".to_string());

        assert_eq!(state.inference_rules.len(), 2);
        assert_eq!(state.inference_rules.get("rule1"), Some(&"hash1".to_string()));
    }

    #[test]
    fn test_add_import() {
        let ontology = FileHashState::new("hash1".to_string(), Utc::now(), 100);
        let manifest = FileHashState::new("hash2".to_string(), Utc::now(), 200);

        let mut state = SyncState::new(ontology, manifest);
        let import_state = FileHashState::new("import_hash".to_string(), Utc::now(), 300);
        state.add_import("imports/schema.ttl".to_string(), import_state);

        assert_eq!(state.imports.len(), 1);
        assert!(state.imports.contains_key("imports/schema.ttl"));
    }
}
