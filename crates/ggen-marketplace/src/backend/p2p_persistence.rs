//! State persistence for P2P registry
//!
//! Provides atomic, versioned persistence of P2P state including:
//! - Known peer list with multiaddrs
//! - Peer reputation data
//! - DHT routing table state
//! - Local package cache
//! - Bootstrap node list

use crate::error::MarketplaceError;
use chrono::{DateTime, Utc};
use libp2p::{Multiaddr, PeerId};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::sync::RwLock;

type Result<T> = std::result::Result<T, MarketplaceError>;

/// Current state file format version
const STATE_VERSION: u32 = 1;

/// Default state file location
const DEFAULT_STATE_FILE: &str = ".ggen/p2p-state.json";

/// Auto-save interval (60 seconds)
const AUTO_SAVE_INTERVAL: Duration = Duration::from_secs(60);

/// Serializable peer information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedPeer {
    pub peer_id: String,
    pub addresses: Vec<String>,
    pub last_seen: DateTime<Utc>,
}

/// Serializable peer reputation data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedReputation {
    pub peer_id: String,
    pub successful_retrievals: u64,
    pub failed_retrievals: u64,
    pub last_seen: DateTime<Utc>,
    pub avg_response_time_ms: u64,
    pub location: Option<SerializedGeoLocation>,
    pub packages_provided: usize,
}

/// Serializable geographic location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedGeoLocation {
    pub latitude: f64,
    pub longitude: f64,
    pub region: Option<String>,
}

/// Serializable DHT routing table entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedDHTEntry {
    pub key: String,
    pub value: Vec<u8>,
    pub stored_at: DateTime<Utc>,
}

/// Serializable package cache entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedCacheEntry {
    pub package_id: String,
    pub package_data: Vec<u8>,
    pub cached_at: DateTime<Utc>,
}

/// Top-level state structure (versioned)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct P2PState {
    /// State format version for migration
    pub version: u32,
    /// Known peers with their multiaddrs
    pub peers: Vec<SerializedPeer>,
    /// Peer reputation data
    pub reputation: Vec<SerializedReputation>,
    /// DHT routing table entries
    pub dht_entries: Vec<SerializedDHTEntry>,
    /// Local package cache
    pub package_cache: Vec<SerializedCacheEntry>,
    /// Bootstrap node addresses
    pub bootstrap_nodes: Vec<String>,
    /// Timestamp of last save
    pub saved_at: DateTime<Utc>,
}

impl Default for P2PState {
    fn default() -> Self {
        Self {
            version: STATE_VERSION,
            peers: Vec::new(),
            reputation: Vec::new(),
            dht_entries: Vec::new(),
            package_cache: Vec::new(),
            bootstrap_nodes: Vec::new(),
            saved_at: Utc::now(),
        }
    }
}

/// P2P state persistence manager
pub struct P2PStatePersistence {
    /// Path to state file
    state_file: PathBuf,
    /// In-memory state cache
    state: RwLock<P2PState>,
    /// Auto-save task handle
    auto_save_handle: RwLock<Option<tokio::task::JoinHandle<()>>>,
}

impl P2PStatePersistence {
    /// Create a new persistence manager with default location
    pub fn new() -> Result<Self> {
        let state_file = Self::get_default_state_path()?;
        Self::new_with_path(state_file)
    }

    /// Create a new persistence manager with custom path
    pub fn new_with_path(state_file: PathBuf) -> Result<Self> {
        // Ensure parent directory exists
        if let Some(parent) = state_file.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                MarketplaceError::persistence_error(format!("Failed to create state directory: {}", e))
            })?;
        }

        Ok(Self {
            state_file,
            state: RwLock::new(P2PState::default()),
            auto_save_handle: RwLock::new(None),
        })
    }

    /// Get the default state file path (~/.ggen/p2p-state.json)
    fn get_default_state_path() -> Result<PathBuf> {
        let home = std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")).map_err(|_| {
            MarketplaceError::persistence_error("Cannot determine home directory")
        })?;

        Ok(PathBuf::from(home).join(DEFAULT_STATE_FILE))
    }

    /// Load state from disk (handles missing file gracefully)
    pub async fn load_state(&self) -> Result<P2PState> {
        // If file doesn't exist, return default state (first run)
        if !self.state_file.exists() {
            tracing::info!("No state file found at {:?}, starting fresh", self.state_file);
            return Ok(P2PState::default());
        }

        // Acquire file lock for reading
        let file = self.open_with_lock(false)?;
        let reader = BufReader::new(file);

        // Deserialize state
        let state: P2PState = serde_json::from_reader(reader).map_err(|e| {
            MarketplaceError::persistence_error(format!("Failed to deserialize state: {}", e))
        })?;

        // Validate version and migrate if needed
        let migrated_state = self.migrate_state(state)?;

        // Update in-memory cache
        *self.state.write().await = migrated_state.clone();

        tracing::info!(
            "Loaded P2P state: {} peers, {} reputation entries",
            migrated_state.peers.len(),
            migrated_state.reputation.len()
        );

        Ok(migrated_state)
    }

    /// Save state to disk atomically
    pub async fn save_state(&self) -> Result<()> {
        let state = self.state.read().await;

        // Update saved_at timestamp
        let mut state_to_save = state.clone();
        state_to_save.saved_at = Utc::now();

        // Write to temporary file first (atomic write pattern)
        let temp_file = self.state_file.with_extension("tmp");

        // Acquire file lock for writing
        let file = self.create_with_lock(&temp_file)?;
        let mut writer = BufWriter::new(file);

        // Serialize state to JSON (human-readable)
        serde_json::to_writer_pretty(&mut writer, &state_to_save).map_err(|e| {
            MarketplaceError::persistence_error(format!("Failed to serialize state: {}", e))
        })?;

        // Ensure all data is written
        writer.flush().map_err(|e| {
            MarketplaceError::persistence_error(format!("Failed to flush state file: {}", e))
        })?;

        // Atomic rename (replaces old file)
        std::fs::rename(&temp_file, &self.state_file).map_err(|e| {
            MarketplaceError::persistence_error(format!("Failed to rename state file: {}", e))
        })?;

        tracing::debug!("Saved P2P state to {:?}", self.state_file);

        Ok(())
    }

    /// Update in-memory state (call before save_state)
    pub async fn update_state(&self, new_state: P2PState) {
        *self.state.write().await = new_state;
    }

    /// Get current in-memory state
    pub async fn get_state(&self) -> P2PState {
        self.state.read().await.clone()
    }

    /// Start periodic auto-save task
    pub async fn start_auto_save(&self) -> Result<()> {
        // Cancel existing auto-save task if any
        self.stop_auto_save().await;

        let state_file = self.state_file.clone();
        let state = self.state.clone();

        let handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(AUTO_SAVE_INTERVAL);

            loop {
                interval.tick().await;

                // Create a temporary persistence instance for saving
                if let Ok(persistence) = P2PStatePersistence::new_with_path(state_file.clone()) {
                    let current_state = state.read().await.clone();
                    persistence.update_state(current_state).await;

                    if let Err(e) = persistence.save_state().await {
                        tracing::error!("Auto-save failed: {}", e);
                    } else {
                        tracing::trace!("Auto-saved P2P state");
                    }
                }
            }
        });

        *self.auto_save_handle.write().await = Some(handle);

        tracing::info!("Started auto-save task (interval: {:?})", AUTO_SAVE_INTERVAL);

        Ok(())
    }

    /// Stop periodic auto-save task
    pub async fn stop_auto_save(&self) {
        if let Some(handle) = self.auto_save_handle.write().await.take() {
            handle.abort();
            tracing::info!("Stopped auto-save task");
        }
    }

    /// Persist state on graceful shutdown
    pub async fn persist_on_shutdown(&self) -> Result<()> {
        // Stop auto-save task
        self.stop_auto_save().await;

        // Save current state
        self.save_state().await?;

        tracing::info!("Persisted P2P state on shutdown");

        Ok(())
    }

    /// Open file with advisory lock (shared for read, exclusive for write)
    #[cfg(unix)]
    fn open_with_lock(&self, exclusive: bool) -> Result<File> {
        use std::os::unix::fs::OpenOptionsExt;

        let file = OpenOptions::new()
            .read(true)
            .open(&self.state_file)
            .map_err(|e| {
                MarketplaceError::persistence_error(format!("Failed to open state file: {}", e))
            })?;

        // Try to acquire advisory lock (non-blocking)
        // Note: This is a best-effort lock, not enforced by the OS
        let lock_result = if exclusive {
            nix::fcntl::flock(file.as_raw_fd(), nix::fcntl::FlockArg::LockExclusiveNonblock)
        } else {
            nix::fcntl::flock(file.as_raw_fd(), nix::fcntl::FlockArg::LockSharedNonblock)
        };

        if lock_result.is_err() {
            tracing::warn!("Could not acquire file lock (file may be in use)");
        }

        Ok(file)
    }

    #[cfg(not(unix))]
    fn open_with_lock(&self, _exclusive: bool) -> Result<File> {
        // Windows and other platforms: no file locking for now
        OpenOptions::new()
            .read(true)
            .open(&self.state_file)
            .map_err(|e| {
                MarketplaceError::persistence_error(format!("Failed to open state file: {}", e))
            })
    }

    /// Create file with advisory lock (exclusive)
    #[cfg(unix)]
    fn create_with_lock(&self, path: &Path) -> Result<File> {
        use std::os::unix::fs::OpenOptionsExt;

        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| {
                MarketplaceError::persistence_error(format!("Failed to create state file: {}", e))
            })?;

        // Try to acquire exclusive advisory lock
        let lock_result = nix::fcntl::flock(
            file.as_raw_fd(),
            nix::fcntl::FlockArg::LockExclusiveNonblock,
        );

        if lock_result.is_err() {
            tracing::warn!("Could not acquire exclusive file lock");
        }

        Ok(file)
    }

    #[cfg(not(unix))]
    fn create_with_lock(&self, path: &Path) -> Result<File> {
        // Windows and other platforms: no file locking for now
        OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| {
                MarketplaceError::persistence_error(format!("Failed to create state file: {}", e))
            })
    }

    /// Migrate state from older versions
    fn migrate_state(&self, state: P2PState) -> Result<P2PState> {
        if state.version == STATE_VERSION {
            // Already current version
            return Ok(state);
        }

        // Future migrations would go here
        // For now, just update version
        let mut migrated = state;
        migrated.version = STATE_VERSION;

        tracing::info!("Migrated state to version {}", STATE_VERSION);

        Ok(migrated)
    }
}

/// Helper functions for converting between runtime and serialized types

/// Convert PeerId to string
pub fn peer_id_to_string(peer_id: &PeerId) -> String {
    peer_id.to_base58()
}

/// Convert string to PeerId
pub fn string_to_peer_id(s: &str) -> Result<PeerId> {
    PeerId::from_bytes(
        &bs58::decode(s)
            .into_vec()
            .map_err(|e| MarketplaceError::persistence_error(format!("Invalid peer ID: {}", e)))?,
    )
    .map_err(|e| MarketplaceError::persistence_error(format!("Invalid peer ID format: {}", e)))
}

/// Convert Multiaddr to string
pub fn multiaddr_to_string(addr: &Multiaddr) -> String {
    addr.to_string()
}

/// Convert string to Multiaddr
pub fn string_to_multiaddr(s: &str) -> Result<Multiaddr> {
    s.parse()
        .map_err(|e| MarketplaceError::persistence_error(format!("Invalid multiaddr: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_persistence_new() {
        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("p2p-state.json");

        let persistence = P2PStatePersistence::new_with_path(state_file.clone()).unwrap();

        // Parent directory should be created
        assert!(state_file.parent().unwrap().exists());
    }

    #[tokio::test]
    async fn test_load_missing_file() {
        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("nonexistent.json");

        let persistence = P2PStatePersistence::new_with_path(state_file).unwrap();

        // Should return default state without error
        let state = persistence.load_state().await.unwrap();
        assert_eq!(state.version, STATE_VERSION);
        assert_eq!(state.peers.len(), 0);
    }

    #[tokio::test]
    async fn test_save_and_load_state() {
        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("p2p-state.json");

        let persistence = P2PStatePersistence::new_with_path(state_file.clone()).unwrap();

        // Create test state
        let mut state = P2PState::default();
        state.peers.push(SerializedPeer {
            peer_id: "12D3KooWTest".to_string(),
            addresses: vec!["/ip4/127.0.0.1/tcp/4001".to_string()],
            last_seen: Utc::now(),
        });
        state.reputation.push(SerializedReputation {
            peer_id: "12D3KooWTest".to_string(),
            successful_retrievals: 10,
            failed_retrievals: 2,
            last_seen: Utc::now(),
            avg_response_time_ms: 150,
            location: None,
            packages_provided: 5,
        });

        // Save state
        persistence.update_state(state.clone()).await;
        persistence.save_state().await.unwrap();

        // Verify file exists
        assert!(state_file.exists());

        // Load state
        let loaded_state = persistence.load_state().await.unwrap();

        // Verify data
        assert_eq!(loaded_state.version, STATE_VERSION);
        assert_eq!(loaded_state.peers.len(), 1);
        assert_eq!(loaded_state.peers[0].peer_id, "12D3KooWTest");
        assert_eq!(loaded_state.reputation.len(), 1);
        assert_eq!(loaded_state.reputation[0].successful_retrievals, 10);
    }

    #[tokio::test]
    async fn test_atomic_write() {
        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("p2p-state.json");

        let persistence = P2PStatePersistence::new_with_path(state_file.clone()).unwrap();

        // Create test state
        let state = P2PState::default();
        persistence.update_state(state).await;

        // Save multiple times (should not corrupt file)
        for _ in 0..5 {
            persistence.save_state().await.unwrap();
        }

        // Load should still work
        let loaded_state = persistence.load_state().await.unwrap();
        assert_eq!(loaded_state.version, STATE_VERSION);
    }

    #[tokio::test]
    async fn test_peer_id_conversion() {
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(keypair.public());

        let serialized = peer_id_to_string(&peer_id);
        let deserialized = string_to_peer_id(&serialized).unwrap();

        assert_eq!(peer_id, deserialized);
    }

    #[tokio::test]
    async fn test_multiaddr_conversion() {
        let addr: Multiaddr = "/ip4/127.0.0.1/tcp/4001".parse().unwrap();

        let serialized = multiaddr_to_string(&addr);
        let deserialized = string_to_multiaddr(&serialized).unwrap();

        assert_eq!(addr, deserialized);
    }
}
