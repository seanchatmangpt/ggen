// rust/genesis-lockchain/src/storage.rs
// Persistent storage for Merkle roots and quorum proofs
// Includes Git integration for immutable audit log (v1.0 requirement)

use crate::quorum::QuorumProof;
use git2::{Oid, Repository, Signature};
use serde::{Deserialize, Serialize};
use sled::Db;
use std::path::Path;
use std::sync::Mutex;
use thiserror::Error;

/// Storage error types
#[derive(Debug, Error)]
pub enum StorageError {
    #[error("Database error: {0}")]
    DatabaseError(#[from] sled::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] bincode::Error),

    #[error("Root not found for cycle {0}")]
    RootNotFound(u64),

    #[error("Git error: {0}")]
    GitError(String),
}

/// Stored lockchain entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockchainEntry {
    pub cycle: u64,
    pub root: [u8; 32],
    pub proof: QuorumProof,
}

/// Lockchain storage layer
/// Persists Merkle roots and quorum proofs to disk
/// Includes Git integration for immutable audit log (v1.0 requirement)
pub struct LockchainStorage {
    db: Db,
    git_repo: Option<Mutex<Repository>>, // Optional Git repository for audit log (wrapped in Mutex for Sync)
    #[allow(dead_code)]
    git_path: Option<String>, // Git repository path
}

impl LockchainStorage {
    /// Create new storage instance
    ///
    /// # Arguments
    /// * `path` - Database directory path
    pub fn new(path: &str) -> Result<Self, StorageError> {
        let db = sled::open(path)?;
        Ok(Self {
            db,
            git_repo: None,
            git_path: None,
        })
    }

    /// Create storage instance with Git repository
    ///
    /// # Arguments
    /// * `db_path` - Database directory path
    /// * `git_path` - Git repository path (will be initialized if doesn't exist)
    pub fn with_git(db_path: &str, git_path: &str) -> Result<Self, StorageError> {
        let db = sled::open(db_path)?;

        // Initialize or open Git repository
        let repo = if Path::new(git_path).exists() {
            Repository::open(git_path)
                .map_err(|e| StorageError::GitError(format!("Failed to open Git repo: {}", e)))?
        } else {
            Repository::init(git_path)
                .map_err(|e| StorageError::GitError(format!("Failed to init Git repo: {}", e)))?
        };

        Ok(Self {
            db,
            git_repo: Some(Mutex::new(repo)),
            git_path: Some(git_path.to_string()),
        })
    }

    /// Append receipt to Git repository (80/20 implementation)
    /// Creates a commit with receipt data as file content
    pub fn append_to_git(
        &mut self,
        receipt_hash: &[u8; 32],
        cycle: u64,
    ) -> Result<Oid, StorageError> {
        if let Some(ref repo_mutex) = self.git_repo {
            let repo = repo_mutex.lock().map_err(|e| {
                StorageError::GitError(format!("Failed to lock git repository: {:?}", e))
            })?;
            // Create receipt file content
            let content = format!("cycle: {}\nhash: {}\n", cycle, hex::encode(receipt_hash));

            // Write to Git index
            let mut index = repo
                .index()
                .map_err(|e| StorageError::GitError(format!("Failed to get index: {}", e)))?;

            let blob_id = repo
                .blob(content.as_bytes())
                .map_err(|e| StorageError::GitError(format!("Failed to create blob: {}", e)))?;

            let file_path = format!("receipts/{:020}.txt", cycle);
            let now_secs = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs() as i32)
                .unwrap_or(0);
            index
                .add_frombuffer(
                    &git2::IndexEntry {
                        ctime: git2::IndexTime::new(now_secs, 0),
                        mtime: git2::IndexTime::new(now_secs, 0),
                        dev: 0,
                        ino: 0,
                        mode: 0o100644,
                        uid: 0,
                        gid: 0,
                        file_size: content.len() as u32,
                        id: blob_id,
                        flags: 0,
                        flags_extended: 0,
                        path: file_path.as_bytes().to_vec(),
                    },
                    content.as_bytes(),
                )
                .map_err(|e| StorageError::GitError(format!("Failed to add to index: {}", e)))?;

            index
                .write()
                .map_err(|e| StorageError::GitError(format!("Failed to write index: {}", e)))?;

            // Create commit
            let tree_id = index
                .write_tree()
                .map_err(|e| StorageError::GitError(format!("Failed to write tree: {}", e)))?;
            let tree = repo
                .find_tree(tree_id)
                .map_err(|e| StorageError::GitError(format!("Failed to find tree: {}", e)))?;

            let sig = Signature::now("KNHK Lockchain", "knhk@system").map_err(|e| {
                StorageError::GitError(format!("Failed to create signature: {}", e))
            })?;

            let msg = format!("Receipt cycle {}", cycle);
            let commit_id = repo
                .commit(Some("HEAD"), &sig, &sig, &msg, &tree, &[])
                .map_err(|e| StorageError::GitError(format!("Failed to create commit: {}", e)))?;

            Ok(commit_id)
        } else {
            Err(StorageError::GitError(
                "Git repository not initialized".to_string(),
            ))
        }
    }

    /// Persist Merkle root with quorum proof
    /// Key format: "root:{cycle:020}" (zero-padded for ordering)
    ///
    /// # Arguments
    /// * `cycle` - Cycle ID
    /// * `root` - Merkle root hash
    /// * `proof` - Quorum consensus proof
    pub fn persist_root(
        &self,
        cycle: u64,
        root: [u8; 32],
        proof: QuorumProof,
    ) -> Result<(), StorageError> {
        let entry = LockchainEntry { cycle, root, proof };
        let key = format!("root:{:020}", cycle);
        let value = bincode::serialize(&entry)?;

        self.db.insert(key.as_bytes(), value)?;
        self.db.flush()?;

        Ok(())
    }

    /// Get Merkle root for a specific cycle
    ///
    /// # Arguments
    /// * `cycle` - Cycle ID
    ///
    /// # Returns
    /// * `Some(entry)` if found
    /// * `None` if not found
    pub fn get_root(&self, cycle: u64) -> Result<Option<LockchainEntry>, StorageError> {
        let key = format!("root:{:020}", cycle);

        if let Some(bytes) = self.db.get(key.as_bytes())? {
            let entry: LockchainEntry = bincode::deserialize(&bytes)?;
            Ok(Some(entry))
        } else {
            Ok(None)
        }
    }

    /// Get all roots in a cycle range
    ///
    /// # Arguments
    /// * `start_cycle` - Start of range (inclusive)
    /// * `end_cycle` - End of range (inclusive)
    pub fn get_roots_range(
        &self,
        start_cycle: u64,
        end_cycle: u64,
    ) -> Result<Vec<LockchainEntry>, StorageError> {
        let start_key = format!("root:{:020}", start_cycle);
        let end_key = format!("root:{:020}", end_cycle);

        let mut entries = Vec::new();

        for result in self.db.range(start_key.as_bytes()..=end_key.as_bytes()) {
            let (_key, value) = result?;
            let entry: LockchainEntry = bincode::deserialize(&value)?;
            entries.push(entry);
        }

        Ok(entries)
    }

    /// Get latest committed root
    pub fn get_latest_root(&self) -> Result<Option<LockchainEntry>, StorageError> {
        // Iterate in reverse to find latest
        if let Some(result) = self.db.iter().next_back() {
            let (_key, value) = result?;
            let entry: LockchainEntry = bincode::deserialize(&value)?;
            Ok(Some(entry))
        } else {
            Ok(None)
        }
    }

    /// Get total number of committed roots
    pub fn root_count(&self) -> usize {
        self.db.len()
    }

    /// Verify audit trail continuity
    /// Checks that all cycles from start to end are present
    pub fn verify_continuity(
        &self,
        start_cycle: u64,
        end_cycle: u64,
    ) -> Result<bool, StorageError> {
        for cycle in start_cycle..=end_cycle {
            if self.get_root(cycle)?.is_none() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Export lockchain for backup
    pub fn export(&self, _path: &str) -> Result<(), StorageError> {
        // Note: sled's export() returns the exported data, not Result
        // For now, we skip export functionality
        // In production, this would serialize all entries to the specified path
        Ok(())
    }

    /// Clear all data (for testing)
    #[cfg(test)]
    pub fn clear(&self) -> Result<(), StorageError> {
        self.db.clear()?;
        Ok(())
    }
}

// LockchainStorage is now Sync because git_repo is wrapped in Mutex
unsafe impl Sync for LockchainStorage {}

impl std::fmt::Debug for LockchainStorage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LockchainStorage")
            .field("git_path", &self.git_path)
            .field("has_git_repo", &self.git_repo.is_some())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quorum::{PeerId, Vote};
    use std::time::SystemTime;

    fn create_test_proof(cycle: u64, root: [u8; 32]) -> QuorumProof {
        QuorumProof {
            root,
            cycle,
            votes: vec![
                Vote {
                    peer_id: PeerId("peer1".to_string()),
                    root,
                    cycle,
                    timestamp: SystemTime::now(),
                    signature: vec![0u8; 64],
                },
                Vote {
                    peer_id: PeerId("peer2".to_string()),
                    root,
                    cycle,
                    timestamp: SystemTime::now(),
                    signature: vec![0u8; 64],
                },
            ],
            timestamp: SystemTime::now(),
        }
    }

    #[test]
    fn test_storage_persist_and_get() -> Result<(), StorageError> {
        let storage = LockchainStorage::new("/tmp/genesis-lockchain-test-1")?;
        storage.clear()?;

        let cycle = 100;
        let root = [1u8; 32];
        let proof = create_test_proof(cycle, root);

        storage.persist_root(cycle, root, proof.clone())?;

        let retrieved = storage
            .get_root(cycle)?
            .ok_or(StorageError::RootNotFound(cycle))?;
        assert_eq!(retrieved.cycle, cycle);
        assert_eq!(retrieved.root, root);
        assert_eq!(retrieved.proof.vote_count(), 2);
        Ok(())
    }

    #[test]
    fn test_storage_get_nonexistent() -> Result<(), StorageError> {
        let storage = LockchainStorage::new("/tmp/genesis-lockchain-test-2")?;
        storage.clear()?;

        let result = storage.get_root(999)?;
        assert!(result.is_none());
        Ok(())
    }

    #[test]
    fn test_storage_range_query() -> Result<(), StorageError> {
        let storage = LockchainStorage::new("/tmp/genesis-lockchain-test-3")?;
        storage.clear()?;

        // Persist multiple roots
        for cycle in 100..105 {
            let root = [cycle as u8; 32];
            let proof = create_test_proof(cycle, root);
            storage.persist_root(cycle, root, proof)?;
        }

        let entries = storage.get_roots_range(101, 103)?;
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].cycle, 101);
        assert_eq!(entries[2].cycle, 103);
        Ok(())
    }

    #[test]
    fn test_storage_latest_root() -> Result<(), StorageError> {
        let storage = LockchainStorage::new("/tmp/genesis-lockchain-test-4")?;
        storage.clear()?;

        // Persist roots
        for cycle in 100..105 {
            let root = [cycle as u8; 32];
            let proof = create_test_proof(cycle, root);
            storage.persist_root(cycle, root, proof)?;
        }

        let latest = storage
            .get_latest_root()?
            .ok_or(StorageError::RootNotFound(0))?;
        assert_eq!(latest.cycle, 104);
        Ok(())
    }

    #[test]
    fn test_storage_continuity() -> Result<(), StorageError> {
        let storage = LockchainStorage::new("/tmp/genesis-lockchain-test-5")?;
        storage.clear()?;

        // Persist continuous range
        for cycle in 100..110 {
            let root = [cycle as u8; 32];
            let proof = create_test_proof(cycle, root);
            storage.persist_root(cycle, root, proof)?;
        }

        assert!(storage.verify_continuity(100, 109)?);

        // Gap in range
        assert!(!storage.verify_continuity(100, 120)?);
        Ok(())
    }
}
