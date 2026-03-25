//! CRDT-backed state store for lock-free concurrent modifications
//!
//! CRDTStore replaces `Arc<RwLock<HashMap>>` with an OR-Set-based store
//! that allows concurrent writes without locking. Reads return immutable snapshots.
//!
//! **Design**:
//! - Insert/update: Write directly to local replica (no lock)
//! - Remove: Mark element as removed (tombstone)
//! - Read: Return immutable snapshot (lock-free)
//! - Merge: Combine state from other regions

use super::or_set::OrSet;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A snapshot of the current state (immutable, lock-free read)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StoreSnapshot<K: Clone + Eq + std::hash::Hash, V: Clone> {
    /// Current key-value pairs in the store
    data: HashMap<K, V>,
}

impl<K: Clone + PartialEq + Eq + std::hash::Hash, V: Clone> StoreSnapshot<K, V> {
    /// Returns a reference to the value for a key
    pub fn get(&self, key: &K) -> Option<&V> {
        self.data.get(key)
    }

    /// Returns all keys in the snapshot
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.data.keys()
    }

    /// Returns all values in the snapshot
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.values()
    }

    /// Returns the number of key-value pairs
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if the snapshot is empty
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Iterates over all key-value pairs
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.data.iter()
    }
}

/// CRDT-backed key-value store with lock-free reads and atomic writes.
///
/// This is a simple wrapper around OR-Set that provides HashMap-like semantics
/// for tracking elements and their removal. In Phase 1, this is a building block.
/// Phase 2 will add actual replication and merge protocols.
///
/// # Thread Safety
/// - Not currently wrapped in Arc/Mutex (designed for single-threaded Phase 1)
/// - Phase 2 will add async merge operations
///
/// # Examples
///
/// ```
/// use osiris_core::crdt::CRDTStore;
///
/// let mut store = CRDTStore::new("region-1", 1);
/// store.insert("key1".to_string(), "value1".to_string());
/// store.insert("key2".to_string(), "value2".to_string());
/// assert_eq!(store.len(), 2);
///
/// store.remove(&"key1".to_string());
/// assert_eq!(store.len(), 1);
///
/// let snapshot = store.snapshot();
/// assert_eq!(snapshot.get(&"key2".to_string()), Some(&"value2".to_string()));
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CRDTStore<K: Clone + PartialEq + Eq + std::hash::Hash, V: Clone> {
    /// OR-Set tracking which elements have been added/removed
    /// Maps from (key, value_id) -> true if present
    elements: OrSet<(K, String)>,
    /// Current values (separate from tombstones)
    /// Maps key to (value, version)
    values: HashMap<K, (V, u64)>,
    /// Region ID for this store
    region_id: String,
    /// Next version number for updates
    next_version: u64,
}

impl<K: Clone + PartialEq + Eq + std::hash::Hash, V: Clone> CRDTStore<K, V> {
    /// Creates a new CRDT-backed store in a given region.
    ///
    /// # Arguments
    /// * `region_id` - Identifier for this region
    /// * `initial_sequence` - Starting sequence for OR-Set operations
    pub fn new(region_id: impl Into<String>, initial_sequence: u64) -> Self {
        let region_id_str = region_id.into();
        Self {
            elements: OrSet::new(region_id_str.clone(), initial_sequence),
            values: HashMap::new(),
            region_id: region_id_str,
            next_version: 1,
        }
    }

    /// Inserts or updates a key-value pair in the store.
    ///
    /// # Returns
    /// The previous value if the key was already present
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        // Generate a unique value ID (region + version)
        let value_id = format!("{}:{}", self.region_id, self.next_version);
        self.next_version += 1;

        // Add to OR-Set to mark this version as present
        self.elements.add((key.clone(), value_id));

        // Store the actual value
        let old = self.values.insert(key, (value, self.next_version - 1));
        old.map(|(v, _)| v)
    }

    /// Removes a key from the store.
    ///
    /// # Returns
    /// The removed value if present
    pub fn remove(&mut self, key: &K) -> Option<V> {
        // Mark all versions of this key as removed in OR-Set
        // Create tuples for all versions of this key
        if let Some(value) = self.values.get(key) {
            let value_id = format!("{}:{}", self.region_id, value.1);
            self.elements.remove_all(&(key.clone(), value_id));
        }

        // Remove from values map
        self.values.remove(key).map(|(v, _)| v)
    }

    /// Checks if a key exists in the store.
    pub fn contains_key(&self, key: &K) -> bool {
        self.values.contains_key(key)
    }

    /// Returns the number of key-value pairs in the store.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Creates an immutable snapshot of the current state (lock-free read).
    pub fn snapshot(&self) -> StoreSnapshot<K, V> {
        StoreSnapshot {
            data: self.values.iter().map(|(k, (v, _))| (k.clone(), v.clone())).collect(),
        }
    }

    /// Merges another store's state into this one (CRDT merge operation).
    ///
    /// Combines the OR-Set elements and takes the highest version for each key.
    pub fn merge(&mut self, other: &Self) {
        // Merge OR-Sets
        self.elements.merge(&other.elements);

        // Merge values, taking higher version for each key
        for (key, (value, version)) in &other.values {
            match self.values.get(key) {
                Some((_, our_version)) if version > our_version => {
                    self.values.insert(key.clone(), (value.clone(), *version));
                }
                None => {
                    self.values.insert(key.clone(), (value.clone(), *version));
                }
                _ => {} // Keep our version
            }
        }
    }

    /// Returns the region ID for this store.
    pub fn region_id(&self) -> &str {
        &self.region_id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crdt_store_insert_and_read() {
        let mut store = CRDTStore::new("region-1", 1);
        store.insert("key1".to_string(), "value1".to_string());

        let snapshot = store.snapshot();
        assert_eq!(snapshot.get(&"key1".to_string()), Some(&"value1".to_string()));
    }

    #[test]
    fn test_crdt_store_insert_overwrites() {
        let mut store = CRDTStore::new("region-1", 1);
        store.insert("key1".to_string(), "old".to_string());
        let old = store.insert("key1".to_string(), "new".to_string());

        assert_eq!(old, Some("old".to_string()));

        let snapshot = store.snapshot();
        assert_eq!(snapshot.get(&"key1".to_string()), Some(&"new".to_string()));
    }

    #[test]
    fn test_crdt_store_remove() {
        let mut store = CRDTStore::new("region-1", 1);
        store.insert("key1".to_string(), "value1".to_string());
        store.insert("key2".to_string(), "value2".to_string());

        assert_eq!(store.len(), 2);
        let removed = store.remove(&"key1".to_string());
        assert_eq!(removed, Some("value1".to_string()));
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_crdt_store_contains_key() {
        let mut store = CRDTStore::new("region-1", 1);
        store.insert("key1".to_string(), "value1".to_string());

        assert!(store.contains_key(&"key1".to_string()));
        assert!(!store.contains_key(&"key2".to_string()));
    }

    #[test]
    fn test_crdt_store_snapshot() {
        let mut store = CRDTStore::new("region-1", 1);
        store.insert("a".to_string(), 1);
        store.insert("b".to_string(), 2);

        let snapshot = store.snapshot();
        assert_eq!(snapshot.len(), 2);
        assert_eq!(snapshot.get(&"a".to_string()), Some(&1));
        assert_eq!(snapshot.get(&"b".to_string()), Some(&2));
    }

    #[test]
    fn test_crdt_store_merge() {
        let mut store1 = CRDTStore::new("region-1", 1);
        store1.insert("key1".to_string(), "value1".to_string());
        store1.insert("key2".to_string(), "value2".to_string());

        let mut store2 = CRDTStore::new("region-2", 100);
        store2.insert("key3".to_string(), "value3".to_string());

        store1.merge(&store2);

        let snapshot = store1.snapshot();
        assert_eq!(snapshot.len(), 3);
        assert!(snapshot.get(&"key1".to_string()).is_some());
        assert!(snapshot.get(&"key2".to_string()).is_some());
        assert!(snapshot.get(&"key3".to_string()).is_some());
    }

    #[test]
    fn test_crdt_store_is_empty() {
        let mut store = CRDTStore::new("region-1", 1);
        assert!(store.is_empty());

        store.insert("key".to_string(), "value".to_string());
        assert!(!store.is_empty());
    }

    #[test]
    fn test_crdt_store_region_id() {
        let store = CRDTStore::<String, String>::new("region-abc", 1);
        assert_eq!(store.region_id(), "region-abc");
    }
}
