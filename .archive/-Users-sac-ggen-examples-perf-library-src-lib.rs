//! High-Performance Rust Library
//!
//! Generated using ggen's AI generate command (dogfooding):
//! ```bash
//! ggen ai generate \
//!   --description "High-performance Rust library with custom hash table using ahash, lock-free concurrent data structures with crossbeam, memory-efficient storage, and comprehensive criterion benchmarks" \
//!   --output examples/perf-library/src/lib.rs
//! ```

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Custom high-performance hash table using ahash for fast hashing
///
/// This implementation provides:
/// - Fast non-cryptographic hashing via ahash
/// - Standard HashMap interface
/// - Zero-cost abstraction over std::collections::HashMap
pub struct FastHashMap<K, V> {
    inner: HashMap<K, V, ahash::RandomState>,
}

impl<K: Hash + Eq, V> FastHashMap<K, V> {
    /// Create a new FastHashMap
    pub fn new() -> Self {
        Self {
            inner: HashMap::with_hasher(ahash::RandomState::new()),
        }
    }

    /// Create with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: HashMap::with_capacity_and_hasher(capacity, ahash::RandomState::new()),
        }
    }

    /// Insert a key-value pair
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }

    /// Get a reference to the value for a key
    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    /// Get a mutable reference to the value for a key
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }

    /// Remove a key-value pair
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.inner.remove(key)
    }

    /// Get the number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the map is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Get the capacity
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }
}

impl<K: Hash + Eq, V> Default for FastHashMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// Lock-free concurrent counter using crossbeam
pub struct ConcurrentCounter {
    value: crossbeam::atomic::AtomicCell<u64>,
}

impl ConcurrentCounter {
    /// Create a new counter starting at 0
    pub fn new() -> Self {
        Self {
            value: crossbeam::atomic::AtomicCell::new(0),
        }
    }

    /// Increment and return the new value
    pub fn increment(&self) -> u64 {
        let mut current = self.value.load();
        loop {
            let new = current + 1;
            match self.value.compare_exchange(current, new) {
                Ok(_) => return new,
                Err(actual) => current = actual,
            }
        }
    }

    /// Get the current value
    pub fn get(&self) -> u64 {
        self.value.load()
    }

    /// Reset to 0
    pub fn reset(&self) {
        self.value.store(0);
    }
}

impl Default for ConcurrentCounter {
    fn default() -> Self {
        Self::new()
    }
}

/// Parallel batch processor using rayon
pub fn parallel_process<T, F, R>(items: &[T], operation: F) -> Vec<R>
where
    T: Sync,
    F: Fn(&T) -> R + Sync + Send,
    R: Send,
{
    use rayon::prelude::*;
    items.par_iter().map(operation).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fast_hash_map() {
        let mut map = FastHashMap::new();
        map.insert("key1", "value1");
        map.insert("key2", "value2");

        assert_eq!(map.get(&"key1"), Some(&"value1"));
        assert_eq!(map.get(&"key2"), Some(&"value2"));
        assert_eq!(map.len(), 2);

        map.remove(&"key1");
        assert_eq!(map.len(), 1);
        assert!(map.get(&"key1").is_none());
    }

    #[test]
    fn test_concurrent_counter() {
        let counter = ConcurrentCounter::new();
        assert_eq!(counter.get(), 0);

        assert_eq!(counter.increment(), 1);
        assert_eq!(counter.increment(), 2);
        assert_eq!(counter.get(), 2);

        counter.reset();
        assert_eq!(counter.get(), 0);
    }

    #[test]
    fn test_parallel_process() {
        let items = vec![1, 2, 3, 4, 5];
        let results = parallel_process(&items, |x| x * 2);
        assert_eq!(results, vec![2, 4, 6, 8, 10]);
    }

    #[test]
    fn test_fast_hash_map_capacity() {
        let map: FastHashMap<i32, i32> = FastHashMap::with_capacity(100);
        assert!(map.capacity() >= 100);
        assert!(map.is_empty());
    }
}
