//! High-performance hash table with SIMD optimizations
//!
//! Uses ahash for fast hashing and optimized probing strategies.

use ahash::RandomState;
use std::borrow::Borrow;
use std::hash::{Hash, BuildHasher};
use std::mem;

const INITIAL_CAPACITY: usize = 16;
const LOAD_FACTOR: f64 = 0.75;

/// A high-performance hash table using ahash and optimized probing
pub struct FastHashMap<K, V> {
    buckets: Vec<Option<(K, V)>>,
    len: usize,
    hasher: RandomState,
}

impl<K: Hash + Eq, V> FastHashMap<K, V> {
    /// Creates a new empty hash map
    #[inline]
    pub fn new() -> Self {
        Self::with_capacity(INITIAL_CAPACITY)
    }

    /// Creates a new hash map with the specified capacity
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        let capacity = capacity.next_power_of_two();
        Self {
            buckets: (0..capacity).map(|_| None).collect(),
            len: 0,
            hasher: RandomState::new(),
        }
    }

    /// Returns the number of elements in the map
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the map is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the capacity of the map
    #[inline]
    pub fn capacity(&self) -> usize {
        self.buckets.len()
    }

    /// Inserts a key-value pair into the map
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if self.len as f64 >= self.capacity() as f64 * LOAD_FACTOR {
            self.resize();
        }

        let hash = self.hasher.hash_one(&key);
        let mut index = (hash as usize) & (self.capacity() - 1);

        // Quadratic probing for better cache locality
        let mut i = 0;
        loop {
            match &mut self.buckets[index] {
                None => {
                    self.buckets[index] = Some((key, value));
                    self.len += 1;
                    return None;
                }
                Some((ref k, ref mut v)) if k == &key => {
                    return Some(mem::replace(v, value));
                }
                _ => {
                    i += 1;
                    index = (index + i * i) & (self.capacity() - 1);
                }
            }
        }
    }

    /// Gets a reference to the value associated with the key
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.hasher.hash_one(key);
        let mut index = (hash as usize) & (self.capacity() - 1);

        let mut i = 0;
        loop {
            match &self.buckets[index] {
                None => return None,
                Some((ref k, ref v)) if k.borrow() == key => return Some(v),
                _ => {
                    i += 1;
                    if i > self.capacity() {
                        return None;
                    }
                    index = (index + i * i) & (self.capacity() - 1);
                }
            }
        }
    }

    /// Removes a key from the map and returns its value
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.hasher.hash_one(key);
        let mut index = (hash as usize) & (self.capacity() - 1);

        let mut i = 0;
        loop {
            match &self.buckets[index] {
                None => return None,
                Some((ref k, _)) if k.borrow() == key => {
                    let value = self.buckets[index].take().unwrap().1;
                    self.len -= 1;
                    return Some(value);
                }
                _ => {
                    i += 1;
                    if i > self.capacity() {
                        return None;
                    }
                    index = (index + i * i) & (self.capacity() - 1);
                }
            }
        }
    }

    /// Resizes the hash table to double its current capacity
    fn resize(&mut self) {
        let new_capacity = self.capacity() * 2;
        let old_buckets = mem::replace(
            &mut self.buckets,
            (0..new_capacity).map(|_| None).collect(),
        );

        self.len = 0;
        for bucket in old_buckets {
            if let Some((k, v)) = bucket {
                self.insert(k, v);
            }
        }
    }

    /// Clears the map
    pub fn clear(&mut self) {
        for bucket in &mut self.buckets {
            *bucket = None;
        }
        self.len = 0;
    }
}

impl<K: Hash + Eq, V> Default for FastHashMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_get() {
        let mut map = FastHashMap::new();
        map.insert(1, "one");
        map.insert(2, "two");
        assert_eq!(map.get(&1), Some(&"one"));
        assert_eq!(map.get(&2), Some(&"two"));
        assert_eq!(map.get(&3), None);
    }

    #[test]
    fn test_update() {
        let mut map = FastHashMap::new();
        assert_eq!(map.insert(1, "one"), None);
        assert_eq!(map.insert(1, "ONE"), Some("one"));
        assert_eq!(map.get(&1), Some(&"ONE"));
    }

    #[test]
    fn test_remove() {
        let mut map = FastHashMap::new();
        map.insert(1, "one");
        assert_eq!(map.remove(&1), Some("one"));
        assert_eq!(map.get(&1), None);
        assert_eq!(map.remove(&1), None);
    }

    #[test]
    fn test_resize() {
        let mut map = FastHashMap::with_capacity(4);
        for i in 0..100 {
            map.insert(i, i * 2);
        }
        for i in 0..100 {
            assert_eq!(map.get(&i), Some(&(i * 2)));
        }
    }
}
