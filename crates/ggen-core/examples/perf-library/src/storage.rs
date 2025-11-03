//! Memory-efficient storage structures
//!
//! Provides compact storage with efficient memory usage and fast access.

use std::collections::HashMap;

/// A compact storage structure for byte vectors
pub struct CompactStorage<T> {
    data: Vec<T>,
    index: HashMap<usize, (usize, usize)>, // id -> (start, len)
    next_id: usize,
}

impl<T: Clone> CompactStorage<T> {
    /// Creates a new compact storage
    #[inline]
    pub fn new() -> Self {
        Self::with_capacity(1024)
    }

    /// Creates a new compact storage with the specified capacity
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            index: HashMap::new(),
            next_id: 0,
        }
    }

    /// Stores a vector and returns its ID
    pub fn store(&mut self, items: Vec<T>) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        let start = self.data.len();
        let len = items.len();
        self.data.extend(items);
        self.index.insert(id, (start, len));

        id
    }

    /// Retrieves a reference to the stored vector by ID
    pub fn retrieve(&self, id: usize) -> Option<&[T]> {
        self.index.get(&id).map(|(start, len)| {
            &self.data[*start..*start + *len]
        })
    }

    /// Removes a stored vector by ID
    pub fn remove(&mut self, id: usize) -> Option<Vec<T>> {
        if let Some((start, len)) = self.index.remove(&id) {
            let result = self.data[start..start + len].to_vec();
            // Mark as removed but don't actually remove to avoid shifting
            Some(result)
        } else {
            None
        }
    }

    /// Returns the number of stored items
    #[inline]
    pub fn len(&self) -> usize {
        self.index.len()
    }

    /// Returns true if the storage is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.index.is_empty()
    }

    /// Returns the total capacity in elements
    #[inline]
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    /// Compacts the storage by removing gaps
    pub fn compact(&mut self) {
        if self.index.is_empty() {
            self.data.clear();
            return;
        }

        let mut new_data = Vec::with_capacity(self.data.capacity());
        let mut new_index = HashMap::with_capacity(self.index.len());

        // Sort by start position to maintain order
        let mut entries: Vec<_> = self.index.iter().collect();
        entries.sort_by_key(|(_, (start, _))| *start);

        for (id, (start, len)) in entries {
            let new_start = new_data.len();
            new_data.extend_from_slice(&self.data[*start..*start + *len]);
            new_index.insert(*id, (new_start, *len));
        }

        self.data = new_data;
        self.index = new_index;
    }

    /// Clears all stored data
    pub fn clear(&mut self) {
        self.data.clear();
        self.index.clear();
        self.next_id = 0;
    }
}

impl<T: Clone> Default for CompactStorage<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A memory-efficient sparse array
pub struct SparseArray<T> {
    data: HashMap<usize, T>,
    max_index: usize,
}

impl<T> SparseArray<T> {
    /// Creates a new sparse array
    #[inline]
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            max_index: 0,
        }
    }

    /// Sets a value at the specified index
    pub fn set(&mut self, index: usize, value: T) {
        self.max_index = self.max_index.max(index);
        self.data.insert(index, value);
    }

    /// Gets a reference to the value at the specified index
    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(&index)
    }

    /// Removes a value at the specified index
    #[inline]
    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.data.remove(&index)
    }

    /// Returns the number of elements
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if the array is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the maximum index
    #[inline]
    pub fn max_index(&self) -> usize {
        self.max_index
    }

    /// Clears all data
    pub fn clear(&mut self) {
        self.data.clear();
        self.max_index = 0;
    }
}

impl<T> Default for SparseArray<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compact_storage() {
        let mut storage = CompactStorage::new();
        let id1 = storage.store(vec![1, 2, 3]);
        let id2 = storage.store(vec![4, 5, 6]);

        assert_eq!(storage.retrieve(id1), Some(&[1, 2, 3][..]));
        assert_eq!(storage.retrieve(id2), Some(&[4, 5, 6][..]));
        assert_eq!(storage.len(), 2);
    }

    #[test]
    fn test_storage_remove() {
        let mut storage = CompactStorage::new();
        let id = storage.store(vec![1, 2, 3]);
        assert_eq!(storage.remove(id), Some(vec![1, 2, 3]));
        assert_eq!(storage.retrieve(id), None);
    }

    #[test]
    fn test_storage_compact() {
        let mut storage = CompactStorage::new();
        let id1 = storage.store(vec![1, 2, 3]);
        let id2 = storage.store(vec![4, 5, 6]);
        storage.remove(id1);

        storage.compact();
        assert_eq!(storage.retrieve(id2), Some(&[4, 5, 6][..]));
    }

    #[test]
    fn test_sparse_array() {
        let mut array = SparseArray::new();
        array.set(0, "zero");
        array.set(100, "hundred");
        array.set(1000, "thousand");

        assert_eq!(array.get(0), Some(&"zero"));
        assert_eq!(array.get(100), Some(&"hundred"));
        assert_eq!(array.get(1000), Some(&"thousand"));
        assert_eq!(array.get(50), None);
        assert_eq!(array.len(), 3);
        assert_eq!(array.max_index(), 1000);
    }
}
