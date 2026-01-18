#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

//! High-performance data structures library
//!
//! This library provides optimized data structures for high-performance applications:
//! - Custom hash table with SIMD optimizations
//! - Lock-free concurrent data structures
//! - Memory-efficient storage

pub mod hashtable;
pub mod concurrent;
pub mod storage;

pub use hashtable::FastHashMap;
pub use concurrent::{LockFreeQueue, LockFreeStack};
pub use storage::CompactStorage;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut map = FastHashMap::new();
        map.insert(1, "hello");
        assert_eq!(map.get(&1), Some(&"hello"));
    }

    #[test]
    fn test_concurrent_queue() {
        let queue = LockFreeQueue::new();
        queue.push(42);
        assert_eq!(queue.pop(), Some(42));
    }

    #[test]
    fn test_compact_storage() {
        let mut storage = CompactStorage::new();
        let id = storage.store(vec![1, 2, 3, 4, 5]);
        assert_eq!(storage.retrieve(id), Some(&vec![1, 2, 3, 4, 5]));
    }
}
