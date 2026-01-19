//! Property-based tests for correctness verification

use perf_library::{FastHashMap, LockFreeQueue, LockFreeStack, CompactStorage};
use proptest::prelude::*;
use std::collections::HashMap;

proptest! {
    #[test]
    fn test_hashmap_insert_get(pairs in prop::collection::vec((0u64..1000, 0u64..1000), 0..100)) {
        let mut fast_map = FastHashMap::new();
        let mut std_map = HashMap::new();

        for (k, v) in pairs.iter() {
            fast_map.insert(*k, *v);
            std_map.insert(*k, *v);
        }

        for (k, v) in std_map.iter() {
            prop_assert_eq!(fast_map.get(k), Some(v));
        }
    }

    #[test]
    fn test_hashmap_remove(pairs in prop::collection::vec((0u64..100, 0u64..100), 0..50)) {
        let mut fast_map = FastHashMap::new();

        for (k, v) in pairs.iter() {
            fast_map.insert(*k, *v);
        }

        for (k, _) in pairs.iter() {
            let _ = fast_map.remove(k);
        }

        for (k, _) in pairs.iter() {
            prop_assert_eq!(fast_map.get(k), None);
        }
    }

    #[test]
    fn test_queue_fifo(values in prop::collection::vec(0u64..1000, 0..100)) {
        let queue = LockFreeQueue::new();

        for v in values.iter() {
            queue.push(*v);
        }

        for v in values.iter() {
            prop_assert_eq!(queue.pop(), Some(*v));
        }

        prop_assert_eq!(queue.pop(), None);
    }

    #[test]
    fn test_stack_lifo(values in prop::collection::vec(0u64..1000, 0..100)) {
        let stack = LockFreeStack::new();

        for v in values.iter() {
            stack.push(*v);
        }

        for v in values.iter().rev() {
            prop_assert_eq!(stack.pop(), Some(*v));
        }

        prop_assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_storage_store_retrieve(vecs in prop::collection::vec(
        prop::collection::vec(0u64..100, 0..20),
        0..10
    )) {
        let mut storage = CompactStorage::new();
        let mut ids = Vec::new();

        for vec in vecs.iter() {
            let id = storage.store(vec.clone());
            ids.push(id);
        }

        for (id, expected) in ids.iter().zip(vecs.iter()) {
            prop_assert_eq!(storage.retrieve(*id), Some(&expected[..]));
        }
    }
}

#[cfg(test)]
mod concurrent_tests {
    use super::*;
    use rayon::prelude::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_concurrent_queue_stress() {
        let queue = LockFreeQueue::new();
        let pushed = Arc::new(AtomicUsize::new(0));
        let popped = Arc::new(AtomicUsize::new(0));

        let push_count = 10000;
        let threads = 8;

        // Multiple threads pushing
        (0..threads).into_par_iter().for_each(|thread_id| {
            for i in 0..push_count {
                queue.push(thread_id * push_count + i);
                pushed.fetch_add(1, Ordering::Relaxed);
            }
        });

        // Multiple threads popping
        (0..threads).into_par_iter().for_each(|_| {
            loop {
                if queue.pop().is_some() {
                    if popped.fetch_add(1, Ordering::Relaxed) >= threads * push_count - 1 {
                        break;
                    }
                } else if popped.load(Ordering::Relaxed) >= threads * push_count {
                    break;
                }
            }
        });

        assert_eq!(pushed.load(Ordering::Relaxed), threads * push_count);
        assert_eq!(popped.load(Ordering::Relaxed), threads * push_count);
    }

    #[test]
    fn test_concurrent_hashmap() {
        let map = Arc::new(parking_lot::Mutex::new(FastHashMap::new()));
        let iterations = 1000;

        (0..8).into_par_iter().for_each(|thread_id| {
            for i in 0..iterations {
                let key = thread_id * iterations + i;
                map.lock().insert(key, key * 2);
            }
        });

        let map = map.lock();
        for thread_id in 0..8 {
            for i in 0..iterations {
                let key = thread_id * iterations + i;
                assert_eq!(map.get(&key), Some(&(key * 2)));
            }
        }
    }
}
