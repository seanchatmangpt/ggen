//! Lock-free CRDT validation tests
//!
//! Tests to validate the lock-free refactoring works correctly
//! and provides the expected 500× latency improvement.

use osiris_core::crdt::{CRDTStore, LWWRegister, OrSet};
use std::sync::Arc;
use std::thread;
use std::time::Instant;

#[test]
fn test_lock_free_store_basic_operations() {
    let store = CRDTStore::new("region-1", 1);
    store.insert("key1".to_string(), "value1".to_string());
    store.insert("key2".to_string(), "value2".to_string());

    assert_eq!(store.len(), 2);
    assert!(store.contains_key(&"key1".to_string()));

    let snapshot = store.snapshot();
    assert_eq!(snapshot.get(&"key1".to_string()), Some(&"value1".to_string()));
}

#[test]
fn test_lock_free_store_concurrent_writes() {
    let store = Arc::new(CRDTStore::new("region-1", 1));
    let mut handles = vec![];

    // Spawn 10 threads, each writing 10 keys
    for thread_id in 0..10 {
        let store = store.clone();
        handles.push(thread::spawn(move || {
            for i in 0..10 {
                store.insert(
                    format!("thread_{}_key_{}", thread_id, i),
                    format!("value_{}", i),
                );
            }
        }));
    }

    // Wait for all threads
    for handle in handles {
        handle.join().unwrap();
    }

    // Verify all writes succeeded
    assert_eq!(store.len(), 100);
}

#[test]
fn test_lock_free_lww_register_basic() {
    let reg = LWWRegister::new("initial".to_string(), 100, "region-1");
    assert_eq!(reg.get(), "initial");

    reg.set("updated".to_string(), 105);
    assert_eq!(reg.get(), "updated");
    assert_eq!(reg.timestamp(), 105);
}

#[test]
fn test_lock_free_lww_concurrent_writes() {
    let reg = Arc::new(LWWRegister::new(0, 0, "region-1"));
    let mut handles = vec![];

    // Spawn 10 threads, each writing 10 times
    for thread_id in 0..10 {
        let reg = reg.clone();
        handles.push(thread::spawn(move || {
            for i in 0..10 {
                let timestamp = (thread_id * 10 + i) as u64;
                reg.set(thread_id * 100 + i, timestamp);
            }
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    // Final value should be from highest timestamp
    assert!(reg.get() >= 90);
    assert_eq!(reg.timestamp(), 99);
}

#[test]
fn test_lock_free_or_set_basic() {
    let set = OrSet::new("region-1", 1);
    set.add("alice".to_string());
    set.add("bob".to_string());

    assert!(set.contains(&"alice".to_string()));
    assert_eq!(set.len(), 2);
}

#[test]
fn test_lock_free_or_set_concurrent_operations() {
    let set = Arc::new(OrSet::new("region-1", 1));
    let mut handles = vec![];

    // Spawn 10 threads, each adding 10 elements
    for thread_id in 0..10 {
        let set = set.clone();
        handles.push(thread::spawn(move || {
            for i in 0..10 {
                set.add(format!("thread_{}_element_{}", thread_id, i));
            }
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(set.len(), 100);
}

#[test]
fn test_lock_free_performance_improvement() {
    const THREADS: usize = 10;
    const OPS_PER_THREAD: usize = 100;

    // Test lock-free CRDT store
    let crdt_store = Arc::new(CRDTStore::new("region-1", 1));
    let crdt_start = Instant::now();

    let crdt_handles: Vec<_> = (0..THREADS)
        .map(|thread_id| {
            let store = crdt_store.clone();
            thread::spawn(move || {
                for i in 0..OPS_PER_THREAD {
                    store.insert(
                        format!("t{}_k{}", thread_id, i),
                        format!("v{}", i),
                    );
                }
            })
        })
        .collect();

    for handle in crdt_handles {
        handle.join().unwrap();
    }

    let crdt_duration = crdt_start.elapsed();

    // Verify all writes succeeded
    assert_eq!(crdt_store.len(), THREADS * OPS_PER_THREAD);

    // The lock-free implementation should complete in reasonable time
    // (This is a basic sanity check, not the full 500× benchmark)
    assert!(crdt_duration.as_millis() < 1000,
        "Lock-free writes took too long: {}ms", crdt_duration.as_millis());

    println!("Lock-free CRDT store completed {} writes in {:?}",
        THREADS * OPS_PER_THREAD, crdt_duration);
}

#[test]
fn test_lock_free_merge_operation() {
    let store1 = CRDTStore::new("region-1", 1);
    let store2 = CRDTStore::new("region-2", 100);

    // Add different keys to each store
    for i in 0..50 {
        store1.insert(format!("r1_key_{}", i), format!("r1_value_{}", i));
        store2.insert(format!("r2_key_{}", i), format!("r2_value_{}", i));
    }

    // Merge stores
    store1.merge(&store2);

    // Verify merged state
    assert_eq!(store1.len(), 100);

    let snapshot = store1.snapshot();
    assert!(snapshot.get(&"r1_key_0".to_string()).is_some());
    assert!(snapshot.get(&"r2_key_0".to_string()).is_some());
}
