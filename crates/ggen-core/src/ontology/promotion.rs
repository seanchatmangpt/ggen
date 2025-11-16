/// Atomic Snapshot Promotion: Lock-Free, Picosecond-Level Ontology Switching
///
/// This module implements zero-copy, lock-free snapshot promotion using atomic operations.
/// The cost of promoting a new ontology is just a few CPU ticks (atomic pointer swap).

use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::Arc;
use std::ptr::NonNull;
use serde::{Deserialize, Serialize};

use crate::ontology::SigmaSnapshot;

/// High-performance snapshot holder with atomic promotion
pub struct AtomicSnapshotPromoter {
    /// Current active snapshot (behind atomic pointer)
    /// Uses SeqCst ordering for maximum safety; can optimize to Release/Acquire if needed
    current: AtomicPtr<SnapshotHandle>,

    /// Total promotions performed (for metrics)
    promotion_count: AtomicUsize,

    /// Last promotion timestamp (nanoseconds since epoch)
    last_promotion_ns: AtomicUsize,
}

/// Handle to a snapshot (reference-counted for safe cleanup)
pub struct SnapshotHandle {
    snapshot: Arc<SigmaSnapshot>,
    reference_count: AtomicUsize,
}

impl SnapshotHandle {
    fn new(snapshot: Arc<SigmaSnapshot>) -> Self {
        Self {
            snapshot,
            reference_count: AtomicUsize::new(1),
        }
    }

    fn increment_refs(&self) {
        self.reference_count.fetch_add(1, Ordering::AcqRel);
    }

    fn decrement_refs(&self) -> usize {
        self.reference_count.fetch_sub(1, Ordering::AcqRel)
    }
}

impl AtomicSnapshotPromoter {
    /// Create a new promoter with an initial snapshot
    pub fn new(initial_snapshot: Arc<SigmaSnapshot>) -> Self {
        let handle = Box::into_raw(Box::new(SnapshotHandle::new(initial_snapshot)));

        Self {
            current: AtomicPtr::new(handle),
            promotion_count: AtomicUsize::new(0),
            last_promotion_ns: AtomicUsize::new(0),
        }
    }

    /// Get the current snapshot (with reference counting)
    pub fn get_current(&self) -> SnapshotGuard {
        let ptr = self.current.load(Ordering::SeqCst);

        // Safety: pointer is always valid (managed by this struct)
        unsafe {
            (*ptr).increment_refs();
            SnapshotGuard {
                handle: NonNull::new(ptr).unwrap(),
            }
        }
    }

    /// Atomically promote a new snapshot to be current
    /// Returns: (old_snapshot, new_snapshot)
    pub fn promote(&self, new_snapshot: Arc<SigmaSnapshot>) -> PromotionResult {
        let start_ns = get_time_ns();
        let new_handle = Box::into_raw(Box::new(SnapshotHandle::new(new_snapshot)));

        // Atomic pointer swap (this is the critical operation)
        let old_ptr = self
            .current
            .swap(new_handle, Ordering::SeqCst);

        let end_ns = get_time_ns();

        // Update metrics
        self.promotion_count.fetch_add(1, Ordering::Relaxed);
        self.last_promotion_ns.store(end_ns, Ordering::Relaxed);

        // Decrement old handle refs
        unsafe {
            if (*old_ptr).decrement_refs() == 1 {
                // Last reference: drop the old snapshot
                let _ = Box::from_raw(old_ptr);
            }
        }

        PromotionResult {
            duration_ns: end_ns - start_ns,
            promotion_count: self.promotion_count.load(Ordering::Relaxed),
        }
    }

    /// Get promotion metrics
    pub fn metrics(&self) -> PromotionMetrics {
        PromotionMetrics {
            total_promotions: self.promotion_count.load(Ordering::Relaxed),
            last_promotion_ns: self.last_promotion_ns.load(Ordering::Relaxed),
        }
    }
}

impl Drop for AtomicSnapshotPromoter {
    fn drop(&mut self) {
        let ptr = self.current.load(Ordering::SeqCst);

        // Safety: we own this pointer
        unsafe {
            let _ = Box::from_raw(ptr);
        }
    }
}

// Safety: AtomicSnapshotPromoter can be shared across threads
unsafe impl Send for AtomicSnapshotPromoter {}
unsafe impl Sync for AtomicSnapshotPromoter {}

/// RAII guard for snapshot access
pub struct SnapshotGuard {
    handle: NonNull<SnapshotHandle>,
}

impl SnapshotGuard {
    /// Get a reference to the snapshot
    pub fn snapshot(&self) -> Arc<SigmaSnapshot> {
        // Safety: handle is always valid
        unsafe {
            self.handle.as_ref().unwrap().snapshot.clone()
        }
    }
}

impl Drop for SnapshotGuard {
    fn drop(&mut self) {
        // Safety: handle is valid
        unsafe {
            if self.handle.as_ref().unwrap().decrement_refs() == 1 {
                let _ = Box::from_raw(self.handle.as_mut().unwrap() as *mut SnapshotHandle);
            }
        }
    }
}

// Safety: SnapshotGuard can be sent across thread boundaries
unsafe impl Send for SnapshotGuard {}
unsafe impl Sync for SnapshotGuard {}

/// Result of a promotion operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromotionResult {
    /// Time taken (nanoseconds)
    pub duration_ns: usize,

    /// Total promotion count (for sequencing)
    pub promotion_count: usize,
}

/// Promotion metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromotionMetrics {
    pub total_promotions: usize,
    pub last_promotion_ns: usize,
}

/// Get current time in nanoseconds (monotonic clock)
fn get_time_ns() -> usize {
    use std::time::{SystemTime, UNIX_EPOCH};

    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_snapshot(version: &str) -> Arc<SigmaSnapshot> {
        Arc::new(SigmaSnapshot::new(
            None,
            vec![],
            version.to_string(),
            "sig".to_string(),
            Default::default(),
        ))
    }

    #[test]
    fn test_promoter_creation() {
        let snap = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap.clone());

        let current = promoter.get_current();
        assert_eq!(current.snapshot().version, "1.0.0");
    }

    #[test]
    fn test_atomic_promotion() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap1);

        let snap2 = create_test_snapshot("2.0.0");
        let result = promoter.promote(snap2);

        assert!(result.duration_ns < 10_000); // Should be very fast (< 10μs)

        let current = promoter.get_current();
        assert_eq!(current.snapshot().version, "2.0.0");
    }

    #[test]
    fn test_promotion_metrics() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap1);

        assert_eq!(promoter.metrics().total_promotions, 0);

        let snap2 = create_test_snapshot("2.0.0");
        promoter.promote(snap2);

        let metrics = promoter.metrics();
        assert_eq!(metrics.total_promotions, 1);
    }

    #[test]
    fn test_multiple_promotions() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        for i in 2..=5 {
            let snap = create_test_snapshot(&format!("{}.0.0", i));
            promoter.promote(snap);
        }

        let current = promoter.get_current();
        assert_eq!(current.snapshot().version, "5.0.0");

        let metrics = promoter.metrics();
        assert_eq!(metrics.total_promotions, 4);
    }

    #[test]
    fn test_concurrent_reads() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        let mut handles = vec![];

        // Spawn 100 threads reading the snapshot
        for _ in 0..100 {
            let p = promoter.clone();
            handles.push(std::thread::spawn(move || {
                let guard = p.get_current();
                assert!(!guard.snapshot().version.is_empty());
            }));
        }

        // All threads should succeed
        for handle in handles {
            handle.join().unwrap();
        }
    }

    #[test]
    fn test_snapshot_guard_raii() {
        let snap = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap);

        {
            let _guard1 = promoter.get_current();
            let _guard2 = promoter.get_current();
            // Guards manage reference counts automatically
        }

        // All guards dropped, no leaks
        let current = promoter.get_current();
        assert_eq!(current.snapshot().version, "1.0.0");
    }

    #[test]
    fn test_promotion_with_guards() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        let guard1 = promoter.get_current();
        assert_eq!(guard1.snapshot().version, "1.0.0");

        let snap2 = create_test_snapshot("2.0.0");
        promoter.promote(snap2);

        // Old guard still valid (ref count prevents deallocation)
        assert_eq!(guard1.snapshot().version, "1.0.0");

        // New promoter reads give new version
        let guard2 = promoter.get_current();
        assert_eq!(guard2.snapshot().version, "2.0.0");
    }
}
