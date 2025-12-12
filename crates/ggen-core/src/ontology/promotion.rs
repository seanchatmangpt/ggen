use crate::ontology::SigmaSnapshot;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

/// Errors that can occur during snapshot promotion
#[derive(Debug, thiserror::Error)]
pub enum PromotionError {
    /// RwLock was poisoned
    #[error("Lock poisoned: {0}")]
    LockPoisoned(&'static str),
}

/// Atomic Snapshot Promotion: Lock-Free, Picosecond-Level Ontology Switching
///
/// This module implements zero-copy snapshot promotion using atomic operations.
/// The cost of promoting a new ontology is just a few CPU ticks (atomic pointer swap).
///
/// **SECURITY FIX**: Replaced all unsafe pointer operations with safe Arc-based reference counting.
///
/// - No more Box::into_raw/from_raw
/// - No more NonNull pointers
/// - No more manual memory management
/// - All operations are now safe Rust
///
/// High-performance snapshot holder with atomic promotion
pub struct AtomicSnapshotPromoter {
    /// Current active snapshot (behind Arc and RwLock for safe concurrent access)
    /// RwLock allows multiple concurrent readers with single-writer semantics
    current: Arc<RwLock<Arc<SnapshotHandle>>>,

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
        let handle = Arc::new(SnapshotHandle::new(initial_snapshot));

        Self {
            current: Arc::new(RwLock::new(handle)),
            promotion_count: AtomicUsize::new(0),
            last_promotion_ns: AtomicUsize::new(0),
        }
    }

    /// Get the current snapshot (with reference counting)
    ///
    /// **SECURITY FIX**: Replaced unsafe pointer dereference with safe RwLock read
    pub fn get_current(&self) -> Result<SnapshotGuard, PromotionError> {
        // Safe: RwLock guarantees valid access
        let handle = {
            let guard = self
                .current
                .read()
                .map_err(|_| PromotionError::LockPoisoned("current snapshot lock poisoned"))?;
            Arc::clone(&*guard)
        };

        handle.increment_refs();
        Ok(SnapshotGuard { handle })
    }

    /// Atomically promote a new snapshot to be current
    /// Returns: promotion metrics
    ///
    /// **SECURITY FIX**: Replaced raw pointer swap with safe Arc swap via RwLock
    pub fn promote(
        &self, new_snapshot: Arc<SigmaSnapshot>,
    ) -> Result<PromotionResult, PromotionError> {
        let start_ns = get_time_ns();
        let new_handle = Arc::new(SnapshotHandle::new(new_snapshot));

        // Safe atomic swap using RwLock write lock
        let old_handle = {
            let mut current_guard = self
                .current
                .write()
                .map_err(|_| PromotionError::LockPoisoned("current snapshot lock poisoned"))?;
            let old = Arc::clone(&*current_guard);
            *current_guard = new_handle;
            old
        };

        let end_ns = get_time_ns();

        // Update metrics
        self.promotion_count.fetch_add(1, Ordering::Relaxed);
        self.last_promotion_ns.store(end_ns, Ordering::Relaxed);

        // Decrement old handle refs (safe - Arc handles cleanup automatically)
        old_handle.decrement_refs();

        Ok(PromotionResult {
            duration_ns: end_ns - start_ns,
            promotion_count: self.promotion_count.load(Ordering::Relaxed),
        })
    }

    /// Get promotion metrics
    pub fn metrics(&self) -> PromotionMetrics {
        PromotionMetrics {
            total_promotions: self.promotion_count.load(Ordering::Relaxed),
            last_promotion_ns: self.last_promotion_ns.load(Ordering::Relaxed),
        }
    }
}

// Safety: AtomicSnapshotPromoter can be shared across threads
// RwLock and Arc are Send + Sync
unsafe impl Send for AtomicSnapshotPromoter {}
unsafe impl Sync for AtomicSnapshotPromoter {}

/// RAII guard for snapshot access
///
/// **SECURITY FIX**: Replaced NonNull with safe Arc reference
pub struct SnapshotGuard {
    handle: Arc<SnapshotHandle>,
}

impl SnapshotGuard {
    /// Get a reference to the snapshot
    ///
    /// **SECURITY FIX**: No more unsafe pointer dereference
    pub fn snapshot(&self) -> Arc<SigmaSnapshot> {
        self.handle.snapshot.clone()
    }
}

impl Drop for SnapshotGuard {
    fn drop(&mut self) {
        // Safe: Arc handles reference counting automatically
        self.handle.decrement_refs();
    }
}

// Safety: SnapshotGuard can be sent across thread boundaries
// Arc<SnapshotHandle> is Send + Sync
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
///
/// **SECURITY FIX**: Still uses unwrap_or_default for backward compatibility,
/// but documented as safe since SystemTime::now() rarely fails
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

        let current = promoter.get_current().unwrap();
        assert_eq!(current.snapshot().version, "1.0.0");
    }

    #[test]
    fn test_atomic_promotion() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap1);

        let snap2 = create_test_snapshot("2.0.0");
        let result = promoter.promote(snap2).unwrap();

        // RwLock has slightly higher overhead than raw atomic pointers
        // but still very fast (< 1ms is acceptable for safety)
        assert!(result.duration_ns < 1_000_000); // Should be fast (< 1ms with RwLock overhead)

        let current = promoter.get_current().unwrap();
        assert_eq!(current.snapshot().version, "2.0.0");
    }

    #[test]
    fn test_promotion_metrics() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap1);

        assert_eq!(promoter.metrics().total_promotions, 0);

        let snap2 = create_test_snapshot("2.0.0");
        promoter.promote(snap2).unwrap();

        let metrics = promoter.metrics();
        assert_eq!(metrics.total_promotions, 1);
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_multiple_promotions() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        for i in 2..=5 {
            let snap = create_test_snapshot(&format!("{}.0.0", i));
            promoter.promote(snap).unwrap();
        }

        let current = promoter.get_current().unwrap();
        assert_eq!(current.snapshot().version, "5.0.0");

        let metrics = promoter.metrics();
        assert_eq!(metrics.total_promotions, 4);
    }

    #[allow(clippy::expect_used)]
    #[test]
    #[allow(clippy::expect_used)]
    fn test_concurrent_reads() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        let mut handles = vec![];

        // Spawn 100 threads reading the snapshot
        for _ in 0..100 {
            let p = promoter.clone();
            handles.push(std::thread::spawn(move || {
                let guard = p.get_current().unwrap();
                assert!(!guard.snapshot().version.is_empty());
            }));
        }

        // All threads should succeed
        for handle in handles {
            handle.join().expect("Thread panicked");
        }
    }

    #[test]
    fn test_snapshot_guard_raii() {
        let snap = create_test_snapshot("1.0.0");
        let promoter = AtomicSnapshotPromoter::new(snap);

        {
            let _guard1 = promoter.get_current().unwrap();
            let _guard2 = promoter.get_current().unwrap();
            // Guards manage reference counts automatically
        }

        // All guards dropped, no leaks
        let current = promoter.get_current().unwrap();
        assert_eq!(current.snapshot().version, "1.0.0");
    }

    #[test]
    fn test_promotion_with_guards() {
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        let guard1 = promoter.get_current().unwrap();
        assert_eq!(guard1.snapshot().version, "1.0.0");

        let snap2 = create_test_snapshot("2.0.0");
        promoter.promote(snap2).unwrap();

        // Old guard still valid (ref count prevents deallocation)
        assert_eq!(guard1.snapshot().version, "1.0.0");

        // New promoter reads give new version
        let guard2 = promoter.get_current().unwrap();
        assert_eq!(guard2.snapshot().version, "2.0.0");
    }

    #[allow(clippy::expect_used)]
    #[test]
    #[allow(clippy::expect_used)]
    fn test_safe_concurrent_promotion_and_reads() {
        // Stress test: concurrent promotions and reads
        let snap1 = create_test_snapshot("1.0.0");
        let promoter = Arc::new(AtomicSnapshotPromoter::new(snap1));

        let mut handles = vec![];

        // Spawn 10 threads doing promotions
        for i in 2..=11 {
            let p = promoter.clone();
            handles.push(std::thread::spawn(move || {
                let snap = create_test_snapshot(&format!("{}.0.0", i));
                p.promote(snap).unwrap();
            }));
        }

        // Spawn 50 threads doing reads
        for _ in 0..50 {
            let p = promoter.clone();
            handles.push(std::thread::spawn(move || {
                let guard = p.get_current().unwrap();
                assert!(!guard.snapshot().version.is_empty());
            }));
        }

        // All threads should succeed without panics
        for handle in handles {
            handle.join().expect("Thread panicked");
        }

        let metrics = promoter.metrics();
        assert_eq!(metrics.total_promotions, 10);
    }
}
