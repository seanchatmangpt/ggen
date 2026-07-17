//! Feature-gated (`dhat-heap`) global-allocator wrapper for per-stage
//! allocation attribution in the DfCM crown suite (surface 7).
//!
//! This is a lightweight counting allocator, not the `dhat` crate: `dhat`
//! produces a JSON heap-profile file meant for its own viewer, which isn't a
//! good fit for programmatically filling in `DfcmBenchReceipt`'s
//! `alloc_count_by_stage`/`bytes_allocated_by_stage` fields per suite stage.
//! A direct global-atomic counter wrapping the system allocator gives exact,
//! structured counts with a two-line `snapshot()` call around each stage,
//! at the cost of not getting dhat's call-tree visualization — a reasonable
//! trade for this specific measurement need.
//!
//! Only compiled under the `dhat-heap` feature; normal builds/tests never
//! link this and never set a non-default global allocator.

#[cfg(feature = "dhat-heap")]
pub mod counting_alloc {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicU64, Ordering};

    static ALLOC_COUNT: AtomicU64 = AtomicU64::new(0);
    static ALLOC_BYTES: AtomicU64 = AtomicU64::new(0);

    /// Global allocator that counts every allocation's count and byte size
    /// before delegating to the system allocator. Install via
    /// `#[global_allocator] static ALLOC: CountingAlloc = CountingAlloc;`
    /// in a binary/bench crate root under `#[cfg(feature = "dhat-heap")]`.
    pub struct CountingAlloc;

    // SAFETY: delegates every operation to `System`, which is itself a
    // sound `GlobalAlloc` implementation; the only addition is a relaxed
    // atomic counter update, which cannot violate the allocator contract.
    unsafe impl GlobalAlloc for CountingAlloc {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
            ALLOC_BYTES.fetch_add(layout.size() as u64, Ordering::Relaxed);
            System.alloc(layout)
        }
        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            System.dealloc(ptr, layout)
        }
        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
            ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
            ALLOC_BYTES.fetch_add(new_size as u64, Ordering::Relaxed);
            System.realloc(ptr, layout, new_size)
        }
    }

    /// Current cumulative (allocation count, bytes allocated) since process start.
    /// Intended usage: snapshot before and after a stage, subtract.
    pub fn snapshot() -> (u64, u64) {
        (
            ALLOC_COUNT.load(Ordering::Relaxed),
            ALLOC_BYTES.load(Ordering::Relaxed),
        )
    }
}
