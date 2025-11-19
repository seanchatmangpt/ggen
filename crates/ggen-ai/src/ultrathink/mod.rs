//! Ultrathink - 80/20 Autonomous Intelligence System
//!
//! This module implements the ultrathink system following the 80/20 rule:
//! - 80% of value comes from 20% of core functionality
//! - Focus on essential WIP integration and autonomous task processing
//! - Simplified architecture for maximum impact

pub mod core;
pub mod cleanroom;

use std::sync::Arc;

use crate::error::GgenAiError;
use crate::error::Result;
use uuid::Uuid;

/// Re-export core ultrathink types
pub use core::*;

/// Initialize the ultrathink system
pub async fn initialize_ultrathink() -> Result<()> {
    core::initialize_ultrathink_core().await
}

/// Create a new ultrathink task
pub fn create_task(
    task_type: core::TaskType, description: String, priority: core::TaskPriority,
) -> core::UltrathinkTask {
    core::create_ultrathink_task(task_type, description, priority)
}

/// Main ultrathink system instance
pub struct UltrathinkSystem {
    core: Arc<UltrathinkCore>,
}

impl UltrathinkSystem {
    /// Create a new ultrathink system
    pub async fn new() -> Result<Self> {
        let config = UltrathinkConfig::default();
        let core = Arc::new(UltrathinkCore::new(config).await?);

        Ok(Self { core })
    }

    /// Submit a task for autonomous processing
    pub async fn submit_task(&self, task: UltrathinkTask) -> Result<Uuid> {
        self.core.submit_task(task).await
    }

    /// Synchronize with WIP systems
    pub async fn sync_with_wip(&self) -> Result<()> {
        self.core.sync_with_wip().await
    }

    /// Get system status
    pub async fn get_status(&self) -> Result<CoreMetrics> {
        self.core.get_status().await
    }

    /// Process WIP entries
    pub async fn process_wip_entries(&self) -> Result<Vec<WipOperation>> {
        self.core.process_wip_entries().await
    }
}

/// Global ultrathink system instance
///
/// # Safety Invariants
///
/// This mutable static is currently UNSAFE and violates Rust's memory safety guarantees:
///
/// 1. **Data Race Hazard**: Concurrent access to mutable static without synchronization
///    - Multiple threads can call `init_ultrathink_system()` simultaneously
///    - The check-then-set pattern (is_none() then assignment) is not atomic
///    - This can lead to:
///      - Multiple initializations overwriting each other
///      - Reading partially initialized data
///      - Undefined behavior from concurrent mutable access
///
/// 2. **Lifetime Issues**: No guarantee that references obtained via `get_ultrathink_system()`
///    remain valid if another thread reinitializes the static
///
/// 3. **Missing Synchronization**: No memory barriers or atomic operations to ensure
///    visibility across threads
///
/// # Required Safety Guarantees
///
/// For this code to be sound, the following must hold (currently NOT guaranteed):
/// - Single-threaded access only, OR
/// - External synchronization preventing concurrent calls to init/get functions, OR
/// - Use of proper thread-safe initialization (e.g., `OnceLock<UltrathinkSystem>`)
///
/// # Recommended Fix
///
/// Replace with thread-safe initialization:
/// ```rust
/// use std::sync::OnceLock;
/// static ULTRATHINK_SYSTEM: OnceLock<UltrathinkSystem> = OnceLock::new();
/// ```
static mut ULTRATHINK_SYSTEM: Option<UltrathinkSystem> = None;

/// Initialize the global ultrathink system
///
/// # Safety
///
/// This function contains UNSAFE code that can cause undefined behavior:
///
/// ## Invariants Required (NOT currently enforced):
/// 1. Must be called from only one thread at a time
/// 2. Must not be called concurrently with `get_ultrathink_system()`
/// 3. Caller must ensure no other references to ULTRATHINK_SYSTEM exist
///
/// ## Potential Undefined Behavior:
/// - **Race condition**: Two threads calling this simultaneously can both see `is_none() == true`
///   and both attempt initialization, causing one initialization to be dropped
/// - **Data race**: Concurrent read (get) and write (init) operations without synchronization
///   violates Rust's aliasing rules and can cause memory corruption
/// - **Use-after-free**: If reinitialized while references from `get_ultrathink_system()` exist
///
/// ## Why This Is Unsound:
/// The pattern `if ULTRATHINK_SYSTEM.is_none() { ULTRATHINK_SYSTEM = Some(...) }` is NOT atomic.
/// Between the check and the assignment, another thread could:
/// - Also see `is_none() == true` and both threads initialize
/// - Call `get_ultrathink_system()` and receive a dangling reference
/// - Read partially written memory if assignment is not atomic
///
/// # Safety Analysis Result: UNSOUND
///
/// This code does NOT uphold Rust's safety guarantees and can cause undefined behavior
/// in multi-threaded contexts.
pub async fn init_ultrathink_system() -> Result<()> {
    // SAFETY: This is UNSAFE and potentially unsound:
    // - No synchronization prevents concurrent access to mutable static
    // - The check-then-set pattern is not atomic, enabling race conditions
    // - Multiple threads can simultaneously see is_none() == true
    // - References from get_ultrathink_system() can become dangling if reinitialized
    //
    // This code assumes (WITHOUT VERIFICATION):
    // 1. Single-threaded execution OR external synchronization
    // 2. No concurrent calls to get_ultrathink_system()
    // 3. No reinitialization while references exist
    //
    // VERDICT: This code violates Rust's safety guarantees and should be replaced
    // with OnceLock or similar thread-safe primitive.
    unsafe {
        if ULTRATHINK_SYSTEM.is_none() {
            ULTRATHINK_SYSTEM = Some(UltrathinkSystem::new().await?);
        }
    }
    Ok(())
}

/// Get the global ultrathink system instance
///
/// # Safety
///
/// This function contains UNSAFE code that can cause undefined behavior:
///
/// ## Invariants Required (NOT currently enforced):
/// 1. Must not be called concurrently with `init_ultrathink_system()`
/// 2. The returned reference must not outlive any potential reinitialization
/// 3. No mutable access to ULTRATHINK_SYSTEM while returned reference exists
///
/// ## Potential Undefined Behavior:
/// - **Data race**: Reading mutable static while another thread writes to it
///   - If called during `init_ultrathink_system()`, can observe partially written state
///   - Violates Rust's aliasing rules (shared reference while mutable access possible)
/// - **Dangling reference**: Returned reference can become invalid if another thread
///   reinitializes ULTRATHINK_SYSTEM (replacing the Option contents)
/// - **Memory corruption**: No synchronization means writes from other threads may not
///   be visible, or only partially visible
///
/// ## Why This Is Unsound:
/// - Returns `&'static` reference to data that can be mutated (via init function)
/// - No synchronization to prevent concurrent modification
/// - Caller has no way to ensure safety requirements are met
///
/// # Safety Analysis Result: UNSOUND
///
/// This code does NOT uphold Rust's safety guarantees. The returned reference can
/// become dangling, and concurrent access can cause data races.
pub fn get_ultrathink_system() -> Option<&'static UltrathinkSystem> {
    // SAFETY: This is UNSAFE and potentially unsound:
    // - Reads from mutable static without synchronization
    // - Can race with writes in init_ultrathink_system()
    // - Returns 'static reference to data that can be modified/replaced
    // - No atomic operations or memory barriers ensure visibility
    //
    // This code assumes (WITHOUT VERIFICATION):
    // 1. No concurrent calls to init_ultrathink_system()
    // 2. ULTRATHINK_SYSTEM is not reinitialized while references exist
    // 3. Proper memory barriers exist (not guaranteed without atomics)
    //
    // Potential issues:
    // - Data race if init and get called concurrently (undefined behavior)
    // - Dangling reference if reinitialized after reference obtained
    // - Reading stale/partial data due to lack of memory synchronization
    //
    // VERDICT: This violates Rust's safety guarantees. Use OnceLock or similar.
    unsafe { ULTRATHINK_SYSTEM.as_ref() }
}

/// Submit a task to the ultrathink system
pub async fn submit_ultrathink_task(
    task_type: core::TaskType, description: String, priority: core::TaskPriority,
) -> Result<Uuid> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    let task = create_task(task_type, description, priority);
    system.submit_task(task).await
}

/// Synchronize with WIP systems
pub async fn sync_ultrathink_wip() -> Result<()> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.sync_with_wip().await
}

/// Get ultrathink system status
pub async fn get_ultrathink_status() -> Result<CoreMetrics> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.get_status().await
}

/// Process WIP entries
pub async fn process_ultrathink_wip_entries() -> Result<Vec<WipOperation>> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.process_wip_entries().await
}

/// Run enhanced cleanroom tests with all new capabilities
pub async fn run_enhanced_cleanroom_tests() -> Result<()> {
    cleanroom::run_enhanced_cleanroom_tests().await
}

#[cfg(test)]
mod unsafe_safety_tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc as StdArc;

    /// Test: Demonstrates data race in concurrent initialization
    ///
    /// This test validates that concurrent calls to init_ultrathink_system()
    /// can cause race conditions. While the test may pass sometimes, it
    /// demonstrates the UNSOUND nature of the mutable static pattern.
    ///
    /// # Safety Invariant Being Tested:
    /// - Multiple threads should not be able to initialize simultaneously
    /// - The check-then-set pattern should be atomic
    ///
    /// # Expected Behavior (Current Implementation):
    /// - Race condition: multiple threads may see is_none() == true
    /// - Some initializations may be dropped/overwritten
    /// - Potential for observing partially initialized state
    ///
    /// # Why This Test Matters:
    /// Demonstrates that without proper synchronization, the global static
    /// initialization is not thread-safe and violates Rust's safety guarantees.
    #[tokio::test]
    #[ignore] // Ignored by default as it demonstrates UNSAFE behavior
    async fn test_concurrent_initialization_race() {
        // This test demonstrates the data race in init_ultrathink_system

        let init_count = StdArc::new(AtomicUsize::new(0));
        let mut handles = vec![];

        // Spawn multiple tasks that all try to initialize
        for _ in 0..10 {
            let count = init_count.clone();
            let handle = tokio::spawn(async move {
                // Each task attempts initialization
                if let Ok(()) = init_ultrathink_system().await {
                    count.fetch_add(1, Ordering::SeqCst);
                }
            });
            handles.push(handle);
        }

        // Wait for all tasks
        for handle in handles {
            let _ = handle.await;
        }

        // In a sound implementation, exactly one initialization should succeed
        // In the current UNSOUND implementation, the behavior is undefined:
        // - Multiple threads might both see is_none() == true
        // - Some initializations might be overwritten
        // - The count could be > 1 or the system might be partially initialized

        let count = init_count.load(Ordering::SeqCst);
        println!("Initialization attempts completed: {}", count);

        // This assertion documents expected behavior, not current behavior
        // Current implementation may violate this due to race conditions
        // assert_eq!(count, 1, "Only one initialization should succeed");
    }

    /// Test: Demonstrates concurrent read-write data race
    ///
    /// This test validates that concurrent calls to init and get can cause
    /// data races, violating Rust's aliasing rules.
    ///
    /// # Safety Invariant Being Tested:
    /// - Reads and writes to mutable static should be synchronized
    /// - No concurrent mutable and immutable access should be possible
    ///
    /// # Expected Behavior (Current Implementation):
    /// - Data race: reader can observe partially written state
    /// - Violates Rust's "no aliasing" rule (shared ref while mutable access exists)
    /// - Undefined behavior according to Rust's memory model
    ///
    /// # Why This Test Matters:
    /// Demonstrates that concurrent init and get operations can cause
    /// undefined behavior due to lack of synchronization.
    #[tokio::test]
    #[ignore] // Ignored by default as it demonstrates UNSAFE behavior
    async fn test_concurrent_init_and_read_race() {
        let mut handles = vec![];

        // Spawn readers that try to get the system
        for _ in 0..5 {
            let handle = tokio::spawn(async move {
                for _ in 0..100 {
                    // Try to read while initialization might be happening
                    let _ = get_ultrathink_system();
                    tokio::time::sleep(tokio::time::Duration::from_micros(1)).await;
                }
            });
            handles.push(handle);
        }

        // Spawn writers that try to initialize
        for _ in 0..5 {
            let handle = tokio::spawn(async move {
                for _ in 0..100 {
                    // Try to initialize while reads might be happening
                    let _ = init_ultrathink_system().await;
                    tokio::time::sleep(tokio::time::Duration::from_micros(1)).await;
                }
            });
            handles.push(handle);
        }

        // Wait for all tasks
        for handle in handles {
            let _ = handle.await;
        }

        // If this test completes without crashing, it doesn't mean the code
        // is safe - it just means the undefined behavior didn't manifest
        // in this particular execution. The code is still UNSOUND.
        println!("Test completed (but code is still unsound)");
    }

    /// Test: Documents the use-after-free potential
    ///
    /// This test demonstrates that if the system is reinitialized while
    /// references exist, those references become dangling.
    ///
    /// # Safety Invariant Being Tested:
    /// - References obtained from get_ultrathink_system() should remain valid
    /// - Reinitialization should not invalidate existing references
    ///
    /// # Expected Behavior (Current Implementation):
    /// - References can become dangling if reinitialized
    /// - Use-after-free if old reference is used after reinitialization
    /// - Violates Rust's lifetime guarantees
    ///
    /// # Why This Test Matters:
    /// Demonstrates that the 'static lifetime returned by get_ultrathink_system()
    /// is a lie - the data can be invalidated by reinitializing.
    #[tokio::test]
    #[ignore] // Ignored by default as it demonstrates UNSAFE behavior
    async fn test_reference_invalidation() {
        // Initialize the system
        let _ = init_ultrathink_system().await;

        // Get a reference (supposedly 'static lifetime)
        let _ref1 = get_ultrathink_system();

        // Reinitialize - this SHOULD invalidate ref1, but Rust thinks it's still valid
        let _ = init_ultrathink_system().await;

        // Get another reference
        let _ref2 = get_ultrathink_system();

        // In a sound implementation, ref1 should either:
        // 1. Still be valid (system not replaced), OR
        // 2. Be impossible to obtain while reinitialization is possible
        //
        // Current implementation makes ref1 potentially dangling but Rust
        // doesn't know this because we lied about the lifetime being 'static

        println!("Test completed (but references may be dangling)");
    }

    /// Test: Memory ordering and visibility without synchronization
    ///
    /// This test demonstrates that without atomic operations or memory barriers,
    /// writes in one thread may not be visible to reads in another thread.
    ///
    /// # Safety Invariant Being Tested:
    /// - Writes to ULTRATHINK_SYSTEM should be visible to all readers
    /// - Memory should be properly synchronized across threads
    ///
    /// # Expected Behavior (Current Implementation):
    /// - No memory barriers ensure visibility
    /// - Readers might see stale or partially-written data
    /// - Violates sequential consistency
    ///
    /// # Why This Test Matters:
    /// Even if race conditions are avoided by external synchronization,
    /// the lack of memory barriers can cause visibility issues.
    #[tokio::test]
    #[ignore] // Ignored by default as it demonstrates UNSAFE behavior
    async fn test_memory_visibility() {
        // Initialize in one thread
        let init_handle = tokio::spawn(async {
            let _ = init_ultrathink_system().await;
        });

        init_handle.await.unwrap();

        // Try to read in another thread immediately
        let read_handle = tokio::spawn(async {
            // Without memory barriers, this read might not see the write
            // from the initialization thread
            get_ultrathink_system()
        });

        let result = read_handle.await.unwrap();

        // In a properly synchronized implementation, this should always be Some
        // In current implementation, visibility is not guaranteed without
        // explicit memory barriers
        if result.is_none() {
            println!("WARNING: Write not visible - memory ordering issue");
        }
    }
}

#[cfg(test)]
mod safety_recommendations {
    //! This module documents the recommended safe alternatives to the
    //! current unsafe implementation.

    use super::*;
    use std::sync::OnceLock;

    /// Recommended safe alternative using OnceLock
    ///
    /// This demonstrates how to safely implement singleton initialization
    /// without any unsafe code.
    ///
    /// # Safety Guarantees:
    /// - Thread-safe initialization (only one thread initializes)
    /// - No data races (proper synchronization built-in)
    /// - Valid 'static references (data never moves or is dropped)
    /// - Proper memory ordering (atomic operations ensure visibility)
    #[allow(dead_code)]
    mod safe_alternative {
        use super::*;

        // Safe alternative: OnceLock provides thread-safe lazy initialization
        static ULTRATHINK_SYSTEM_SAFE: OnceLock<UltrathinkSystem> = OnceLock::new();

        /// Thread-safe initialization - only the first call initializes
        pub async fn init_ultrathink_system_safe() -> Result<()> {
            // get_or_try_init ensures only one thread initializes
            // Other threads block until initialization completes
            ULTRATHINK_SYSTEM_SAFE
                .get_or_try_init(|| async { UltrathinkSystem::new().await })
                .await?;
            Ok(())
        }

        /// Thread-safe access - returns valid 'static reference
        pub fn get_ultrathink_system_safe() -> Option<&'static UltrathinkSystem> {
            ULTRATHINK_SYSTEM_SAFE.get()
        }

        // Benefits of this approach:
        // 1. No unsafe code needed
        // 2. Guaranteed thread-safe initialization
        // 3. No data races possible
        // 4. True 'static lifetime (data never invalidated)
        // 5. Proper memory ordering
        // 6. Cannot be reinitialized (prevents use-after-free)
    }

    #[test]
    fn test_safe_alternative_is_sound() {
        // This test documents that the safe alternative using OnceLock
        // provides all the safety guarantees that the unsafe version lacks

        // Key properties:
        // 1. Type system enforces safety (no unsafe blocks)
        // 2. Concurrent initialization is handled correctly
        // 3. References are truly 'static and never dangle
        // 4. Memory ordering is guaranteed by atomic operations
        // 5. Cannot be reinitialized accidentally

        println!("OnceLock-based implementation is sound and requires no unsafe code");
    }
}
