use lazy_static::lazy_static;
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};
use tokio::runtime::{Builder, Runtime};

// Custom allocator to track memory usage
struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = System.alloc(layout);
        if !ret.is_null() {
            ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        }
        ret
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        DEALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
    }
}

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;

pub fn get_current_memory_usage() -> usize {
    ALLOCATED.load(Ordering::SeqCst) - DEALLOCATED.load(Ordering::SeqCst)
}

pub fn reset_memory_tracking() {
    ALLOCATED.store(0, Ordering::SeqCst);
    DEALLOCATED.store(0, Ordering::SeqCst);
}

// Memory profiling tests
#[cfg(test)]
mod memory_tests {
    #[allow(unused_imports)]
    use super::*;

    async fn sample_async_work() -> String {
        let mut result = String::new();
        for i in 0..100 {
            result.push_str(&format!("data_{}\n", i));
        }
        result
    }

    #[test]
    fn profile_new_runtime_memory() {
        reset_memory_tracking();
        let baseline = get_current_memory_usage();

        // Create and use new runtime
        {
            let rt = Runtime::new().unwrap();
            let _result = rt.block_on(sample_async_work());
        }

        let peak = get_current_memory_usage();
        let leaked = get_current_memory_usage() - baseline;

        println!("New Runtime Memory Profile:");
        println!("  Baseline: {} bytes", baseline);
        println!("  Peak: {} bytes", peak);
        println!("  Leaked: {} bytes", leaked);
        println!("  Peak overhead: {} KB", (peak - baseline) / 1024);
    }

    #[test]
    fn profile_shared_runtime_memory() {
        lazy_static! {
            static ref SHARED_RT: Runtime = Runtime::new().unwrap();
        }

        reset_memory_tracking();
        let baseline = get_current_memory_usage();

        // Use shared runtime multiple times
        for _ in 0..10 {
            let _result = SHARED_RT.block_on(sample_async_work());
        }

        let peak = get_current_memory_usage();

        println!("Shared Runtime Memory Profile:");
        println!("  Baseline: {} bytes", baseline);
        println!("  Peak (10 executions): {} bytes", peak);
        println!("  Per-execution overhead: {} bytes", (peak - baseline) / 10);
    }

    #[test]
    fn profile_runtime_creation_memory() {
        let mut measurements = Vec::new();

        for _ in 0..5 {
            reset_memory_tracking();
            let before = get_current_memory_usage();

            let rt = Runtime::new().unwrap();

            let after = get_current_memory_usage();
            measurements.push(after - before);

            drop(rt);
        }

        let avg_overhead: usize = measurements.iter().sum::<usize>() / measurements.len();

        println!("Runtime Creation Memory Overhead:");
        println!("  Measurements: {:?}", measurements);
        println!("  Average: {} KB", avg_overhead / 1024);
        println!("  Min: {} KB", measurements.iter().min().unwrap() / 1024);
        println!("  Max: {} KB", measurements.iter().max().unwrap() / 1024);
    }

    #[test]
    fn profile_multi_thread_runtime_memory() {
        reset_memory_tracking();
        let baseline = get_current_memory_usage();

        let rt = Builder::new_multi_thread()
            .worker_threads(4)
            .enable_all()
            .build()
            .unwrap();

        let after_creation = get_current_memory_usage();

        rt.block_on(async {
            for _ in 0..10 {
                let _result = sample_async_work().await;
            }
        });

        let after_execution = get_current_memory_usage();

        println!("Multi-thread Runtime Memory Profile:");
        println!("  Baseline: {} bytes", baseline);
        println!("  After creation: {} bytes", after_creation);
        println!("  After execution: {} bytes", after_execution);
        println!(
            "  Creation overhead: {} KB",
            (after_creation - baseline) / 1024
        );
        println!(
            "  Execution overhead: {} KB",
            (after_execution - after_creation) / 1024
        );

        drop(rt);

        let after_drop = get_current_memory_usage();
        println!("  After drop: {} bytes", after_drop);
        println!(
            "  Memory reclaimed: {} KB",
            (after_execution - after_drop) / 1024
        );
    }

    #[test]
    fn profile_current_thread_runtime_memory() {
        reset_memory_tracking();
        let baseline = get_current_memory_usage();

        let rt = Builder::new_current_thread().enable_all().build().unwrap();

        let after_creation = get_current_memory_usage();

        rt.block_on(async {
            for _ in 0..10 {
                let _result = sample_async_work().await;
            }
        });

        let after_execution = get_current_memory_usage();

        println!("Current-thread Runtime Memory Profile:");
        println!("  Baseline: {} bytes", baseline);
        println!("  After creation: {} bytes", after_creation);
        println!("  After execution: {} bytes", after_execution);
        println!(
            "  Creation overhead: {} KB",
            (after_creation - baseline) / 1024
        );
        println!(
            "  Execution overhead: {} KB",
            (after_execution - after_creation) / 1024
        );
    }
}

// Comparative analysis
pub fn run_memory_comparison() {
    println!("\n=== Memory Usage Comparison ===\n");

    // Option A: New runtime per command
    println!("Option A: New Runtime Per Command");
    {
        reset_memory_tracking();
        let start = get_current_memory_usage();

        for _ in 0..10 {
            let rt = Runtime::new().unwrap();
            rt.block_on(async {
                let mut data = String::new();
                for i in 0..100 {
                    data.push_str(&format!("line_{}\n", i));
                }
                data
            });
        }

        let end = get_current_memory_usage();
        println!("  Total for 10 commands: {} KB", (end - start) / 1024);
        println!("  Average per command: {} KB", (end - start) / 10 / 1024);
    }

    // Option B: Shared static runtime
    println!("\nOption B: Shared Static Runtime");
    {
        lazy_static! {
            static ref SHARED: Runtime = Runtime::new().unwrap();
        }

        reset_memory_tracking();
        let start = get_current_memory_usage();

        for _ in 0..10 {
            SHARED.block_on(async {
                let mut data = String::new();
                for i in 0..100 {
                    data.push_str(&format!("line_{}\n", i));
                }
                data
            });
        }

        let end = get_current_memory_usage();
        println!("  Total for 10 commands: {} KB", (end - start) / 1024);
        println!("  Average per command: {} KB", (end - start) / 10 / 1024);
    }

    // Option C: Lazy static runtime
    println!("\nOption C: Lazy Static Runtime");
    {
        lazy_static! {
            static ref LAZY: Runtime = Builder::new_multi_thread()
                .worker_threads(4)
                .enable_all()
                .build()
                .unwrap();
        }

        reset_memory_tracking();
        let start = get_current_memory_usage();

        for _ in 0..10 {
            LAZY.block_on(async {
                let mut data = String::new();
                for i in 0..100 {
                    data.push_str(&format!("line_{}\n", i));
                }
                data
            });
        }

        let end = get_current_memory_usage();
        println!("  Total for 10 commands: {} KB", (end - start) / 1024);
        println!("  Average per command: {} KB", (end - start) / 10 / 1024);
    }
}

#[cfg(test)]
mod integration_tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn run_all_memory_comparisons() {
        run_memory_comparison();
    }
}
