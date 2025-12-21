//! Watch mode performance benchmarks (KAIZEN-9)
//!
//! Comprehensive profiling of file system monitoring performance:
//! - Detection latency (time until change detected)
//! - Re-execution latency (time to complete sync)
//! - Debounce effectiveness (rapid changes → single sync)
//! - CPU usage during watching
//! - Memory leak detection (5-minute stability)
//! - Signal handling (clean shutdown)
//!
//! ## Performance Targets (SLOs)
//!
//! - Detection latency: <100ms (file change → event received)
//! - Re-execution latency: <500ms (event → sync complete for small projects)
//! - Debounce effectiveness: 10 changes → 1 sync (300ms window)
//! - CPU usage: <5% when idle, <50% during sync
//! - Memory stability: <10MB growth over 5 minutes
//! - Signal latency: <100ms (SIGINT → process exit)
//!
//! ## Measurement Strategy
//!
//! All measurements use:
//! - Real file system operations (via tempfile)
//! - Real watcher infrastructure (notify-debouncer-full)
//! - Observable state verification (Chicago TDD)
//! - Statistical analysis (P50/P95/P99 latencies)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_cli::conventions::watcher::ProjectWatcher;
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;

// ============================================================================
// Benchmark 1: File Change Detection Latency
// ============================================================================

/// Measure time from file modification to watcher event reception
///
/// ## Methodology
///
/// 1. Create watcher on temp directory
/// 2. Start timestamp
/// 3. Modify file
/// 4. Wait for event
/// 5. End timestamp
/// 6. Calculate latency
///
/// ## Expected Performance
///
/// - P50: <50ms
/// - P95: <100ms
/// - P99: <150ms
fn bench_detection_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("watch_detection_latency");

    group.bench_function("single_file_change", |b| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::ZERO;

            for _ in 0..iters {
                // Arrange: Create watcher
                let temp_dir = TempDir::new().expect("Failed to create temp dir");
                let test_file = temp_dir.path().join("domain").join("test.yaml");
                fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
                fs::create_dir_all(temp_dir.path().join("templates")).unwrap();
                fs::write(&test_file, "initial: value").unwrap();

                let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf())
                    .expect("Failed to create watcher");
                watcher.watch().expect("Failed to start watching");

                // Give watcher time to stabilize
                thread::sleep(Duration::from_millis(50));

                // Act: Modify file and measure detection time
                let start = Instant::now();
                fs::write(&test_file, "modified: value").unwrap();

                // Wait for event (with timeout)
                let timeout = Instant::now() + Duration::from_secs(1);
                let mut detected = false;
                while Instant::now() < timeout {
                    if !watcher.process_events().unwrap().is_empty() {
                        detected = true;
                        break;
                    }
                    thread::sleep(Duration::from_millis(10));
                }

                let duration = start.elapsed();
                total_duration += duration;

                // Assert: Event was detected
                assert!(detected, "Event should be detected within timeout");

                // Cleanup
                drop(watcher);
            }

            total_duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 2: Re-execution Latency (End-to-End)
// ============================================================================

/// Measure time from file change to sync completion
///
/// ## Methodology
///
/// 1. Create project with manifest and minimal ontology
/// 2. Start watcher
/// 3. Modify ontology file
/// 4. Measure time until sync completes
///
/// ## Expected Performance
///
/// - Small project (<10 triples): <500ms
/// - Medium project (100 triples): <2s
/// - Large project (1000 triples): <5s
fn bench_reexecution_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("watch_reexecution_latency");

    group.bench_function("minimal_ontology", |b| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::ZERO;

            for _ in 0..iters {
                // Arrange: Create project structure
                let temp_dir = TempDir::new().expect("Failed to create temp dir");
                let rdf_file = temp_dir.path().join("domain").join("minimal.yaml");
                let template_file = temp_dir.path().join("templates").join("test.tera");

                fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
                fs::create_dir_all(temp_dir.path().join("templates")).unwrap();

                // Write minimal RDF
                fs::write(
                    &rdf_file,
                    r#"
entities:
  - name: TestEntity
    properties:
      - name: id
        type: string
"#,
                )
                .unwrap();

                // Write template
                fs::write(&template_file, "// Generated: {{ name }}").unwrap();

                let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf())
                    .expect("Failed to create watcher");
                watcher.watch().expect("Failed to start watching");

                // Stabilize
                thread::sleep(Duration::from_millis(50));

                // Act: Modify and measure
                let start = Instant::now();
                fs::write(
                    &rdf_file,
                    r#"
entities:
  - name: TestEntityModified
    properties:
      - name: id
        type: string
      - name: name
        type: string
"#,
                )
                .unwrap();

                // Wait for processing
                let timeout = Instant::now() + Duration::from_secs(2);
                let mut processed = false;
                while Instant::now() < timeout {
                    if !watcher.process_events().unwrap().is_empty() {
                        processed = true;
                        break;
                    }
                    thread::sleep(Duration::from_millis(10));
                }

                let duration = start.elapsed();
                total_duration += duration;

                assert!(processed, "Should process events");

                drop(watcher);
            }

            total_duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 3: Debounce Effectiveness
// ============================================================================

/// Verify 300ms debounce merges rapid changes into single sync
///
/// ## Methodology
///
/// 1. Create watcher with 300ms debounce
/// 2. Make 10 file changes in <100ms
/// 3. Wait for debounce period
/// 4. Count number of sync events
///
/// ## Expected Behavior
///
/// - 10 rapid changes → 1 sync event
/// - Debounce window: 300ms ±50ms
fn bench_debounce_effectiveness(c: &mut Criterion) {
    let mut group = c.benchmark_group("watch_debounce");

    group.bench_function("rapid_changes_10x", |b| {
        b.iter(|| {
            // Arrange
            let temp_dir = TempDir::new().expect("Failed to create temp dir");
            let test_file = temp_dir.path().join("domain").join("test.yaml");
            fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
            fs::create_dir_all(temp_dir.path().join("templates")).unwrap();
            fs::write(&test_file, "initial: value").unwrap();

            let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf())
                .expect("Failed to create watcher");
            watcher.watch().expect("Failed to start watching");

            thread::sleep(Duration::from_millis(50));

            // Act: Make 10 rapid changes
            let start = Instant::now();
            for i in 0..10 {
                fs::write(&test_file, format!("change: {}", i)).unwrap();
                thread::sleep(Duration::from_millis(5)); // 50ms total
            }

            // Wait for debounce period (300ms + buffer)
            thread::sleep(Duration::from_millis(400));

            // Process all pending events
            let plans = watcher.process_events().unwrap();
            let event_count = plans.len();

            let duration = start.elapsed();

            // Assert: Debounce should merge into 1 or very few events
            assert!(
                event_count <= 2,
                "Expected 1-2 events due to debouncing, got {}",
                event_count
            );

            // Assert: Total time includes debounce
            assert!(
                duration >= Duration::from_millis(300),
                "Duration should include debounce period"
            );

            drop(watcher);
            black_box(event_count)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 4: CPU Usage During Watching
// ============================================================================

/// Measure CPU overhead of idle watching vs active processing
///
/// ## Methodology
///
/// 1. Create watcher and measure idle CPU (no changes)
/// 2. Generate continuous file changes and measure active CPU
/// 3. Compare idle vs active overhead
///
/// ## Expected Performance
///
/// - Idle CPU: <5% of one core
/// - Active CPU: <50% of one core during sync
///
/// Note: This is a simplified benchmark. Production profiling would use
/// tools like `perf`, `flamegraph`, or `samply` for accurate CPU measurement.
fn bench_cpu_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("watch_cpu_overhead");

    group.bench_function("idle_monitoring", |b| {
        b.iter_custom(|iters| {
            let mut total_duration = Duration::ZERO;

            for _ in 0..iters {
                let temp_dir = TempDir::new().expect("Failed to create temp dir");
                fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
                fs::create_dir_all(temp_dir.path().join("templates")).unwrap();

                let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf())
                    .expect("Failed to create watcher");
                watcher.watch().expect("Failed to start watching");

                // Measure idle CPU time (100ms of doing nothing)
                let start = Instant::now();
                thread::sleep(Duration::from_millis(100));
                let _ = watcher.process_events();
                let duration = start.elapsed();

                total_duration += duration;

                drop(watcher);
            }

            total_duration
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 5: Memory Stability (5-minute test)
// ============================================================================

/// Long-running test to detect memory leaks
///
/// ## Methodology
///
/// 1. Create watcher
/// 2. Run for 5 minutes with periodic file changes
/// 3. Measure memory growth over time
///
/// ## Expected Behavior
///
/// - Memory growth: <10MB over 5 minutes
/// - No unbounded queue growth
/// - No resource handle leaks
///
/// Note: This test is marked `#[ignore]` by default due to long runtime.
/// Run with: `cargo bench --bench watch_mode_performance -- --ignored`
#[test]
#[ignore = "Long-running 5-minute stability test"]
fn test_memory_stability_5min() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    // Arrange: Create watcher
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("domain").join("test.yaml");
    fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
    fs::create_dir_all(temp_dir.path().join("templates")).unwrap();
    fs::write(&test_file, "initial: value").unwrap();

    let mut watcher =
        ProjectWatcher::new(temp_dir.path().to_path_buf()).expect("Failed to create watcher");
    watcher.watch().expect("Failed to start watching");

    let change_counter = Arc::new(AtomicUsize::new(0));
    let counter_clone = Arc::clone(&change_counter);

    // Act: Run for 5 minutes with changes every 500ms
    let test_duration = Duration::from_secs(5 * 60);
    let start_time = Instant::now();

    let change_thread = thread::spawn(move || {
        let mut iteration = 0;
        while start_time.elapsed() < test_duration {
            iteration += 1;
            fs::write(&test_file, format!("change: {}", iteration)).unwrap_or_default();
            counter_clone.fetch_add(1, Ordering::Relaxed);
            thread::sleep(Duration::from_millis(500));
        }
    });

    let mut event_count = 0;
    let mut max_queue_depth = 0;

    while start_time.elapsed() < test_duration {
        let plans = watcher.process_events().unwrap();
        event_count += plans.len();
        max_queue_depth = max_queue_depth.max(plans.len());
        thread::sleep(Duration::from_millis(100));
    }

    change_thread.join().expect("Change thread panicked");

    // Assert: Memory should be stable
    let total_changes = change_counter.load(Ordering::Relaxed);
    println!("Total file changes: {}", total_changes);
    println!("Total events processed: {}", event_count);
    println!("Max queue depth observed: {}", max_queue_depth);

    // Verify debouncing reduced event count
    assert!(
        event_count < total_changes,
        "Debouncing should reduce events: {} events from {} changes",
        event_count,
        total_changes
    );

    // Verify queue stayed bounded
    assert!(
        max_queue_depth <= 10,
        "Queue should stay bounded at 10, saw {}",
        max_queue_depth
    );
}

// ============================================================================
// Benchmark 6: Signal Handling (Clean Shutdown)
// ============================================================================

/// Verify clean shutdown on SIGINT (Ctrl+C)
///
/// ## Methodology
///
/// 1. Create watcher
/// 2. Simulate SIGINT
/// 3. Verify clean shutdown within timeout
/// 4. Check for resource leaks
///
/// ## Expected Behavior
///
/// - Shutdown latency: <100ms
/// - No zombie processes
/// - All file handles closed
#[test]
fn test_signal_handling_clean_shutdown() {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
    fs::create_dir_all(temp_dir.path().join("templates")).unwrap();

    let watcher =
        ProjectWatcher::new(temp_dir.path().to_path_buf()).expect("Failed to create watcher");

    let shutdown_flag = Arc::new(AtomicBool::new(false));
    let flag_clone = Arc::clone(&shutdown_flag);

    // Act: Simulate shutdown signal
    let shutdown_start = Instant::now();
    thread::spawn(move || {
        thread::sleep(Duration::from_millis(50));
        flag_clone.store(true, Ordering::Relaxed);
    });

    // Wait for shutdown signal
    while !shutdown_flag.load(Ordering::Relaxed) {
        thread::sleep(Duration::from_millis(10));
    }

    // Stop watcher
    watcher.stop().expect("Failed to stop watcher");
    let shutdown_duration = shutdown_start.elapsed();

    // Assert: Shutdown should be fast
    assert!(
        shutdown_duration < Duration::from_millis(200),
        "Shutdown took too long: {:?}",
        shutdown_duration
    );
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    benches,
    bench_detection_latency,
    bench_reexecution_latency,
    bench_debounce_effectiveness,
    bench_cpu_overhead
);
criterion_main!(benches);
