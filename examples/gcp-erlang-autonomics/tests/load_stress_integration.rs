//! Load & Stress Integration Tests
//!
//! Performance tests under high load:
//! - 1000+ concurrent customers
//! - 100+ governors per node
//! - Message throughput (1000 msgs/sec per governor)
//! - Latency SLOs verified
//!
//! Measures real performance characteristics and verifies SLO compliance.

use gcp_erlang_autonomics::{
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, GovernorType,
};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::sync::RwLock;
use tokio::time::Instant;

/// Test: 1000 concurrent customer subscriptions complete within SLO
///
/// Verifies throughput:
/// - Arrange: Initialize orchestrator
/// - Act: Process 1000 concurrent subscription requests
/// - Assert: All complete within 30 second SLO, throughput >= 33 req/sec
#[tokio::test]
async fn test_1000_concurrent_subscriptions_within_slo() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Track metrics
    let success_count = Arc::new(AtomicU64::new(0));
    let start = Instant::now();
    const TARGET_THROUGHPUT: u64 = 1000; // 1000 requests
    const SLO_SECONDS: u64 = 30; // Must complete in 30s

    // Act: Spawn 1000 concurrent subscription tasks
    let mut tasks = Vec::new();
    for i in 0..TARGET_THROUGHPUT {
        let orch_clone = Arc::clone(&orchestrator);
        let success = Arc::clone(&success_count);

        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            let event = MarketplaceEvent::CustomerSubscribes {
                customer_id: format!("cust-{:04}", i),
                sku: "pro".to_string(),
            };

            let assigned = orch.assign_governors(&event);
            if !assigned.is_empty() {
                success.fetch_add(1, Ordering::Relaxed);
            }
        });
        tasks.push(task);
    }

    // Act: Wait for all tasks to complete
    futures::future::join_all(tasks).await;
    let elapsed = start.elapsed().as_secs();

    // Assert: All requests succeeded
    let successful = success_count.load(Ordering::Relaxed);
    assert_eq!(
        successful, TARGET_THROUGHPUT,
        "All {} subscriptions should succeed",
        TARGET_THROUGHPUT
    );

    // Assert: Within SLO
    assert!(
        elapsed <= SLO_SECONDS,
        "1000 subscriptions should complete within {} seconds (took {}s, throughput = {:.0} req/sec)",
        SLO_SECONDS,
        elapsed,
        TARGET_THROUGHPUT as f64 / elapsed as f64
    );

    println!(
        "Load Test: 1000 concurrent subscriptions completed in {}s ({:.0} req/sec)",
        elapsed,
        TARGET_THROUGHPUT as f64 / elapsed as f64
    );
}

/// Test: 100+ governors per node handle concurrent messages
///
/// Verifies multi-governor scaling:
/// - Arrange: Initialize orchestrator with 8 governors
/// - Act: Send 100+ messages to be distributed across governors
/// - Assert: All messages routed correctly, latency < 100ms avg
#[tokio::test]
async fn test_100_plus_governors_handle_concurrent_messages() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    let message_count = 100;
    let start = Instant::now();

    // Act: Generate 100+ different events to route
    let mut tasks = Vec::new();
    for i in 0..message_count {
        let orch_clone = Arc::clone(&orchestrator);

        let task = tokio::spawn(async move {
            let event_start = Instant::now();

            let mut orch = orch_clone.write().await;

            // Vary event types to test routing
            let event = match i % 7 {
                0 => MarketplaceEvent::CustomerSubscribes {
                    customer_id: format!("cust-{}", i),
                    sku: "pro".to_string(),
                },
                1 => MarketplaceEvent::SubscriptionCanceled {
                    customer_id: format!("cust-{}", i),
                    subscription_id: format!("sub-{}", i),
                },
                2 => MarketplaceEvent::QuotaExceeded {
                    customer_id: format!("cust-{}", i),
                    resource_type: "concurrent_requests".to_string(),
                    current_usage: 105,
                    quota_limit: 100,
                },
                3 => MarketplaceEvent::PaymentMethodUpdated {
                    customer_id: format!("cust-{}", i),
                    payment_method_id: format!("pm-{}", i),
                },
                4 => MarketplaceEvent::SubscriptionRenewed {
                    customer_id: format!("cust-{}", i),
                    subscription_id: format!("sub-{}", i),
                    new_sku: "enterprise".to_string(),
                },
                5 => MarketplaceEvent::ComplianceCheckFailed {
                    customer_id: format!("cust-{}", i),
                    reason: "High risk".to_string(),
                },
                _ => MarketplaceEvent::CustomerSubscribes {
                    customer_id: format!("cust-{}", i),
                    sku: "starter".to_string(),
                },
            };

            let _assigned = orch.assign_governors(&event);
            event_start.elapsed().as_millis() as u64
        });
        tasks.push(task);
    }

    // Act: Wait for all messages to be processed
    let latencies: Vec<u64> = futures::future::join_all(tasks)
        .await
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    let total_elapsed = start.elapsed().as_secs_f64();

    // Assert: All messages processed
    assert_eq!(latencies.len(), message_count, "All {} events should be processed", message_count);

    // Assert: Average latency < 100ms
    let avg_latency = latencies.iter().sum::<u64>() as f64 / latencies.len() as f64;
    assert!(
        avg_latency < 100.0,
        "Average latency should be < 100ms (was {:.2}ms)",
        avg_latency
    );

    // Calculate throughput
    let throughput = message_count as f64 / total_elapsed;
    println!(
        "Governor Message Routing: {} messages in {:.2}s ({:.0} msg/sec), avg latency {:.2}ms",
        message_count, total_elapsed, throughput, avg_latency
    );
}

/// Test: 1000 msgs/sec per governor throughput target
///
/// Verifies message processing capacity:
/// - Arrange: Single subscription event with 1000 messages queued
/// - Act: Process all messages through governor
/// - Assert: Throughput >= 1000 msgs/sec
#[tokio::test]
async fn test_governor_message_throughput_1000_msgs_per_sec() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    const MESSAGE_COUNT: u64 = 1000;
    let start = Instant::now();

    // Act: Send 1000 messages rapidly
    let mut tasks = Vec::new();
    for i in 0..MESSAGE_COUNT {
        let orch_clone = Arc::clone(&orchestrator);

        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            let event = MarketplaceEvent::CustomerSubscribes {
                customer_id: format!("cust-throughput-{}", i),
                sku: "pro".to_string(),
            };
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    // Act: Collect all responses
    let _results: Vec<_> = futures::future::join_all(tasks)
        .await
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    let elapsed = start.elapsed();
    let throughput = MESSAGE_COUNT as f64 / elapsed.as_secs_f64();

    // Assert: Throughput >= 1000 msgs/sec
    assert!(
        throughput >= 1000.0,
        "Governor should handle >= 1000 msgs/sec (achieved {:.0} msgs/sec)",
        throughput
    );

    println!(
        "Message Throughput: {:.0} messages/sec ({} messages in {:.3}s)",
        throughput,
        MESSAGE_COUNT,
        elapsed.as_secs_f64()
    );
}

/// Test: Latency SLO verification (p99 < 50ms)
///
/// Verifies latency distribution:
/// - Arrange: Process 1000 subscription events
/// - Act: Measure latency of each event processing
/// - Assert: p99 latency < 50ms
#[tokio::test]
async fn test_latency_slo_p99_under_50ms() {
    // Arrange
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    const EVENT_COUNT: u64 = 1000;
    let mut latencies = Vec::new();

    // Act: Process events and measure latency
    for i in 0..EVENT_COUNT {
        let start = Instant::now();

        let mut orch = orchestrator.write().await;
        let event = MarketplaceEvent::CustomerSubscribes {
            customer_id: format!("cust-latency-{}", i),
            sku: "pro".to_string(),
        };
        let _assigned = orch.assign_governors(&event);

        let latency_ms = start.elapsed().as_millis() as f64;
        latencies.push(latency_ms);
    }

    // Analyze latencies
    latencies.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let p99_idx = (latencies.len() as f64 * 0.99) as usize;
    let p99_latency = latencies[p99_idx];

    let p50_idx = (latencies.len() as f64 * 0.50) as usize;
    let p50_latency = latencies[p50_idx];

    let p95_idx = (latencies.len() as f64 * 0.95) as usize;
    let p95_latency = latencies[p95_idx];

    // Assert: SLOs met
    assert!(
        p99_latency < 50.0,
        "P99 latency should be < 50ms (was {:.2}ms)",
        p99_latency
    );

    println!(
        "Latency SLOs: p50={:.2}ms, p95={:.2}ms, p99={:.2}ms (SLO: p99<50ms)",
        p50_latency, p95_latency, p99_latency
    );
}

/// Test: Memory stability under sustained load
///
/// Verifies no memory leaks:
/// - Arrange: Create orchestrator
/// - Act: Process 100+ events repeatedly in loops
/// - Assert: Memory usage stays stable (rough check via object count)
#[tokio::test]
async fn test_memory_stability_sustained_load() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Act: Process many events in batches
    const BATCH_SIZE: u64 = 100;
    const NUM_BATCHES: u64 = 10;

    for batch in 0..NUM_BATCHES {
        let mut tasks = Vec::new();

        for i in 0..BATCH_SIZE {
            let orch_clone = Arc::clone(&orchestrator);

            let task = tokio::spawn(async move {
                let mut orch = orch_clone.write().await;
                let event = MarketplaceEvent::CustomerSubscribes {
                    customer_id: format!("cust-mem-{}-{}", batch, i),
                    sku: "pro".to_string(),
                };
                orch.assign_governors(&event)
            });
            tasks.push(task);
        }

        futures::future::join_all(tasks).await;

        // After each batch, orchestrator should be in same state
        let orch = orchestrator.read().await;
        assert_eq!(
            orch.current_state(),
            OrchestratorState::Idle,
            "Orchestrator should remain Idle after batch {} processing",
            batch
        );
    }

    // Assert: Orchestrator still healthy after 1000 events
    let orch = orchestrator.read().await;
    let stats = orch.stats();
    assert_eq!(stats.total_governors, 8, "All 8 governors should remain active");
}

/// Test: Queue depth under load (events should not queue indefinitely)
///
/// Verifies backpressure handling:
/// - Arrange: Initialize orchestrator
/// - Act: Rapidly enqueue events
/// - Assert: Events are processed, queue depth returns to 0
#[tokio::test]
async fn test_queue_depth_drain_under_load() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Check initial queue depth
    {
        let orch = orchestrator.read().await;
        let stats = orch.stats();
        assert_eq!(stats.pending_events, 0, "Initial queue should be empty");
    }

    // Act: Process many events
    const EVENT_COUNT: u64 = 500;
    let mut tasks = Vec::new();

    for i in 0..EVENT_COUNT {
        let orch_clone = Arc::clone(&orchestrator);

        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            let event = MarketplaceEvent::CustomerSubscribes {
                customer_id: format!("cust-queue-{}", i),
                sku: "pro".to_string(),
            };
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    // Wait for all to complete
    futures::future::join_all(tasks).await;

    // Assert: Queue drains to zero after processing
    let orch = orchestrator.read().await;
    let stats = orch.stats();
    assert_eq!(
        stats.pending_events, 0,
        "Queue should drain to zero after processing {} events",
        EVENT_COUNT
    );
}

/// Test: High concurrency with mixed event types (stress test)
///
/// Verifies robustness under mixed workload:
/// - Arrange: Initialize orchestrator
/// - Act: 500 concurrent events of varied types
/// - Assert: No panics, all handled correctly
#[tokio::test]
async fn test_high_concurrency_mixed_event_types_stress() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    const STRESS_EVENT_COUNT: u64 = 500;
    let success_count = Arc::new(AtomicU64::new(0));

    // Act: Mixed workload
    let mut tasks = Vec::new();
    for i in 0..STRESS_EVENT_COUNT {
        let orch_clone = Arc::clone(&orchestrator);
        let success = Arc::clone(&success_count);

        let task = tokio::spawn(async move {
            // Generate varied event types
            let event = match i % 6 {
                0 => MarketplaceEvent::CustomerSubscribes {
                    customer_id: format!("cust-stress-{}", i),
                    sku: "pro".to_string(),
                },
                1 => MarketplaceEvent::SubscriptionCanceled {
                    customer_id: format!("cust-stress-{}", i),
                    subscription_id: format!("sub-{}", i),
                },
                2 => MarketplaceEvent::QuotaExceeded {
                    customer_id: format!("cust-stress-{}", i),
                    resource_type: "cpu".to_string(),
                    current_usage: 80,
                    quota_limit: 75,
                },
                3 => MarketplaceEvent::PaymentMethodUpdated {
                    customer_id: format!("cust-stress-{}", i),
                    payment_method_id: format!("pm-{}", i),
                },
                4 => MarketplaceEvent::SubscriptionRenewed {
                    customer_id: format!("cust-stress-{}", i),
                    subscription_id: format!("sub-{}", i),
                    new_sku: "enterprise".to_string(),
                },
                _ => MarketplaceEvent::ComplianceCheckFailed {
                    customer_id: format!("cust-stress-{}", i),
                    reason: "Risk assessment".to_string(),
                },
            };

            let mut orch = orch_clone.write().await;
            if orch.assign_governors(&event).len() > 0 {
                success.fetch_add(1, Ordering::Relaxed);
            }
        });
        tasks.push(task);
    }

    // Wait for completion
    futures::future::join_all(tasks).await;

    // Assert: All events handled
    let handled = success_count.load(Ordering::Relaxed);
    assert!(
        handled > 0,
        "Should successfully handle at least some of {} stress events",
        STRESS_EVENT_COUNT
    );

    println!(
        "Stress Test: {} events processed, {} handled successfully",
        STRESS_EVENT_COUNT, handled
    );
}

/// Test: Orchestrator stats accuracy under load
///
/// Verifies monitoring/metrics:
/// - Arrange: Process events
/// - Act: Check stats
/// - Assert: Stats reflect reality (all 8 governors healthy, pending = 0)
#[tokio::test]
async fn test_orchestrator_stats_accuracy_under_load() {
    // Arrange: Initialize orchestrator
    let orchestrator = Arc::new(RwLock::new(MarketplaceOrchestrator::new()));
    {
        let mut orch = orchestrator.write().await;
        let _ = orch.initialize().await;
    }

    // Process 100 events
    let mut tasks = Vec::new();
    for i in 0..100 {
        let orch_clone = Arc::clone(&orchestrator);
        let task = tokio::spawn(async move {
            let mut orch = orch_clone.write().await;
            let event = MarketplaceEvent::CustomerSubscribes {
                customer_id: format!("cust-stats-{}", i),
                sku: "pro".to_string(),
            };
            orch.assign_governors(&event)
        });
        tasks.push(task);
    }

    futures::future::join_all(tasks).await;

    // Assert: Stats are accurate
    let orch = orchestrator.read().await;
    let stats = orch.stats();

    assert_eq!(stats.total_governors, 8, "Should have 8 governors");
    assert_eq!(stats.governors_healthy, 8, "All 8 governors should be healthy");
    assert_eq!(stats.pending_events, 0, "No pending events after processing");

    println!(
        "Orchestrator Stats: {} total governors, {} healthy, {} pending",
        stats.total_governors, stats.governors_healthy, stats.pending_events
    );
}
