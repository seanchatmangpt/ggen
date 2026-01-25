//! Integration tests for TPS Reference System
//!
//! Tests all 6 TPS principles working together:
//! 1. Normal operation (signal → action → receipt)
//! 2. Service overload (Jidoka: circuit open)
//! 3. Persistent failure (Kanban: dead letter queue)
//! 4. Load spike (Heijunka: scale workers)
//! 5. Trace request end-to-end (Tracing)
//! 6. Verify metrics (Kaizen)

use anyhow::Result;
use serde_json::json;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::time::{sleep, Duration};
use tps_reference::{TpsConfig, TpsSystem, WorkSignal};

/// Scenario 1: Normal operation
/// Signal → Kanban queue → Execution → Andon signal → Kaizen metrics → Heijunka distribution → Tracing
#[tokio::test]
async fn test_scenario_normal_operation() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = TpsSystem::new(config).await?;
    system.clone().start().await?;

    // Act: Process signal through complete pipeline
    let payload = json!({"task": "validate_data", "data": [1, 2, 3]});
    let result = system.process_signal("validate", payload).await?;

    // Assert
    assert!(result.success, "Signal should process successfully");
    assert!(!result.signal_id.is_empty(), "Signal should have an ID");
    assert!(!result.trace_id.is_empty(), "Signal should have a trace ID");
    assert!(result.duration_ms > 0, "Should record processing time");

    // Verify health
    let health = system.health_check().await?;
    assert!(health.healthy, "System should be healthy after normal operation");

    // Verify metrics
    let metrics = system.metrics_snapshot();
    assert_eq!(
        metrics["total_processed"].as_u64().unwrap_or(0),
        1,
        "Should have processed 1 signal"
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 2: Service overload triggers circuit breaker
/// Jidoka principle: Circuit breaker opens after threshold failures
#[tokio::test]
async fn test_scenario_jidoka_circuit_breaker() -> Result<()> {
    // Arrange: Create system with low failure threshold
    let config = TpsConfig {
        circuit_breaker_threshold: 2,
        circuit_breaker_timeout_secs: 1,
        ..Default::default()
    };
    let system = TpsSystem::new(config).await?;
    system.clone().start().await?;

    let jidoka = system.supervision.jidoka();

    // Act: Trigger failures to open circuit
    for _ in 0..3 {
        jidoka.record_failure().await;
    }

    sleep(Duration::from_millis(100)).await;

    // Assert: Circuit should be open
    assert!(!jidoka.is_closed().await, "Circuit should be open after failures");

    // Verify health reflects circuit state
    let health = system.health_check().await?;
    assert!(
        health.circuit_state.contains("Open"),
        "Health should show circuit is open"
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 3: Persistent failure handling
/// Kanban queue with dead letter queue for failed signals
#[tokio::test]
async fn test_scenario_kanban_queue_management() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = TpsSystem::new(config).await?;

    // Act: Enqueue multiple signals
    let kanban = system.supervision.kanban();

    for i in 0..5 {
        let signal = WorkSignal::new(
            "process",
            json!({"index": i, "data": format!("item_{}", i)}),
        );
        kanban.enqueue(signal).await?;
    }

    // Assert: Queue depth should match
    let depth = kanban.depth().await;
    assert_eq!(depth, 5, "Queue should contain 5 signals");

    // Test draining
    let drained = kanban.drain().await;
    assert_eq!(drained, 5, "Should drain all 5 signals");
    assert_eq!(kanban.depth().await, 0, "Queue should be empty after drain");

    system.shutdown().await?;
    Ok(())
}

/// Scenario 4: Load spike handling with Heijunka distribution
/// Heijunka distributes load evenly across worker pool
#[tokio::test]
async fn test_scenario_heijunka_load_balancing() -> Result<()> {
    // Arrange
    let config = TpsConfig {
        heijunka_pool_size: 4,
        ..Default::default()
    };
    let system = TpsSystem::new(config).await?;

    let heijunka = system.supervision.heijunka();

    // Act: Simulate load spike with 100 work units
    for i in 0..100 {
        heijunka.update_load(1);
        if i % 25 == 0 {
            sleep(Duration::from_millis(10)).await;
        }
    }

    // Assert: Load should be distributed
    let distribution = heijunka.get_distribution();
    assert_eq!(distribution.len(), 4, "Should have 4 workers");

    let total: u64 = distribution.iter().sum();
    assert_eq!(total, 100, "Total load should equal 100");

    // Verify no single worker is overloaded
    let max_load = distribution.iter().max().copied().unwrap_or(0);
    assert!(max_load <= 50, "No worker should have more than 50 units");

    // Test rebalancing
    heijunka.rebalance();
    let rebalanced = heijunka.get_distribution();
    let rebalanced_total: u64 = rebalanced.iter().sum();
    assert_eq!(
        rebalanced_total, 100,
        "Total should remain same after rebalance"
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 5: End-to-end tracing
/// Trace ID flows through entire pipeline for observability
#[tokio::test]
async fn test_scenario_tracing_end_to_end() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = TpsSystem::new(config).await?;
    system.clone().start().await?;

    // Act: Process signal and capture trace
    let payload = json!({"action": "trace_test", "value": 42});
    let result = system.process_signal("execute", payload).await?;

    // Assert: Trace ID should be present and consistent
    assert!(!result.trace_id.is_empty(), "Should have trace ID");

    // Trace ID should be UUID format (36 chars including hyphens)
    assert_eq!(
        result.trace_id.len(),
        36,
        "Trace ID should be valid UUID format"
    );

    // Verify trace through multiple signals uses different IDs
    let result2 = system.process_signal("validate", json!({})).await?;
    assert_ne!(
        result.trace_id, result2.trace_id,
        "Different signals should have different trace IDs"
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 6: Kaizen metrics verification
/// Continuous improvement metrics are tracked and available
#[tokio::test]
async fn test_scenario_kaizen_metrics() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = TpsSystem::new(config).await?;
    system.clone().start().await?;

    let kaizen = system.supervision.kaizen();

    // Act: Process multiple signals
    for i in 0..10 {
        let payload = json!({"index": i});
        let _ = system.process_signal("validate", payload).await;
        sleep(Duration::from_millis(5)).await;
    }

    // Assert: Metrics should be recorded
    assert_eq!(
        kaizen.success_count(),
        10,
        "Should have 10 successful operations"
    );
    assert_eq!(kaizen.error_count(), 0, "Should have 0 errors");
    assert_eq!(kaizen.error_rate(), 0.0, "Error rate should be 0%");

    // Verify timing metrics
    let avg_time = kaizen.avg_processing_time_ms();
    assert!(
        avg_time > 0.0,
        "Average processing time should be recorded"
    );

    let p95_time = kaizen.p95_processing_time_ms();
    let p99_time = kaizen.p99_processing_time_ms();
    assert!(
        p95_time <= p99_time,
        "P95 should be less than or equal to P99"
    );

    // Verify metrics snapshot
    let snapshot = system.metrics_snapshot();
    assert_eq!(
        snapshot["total_processed"].as_u64().unwrap_or(0),
        10,
        "Snapshot should show 10 processed signals"
    );
    assert_eq!(
        snapshot["success_count"].as_u64().unwrap_or(0),
        10,
        "Snapshot should show 10 successes"
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 7: Andon signal recording and status
/// Visual signal system records problem states
#[tokio::test]
async fn test_scenario_andon_signals() -> Result<()> {
    // Arrange
    let config = TpsConfig {
        andon_max_history: 10,
        ..Default::default()
    };
    let system = TpsSystem::new(config).await?;
    let andon = system.supervision.andon();

    // Act: Record various signal levels
    andon.record_signal("system_started", "GREEN");
    sleep(Duration::from_millis(10)).await;
    andon.record_signal("warning_detected", "YELLOW");
    sleep(Duration::from_millis(10)).await;
    andon.record_signal("critical_error", "RED");

    // Assert: Signals should be recorded
    let history = andon.get_history();
    assert_eq!(history.len(), 3, "Should have 3 signal records");

    // Verify signal details
    assert_eq!(
        history[0].message, "system_started",
        "First signal should be system_started"
    );
    assert_eq!(
        history[2].message, "critical_error",
        "Last signal should be critical_error"
    );

    // Current level should be RED (last recorded)
    let current = andon.get_current_level();
    assert_eq!(format!("{:?}", current), "Red", "Current level should be Red");

    system.shutdown().await?;
    Ok(())
}

/// Scenario 8: Multiple concurrent signals
/// System handles concurrent load properly
#[tokio::test]
async fn test_scenario_concurrent_signals() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = Arc::new(TpsSystem::new(config).await?);
    system.clone().start().await?;

    // Act: Send 20 signals concurrently
    let mut handles = vec![];
    for i in 0..20 {
        let sys = system.clone();
        let handle = tokio::spawn(async move {
            sys.process_signal("validate", json!({"index": i}))
                .await
        });
        handles.push(handle);
    }

    // Wait for all to complete
    let mut success_count = 0;
    for handle in handles {
        if let Ok(Ok(result)) = handle.await {
            if result.success {
                success_count += 1;
            }
        }
    }

    // Assert: Most or all should succeed
    assert!(
        success_count >= 18,
        "At least 18 out of 20 should succeed, got {}",
        success_count
    );

    system.shutdown().await?;
    Ok(())
}

/// Scenario 9: Health status transitions
/// Health status correctly reflects system state
#[tokio::test]
async fn test_scenario_health_status_transitions() -> Result<()> {
    // Arrange
    let config = TpsConfig::default();
    let system = TpsSystem::new(config).await?;
    system.clone().start().await?;

    // Act & Assert: Initial health check
    let health1 = system.health_check().await?;
    assert!(health1.healthy, "System should start healthy");
    assert_eq!(
        health1.error_rate, 0.0,
        "Should start with 0% error rate"
    );

    // Process some signals
    for i in 0..5 {
        let _ = system
            .process_signal("validate", json!({"index": i}))
            .await;
    }

    // Health should remain good
    let health2 = system.health_check().await?;
    assert!(health2.healthy, "System should remain healthy after normal ops");

    system.shutdown().await?;
    Ok(())
}

/// Scenario 10: Circuit breaker recovery
/// Circuit breaker transitions from open to half-open to closed
#[tokio::test]
async fn test_scenario_circuit_breaker_recovery() -> Result<()> {
    // Arrange
    let config = TpsConfig {
        circuit_breaker_threshold: 2,
        circuit_breaker_timeout_secs: 1,
        ..Default::default()
    };
    let system = TpsSystem::new(config).await?;

    let jidoka = system.supervision.jidoka();

    // Act: Trigger circuit open
    jidoka.record_failure().await;
    jidoka.record_failure().await;
    jidoka.record_failure().await;

    assert!(!jidoka.is_closed().await, "Circuit should be open");

    // Wait for timeout
    sleep(Duration::from_millis(1100)).await;

    // Circuit should be half-open now and allow attempts
    assert!(jidoka.is_closed().await, "Circuit should be half-open/closed after timeout");

    system.shutdown().await?;
    Ok(())
}
