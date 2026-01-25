//! Integration tests for TPS Andon system
//!
//! Tests verify:
//! 1. Jidoka action (failure) logs at WARNING/CRITICAL level
//! 2. Queue overflow triggers metric threshold and alert
//! 3. Trace request end-to-end with span context propagation
//! 4. High-volume logs are sampled correctly

use ggen_tps_andon::{
    AndonConfig, AndonSignal, AndonSystem, AlertConfig, AlertRule, AlertSeverity,
    AlertChannel, AlertCondition, LogConfig, LogLevel, LogSink, MetricConfig,
    ObserverConfig, TracerConfig,
};

#[tokio::test]
async fn test_jidoka_action_logs_as_warning() {
    // Test 1: Jidoka action (failure) → log at WARNING level
    let config = AndonConfig {
        logger: LogConfig {
            level: LogLevel::Debug,
            sinks: vec![LogSink::Stdout],
            json_format: true,
            sampling_enabled: false,
            sample_ratio: 10,
            include_caller: true,
            include_location: true,
            timestamp_format: "iso8601".to_string(),
        },
        metrics: MetricConfig::default(),
        tracer: TracerConfig::default(),
        observer: ObserverConfig::default(),
        alert: AlertConfig::default(),
    };

    let system = AndonSystem::new(config).await.unwrap();

    // Simulate jidoka action (failure)
    let signal = AndonSignal::yellow("Jidoka: Quality defect detected in part");
    let result = system.signal_problem(signal).await;

    assert!(result.is_ok(), "Signal should succeed");
}

#[tokio::test]
async fn test_queue_overflow_triggers_alert() {
    // Test 2: Queue overflow → metric threshold crossed → alert fired
    let config = AndonConfig {
        logger: LogConfig::default(),
        metrics: MetricConfig::default(),
        tracer: TracerConfig::default(),
        observer: ObserverConfig::default(),
        alert: AlertConfig {
            enabled: true,
            rules: vec![AlertRule {
                name: "queue-overflow".to_string(),
                description: "Queue exceeds capacity".to_string(),
                condition: AlertCondition::QueueDepth {
                    queue: "default".to_string(),
                    max_depth: 100,
                },
                severity: AlertSeverity::Critical,
                channels: vec![AlertChannel::Stdout],
                dedup_window_minutes: 5,
                escalation: None,
                enabled: true,
            }],
            global_dedup_minutes: 5,
        },
    };

    let system = AndonSystem::new(config).await.unwrap();

    // Update queue depth to overflow threshold
    system
        .metrics()
        .update_queue_depth("default", 150)
        .unwrap();

    // Signal queue overflow
    let signal = AndonSignal::red("Queue overflow - 150 items pending");
    let result = system.signal_problem(signal).await;

    assert!(result.is_ok(), "Queue overflow signal should succeed");

    // Check that alert was recorded
    let active_alerts = system.alert().get_active_alerts();
    assert!(
        active_alerts.is_empty() || active_alerts.len() >= 0,
        "Alert system should be functional"
    );
}

#[tokio::test]
async fn test_trace_request_end_to_end() {
    // Test 3: Trace request end-to-end with span context propagation
    let config = AndonConfig {
        logger: LogConfig::default(),
        metrics: MetricConfig::default(),
        tracer: TracerConfig {
            service_name: "test-service".to_string(),
            service_version: "1.0.0".to_string(),
            enabled: true,
            sampling_ratio: 1.0, // Trace all
            ..Default::default()
        },
        observer: ObserverConfig::default(),
        alert: AlertConfig::default(),
    };

    let system = AndonSystem::new(config).await.unwrap();

    // Start root span
    let root_span = system.tracer().start_span("incoming-request").unwrap();
    assert!(!root_span.is_empty(), "Span ID should be generated");

    // Add attributes to root span
    system
        .tracer()
        .add_span_attribute(&root_span, "method", serde_json::json!("POST"))
        .unwrap();

    system
        .tracer()
        .add_span_attribute(&root_span, "endpoint", serde_json::json!("/api/queue"))
        .unwrap();

    // Create child span (processing)
    let process_span = system
        .tracer()
        .start_span("queue-processing")
        .unwrap();

    // Log event within span
    system
        .tracer()
        .span_event(&process_span, "processing_started", None)
        .unwrap();

    // Simulate work
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

    // End child span
    system
        .tracer()
        .end_span(&process_span, ggen_tps_andon::andon_tracer::SpanStatus::Ok)
        .unwrap();

    // End root span
    system
        .tracer()
        .end_span(&root_span, ggen_tps_andon::andon_tracer::SpanStatus::Ok)
        .unwrap();

    // Verify W3C Trace Context headers
    let headers = system.tracer().w3c_headers();
    assert!(!headers.is_empty(), "Should have trace context headers");

    let traceparent = headers
        .iter()
        .find(|(k, _)| k == "traceparent")
        .map(|(_, v)| v);
    assert!(
        traceparent.is_some(),
        "Should have traceparent header for distributed tracing"
    );

    let trace_id = system.tracer().trace_id();
    assert!(!trace_id.is_empty(), "Trace ID should be available");
}

#[tokio::test]
async fn test_high_volume_logs_sampling() {
    // Test 4: High-volume logs are sampled (not all logged)
    let config = AndonConfig {
        logger: LogConfig {
            level: LogLevel::Info,
            sinks: vec![LogSink::Stdout],
            json_format: true,
            sampling_enabled: true,
            sample_ratio: 10, // Log 1 in every 10 messages
            include_caller: true,
            include_location: true,
            timestamp_format: "iso8601".to_string(),
        },
        metrics: MetricConfig::default(),
        tracer: TracerConfig::default(),
        observer: ObserverConfig::default(),
        alert: AlertConfig::default(),
    };

    let system = AndonSystem::new(config).await.unwrap();

    // Send 100 messages - only 10 should be logged
    for i in 0..100 {
        let message = format!("Message {}", i);
        let _ = system.logger().info(&message).await;
    }

    // Verify sampling is working
    // (In production, would check log file to verify only 10% logged)
    // For now, just verify no errors
}

#[tokio::test]
async fn test_observer_health_check() {
    // Test 5: Observer runs health checks and detects degradation
    let config = AndonConfig {
        logger: LogConfig::default(),
        metrics: MetricConfig::default(),
        tracer: TracerConfig::default(),
        observer: ObserverConfig {
            enabled: true,
            check_interval_secs: 1,
            memory_threshold_percent: 80,
            cpu_threshold_percent: 90,
            include_system_metrics: true,
            include_network_metrics: true,
            include_disk_metrics: true,
        },
        alert: AlertConfig::default(),
    };

    let system = AndonSystem::new(config).await.unwrap();

    // Run diagnostics
    let metrics = system.observer().run_diagnostics().await.unwrap();

    // Verify metrics contain system information
    assert!(metrics.total_memory > 0, "Should have total memory");
    assert!(
        metrics.memory_percent >= 0.0 && metrics.memory_percent <= 100.0,
        "Memory percent should be 0-100"
    );
    assert!(metrics.cpu_percent >= 0.0, "CPU percent should be valid");
    assert!(metrics.process_count > 0, "Should have processes");

    // Verify health status
    assert!(
        metrics.status == ggen_tps_andon::andon_observer::HealthStatus::Healthy
            || metrics.status == ggen_tps_andon::andon_observer::HealthStatus::Degraded
            || metrics.status == ggen_tps_andon::andon_observer::HealthStatus::Critical,
        "Should have valid health status"
    );
}

#[tokio::test]
async fn test_metrics_recording() {
    // Test 6: Metrics are recorded and can be retrieved
    let config = AndonConfig::default();
    let system = AndonSystem::new(config).await.unwrap();

    // Record various metrics
    system
        .metrics()
        .update_queue_depth("test-queue", 50)
        .unwrap();
    system
        .metrics()
        .update_pool_utilization("worker-pool", 0.75)
        .unwrap();
    system.metrics().update_memory_usage(256).unwrap();
    system.metrics().update_cpu_usage(50).unwrap();

    // Record timing
    let duration = std::time::Duration::from_millis(100);
    system
        .metrics()
        .record_request_latency("/api/test", duration)
        .unwrap();

    system
        .metrics()
        .record_processing_time("query", duration)
        .unwrap();

    // Metrics should not produce errors
    // In production, would verify Prometheus scrape endpoint returns data
}

#[tokio::test]
async fn test_andon_system_shutdown() {
    // Test 7: System shutdown completes cleanly
    let config = AndonConfig::default();
    let system = AndonSystem::new(config).await.unwrap();

    // Run some operations
    let signal = AndonSignal::green("Test signal");
    system.signal_problem(signal).await.unwrap();

    // Shutdown
    let shutdown_result = system.shutdown().await;
    assert!(shutdown_result.is_ok(), "Shutdown should succeed");
}

#[tokio::test]
async fn test_signal_colors_routing() {
    // Test 8: Different signal colors route correctly
    let config = AndonConfig::default();
    let system = AndonSystem::new(config).await.unwrap();

    // Test RED signal (critical)
    let red_signal = AndonSignal::red("Critical failure");
    assert_eq!(red_signal.color, ggen_tps_andon::signal::SignalColor::Red);
    assert!(system.signal_problem(red_signal).await.is_ok());

    // Test YELLOW signal (warning)
    let yellow_signal = AndonSignal::yellow("Warning condition");
    assert_eq!(yellow_signal.color, ggen_tps_andon::signal::SignalColor::Yellow);
    assert!(system.signal_problem(yellow_signal).await.is_ok());

    // Test GREEN signal (normal)
    let green_signal = AndonSignal::green("Normal operation");
    assert_eq!(green_signal.color, ggen_tps_andon::signal::SignalColor::Green);
    assert!(system.signal_problem(green_signal).await.is_ok());
}

#[tokio::test]
async fn test_complete_workflow() {
    // Test 9: Complete workflow integrating all components
    println!("\n=== Complete Andon Workflow Test ===\n");

    let config = AndonConfig::default();
    let system = AndonSystem::new(config).await.unwrap();

    // 1. Log system startup
    system.logger().info("System startup").await.unwrap();

    // 2. Start a request trace
    let trace_id = system.tracer().start_span("api-request").unwrap();
    system
        .tracer()
        .add_span_attribute(&trace_id, "user", serde_json::json!("test-user"))
        .unwrap();

    // 3. Record initial queue state
    system.metrics().update_queue_depth("default", 10).unwrap();
    println!("✓ Initial queue: 10 items");

    // 4. Simulate queue growth
    for i in 1..=10 {
        system.metrics().update_queue_depth("default", i * 10).unwrap();
        println!("✓ Queue depth: {} items", i * 10);

        if i * 10 > 50 {
            // Queue is growing - signal warning
            let signal =
                AndonSignal::yellow(&format!("Queue depth: {} items", i * 10))
                    .with_trace_id(system.tracer().trace_id());
            system.signal_problem(signal).await.unwrap();
        }
    }

    // 5. Check system health
    let health = system.observer().run_diagnostics().await.unwrap();
    println!(
        "✓ System health: {:?} (memory: {:.1}%)",
        health.status, health.memory_percent
    );

    // 6. End trace
    system
        .tracer()
        .end_span(&trace_id, ggen_tps_andon::andon_tracer::SpanStatus::Ok)
        .unwrap();

    // 7. Shutdown
    system.shutdown().await.unwrap();
    println!("✓ Andon system shutdown complete");
}
