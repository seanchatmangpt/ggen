//! Andon system demonstration
//!
//! Shows how to use the TPS Andon system for visibility, alerting, and diagnostics

use ggen_tps_andon::{
    AlertChannel, AlertCondition, AlertConfig, AlertRule, AlertSeverity, AndonConfig, AndonSignal,
    AndonSystem, LogConfig, LogLevel, LogSink, MetricConfig, ObserverConfig, TracerConfig,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing for better visibility
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== TPS Andon System Demo ===\n");

    // 1. Configure logging
    let log_config = LogConfig {
        level: LogLevel::Info,
        sinks: vec![LogSink::Stdout],
        json_format: true,
        sampling_enabled: false,
        sample_ratio: 10,
        include_caller: true,
        include_location: true,
        timestamp_format: "iso8601".to_string(),
    };

    // 2. Configure metrics
    let metric_config = MetricConfig::default();

    // 3. Configure tracing
    let tracer_config = TracerConfig {
        service_name: "andon-demo".to_string(),
        service_version: "0.1.0".to_string(),
        enabled: true,
        sampling_ratio: 1.0, // Trace all requests in demo
        ..Default::default()
    };

    // 4. Configure observer
    let observer_config = ObserverConfig {
        enabled: true,
        check_interval_secs: 60,
        memory_threshold_percent: 80,
        cpu_threshold_percent: 90,
        include_system_metrics: true,
        include_network_metrics: true,
        include_disk_metrics: true,
    };

    // 5. Configure alerts
    let alert_config = AlertConfig {
        enabled: true,
        rules: vec![AlertRule {
            name: "high-memory".to_string(),
            description: "Memory usage exceeds 80%".to_string(),
            condition: AlertCondition::MemoryUsage { percent: 80 },
            severity: AlertSeverity::Warning,
            channels: vec![AlertChannel::Stdout],
            dedup_window_minutes: 5,
            escalation: None,
            enabled: true,
        }],
        global_dedup_minutes: 5,
    };

    // 6. Create Andon system with all components
    let config = AndonConfig {
        logger: log_config,
        metrics: metric_config,
        tracer: tracer_config,
        observer: observer_config,
        alert: alert_config,
    };

    println!("Creating Andon system...\n");
    let system = AndonSystem::new(config).await?;

    println!("✓ Andon system initialized\n");

    // 7. Log messages at different levels
    println!("--- Logging Demo ---");
    system.logger().info("System initialized").await?;
    system.logger().debug("Debug information").await?;
    system.logger().warn("Warning: check this").await?;
    system.logger().critical("Critical: stop the line!").await?;

    // 8. Signal problems (pull the Andon cord)
    println!("\n--- Andon Signal Demo ---");

    let signal_green = AndonSignal::green("System operating normally")
        .with_component("queue-handler")
        .with_trace_id("trace-001");

    println!("Signaling GREEN (normal):");
    system.signal_problem(signal_green).await?;

    let signal_yellow = AndonSignal::yellow("Queue depth approaching threshold")
        .with_component("queue-handler")
        .with_trace_id("trace-002");

    println!("\nSignaling YELLOW (warning):");
    system.signal_problem(signal_yellow).await?;

    let signal_red = AndonSignal::red("Queue overflow detected - stop processing")
        .with_component("queue-handler")
        .with_trace_id("trace-003");

    println!("\nSignaling RED (critical):");
    system.signal_problem(signal_red).await?;

    // 9. Metrics demo
    println!("\n--- Metrics Demo ---");
    system.metrics().update_queue_depth("default", 150)?;
    system
        .metrics()
        .update_pool_utilization("worker-pool", 0.95)?;
    system.metrics().update_memory_usage(512)?;
    system.metrics().update_cpu_usage(75)?;

    println!("✓ Metrics recorded");

    // 10. Tracing demo
    println!("\n--- Distributed Tracing Demo ---");

    let span_id = system.tracer().start_span("request-processing")?;
    println!("Started span: {}", span_id);

    system
        .tracer()
        .add_span_attribute(&span_id, "user_id", serde_json::json!("user-123"))?;

    system
        .tracer()
        .add_span_attribute(&span_id, "endpoint", serde_json::json!("/api/queue"))?;

    system
        .tracer()
        .span_event(&span_id, "processing_started", None)?;

    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    system
        .tracer()
        .end_span(&span_id, ggen_tps_andon::andon_tracer::SpanStatus::Ok)?;
    println!("✓ Span completed");

    // 11. Observer demo (diagnostics)
    println!("\n--- Runtime Diagnostics ---");
    let metrics = system.observer().run_diagnostics().await?;

    println!(
        "Memory: {} MB / {} MB ({:.1}%)",
        metrics.used_memory / 1024 / 1024,
        metrics.total_memory / 1024 / 1024,
        metrics.memory_percent
    );
    println!("CPU: {:.1}%", metrics.cpu_percent);
    println!("Processes: {}", metrics.process_count);
    println!("Health: {:?}", metrics.status);

    // 12. Get W3C Trace Context headers (for propagating traces)
    println!("\n--- W3C Trace Context (for distributed systems) ---");
    let headers = system.tracer().w3c_headers();
    for (key, value) in headers {
        println!("{}: {}", key, value);
    }

    // 13. Summary
    println!("\n--- Andon System Summary ---");
    println!("✓ Logging: Structured JSON logs with levels and sampling");
    println!("✓ Metrics: Prometheus metrics (counters, gauges, histograms)");
    println!("✓ Tracing: OpenTelemetry distributed tracing with W3C headers");
    println!("✓ Diagnostics: Runtime health checks (memory, CPU, processes)");
    println!("✓ Alerting: Threshold-based alerts with escalation");
    println!("\nAndon Implementation Complete!");

    // Cleanup
    system.shutdown().await?;

    Ok(())
}
