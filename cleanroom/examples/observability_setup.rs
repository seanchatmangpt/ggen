//! Examples demonstrating observability setup
//!
//! This example shows how to set up comprehensive observability for the
//! cleanroom environment using tracing, metrics, and span collection.

use cleanroom::builder::CleanroomBuilder;
use cleanroom::observability::metrics::{
    AnomalyDetection, MetricsAggregator, MetricsAnalyzer, TrendAnalysis, metrics_aggregator,
    metrics_analyzer,
};
use cleanroom::observability::{
    ContainerMetrics, Metrics, ObservabilityLayer, PerformanceMetrics, ResourceUsageMetrics,
    SpanStatus, TestMetrics, TracingLevel, console_metrics_exporter, console_span_collector,
    observability_layer,
};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use uuid::Uuid;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Cleanroom Observability Setup Examples");
    println!("=======================================");

    // Example 1: Basic observability layer setup
    println!("\n1. Basic Observability Layer Setup");
    let layer = ObservabilityLayer::new()
        .with_tracing_level(TracingLevel::Info)
        .with_sampling_rate(1.0);

    println!("✓ Observability layer created");
    println!("✓ Tracing level: {:?}", layer.tracing_config().level);
    println!(
        "✓ Sampling rate: {:.1}",
        layer.tracing_config().sampling_rate
    );

    // Example 2: Advanced observability configuration
    println!("\n2. Advanced Observability Configuration");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;

    let layer = ObservabilityLayer::new()
        .with_tracing_level(TracingLevel::Debug)
        .with_tokio_console()
        .with_distributed_tracing()
        .with_sampling_rate(0.8)
        .with_exporter(console_metrics_exporter())
        .with_span_collector(console_span_collector());

    let mut manager = layer.attach(&environment)?;
    println!("✓ Advanced observability layer attached");
    println!(
        "✓ Tokio console integration: {}",
        manager.tracing_config.tokio_console
    );
    println!(
        "✓ Distributed tracing: {}",
        manager.tracing_config.distributed_tracing
    );

    // Example 3: Metrics collection
    println!("\n3. Metrics Collection");
    manager.start_metrics_collection().await?;
    println!("✓ Metrics collection started");

    // Simulate some activity
    tokio::time::sleep(Duration::from_millis(100)).await;

    let history = manager.get_metrics_history().await;
    println!("✓ Collected {} metrics", history.len());

    if !history.is_empty() {
        let latest = &history[history.len() - 1];
        println!("✓ Latest metrics:");
        println!("  - Session ID: {}", latest.session_id);
        println!(
            "  - CPU usage: {:.1}%",
            latest.resource_usage.cpu_usage_percent
        );
        println!(
            "  - Memory usage: {} bytes",
            latest.resource_usage.memory_usage_bytes
        );
        println!(
            "  - Throughput: {:.2} tests/sec",
            latest.performance.throughput
        );
        println!("  - Tests executed: {}", latest.tests.total_executed);
        println!(
            "  - Success rate: {:.1}%",
            latest.tests.success_rate * 100.0
        );
    }

    manager.stop_metrics_collection().await;
    println!("✓ Metrics collection stopped");

    // Example 4: Span management
    println!("\n4. Span Management");
    let span_id = manager
        .start_span("test_operation".to_string(), None)
        .await?;
    println!("✓ Started span: {}", span_id);

    // Add span attributes
    manager
        .add_span_attribute(&span_id, "operation".to_string(), "test".to_string())
        .await?;
    manager
        .add_span_attribute(&span_id, "environment".to_string(), "cleanroom".to_string())
        .await?;
    println!("✓ Added span attributes");

    // Add span events
    let mut event_attrs = HashMap::new();
    event_attrs.insert("step".to_string(), "initialization".to_string());
    manager
        .add_span_event(&span_id, "step_started".to_string(), event_attrs)
        .await?;
    println!("✓ Added span event");

    // Simulate some work
    tokio::time::sleep(Duration::from_millis(50)).await;

    // End the span
    manager.end_span(&span_id, SpanStatus::Completed).await?;
    println!("✓ Ended span: {}", span_id);

    let active_spans = manager.get_active_spans().await;
    println!("✓ Active spans: {}", active_spans.len());

    // Example 5: Metrics aggregator
    println!("\n5. Metrics Aggregator");
    let mut aggregator = MetricsAggregator::new(Duration::from_secs(60), 100);

    // Add some sample metrics
    for i in 0..5 {
        let metrics = Metrics {
            timestamp: Instant::now() - Duration::from_secs(i * 10),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 20.0 + (i as f64 * 5.0),
                memory_usage_bytes: 1024 * 1024 * (i + 1) as u64,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100 + i * 10),
                container_startup_time: Duration::from_millis(50 + i * 5),
                total_execution_time: Duration::from_secs(10 + i),
                throughput: 10.0 + i as f64,
            },
            containers: ContainerMetrics {
                total_created: 5 + i,
                total_destroyed: 3 + i,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60 + i * 10),
            },
            tests: TestMetrics {
                total_executed: 100 + i * 20,
                passed: 95 + i * 18,
                failed: 5 + i * 2,
                success_rate: 0.95 - (i as f64 * 0.01),
                avg_duration: Duration::from_millis(100 + i * 5),
            },
        };

        aggregator.add_metrics(metrics);
    }

    println!(
        "✓ Added {} metrics to aggregator",
        aggregator.metrics_count()
    );

    // Get aggregated metrics for the last 60 seconds
    if let Some(aggregated) = aggregator.get_window_aggregated_metrics() {
        println!("✓ Aggregated metrics:");
        println!("  - Count: {}", aggregated.count);
        println!(
            "  - Avg CPU: {:.1}%",
            aggregated.resource_usage.avg_cpu_usage_percent
        );
        println!(
            "  - Peak CPU: {:.1}%",
            aggregated.resource_usage.peak_cpu_usage_percent
        );
        println!(
            "  - Avg Memory: {} bytes",
            aggregated.resource_usage.avg_memory_usage_bytes
        );
        println!(
            "  - Peak Memory: {} bytes",
            aggregated.resource_usage.peak_memory_usage_bytes
        );
        println!(
            "  - Avg Throughput: {:.2} tests/sec",
            aggregated.performance.avg_throughput
        );
        println!(
            "  - Peak Throughput: {:.2} tests/sec",
            aggregated.performance.peak_throughput
        );
        println!(
            "  - Avg Success Rate: {:.1}%",
            aggregated.tests.avg_success_rate * 100.0
        );
        println!(
            "  - Min Success Rate: {:.1}%",
            aggregated.tests.min_success_rate * 100.0
        );
        println!(
            "  - Max Success Rate: {:.1}%",
            aggregated.tests.max_success_rate * 100.0
        );
    }

    let stats = aggregator.get_statistics();
    println!("✓ Aggregator statistics:");
    println!("  - Total metrics: {}", stats.total_metrics);
    println!("  - Window size: {:?}", stats.window_size);
    println!("  - Max metrics: {}", stats.max_metrics);

    // Example 6: Metrics analyzer
    println!("\n6. Metrics Analyzer");
    let mut analyzer = MetricsAnalyzer::new(
        Duration::from_secs(300), // 5 minutes
        1000,
        Duration::from_secs(60), // 1 minute
    );

    // Add some metrics with anomalies
    let anomalous_metrics = Metrics {
        timestamp: Instant::now(),
        session_id: Uuid::new_v4(),
        resource_usage: ResourceUsageMetrics {
            cpu_usage_percent: 90.0,                    // High CPU usage
            memory_usage_bytes: 2 * 1024 * 1024 * 1024, // High memory usage
            disk_usage_bytes: 0,
            network_bytes_sent: 0,
            network_bytes_received: 0,
        },
        performance: PerformanceMetrics {
            avg_test_execution_time: Duration::from_millis(100),
            container_startup_time: Duration::from_millis(50),
            total_execution_time: Duration::from_secs(10),
            throughput: 0.5, // Low throughput
        },
        containers: ContainerMetrics {
            total_created: 5,
            total_destroyed: 3,
            currently_running: 2,
            avg_lifetime: Duration::from_secs(60),
        },
        tests: TestMetrics {
            total_executed: 100,
            passed: 80, // Low success rate
            failed: 20,
            success_rate: 0.80,
            avg_duration: Duration::from_millis(100),
        },
    };

    analyzer.add_metrics(anomalous_metrics);
    println!("✓ Added anomalous metrics to analyzer");

    // Detect anomalies
    let anomalies = analyzer.detect_anomalies();
    println!("✓ Detected {} anomalies", anomalies.anomalies.len());

    for anomaly in &anomalies.anomalies {
        println!(
            "  - {:?} ({:?}): {}",
            anomaly.anomaly_type, anomaly.severity, anomaly.description
        );
        println!(
            "    Value: {:.2}, Expected: {:.2}-{:.2}",
            anomaly.value, anomaly.expected_range.0, anomaly.expected_range.1
        );
    }

    // Analyze trends
    if let Some(trends) = analyzer.analyze_trends() {
        println!("✓ Trend analysis:");
        println!("  - CPU trend: {:?}", trends.cpu_trend);
        println!("  - Memory trend: {:?}", trends.memory_trend);
        println!("  - Throughput trend: {:?}", trends.throughput_trend);
        println!("  - Success rate trend: {:?}", trends.success_rate_trend);
    }

    // Example 7: Observability statistics
    println!("\n7. Observability Statistics");
    let stats = manager.get_statistics().await;
    println!("✓ Observability statistics:");
    println!(
        "  - Total metrics collected: {}",
        stats.total_metrics_collected
    );
    println!("  - Active spans: {}", stats.active_spans_count);
    println!("  - Exporters: {}", stats.exporters_count);
    println!("  - Span collectors: {}", stats.span_collectors_count);
    println!(
        "  - Metrics collection enabled: {}",
        stats.metrics_collection_enabled
    );
    println!("  - Tracing enabled: {}", stats.tracing_enabled);

    // Example 8: Convenience functions
    println!("\n8. Convenience Functions");
    let _layer = observability_layer();
    let _exporter = console_metrics_exporter();
    let _collector = console_span_collector();
    let _aggregator = metrics_aggregator(Duration::from_secs(60), 100);
    let _analyzer = metrics_analyzer(Duration::from_secs(60), 100, Duration::from_secs(30));

    println!("✓ All convenience functions work correctly");

    // Example 9: Performance monitoring
    println!("\n9. Performance Monitoring");
    let start_time = Instant::now();

    // Simulate some work
    tokio::time::sleep(Duration::from_millis(100)).await;

    let duration = start_time.elapsed();
    println!("✓ Work completed in {:?}", duration);

    // Create performance metrics
    let perf_metrics = PerformanceMetrics {
        avg_test_execution_time: duration,
        container_startup_time: Duration::from_millis(50),
        total_execution_time: duration,
        throughput: 1.0 / duration.as_secs_f64(),
    };

    println!("✓ Performance metrics:");
    println!(
        "  - Avg test execution time: {:?}",
        perf_metrics.avg_test_execution_time
    );
    println!(
        "  - Container startup time: {:?}",
        perf_metrics.container_startup_time
    );
    println!(
        "  - Total execution time: {:?}",
        perf_metrics.total_execution_time
    );
    println!("  - Throughput: {:.2} ops/sec", perf_metrics.throughput);

    // Example 10: Resource monitoring
    println!("\n10. Resource Monitoring");
    let resource_metrics = ResourceUsageMetrics {
        cpu_usage_percent: 25.0,
        memory_usage_bytes: 512 * 1024 * 1024,   // 512MB
        disk_usage_bytes: 1024 * 1024 * 1024,    // 1GB
        network_bytes_sent: 1024 * 1024,         // 1MB
        network_bytes_received: 2 * 1024 * 1024, // 2MB
    };

    println!("✓ Resource usage metrics:");
    println!("  - CPU usage: {:.1}%", resource_metrics.cpu_usage_percent);
    println!(
        "  - Memory usage: {} bytes ({:.1} MB)",
        resource_metrics.memory_usage_bytes,
        resource_metrics.memory_usage_bytes as f64 / (1024.0 * 1024.0)
    );
    println!(
        "  - Disk usage: {} bytes ({:.1} GB)",
        resource_metrics.disk_usage_bytes,
        resource_metrics.disk_usage_bytes as f64 / (1024.0 * 1024.0 * 1024.0)
    );
    println!(
        "  - Network sent: {} bytes ({:.1} MB)",
        resource_metrics.network_bytes_sent,
        resource_metrics.network_bytes_sent as f64 / (1024.0 * 1024.0)
    );
    println!(
        "  - Network received: {} bytes ({:.1} MB)",
        resource_metrics.network_bytes_received,
        resource_metrics.network_bytes_received as f64 / (1024.0 * 1024.0)
    );

    println!("\n=== All Observability Setup Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_observability_layer() {
        let layer = ObservabilityLayer::new()
            .with_tracing_level(TracingLevel::Debug)
            .with_sampling_rate(0.5);

        assert!(layer.metrics_config().enabled);
        assert!(layer.tracing_config().enabled);
        assert_eq!(layer.tracing_config().level, TracingLevel::Debug);
        assert_eq!(layer.tracing_config().sampling_rate, 0.5);
    }

    #[tokio::test]
    async fn test_observability_manager() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");

        let layer = ObservabilityLayer::new();
        let mut manager = layer.attach(&environment).unwrap();

        assert!(manager.metrics_config.enabled);
        assert!(manager.tracing_config.enabled);
    }

    #[tokio::test]
    async fn test_span_management() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");

        let layer = ObservabilityLayer::new();
        let manager = layer.attach(&environment).unwrap();

        let span_id = manager
            .start_span("test_span".to_string(), None)
            .await
            .unwrap();
        assert!(!span_id.is_empty());

        manager
            .add_span_attribute(&span_id, "key".to_string(), "value".to_string())
            .await
            .unwrap();
        manager
            .add_span_event(&span_id, "test_event".to_string(), HashMap::new())
            .await
            .unwrap();

        manager
            .end_span(&span_id, SpanStatus::Completed)
            .await
            .unwrap();

        let active_spans = manager.get_active_spans().await;
        assert_eq!(active_spans.len(), 0);
    }

    #[tokio::test]
    async fn test_metrics_collection() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");

        let layer = ObservabilityLayer::new().with_exporter(console_metrics_exporter());
        let mut manager = layer.attach(&environment).unwrap();

        manager.start_metrics_collection().await.unwrap();

        tokio::time::sleep(Duration::from_millis(100)).await;

        let history = manager.get_metrics_history().await;
        assert!(!history.is_empty());

        manager.stop_metrics_collection().await;
    }

    #[test]
    fn test_metrics_aggregator() {
        let mut aggregator = MetricsAggregator::new(Duration::from_secs(60), 100);

        let metrics = Metrics {
            timestamp: Instant::now(),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 25.0,
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100),
                container_startup_time: Duration::from_millis(50),
                total_execution_time: Duration::from_secs(10),
                throughput: 10.0,
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 95,
                failed: 5,
                success_rate: 0.95,
                avg_duration: Duration::from_millis(100),
            },
        };

        aggregator.add_metrics(metrics);
        assert_eq!(aggregator.metrics_count(), 1);
    }

    #[test]
    fn test_metrics_analyzer() {
        let mut analyzer = MetricsAnalyzer::default();

        let metrics = Metrics {
            timestamp: Instant::now(),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 90.0,
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100),
                container_startup_time: Duration::from_millis(50),
                total_execution_time: Duration::from_secs(10),
                throughput: 0.5,
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 80,
                failed: 20,
                success_rate: 0.80,
                avg_duration: Duration::from_millis(100),
            },
        };

        analyzer.add_metrics(metrics);

        let anomalies = analyzer.detect_anomalies();
        assert!(!anomalies.anomalies.is_empty());
    }

    #[test]
    fn test_convenience_functions() {
        let _layer = observability_layer();
        let _exporter = console_metrics_exporter();
        let _collector = console_span_collector();
        let _aggregator = metrics_aggregator(Duration::from_secs(60), 100);
        let _analyzer = metrics_analyzer(Duration::from_secs(60), 100, Duration::from_secs(30));

        assert!(_layer.metrics_config().enabled);
        assert_eq!(_exporter.name(), "console");
        assert_eq!(_collector.name(), "console");
    }
}
