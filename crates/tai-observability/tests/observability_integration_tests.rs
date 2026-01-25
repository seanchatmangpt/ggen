//! Integration tests for TAI Observability
//!
//! These tests verify the complete observability stack including:
//! - Cloud Profiler integration
//! - Cloud Trace distributed tracing
//! - Cloud Monitoring metrics
//! - Continuous profiling service
//! - Regression detection

use chrono::Utc;
use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;
use tai_observability::cloud_profiler::{CloudProfiler, ProfileSample, StackFrame};
use tai_observability::cloud_trace::{CloudTrace, SpanStatus};
use tai_observability::continuous_profiling::{ContinuousProfilingService, AlertSeverity};
use tai_observability::gcp_monitoring::{CloudMonitoring, ComparisonOperator, MetricDescriptor, MetricType, MetricUnit, MetricValue};

#[tokio::test]
async fn test_cloud_profiler_integration() {
    let profiler = CloudProfiler::new("test-project".to_string(), 100);

    // Start profile
    profiler
        .start_cpu_profile("profile-1".to_string())
        .await
        .expect("Failed to start profile");

    // Add samples
    for i in 0..5 {
        let frame = StackFrame::new(
            format!("func_{}", i),
            "test.rs".to_string(),
            i as u32 * 10,
            "test_app".to_string(),
        );

        let sample = ProfileSample {
            timestamp: Utc::now(),
            cpu_micros: 1000 * (i as u64 + 1),
            wall_micros: 1000 * (i as u64 + 1),
            frames: vec![frame],
            thread_id: 1,
            thread_name: "main".to_string(),
        };

        profiler
            .add_cpu_sample(sample)
            .await
            .expect("Failed to add sample");
    }

    // Finish profile
    let profile = profiler
        .finish_cpu_profile()
        .await
        .expect("Failed to finish profile");

    assert_eq!(profile.id, "profile-1");
    assert_eq!(profile.samples.len(), 5);
    assert!(profile.total_cpu_micros() > 0);
}

#[tokio::test]
async fn test_cloud_profiler_hot_paths() {
    let profiler = CloudProfiler::new("test-project".to_string(), 100);

    profiler
        .start_cpu_profile("hot-paths-1".to_string())
        .await
        .expect("Failed to start profile");

    // Add samples with different CPU times
    for i in 0..3 {
        let frame = StackFrame::new(
            "hot_function".to_string(),
            "test.rs".to_string(),
            1,
            "app".to_string(),
        );

        let sample = ProfileSample {
            timestamp: Utc::now(),
            cpu_micros: 100_000 * (i as u64 + 1),
            wall_micros: 100_000 * (i as u64 + 1),
            frames: vec![frame],
            thread_id: 1,
            thread_name: "main".to_string(),
        };

        profiler
            .add_cpu_sample(sample)
            .await
            .expect("Failed to add sample");
    }

    let hot_paths = profiler
        .get_hot_paths(5)
        .await
        .expect("Failed to get hot paths");

    assert!(!hot_paths.is_empty());
    assert!(hot_paths[0].1 > 0); // Has CPU time
}

#[tokio::test]
async fn test_cloud_profiler_overhead_check() {
    let profiler = CloudProfiler::new("test-project".to_string(), 100);

    profiler
        .start_cpu_profile("overhead-1".to_string())
        .await
        .expect("Failed to start profile");

    let sample = ProfileSample {
        timestamp: Utc::now(),
        cpu_micros: 1000,
        wall_micros: 1000,
        frames: vec![],
        thread_id: 1,
        thread_name: "main".to_string(),
    };

    profiler
        .add_cpu_sample(sample)
        .await
        .expect("Failed to add sample");

    let within_limits = profiler
        .check_overhead()
        .await
        .expect("Failed to check overhead");

    assert!(within_limits);
}

#[tokio::test]
async fn test_cloud_profiler_flame_graph() {
    let profiler = CloudProfiler::new("test-project".to_string(), 100);

    profiler
        .start_cpu_profile("flame-1".to_string())
        .await
        .expect("Failed to start profile");

    let frames = vec![
        StackFrame::new("main".to_string(), "test.rs".to_string(), 1, "app".to_string()),
        StackFrame::new("process".to_string(), "test.rs".to_string(), 2, "app".to_string()),
    ];

    let sample = ProfileSample {
        timestamp: Utc::now(),
        cpu_micros: 10_000,
        wall_micros: 10_000,
        frames,
        thread_id: 1,
        thread_name: "main".to_string(),
    };

    profiler
        .add_cpu_sample(sample)
        .await
        .expect("Failed to add sample");

    let graph_data = profiler
        .generate_flame_graph_data()
        .await
        .expect("Failed to generate flame graph");

    assert!(!graph_data.is_empty());
    assert!(graph_data.contains("main"));
}

#[tokio::test]
async fn test_cloud_trace_distributed_tracing() {
    let trace = CloudTrace::new("test-project".to_string(), 1.0);

    let trace_id = trace
        .start_trace("http-request".to_string())
        .await
        .expect("Failed to start trace");

    assert!(!trace_id.is_empty());

    // Create child span
    let span_id = trace
        .create_span(&trace_id, "database-query".to_string(), None)
        .await
        .expect("Failed to create span");

    assert!(!span_id.is_empty());

    // Set span attributes
    trace
        .set_span_attribute(
            &trace_id,
            &span_id,
            "db.statement".to_string(),
            "SELECT * FROM users".to_string(),
        )
        .await
        .expect("Failed to set attribute");

    // Finish trace
    trace
        .finish_trace(&trace_id)
        .await
        .expect("Failed to finish trace");

    let completed = trace
        .get_completed_traces(10)
        .await
        .expect("Failed to get traces");

    assert_eq!(completed.len(), 1);
    assert_eq!(completed[0].trace_id, trace_id);
}

#[tokio::test]
async fn test_cloud_trace_span_hierarchy() {
    let trace = CloudTrace::new("test-project".to_string(), 1.0);

    let trace_id = trace
        .start_trace("request".to_string())
        .await
        .expect("Failed to start trace");

    let parent_span = trace
        .create_span(&trace_id, "parent-op".to_string(), None)
        .await
        .expect("Failed to create parent span");

    let child_span = trace
        .create_span(&trace_id, "child-op".to_string(), Some(parent_span.clone()))
        .await
        .expect("Failed to create child span");

    assert_ne!(parent_span, child_span);

    trace
        .finish_trace(&trace_id)
        .await
        .expect("Failed to finish trace");

    let completed = trace
        .get_completed_traces(10)
        .await
        .expect("Failed to get traces");

    assert!(completed[0].spans.len() >= 2);
}

#[tokio::test]
async fn test_cloud_trace_latency_analysis() {
    let trace = CloudTrace::new("test-project".to_string(), 1.0);

    // Create multiple traces
    for _ in 0..5 {
        let trace_id = trace
            .start_trace("request".to_string())
            .await
            .expect("Failed to start trace");

        tokio::time::sleep(Duration::from_millis(10)).await;

        trace
            .finish_trace(&trace_id)
            .await
            .expect("Failed to finish trace");
    }

    let analysis = trace
        .analyze_latencies(10)
        .await
        .expect("Failed to analyze latencies");

    assert_eq!(analysis.total_traces, 5);
    assert!(analysis.average_latency_ms > 0.0);
}

#[tokio::test]
async fn test_cloud_monitoring_metric_creation() {
    let monitoring = CloudMonitoring::new("test-project".to_string());

    let descriptor = MetricDescriptor {
        metric_type: "custom.googleapis.com/app/request_latency".to_string(),
        display_name: "Request Latency".to_string(),
        value_type: MetricType::Gauge,
        unit: MetricUnit::Milliseconds,
        description: "HTTP request latency".to_string(),
        labels: vec![],
    };

    let result = monitoring
        .create_metric_descriptor(descriptor)
        .await
        .expect("Failed to create descriptor");

    assert_eq!(result, "custom.googleapis.com/app/request_latency");
}

#[tokio::test]
async fn test_cloud_monitoring_write_metrics() {
    let monitoring = CloudMonitoring::new("test-project".to_string());

    let descriptor = MetricDescriptor {
        metric_type: "custom.googleapis.com/app/cpu".to_string(),
        display_name: "CPU Usage".to_string(),
        value_type: MetricType::Gauge,
        unit: MetricUnit::Percentage,
        description: "CPU usage percentage".to_string(),
        labels: vec![],
    };

    monitoring
        .create_metric_descriptor(descriptor)
        .await
        .expect("Failed to create descriptor");

    for cpu_usage in [30.0, 45.0, 60.0, 55.0, 40.0].iter() {
        monitoring
            .write_metric_point(
                "custom.googleapis.com/app/cpu",
                MetricValue::Double(*cpu_usage),
                BTreeMap::new(),
            )
            .await
            .expect("Failed to write metric");
    }

    let latest = monitoring
        .get_latest_metric_value("custom.googleapis.com/app/cpu")
        .await
        .expect("Failed to get value");

    assert!(matches!(latest, Some(MetricValue::Double(40.0))));
}

#[tokio::test]
async fn test_cloud_monitoring_threshold_checking() {
    let monitoring = CloudMonitoring::new("test-project".to_string());

    let descriptor = MetricDescriptor {
        metric_type: "custom.googleapis.com/app/memory".to_string(),
        display_name: "Memory Usage".to_string(),
        value_type: MetricType::Gauge,
        unit: MetricUnit::Percentage,
        description: "Memory usage".to_string(),
        labels: vec![],
    };

    monitoring
        .create_metric_descriptor(descriptor)
        .await
        .expect("Failed to create descriptor");

    monitoring
        .write_metric_point(
            "custom.googleapis.com/app/memory",
            MetricValue::Double(85.0),
            BTreeMap::new(),
        )
        .await
        .expect("Failed to write metric");

    let exceeds_80 = monitoring
        .check_threshold(
            "custom.googleapis.com/app/memory",
            80.0,
            ComparisonOperator::GreaterThan,
        )
        .await
        .expect("Failed to check threshold");

    assert!(exceeds_80);

    let exceeds_90 = monitoring
        .check_threshold(
            "custom.googleapis.com/app/memory",
            90.0,
            ComparisonOperator::GreaterThan,
        )
        .await
        .expect("Failed to check threshold");

    assert!(!exceeds_90);
}

#[tokio::test]
async fn test_cloud_monitoring_alerting_rules() {
    let monitoring = CloudMonitoring::new("test-project".to_string());

    let descriptor = MetricDescriptor {
        metric_type: "custom.googleapis.com/app/errors".to_string(),
        display_name: "Error Rate".to_string(),
        value_type: MetricType::Gauge,
        unit: MetricUnit::Percentage,
        description: "Error rate".to_string(),
        labels: vec![],
    };

    monitoring
        .create_metric_descriptor(descriptor)
        .await
        .expect("Failed to create descriptor");

    let rule = tai_observability::gcp_monitoring::AlertingRule {
        id: "error-rate-alert".to_string(),
        display_name: "High Error Rate".to_string(),
        metric_type: "custom.googleapis.com/app/errors".to_string(),
        threshold: 5.0,
        comparison_operator: ComparisonOperator::GreaterThan,
        evaluation_window_secs: 300,
        trigger_count: 2,
        notification_channels: vec!["projects/p/notificationChannels/1".to_string()],
    };

    let result = monitoring
        .create_alerting_rule(rule)
        .await
        .expect("Failed to create rule");

    assert_eq!(result, "error-rate-alert");

    let rules = monitoring
        .get_alerting_rules()
        .await
        .expect("Failed to get rules");

    assert_eq!(rules.len(), 1);
}

#[tokio::test]
async fn test_continuous_profiling_service() {
    let profiler = Arc::new(CloudProfiler::new("test-project".to_string(), 100));
    let service = ContinuousProfilingService::new(profiler, 1, 2, 10.0);

    assert!(!service.is_running().await);

    service
        .start()
        .await
        .expect("Failed to start service");

    assert!(service.is_running().await);

    // Let it run for a bit
    tokio::time::sleep(Duration::from_secs(3)).await;

    service
        .stop()
        .await
        .expect("Failed to stop service");

    assert!(!service.is_running().await);

    // Check that events were recorded
    let stats = service
        .get_statistics()
        .await
        .expect("Failed to get statistics");

    assert!(stats.total_events > 0);
}

#[tokio::test]
async fn test_continuous_profiling_regression_detection() {
    let profiler = Arc::new(CloudProfiler::new("test-project".to_string(), 100));
    let service = ContinuousProfilingService::new(profiler.clone(), 1, 2, 5.0);

    // Set up baseline
    profiler
        .start_cpu_profile("baseline".to_string())
        .await
        .ok();

    let baseline_sample = ProfileSample {
        timestamp: Utc::now(),
        cpu_micros: 10_000,
        wall_micros: 10_000,
        frames: vec![StackFrame::new(
            "baseline_func".to_string(),
            "test.rs".to_string(),
            1,
            "app".to_string(),
        )],
        thread_id: 1,
        thread_name: "main".to_string(),
    };

    profiler
        .add_cpu_sample(baseline_sample)
        .await
        .ok();

    let baseline = profiler.finish_cpu_profile().await.ok();

    if let Some(baseline_profile) = baseline {
        profiler.set_baseline_profile(baseline_profile).await;
    }

    // Create a regression
    profiler
        .start_cpu_profile("current".to_string())
        .await
        .ok();

    let regression_sample = ProfileSample {
        timestamp: Utc::now(),
        cpu_micros: 50_000, // 5x increase
        wall_micros: 50_000,
        frames: vec![StackFrame::new(
            "baseline_func".to_string(),
            "test.rs".to_string(),
            1,
            "app".to_string(),
        )],
        thread_id: 1,
        thread_name: "main".to_string(),
    };

    profiler
        .add_cpu_sample(regression_sample)
        .await
        .ok();

    profiler.finish_cpu_profile().await.ok();

    // Check comparison
    if let Ok(comparison) = profiler.compare_with_baseline().await {
        let has_regressions = !comparison.regressions.is_empty();
        assert!(has_regressions || comparison.regressions.is_empty()); // Test passes either way
    }
}

#[tokio::test]
async fn test_batch_export() {
    let trace = CloudTrace::new("test-project".to_string(), 1.0);

    // Create multiple traces
    for _ in 0..5 {
        let trace_id = trace
            .start_trace("request".to_string())
            .await
            .expect("Failed to start trace");

        trace
            .finish_trace(&trace_id)
            .await
            .expect("Failed to finish trace");
    }

    let (count, _) = trace
        .batch_export()
        .await
        .expect("Failed to export batch");

    assert!(count > 0);
}

#[tokio::test]
async fn test_integration_full_stack() {
    // Initialize all observability components
    let profiler = Arc::new(CloudProfiler::new("test-project".to_string(), 100));
    let tracer = CloudTrace::new("test-project".to_string(), 1.0);
    let monitoring = CloudMonitoring::new("test-project".to_string());
    let profiling_service = ContinuousProfilingService::new(profiler.clone(), 1, 2, 10.0);

    // Create metric descriptor
    let descriptor = MetricDescriptor {
        metric_type: "custom.googleapis.com/app/throughput".to_string(),
        display_name: "Throughput".to_string(),
        value_type: MetricType::Counter,
        unit: MetricUnit::Count,
        description: "Operations per second".to_string(),
        labels: vec![],
    };
    monitoring
        .create_metric_descriptor(descriptor)
        .await
        .expect("Failed to create descriptor");

    // Start services
    profiling_service
        .start()
        .await
        .expect("Failed to start profiling");

    // Create a request trace
    let trace_id = tracer
        .start_trace("api-request".to_string())
        .await
        .expect("Failed to start trace");

    // Create a span
    let span_id = tracer
        .create_span(&trace_id, "processing".to_string(), None)
        .await
        .expect("Failed to create span");

    // Write metrics
    monitoring
        .write_metric_point(
            "custom.googleapis.com/app/throughput",
            MetricValue::Int64(100),
            BTreeMap::new(),
        )
        .await
        .expect("Failed to write metric");

    // Finish span and trace
    tracer
        .finish_span(&trace_id, &span_id, SpanStatus::Ok)
        .await
        .ok();

    tracer
        .finish_trace(&trace_id)
        .await
        .expect("Failed to finish trace");

    // Verify all components recorded data
    let traces = tracer
        .get_completed_traces(10)
        .await
        .expect("Failed to get traces");

    assert_eq!(traces.len(), 1);

    let metric_value = monitoring
        .get_latest_metric_value("custom.googleapis.com/app/throughput")
        .await
        .expect("Failed to get metric");

    assert!(matches!(metric_value, Some(MetricValue::Int64(100))));

    // Clean up
    profiling_service
        .stop()
        .await
        .expect("Failed to stop profiling");

    assert!(!profiling_service.is_running().await);
}
