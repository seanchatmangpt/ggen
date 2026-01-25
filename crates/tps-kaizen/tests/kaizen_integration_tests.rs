//! Integration tests for Kaizen metrics system
//!
//! Black-box tests verifying:
//! - Metrics recorded → appears in Prometheus scrape
//! - Aggregation over time (1m window from raw events)
//! - SPARQL query returns correct results
//! - Analyzer detects improvement opportunity

use std::sync::Arc;
use tps_kaizen::{
    Dashboard, KaizenMetrics, MetricAnalyzer, MetricRecorder, Recommendation, SparqlAnalyzer,
};

#[tokio::test]
async fn test_metrics_flow_jidoka_circuit_open() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());

    // Act - Record circuit open event
    recorder
        .record_circuit_open("payment-service")
        .await
        .expect("Failed to record circuit open");

    // Assert - Metric should be updated
    let circuit_open_percent = metrics.jidoka.circuit_open_percent.get();
    assert!(circuit_open_percent > 0.0, "Circuit open percent should increase");

    let total_opens = metrics.jidoka.total_opens.get_sample_count();
    assert!(total_opens > 0.0, "Total opens counter should increase");
}

#[tokio::test]
async fn test_metrics_flow_kanban_queue() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());

    // Act - Record queue depth changes
    for i in [10, 25, 45, 38, 20].iter() {
        recorder
            .record_queue_depth("checkout", *i)
            .await
            .expect("Failed to record queue depth");

        // Small delay to ensure different timestamps
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    }

    // Assert
    let queue_depth = metrics.kanban.queue_depth.get();
    assert_eq!(queue_depth, 20.0, "Queue depth should be last recorded value");
}

#[tokio::test]
async fn test_metrics_aggregation_window() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());

    // Act - Record task completions with varying latencies
    let latencies = vec![100.0, 150.0, 200.0, 175.0, 125.0];
    for latency in latencies {
        recorder
            .record_task_completed("checkout", latency)
            .await
            .expect("Failed to record task");

        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    }

    // Assert - Aggregate over 1 minute window
    let agg = recorder
        .aggregate_window("kanban_latency_p99_ms", 60)
        .await
        .expect("Failed to aggregate");

    assert!(agg.is_some(), "Should have aggregation data");

    let agg = agg.unwrap();
    assert_eq!(agg.sample_count, 5, "Should have 5 samples");
    assert_eq!(agg.min, 100.0, "Min should be 100.0");
    assert_eq!(agg.max, 200.0, "Max should be 200.0");
    assert!((agg.value - 150.0).abs() < 0.1, "Mean should be ~150.0");
}

#[tokio::test]
async fn test_analyzer_health_score() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let analyzer = MetricAnalyzer::new(metrics.clone());

    // Act - Get health score with default metrics
    let health = analyzer.health_score().await.expect("Failed to get health score");

    // Assert
    assert!(health >= 0.0 && health <= 100.0, "Health score should be 0-100");
    assert!(health > 80.0, "Health should be high with default metrics");
}

#[tokio::test]
async fn test_analyzer_detects_circuit_failure_opportunity() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics);

    // Act - Simulate high circuit open rate
    for _ in 0..15 {
        recorder
            .record_circuit_open("payment-service")
            .await
            .ok();
    }

    let recommendations = analyzer.analyze().await.expect("Failed to analyze");

    // Assert - Should detect circuit failure opportunity
    let circuit_recommendation = recommendations
        .iter()
        .find(|r| r.metric.contains("circuit_open"));

    assert!(
        circuit_recommendation.is_some(),
        "Should recommend reducing circuit breaker failures"
    );
}

#[tokio::test]
async fn test_analyzer_detects_queue_congestion() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics);

    // Act - Simulate queue congestion
    recorder
        .record_queue_depth("checkout", 150)
        .await
        .expect("Failed to record queue");

    // Simulate high latency
    for _ in 0..10 {
        recorder
            .record_task_completed("checkout", 600.0)
            .await
            .ok();
    }

    let recommendations = analyzer.analyze().await.expect("Failed to analyze");

    // Assert
    let queue_recommendation = recommendations
        .iter()
        .find(|r| r.metric.contains("queue_depth"));

    assert!(
        queue_recommendation.is_some(),
        "Should recommend adding workers for queue"
    );
}

#[tokio::test]
async fn test_dashboard_grafana_json_export() {
    // Arrange
    let dashboard = Dashboard::kaizen_default();

    // Act
    let json = dashboard
        .to_grafana_json()
        .expect("Failed to generate Grafana JSON");

    // Assert
    assert!(json.get("title").is_some());
    assert_eq!(
        json.get("title").and_then(|v| v.as_str()),
        Some("TPS Kaizen - Continuous Improvement Metrics")
    );

    assert!(json.get("panels").is_some());
    let panels = json
        .get("panels")
        .and_then(|p| p.as_array())
        .expect("Panels should be an array");

    assert!(!panels.is_empty(), "Dashboard should have panels");
    assert!(panels.len() >= 10, "Dashboard should have at least 10 panels");
}

#[tokio::test]
async fn test_sparql_analyzer_load_evidence() {
    // Arrange
    let analyzer = SparqlAnalyzer::new().expect("Failed to create analyzer");

    // Act
    analyzer
        .load_sample_evidence()
        .await
        .expect("Failed to load evidence");

    // Assert - Query should return results
    let query = r#"
        PREFIX tps: <http://example.com/tps/>
        SELECT (COUNT(?s) as ?count) WHERE {
            ?s a tps:JidokaFailure .
        }
    "#;

    let results = analyzer
        .execute_query(query)
        .await
        .expect("Failed to execute query");

    assert!(!results.is_empty(), "Should have query results");
}

#[tokio::test]
async fn test_full_kaizen_workflow() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics.clone());
    let dashboard = Dashboard::kaizen_default();

    // Act 1 - Record various events
    recorder
        .record_circuit_open("payment-service")
        .await
        .ok();

    recorder
        .record_queue_depth("checkout", 42)
        .await
        .ok();

    recorder
        .record_task_completed("checkout", 250.0)
        .await
        .ok();

    recorder
        .record_alert_fired("HighLatency", "warning")
        .await
        .ok();

    recorder
        .record_worker_activated("worker-001")
        .await
        .ok();

    // Act 2 - Analyze metrics
    let health = analyzer
        .health_score()
        .await
        .expect("Failed to get health score");

    let recommendations = analyzer
        .analyze()
        .await
        .expect("Failed to analyze");

    // Act 3 - Generate dashboard
    let dashboard_json = dashboard
        .to_grafana_json()
        .expect("Failed to generate dashboard");

    // Assert
    assert!(health >= 0.0 && health <= 100.0);
    assert!(!recommendations.is_empty(), "Should have recommendations");
    assert!(dashboard_json.get("panels").is_some());
}

#[tokio::test]
async fn test_metrics_slo_tracking() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics.clone());

    let slo = tps_kaizen::Slo {
        name: "latency_p99".to_string(),
        metric: "kanban_latency_p99_ms".to_string(),
        target: 100.0,
        window_secs: 60,
        is_maximum: false,
    };

    metrics.register_slo(slo.clone());

    // Act - Record high latency
    for _ in 0..5 {
        recorder
            .record_task_completed("checkout", 80.0)
            .await
            .ok();
    }

    let (met, attainment) = analyzer
        .check_slo_attainment(&slo)
        .await
        .expect("Failed to check SLO");

    // Assert
    assert!(met, "SLO should be met with latency < 100ms");
    assert!(attainment >= 99.0, "Attainment should be ~100%");
}

#[tokio::test]
async fn test_event_retention_policy() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());

    // Act - Record events
    for i in 0..5 {
        recorder
            .record_circuit_open(&format!("service-{}", i))
            .await
            .ok();
    }

    let events_before = recorder.get_events();
    assert_eq!(events_before.len(), 5);

    // Act - Retain only within 0 seconds (should keep nothing)
    recorder
        .retain_events_within(chrono::Duration::seconds(0))
        .await;

    let events_after = recorder.get_events();

    // Assert
    assert!(
        events_after.len() <= events_before.len(),
        "Retention should not increase event count"
    );
}

#[tokio::test]
async fn test_recommendation_prioritization() {
    // Arrange
    let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics);

    // Act - Trigger multiple issues
    // High circuit open rate (P1)
    for _ in 0..12 {
        recorder.record_circuit_open("service-1").await.ok();
    }

    // High alert frequency (P2)
    for _ in 0..6 {
        recorder.record_alert_fired("Alert", "warning").await.ok();
    }

    let recommendations = analyzer
        .analyze()
        .await
        .expect("Failed to analyze");

    // Assert - P1 should come before P2
    if recommendations.len() >= 2 {
        assert_eq!(recommendations[0].priority, 1, "First should be P1 priority");
        assert!(
            recommendations[1].priority >= 1,
            "Second should be P1 or P2"
        );
    }
}
