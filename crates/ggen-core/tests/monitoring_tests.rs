//! Integration tests for monitoring infrastructure
//!
//! These tests verify the monitoring infrastructure using Chicago TDD principles:
//! - State-based testing (verify observable outputs)
//! - Real collaborators (use actual Prometheus/Grafana configs)
//! - AAA pattern (Arrange, Act, Assert)
//! - Behavior verification (test what the code does, not how)

use ggen_core::monitoring::grafana::{GrafanaDashboard, GrafanaProvisioning, PanelType};
use ggen_core::monitoring::prometheus::{
    AlertSeverity, PrometheusExporter, ScrapeTarget, AlertRule, AlertRuleGroup,
};
use std::time::Duration;

// ============================================================================
// Prometheus Configuration Tests
// ============================================================================

#[test]
fn test_prometheus_config_generation_with_single_target() {
    // Arrange
    let mut exporter = PrometheusExporter::new();
    let target = ScrapeTarget {
        job_name: "ggen-api".to_string(),
        targets: vec!["localhost:9090".to_string()],
        scrape_interval: Duration::from_secs(15),
        scrape_timeout: Duration::from_secs(10),
        metrics_path: "/metrics".to_string(),
    };
    exporter
        .add_target(target)
        .expect("Failed to add scrape target");

    // Act
    let result = exporter.generate_config();

    // Assert
    assert!(result.is_ok(), "Config generation should succeed");
    let yaml = result.expect("Failed to generate config");

    // Verify YAML structure
    assert!(yaml.contains("global:"), "Should contain global config");
    assert!(
        yaml.contains("scrape_configs:"),
        "Should contain scrape configs"
    );
    assert!(
        yaml.contains("job_name: ggen-api"),
        "Should contain job name"
    );
    assert!(
        yaml.contains("localhost:9090"),
        "Should contain target endpoint"
    );
}

#[test]
fn test_prometheus_config_generation_with_multiple_targets() {
    // Arrange
    let mut exporter = PrometheusExporter::new();

    let target1 = ScrapeTarget {
        job_name: "ggen-api".to_string(),
        targets: vec!["localhost:9090".to_string()],
        scrape_interval: Duration::from_secs(15),
        scrape_timeout: Duration::from_secs(10),
        metrics_path: "/metrics".to_string(),
    };

    let target2 = ScrapeTarget {
        job_name: "ggen-worker".to_string(),
        targets: vec![
            "worker1:9091".to_string(),
            "worker2:9091".to_string(),
        ],
        scrape_interval: Duration::from_secs(30),
        scrape_timeout: Duration::from_secs(20),
        metrics_path: "/metrics".to_string(),
    };

    exporter.add_target(target1).expect("Failed to add target1");
    exporter.add_target(target2).expect("Failed to add target2");

    // Act
    let result = exporter.generate_config();

    // Assert
    assert!(result.is_ok(), "Config generation should succeed");
    let yaml = result.expect("Failed to generate config");

    assert!(yaml.contains("job_name: ggen-api"), "Should contain API job");
    assert!(
        yaml.contains("job_name: ggen-worker"),
        "Should contain worker job"
    );
    assert!(
        yaml.contains("worker1:9091"),
        "Should contain worker1 endpoint"
    );
    assert!(
        yaml.contains("worker2:9091"),
        "Should contain worker2 endpoint"
    );
}

#[test]
fn test_prometheus_default_slo_alerts() {
    // Arrange
    let mut exporter = PrometheusExporter::new();

    // Act
    exporter.add_default_slo_alerts();
    let result = exporter.generate_alert_rules();

    // Assert
    assert!(result.is_ok(), "Alert rules generation should succeed");
    let yaml = result.expect("Failed to generate alert rules");

    // Verify all default alerts are present
    assert!(
        yaml.contains("HighErrorRate"),
        "Should contain HighErrorRate alert"
    );
    assert!(
        yaml.contains("HighLatency"),
        "Should contain HighLatency alert"
    );
    assert!(
        yaml.contains("LowThroughput"),
        "Should contain LowThroughput alert"
    );

    // Verify alert structure
    assert!(yaml.contains("alert:"), "Should contain alert field");
    assert!(yaml.contains("expr:"), "Should contain expr field");
    assert!(yaml.contains("for:"), "Should contain for field");
    assert!(yaml.contains("severity:"), "Should contain severity field");
}

#[test]
fn test_prometheus_custom_alert_rule() {
    // Arrange
    let mut exporter = PrometheusExporter::new();
    let custom_rule = AlertRule {
        alert: "CustomAlert".to_string(),
        expr: "custom_metric > 100".to_string(),
        for_duration: Duration::from_secs(300),
        severity: AlertSeverity::Warning,
        summary: "Custom metric threshold exceeded".to_string(),
        description: "The custom metric has exceeded the threshold of 100".to_string(),
    };

    let rule_group = AlertRuleGroup {
        name: "custom_alerts".to_string(),
        interval: Duration::from_secs(30),
        rules: vec![custom_rule],
    };

    // Act
    exporter.add_alert_group(rule_group);
    let result = exporter.generate_alert_rules();

    // Assert
    assert!(result.is_ok(), "Alert rules generation should succeed");
    let yaml = result.expect("Failed to generate alert rules");

    assert!(
        yaml.contains("CustomAlert"),
        "Should contain custom alert name"
    );
    assert!(
        yaml.contains("custom_metric > 100"),
        "Should contain custom expression"
    );
    assert!(
        yaml.contains("warning"),
        "Should contain warning severity"
    );
}

// ============================================================================
// Grafana Dashboard Tests
// ============================================================================

#[test]
fn test_grafana_dashboard_creation() {
    // Arrange & Act
    let dashboard = GrafanaDashboard::new("Test Dashboard", "test-dashboard");

    // Assert
    let json_result = dashboard.generate_json();
    assert!(json_result.is_ok(), "Dashboard JSON generation should succeed");

    let json = json_result.expect("Failed to generate JSON");
    assert!(json.contains("Test Dashboard"), "Should contain title");
    assert!(
        json.contains("test-dashboard"),
        "Should contain dashboard UID"
    );
}

#[test]
fn test_grafana_dashboard_with_panels() {
    // Arrange
    let mut dashboard = GrafanaDashboard::new("Metrics Dashboard", "metrics-dash");

    // Act
    dashboard.add_panel(
        "Throughput",
        PanelType::Graph,
        "rate(requests_total[5m])",
        0,
        0,
        12,
        8,
    );

    dashboard.add_panel(
        "Error Rate",
        PanelType::Stat,
        "rate(errors_total[5m])",
        12,
        0,
        12,
        8,
    );

    let json_result = dashboard.generate_json();

    // Assert
    assert!(json_result.is_ok(), "Dashboard JSON generation should succeed");
    let json = json_result.expect("Failed to generate JSON");

    assert!(json.contains("Throughput"), "Should contain Throughput panel");
    assert!(
        json.contains("Error Rate"),
        "Should contain Error Rate panel"
    );
    assert!(
        json.contains("rate(requests_total[5m])"),
        "Should contain throughput query"
    );
    assert!(
        json.contains("rate(errors_total[5m])"),
        "Should contain error rate query"
    );
}

#[test]
fn test_grafana_job_monitoring_dashboard_structure() {
    // Arrange & Act
    let dashboard = GrafanaDashboard::create_job_monitoring_dashboard();

    // Assert
    let json_result = dashboard.generate_json();
    assert!(json_result.is_ok(), "Dashboard JSON generation should succeed");

    let json = json_result.expect("Failed to generate JSON");

    // Verify all expected panels are present
    assert!(
        json.contains("Job Throughput"),
        "Should contain throughput panel"
    );
    assert!(
        json.contains("Error Rate"),
        "Should contain error rate panel"
    );
    assert!(
        json.contains("Job Latency (p50)"),
        "Should contain p50 latency panel"
    );
    assert!(
        json.contains("Job Latency (p95)"),
        "Should contain p95 latency panel"
    );
    assert!(
        json.contains("Job Latency (p99)"),
        "Should contain p99 latency panel"
    );
    assert!(
        json.contains("Queue Depth"),
        "Should contain queue depth panel"
    );

    // Verify PromQL queries
    assert!(
        json.contains("rate(jobs_processed_total[5m])"),
        "Should contain throughput query"
    );
    assert!(
        json.contains("histogram_quantile"),
        "Should contain latency queries"
    );
    assert!(
        json.contains("job_queue_depth"),
        "Should contain queue depth query"
    );
}

#[test]
fn test_grafana_dashboard_panel_ids_are_unique() {
    // Arrange
    let mut dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");

    // Act
    dashboard.add_panel("Panel 1", PanelType::Graph, "query1", 0, 0, 12, 8);
    dashboard.add_panel("Panel 2", PanelType::Graph, "query2", 12, 0, 12, 8);
    dashboard.add_panel("Panel 3", PanelType::Graph, "query3", 0, 8, 12, 8);

    let json_result = dashboard.generate_json();

    // Assert
    assert!(json_result.is_ok(), "Dashboard JSON generation should succeed");
    let json = json_result.expect("Failed to generate JSON");

    // Parse JSON to verify panel IDs
    let parsed: serde_json::Value =
        serde_json::from_str(&json).expect("Failed to parse JSON");
    let panels = parsed["panels"]
        .as_array()
        .expect("Panels should be an array");

    assert_eq!(panels.len(), 3, "Should have 3 panels");

    let panel_ids: Vec<i64> = panels
        .iter()
        .map(|p| p["id"].as_i64().expect("Panel should have id"))
        .collect();

    assert_eq!(panel_ids.len(), 3, "Should have 3 panel IDs");
    assert_eq!(
        panel_ids.iter().collect::<std::collections::HashSet<_>>().len(),
        3,
        "All panel IDs should be unique"
    );
}

#[test]
fn test_grafana_provisioning_config_generation() {
    // Arrange
    let provisioning = GrafanaProvisioning::default_config();

    // Act
    let result = provisioning.generate_yaml();

    // Assert
    assert!(
        result.is_ok(),
        "Provisioning YAML generation should succeed"
    );
    let yaml = result.expect("Failed to generate YAML");

    assert!(yaml.contains("apiVersion"), "Should contain apiVersion");
    assert!(yaml.contains("providers"), "Should contain providers");
    assert!(
        yaml.contains("ggen-dashboards"),
        "Should contain provider name"
    );
    assert!(yaml.contains("type: file"), "Should specify file type");
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_full_monitoring_stack_configuration() {
    // Arrange
    let mut prometheus_exporter = PrometheusExporter::new();
    let mut grafana_dashboard = GrafanaDashboard::new("Full Stack Monitor", "full-stack");

    // Act - Configure Prometheus
    let scrape_target = ScrapeTarget {
        job_name: "ggen-full-stack".to_string(),
        targets: vec!["localhost:9090".to_string()],
        scrape_interval: Duration::from_secs(15),
        scrape_timeout: Duration::from_secs(10),
        metrics_path: "/metrics".to_string(),
    };
    prometheus_exporter
        .add_target(scrape_target)
        .expect("Failed to add target");
    prometheus_exporter.add_default_slo_alerts();

    // Act - Configure Grafana
    grafana_dashboard.add_panel(
        "System Metrics",
        PanelType::Graph,
        "up{job=\"ggen-full-stack\"}",
        0,
        0,
        24,
        8,
    );

    // Assert - Verify Prometheus config
    let prometheus_config = prometheus_exporter
        .generate_config()
        .expect("Failed to generate Prometheus config");
    assert!(
        prometheus_config.contains("ggen-full-stack"),
        "Prometheus config should contain job name"
    );

    // Assert - Verify alert rules
    let alert_rules = prometheus_exporter
        .generate_alert_rules()
        .expect("Failed to generate alert rules");
    assert!(
        alert_rules.contains("HighErrorRate"),
        "Should contain default alerts"
    );

    // Assert - Verify Grafana dashboard
    let dashboard_json = grafana_dashboard
        .generate_json()
        .expect("Failed to generate dashboard JSON");
    assert!(
        dashboard_json.contains("Full Stack Monitor"),
        "Dashboard should contain title"
    );
    assert!(
        dashboard_json.contains("System Metrics"),
        "Dashboard should contain panel"
    );
}

#[test]
fn test_error_handling_invalid_scrape_target() {
    // Arrange
    let mut exporter = PrometheusExporter::new();
    let invalid_target = ScrapeTarget {
        job_name: String::new(), // Invalid: empty job name
        targets: vec!["localhost:9090".to_string()],
        scrape_interval: Duration::from_secs(15),
        scrape_timeout: Duration::from_secs(10),
        metrics_path: "/metrics".to_string(),
    };

    // Act
    let result = exporter.add_target(invalid_target);

    // Assert
    assert!(result.is_err(), "Should fail with empty job name");
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("Job name cannot be empty"),
        "Error message should mention empty job name"
    );
}

#[test]
fn test_error_handling_timeout_exceeds_interval() {
    // Arrange
    let mut exporter = PrometheusExporter::new();
    let invalid_target = ScrapeTarget {
        job_name: "test-job".to_string(),
        targets: vec!["localhost:9090".to_string()],
        scrape_interval: Duration::from_secs(10),
        scrape_timeout: Duration::from_secs(15), // Invalid: timeout > interval
        metrics_path: "/metrics".to_string(),
    };

    // Act
    let result = exporter.add_target(invalid_target);

    // Assert
    assert!(
        result.is_err(),
        "Should fail when timeout exceeds interval"
    );
    assert!(
        result.unwrap_err().to_string().contains("timeout"),
        "Error message should mention timeout"
    );
}

// ============================================================================
// Docker Compose Integration Tests (Testcontainers)
// ============================================================================

#[cfg(feature = "testcontainers")]
mod testcontainer_tests {
    use super::*;
    use testcontainers::{clients::Cli, images::generic::GenericImage};

    #[test]
    fn test_prometheus_container_with_generated_config() {
        // Arrange
        let docker = Cli::default();
        let mut exporter = PrometheusExporter::new();

        let target = ScrapeTarget {
            job_name: "test-service".to_string(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };
        exporter.add_target(target).expect("Failed to add target");

        let config_yaml = exporter
            .generate_config()
            .expect("Failed to generate config");

        // Act - Start Prometheus container with generated config
        let prometheus_image = GenericImage::new("prom/prometheus", "latest")
            .with_exposed_port(9090);

        let _prometheus_container = docker.run(prometheus_image);

        // Assert - Verify config structure
        assert!(
            config_yaml.contains("test-service"),
            "Config should contain test service"
        );
        assert!(
            config_yaml.contains("global:"),
            "Config should have global section"
        );

        // Note: In real integration test, we would:
        // 1. Write config to temp file
        // 2. Mount config into container
        // 3. Query Prometheus API to verify configuration loaded
        // 4. Verify metrics scraping works
    }

    #[test]
    fn test_grafana_container_with_provisioned_dashboard() {
        // Arrange
        let docker = Cli::default();
        let dashboard = GrafanaDashboard::create_job_monitoring_dashboard();
        let dashboard_json = dashboard
            .generate_json()
            .expect("Failed to generate dashboard");

        // Act - Start Grafana container
        let grafana_image = GenericImage::new("grafana/grafana", "latest")
            .with_exposed_port(3000);

        let _grafana_container = docker.run(grafana_image);

        // Assert - Verify dashboard structure
        assert!(
            dashboard_json.contains("ggen Job Monitoring"),
            "Dashboard should have correct title"
        );

        // Note: In real integration test, we would:
        // 1. Write dashboard JSON to temp file
        // 2. Write provisioning config to temp file
        // 3. Mount both into container
        // 4. Query Grafana API to verify dashboard loaded
        // 5. Verify dashboard is accessible
    }
}
