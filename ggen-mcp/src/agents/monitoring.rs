//! Monitoring Agent - Observability and Metrics Collection
//!
//! This agent provides comprehensive monitoring and observability for the MCP server:
//! - Distributed tracing across all operations
//! - Metrics collection and aggregation
//! - Health checks and status monitoring
//! - Alerting and notification systems
//! - Performance profiling and analysis
//!
//! # Monitoring Patterns
//!
//! ## Observability Pillars
//! 1. **Metrics** - Quantitative measurements over time
//! 2. **Logs** - Discrete events with context
//! 3. **Traces** - Request flow through the system
//! 4. **Profiles** - Detailed performance analysis
//!
//! ## Metric Types
//! - **Counters** - Monotonically increasing values
//! - **Gauges** - Values that can go up or down
//! - **Histograms** - Distribution of values
//! - **Summaries** - Quantiles over time
//!
//! ## Alerting Strategies
//! - **Threshold-based** - Alert when metrics exceed thresholds
//! - **Anomaly detection** - Alert on unusual patterns
//! - **Composite alerts** - Multiple conditions
//! - **Escalation policies** - Progressive alerting

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Metric value types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MetricValue {
    Counter(u64),
    Gauge(f64),
    Histogram(Vec<f64>),
    Summary { count: u64, sum: f64, quantiles: HashMap<String, f64> },
}

/// Metric definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metric {
    pub name: String,
    pub value: MetricValue,
    pub labels: HashMap<String, String>,
    pub timestamp: chrono::DateTime<Utc>,
    pub description: String,
}

/// Trace span
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceSpan {
    pub trace_id: Uuid,
    pub span_id: Uuid,
    pub parent_span_id: Option<Uuid>,
    pub operation_name: String,
    pub start_time: chrono::DateTime<Utc>,
    pub end_time: Option<chrono::DateTime<Utc>>,
    pub duration_ms: Option<u64>,
    pub status: SpanStatus,
    pub tags: HashMap<String, String>,
    pub logs: Vec<SpanLog>,
}

/// Span status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SpanStatus {
    Ok,
    Error,
    Timeout,
    Cancelled,
}

/// Span log entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanLog {
    pub timestamp: chrono::DateTime<Utc>,
    pub level: String,
    pub message: String,
    pub fields: HashMap<String, serde_json::Value>,
}

/// Health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckResult {
    pub component: String,
    pub status: HealthStatus,
    pub message: String,
    pub details: HashMap<String, serde_json::Value>,
    pub timestamp: chrono::DateTime<Utc>,
    pub response_time_ms: u64,
}

/// Health status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum HealthStatus {
    Healthy,
    Degraded,
    Unhealthy,
    Unknown,
}

/// Alert definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub severity: AlertSeverity,
    pub status: AlertStatus,
    pub condition: String,
    pub threshold: f64,
    pub current_value: f64,
    pub triggered_at: Option<chrono::DateTime<Utc>>,
    pub resolved_at: Option<chrono::DateTime<Utc>>,
    pub labels: HashMap<String, String>,
}

/// Alert severity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AlertSeverity {
    Info,
    Warning,
    Critical,
    Emergency,
}

/// Alert status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AlertStatus {
    Firing,
    Resolved,
    Suppressed,
}

/// Monitoring Agent implementation
pub struct MonitoringAgent {
    id: AgentId,
    metrics: Vec<Metric>,
    traces: Vec<TraceSpan>,
    health_checks: Vec<HealthCheckResult>,
    alerts: Vec<Alert>,
    alert_rules: HashMap<String, AlertRule>,
    performance_profiles: Vec<PerformanceProfile>,
}

/// Alert rule definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertRule {
    pub name: String,
    pub condition: String,
    pub threshold: f64,
    pub severity: AlertSeverity,
    pub duration: Duration,
    pub enabled: bool,
}

/// Performance profile
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceProfile {
    pub id: Uuid,
    pub operation_name: String,
    pub start_time: chrono::DateTime<Utc>,
    pub end_time: chrono::DateTime<Utc>,
    pub duration_ms: u64,
    pub cpu_usage_percent: f64,
    pub memory_usage_mb: f64,
    pub disk_io_bytes: u64,
    pub network_io_bytes: u64,
    pub call_count: u32,
    pub error_count: u32,
    pub details: HashMap<String, serde_json::Value>,
}

impl MonitoringAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            metrics: Vec::new(),
            traces: Vec::new(),
            health_checks: Vec::new(),
            alerts: Vec::new(),
            alert_rules: HashMap::new(),
            performance_profiles: Vec::new(),
        };

        // Initialize alert rules
        agent.initialize_alert_rules();

        agent
    }

    /// Initialize alert rules
    fn initialize_alert_rules(&mut self) {
        self.alert_rules.insert("high_error_rate".to_string(), AlertRule {
            name: "High Error Rate".to_string(),
            condition: "error_rate > 0.05".to_string(),
            threshold: 0.05,
            severity: AlertSeverity::Warning,
            duration: Duration::from_secs(300), // 5 minutes
            enabled: true,
        });

        self.alert_rules.insert("high_latency".to_string(), AlertRule {
            name: "High Latency".to_string(),
            condition: "avg_latency_ms > 5000".to_string(),
            threshold: 5000.0,
            severity: AlertSeverity::Critical,
            duration: Duration::from_secs(60), // 1 minute
            enabled: true,
        });

        self.alert_rules.insert("memory_usage".to_string(), AlertRule {
            name: "High Memory Usage".to_string(),
            condition: "memory_usage_mb > 500".to_string(),
            threshold: 500.0,
            severity: AlertSeverity::Warning,
            duration: Duration::from_secs(300), // 5 minutes
            enabled: true,
        });
    }

    /// Record a metric
    pub fn record_metric(&mut self, name: String, value: MetricValue, labels: HashMap<String, String>, description: String) {
        let metric = Metric {
            name,
            value,
            labels,
            timestamp: Utc::now(),
            description,
        };

        self.metrics.push(metric);

        // Keep only last 100000 metrics
        if self.metrics.len() > 100000 {
            self.metrics.remove(0);
        }

        // Check for alerts
        self.check_alerts();
    }

    /// Start a trace span
    pub fn start_span(&mut self, operation_name: String, parent_span_id: Option<Uuid>) -> Uuid {
        let trace_id = Uuid::new_v4();
        let span_id = Uuid::new_v4();

        let span = TraceSpan {
            trace_id,
            span_id,
            parent_span_id,
            operation_name,
            start_time: Utc::now(),
            end_time: None,
            duration_ms: None,
            status: SpanStatus::Ok,
            tags: HashMap::new(),
            logs: Vec::new(),
        };

        self.traces.push(span);

        // Keep only last 10000 traces
        if self.traces.len() > 10000 {
            self.traces.remove(0);
        }

        span_id
    }

    /// Finish a trace span
    pub fn finish_span(&mut self, span_id: Uuid, status: SpanStatus) {
        if let Some(span) = self.traces.iter_mut().find(|s| s.span_id == span_id) {
            span.end_time = Some(Utc::now());
            span.status = status;
            span.duration_ms = Some(
                span.end_time.unwrap()
                    .signed_duration_since(span.start_time)
                    .num_milliseconds() as u64
            );
        }
    }

    /// Add log to span
    pub fn add_span_log(&mut self, span_id: Uuid, level: String, message: String, fields: HashMap<String, serde_json::Value>) {
        if let Some(span) = self.traces.iter_mut().find(|s| s.span_id == span_id) {
            let log = SpanLog {
                timestamp: Utc::now(),
                level,
                message,
                fields,
            };
            span.logs.push(log);
        }
    }

    /// Perform health check
    pub async fn perform_health_check(&mut self, component: String) -> HealthCheckResult {
        let start_time = Utc::now();
        
        // Simulate health check
        let (status, message) = self.simulate_health_check(&component).await;
        let response_time = Utc::now().signed_duration_since(start_time).num_milliseconds() as u64;

        let result = HealthCheckResult {
            component: component.clone(),
            status,
            message,
            details: HashMap::new(),
            timestamp: Utc::now(),
            response_time_ms: response_time,
        };

        self.health_checks.push(result.clone());

        // Keep only last 1000 health checks
        if self.health_checks.len() > 1000 {
            self.health_checks.remove(0);
        }

        result
    }

    /// Simulate health check (placeholder for real health checks)
    async fn simulate_health_check(&self, component: &str) -> (HealthStatus, String) {
        // Simulate 95% success rate
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        component.hash(&mut hasher);
        let hash = hasher.finish();
        
        if hash % 100 < 95 {
            (HealthStatus::Healthy, "Component is healthy".to_string())
        } else if hash % 100 < 98 {
            (HealthStatus::Degraded, "Component is degraded".to_string())
        } else {
            (HealthStatus::Unhealthy, "Component is unhealthy".to_string())
        }
    }

    /// Check for alerts
    fn check_alerts(&mut self) {
        for (rule_name, rule) in &self.alert_rules.clone() {
            if !rule.enabled {
                continue;
            }

            let current_value = self.calculate_metric_value(&rule.condition);
            let should_alert = current_value > rule.threshold;

            // Check if alert is already firing
            let existing_alert = self.alerts.iter().find(|a| a.name == rule.name && a.status == AlertStatus::Firing);

            if should_alert && existing_alert.is_none() {
                // Trigger new alert
                let alert = Alert {
                    id: Uuid::new_v4(),
                    name: rule.name.clone(),
                    description: format!("{} exceeded threshold", rule.condition),
                    severity: rule.severity.clone(),
                    status: AlertStatus::Firing,
                    condition: rule.condition.clone(),
                    threshold: rule.threshold,
                    current_value,
                    triggered_at: Some(Utc::now()),
                    resolved_at: None,
                    labels: HashMap::new(),
                };

                self.alerts.push(alert);
            } else if !should_alert && existing_alert.is_some() {
                // Resolve existing alert
                if let Some(alert) = self.alerts.iter_mut().find(|a| a.name == rule.name && a.status == AlertStatus::Firing) {
                    alert.status = AlertStatus::Resolved;
                    alert.resolved_at = Some(Utc::now());
                }
            }
        }
    }

    /// Calculate metric value for alert condition
    fn calculate_metric_value(&self, condition: &str) -> f64 {
        // Simplified metric calculation (placeholder for real implementation)
        match condition {
            "error_rate > 0.05" => {
                let total_operations = self.traces.len();
                let error_operations = self.traces.iter().filter(|t| t.status == SpanStatus::Error).count();
                if total_operations > 0 {
                    error_operations as f64 / total_operations as f64
                } else {
                    0.0
                }
            }
            "avg_latency_ms > 5000" => {
                let durations: Vec<u64> = self.traces.iter()
                    .filter_map(|t| t.duration_ms)
                    .collect();
                if !durations.is_empty() {
                    durations.iter().sum::<u64>() as f64 / durations.len() as f64
                } else {
                    0.0
                }
            }
            "memory_usage_mb > 500" => {
                // Simulate memory usage
                100.0
            }
            _ => 0.0,
        }
    }

    /// Create performance profile
    pub fn create_performance_profile(&mut self, operation_name: String, start_time: chrono::DateTime<Utc>, end_time: chrono::DateTime<Utc>) -> PerformanceProfile {
        let duration = end_time.signed_duration_since(start_time).num_milliseconds() as u64;
        
        let profile = PerformanceProfile {
            id: Uuid::new_v4(),
            operation_name,
            start_time,
            end_time,
            duration_ms: duration,
            cpu_usage_percent: 25.0, // Simulated
            memory_usage_mb: 50.0,   // Simulated
            disk_io_bytes: 1024,     // Simulated
            network_io_bytes: 512,   // Simulated
            call_count: 1,
            error_count: 0,
            details: HashMap::new(),
        };

        self.performance_profiles.push(profile.clone());

        // Keep only last 1000 profiles
        if self.performance_profiles.len() > 1000 {
            self.performance_profiles.remove(0);
        }

        profile
    }

    /// Get monitoring dashboard data
    pub fn get_dashboard_data(&self) -> serde_json::Value {
        let total_metrics = self.metrics.len();
        let total_traces = self.traces.len();
        let active_alerts = self.alerts.iter().filter(|a| a.status == AlertStatus::Firing).count();
        let total_alerts = self.alerts.len();

        let recent_health_checks: Vec<&HealthCheckResult> = self.health_checks
            .iter()
            .rev()
            .take(10)
            .collect();

        let healthy_components = recent_health_checks.iter().filter(|h| h.status == HealthStatus::Healthy).count();
        let total_components = recent_health_checks.len();

        serde_json::json!({
            "summary": {
                "total_metrics": total_metrics,
                "total_traces": total_traces,
                "active_alerts": active_alerts,
                "total_alerts": total_alerts,
                "healthy_components": healthy_components,
                "total_components": total_components,
                "health_percentage": if total_components > 0 { healthy_components as f64 / total_components as f64 } else { 0.0 }
            },
            "recent_alerts": self.alerts.iter().rev().take(10).collect::<Vec<_>>(),
            "recent_health_checks": recent_health_checks,
            "performance_profiles": self.performance_profiles.iter().rev().take(10).collect::<Vec<_>>()
        })
    }

    /// Get metrics
    pub fn get_metrics(&self) -> &Vec<Metric> {
        &self.metrics
    }

    /// Get traces
    pub fn get_traces(&self) -> &Vec<TraceSpan> {
        &self.traces
    }

    /// Get alerts
    pub fn get_alerts(&self) -> &Vec<Alert> {
        &self.alerts
    }

    /// Get health checks
    pub fn get_health_checks(&self) -> &Vec<HealthCheckResult> {
        &self.health_checks
    }
}

#[async_trait::async_trait]
impl Agent for MonitoringAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Monitoring Agent initialized with ID: {}", self.id);
        tracing::info!("Observability and monitoring enabled");
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = MonitoringAgent::new();
        
        let result = match operation {
            "get_dashboard_data" => {
                serde_json::to_value(agent.get_dashboard_data())?
            }
            "perform_health_check" => {
                let component = input.get("component")
                    .and_then(|v| v.as_str())
                    .unwrap_or("default");
                serde_json::to_value(agent.perform_health_check(component.to_string()).await)?
            }
            "start_span" => {
                let operation_name = input.get("operation_name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                serde_json::to_value(agent.start_span(operation_name.to_string(), None))?
            }
            "record_metric" => {
                let name = input.get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                let value = input.get("value")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0);
                
                agent.record_metric(
                    name.to_string(),
                    MetricValue::Gauge(value),
                    HashMap::new(),
                    "Manual metric".to_string()
                );
                
                serde_json::json!({"success": true})
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "MonitoringAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "metrics_collection".to_string(),
                "distributed_tracing".to_string(),
                "health_monitoring".to_string(),
                "alerting".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Monitoring agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Monitoring Agent shutting down");
        tracing::info!("Collected {} metrics", self.metrics.len());
        tracing::info!("Traced {} operations", self.traces.len());
        tracing::info!("Generated {} alerts", self.alerts.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_monitoring_agent_creation() {
        let agent = MonitoringAgent::new();
        
        assert_eq!(agent.metrics.len(), 0);
        assert_eq!(agent.traces.len(), 0);
        assert!(!agent.alert_rules.is_empty());
    }

    #[test]
    fn test_metric_recording() {
        let mut agent = MonitoringAgent::new();
        
        agent.record_metric(
            "test_metric".to_string(),
            MetricValue::Counter(42),
            HashMap::new(),
            "Test metric".to_string()
        );
        
        assert_eq!(agent.metrics.len(), 1);
        assert_eq!(agent.metrics[0].name, "test_metric");
    }

    #[test]
    fn test_trace_span_creation() {
        let mut agent = MonitoringAgent::new();
        
        let span_id = agent.start_span("test_operation".to_string(), None);
        assert_eq!(agent.traces.len(), 1);
        
        agent.finish_span(span_id, SpanStatus::Ok);
        let span = &agent.traces[0];
        assert!(span.end_time.is_some());
        assert_eq!(span.status, SpanStatus::Ok);
    }

    #[tokio::test]
    async fn test_health_check() {
        let mut agent = MonitoringAgent::new();
        
        let result = agent.perform_health_check("test_component".to_string()).await;
        
        assert_eq!(result.component, "test_component");
        assert!(result.response_time_ms > 0);
    }

    #[test]
    fn test_performance_profile() {
        let mut agent = MonitoringAgent::new();
        
        let start_time = Utc::now();
        let end_time = start_time + chrono::Duration::milliseconds(1000);
        
        let profile = agent.create_performance_profile("test_operation".to_string(), start_time, end_time);
        
        assert_eq!(profile.operation_name, "test_operation");
        assert_eq!(profile.duration_ms, 1000);
    }

    #[test]
    fn test_dashboard_data() {
        let mut agent = MonitoringAgent::new();
        
        // Add some data
        agent.record_metric("test".to_string(), MetricValue::Counter(1), HashMap::new(), "test".to_string());
        let span_id = agent.start_span("test".to_string(), None);
        agent.finish_span(span_id, SpanStatus::Ok);
        
        let dashboard = agent.get_dashboard_data();
        assert!(dashboard.get("summary").is_some());
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = MonitoringAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "get_dashboard_data"
        });
        
        let result = agent.execute(input).await.unwrap();
        assert!(result.get("summary").is_some());
    }
}
