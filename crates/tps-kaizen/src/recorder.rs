//! Event recording and time-series aggregation
//!
//! Records all Kaizen-relevant events:
//! - Jidoka failures and circuit opens
//! - Kanban capacity changes
//! - Andon alerts
//! - Heijunka rebalancing
//!
//! Aggregates data over time windows (1m, 5m, 1h, 1d, 1w)

use crate::{KaizenMetrics, Result, TimestampedValue};
use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;

/// Event type for recording
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub enum EventType {
    /// Jidoka: circuit breaker opened
    CircuitOpen { service: String },
    /// Jidoka: circuit breaker closed
    CircuitClosed { service: String },
    /// Jidoka: failure detected
    Failure { service: String, reason: String },
    /// Kanban: queue depth changed
    QueueDepthChanged { queue: String, depth: i32 },
    /// Kanban: task completed
    TaskCompleted { queue: String, latency_ms: f64 },
    /// Andon: alert fired
    AlertFired { alert_type: String, severity: String },
    /// Andon: alert cleared
    AlertCleared { alert_type: String },
    /// Heijunka: worker activated
    WorkerActivated { worker_id: String },
    /// Heijunka: worker deactivated
    WorkerDeactivated { worker_id: String },
    /// Custom metric recorded
    CustomMetric { name: String, value: f64 },
}

/// Recorded event
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct RecordedEvent {
    /// Event timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Event type
    pub event_type: EventType,
    /// Event metadata
    pub metadata: HashMap<String, String>,
}

/// Metric aggregation over time windows
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Aggregation {
    /// Window start time
    pub window_start: chrono::DateTime<chrono::Utc>,
    /// Window end time
    pub window_end: chrono::DateTime<chrono::Utc>,
    /// Aggregated value (mean, sum, etc.)
    pub value: f64,
    /// Count of samples in window
    pub sample_count: u64,
    /// Min value in window
    pub min: f64,
    /// Max value in window
    pub max: f64,
    /// Stddev of samples
    pub stddev: f64,
}

/// Metric recorder - records events and aggregates metrics
#[derive(Clone)]
pub struct MetricRecorder {
    metrics: Arc<KaizenMetrics>,
    /// Event log
    events: Arc<DashMap<String, Vec<RecordedEvent>>>,
    /// Time-series aggregations
    aggregations: Arc<DashMap<String, Vec<Aggregation>>>,
}

impl MetricRecorder {
    /// Create new metric recorder
    pub fn new(metrics: Arc<KaizenMetrics>) -> Self {
        Self {
            metrics,
            events: Arc::new(DashMap::new()),
            aggregations: Arc::new(DashMap::new()),
        }
    }

    /// Record event
    pub async fn record_event(&self, event_type: EventType, metadata: HashMap<String, String>) -> Result<()> {
        let event = RecordedEvent {
            timestamp: chrono::Utc::now(),
            event_type,
            metadata,
        };

        let key = format!("{:?}", &event.event_type);
        self.events
            .entry(key)
            .or_insert_with(Vec::new)
            .push(event);

        Ok(())
    }

    /// Record circuit open event
    pub async fn record_circuit_open(&self, service: &str) -> Result<()> {
        self.metrics.jidoka.total_opens.inc();
        self.metrics.jidoka.circuit_open_percent.add(1.0);
        self.metrics.jidoka.circuit_status.set(1);

        let mut metadata = HashMap::new();
        metadata.insert("service".to_string(), service.to_string());

        self.record_event(EventType::CircuitOpen { service: service.to_string() }, metadata)
            .await
    }

    /// Record circuit closed event
    pub async fn record_circuit_closed(&self, service: &str) -> Result<()> {
        self.metrics.jidoka.circuit_open_percent.sub(1.0);
        self.metrics.jidoka.circuit_status.set(0);

        let mut metadata = HashMap::new();
        metadata.insert("service".to_string(), service.to_string());

        self.record_event(EventType::CircuitClosed { service: service.to_string() }, metadata)
            .await
    }

    /// Record failure
    pub async fn record_failure(&self, service: &str, reason: &str) -> Result<()> {
        self.metrics.jidoka.total_failures.inc();
        self.metrics.jidoka.failure_rate.add(1.0);

        let mut metadata = HashMap::new();
        metadata.insert("service".to_string(), service.to_string());
        metadata.insert("reason".to_string(), reason.to_string());

        self.record_event(
            EventType::Failure {
                service: service.to_string(),
                reason: reason.to_string(),
            },
            metadata,
        )
        .await
    }

    /// Record queue depth change
    pub async fn record_queue_depth(&self, queue: &str, depth: i32) -> Result<()> {
        self.metrics.kanban.queue_depth.set(depth as f64);

        let mut metadata = HashMap::new();
        metadata.insert("queue".to_string(), queue.to_string());
        metadata.insert("depth".to_string(), depth.to_string());

        self.record_event(EventType::QueueDepthChanged { queue: queue.to_string(), depth }, metadata)
            .await
    }

    /// Record task completion
    pub async fn record_task_completed(&self, queue: &str, latency_ms: f64) -> Result<()> {
        self.metrics.kanban.latency_histogram.observe(latency_ms / 1000.0);
        self.metrics.kanban.latency_p99_ms.set(latency_ms);
        self.metrics.kanban.throughput_per_min.add(1.0);

        let mut metadata = HashMap::new();
        metadata.insert("queue".to_string(), queue.to_string());
        metadata.insert("latency_ms".to_string(), latency_ms.to_string());

        self.record_event(EventType::TaskCompleted { queue: queue.to_string(), latency_ms }, metadata)
            .await
    }

    /// Record alert fired
    pub async fn record_alert_fired(&self, alert_type: &str, severity: &str) -> Result<()> {
        self.metrics.andon.alert_frequency_per_min.add(1.0);
        self.metrics.andon.alert_types_total.inc();

        if severity == "critical" {
            self.metrics.andon.critical_alerts.add(1.0);
        }

        let mut metadata = HashMap::new();
        metadata.insert("alert_type".to_string(), alert_type.to_string());
        metadata.insert("severity".to_string(), severity.to_string());

        self.record_event(
            EventType::AlertFired {
                alert_type: alert_type.to_string(),
                severity: severity.to_string(),
            },
            metadata,
        )
        .await
    }

    /// Record alert cleared
    pub async fn record_alert_cleared(&self, alert_type: &str) -> Result<()> {
        self.metrics.andon.critical_alerts.sub(1.0);

        let mut metadata = HashMap::new();
        metadata.insert("alert_type".to_string(), alert_type.to_string());

        self.record_event(EventType::AlertCleared { alert_type: alert_type.to_string() }, metadata)
            .await
    }

    /// Record worker activation
    pub async fn record_worker_activated(&self, worker_id: &str) -> Result<()> {
        self.metrics.heijunka.workers_active.inc();

        let mut metadata = HashMap::new();
        metadata.insert("worker_id".to_string(), worker_id.to_string());

        self.record_event(EventType::WorkerActivated { worker_id: worker_id.to_string() }, metadata)
            .await
    }

    /// Record worker deactivation
    pub async fn record_worker_deactivated(&self, worker_id: &str) -> Result<()> {
        self.metrics.heijunka.workers_active.dec();
        self.metrics.heijunka.workers_idle.inc();

        let mut metadata = HashMap::new();
        metadata.insert("worker_id".to_string(), worker_id.to_string());

        self.record_event(EventType::WorkerDeactivated { worker_id: worker_id.to_string() }, metadata)
            .await
    }

    /// Record custom metric
    pub async fn record_custom_metric(&self, name: &str, value: f64) -> Result<()> {
        let mut tags = HashMap::new();
        tags.insert("metric".to_string(), name.to_string());
        self.metrics.record_value(name.to_string(), value, tags);

        let mut metadata = HashMap::new();
        metadata.insert("name".to_string(), name.to_string());
        metadata.insert("value".to_string(), value.to_string());

        self.record_event(EventType::CustomMetric { name: name.to_string(), value }, metadata)
            .await
    }

    /// Aggregate metrics over a time window
    pub async fn aggregate_window(
        &self,
        metric: &str,
        window_secs: u64,
    ) -> Result<Option<Aggregation>> {
        let history = self.metrics.get_history(metric);

        if history.is_empty() {
            return Ok(None);
        }

        let now = chrono::Utc::now();
        let window_start = now - chrono::Duration::seconds(window_secs as i64);

        let values: Vec<f64> = history
            .iter()
            .filter(|v| v.timestamp >= window_start && v.timestamp <= now)
            .map(|v| v.value)
            .collect();

        if values.is_empty() {
            return Ok(None);
        }

        let sum: f64 = values.iter().sum();
        let mean = sum / values.len() as f64;
        let min = values.iter().cloned().fold(f64::INFINITY, f64::min);
        let max = values.iter().cloned().fold(f64::NEG_INFINITY, f64::max);

        let variance = values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / values.len() as f64;
        let stddev = variance.sqrt();

        Ok(Some(Aggregation {
            window_start,
            window_end: now,
            value: mean,
            sample_count: values.len() as u64,
            min,
            max,
            stddev,
        }))
    }

    /// Get all recorded events
    pub fn get_events(&self) -> Vec<RecordedEvent> {
        self.events
            .iter()
            .flat_map(|r| r.value().clone())
            .collect()
    }

    /// Get events by type
    pub fn get_events_by_type(&self, event_type_str: &str) -> Vec<RecordedEvent> {
        self.events
            .iter()
            .filter(|r| r.key().contains(event_type_str))
            .flat_map(|r| r.value().clone())
            .collect()
    }

    /// Retain events only within the last `duration`
    pub async fn retain_events_within(&self, duration: chrono::Duration) {
        let cutoff = chrono::Utc::now() - duration;
        self.events.iter_mut().for_each(|mut r| {
            r.value_mut().retain(|e| e.timestamp > cutoff);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_record_circuit_open() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let recorder = MetricRecorder::new(metrics);

        recorder.record_circuit_open("payment-service").await.expect("Failed to record");
        let events = recorder.get_events_by_type("CircuitOpen");

        assert!(!events.is_empty());
    }

    #[tokio::test]
    async fn test_record_task_completed() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let recorder = MetricRecorder::new(metrics);

        recorder.record_task_completed("checkout", 150.0).await.expect("Failed to record");
        let events = recorder.get_events_by_type("TaskCompleted");

        assert!(!events.is_empty());
    }

    #[tokio::test]
    async fn test_aggregate_window() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let recorder = MetricRecorder::new(metrics.clone());

        // Record some custom metrics
        recorder.record_custom_metric("test_metric", 10.0).await.ok();
        recorder.record_custom_metric("test_metric", 20.0).await.ok();
        recorder.record_custom_metric("test_metric", 30.0).await.ok();

        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        let agg = recorder.aggregate_window("test_metric", 60).await.expect("Failed to aggregate");

        assert!(agg.is_some());
        let agg = agg.unwrap();
        assert_eq!(agg.sample_count, 3);
        assert_eq!(agg.value, 20.0); // mean of 10, 20, 30
        assert_eq!(agg.min, 10.0);
        assert_eq!(agg.max, 30.0);
    }
}
