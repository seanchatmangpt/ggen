//! Observability layer with tracing, metrics, and structured logging

use crate::error::GatewayError;
use serde::Serialize;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

/// Request metrics
#[derive(Debug, Clone, Serialize)]
pub struct RequestMetrics {
    /// Total requests processed
    pub total_requests: u64,
    /// Successful requests
    pub successful_requests: u64,
    /// Failed requests
    pub failed_requests: u64,
    /// Rate-limited requests
    pub rate_limited_requests: u64,
    /// Average response time in milliseconds
    pub avg_response_time_ms: f64,
    /// Total bytes processed
    pub total_bytes_processed: u64,
}

/// Gateway metrics collector
#[derive(Debug, Clone)]
pub struct MetricsCollector {
    total_requests: Arc<AtomicU64>,
    successful_requests: Arc<AtomicU64>,
    failed_requests: Arc<AtomicU64>,
    rate_limited_requests: Arc<AtomicU64>,
    total_response_time_ms: Arc<AtomicU64>,
    total_bytes_processed: Arc<AtomicU64>,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            total_requests: Arc::new(AtomicU64::new(0)),
            successful_requests: Arc::new(AtomicU64::new(0)),
            failed_requests: Arc::new(AtomicU64::new(0)),
            rate_limited_requests: Arc::new(AtomicU64::new(0)),
            total_response_time_ms: Arc::new(AtomicU64::new(0)),
            total_bytes_processed: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Record a request
    pub fn record_request(&self) {
        self.total_requests.fetch_add(1, Ordering::Relaxed);
    }

    /// Record a successful response
    pub fn record_success(&self, response_time_ms: u64) {
        self.successful_requests.fetch_add(1, Ordering::Relaxed);
        self.total_response_time_ms.fetch_add(response_time_ms, Ordering::Relaxed);
    }

    /// Record a failed response
    pub fn record_failure(&self, response_time_ms: u64) {
        self.failed_requests.fetch_add(1, Ordering::Relaxed);
        self.total_response_time_ms.fetch_add(response_time_ms, Ordering::Relaxed);
    }

    /// Record a rate-limited request
    pub fn record_rate_limited(&self) {
        self.rate_limited_requests.fetch_add(1, Ordering::Relaxed);
    }

    /// Record bytes processed
    pub fn record_bytes(&self, bytes: u64) {
        self.total_bytes_processed.fetch_add(bytes, Ordering::Relaxed);
    }

    /// Get current metrics
    pub fn metrics(&self) -> RequestMetrics {
        let total = self.total_requests.load(Ordering::Relaxed);
        let successful = self.successful_requests.load(Ordering::Relaxed);
        let failed = self.failed_requests.load(Ordering::Relaxed);
        let rate_limited = self.rate_limited_requests.load(Ordering::Relaxed);
        let total_time = self.total_response_time_ms.load(Ordering::Relaxed);
        let total_bytes = self.total_bytes_processed.load(Ordering::Relaxed);

        let avg_response_time = if successful + failed > 0 {
            total_time as f64 / (successful + failed) as f64
        } else {
            0.0
        };

        RequestMetrics {
            total_requests: total,
            successful_requests: successful,
            failed_requests: failed,
            rate_limited_requests: rate_limited,
            avg_response_time_ms: avg_response_time,
            total_bytes_processed: total_bytes,
        }
    }

    /// Reset all metrics
    pub fn reset(&self) {
        self.total_requests.store(0, Ordering::Relaxed);
        self.successful_requests.store(0, Ordering::Relaxed);
        self.failed_requests.store(0, Ordering::Relaxed);
        self.rate_limited_requests.store(0, Ordering::Relaxed);
        self.total_response_time_ms.store(0, Ordering::Relaxed);
        self.total_bytes_processed.store(0, Ordering::Relaxed);
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Structured log event
#[derive(Debug, Clone, Serialize)]
pub struct LogEvent {
    /// Event level (DEBUG, INFO, WARN, ERROR)
    pub level: String,
    /// Event message
    pub message: String,
    /// Request ID for correlation
    pub request_id: Option<String>,
    /// Event timestamp (ISO 8601)
    pub timestamp: String,
    /// Additional context
    pub context: std::collections::HashMap<String, String>,
}

impl LogEvent {
    /// Create a new log event
    pub fn new(level: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            level: level.into(),
            message: message.into(),
            request_id: None,
            timestamp: chrono::Utc::now().to_rfc3339(),
            context: std::collections::HashMap::new(),
        }
    }

    /// Set request ID
    pub fn with_request_id(mut self, id: String) -> Self {
        self.request_id = Some(id);
        self
    }

    /// Add context field
    pub fn with_context(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.context.insert(key.into(), value.into());
        self
    }
}

/// Request timing tracker
pub struct TimingTracker {
    start: Instant,
}

impl TimingTracker {
    /// Start a new timer
    pub fn start() -> Self {
        Self {
            start: Instant::now(),
        }
    }

    /// Get elapsed time in milliseconds
    pub fn elapsed_ms(&self) -> u64 {
        self.start.elapsed().as_millis() as u64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_collector_creation() {
        let collector = MetricsCollector::new();
        let metrics = collector.metrics();

        assert_eq!(metrics.total_requests, 0);
        assert_eq!(metrics.successful_requests, 0);
    }

    #[test]
    fn test_metrics_collector_record_request() {
        let collector = MetricsCollector::new();

        collector.record_request();
        collector.record_request();

        let metrics = collector.metrics();
        assert_eq!(metrics.total_requests, 2);
    }

    #[test]
    fn test_metrics_collector_record_success() {
        let collector = MetricsCollector::new();

        collector.record_request();
        collector.record_success(100);
        collector.record_success(200);

        let metrics = collector.metrics();
        assert_eq!(metrics.successful_requests, 2);
        assert_eq!(metrics.avg_response_time_ms, 150.0);
    }

    #[test]
    fn test_metrics_collector_record_rate_limited() {
        let collector = MetricsCollector::new();

        collector.record_rate_limited();
        collector.record_rate_limited();

        let metrics = collector.metrics();
        assert_eq!(metrics.rate_limited_requests, 2);
    }

    #[test]
    fn test_metrics_collector_reset() {
        let collector = MetricsCollector::new();

        collector.record_request();
        collector.record_success(100);

        let metrics = collector.metrics();
        assert_eq!(metrics.total_requests, 1);

        collector.reset();
        let metrics = collector.metrics();
        assert_eq!(metrics.total_requests, 0);
        assert_eq!(metrics.successful_requests, 0);
    }

    #[test]
    fn test_log_event_creation() {
        let event = LogEvent::new("INFO", "Test message")
            .with_request_id("req-123".to_string())
            .with_context("user_id", "user-456");

        assert_eq!(event.level, "INFO");
        assert_eq!(event.message, "Test message");
        assert_eq!(event.request_id, Some("req-123".to_string()));
        assert_eq!(event.context.get("user_id"), Some(&"user-456".to_string()));
    }

    #[test]
    fn test_timing_tracker() {
        let tracker = TimingTracker::start();
        std::thread::sleep(std::time::Duration::from_millis(10));
        let elapsed = tracker.elapsed_ms();
        assert!(elapsed >= 10);
    }
}
