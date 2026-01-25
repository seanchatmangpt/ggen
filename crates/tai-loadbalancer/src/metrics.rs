//! Metrics collection and observability

use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::SystemTime;

/// Load balancer metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LoadBalancerMetrics {
    /// Total requests processed
    pub total_requests: u64,
    /// Total successful requests
    pub successful_requests: u64,
    /// Total failed requests
    pub failed_requests: u64,
    /// Total timeouts
    pub timeouts: u64,
    /// Average latency in milliseconds
    pub average_latency_ms: f64,
    /// p50 latency in milliseconds
    pub p50_latency_ms: f64,
    /// p95 latency in milliseconds
    pub p95_latency_ms: f64,
    /// p99 latency in milliseconds
    pub p99_latency_ms: f64,
    /// Last update timestamp
    pub last_updated: Option<SystemTime>,
}

/// Atomic metrics for concurrent updates
pub struct AtomicMetrics {
    total_requests: AtomicU64,
    successful_requests: AtomicU64,
    failed_requests: AtomicU64,
    timeouts: AtomicU64,
}

impl AtomicMetrics {
    /// Create new atomic metrics
    pub fn new() -> Self {
        Self {
            total_requests: AtomicU64::new(0),
            successful_requests: AtomicU64::new(0),
            failed_requests: AtomicU64::new(0),
            timeouts: AtomicU64::new(0),
        }
    }

    /// Increment total requests
    pub fn record_request(&self) {
        self.total_requests.fetch_add(1, Ordering::Relaxed);
    }

    /// Record successful request
    pub fn record_success(&self) {
        self.successful_requests.fetch_add(1, Ordering::Relaxed);
    }

    /// Record failed request
    pub fn record_failure(&self) {
        self.failed_requests.fetch_add(1, Ordering::Relaxed);
    }

    /// Record timeout
    pub fn record_timeout(&self) {
        self.timeouts.fetch_add(1, Ordering::Relaxed);
    }

    /// Get current metrics snapshot
    pub fn snapshot(&self) -> LoadBalancerMetrics {
        LoadBalancerMetrics {
            total_requests: self.total_requests.load(Ordering::Relaxed),
            successful_requests: self.successful_requests.load(Ordering::Relaxed),
            failed_requests: self.failed_requests.load(Ordering::Relaxed),
            timeouts: self.timeouts.load(Ordering::Relaxed),
            average_latency_ms: 0.0,
            p50_latency_ms: 0.0,
            p95_latency_ms: 0.0,
            p99_latency_ms: 0.0,
            last_updated: Some(SystemTime::now()),
        }
    }

    /// Reset all metrics
    pub fn reset(&self) {
        self.total_requests.store(0, Ordering::Relaxed);
        self.successful_requests.store(0, Ordering::Relaxed);
        self.failed_requests.store(0, Ordering::Relaxed);
        self.timeouts.store(0, Ordering::Relaxed);
    }
}

impl Default for AtomicMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Latency histogram for percentile calculations
#[derive(Debug, Clone)]
pub struct LatencyHistogram {
    buckets: Vec<u64>,
    bucket_size_ms: u64,
}

impl LatencyHistogram {
    /// Create a new histogram
    pub fn new(bucket_count: usize, bucket_size_ms: u64) -> Self {
        Self {
            buckets: vec![0; bucket_count],
            bucket_size_ms,
        }
    }

    /// Record a latency measurement
    pub fn record(&mut self, latency_ms: u64) {
        let bucket_idx = (latency_ms / self.bucket_size_ms) as usize;
        if bucket_idx < self.buckets.len() {
            self.buckets[bucket_idx] += 1;
        } else {
            self.buckets[self.buckets.len() - 1] += 1;
        }
    }

    /// Get percentile latency
    pub fn percentile(&self, percentile: f64) -> u64 {
        let total: u64 = self.buckets.iter().sum();
        if total == 0 {
            return 0;
        }

        let target_count = (total as f64 * percentile / 100.0) as u64;
        let mut count = 0;

        for (idx, &bucket_count) in self.buckets.iter().enumerate() {
            count += bucket_count;
            if count >= target_count {
                return (idx as u64) * self.bucket_size_ms;
            }
        }

        ((self.buckets.len() - 1) as u64) * self.bucket_size_ms
    }

    /// Reset histogram
    pub fn reset(&mut self) {
        self.buckets.fill(0);
    }
}

impl Default for LatencyHistogram {
    fn default() -> Self {
        Self::new(100, 10) // 100 buckets of 10ms each (0-1000ms range)
    }
}

/// Per-endpoint metrics
#[derive(Debug, Clone, Default)]
pub struct EndpointMetrics {
    /// Total requests to this endpoint
    pub total_requests: u64,
    /// Successful requests
    pub successful_requests: u64,
    /// Failed requests
    pub failed_requests: u64,
    /// Average latency
    pub average_latency_ms: f64,
    /// Last request timestamp
    pub last_request: Option<SystemTime>,
}

/// Per-service metrics tracker
pub struct ServiceMetricsTracker {
    metrics: Arc<parking_lot::RwLock<std::collections::HashMap<String, EndpointMetrics>>>,
}

impl ServiceMetricsTracker {
    /// Create a new tracker
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(parking_lot::RwLock::new(std::collections::HashMap::new())),
        }
    }

    /// Record a request to an endpoint
    pub fn record_request(
        &self,
        endpoint: &str,
        success: bool,
        latency_ms: f64,
    ) {
        let mut metrics = self.metrics.write();
        let entry = metrics.entry(endpoint.to_string()).or_insert_with(EndpointMetrics::default);

        entry.total_requests += 1;
        entry.last_request = Some(SystemTime::now());

        if success {
            entry.successful_requests += 1;
        } else {
            entry.failed_requests += 1;
        }

        // Update average latency (simple moving average)
        if entry.average_latency_ms == 0.0 {
            entry.average_latency_ms = latency_ms;
        } else {
            entry.average_latency_ms = (entry.average_latency_ms * 0.9) + (latency_ms * 0.1);
        }
    }

    /// Get metrics for an endpoint
    pub fn get_metrics(&self, endpoint: &str) -> Option<EndpointMetrics> {
        self.metrics.read().get(endpoint).cloned()
    }

    /// Get all metrics
    pub fn get_all_metrics(&self) -> Vec<(String, EndpointMetrics)> {
        self.metrics
            .read()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Reset metrics
    pub fn reset(&self) {
        self.metrics.write().clear();
    }
}

impl Default for ServiceMetricsTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_metrics() {
        let metrics = AtomicMetrics::new();
        metrics.record_request();
        metrics.record_success();
        metrics.record_request();
        metrics.record_failure();

        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.total_requests, 2);
        assert_eq!(snapshot.successful_requests, 1);
        assert_eq!(snapshot.failed_requests, 1);
    }

    #[test]
    fn test_latency_histogram() {
        let mut histogram = LatencyHistogram::new(100, 10);

        // Record some latencies
        for _ in 0..50 {
            histogram.record(15); // p50 should be around 20ms
        }
        for _ in 0..40 {
            histogram.record(50); // p95 should be around 50ms
        }
        for _ in 0..10 {
            histogram.record(100); // p99 should be around 100ms
        }

        assert!(histogram.percentile(50.0) <= 50);
        assert!(histogram.percentile(95.0) <= 100);
        assert!(histogram.percentile(99.0) <= 110);
    }

    #[test]
    fn test_service_metrics_tracker() {
        let tracker = ServiceMetricsTracker::new();

        tracker.record_request("endpoint-1", true, 10.0);
        tracker.record_request("endpoint-1", true, 20.0);
        tracker.record_request("endpoint-1", false, 30.0);

        let metrics = tracker.get_metrics("endpoint-1").unwrap();
        assert_eq!(metrics.total_requests, 3);
        assert_eq!(metrics.successful_requests, 2);
        assert_eq!(metrics.failed_requests, 1);
        assert!(metrics.average_latency_ms > 0.0);
    }
}
