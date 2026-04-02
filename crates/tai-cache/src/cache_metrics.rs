//! Cache metrics and monitoring for observability and optimization.
//!
//! Tracks:
//! - Cache hit/miss rate
//! - Cache size monitoring
//! - Eviction rate (LRU evictions)
//! - Memory usage
//! - Operations per second
//! - Latency (cache hit vs miss)

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

/// Cache operation metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheMetrics {
    /// Total hits
    pub hits: u64,
    /// Total misses
    pub misses: u64,
    /// Total evictions
    pub evictions: u64,
    /// Total operations
    pub total_ops: u64,
    /// Average hit latency in microseconds
    pub avg_hit_latency_us: u64,
    /// Average miss latency in microseconds
    pub avg_miss_latency_us: u64,
    /// Cache size in bytes
    pub cache_size_bytes: u64,
    /// Number of items in cache
    pub item_count: u64,
    /// Peak cache size in bytes
    pub peak_cache_size_bytes: u64,
}

impl CacheMetrics {
    /// Calculate hit rate as percentage
    pub fn hit_rate(&self) -> f64 {
        if self.total_ops == 0 {
            0.0
        } else {
            (self.hits as f64 / self.total_ops as f64) * 100.0
        }
    }

    /// Calculate miss rate as percentage
    pub fn miss_rate(&self) -> f64 {
        100.0 - self.hit_rate()
    }

    /// Calculate eviction rate (evictions per operation)
    pub fn eviction_rate(&self) -> f64 {
        if self.total_ops == 0 {
            0.0
        } else {
            (self.evictions as f64 / self.total_ops as f64) * 100.0
        }
    }

    /// Format metrics as human-readable string
    pub fn format_summary(&self) -> String {
        format!(
            "Cache Metrics:\n  \
             Hit Rate: {:.2}%\n  \
             Miss Rate: {:.2}%\n  \
             Evictions: {}\n  \
             Eviction Rate: {:.2}%\n  \
             Total Ops: {}\n  \
             Avg Hit Latency: {}µs\n  \
             Avg Miss Latency: {}µs\n  \
             Cache Size: {} bytes\n  \
             Items: {}\n  \
             Peak Size: {} bytes",
            self.hit_rate(),
            self.miss_rate(),
            self.evictions,
            self.eviction_rate(),
            self.total_ops,
            self.avg_hit_latency_us,
            self.avg_miss_latency_us,
            self.cache_size_bytes,
            self.item_count,
            self.peak_cache_size_bytes,
        )
    }
}

/// Metrics collector for caches
pub struct MetricsCollector {
    /// Hit counter
    hits: Arc<AtomicU64>,
    /// Miss counter
    misses: Arc<AtomicU64>,
    /// Eviction counter
    evictions: Arc<AtomicU64>,
    /// Total operations counter
    total_ops: Arc<AtomicU64>,
    /// Sum of hit latencies (for average calculation)
    hit_latency_sum: Arc<AtomicU64>,
    /// Sum of miss latencies (for average calculation)
    miss_latency_sum: Arc<AtomicU64>,
    /// Current cache size in bytes
    cache_size: Arc<AtomicU64>,
    /// Peak cache size
    peak_cache_size: Arc<AtomicU64>,
    /// Item count
    item_count: Arc<AtomicU64>,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            hits: Arc::new(AtomicU64::new(0)),
            misses: Arc::new(AtomicU64::new(0)),
            evictions: Arc::new(AtomicU64::new(0)),
            total_ops: Arc::new(AtomicU64::new(0)),
            hit_latency_sum: Arc::new(AtomicU64::new(0)),
            miss_latency_sum: Arc::new(AtomicU64::new(0)),
            cache_size: Arc::new(AtomicU64::new(0)),
            peak_cache_size: Arc::new(AtomicU64::new(0)),
            item_count: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Record a cache hit with latency
    pub fn record_hit(&self, latency_us: u64) {
        self.hits.fetch_add(1, Ordering::Relaxed);
        self.total_ops.fetch_add(1, Ordering::Relaxed);
        self.hit_latency_sum.fetch_add(latency_us, Ordering::Relaxed);
        debug!("Cache hit recorded: {}µs latency", latency_us);
    }

    /// Record a cache miss with latency
    pub fn record_miss(&self, latency_us: u64) {
        self.misses.fetch_add(1, Ordering::Relaxed);
        self.total_ops.fetch_add(1, Ordering::Relaxed);
        self.miss_latency_sum.fetch_add(latency_us, Ordering::Relaxed);
        debug!("Cache miss recorded: {}µs latency", latency_us);
    }

    /// Record an eviction
    pub fn record_eviction(&self) {
        self.evictions.fetch_add(1, Ordering::Relaxed);
        debug!("Cache eviction recorded");
    }

    /// Update cache size
    pub fn update_cache_size(&self, bytes: u64) {
        self.cache_size.store(bytes, Ordering::Relaxed);

        // Update peak if necessary
        loop {
            let peak = self.peak_cache_size.load(Ordering::Relaxed);
            if bytes <= peak {
                break;
            }
            if self.peak_cache_size.compare_exchange(
                peak,
                bytes,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ).is_ok() {
                break;
            }
        }
    }

    /// Update item count
    pub fn update_item_count(&self, count: u64) {
        self.item_count.store(count, Ordering::Relaxed);
    }

    /// Get current metrics snapshot
    pub fn snapshot(&self) -> CacheMetrics {
        let hits = self.hits.load(Ordering::Relaxed);
        let misses = self.misses.load(Ordering::Relaxed);
        let total_ops = hits + misses;
        let hit_latency_sum = self.hit_latency_sum.load(Ordering::Relaxed);
        let miss_latency_sum = self.miss_latency_sum.load(Ordering::Relaxed);

        let avg_hit_latency_us = if hits > 0 {
            hit_latency_sum / hits
        } else {
            0
        };

        let avg_miss_latency_us = if misses > 0 {
            miss_latency_sum / misses
        } else {
            0
        };

        CacheMetrics {
            hits,
            misses,
            evictions: self.evictions.load(Ordering::Relaxed),
            total_ops,
            avg_hit_latency_us,
            avg_miss_latency_us,
            cache_size_bytes: self.cache_size.load(Ordering::Relaxed),
            item_count: self.item_count.load(Ordering::Relaxed),
            peak_cache_size_bytes: self.peak_cache_size.load(Ordering::Relaxed),
        }
    }

    /// Reset all metrics
    pub fn reset(&self) {
        self.hits.store(0, Ordering::Relaxed);
        self.misses.store(0, Ordering::Relaxed);
        self.evictions.store(0, Ordering::Relaxed);
        self.total_ops.store(0, Ordering::Relaxed);
        self.hit_latency_sum.store(0, Ordering::Relaxed);
        self.miss_latency_sum.store(0, Ordering::Relaxed);
        self.cache_size.store(0, Ordering::Relaxed);
        self.item_count.store(0, Ordering::Relaxed);
        info!("Metrics reset");
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for MetricsCollector {
    fn clone(&self) -> Self {
        Self {
            hits: Arc::clone(&self.hits),
            misses: Arc::clone(&self.misses),
            evictions: Arc::clone(&self.evictions),
            total_ops: Arc::clone(&self.total_ops),
            hit_latency_sum: Arc::clone(&self.hit_latency_sum),
            miss_latency_sum: Arc::clone(&self.miss_latency_sum),
            cache_size: Arc::clone(&self.cache_size),
            peak_cache_size: Arc::clone(&self.peak_cache_size),
            item_count: Arc::clone(&self.item_count),
        }
    }
}

/// Time-series metrics for tracking trends
pub struct TimeSeriesMetrics {
    /// Snapshots indexed by timestamp
    snapshots: Arc<DashMap<Instant, CacheMetrics>>,
    /// Retention duration
    retention: Duration,
}

impl TimeSeriesMetrics {
    /// Create a new time-series metrics tracker
    pub fn new(retention: Duration) -> Self {
        Self {
            snapshots: Arc::new(DashMap::new()),
            retention,
        }
    }

    /// Record a metrics snapshot
    pub fn record(&self, metrics: CacheMetrics) {
        let now = Instant::now();
        self.snapshots.insert(now, metrics);

        // Cleanup old entries
        let cutoff = now - self.retention;
        self.snapshots.retain(|instant, _| *instant > cutoff);
    }

    /// Get metrics trend over time
    pub fn get_trend(&self) -> Vec<(Instant, CacheMetrics)> {
        let mut entries: Vec<_> = self.snapshots
            .iter()
            .map(|entry| (*entry.key(), entry.value().clone()))
            .collect();
        entries.sort_by_key(|(instant, _)| *instant);
        entries
    }

    /// Calculate average hit rate over time
    pub fn avg_hit_rate(&self) -> f64 {
        let entries = self.get_trend();
        if entries.is_empty() {
            return 0.0;
        }

        let sum: f64 = entries.iter().map(|(_, m)| m.hit_rate()).sum();
        sum / entries.len() as f64
    }

    /// Calculate average miss rate over time
    pub fn avg_miss_rate(&self) -> f64 {
        100.0 - self.avg_hit_rate()
    }

    /// Clear all time-series data
    pub fn clear(&self) {
        self.snapshots.clear();
    }
}

impl Clone for TimeSeriesMetrics {
    fn clone(&self) -> Self {
        Self {
            snapshots: Arc::clone(&self.snapshots),
            retention: self.retention,
        }
    }
}

/// Performance SLO checker
pub struct SloChecker {
    /// Target hit rate percentage
    min_hit_rate: f64,
    /// Maximum acceptable hit latency in microseconds
    max_hit_latency_us: u64,
    /// Maximum acceptable miss latency in microseconds
    max_miss_latency_us: u64,
    /// Maximum eviction rate percentage
    max_eviction_rate: f64,
}

impl SloChecker {
    /// Create a new SLO checker
    pub fn new(
        min_hit_rate: f64,
        max_hit_latency_us: u64,
        max_miss_latency_us: u64,
        max_eviction_rate: f64,
    ) -> Self {
        Self {
            min_hit_rate,
            max_hit_latency_us,
            max_miss_latency_us,
            max_eviction_rate,
        }
    }

    /// Check if metrics meet SLO targets
    pub fn check(&self, metrics: &CacheMetrics) -> SloCheckResult {
        let mut violations = Vec::new();

        if metrics.hit_rate() < self.min_hit_rate {
            violations.push(format!(
                "Hit rate {:.2}% below target {:.2}%",
                metrics.hit_rate(),
                self.min_hit_rate
            ));
        }

        if metrics.avg_hit_latency_us > self.max_hit_latency_us {
            violations.push(format!(
                "Hit latency {}µs exceeds max {}µs",
                metrics.avg_hit_latency_us, self.max_hit_latency_us
            ));
        }

        if metrics.avg_miss_latency_us > self.max_miss_latency_us {
            violations.push(format!(
                "Miss latency {}µs exceeds max {}µs",
                metrics.avg_miss_latency_us, self.max_miss_latency_us
            ));
        }

        if metrics.eviction_rate() > self.max_eviction_rate {
            violations.push(format!(
                "Eviction rate {:.2}% exceeds max {:.2}%",
                metrics.eviction_rate(),
                self.max_eviction_rate
            ));
        }

        SloCheckResult {
            passed: violations.is_empty(),
            violations,
        }
    }
}

/// Result of SLO check
#[derive(Debug, Clone, Serialize)]
pub struct SloCheckResult {
    pub passed: bool,
    pub violations: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_hit_rate() {
        let metrics = CacheMetrics {
            hits: 80,
            misses: 20,
            evictions: 0,
            total_ops: 100,
            avg_hit_latency_us: 100,
            avg_miss_latency_us: 500,
            cache_size_bytes: 1024,
            item_count: 10,
            peak_cache_size_bytes: 2048,
        };

        assert_eq!(metrics.hit_rate(), 80.0);
        assert_eq!(metrics.miss_rate(), 20.0);
    }

    #[test]
    fn test_metrics_collector() {
        let collector = MetricsCollector::new();
        collector.record_hit(100);
        collector.record_miss(500);

        let snapshot = collector.snapshot();
        assert_eq!(snapshot.hits, 1);
        assert_eq!(snapshot.misses, 1);
        assert_eq!(snapshot.hit_rate(), 50.0);
    }

    #[test]
    fn test_slo_checker() {
        let slo = SloChecker::new(80.0, 200, 1000, 5.0);
        let metrics = CacheMetrics {
            hits: 80,
            misses: 20,
            evictions: 0,
            total_ops: 100,
            avg_hit_latency_us: 100,
            avg_miss_latency_us: 500,
            cache_size_bytes: 1024,
            item_count: 10,
            peak_cache_size_bytes: 2048,
        };

        let result = slo.check(&metrics);
        assert!(result.passed);
    }
}
