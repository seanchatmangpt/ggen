//! Observability and metrics collection
//!
//! Features:
//! - Structured metrics collection with p50/p95/p99 latency tracking
//! - Event tracking with timestamps
//! - Performance monitoring with histograms
//! - Tracing integration with spans
//! - Prometheus export format
//! - Cache hit/miss tracking
//! - Error rate monitoring

use async_trait::async_trait;
use dashmap::DashMap;
use parking_lot::RwLock;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::Arc;
use tracing::{debug, info_span, instrument, Span};

use crate::error::{ErrorCategory, Result};
use crate::traits::Observable;

/// Maximum number of latency samples to retain for percentile calculation
const MAX_LATENCY_SAMPLES: usize = 10000;

/// Latency histogram for percentile calculation
#[derive(Debug)]
pub struct LatencyHistogram {
    /// Ordered samples (newest last)
    samples: RwLock<VecDeque<u64>>,
    /// Running sum for average
    sum: AtomicU64,
    /// Count of samples
    count: AtomicU64,
}

impl LatencyHistogram {
    /// Create new histogram
    pub fn new() -> Self {
        Self {
            samples: RwLock::new(VecDeque::with_capacity(MAX_LATENCY_SAMPLES)),
            sum: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }

    /// Record a latency sample in milliseconds
    pub fn record(&self, duration_ms: u64) {
        self.sum.fetch_add(duration_ms, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);

        let mut samples = self.samples.write();
        if samples.len() >= MAX_LATENCY_SAMPLES {
            samples.pop_front();
        }
        samples.push_back(duration_ms);
    }

    /// Get average latency
    pub fn avg(&self) -> u64 {
        let count = self.count.load(Ordering::Relaxed);
        if count == 0 {
            return 0;
        }
        self.sum.load(Ordering::Relaxed) / count
    }

    /// Get percentile (0-100)
    pub fn percentile(&self, p: u8) -> u64 {
        let samples = self.samples.read();
        if samples.is_empty() {
            return 0;
        }

        let mut sorted: Vec<u64> = samples.iter().copied().collect();
        sorted.sort_unstable();

        let index = ((p as usize) * sorted.len() / 100).saturating_sub(1).max(0);
        sorted.get(index).copied().unwrap_or(0)
    }

    /// Get p50 (median)
    pub fn p50(&self) -> u64 {
        self.percentile(50)
    }

    /// Get p95
    pub fn p95(&self) -> u64 {
        self.percentile(95)
    }

    /// Get p99
    pub fn p99(&self) -> u64 {
        self.percentile(99)
    }

    /// Get min latency
    pub fn min(&self) -> u64 {
        let samples = self.samples.read();
        samples.iter().copied().min().unwrap_or(0)
    }

    /// Get max latency
    pub fn max(&self) -> u64 {
        let samples = self.samples.read();
        samples.iter().copied().max().unwrap_or(0)
    }

    /// Get sample count
    pub fn count(&self) -> u64 {
        self.count.load(Ordering::Relaxed)
    }

    /// Export as Prometheus histogram buckets
    pub fn prometheus_buckets(&self) -> Vec<(u64, u64)> {
        let buckets = [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000];
        let samples = self.samples.read();

        buckets
            .iter()
            .map(|&bucket| {
                let count = samples.iter().filter(|&&s| s <= bucket).count() as u64;
                (bucket, count)
            })
            .collect()
    }
}

impl Default for LatencyHistogram {
    fn default() -> Self {
        Self::new()
    }
}

/// Error rate tracker
#[derive(Debug)]
pub struct ErrorRateTracker {
    /// Total operations
    total: AtomicU64,
    /// Errors by category
    errors_by_category: DashMap<String, AtomicU64>,
    /// Recent errors (for rate calculation)
    recent_errors: RwLock<VecDeque<(chrono::DateTime<chrono::Utc>, String)>>,
}

impl ErrorRateTracker {
    /// Create new tracker
    pub fn new() -> Self {
        Self {
            total: AtomicU64::new(0),
            errors_by_category: DashMap::new(),
            recent_errors: RwLock::new(VecDeque::with_capacity(1000)),
        }
    }

    /// Record an operation (success or failure)
    pub fn record_operation(&self) {
        self.total.fetch_add(1, Ordering::Relaxed);
    }

    /// Record an error
    pub fn record_error(&self, category: ErrorCategory) {
        let cat_str = category.to_string();

        self.errors_by_category
            .entry(cat_str.clone())
            .or_insert_with(|| AtomicU64::new(0))
            .fetch_add(1, Ordering::Relaxed);

        let mut recent = self.recent_errors.write();
        if recent.len() >= 1000 {
            recent.pop_front();
        }
        recent.push_back((chrono::Utc::now(), cat_str));
    }

    /// Get total error count
    pub fn total_errors(&self) -> u64 {
        self.errors_by_category
            .iter()
            .map(|entry| entry.value().load(Ordering::Relaxed))
            .sum()
    }

    /// Get error rate (errors / total)
    pub fn error_rate(&self) -> f64 {
        let total = self.total.load(Ordering::Relaxed);
        if total == 0 {
            return 0.0;
        }
        self.total_errors() as f64 / total as f64
    }

    /// Get errors by category
    pub fn errors_by_category(&self) -> Vec<(String, u64)> {
        self.errors_by_category
            .iter()
            .map(|entry| (entry.key().clone(), entry.value().load(Ordering::Relaxed)))
            .collect()
    }

    /// Get error rate for the last N minutes
    pub fn recent_error_rate(&self, minutes: i64) -> f64 {
        let cutoff = chrono::Utc::now() - chrono::Duration::minutes(minutes);
        let recent = self.recent_errors.read();
        let recent_count = recent.iter().filter(|(ts, _)| *ts > cutoff).count();

        // Approximate total operations in this window
        let total = self.total.load(Ordering::Relaxed);
        if total == 0 {
            return 0.0;
        }
        recent_count as f64 / (total as f64 / 60.0 * minutes as f64).max(1.0)
    }
}

impl Default for ErrorRateTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Latency metrics summary with percentile tracking
#[derive(Clone, Debug)]
pub struct LatencyMetrics {
    /// Average latency in milliseconds
    pub avg_ms: u64,
    /// 50th percentile latency
    pub p50_ms: u64,
    /// 95th percentile latency
    pub p95_ms: u64,
    /// 99th percentile latency
    pub p99_ms: u64,
    /// Minimum latency observed
    pub min_ms: u64,
    /// Maximum latency observed
    pub max_ms: u64,
    /// Total number of samples
    pub count: u64,
}

/// Error metrics summary
#[derive(Clone, Debug)]
pub struct ErrorMetrics {
    /// Total errors recorded
    pub total_errors: u64,
    /// Overall error rate
    pub error_rate: f64,
    /// Errors by category
    pub errors_by_category: Vec<(String, u64)>,
    /// Recent error rate (5 minute window)
    pub recent_error_rate_5m: f64,
}

/// Cache hit metrics summary
#[derive(Clone, Debug)]
pub struct CacheHitMetrics {
    /// Total cache hits
    pub hits: u64,
    /// Total cache misses
    pub misses: u64,
    /// Total evictions
    pub evictions: u64,
    /// Current cache size
    pub size: u64,
    /// Hit rate (0.0-1.0)
    pub hit_rate: f64,
}

/// Cache metrics tracker
#[derive(Debug)]
pub struct CacheMetrics {
    /// Cache hits
    hits: AtomicU64,
    /// Cache misses
    misses: AtomicU64,
    /// Evictions
    evictions: AtomicU64,
    /// Current size
    size: AtomicU64,
}

impl CacheMetrics {
    /// Create new cache metrics
    pub fn new() -> Self {
        Self {
            hits: AtomicU64::new(0),
            misses: AtomicU64::new(0),
            evictions: AtomicU64::new(0),
            size: AtomicU64::new(0),
        }
    }

    /// Record cache hit
    pub fn record_hit(&self) {
        self.hits.fetch_add(1, Ordering::Relaxed);
    }

    /// Record cache miss
    pub fn record_miss(&self) {
        self.misses.fetch_add(1, Ordering::Relaxed);
    }

    /// Record eviction
    pub fn record_eviction(&self) {
        self.evictions.fetch_add(1, Ordering::Relaxed);
    }

    /// Update size
    pub fn set_size(&self, size: u64) {
        self.size.store(size, Ordering::Relaxed);
    }

    /// Get hit rate
    pub fn hit_rate(&self) -> f64 {
        let hits = self.hits.load(Ordering::Relaxed);
        let misses = self.misses.load(Ordering::Relaxed);
        let total = hits + misses;
        if total == 0 {
            return 0.0;
        }
        hits as f64 / total as f64
    }

    /// Get miss rate
    pub fn miss_rate(&self) -> f64 {
        1.0 - self.hit_rate()
    }

    /// Get totals
    pub fn totals(&self) -> (u64, u64, u64, u64) {
        (
            self.hits.load(Ordering::Relaxed),
            self.misses.load(Ordering::Relaxed),
            self.evictions.load(Ordering::Relaxed),
            self.size.load(Ordering::Relaxed),
        )
    }
}

impl Default for CacheMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Metrics collector for marketplace operations
pub struct MetricsCollector {
    // Counters
    searches: Arc<AtomicU64>,
    search_hits: Arc<AtomicU64>,
    installations: Arc<AtomicU64>,
    validations: Arc<AtomicU64>,
    signature_verifications: Arc<AtomicU64>,

    // Performance metrics (legacy - for backward compatibility)
    avg_search_duration_ms: Arc<AtomicI64>,
    avg_install_duration_ms: Arc<AtomicI64>,

    // Latency histograms (new - for percentiles)
    search_latency: Arc<LatencyHistogram>,
    install_latency: Arc<LatencyHistogram>,
    validation_latency: Arc<LatencyHistogram>,
    rdf_query_latency: Arc<LatencyHistogram>,

    // Error tracking
    error_tracker: Arc<ErrorRateTracker>,

    // Cache metrics
    cache_metrics: Arc<CacheMetrics>,

    // Events
    events: Arc<DashMap<String, EventMetric>>,
}

/// Event metric information
#[derive(Clone, Debug)]
pub struct EventMetric {
    /// Event name
    pub name: String,
    /// Number of occurrences
    pub count: u64,
    /// When first occurred
    pub first_at: chrono::DateTime<chrono::Utc>,
    /// When last occurred
    pub last_at: chrono::DateTime<chrono::Utc>,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            searches: Arc::new(AtomicU64::new(0)),
            search_hits: Arc::new(AtomicU64::new(0)),
            installations: Arc::new(AtomicU64::new(0)),
            validations: Arc::new(AtomicU64::new(0)),
            signature_verifications: Arc::new(AtomicU64::new(0)),
            avg_search_duration_ms: Arc::new(AtomicI64::new(0)),
            avg_install_duration_ms: Arc::new(AtomicI64::new(0)),
            search_latency: Arc::new(LatencyHistogram::new()),
            install_latency: Arc::new(LatencyHistogram::new()),
            validation_latency: Arc::new(LatencyHistogram::new()),
            rdf_query_latency: Arc::new(LatencyHistogram::new()),
            error_tracker: Arc::new(ErrorRateTracker::new()),
            cache_metrics: Arc::new(CacheMetrics::new()),
            events: Arc::new(DashMap::new()),
        }
    }

    /// Record a search operation
    #[instrument(skip(self), fields(duration_ms, results_found))]
    pub fn record_search(&self, duration_ms: u64, results_found: u64) {
        self.searches.fetch_add(1, Ordering::Relaxed);
        if results_found > 0 {
            self.search_hits.fetch_add(1, Ordering::Relaxed);
        }

        // Record in histogram for percentiles
        self.search_latency.record(duration_ms);

        // Update legacy average
        let current = self.avg_search_duration_ms.load(Ordering::Relaxed) as u64;
        let count = self.searches.load(Ordering::Relaxed);
        let new_avg = ((current * (count - 1)) + duration_ms) / count;
        self.avg_search_duration_ms
            .store(new_avg as i64, Ordering::Relaxed);

        // Record operation for error tracking
        self.error_tracker.record_operation();

        debug!(
            "Search recorded: duration={}ms, results={}, avg={}ms, p50={}ms, p95={}ms, p99={}ms",
            duration_ms, results_found, new_avg,
            self.search_latency.p50(),
            self.search_latency.p95(),
            self.search_latency.p99()
        );
    }

    /// Record an installation operation
    #[instrument(skip(self), fields(duration_ms, packages_count))]
    pub fn record_installation(&self, duration_ms: u64, packages_count: u64) {
        self.installations.fetch_add(1, Ordering::Relaxed);

        // Record in histogram for percentiles
        self.install_latency.record(duration_ms);

        // Update legacy average
        let current = self.avg_install_duration_ms.load(Ordering::Relaxed) as u64;
        let count = self.installations.load(Ordering::Relaxed);
        let new_avg = ((current * (count - 1)) + duration_ms) / count;
        self.avg_install_duration_ms
            .store(new_avg as i64, Ordering::Relaxed);

        // Record operation for error tracking
        self.error_tracker.record_operation();

        debug!(
            "Installation recorded: duration={}ms, packages={}, avg={}ms, p50={}ms, p95={}ms, p99={}ms",
            duration_ms, packages_count, new_avg,
            self.install_latency.p50(),
            self.install_latency.p95(),
            self.install_latency.p99()
        );
    }

    /// Record a validation operation
    #[instrument(skip(self))]
    pub fn record_validation(&self) {
        self.validations.fetch_add(1, Ordering::Relaxed);
        self.error_tracker.record_operation();
    }

    /// Record a validation operation with duration
    #[instrument(skip(self), fields(duration_ms))]
    pub fn record_validation_with_duration(&self, duration_ms: u64) {
        self.validations.fetch_add(1, Ordering::Relaxed);
        self.validation_latency.record(duration_ms);
        self.error_tracker.record_operation();
    }

    /// Record an RDF query
    #[instrument(skip(self), fields(duration_ms))]
    pub fn record_rdf_query(&self, duration_ms: u64) {
        self.rdf_query_latency.record(duration_ms);
        self.error_tracker.record_operation();
    }

    /// Record a signature verification
    #[instrument(skip(self), fields(verified))]
    pub fn record_signature_verification(&self, verified: bool) {
        self.signature_verifications.fetch_add(1, Ordering::Relaxed);
        self.error_tracker.record_operation();
        if verified {
            debug!("Signature verification successful");
        }
    }

    /// Record an error
    pub fn record_error(&self, category: ErrorCategory) {
        self.error_tracker.record_error(category);
    }

    /// Record cache hit
    pub fn record_cache_hit(&self) {
        self.cache_metrics.record_hit();
    }

    /// Record cache miss
    pub fn record_cache_miss(&self) {
        self.cache_metrics.record_miss();
    }

    /// Record a custom event
    pub async fn record_custom_event(&self, name: impl Into<String>) {
        let name = name.into();
        let now = chrono::Utc::now();

        self.events
            .entry(name.clone())
            .and_modify(|e| {
                e.count += 1;
                e.last_at = now;
            })
            .or_insert_with(|| EventMetric {
                name: name.clone(),
                count: 1,
                first_at: now,
                last_at: now,
            });
    }

    /// Get search metrics
    pub fn search_metrics(&self) -> SearchMetrics {
        let total = self.searches.load(Ordering::Relaxed);
        let hits = self.search_hits.load(Ordering::Relaxed);
        let avg_duration = self.avg_search_duration_ms.load(Ordering::Relaxed);

        SearchMetrics {
            total_searches: total,
            successful_searches: hits,
            success_rate: if total > 0 {
                hits as f64 / total as f64
            } else {
                0.0
            },
            avg_duration_ms: avg_duration as u64,
        }
    }

    /// Get detailed search latency metrics
    pub fn search_latency_metrics(&self) -> LatencyMetrics {
        LatencyMetrics {
            avg_ms: self.search_latency.avg(),
            p50_ms: self.search_latency.p50(),
            p95_ms: self.search_latency.p95(),
            p99_ms: self.search_latency.p99(),
            min_ms: self.search_latency.min(),
            max_ms: self.search_latency.max(),
            count: self.search_latency.count(),
        }
    }

    /// Get detailed installation latency metrics
    pub fn install_latency_metrics(&self) -> LatencyMetrics {
        LatencyMetrics {
            avg_ms: self.install_latency.avg(),
            p50_ms: self.install_latency.p50(),
            p95_ms: self.install_latency.p95(),
            p99_ms: self.install_latency.p99(),
            min_ms: self.install_latency.min(),
            max_ms: self.install_latency.max(),
            count: self.install_latency.count(),
        }
    }

    /// Get RDF query latency metrics
    pub fn rdf_query_latency_metrics(&self) -> LatencyMetrics {
        LatencyMetrics {
            avg_ms: self.rdf_query_latency.avg(),
            p50_ms: self.rdf_query_latency.p50(),
            p95_ms: self.rdf_query_latency.p95(),
            p99_ms: self.rdf_query_latency.p99(),
            min_ms: self.rdf_query_latency.min(),
            max_ms: self.rdf_query_latency.max(),
            count: self.rdf_query_latency.count(),
        }
    }

    /// Get installation metrics
    pub fn installation_metrics(&self) -> InstallationMetrics {
        let total = self.installations.load(Ordering::Relaxed);
        let avg_duration = self.avg_install_duration_ms.load(Ordering::Relaxed);

        InstallationMetrics {
            total_installations: total,
            avg_duration_ms: avg_duration as u64,
        }
    }

    /// Get error metrics
    pub fn error_metrics(&self) -> ErrorMetrics {
        ErrorMetrics {
            total_errors: self.error_tracker.total_errors(),
            error_rate: self.error_tracker.error_rate(),
            errors_by_category: self.error_tracker.errors_by_category(),
            recent_error_rate_5m: self.error_tracker.recent_error_rate(5),
        }
    }

    /// Get cache metrics
    pub fn cache_hit_metrics(&self) -> CacheHitMetrics {
        let (hits, misses, evictions, size) = self.cache_metrics.totals();
        CacheHitMetrics {
            hits,
            misses,
            evictions,
            size,
            hit_rate: self.cache_metrics.hit_rate(),
        }
    }

    /// Get all metrics summary
    pub fn summary(&self) -> MetricsSummary {
        MetricsSummary {
            searches: self.search_metrics(),
            installations: self.installation_metrics(),
            validations: self.validations.load(Ordering::Relaxed),
            signature_verifications: self.signature_verifications.load(Ordering::Relaxed),
            events_count: self.events.len() as u64,
        }
    }

    /// Export metrics in Prometheus format
    pub fn prometheus_export(&self) -> String {
        let mut output = String::new();

        // Search metrics
        output.push_str("# HELP ggen_marketplace_searches_total Total number of searches\n");
        output.push_str("# TYPE ggen_marketplace_searches_total counter\n");
        output.push_str(&format!(
            "ggen_marketplace_searches_total {}\n",
            self.searches.load(Ordering::Relaxed)
        ));

        output.push_str("# HELP ggen_marketplace_search_hits_total Successful searches\n");
        output.push_str("# TYPE ggen_marketplace_search_hits_total counter\n");
        output.push_str(&format!(
            "ggen_marketplace_search_hits_total {}\n",
            self.search_hits.load(Ordering::Relaxed)
        ));

        // Search latency histogram
        output.push_str("# HELP ggen_marketplace_search_duration_ms Search latency in milliseconds\n");
        output.push_str("# TYPE ggen_marketplace_search_duration_ms histogram\n");
        for (bucket, count) in self.search_latency.prometheus_buckets() {
            output.push_str(&format!(
                "ggen_marketplace_search_duration_ms_bucket{{le=\"{}\"}} {}\n",
                bucket, count
            ));
        }
        output.push_str(&format!(
            "ggen_marketplace_search_duration_ms_bucket{{le=\"+Inf\"}} {}\n",
            self.search_latency.count()
        ));
        output.push_str(&format!(
            "ggen_marketplace_search_duration_ms_sum {}\n",
            self.search_latency.avg() * self.search_latency.count()
        ));
        output.push_str(&format!(
            "ggen_marketplace_search_duration_ms_count {}\n",
            self.search_latency.count()
        ));

        // Installation metrics
        output.push_str("# HELP ggen_marketplace_installations_total Total installations\n");
        output.push_str("# TYPE ggen_marketplace_installations_total counter\n");
        output.push_str(&format!(
            "ggen_marketplace_installations_total {}\n",
            self.installations.load(Ordering::Relaxed)
        ));

        // Install latency histogram
        output.push_str("# HELP ggen_marketplace_install_duration_ms Install latency in milliseconds\n");
        output.push_str("# TYPE ggen_marketplace_install_duration_ms histogram\n");
        for (bucket, count) in self.install_latency.prometheus_buckets() {
            output.push_str(&format!(
                "ggen_marketplace_install_duration_ms_bucket{{le=\"{}\"}} {}\n",
                bucket, count
            ));
        }
        output.push_str(&format!(
            "ggen_marketplace_install_duration_ms_bucket{{le=\"+Inf\"}} {}\n",
            self.install_latency.count()
        ));
        output.push_str(&format!(
            "ggen_marketplace_install_duration_ms_sum {}\n",
            self.install_latency.avg() * self.install_latency.count()
        ));
        output.push_str(&format!(
            "ggen_marketplace_install_duration_ms_count {}\n",
            self.install_latency.count()
        ));

        // Validation metrics
        output.push_str("# HELP ggen_marketplace_validations_total Total validations\n");
        output.push_str("# TYPE ggen_marketplace_validations_total counter\n");
        output.push_str(&format!(
            "ggen_marketplace_validations_total {}\n",
            self.validations.load(Ordering::Relaxed)
        ));

        // Signature verifications
        output.push_str("# HELP ggen_marketplace_signature_verifications_total Total signature verifications\n");
        output.push_str("# TYPE ggen_marketplace_signature_verifications_total counter\n");
        output.push_str(&format!(
            "ggen_marketplace_signature_verifications_total {}\n",
            self.signature_verifications.load(Ordering::Relaxed)
        ));

        // Error rate
        output.push_str("# HELP ggen_marketplace_error_rate Error rate (0-1)\n");
        output.push_str("# TYPE ggen_marketplace_error_rate gauge\n");
        output.push_str(&format!(
            "ggen_marketplace_error_rate {}\n",
            self.error_tracker.error_rate()
        ));

        // Errors by category
        output.push_str("# HELP ggen_marketplace_errors_total Errors by category\n");
        output.push_str("# TYPE ggen_marketplace_errors_total counter\n");
        for (category, count) in self.error_tracker.errors_by_category() {
            output.push_str(&format!(
                "ggen_marketplace_errors_total{{category=\"{}\"}} {}\n",
                category, count
            ));
        }

        // Cache metrics
        let (hits, misses, evictions, size) = self.cache_metrics.totals();
        output.push_str("# HELP ggen_marketplace_cache_hits_total Cache hits\n");
        output.push_str("# TYPE ggen_marketplace_cache_hits_total counter\n");
        output.push_str(&format!("ggen_marketplace_cache_hits_total {}\n", hits));

        output.push_str("# HELP ggen_marketplace_cache_misses_total Cache misses\n");
        output.push_str("# TYPE ggen_marketplace_cache_misses_total counter\n");
        output.push_str(&format!("ggen_marketplace_cache_misses_total {}\n", misses));

        output.push_str("# HELP ggen_marketplace_cache_evictions_total Cache evictions\n");
        output.push_str("# TYPE ggen_marketplace_cache_evictions_total counter\n");
        output.push_str(&format!("ggen_marketplace_cache_evictions_total {}\n", evictions));

        output.push_str("# HELP ggen_marketplace_cache_size Current cache size\n");
        output.push_str("# TYPE ggen_marketplace_cache_size gauge\n");
        output.push_str(&format!("ggen_marketplace_cache_size {}\n", size));

        output.push_str("# HELP ggen_marketplace_cache_hit_rate Cache hit rate (0-1)\n");
        output.push_str("# TYPE ggen_marketplace_cache_hit_rate gauge\n");
        output.push_str(&format!(
            "ggen_marketplace_cache_hit_rate {}\n",
            self.cache_metrics.hit_rate()
        ));

        output
    }

    /// Export metrics as JSON
    pub fn json_export(&self) -> serde_json::Value {
        serde_json::json!({
            "searches": {
                "total": self.searches.load(Ordering::Relaxed),
                "hits": self.search_hits.load(Ordering::Relaxed),
                "success_rate": self.search_metrics().success_rate,
                "latency": {
                    "avg_ms": self.search_latency.avg(),
                    "p50_ms": self.search_latency.p50(),
                    "p95_ms": self.search_latency.p95(),
                    "p99_ms": self.search_latency.p99(),
                    "min_ms": self.search_latency.min(),
                    "max_ms": self.search_latency.max()
                }
            },
            "installations": {
                "total": self.installations.load(Ordering::Relaxed),
                "latency": {
                    "avg_ms": self.install_latency.avg(),
                    "p50_ms": self.install_latency.p50(),
                    "p95_ms": self.install_latency.p95(),
                    "p99_ms": self.install_latency.p99(),
                    "min_ms": self.install_latency.min(),
                    "max_ms": self.install_latency.max()
                }
            },
            "validations": self.validations.load(Ordering::Relaxed),
            "signature_verifications": self.signature_verifications.load(Ordering::Relaxed),
            "errors": {
                "total": self.error_tracker.total_errors(),
                "rate": self.error_tracker.error_rate(),
                "by_category": self.error_tracker.errors_by_category()
            },
            "cache": {
                "hits": self.cache_metrics.totals().0,
                "misses": self.cache_metrics.totals().1,
                "hit_rate": self.cache_metrics.hit_rate()
            },
            "events_count": self.events.len()
        })
    }

    /// Create a tracing span for an operation
    pub fn create_span(&self, operation: &str) -> Span {
        info_span!(
            "marketplace_operation",
            operation = operation,
            search_count = self.searches.load(Ordering::Relaxed),
            install_count = self.installations.load(Ordering::Relaxed)
        )
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Observable for MetricsCollector {
    async fn record_metric(&self, name: &str, value: f64) -> Result<()> {
        debug!("Recorded metric: {} = {}", name, value);
        self.record_custom_event(format!("metric_{}", name)).await;
        Ok(())
    }

    async fn record_event(&self, name: &str, _data: &str) -> Result<()> {
        debug!("Recorded event: {}", name);
        self.record_custom_event(name).await;
        Ok(())
    }

    async fn get_metrics(&self) -> Result<String> {
        let summary = self.summary();
        Ok(summary.to_string())
    }
}

/// Search operation metrics
#[derive(Clone, Debug)]
pub struct SearchMetrics {
    /// Total searches performed
    pub total_searches: u64,
    /// Successful searches (found results)
    pub successful_searches: u64,
    /// Success rate (0.0-1.0)
    pub success_rate: f64,
    /// Average search duration in ms
    pub avg_duration_ms: u64,
}

impl std::fmt::Display for SearchMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Search: {} total, {} successful ({:.1}%), avg {}ms",
            self.total_searches,
            self.successful_searches,
            self.success_rate * 100.0,
            self.avg_duration_ms
        )
    }
}

/// Installation operation metrics
#[derive(Clone, Debug)]
pub struct InstallationMetrics {
    /// Total installations
    pub total_installations: u64,
    /// Average installation duration in ms
    pub avg_duration_ms: u64,
}

impl std::fmt::Display for InstallationMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Installations: {} total, avg {}ms",
            self.total_installations, self.avg_duration_ms
        )
    }
}

/// Complete metrics summary
#[derive(Clone, Debug)]
pub struct MetricsSummary {
    /// Search metrics
    pub searches: SearchMetrics,
    /// Installation metrics
    pub installations: InstallationMetrics,
    /// Validation count
    pub validations: u64,
    /// Signature verification count
    pub signature_verifications: u64,
    /// Number of distinct events
    pub events_count: u64,
}

impl std::fmt::Display for MetricsSummary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Marketplace Metrics Summary ===")?;
        writeln!(f, "{}", self.searches)?;
        writeln!(f, "{}", self.installations)?;
        writeln!(f, "Validations: {}", self.validations)?;
        writeln!(
            f,
            "Signature verifications: {}",
            self.signature_verifications
        )?;
        writeln!(f, "Events: {}", self.events_count)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_metrics_recording() {
        let metrics = MetricsCollector::new();

        metrics.record_search(100, 5);
        metrics.record_search(150, 3);

        let search_metrics = metrics.search_metrics();
        assert_eq!(search_metrics.total_searches, 2);
        assert_eq!(search_metrics.successful_searches, 2);
    }

    #[tokio::test]
    async fn test_event_recording() {
        let metrics = MetricsCollector::new();

        metrics.record_custom_event("test_event").await;
        metrics.record_custom_event("test_event").await;

        assert_eq!(metrics.events.len(), 1);
    }

    #[tokio::test]
    async fn test_metrics_summary() {
        let metrics = MetricsCollector::new();

        metrics.record_search(100, 5);
        metrics.record_installation(200, 3);
        metrics.record_validation();

        let summary = metrics.summary();
        assert_eq!(summary.searches.total_searches, 1);
        assert_eq!(summary.installations.total_installations, 1);
        assert_eq!(summary.validations, 1);
    }
}
