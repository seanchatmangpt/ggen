//! Observability and metrics collection
//!
//! Features:
//! - Structured metrics collection
//! - Event tracking
//! - Performance monitoring
//! - Tracing integration

use async_trait::async_trait;
use dashmap::DashMap;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::Arc;
use tracing::debug;

use crate::error::Result;
use crate::traits::Observable;

/// Metrics collector for marketplace operations
pub struct MetricsCollector {
    // Counters
    searches: Arc<AtomicU64>,
    search_hits: Arc<AtomicU64>,
    installations: Arc<AtomicU64>,
    validations: Arc<AtomicU64>,
    signature_verifications: Arc<AtomicU64>,

    // Performance metrics
    avg_search_duration_ms: Arc<AtomicI64>,
    avg_install_duration_ms: Arc<AtomicI64>,

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
            events: Arc::new(DashMap::new()),
        }
    }

    /// Record a search operation
    pub fn record_search(&self, duration_ms: u64, results_found: u64) {
        self.searches.fetch_add(1, Ordering::Relaxed);
        if results_found > 0 {
            self.search_hits.fetch_add(1, Ordering::Relaxed);
        }

        let current = self.avg_search_duration_ms.load(Ordering::Relaxed) as u64;
        let count = self.searches.load(Ordering::Relaxed);
        let new_avg = ((current * (count - 1)) + duration_ms) / count;
        self.avg_search_duration_ms
            .store(new_avg as i64, Ordering::Relaxed);

        debug!(
            "Search recorded: duration={}ms, results={}, avg={}ms",
            duration_ms, results_found, new_avg
        );
    }

    /// Record an installation operation
    pub fn record_installation(&self, duration_ms: u64, packages_count: u64) {
        self.installations.fetch_add(1, Ordering::Relaxed);

        let current = self.avg_install_duration_ms.load(Ordering::Relaxed) as u64;
        let count = self.installations.load(Ordering::Relaxed);
        let new_avg = ((current * (count - 1)) + duration_ms) / count;
        self.avg_install_duration_ms
            .store(new_avg as i64, Ordering::Relaxed);

        debug!(
            "Installation recorded: duration={}ms, packages={}, avg={}ms",
            duration_ms, packages_count, new_avg
        );
    }

    /// Record a validation operation
    pub fn record_validation(&self) {
        self.validations.fetch_add(1, Ordering::Relaxed);
    }

    /// Record a signature verification
    pub fn record_signature_verification(&self, verified: bool) {
        self.signature_verifications.fetch_add(1, Ordering::Relaxed);
        if verified {
            debug!("Signature verification successful");
        }
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

    /// Get installation metrics
    pub fn installation_metrics(&self) -> InstallationMetrics {
        let total = self.installations.load(Ordering::Relaxed);
        let avg_duration = self.avg_install_duration_ms.load(Ordering::Relaxed);

        InstallationMetrics {
            total_installations: total,
            avg_duration_ms: avg_duration as u64,
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
