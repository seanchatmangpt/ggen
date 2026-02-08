//! Registration Statistics
//!
//! Provides statistics tracking for tool registration operations.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Registration statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RegistrationStats {
    /// Total tools registered
    pub total_registered: u64,
    /// Total tools unregistered
    pub total_unregistered: u64,
    /// Total registration failures
    pub total_failures: u64,
    /// A2A agents registered to core registry
    pub agents_registered: u64,
    /// Last registration timestamp
    pub last_registration: Option<chrono::DateTime<chrono::Utc>>,
    /// First registration timestamp
    pub first_registration: Option<chrono::DateTime<chrono::Utc>>,
    /// Source-specific statistics
    pub source_stats: HashMap<String, SourceStatistics>,
}

impl RegistrationStats {
    /// Record a successful registration
    pub fn record_registration(&mut self, _source: &str) {
        self.total_registered += 1;
        let now = chrono::Utc::now();

        if self.first_registration.is_none() {
            self.first_registration = Some(now);
        }
        self.last_registration = Some(now);

        // Note: total_from_source is called from the registration manager
        // which has access to the async context
    }

    /// Record an unregistration
    pub fn record_unregistration(&mut self) {
        self.total_unregistered += 1;
    }

    /// Record a failure
    pub fn record_failure(&mut self) {
        self.total_failures += 1;
    }

    /// Get total active tools (registered - unregistered)
    pub fn active_tools(&self) -> u64 {
        self.total_registered.saturating_sub(self.total_unregistered)
    }

    /// Get success rate as percentage
    pub fn success_rate(&self) -> f64 {
        let total = self.total_registered + self.total_failures;
        if total == 0 {
            100.0
        } else {
            (self.total_registered as f64 / total as f64) * 100.0
        }
    }

    /// Get statistics for a specific source
    pub fn source_statistics(&self, source: &str) -> Option<&SourceStatistics> {
        self.source_stats.get(source)
    }

    /// Increment count for a source (called by parent)
    pub async fn total_from_source(&mut self, source: &str) {
        let entry = self
            .source_stats
            .entry(source.to_string())
            .or_insert_with(|| SourceStatistics::new(source.to_string()));
        entry.count += 1;
        entry.last_seen = Some(chrono::Utc::now());
    }
}

/// Statistics for a specific source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceStatistics {
    /// Source URL
    pub source: String,
    /// Number of tools from this source
    pub count: u64,
    /// First registration from this source
    pub first_seen: Option<chrono::DateTime<chrono::Utc>>,
    /// Last registration from this source
    pub last_seen: Option<chrono::DateTime<chrono::Utc>>,
    /// Registration failures for this source
    pub failures: u64,
}

impl SourceStatistics {
    /// Create new source statistics
    pub fn new(source: String) -> Self {
        let now = chrono::Utc::now();
        Self {
            source,
            count: 0,
            first_seen: Some(now),
            last_seen: Some(now),
            failures: 0,
        }
    }

    /// Record a failure for this source
    pub fn record_failure(&mut self) {
        self.failures += 1;
    }

    /// Get success rate for this source
    pub fn success_rate(&self) -> f64 {
        let total = self.count + self.failures;
        if total == 0 {
            100.0
        } else {
            (self.count as f64 / total as f64) * 100.0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registration_stats_default() {
        let stats = RegistrationStats::default();
        assert_eq!(stats.total_registered, 0);
        assert_eq!(stats.total_unregistered, 0);
        assert_eq!(stats.total_failures, 0);
    }

    #[test]
    fn test_record_registration() {
        let mut stats = RegistrationStats::default();
        stats.record_registration("http://localhost:3000");

        assert_eq!(stats.total_registered, 1);
        assert!(stats.first_registration.is_some());
        assert!(stats.last_registration.is_some());
    }

    #[test]
    fn test_record_unregistration() {
        let mut stats = RegistrationStats::default();
        stats.record_unregistration();

        assert_eq!(stats.total_unregistered, 1);
    }

    #[test]
    fn test_record_failure() {
        let mut stats = RegistrationStats::default();
        stats.record_failure();

        assert_eq!(stats.total_failures, 1);
    }

    #[test]
    fn test_active_tools() {
        let mut stats = RegistrationStats::default();
        stats.record_registration("source");
        stats.record_registration("source");
        stats.record_unregistration();

        assert_eq!(stats.active_tools(), 1);
    }

    #[test]
    fn test_success_rate() {
        let mut stats = RegistrationStats::default();
        stats.record_registration("source");
        stats.record_registration("source");
        stats.record_failure();

        assert!((stats.success_rate() - 66.66).abs() < 0.1);
    }

    #[test]
    fn test_success_rate_empty() {
        let stats = RegistrationStats::default();
        assert_eq!(stats.success_rate(), 100.0);
    }

    #[test]
    fn test_source_stats_new() {
        let source_stats = SourceStatistics::new("http://localhost:3000".to_string());
        assert_eq!(source_stats.count, 0);
        assert!(source_stats.first_seen.is_some());
        assert!(source_stats.last_seen.is_some());
    }

    #[test]
    fn test_source_stats_record_failure() {
        let mut source_stats = SourceStatistics::new("source".to_string());
        source_stats.record_failure();
        assert_eq!(source_stats.failures, 1);
    }

    #[test]
    fn test_source_stats_success_rate() {
        let mut source_stats = SourceStatistics::new("source".to_string());
        source_stats.count = 8;
        source_stats.failures = 2;

        assert_eq!(source_stats.success_rate(), 80.0);
    }

    #[test]
    fn test_source_stats_success_rate_empty() {
        let source_stats = SourceStatistics::new("source".to_string());
        assert_eq!(source_stats.success_rate(), 100.0);
    }

    #[tokio::test]
    async fn test_total_from_source() {
        let mut stats = RegistrationStats::default();
        stats.total_from_source("http://localhost:3000").await;
        stats.total_from_source("http://localhost:3000").await;

        let source_stat = stats.source_statistics("http://localhost:3000");
        assert!(source_stat.is_some());
        assert_eq!(source_stat.unwrap().count, 2);
    }
}
