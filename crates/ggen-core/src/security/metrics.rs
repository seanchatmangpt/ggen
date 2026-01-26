//! Security metrics collection and aggregation
//!
//! This module provides comprehensive security metrics for monitoring and analysis.

use super::events::{AttackPattern, EventCategory, SecurityEvent, SecuritySeverity};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

/// Metrics collection errors
#[derive(Debug, Error)]
pub enum MetricsError {
    #[error("Metric not found: {0}")]
    MetricNotFound(String),

    #[error("Invalid time range")]
    InvalidTimeRange,

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

/// Time window for metric aggregation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeWindow {
    /// Last minute
    Minute,
    /// Last hour
    Hour,
    /// Last day
    Day,
    /// Last week
    Week,
    /// All time
    AllTime,
}

impl TimeWindow {
    /// Get the duration for this time window
    pub fn duration(&self) -> Option<Duration> {
        match self {
            Self::Minute => Some(Duration::minutes(1)),
            Self::Hour => Some(Duration::hours(1)),
            Self::Day => Some(Duration::days(1)),
            Self::Week => Some(Duration::weeks(1)),
            Self::AllTime => None,
        }
    }

    /// Check if a timestamp is within this window
    pub fn contains(&self, timestamp: DateTime<Utc>, now: DateTime<Utc>) -> bool {
        match self.duration() {
            Some(duration) => now - timestamp <= duration,
            None => true,
        }
    }
}

/// Security metrics summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityMetrics {
    /// Total events count
    pub total_events: usize,
    /// Events by severity
    pub by_severity: HashMap<String, usize>,
    /// Events by category
    pub by_category: HashMap<String, usize>,
    /// Attack events count
    pub total_attacks: usize,
    /// Attacks by pattern
    pub by_attack_pattern: HashMap<String, usize>,
    /// Failed authentication attempts
    pub failed_auth_count: usize,
    /// Failed authorization attempts
    pub failed_authz_count: usize,
    /// Unique source IPs
    pub unique_sources: usize,
    /// Time window
    pub time_window: TimeWindow,
    /// Calculation timestamp
    pub calculated_at: DateTime<Utc>,
}

impl SecurityMetrics {
    /// Create empty metrics
    pub fn new(time_window: TimeWindow) -> Self {
        Self {
            total_events: 0,
            by_severity: HashMap::new(),
            by_category: HashMap::new(),
            total_attacks: 0,
            by_attack_pattern: HashMap::new(),
            failed_auth_count: 0,
            failed_authz_count: 0,
            unique_sources: 0,
            time_window,
            calculated_at: Utc::now(),
        }
    }
}

/// Security metrics collector
pub struct MetricsCollector {
    /// All collected events
    events: Vec<SecurityEvent>,
    /// Maximum events to retain
    max_events: usize,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            max_events: 10_000,
        }
    }

    /// Create a metrics collector with custom max events
    pub fn with_max_events(max_events: usize) -> Self {
        Self {
            events: Vec::new(),
            max_events,
        }
    }

    /// Record a security event
    pub fn record(&mut self, event: SecurityEvent) {
        self.events.push(event);

        // Trim old events if we exceed max
        if self.events.len() > self.max_events {
            let remove_count = self.events.len() - self.max_events;
            self.events.drain(0..remove_count);
        }
    }

    /// Get metrics for a time window
    pub fn get_metrics(&self, window: TimeWindow) -> SecurityMetrics {
        let now = Utc::now();
        let events: Vec<_> = self
            .events
            .iter()
            .filter(|e| window.contains(e.timestamp, now))
            .collect();

        let mut metrics = SecurityMetrics::new(window);
        metrics.total_events = events.len();

        // Count by severity
        for event in &events {
            let severity = event.severity.to_string();
            *metrics.by_severity.entry(severity).or_insert(0) += 1;
        }

        // Count by category
        for event in &events {
            let category = event.category.to_string();
            *metrics.by_category.entry(category).or_insert(0) += 1;
        }

        // Count attacks
        metrics.total_attacks = events.iter().filter(|e| e.is_attack()).count();

        // Count by attack pattern
        for event in events.iter().filter(|e| e.is_attack()) {
            let pattern = event.attack_pattern.to_string();
            *metrics.by_attack_pattern.entry(pattern).or_insert(0) += 1;
        }

        // Count failed authentication
        metrics.failed_auth_count = events
            .iter()
            .filter(|e| matches!(e.category, EventCategory::Authentication) && !e.success)
            .count();

        // Count failed authorization
        metrics.failed_authz_count = events
            .iter()
            .filter(|e| matches!(e.category, EventCategory::Authorization) && !e.success)
            .count();

        // Count unique sources
        metrics.unique_sources = events
            .iter()
            .filter_map(|e| e.source_ip.as_ref())
            .collect::<std::collections::HashSet<_>>()
            .len();

        metrics
    }

    /// Get events for a time window
    pub fn get_events(&self, window: TimeWindow) -> Vec<&SecurityEvent> {
        let now = Utc::now();
        self.events
            .iter()
            .filter(|e| window.contains(e.timestamp, now))
            .collect()
    }

    /// Get events by severity
    pub fn get_events_by_severity(
        &self, severity: SecuritySeverity, window: TimeWindow,
    ) -> Vec<&SecurityEvent> {
        let now = Utc::now();
        self.events
            .iter()
            .filter(|e| e.severity == severity && window.contains(e.timestamp, now))
            .collect()
    }

    /// Get events by category
    pub fn get_events_by_category(
        &self, category: EventCategory, window: TimeWindow,
    ) -> Vec<&SecurityEvent> {
        let now = Utc::now();
        self.events
            .iter()
            .filter(|e| e.category == category && window.contains(e.timestamp, now))
            .collect()
    }

    /// Get attack events
    pub fn get_attack_events(&self, window: TimeWindow) -> Vec<&SecurityEvent> {
        let now = Utc::now();
        self.events
            .iter()
            .filter(|e| e.is_attack() && window.contains(e.timestamp, now))
            .collect()
    }

    /// Get top attack sources
    pub fn get_top_attack_sources(&self, window: TimeWindow, limit: usize) -> Vec<(String, usize)> {
        let now = Utc::now();
        let mut source_counts: HashMap<String, usize> = HashMap::new();

        for event in self.events.iter().filter(|e| e.is_attack()) {
            if !window.contains(event.timestamp, now) {
                continue;
            }

            if let Some(ip) = &event.source_ip {
                *source_counts.entry(ip.to_string()).or_insert(0) += 1;
            }
        }

        let mut sources: Vec<_> = source_counts.into_iter().collect();
        sources.sort_by(|a, b| b.1.cmp(&a.1));
        sources.truncate(limit);
        sources
    }

    /// Get total event count
    pub fn total_events(&self) -> usize {
        self.events.len()
    }

    /// Clear all events
    pub fn clear(&mut self) {
        self.events.clear();
    }

    /// Export metrics as JSON
    pub fn export_json(&self, window: TimeWindow) -> Result<String, MetricsError> {
        let metrics = self.get_metrics(window);
        Ok(serde_json::to_string_pretty(&metrics)?)
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_metrics_collector_creation() {
        // Arrange & Act
        let collector = MetricsCollector::new();

        // Assert
        assert_eq!(collector.total_events(), 0);
    }

    #[test]
    fn test_record_event() {
        // Arrange
        let mut collector = MetricsCollector::new();
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Test event",
        );

        // Act
        collector.record(event);

        // Assert
        assert_eq!(collector.total_events(), 1);
    }

    #[test]
    fn test_get_metrics_all_time() {
        // Arrange
        let mut collector = MetricsCollector::new();

        collector.record(SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Event 1",
        ));
        collector.record(SecurityEvent::new(
            SecuritySeverity::Medium,
            EventCategory::Authorization,
            "Event 2",
        ));
        collector.record(SecurityEvent::input_validation_failed(
            "SELECT * FROM users",
            AttackPattern::SqlInjection,
        ));

        // Act
        let metrics = collector.get_metrics(TimeWindow::AllTime);

        // Assert
        assert_eq!(metrics.total_events, 3);
        assert_eq!(metrics.total_attacks, 1);
        assert_eq!(*metrics.by_severity.get("HIGH").unwrap_or(&0), 2);
        assert_eq!(*metrics.by_severity.get("MEDIUM").unwrap_or(&0), 1);
    }

    #[test]
    fn test_get_events_by_severity() {
        // Arrange
        let mut collector = MetricsCollector::new();

        collector.record(SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::Integrity,
            "Critical event",
        ));
        collector.record(SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "High event",
        ));
        collector.record(SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::DataAccess,
            "Another critical",
        ));

        // Act
        let critical_events =
            collector.get_events_by_severity(SecuritySeverity::Critical, TimeWindow::AllTime);

        // Assert
        assert_eq!(critical_events.len(), 2);
    }

    #[test]
    fn test_get_events_by_category() {
        // Arrange
        let mut collector = MetricsCollector::new();

        collector.record(SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Auth event 1",
        ));
        collector.record(SecurityEvent::new(
            SecuritySeverity::Medium,
            EventCategory::Authorization,
            "Authz event",
        ));
        collector.record(SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Auth event 2",
        ));

        // Act
        let auth_events =
            collector.get_events_by_category(EventCategory::Authentication, TimeWindow::AllTime);

        // Assert
        assert_eq!(auth_events.len(), 2);
    }

    #[test]
    fn test_get_attack_events() {
        // Arrange
        let mut collector = MetricsCollector::new();

        collector.record(SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Normal event",
        ));
        collector.record(SecurityEvent::input_validation_failed(
            "' OR '1'='1",
            AttackPattern::SqlInjection,
        ));
        collector.record(SecurityEvent::input_validation_failed(
            "<script>alert(1)</script>",
            AttackPattern::Xss,
        ));

        // Act
        let attacks = collector.get_attack_events(TimeWindow::AllTime);

        // Assert
        assert_eq!(attacks.len(), 2);
    }

    #[test]
    fn test_failed_auth_count() {
        // Arrange
        let mut collector = MetricsCollector::new();
        let ip = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));

        collector.record(SecurityEvent::authentication_failed("user1", ip));
        collector.record(SecurityEvent::authentication_failed("user2", ip));
        collector.record(SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Success",
        ));

        // Act
        let metrics = collector.get_metrics(TimeWindow::AllTime);

        // Assert
        assert_eq!(metrics.failed_auth_count, 2);
    }

    #[test]
    fn test_failed_authz_count() {
        // Arrange
        let mut collector = MetricsCollector::new();

        collector.record(SecurityEvent::authorization_failed(
            "user1", "/admin", "read",
        ));
        collector.record(SecurityEvent::authorization_failed(
            "user2", "/admin", "write",
        ));

        // Act
        let metrics = collector.get_metrics(TimeWindow::AllTime);

        // Assert
        assert_eq!(metrics.failed_authz_count, 2);
    }

    #[test]
    fn test_unique_sources() {
        // Arrange
        let mut collector = MetricsCollector::new();
        let ip1 = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));
        let ip2 = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 2));

        collector.record(SecurityEvent::authentication_failed("user1", ip1));
        collector.record(SecurityEvent::authentication_failed("user2", ip1));
        collector.record(SecurityEvent::authentication_failed("user3", ip2));

        // Act
        let metrics = collector.get_metrics(TimeWindow::AllTime);

        // Assert
        assert_eq!(metrics.unique_sources, 2);
    }

    #[test]
    fn test_get_top_attack_sources() {
        // Arrange
        let mut collector = MetricsCollector::new();
        let ip1 = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));
        let ip2 = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 2));

        for _ in 0..5 {
            collector.record(
                SecurityEvent::input_validation_failed("' OR '1'='1", AttackPattern::SqlInjection)
                    .with_source_ip(ip1),
            );
        }

        for _ in 0..3 {
            collector.record(
                SecurityEvent::input_validation_failed("<script>", AttackPattern::Xss)
                    .with_source_ip(ip2),
            );
        }

        // Act
        let top_sources = collector.get_top_attack_sources(TimeWindow::AllTime, 2);

        // Assert
        assert_eq!(top_sources.len(), 2);
        assert_eq!(top_sources[0].0, ip1.to_string());
        assert_eq!(top_sources[0].1, 5);
        assert_eq!(top_sources[1].0, ip2.to_string());
        assert_eq!(top_sources[1].1, 3);
    }

    #[test]
    fn test_max_events_limit() {
        // Arrange
        let mut collector = MetricsCollector::with_max_events(100);

        // Act - add more than max
        for i in 0..150 {
            collector.record(SecurityEvent::new(
                SecuritySeverity::Info,
                EventCategory::DataAccess,
                format!("Event {}", i),
            ));
        }

        // Assert - should trim to max
        assert_eq!(collector.total_events(), 100);
    }

    #[test]
    fn test_clear() {
        // Arrange
        let mut collector = MetricsCollector::new();
        collector.record(SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Test",
        ));

        // Act
        collector.clear();

        // Assert
        assert_eq!(collector.total_events(), 0);
    }

    #[test]
    fn test_export_json() {
        // Arrange
        let mut collector = MetricsCollector::new();
        collector.record(SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Test event",
        ));

        // Act
        let result = collector.export_json(TimeWindow::AllTime);

        // Assert
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("total_events"));
        assert!(json.contains("by_severity"));
    }

    #[test]
    fn test_time_window_contains() {
        // Arrange
        let now = Utc::now();
        let recent = now - Duration::seconds(30);
        let old = now - Duration::hours(2);

        // Act & Assert
        assert!(TimeWindow::Minute.contains(recent, now));
        assert!(!TimeWindow::Minute.contains(old, now));
        assert!(TimeWindow::Hour.contains(recent, now));
        assert!(!TimeWindow::Hour.contains(old, now));
        assert!(TimeWindow::AllTime.contains(old, now));
    }
}
