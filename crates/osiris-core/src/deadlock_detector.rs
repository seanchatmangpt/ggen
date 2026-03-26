//! Deadlock Detection System
//!
//! Monitors timeout events across all TimedLock instances and provides
//! alerts when sustained deadlocks are detected.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::RwLock;
use tracing::{error, info, warn};

/// Alert severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AlertSeverity {
    Info = 0,
    Warning = 1,
    Critical = 2,
}

impl std::fmt::Display for AlertSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlertSeverity::Info => write!(f, "INFO"),
            AlertSeverity::Warning => write!(f, "WARNING"),
            AlertSeverity::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// Deadlock alert details
#[derive(Debug, Clone)]
pub struct DeadlockAlert {
    /// Component that timed out
    pub component: String,
    /// Number of timeouts detected
    pub timeout_count: usize,
    /// Alert severity
    pub severity: AlertSeverity,
    /// Time of first timeout in this sequence
    pub first_occurrence: SystemTime,
    /// Time of most recent timeout
    pub last_occurrence: SystemTime,
    /// Time between first and last timeout
    pub duration: Duration,
    /// Human-readable message
    pub message: String,
}

impl std::fmt::Display for DeadlockAlert {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}] {} - {} timeouts in {:?} (component: {})",
            self.severity, self.message, self.timeout_count, self.duration, self.component
        )
    }
}

/// Per-component timeout statistics
#[derive(Debug, Clone)]
struct ComponentStats {
    timeout_count: usize,
    first_timeout: SystemTime,
    last_timeout: SystemTime,
}

impl ComponentStats {
    fn new() -> Self {
        Self {
            timeout_count: 0,
            first_timeout: SystemTime::now(),
            last_timeout: SystemTime::now(),
        }
    }

    fn record_timeout(&mut self) {
        self.timeout_count += 1;
        self.last_timeout = SystemTime::now();
    }

    fn duration(&self) -> Duration {
        self.last_timeout
            .duration_since(self.first_timeout)
            .unwrap_or_default()
    }

    fn is_sustained(&self, threshold: usize) -> bool {
        self.timeout_count >= threshold
    }
}

/// Deadlock detector monitors timeout patterns
pub struct DeadlockDetector {
    stats: Arc<RwLock<HashMap<String, ComponentStats>>>,
    alerts: Arc<RwLock<Vec<DeadlockAlert>>>,
    cleanup_interval: Duration,
    sustained_threshold: usize,
}

impl DeadlockDetector {
    /// Create a new deadlock detector
    pub fn new() -> Self {
        Self {
            stats: Arc::new(RwLock::new(HashMap::new())),
            alerts: Arc::new(RwLock::new(Vec::new())),
            cleanup_interval: Duration::from_secs(300), // 5 minutes
            sustained_threshold: 10,
        }
    }

    /// Create with custom sustained threshold
    pub fn with_threshold(sustained_threshold: usize) -> Self {
        Self {
            stats: Arc::new(RwLock::new(HashMap::new())),
            alerts: Arc::new(RwLock::new(Vec::new())),
            cleanup_interval: Duration::from_secs(300),
            sustained_threshold,
        }
    }

    /// Record a timeout event
    pub async fn record_timeout(&self, component: &str) -> Option<DeadlockAlert> {
        // Get or create stats for this component
        let (timeout_count, first_time, last_time, severity) = {
            let mut stats = self.stats.write().await;
            let entry = stats
                .entry(component.to_string())
                .or_insert_with(ComponentStats::new);

            entry.record_timeout();
            let first_time = entry.first_timeout;
            let last_time = entry.last_timeout;

            if entry.timeout_count >= self.sustained_threshold {
                let severity = match entry.timeout_count {
                    10..=20 => AlertSeverity::Warning,
                    21..=50 => AlertSeverity::Critical,
                    _ => AlertSeverity::Critical,
                };
                (entry.timeout_count, first_time, last_time, Some(severity))
            } else if entry.timeout_count == 1 {
                warn!(
                    component = component,
                    "First timeout detected - monitoring for sustained pattern"
                );
                (entry.timeout_count, first_time, last_time, None)
            } else {
                (entry.timeout_count, first_time, last_time, None)
            }
        };

        // If sustained, create and record alert
        if let Some(severity) = severity {
            let duration = last_time.duration_since(first_time).unwrap_or_default();
            let alert = DeadlockAlert {
                component: component.to_string(),
                timeout_count,
                severity,
                first_occurrence: first_time,
                last_occurrence: last_time,
                duration,
                message: format!(
                    "Potential deadlock in {} - {} timeouts detected",
                    component, timeout_count
                ),
            };

            let mut alerts = self.alerts.write().await;
            alerts.push(alert.clone());

            if severity == AlertSeverity::Critical {
                error!("{}", alert);
            } else if severity == AlertSeverity::Warning {
                warn!("{}", alert);
            } else {
                info!("{}", alert);
            }

            Some(alert)
        } else {
            None
        }
    }

    /// Get current alerts
    pub async fn get_alerts(&self) -> Vec<DeadlockAlert> {
        self.alerts.read().await.clone()
    }

    /// Get alerts of specific severity or higher
    pub async fn get_alerts_by_severity(&self, min_severity: AlertSeverity) -> Vec<DeadlockAlert> {
        self.alerts
            .read()
            .await
            .iter()
            .filter(|alert| alert.severity >= min_severity)
            .cloned()
            .collect()
    }

    /// Get stats for a specific component
    pub async fn get_component_stats(&self, component: &str) -> Option<(usize, Duration)> {
        self.stats
            .read()
            .await
            .get(component)
            .map(|s| (s.timeout_count, s.duration()))
    }

    /// Clear alerts and stats
    pub async fn reset(&self) {
        let mut stats = self.stats.write().await;
        let mut alerts = self.alerts.write().await;
        stats.clear();
        alerts.clear();
        info!("Deadlock detector reset");
    }

    /// Clear alerts only
    pub async fn clear_alerts(&self) {
        let mut alerts = self.alerts.write().await;
        alerts.clear();
    }

    /// Get all current component stats
    pub async fn get_all_stats(&self) -> HashMap<String, (usize, Duration)> {
        self.stats
            .read()
            .await
            .iter()
            .map(|(k, v)| (k.clone(), (v.timeout_count, v.duration())))
            .collect()
    }

    /// Check if any sustained deadlocks are detected
    pub async fn has_sustained_deadlock(&self) -> bool {
        let threshold = self.sustained_threshold;
        self.stats
            .read()
            .await
            .values()
            .any(|s| s.is_sustained(threshold))
    }

    /// Get number of sustained deadlocks
    pub async fn count_sustained_deadlocks(&self) -> usize {
        let threshold = self.sustained_threshold;
        self.stats
            .read()
            .await
            .values()
            .filter(|s| s.is_sustained(threshold))
            .count()
    }
}

impl Default for DeadlockDetector {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for DeadlockDetector {
    fn clone(&self) -> Self {
        Self {
            stats: Arc::clone(&self.stats),
            alerts: Arc::clone(&self.alerts),
            cleanup_interval: self.cleanup_interval,
            sustained_threshold: self.sustained_threshold,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_detector_creation() {
        let detector = DeadlockDetector::new();
        assert!(!detector.has_sustained_deadlock().await);
    }

    #[tokio::test]
    async fn test_record_timeout() {
        let detector = DeadlockDetector::with_threshold(3);
        detector.record_timeout("test_component").await;

        let stats = detector.get_component_stats("test_component").await;
        assert!(stats.is_some());
        assert_eq!(stats.unwrap().0, 1);
    }

    #[tokio::test]
    async fn test_sustained_deadlock() {
        let detector = DeadlockDetector::with_threshold(3);

        // Record timeouts
        for _ in 0..5 {
            detector.record_timeout("test_component").await;
        }

        assert!(detector.has_sustained_deadlock().await);
        assert_eq!(detector.count_sustained_deadlocks().await, 1);

        let alerts = detector.get_alerts().await;
        assert!(!alerts.is_empty());
        assert_eq!(alerts[0].component, "test_component");
    }

    #[tokio::test]
    async fn test_reset() {
        let detector = DeadlockDetector::new();
        detector.record_timeout("component1").await;
        detector.reset().await;

        assert!(detector.get_alerts().await.is_empty());
        assert!(detector.get_component_stats("component1").await.is_none());
    }
}
