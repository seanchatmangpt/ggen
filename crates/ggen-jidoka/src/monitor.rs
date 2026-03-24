//! Real-time signal monitoring and alerting
//!
//! Monitors gate execution and tracks signal history.

use crate::{AndonSignal, Gate, Result};
use chrono::{DateTime, Utc};
use dashmap::DashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::{debug, info};

/// Signal event record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignalEvent {
    /// Timestamp of the event
    pub timestamp: DateTime<Utc>,
    /// Gate that emitted the signal
    pub gate_name: String,
    /// Signal level
    pub signal: AndonSignal,
    /// Optional details
    pub details: Option<String>,
}

impl SignalEvent {
    /// Create a new signal event
    #[must_use]
    pub fn new(gate_name: impl Into<String>, signal: AndonSignal) -> Self {
        Self {
            timestamp: Utc::now(),
            gate_name: gate_name.into(),
            signal,
            details: None,
        }
    }

    /// Add details to the event
    #[must_use]
    pub fn with_details(mut self, details: impl Into<String>) -> Self {
        self.details = Some(details.into());
        self
    }
}

/// Signal statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SignalStats {
    /// Total number of checks
    pub total_checks: u64,
    /// Number of green signals
    pub green_count: u64,
    /// Number of yellow signals
    pub yellow_count: u64,
    /// Number of red signals
    pub red_count: u64,
}

impl SignalStats {
    /// Calculate success rate (green / total)
    #[must_use]
    pub fn success_rate(&self) -> f64 {
        if self.total_checks == 0 {
            0.0
        } else {
            f64::from(self.green_count as u32) / f64::from(self.total_checks as u32)
        }
    }

    /// Calculate warning rate (yellow / total)
    #[must_use]
    pub fn warning_rate(&self) -> f64 {
        if self.total_checks == 0 {
            0.0
        } else {
            f64::from(self.yellow_count as u32) / f64::from(self.total_checks as u32)
        }
    }

    /// Calculate failure rate (red / total)
    #[must_use]
    pub fn failure_rate(&self) -> f64 {
        if self.total_checks == 0 {
            0.0
        } else {
            f64::from(self.red_count as u32) / f64::from(self.total_checks as u32)
        }
    }
}

/// Real-time signal monitor
pub struct SignalMonitor {
    /// Event history (bounded)
    events: Arc<RwLock<Vec<SignalEvent>>>,
    /// Statistics per gate
    stats: Arc<DashMap<String, SignalStats>>,
    /// Maximum events to retain
    max_events: usize,
}

impl SignalMonitor {
    /// Create a new signal monitor
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacity(1000)
    }

    /// Create a monitor with specific event capacity
    #[must_use]
    pub fn with_capacity(max_events: usize) -> Self {
        Self {
            events: Arc::new(RwLock::new(Vec::with_capacity(max_events))),
            stats: Arc::new(DashMap::new()),
            max_events,
        }
    }

    /// Record a signal event
    pub fn record(&self, event: SignalEvent) {
        debug!("Recording signal event: {} - {}", event.gate_name, event.signal);

        // Update statistics
        let mut stats = self
            .stats
            .entry(event.gate_name.clone())
            .or_default();
        stats.total_checks += 1;
        match event.signal {
            AndonSignal::Green => stats.green_count += 1,
            AndonSignal::Yellow => stats.yellow_count += 1,
            AndonSignal::Red => stats.red_count += 1,
        }

        // Add to event history
        let mut events = self.events.write();
        events.push(event);

        // Trim if exceeds capacity
        if events.len() > self.max_events {
            let excess = events.len() - self.max_events;
            events.drain(0..excess);
        }
    }

    /// Get all events
    #[must_use]
    pub fn get_events(&self) -> Vec<SignalEvent> {
        self.events.read().clone()
    }

    /// Get events for a specific gate
    #[must_use]
    pub fn get_gate_events(&self, gate_name: &str) -> Vec<SignalEvent> {
        self.events
            .read()
            .iter()
            .filter(|e| e.gate_name == gate_name)
            .cloned()
            .collect()
    }

    /// Get statistics for a gate
    #[must_use]
    pub fn get_stats(&self, gate_name: &str) -> Option<SignalStats> {
        self.stats.get(gate_name).map(|s| s.clone())
    }

    /// Get all statistics
    #[must_use]
    pub fn get_all_stats(&self) -> Vec<(String, SignalStats)> {
        self.stats
            .iter()
            .map(|entry| (entry.key().clone(), entry.value().clone()))
            .collect()
    }

    /// Monitor a gate and record results
    pub async fn monitor_gate(&self, gate: &dyn Gate) -> Result<AndonSignal> {
        let gate_name = gate.name();
        info!("🔍 Monitoring gate: {}", gate_name);

        let signal = gate.execute().await?;
        let event = SignalEvent::new(gate_name, signal);
        self.record(event);

        Ok(signal)
    }

    /// Clear all events and statistics
    pub fn clear(&self) {
        self.events.write().clear();
        self.stats.clear();
        info!("📊 Monitor cleared");
    }

    /// Get event count
    #[must_use]
    pub fn event_count(&self) -> usize {
        self.events.read().len()
    }
}

impl Default for SignalMonitor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_event_new() {
        // Arrange & Act
        let event = SignalEvent::new("Test Gate", AndonSignal::Green);

        // Assert
        assert_eq!(event.gate_name, "Test Gate");
        assert_eq!(event.signal, AndonSignal::Green);
        assert!(event.details.is_none());
    }

    #[test]
    fn test_signal_event_with_details() {
        // Arrange & Act
        let event = SignalEvent::new("Test Gate", AndonSignal::Yellow)
            .with_details("Unused variable");

        // Assert
        assert_eq!(event.gate_name, "Test Gate");
        assert_eq!(event.signal, AndonSignal::Yellow);
        assert_eq!(event.details, Some("Unused variable".to_string()));
    }

    #[test]
    fn test_signal_stats_rates() {
        // Arrange
        let stats = SignalStats {
            total_checks: 100,
            green_count: 80,
            yellow_count: 15,
            red_count: 5,
        };

        // Act & Assert
        assert_eq!(stats.success_rate(), 0.8);
        assert_eq!(stats.warning_rate(), 0.15);
        assert_eq!(stats.failure_rate(), 0.05);
    }

    #[test]
    fn test_signal_stats_rates_zero_checks() {
        // Arrange
        let stats = SignalStats::default();

        // Act & Assert
        assert_eq!(stats.success_rate(), 0.0);
        assert_eq!(stats.warning_rate(), 0.0);
        assert_eq!(stats.failure_rate(), 0.0);
    }

    #[test]
    fn test_signal_monitor_record() {
        // Arrange
        let monitor = SignalMonitor::new();
        let event = SignalEvent::new("Test Gate", AndonSignal::Green);

        // Act
        monitor.record(event);

        // Assert
        assert_eq!(monitor.event_count(), 1);
        let events = monitor.get_events();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].gate_name, "Test Gate");
        assert_eq!(events[0].signal, AndonSignal::Green);
    }

    #[test]
    fn test_signal_monitor_stats_tracking() {
        // Arrange
        let monitor = SignalMonitor::new();

        // Act
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Green));
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Yellow));
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Red));
        monitor.record(SignalEvent::new("Gate B", AndonSignal::Green));

        // Assert
        let stats_a = monitor.get_stats("Gate A").unwrap();
        assert_eq!(stats_a.total_checks, 3);
        assert_eq!(stats_a.green_count, 1);
        assert_eq!(stats_a.yellow_count, 1);
        assert_eq!(stats_a.red_count, 1);

        let stats_b = monitor.get_stats("Gate B").unwrap();
        assert_eq!(stats_b.total_checks, 1);
        assert_eq!(stats_b.green_count, 1);
    }

    #[test]
    fn test_signal_monitor_get_gate_events() {
        // Arrange
        let monitor = SignalMonitor::new();
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Green));
        monitor.record(SignalEvent::new("Gate B", AndonSignal::Yellow));
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Red));

        // Act
        let gate_a_events = monitor.get_gate_events("Gate A");

        // Assert
        assert_eq!(gate_a_events.len(), 2);
        assert!(gate_a_events.iter().all(|e| e.gate_name == "Gate A"));
    }

    #[test]
    fn test_signal_monitor_capacity_limit() {
        // Arrange
        let monitor = SignalMonitor::with_capacity(5);

        // Act - Add more than capacity
        for i in 0..10 {
            monitor.record(SignalEvent::new(format!("Gate {}", i), AndonSignal::Green));
        }

        // Assert - Should only retain last 5
        let events = monitor.get_events();
        assert_eq!(events.len(), 5);
        assert_eq!(events[0].gate_name, "Gate 5");
        assert_eq!(events[4].gate_name, "Gate 9");
    }

    #[test]
    fn test_signal_monitor_clear() {
        // Arrange
        let monitor = SignalMonitor::new();
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Green));
        monitor.record(SignalEvent::new("Gate B", AndonSignal::Yellow));

        // Act
        monitor.clear();

        // Assert
        assert_eq!(monitor.event_count(), 0);
        assert_eq!(monitor.get_all_stats().len(), 0);
    }

    #[test]
    fn test_signal_monitor_get_all_stats() {
        // Arrange
        let monitor = SignalMonitor::new();
        monitor.record(SignalEvent::new("Gate A", AndonSignal::Green));
        monitor.record(SignalEvent::new("Gate B", AndonSignal::Yellow));
        monitor.record(SignalEvent::new("Gate C", AndonSignal::Red));

        // Act
        let all_stats = monitor.get_all_stats();

        // Assert
        assert_eq!(all_stats.len(), 3);
        assert!(all_stats.iter().any(|(name, _)| name == "Gate A"));
        assert!(all_stats.iter().any(|(name, _)| name == "Gate B"));
        assert!(all_stats.iter().any(|(name, _)| name == "Gate C"));
    }
}
