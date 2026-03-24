//! TPS Signal System
//!
//! Signals specific to TPS (Toyota Production System)

use chrono::{DateTime, Utc};
use serde_json::{json, Value};
use std::collections::HashMap;

/// TPS Signal levels
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TPSLevel {
    /// Informational signals
    Information,
    /// Warning signals
    Warning,
    /// Critical signals requiring immediate attention
    Critical,
}

impl std::fmt::Display for TPSLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TPSLevel::Information => write!(f, "INFORMATION"),
            TPSLevel::Warning => write!(f, "WARNING"),
            TPSLevel::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// TPS Signal for Toyota Production System communication
#[derive(Debug, Clone)]
pub struct TPSSignal {
    /// Signal type
    pub signal_type: String,
    /// Signal message
    pub message: String,
    /// Signal level
    pub level: TPSLevel,
    /// Signal timestamp
    pub timestamp: DateTime<Utc>,
    /// Signal metadata
    pub metadata: HashMap<String, Value>,
    /// Signal source
    pub source: Option<String>,
    /// Signal target
    pub target: Option<String>,
}

impl TPSSignal {
    /// Create a new signal
    pub fn new(signal_type: String, message: String, level: TPSLevel) -> Self {
        Self {
            signal_type,
            message,
            level,
            timestamp: Utc::now(),
            metadata: HashMap::new(),
            source: None,
            target: None,
        }
    }

    /// Create a signal with source and target
    pub fn with_routing(
        signal_type: String, message: String, level: TPSLevel, source: Option<String>,
        target: Option<String>,
    ) -> Self {
        Self {
            signal_type,
            message,
            level,
            timestamp: Utc::now(),
            metadata: HashMap::new(),
            source,
            target,
        }
    }

    /// Add metadata to the signal
    pub fn with_metadata(mut self, key: String, value: Value) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Set the source of the signal
    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }

    /// Check if signal is critical
    pub fn is_critical(&self) -> bool {
        matches!(self.level, TPSLevel::Critical)
    }

    /// Check if signal needs immediate attention
    pub fn needs_attention(&self) -> bool {
        matches!(self.level, TPSLevel::Warning | TPSLevel::Critical)
    }

    /// Get signal severity weight
    pub fn severity_weight(&self) -> u8 {
        match self.level {
            TPSLevel::Information => 1,
            TPSLevel::Warning => 2,
            TPSLevel::Critical => 3,
        }
    }

    /// Convert signal to JSON
    pub fn to_json(&self) -> Value {
        json!({
            "signal_type": self.signal_type,
            "message": self.message,
            "level": self.level.to_string(),
            "timestamp": self.timestamp.to_rfc3339(),
            "metadata": self.metadata,
            "source": self.source,
            "target": self.target,
        })
    }
}

impl std::fmt::Display for TPSSignal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}] {} - {}: {}",
            self.timestamp.format("%Y-%m-%d %H:%M:%S"),
            self.level,
            self.signal_type,
            self.message
        )
    }
}

/// Predefined TPS signal types
pub mod signal_types {
    use super::*;

    /// Andon signals
    pub const ANDON_ALERT: &str = "andon_alert";
    pub const ANDON_ACKNOWLEDGE: &str = "andon_acknowledge";
    pub const ANDON_RESOLVE: &str = "andon_resolve";
    pub const LINE_STOP: &str = "line_stop";
    pub const LINE_RESUME: &str = "line_resume";

    /// Jidoka signals
    pub const CRITICAL_ERROR: &str = "critical_error";
    pub const SAFETY_HAZARD: &str = "safety_hazard";
    pub const QUALITY_ISSUE: &str = "quality_issue";
    pub const PROCESS_DEVIATION: &str = "process_deviation";
    pub const MINOR_ANOMALY: &str = "minor_anomaly";

    /// Kaizen signals
    pub const IMPROVEMENT_SUGGESTION: &str = "improvement_suggestion";
    pub const PROCESS_OBSERVATION: &str = "process_observation";
    pub const IMPROVEMENT_IMPLEMENTED: &str = "improvement_implemented";
    pub const CYCLE_COMPLETE: &str = "cycle_complete";

    /// JIT (Just-in-Time) signals
    pub const KANBAN_SIGNAL: &str = "kanban_signal";
    pub const STOCK_DEPLETION: &str = "stock_depletion";
    pub const PULL_REQUEST: &str = "pull_request";
    pub const PRODUCTION_ORDER: &str = "production_order";

    /// General TPS signals
    pub const DAILY_GOAL_REACHED: &str = "daily_goal_reached";
    pub const EFFICIENCY_IMPROVEMENT: &str = "efficiency_improvement";
    pub const WASTE_IDENTIFIED: &str = "waste_identified";
    pub const SAFETY_INCIDENT: &str = "safety_incident";
    pub const QUALITY_DEVIATION: &str = "quality_deviation";
}

/// Helper functions for creating common TPS signals
pub struct TPSSignalBuilder;

impl TPSSignalBuilder {
    /// Create an Andon alert signal
    pub fn andon_alert(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::ANDON_ALERT.to_string(),
            message,
            TPSLevel::Warning,
        )
    }

    /// Create a critical error signal
    pub fn critical_error(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::CRITICAL_ERROR.to_string(),
            message,
            TPSLevel::Critical,
        )
    }

    /// Create a quality issue signal
    pub fn quality_issue(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::QUALITY_ISSUE.to_string(),
            message,
            TPSLevel::Warning,
        )
    }

    /// Create an improvement suggestion signal
    pub fn improvement_suggestion(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::IMPROVEMENT_SUGGESTION.to_string(),
            message,
            TPSLevel::Information,
        )
    }

    /// Create a line stop signal
    pub fn line_stop(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::LINE_STOP.to_string(),
            message,
            TPSLevel::Critical,
        )
    }

    /// Create a line resume signal
    pub fn line_resume(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::LINE_RESUME.to_string(),
            message,
            TPSLevel::Information,
        )
    }

    /// Create a safety hazard signal
    pub fn safety_hazard(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::SAFETY_HAZARD.to_string(),
            message,
            TPSLevel::Critical,
        )
    }

    /// Create a Kanban signal
    pub fn kanban_signal(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::KANBAN_SIGNAL.to_string(),
            message,
            TPSLevel::Information,
        )
    }

    /// Create a daily goal reached signal
    pub fn daily_goal_reached(message: String) -> TPSSignal {
        TPSSignal::new(
            signal_types::DAILY_GOAL_REACHED.to_string(),
            message,
            TPSLevel::Information,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tps_signal_creation() {
        let signal = TPSSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Information,
        );

        assert_eq!(signal.signal_type, "test_signal");
        assert_eq!(signal.message, "Test message");
        assert_eq!(signal.level, TPSLevel::Information);
        assert!(signal.source.is_none());
        assert!(signal.target.is_none());
    }

    #[test]
    fn test_tps_signal_with_routing() {
        let signal = TPSSignal::with_routing(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Warning,
            Some("machine_1".to_string()),
            Some("operator_1".to_string()),
        );

        assert_eq!(signal.source, Some("machine_1".to_string()));
        assert_eq!(signal.target, Some("operator_1".to_string()));
    }

    #[test]
    fn test_tps_signal_builder() {
        let alert_signal = TPSSignalBuilder::andon_alert("Machine overheating".to_string());
        assert_eq!(alert_signal.signal_type, signal_types::ANDON_ALERT);
        assert_eq!(alert_signal.level, TPSLevel::Warning);

        let error_signal = TPSSignalBuilder::critical_error("System failure".to_string());
        assert_eq!(error_signal.signal_type, signal_types::CRITICAL_ERROR);
        assert_eq!(error_signal.level, TPSLevel::Critical);

        let suggestion_signal =
            TPSSignalBuilder::improvement_suggestion("Reduce setup time".to_string());
        assert_eq!(
            suggestion_signal.signal_type,
            signal_types::IMPROVEMENT_SUGGESTION
        );
        assert_eq!(suggestion_signal.level, TPSLevel::Information);
    }

    #[test]
    fn test_tps_signal_checks() {
        let info_signal = TPSSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Information,
        );

        let critical_signal = TPSSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Critical,
        );

        assert!(!info_signal.is_critical());
        assert!(!info_signal.needs_attention());
        assert_eq!(info_signal.severity_weight(), 1);

        assert!(critical_signal.is_critical());
        assert!(critical_signal.needs_attention());
        assert_eq!(critical_signal.severity_weight(), 3);
    }

    #[test]
    fn test_tps_signal_to_json() {
        let signal = TPSSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Warning,
        )
        .with_source("source".to_string())
        .with_target("target".to_string())
        .with_metadata("custom_key".to_string(), serde_json::json!(42));

        let json = signal.to_json();
        assert_eq!(json["signal_type"], "test_signal");
        assert_eq!(json["message"], "Test message");
        assert_eq!(json["level"], "WARNING");
        assert_eq!(json["source"], "source");
        assert_eq!(json["target"], "target");
        assert_eq!(json["metadata"]["custom_key"], 42);
    }
}
