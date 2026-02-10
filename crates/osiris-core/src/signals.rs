//! OSIRIS Signal System
//!
//! Manages signals for autonomic life management

use serde_json::{Value, json};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use tracing::{error, warn, info, debug};

/// Signal levels in the OSIRIS system
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalLevel {
    /// Informational signals
    Info,
    /// Warning signals
    Warning,
    /// Critical signals requiring immediate attention
    Critical,
}

impl std::fmt::Display for SignalLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignalLevel::Info => write!(f, "INFO"),
            SignalLevel::Warning => write!(f, "WARNING"),
            SignalLevel::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// OSIRIS Signal for system communication
#[derive(Debug, Clone)]
pub struct OSIRISSignal {
    /// Signal type
    pub signal_type: String,
    /// Signal message
    pub message: String,
    /// Signal level
    pub level: SignalLevel,
    /// Signal timestamp
    pub timestamp: DateTime<Utc>,
    /// Signal metadata
    pub metadata: HashMap<String, Value>,
    /// Signal source
    pub source: Option<String>,
    /// Signal target
    pub target: Option<String>,
}

impl OSIRISSignal {
    /// Create a new signal
    pub fn new(signal_type: String, message: String, level: SignalLevel) -> Self {
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
        signal_type: String,
        message: String,
        level: SignalLevel,
        source: Option<String>,
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

    /// Set signal source
    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }

    /// Set signal target
    pub fn with_target(mut self, target: String) -> Self {
        self.target = Some(target);
        self
    }

    /// Check if signal is critical
    pub fn is_critical(&self) -> bool {
        matches!(self.level, SignalLevel::Critical)
    }

    /// Check if signal needs immediate attention
    pub fn needs_attention(&self) -> bool {
        matches!(self.level, SignalLevel::Warning | SignalLevel::Critical)
    }

    /// Get signal severity weight
    pub fn severity_weight(&self) -> u8 {
        match self.level {
            SignalLevel::Info => 1,
            SignalLevel::Warning => 2,
            SignalLevel::Critical => 3,
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

impl std::fmt::Display for OSIRISSignal {
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

/// Signal handler for processing OSIRIS signals
pub struct SignalHandler;

impl SignalHandler {
    /// Process an OSIRIS signal
    pub async fn process(signal: OSIRISSignal) -> Result<(), String> {
        // Log the signal
        match signal.level {
            SignalLevel::Info => info!("OSIRIS Signal: {}", signal),
            SignalLevel::Warning => warn!("OSIRIS Signal: {}", signal),
            SignalLevel::Critical => error!("OSIRIS Signal: {}", signal),
        }

        // Process based on signal type
        match signal.signal_type.as_str() {
            "health_check_summary" => {
                debug!("Processing health check summary signal");
            }
            "domain_registered" => {
                debug!("Processing domain registration signal");
            }
            "domain_unregistered" => {
                debug!("Processing domain unregistration signal");
            }
            "pattern_registered" => {
                debug!("Processing pattern registration signal");
            }
            "pattern_unregistered" => {
                debug!("Processing pattern unregistration signal");
            }
            "state_transition" => {
                debug!("Processing state transition signal");
            }
            _ => {
                debug!("Processing unknown signal type: {}", signal.signal_type);
            }
        }

        // In a full implementation, this would trigger appropriate responses
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_creation() {
        let signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Info,
        );

        assert_eq!(signal.signal_type, "test_signal");
        assert_eq!(signal.message, "Test message");
        assert_eq!(signal.level, SignalLevel::Info);
        assert!(signal.source.is_none());
        assert!(signal.target.is_none());
    }

    #[test]
    fn test_signal_with_routing() {
        let signal = OSIRISSignal::with_routing(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Warning,
            Some("domain1".to_string()),
            Some("engine".to_string()),
        );

        assert_eq!(signal.source, Some("domain1".to_string()));
        assert_eq!(signal.target, Some("engine".to_string()));
    }

    #[test]
    fn test_signal_metadata() {
        let signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Info,
        )
        .with_metadata("key1".to_string(), serde_json::json!("value1"));

        assert_eq!(signal.metadata["key1"], serde_json::json!("value1"));
    }

    #[test]
    fn test_signal_checks() {
        let info_signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Info,
        );

        let warning_signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Warning,
        );

        let critical_signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Critical,
        );

        assert!(!info_signal.is_critical());
        assert!(!info_signal.needs_attention());
        assert_eq!(info_signal.severity_weight(), 1);

        assert!(!warning_signal.is_critical());
        assert!(warning_signal.needs_attention());
        assert_eq!(warning_signal.severity_weight(), 2);

        assert!(critical_signal.is_critical());
        assert!(critical_signal.needs_attention());
        assert_eq!(critical_signal.severity_weight(), 3);
    }

    #[test]
    fn test_signal_to_json() {
        let signal = OSIRISSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            SignalLevel::Info,
        )
        .with_source("source".to_string())
        .with_target("target".to_string())
        .with_metadata("custom_key".to_string(), serde_json::json!(42));

        let json = signal.to_json();
        assert_eq!(json["signal_type"], "test_signal");
        assert_eq!(json["message"], "Test message");
        assert_eq!(json["level"], "INFO");
        assert_eq!(json["source"], "source");
        assert_eq!(json["target"], "target");
        assert_eq!(json["metadata"]["custom_key"], 42);
    }
}