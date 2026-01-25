//! Andon signal types (visual control system)
//!
//! In Toyota production systems, Andon is a visual control that alerts operators of a problem.
//! - Red (Andon cord pulled): Critical problem - stop the line
//! - Yellow (Problem light): Warning - investigate before release
//! - Green (No problem): System operating normally

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Andon signal color (visual control indicator)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum SignalColor {
    /// Red: Critical problem - stop the line immediately (Andon cord pulled)
    #[serde(rename = "red")]
    Red,

    /// Yellow: Warning - investigate before proceeding
    #[serde(rename = "yellow")]
    Yellow,

    /// Green: Normal operation
    #[serde(rename = "green")]
    Green,
}

impl SignalColor {
    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            Self::Red => "CRITICAL",
            Self::Yellow => "WARNING",
            Self::Green => "OK",
        }
    }

    /// Get short code
    pub fn code(&self) -> &'static str {
        match self {
            Self::Red => "RED",
            Self::Yellow => "YLW",
            Self::Green => "GRN",
        }
    }

    /// Check if signal requires immediate action
    pub fn requires_action(&self) -> bool {
        matches!(self, Self::Red | Self::Yellow)
    }

    /// Check if signal is critical (stop-the-line)
    pub fn is_critical(&self) -> bool {
        matches!(self, Self::Red)
    }
}

impl std::fmt::Display for SignalColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// TPS Andon signal (problem notification)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonSignal {
    /// Unique signal ID
    pub id: String,

    /// Signal color (RED/YELLOW/GREEN)
    pub color: SignalColor,

    /// Problem message
    pub message: String,

    /// Component/location where problem occurred
    pub component: String,

    /// Problem details (JSON object)
    pub details: serde_json::Value,

    /// When the signal was generated
    pub timestamp: DateTime<Utc>,

    /// Request ID for tracing through system
    pub trace_id: Option<String>,

    /// Count of times this signal has occurred
    pub occurrence_count: u32,
}

impl AndonSignal {
    /// Create a critical RED signal (stop-the-line)
    pub fn red(message: impl Into<String>) -> Self {
        Self::with_color(SignalColor::Red, message)
    }

    /// Create a WARNING YELLOW signal (investigate before release)
    pub fn yellow(message: impl Into<String>) -> Self {
        Self::with_color(SignalColor::Yellow, message)
    }

    /// Create a GREEN signal (normal operation)
    pub fn green(message: impl Into<String>) -> Self {
        Self::with_color(SignalColor::Green, message)
    }

    /// Create a signal with specified color
    pub fn with_color(color: SignalColor, message: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            color,
            message: message.into(),
            component: "unknown".to_string(),
            details: serde_json::json!({}),
            timestamp: Utc::now(),
            trace_id: None,
            occurrence_count: 1,
        }
    }

    /// Set the component where problem occurred
    pub fn with_component(mut self, component: impl Into<String>) -> Self {
        self.component = component.into();
        self
    }

    /// Add details to the signal
    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = details;
        self
    }

    /// Set trace ID for request tracking
    pub fn with_trace_id(mut self, trace_id: impl Into<String>) -> Self {
        self.trace_id = Some(trace_id.into());
        self
    }

    /// Increment occurrence count (signal repeated)
    pub fn increment_occurrence(&mut self) {
        self.occurrence_count += 1;
    }

    /// Format as structured JSON for logging
    pub fn as_json(&self) -> serde_json::Value {
        serde_json::json!({
            "signal_id": self.id,
            "color": self.color.code(),
            "message": self.message,
            "component": self.component,
            "timestamp": self.timestamp.to_rfc3339(),
            "trace_id": self.trace_id,
            "occurrence_count": self.occurrence_count,
            "details": self.details,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_colors() {
        assert_eq!(SignalColor::Red.name(), "CRITICAL");
        assert_eq!(SignalColor::Yellow.name(), "WARNING");
        assert_eq!(SignalColor::Green.name(), "OK");

        assert!(SignalColor::Red.is_critical());
        assert!(!SignalColor::Green.is_critical());

        assert!(SignalColor::Red.requires_action());
        assert!(SignalColor::Yellow.requires_action());
        assert!(!SignalColor::Green.requires_action());
    }

    #[test]
    fn test_signal_creation() {
        let signal = AndonSignal::red("Queue overflow")
            .with_component("queue-handler")
            .with_trace_id("trace-123");

        assert_eq!(signal.color, SignalColor::Red);
        assert_eq!(signal.message, "Queue overflow");
        assert_eq!(signal.component, "queue-handler");
        assert_eq!(signal.trace_id, Some("trace-123".to_string()));
    }

    #[test]
    fn test_signal_json_format() {
        let signal = AndonSignal::yellow("Memory pressure")
            .with_component("memory-manager");

        let json = signal.as_json();
        assert_eq!(json["color"], "YLW");
        assert_eq!(json["message"], "Memory pressure");
        assert_eq!(json["component"], "memory-manager");
    }
}
