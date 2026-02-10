//! Jidoka gates and andon signal system for ggen
//!
//! This crate implements the Toyota Production System's Jidoka principle:
//! automation with a human touch. Each gate checks quality and raises
//! andon signals. Red signals halt the production line immediately.

pub mod gate;
pub mod line;
pub mod monitor;

// Re-export commonly used types
pub use gate::Gate;
pub use line::{GateResult, ProductionLine};
pub use monitor::{SignalEvent, SignalMonitor, SignalStats};

use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Andon signal levels indicating quality status
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AndonSignal {
    /// All checks pass - proceed with production
    Green,
    /// Warning detected - stop before release
    Yellow,
    /// Critical failure - halt the line immediately
    Red,
}

impl AndonSignal {
    /// Check if signal requires stopping the line
    #[must_use]
    pub fn should_stop(&self) -> bool {
        matches!(self, Self::Red)
    }

    /// Check if signal is a warning
    #[must_use]
    pub fn is_warning(&self) -> bool {
        matches!(self, Self::Yellow)
    }

    /// Check if signal is green (all clear)
    #[must_use]
    pub fn is_green(&self) -> bool {
        matches!(self, Self::Green)
    }
}

impl fmt::Display for AndonSignal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Green => write!(f, "🟢 GREEN"),
            Self::Yellow => write!(f, "🟡 YELLOW"),
            Self::Red => write!(f, "🔴 RED"),
        }
    }
}

/// Errors that can occur in Jidoka system
#[derive(Debug, Error)]
pub enum JidokaError {
    /// Gate execution failed
    #[error("Gate execution failed: {0}")]
    GateFailure(String),

    /// Production line halted
    #[error("Production line halted: {signal} - {reason}")]
    LineHalted { signal: AndonSignal, reason: String },

    /// Command execution failed
    #[error("Command execution failed: {0}")]
    CommandFailed(String),

    /// Invalid gate configuration
    #[error("Invalid gate configuration: {0}")]
    InvalidConfiguration(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for Jidoka operations
pub type Result<T> = std::result::Result<T, JidokaError>;

/// Signal trait for quality checks
#[async_trait::async_trait]
pub trait Signal: Send + Sync {
    /// Execute the quality check and return signal level
    async fn check(&self) -> Result<AndonSignal>;

    /// Get the name of this signal source
    fn name(&self) -> &str;

    /// Get a description of what this signal checks
    fn description(&self) -> &str;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_andon_signal_should_stop() {
        // Arrange & Act & Assert
        assert!(AndonSignal::Red.should_stop());
        assert!(!AndonSignal::Yellow.should_stop());
        assert!(!AndonSignal::Green.should_stop());
    }

    #[test]
    fn test_andon_signal_is_warning() {
        // Arrange & Act & Assert
        assert!(!AndonSignal::Red.is_warning());
        assert!(AndonSignal::Yellow.is_warning());
        assert!(!AndonSignal::Green.is_warning());
    }

    #[test]
    fn test_andon_signal_is_green() {
        // Arrange & Act & Assert
        assert!(!AndonSignal::Red.is_green());
        assert!(!AndonSignal::Yellow.is_green());
        assert!(AndonSignal::Green.is_green());
    }

    #[test]
    fn test_andon_signal_ordering() {
        // Arrange & Act & Assert
        assert!(AndonSignal::Green < AndonSignal::Yellow);
        assert!(AndonSignal::Yellow < AndonSignal::Red);
        assert!(AndonSignal::Green < AndonSignal::Red);
    }

    #[test]
    fn test_andon_signal_display() {
        // Arrange & Act & Assert
        assert_eq!(format!("{}", AndonSignal::Green), "🟢 GREEN");
        assert_eq!(format!("{}", AndonSignal::Yellow), "🟡 YELLOW");
        assert_eq!(format!("{}", AndonSignal::Red), "🔴 RED");
    }
}
